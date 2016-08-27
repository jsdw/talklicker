{-# LANGUAGE DeriveGeneric, RecordWildCards, DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Routes (routes, Routes) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (ask)
import Sessions (Sessions)
import Control.Monad.Trans (liftIO, MonadIO)
import Crypto.KDF.BCrypt (validatePassword, hashPassword)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Monoid ((<>))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.ByteString.Char8 as Bytes
import qualified Database
-- import Database (Database)
import qualified Sessions
import qualified Id

import Lens.Micro.Platform
import Types
import Servant
import Application
import Middleware

type UserSessions = Sessions String
type UserSession = Session User
type AdminUserSession = AdminSession User

--
-- Our routing contents table.
--

type Routes = "core"    :> CoreRoutes
         :<|> "entries" :> EntryRoutes
         :<|> "users"   :> UserRoutes
         :<|> "days"    :> DayRoutes

routes = coreRoutes
    :<|> entryRoutes
    :<|> userRoutes
    :<|> dayRoutes

type CoreRoutes = Login :<|> Logout
coreRoutes      = login :<|> logout

type EntryRoutes = GetEntries :<|> GetEntry :<|> SetEntry :<|> SetEntryOrder :<|> AddEntry :<|> RemoveEntry
entryRoutes      = getEntries :<|> getEntry :<|> setEntry :<|> setEntryOrder :<|> addEntry :<|> removeEntry

type UserRoutes = GetCurrentUser :<|> GetUsers :<|> GetUser :<|> SetUser :<|> AddUser :<|> RemoveUser
userRoutes      = getCurrentUser :<|> getUsers :<|> getUser :<|> setUser :<|> addUser :<|> removeUser

type DayRoutes = GetDays :<|> GetDay :<|> SetDay :<|> AddDay :<|> RemoveDay
dayRoutes      = getDays :<|> getDay :<|> setDay :<|> addDay :<|> removeDay


-- ############
-- ### CORE ###
-- ############

--
-- LOGIN
--

type Login = "login" :> ReqBody '[JSON] LoginInput :> Post '[JSON] (Headers '[Header "Set-Cookie" String] UserOutput)

login :: LoginInput -> Application (Headers '[Header "Set-Cookie" String] UserOutput)
login LoginInput{..} = do

    sess <- getSessions
    user <- getUserOr loginName (throwError err401{ errReasonPhrase = "BAD_USER" })

    -- if pass not set (empty passHash) or pass provided matches passHash, it's valid.
    let isValidPass = if null (userPass user) then True
                      else validatePassword (Bytes.pack loginPass) (Bytes.pack $ userPass user)

    throwIf (not isValidPass) err401{ errReasonPhrase = "BAD_PASSWORD" }

    sessId <- Sessions.create (loginName) sess
    return $ addHeader (cookie "talklicker_session" sessId) (toUserOutput user)

  where
    -- cookie lasts for a month and not accessible in http:
    cookie key val = key <> "=" <> val <> "; Max-Age=2592000; Path=/; HttpOnly"

--
-- LOGOUT
--

type Logout = HasSession :> "logout" :> Post '[JSON] ()

logout :: UserSession -> Application ()
logout (Session sessId _) = do
    sessions <- getSessions
    Sessions.remove sessId sessions
    return ()


-- ###############
-- ### ENTRIES ###
-- ###############

--
-- GET ALL ENTRIES
--

type GetEntries = Get '[JSON] [Entry]

getEntries :: Application [Entry]
getEntries = fmap allEntries getEverything

--
-- GET ONE ENTRY
--

type GetEntry = Capture "entryId" Id :> Get '[JSON] Entry

getEntry :: Id -> Application Entry
getEntry eId = getEntryOr eId (throwError err404)

--
-- SET ONE ENTRY
--

type SetEntry = HasSession :> Capture "entryId" Id :> ReqBody '[JSON] EntryInput :> Post '[JSON] Entry

setEntry :: UserSession -> Id -> EntryInput -> Application Entry
setEntry (Session _ sessUser) eId input = do

    let isAdmin = userType sessUser == Admin

    -- expects entry to have name and description and nonzero duration if these are set:
    let trueForJust cond mVal = if mVal == Nothing then True else fmap cond mVal == Just True
    throwIf ((<= 0) `trueForJust` eiDuration input) err400{ errReasonPhrase = "BAD_DURATION" }
    throwIf (null `trueForJust` eiName input) err400{ errReasonPhrase = "BAD_NAME" }
    throwIf (null `trueForJust` eiDescription input) err400{ errReasonPhrase = "BAD_DESCRIPTION" }

    currTime <- getTimeMillis

    let update entry = entry
            & entryDurationL    ~? eiDuration input
            & entryNameL        ~? eiName input
            & entryDescriptionL ~? eiDescription input
            & entryTypeL        ~? eiType input
            & entryModifiedL    .~ currTime
            & entryUserL        ~? if isAdmin then Nothing else eiUser input

    mNewEntry <- modifyItems allEntriesL $ \e ->
        if (isAdmin || entryUser e == userName sessUser) && eId == entryId e
        then Just (update e) else Nothing

    case mNewEntry of
        Nothing -> throwError err404
        Just e  -> return e

--
-- SET ENTRY ORDER
--

type SetEntryOrder = HasSession :> "order" :> ReqBody '[JSON] [Id] :> Post '[JSON] [Id]

setEntryOrder :: UserSession -> [Id] -> Application [Id]
setEntryOrder _ ids = do

    newEverything <- modifyDb $ \everything ->
        let e = over allEntriesL (sortListBy entryId ids) everything
        in (e,e)

    return $ fmap entryId $ allEntries newEverything

--
-- ADD ENTRY
--

type AddEntry = HasSession :> ReqBody '[JSON] AddEntryInput :> Post '[JSON] Entry

addEntry :: UserSession -> AddEntryInput -> Application Entry
addEntry (Session _ sessUser) input = do

    let nameToAddAs = case addEntryUser input of
            Nothing -> userName sessUser
            Just n -> n

    -- entry must be same user as session user unless user is admin:
    throwIf (userType sessUser /= Admin && nameToAddAs /= userName sessUser) err401

    -- expects entry to have name and description and nonzero duration
    throwIf (addEntryDuration input <= 0) err400{ errReasonPhrase = "BAD_DURATION" }
    throwIf (null (addEntryName input)) err400{ errReasonPhrase = "BAD_NAME" }
    throwIf (null (addEntryDescription input)) err400{ errReasonPhrase = "BAD_DESCRIPTION" }

    currTime <- getTimeMillis
    newId <- Id <$> Id.generate
    mItem <- getItem allEntriesL (\e -> entryId e == newId)

    let newItem = Entry
            { entryId          = newId
            , entryUser        = nameToAddAs
            , entryDuration    = addEntryDuration input
            , entryName        = addEntryName input
            , entryDescription = addEntryDescription input
            , entryType        = addEntryType input
            , entryCreated     = currTime
            , entryModified    = currTime
            }

    case mItem of
        Nothing -> addItem allEntriesL newItem >> return newItem
        Just _ -> throwError err500


--
-- REMOVE ENTRY
--

type RemoveEntry = HasSession :> Capture "entryId" Id :> Delete '[JSON] ()

removeEntry :: UserSession -> Id -> Application ()
removeEntry (Session _ sessUser) eId = do
    getEntryOr eId (throwError err404)
    modifyDb $ \e ->
        let keepFn en = if userType sessUser == Admin
                then entryId en /= eId
                else entryUser en /= userName sessUser || entryId en /= eId
            newE = over allEntriesL (filter keepFn) e
        in (cleanupEverything newE, ())

    return ()


-- #############
-- ### USERS ###
-- #############

--
-- GET CURRENT USER
--

type GetCurrentUser = HasSession :> "current" :> Get '[JSON] UserOutput

getCurrentUser :: UserSession -> Application UserOutput
getCurrentUser (Session _ user) = return (toUserOutput user)

--
-- GET ALL USER INFO
--

type GetUsers = Get '[JSON] [UserOutput]

getUsers :: Application [UserOutput]
getUsers = fmap allUsers getEverything >>= return . fmap toUserOutput

--
-- GET ONE USER INFO
--

type GetUser = Capture "username" String :> Get '[JSON] UserOutput

getUser :: String -> Application UserOutput
getUser name = getUserOr name (throwError err404) >>= return . toUserOutput

--
-- SET ONE USER INFO
--

type SetUser = HasSession :> Capture "username" String :> ReqBody '[JSON] UserInput :> Post '[JSON] UserOutput

setUser :: UserSession -> String -> UserInput -> Application UserOutput
setUser (Session _ sessUser) name input = do

    -- we must be an admin or be the user we're editing
    let isAdmin = userType sessUser == Admin
    throwIf (not isAdmin && userName sessUser /= name) err401

    -- hash the provided password if necessary
    mHash <- case uiPass input of
        Just pass -> Just <$> (if null pass then return "" else hashPass pass)
        Nothing -> return Nothing

    -- update the user using any details provided. Leave values
    -- alone if they are Nothing, alter if Maybe val.
    let update user = user
            & userFullNameL ~? uiFullName input
            & userPassL     ~? mHash
            & userTypeL     ~? if isAdmin then uiType input else Nothing

    -- perform our update on the db
    mUser <- modifyItems allUsersL $ \u ->
        if name == userName u then Just (update u) else Nothing

    -- err if user was not found else return new details:
    case mUser of
        Nothing -> throwError err404
        Just u -> return (toUserOutput u)

--
-- ADD USER
--

type AddUser = IsAdmin :> ReqBody '[JSON] User :> Post '[JSON] UserOutput

addUser :: AdminUserSession -> User -> Application UserOutput
addUser _ input = do
    mUser <- getItem allUsersL (\u -> userName u == userName input)
    case mUser of
        Nothing -> do
            pass <- if null (userPass input) then return "" else hashPass (userPass input)
            toUserOutput <$> addItem allUsersL input{ userPass = pass }
        Just _ -> throwError err400 -- username taken!



--
-- REMOVE USER
--

type RemoveUser = IsAdmin :> Capture "username" String :> Delete '[JSON] ()

removeUser :: AdminUserSession -> String -> Application ()
removeUser _ name = do

    -- throw if user not found.
    getUserOr name (throwError err404)

    -- cleanup, removing the user and all entries with that user name.
    modifyDb $ \e ->
        let newE = cleanupEverything $ over allUsersL (filter (\a -> userName a /= name)) e
        in (newE, ())

    return ()


-- ############
-- ### DAYS ###
-- ############

--
-- GET DAYS
--

type GetDays = Get '[JSON] [Day]

getDays :: Application [Day]
getDays = fmap allDays getEverything

--
-- GET DAY
--

type GetDay = Capture "dayId" Id :> Get '[JSON] Day

getDay :: Id -> Application Day
getDay dId = getDayOr dId (throwError err404)

--
-- SET DAY
--

type SetDay = IsAdmin :> Capture "dayId" Id :> ReqBody '[JSON] DayInput :> Post '[JSON] Day

setDay :: AdminUserSession -> Id -> DayInput -> Application Day
setDay _ dId input = do

    let mEntryIds = diEntries input
        update day = day
            & dayTitleL  ~? diTitle input
            & dayDateL   ~? diDate input
            & dayEntriesL ~? mEntryIds

    entriesExist <- case mEntryIds of
        Nothing -> return True
        Just eIds -> doEntriesExist eIds

    throwIf (not entriesExist) err400

    mDay <- modifyItems allDaysL $ \day ->
        if dayId day == dId then Just (update day) else Nothing

    case mDay of
        Nothing -> throwError err404
        Just newDay -> return newDay

--
-- ADD DAY
--

type AddDay = IsAdmin :> ReqBody '[JSON] AddDayInput :> Post '[JSON] Day

addDay :: AdminUserSession -> AddDayInput -> Application Day
addDay _ input = do

    let entryIds = addDayEntries input

    entriesExist <- doEntriesExist entryIds
    throwIf (not entriesExist) err400

    newId <- Id <$> Id.generate
    mItem <- getItem allDaysL (\d -> dayId d == newId)

    let newDay = Day
            { dayId = newId
            , dayTitle = addDayTitle input
            , dayDate = addDayDate input
            , dayEntries = entryIds
            }

    case mItem of
        Nothing -> addItem allDaysL newDay >> return newDay
        Just _ -> throwError err500

--
-- REMOVE DAY
--

type RemoveDay = IsAdmin :> ReqBody '[JSON] Id :> Delete '[JSON] Bool

removeDay :: AdminUserSession -> Id -> Application Bool
removeDay _ dId = do
    getDayOr dId (throwError err404)
    isRemoved <- removeItems allDaysL (\d -> dayId d == dId)
    if isRemoved then return True else throwError err401

--
-- Utility functions:
--

-- consistify stuff, removing
-- - entries corresponding to users that no longer exist
-- - entry IDs in days corresponding to entries that no longer exist
cleanupEverything :: Everything -> Everything
cleanupEverything = cleanupDays . cleanupEntries
  where
    cleanupEntries e =
        let userSet = Set.fromList (fmap userName $ allUsers e)
        in over allEntriesL (filter (\en -> Set.member (entryUser en) userSet)) e
    cleanupDays e =
        let entrySet = Set.fromList (fmap entryId $ allEntries e)
        in over (allDaysL . each . dayEntriesL) (filter (\eId -> Set.member eId entrySet)) e

-- check whether all Ids provided are valid entries in the DB
doEntriesExist :: [Id] -> Application Bool
doEntriesExist ids = do
    entrySet <- (Set.fromList . fmap entryId . allEntries) <$> getEverything
    return $ List.all (== True) $ fmap (\i -> Set.member i entrySet) ids

-- get current time in milliseconds
getTimeMillis :: (Integral t, MonadIO m) => m t
getTimeMillis = liftIO (round . (* 1000) <$> getPOSIXTime)

-- sort the subitems [b] into the order provided within
-- list [a], leaving other a's alone relatively.
sortListBy :: Ord b => (a -> b) -> [b] -> [a] -> [a]
sortListBy toOrderItem order list = doSort orderList order
  where
    orderList = fmap (\a -> (toOrderItem a, a)) list
    orderSet = Set.fromList order
    listMap = Map.fromList orderList

    doSort [] _ = []
    doSort as [] = fmap snd as
    doSort items@((aOrder,a):as) (o:os) = if Set.member aOrder orderSet
        then case Map.lookup o listMap of
            Just newA -> newA : doSort as os
            Nothing   -> doSort items os
        else a : doSort as (o:os)

hashPass :: MonadIO m => String -> m String
hashPass = fmap Bytes.unpack . liftIO . hashPassword 13 . Bytes.pack

getUserOr :: String -> Application User -> Application User
getUserOr name = getOr allUsersL (\u -> userName u == name)

getDayOr :: Id -> Application Day -> Application Day
getDayOr dId = getOr allDaysL (\d -> dayId d == dId)

getEntryOr :: Id -> Application Entry -> Application Entry
getEntryOr dId = getOr allEntriesL (\d -> entryId d == dId)

getOr :: Lens' Everything [a] -> (a -> Bool) -> Application a -> Application a
getOr l compareFn orElse = do
    mItem <- getItem l compareFn
    case mItem of
        Nothing -> orElse
        Just item -> return item

getItem :: Lens' Everything [a] -> (a -> Bool) -> Application (Maybe a)
getItem l compareFn = do
    e <- getEverything
    return $ List.find compareFn (view l e)

addItem :: Lens' Everything [a] -> a -> Application a
addItem l item = modifyDb $ \everything -> (over l (++ [item]) everything, item)

removeItems :: Lens' Everything [a] -> (a -> Bool) -> Application Bool
removeItems l removeThese = modifyDb $ \everything ->
    let (removed, kept) = List.partition removeThese (view l everything)
    in (set l kept everything, not (null removed))

modifyItems :: Lens' Everything [a] -> (a -> Maybe a) -> Application (Maybe a)
modifyItems l modifyFn = modifyDb $ \everything ->
    let (newItems, mItem) = foldr fn ([], Nothing) (view l everything)
    in (set l newItems everything, mItem)
  where
    fn item (items, mItem) = case modifyFn item of
        Nothing      -> (item : items, mItem)
        Just newItem -> (newItem : items, Just newItem)

modifyDb :: (Everything -> (Everything, a)) -> Application a
modifyDb fn = do
    db <- appDatabase <$> ask
    Database.modify db (return . fn)

getSessions :: Application UserSessions
getSessions = ask >>= return . appSessions

getEverything :: Application Everything
getEverything = ask >>= Database.read . appDatabase

throwIf :: MonadError e m => Bool -> e -> m ()
throwIf b err = if b then throwError err else return ()

-- set the value pointed at by the lens to the provided Maybe a
-- if it's Just a, or leave the value alone if it's Nothing:
(~?) :: Lens' a b -> Maybe b -> a -> a
(~?) l mVal = over l $ \val -> case mVal of
    Nothing -> val
    Just v  -> v
