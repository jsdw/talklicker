{-# LANGUAGE DeriveGeneric, RecordWildCards, DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Routes (routes, Routes) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (ask)
import Sessions (Sessions)
import Control.Monad.Trans (liftIO, MonadIO)
import Crypto.KDF.BCrypt (validatePassword, hashPassword)

import qualified Data.List as List
import qualified Data.ByteString.Char8 as Bytes
import qualified Database
-- import Database (Database)
import qualified Sessions

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

type EntryRoutes = GetEntries :<|> GetEntry :<|> SetEntry :<|> AddEntry :<|> RemoveEntry
entryRoutes      = getEntries :<|> getEntry :<|> setEntry :<|> addEntry :<|> removeEntry

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

type Login = "login" :> ReqBody '[JSON] LoginInput :> Post '[JSON] String

login :: LoginInput -> Application String
login LoginInput{..} = do

    sess <- getSessions
    user <- getUserOr loginName (throwError err401)

    -- if pass not set (empty passHash) or pass provided matches passHash, it's valid.
    let isValidPass = if null (userPass user) then True
                      else validatePassword (Bytes.pack loginPass) (Bytes.pack $ userPass user)

    throwIf (not isValidPass) err401

    sessId <- Sessions.create (loginName) sess
    return sessId

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

type SetEntry = HasSession :> Capture "entryId" Id :> ReqBody '[JSON] Entry :> Post '[JSON] Entry

setEntry :: UserSession -> Id -> Entry -> Application Entry
setEntry (Session _ sessUser) eId input = do

    return undefined

--
-- ADD ENTRY
--

type AddEntry = HasSession :> ReqBody '[JSON] Entry :> Post '[JSON] Entry

addEntry :: UserSession -> Entry -> Application Entry
addEntry (Session _ sessUser) input = do

    return undefined

--
-- REMOVE ENTRY
--

type RemoveEntry = HasSession :> Capture "entryId" Id :> Delete '[JSON] Bool

removeEntry :: UserSession -> Id -> Application Bool
removeEntry (Session _ sessUser) eId = do
    getEntryOr eId (throwError err404)
    isRemoved <- removeItems allEntriesL (\e -> entryUser e == userName sessUser && entryId e == eId)
    if isRemoved then return True else throwError err401


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
        Just pass -> Just <$> hashPass pass
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
    passHash <- hashPass (userPass input)
    toUserOutput <$> addItem allUsersL input{ userPass = passHash }

--
-- REMOVE USER
--

type RemoveUser = IsAdmin :> Capture "username" String :> Delete '[JSON] Bool

removeUser :: AdminUserSession -> String -> Application Bool
removeUser _ name = do
    getUserOr name (throwError err404)
    isRemoved <- removeItems allUsersL (\u -> userName u == name)
    if isRemoved then return True else throwError err401


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

    let update day = day
            & dayTitleL  ~? diTitle input
            & dayDateL   ~? diDate input
            & dayEventsL ~? diEvents input

    mDay <- modifyItems allDaysL $ \day ->
        if dayId day == dId then Just (update day) else Nothing

    case mDay of
        Nothing -> throwError err404
        Just newDay -> return newDay

--
-- ADD DAY
--

type AddDay = IsAdmin :> ReqBody '[JSON] DayInput :> Post '[JSON] Day

addDay :: AdminUserSession -> DayInput -> Application Day
addDay _ input = do

    return undefined

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
addItem l item = modifyDb $ \everything -> (over l (item :) everything, item)

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
