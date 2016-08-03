{-# LANGUAGE DeriveGeneric, RecordWildCards, DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Routes (routes, Routes) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (ask)
import Sessions (Sessions)
import GHC.Generics (Generic)
import Control.Monad.Trans (liftIO)
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

type Routes = Login :<|> Logout :<|> GetCurrentUser :<|> GetEntries :<|> GetEntry :<|> GetUsers :<|> GetUser :<|> SetUser :<|> GetDays
routes      = login :<|> logout :<|> getCurrentUser :<|> getEntries :<|> getEntry :<|> getUsers :<|> getUser :<|> setUser :<|> getDays

--
-- LOGIN
--

type Login = "login" :> ReqBody '[JSON] LoginInput :> Post '[JSON] String

login :: LoginInput -> Application String
login LoginInput{..} = do

    sess <- getSessions
    user <- getUserOr loginName (throwError err401)

    -- if pass not set (empty passHash) or pass provided matches passHash, it's valid.
    let isValidPass = if null (userPassHash user) then True
                      else validatePassword (Bytes.pack loginPass) (Bytes.pack $ userPassHash user)

    throwIf (not isValidPass) err401

    sessId <- Sessions.create (loginName) sess
    return sessId

data LoginInput = LoginInput
    { loginName :: String
    , loginPass :: String
    } deriving (Eq, Show, Generic)

instance FromJSON LoginInput where parseJSON = fromPrefix "login"

--
-- LOGOUT
--

type Logout = HasSession :> "logout" :> Post '[JSON] ()

logout :: UserSession -> Application ()
logout (Session sessId _) = do
    sessions <- getSessions
    Sessions.remove sessId sessions
    return ()

--
-- GET CURRENT USER
--

type GetCurrentUser = HasSession :> "users" :> "current" :> Get '[JSON] UserOutput

getCurrentUser :: UserSession -> Application UserOutput
getCurrentUser (Session _ user) = return (toUserOutput user)

data UserOutput = UserOutput
    { _uoUserName :: String
    , _uoUserFullName :: String
    } deriving (Eq, Show, Generic)

instance ToJSON UserOutput where toJSON = toPrefix "_uoUser"

toUserOutput :: User -> UserOutput
toUserOutput User{..} = UserOutput userName userFullName

--
-- GET ALL ENTRIES
--

type GetEntries = "entries" :> Get '[JSON] [Entry]

getEntries :: Application [Entry]
getEntries = fmap allEntries getEverything

--
-- GET ONE ENTRY
--

type GetEntry = "entries" :> Capture "entry" Id :> Get '[JSON] Entry

getEntry :: Id -> Application Entry
getEntry entryId = getEntryOr entryId (throwError err404)

--
-- GET ALL USER INFO
--

type GetUsers = "users" :> Get '[JSON] [UserOutput]

getUsers :: Application [UserOutput]
getUsers = fmap allUsers getEverything >>= return . fmap toUserOutput

--
-- GET ONE USER INFO
--

type GetUser = "users" :> Capture "username" String :> Get '[JSON] UserOutput

getUser :: String -> Application UserOutput
getUser name = getUserOr name (throwError err404) >>= return . toUserOutput

--
-- SET ONE USER INFO
--

type SetUser = "users" :> HasSession :> Capture "username" String :> ReqBody '[JSON] UserInput :> Post '[JSON] UserOutput

setUser :: UserSession -> String -> UserInput -> Application UserOutput
setUser (Session _ sessUser) name input = do

    -- we must be an admin or be the user we're editing
    throwIf (userType sessUser /= Admin && userName sessUser /= name) err401

    -- hash the provided password if necessary
    mHash <- case uiPass input of
        Just pass -> Just . Bytes.unpack <$> liftIO (hashPassword 13 (Bytes.pack pass))
        Nothing -> return Nothing

    -- update the user using any details provided. Leave values
    -- alone if they are Nothing, alter if Maybe val.
    let update user = user
            & userFullNameL ?= uiFullName input
            & userPassHashL ?= mHash

    -- perform our update on the db
    mUser <- modifyItems allUsersL $ \u ->
        if name == userName u then Just (update u) else Nothing

    -- err if user was not found else return new details:
    case mUser of
        Nothing -> throwError err404
        Just u -> return (toUserOutput u)


data UserInput = UserInput
    { uiFullName :: Maybe String
    , uiPass     :: Maybe String
    } deriving (Show, Eq, Generic)

instance FromJSON UserInput where parseJSON = fromPrefix "ui"

--
-- GET DAYS
--

type GetDays = "days" :> Get '[JSON] [Day]

getDays :: Application [Day]
getDays = fmap allDays getEverything

--
-- SET DAY
--

type SetDay = IsAdmin :> "days" :> ReqBody '[JSON] DayInput :> Post '[JSON] Day

setDay :: AdminUserSession -> DayInput -> Application Day
setDay (AdminSession sessId sessUser) input = do

    let update day = day
            & dayTitleL  ?= diTitle input
            & dayDateL   ?= diDate input
            & dayEventsL ?= diEvents input

    mDay <- modifyItems allDaysL $ \day ->
        if dayId day == diId input then Just (update day) else Nothing

    case mDay of
        Nothing -> throwError err404
        Just newDay -> return newDay

data DayInput = DayInput
    { diId :: Id
    , diTitle :: Maybe String
    , diDate :: Maybe Int
    , diEvents :: Maybe [Id]
    } deriving (Show, Eq, Generic)

instance FromJSON DayInput where parseJSON = fromPrefix "di"

--
-- Utility functions:
--

getUserOr :: String -> Application User -> Application User
getUserOr name = getOr allUsersL (\u -> userName u == name)

getDayOr :: Id -> Application Day -> Application Day
getDayOr dId = getOr allDaysL (\d -> dayId d == dId)

getEntryOr :: Id -> Application Entry -> Application Entry
getEntryOr dId = getOr allEntriesL (\d -> entryId d == dId)

getOr :: Lens' Everything [a] -> (a -> Bool) -> Application a -> Application a
getOr l compareFn orElse = do
    users <- getEverything
    case List.find compareFn (users ^. l) of
        Nothing -> orElse
        Just u -> return u

removeItems :: Lens' Everything [a] -> (a -> Bool) -> Application Bool
removeItems l removeThese = do
    db <- appDatabase <$> ask
    Database.modify db $ \everything ->
        let (removed, kept) = List.partition removeThese (view l everything)
        in return (set l kept everything, not (null removed))

modifyItems :: Lens' Everything [a] -> (a -> Maybe a) -> Application (Maybe a)
modifyItems l modifyFn = do
    db <- appDatabase <$> ask
    Database.modify db $ \everything ->
        let (newItems, mItem) = foldr fn ([], Nothing) (view l everything)
        in return (set l newItems everything, mItem)
  where
    fn item (items, mItem) = case modifyFn item of
        Nothing      -> (item : items, mItem)
        Just newItem -> (newItem : items, Just newItem)

getSessions :: Application UserSessions
getSessions = ask >>= return . appSessions

getEverything :: Application Everything
getEverything = ask >>= Database.read . appDatabase

throwIf :: MonadError e m => Bool -> e -> m ()
throwIf b err = if b then throwError err else return ()

-- set the value pointed at by the lens to the provided Maybe a
-- if it's Just a, or leave the value alone if it's Nothing:
(?=) :: Lens' a b -> Maybe b -> a -> a
(?=) l mVal = over l $ \val -> case mVal of
    Nothing -> val
    Just v  -> v

--ifMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
--ifMaybe m act = case m of
--    Nothing -> return ()
--    Just a -> act a

--
-- Here's our API..
--

-- get events
-- set event
-- remove event
-- order events

-- get days
-- set day
-- remove day

-- get users
-- get session for user
-- set user (name/password bcrypt hash settable)