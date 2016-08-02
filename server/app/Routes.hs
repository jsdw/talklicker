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

import Types
import Servant
import Application
import Middleware

type UserSessions = Sessions String
type UserSession = Session User

type Routes = Login :<|> Logout :<|> GetCurrentUser :<|> GetEntries :<|> GetUsers :<|> GetUser :<|> SetUser :<|> GetDays
routes      = login :<|> logout :<|> getCurrentUser :<|> getEntries :<|> getUsers :<|> getUser :<|> setUser :<|> getDays

--
-- LOGIN
--

type Login = "login" :> ReqBody '[JSON] LoginInput :> Post '[JSON] String

login :: LoginInput -> Application String
login LoginInput{..} = do

    sess <- getSessions
    user <- getUserOr loginName (throwError err401)

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
-- GET ALL USER INFO
--

type GetUsers = "users" :> Get '[JSON] [UserOutput]

getUsers :: Application [UserOutput]
getUsers = fmap (fmap toUserOutput . allUsers) getEverything

--
-- GET ONE USER INFO
--

type GetUser = "users" :> Capture "username" String :> Get '[JSON] UserOutput

getUser :: String -> Application UserOutput
getUser name = do
    user <- getUserOr name (throwError err404)
    return (toUserOutput user)

--
-- SET ONE USER INFO
--

type SetUser = "users" :> HasSession :> Capture "username" String :> ReqBody '[JSON] UserInput :> Post '[JSON] UserOutput

setUser :: UserSession -> String -> UserInput -> Application UserOutput
setUser (Session _ sessUser) name input = do

    user <- getUserOr name (throwError err404)
    throwIf (userName sessUser /= userName user) err401

    mHash <- case uiPass input of
        Just pass -> Just <$> liftIO (hashPassword 13 (Bytes.pack pass))
        Nothing -> return Nothing

    let user' = user
          { userFullName = maybe (userFullName user) id (uiFullName input)
          , userPassHash = maybe (userPassHash user) Bytes.unpack mHash
          }

    appState <- ask
    Database.modify (appDatabase appState) $ \db -> do
        return db{ allUsers = fmap (\u -> if userName user == userName u then user' else u) (allUsers db) }

    return (toUserOutput user')



data UserInput = UserInput
    { uiFullName :: Maybe String
    , uiPass     :: Maybe String
    } deriving (Show, Eq, Generic)

instance FromJSON UserOutput where parseJSON = fromPrefix "ui"

--
-- GET DAYS
--

type GetDays = "days" :> Get '[JSON] [Day]

getDays :: Application [Day]
getDays = fmap allDays getEverything



--
-- Utility functions:
--

getUserOr :: String -> Application User -> Application User
getUserOr name orElse = do
    users <- fmap allUsers getEverything
    case List.find (\u -> userName u == name) users of
        Nothing -> orElse
        Just u -> return u

getSessions :: Application UserSessions
getSessions = ask >>= return . appSessions

getEverything :: Application Everything
getEverything = ask >>= Database.read . appDatabase

throwIf :: MonadError e m => Bool -> e -> m ()
throwIf b err = if b then throwError err else return ()

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