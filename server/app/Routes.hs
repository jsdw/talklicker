{-# LANGUAGE DeriveGeneric, RecordWildCards, DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Routes (routes, Routes) where

import qualified Data.List as List
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (ask)
import qualified Database
-- import Database (Database)
import qualified Sessions
import Sessions (Session(Session))
import GHC.Generics (Generic)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString.Char8 as Bytes

import Types
import Servant
import Application

type HasSession = AuthProtect "session"

type Routes = Login :<|> Logout :<|> GetCurrentUser :<|> GetEntries :<|> GetUsers :<|> GetDays
routes = login :<|> logout :<|> getCurrentUser :<|> getEntries :<|> getUsers :<|> getDays

--
-- LOGIN
--

type Login = "login" :> ReqBody '[JSON] LoginInput :> Post '[JSON] String

login :: LoginInput -> Application String
login LoginInput{..} = do

    sess <- getSessions
    users <- fmap allUsers getEverything

    let mUser = List.find (\u -> userName u == loginName) users
    let isValidPass = case mUser of
            Nothing -> False
            Just u -> validatePassword (Bytes.pack loginPass) (Bytes.pack $ userPassHash u)

    throwIf (not isValidPass) err500

    Session sessId _ <- Sessions.create (loginName) sess
    return sessId

data LoginInput = LoginInput
    { loginName :: String
    , loginPass :: String
    } deriving (Eq, Show, Generic)

instance ToJSON LoginInput where toJSON = toPrefix "login"
instance FromJSON LoginInput where parseJSON = fromPrefix "login"

--
-- LOGOUT
--

type Logout = HasSession :> "logout" :> Post '[JSON] ()

logout :: UserSession -> Application ()
logout session = do
    sessions <- getSessions
    Sessions.removeSession session sessions
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
instance FromJSON UserOutput where parseJSON = fromPrefix "_uoUser"

toUserOutput :: User -> UserOutput
toUserOutput User{..} = UserOutput userName userFullName

--
-- GET ALL ENTRIES
--

type GetEntries = "entries" :> Get '[JSON] [Entry]

getEntries :: Application [Entry]
getEntries = fmap allEntries getEverything

--
-- GET USER INFO
--

type GetUsers = "users" :> Get '[JSON] [UserOutput]

getUsers :: Application [UserOutput]
getUsers = fmap (fmap toUserOutput . allUsers) getEverything

--
-- GET DAYS
--

type GetDays = "days" :> Get '[JSON] [Day]

getDays :: Application [Day]
getDays = fmap allDays getEverything



--
-- Utility functions:
--

getSessions :: Application UserSessions
getSessions = ask >>= return . appSessions

getEverything :: Application Everything
getEverything = ask >>= Database.read . appDatabase

throwIf :: MonadError e m => Bool -> e -> m ()
throwIf b err = if b then throwError err else return ()

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