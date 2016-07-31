{-# LANGUAGE RecordWildCards, DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Routes (routes, Routes) where

import qualified Data.List as List
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (ask)
import qualified Database
import Database (Database)
import qualified Sessions
import Sessions (Session(Session))

import Types
import Servant
import Application

type AuthorizedUser = AuthProtect "session"

type Routes = Login :<|> Logout :<|> GetCurrentUser :<|> GetEntries :<|> GetUsers
routes = login :<|> logout :<|> getCurrentUser :<|> getEntries :<|> getUsers

type Login = "login" :> ReqBody '[JSON] LoginInfo :> Post '[JSON] String

login :: LoginInfo -> Application String
login LoginInfo{..} = do

    sess <- getSessions
    users <- fmap allUsers getEverything

    throwIf (not $ List.any (\u -> userName u == loginName) users) err500

    Session sessId _ <- Sessions.create (loginName) sess
    return sessId


type Logout = AuthorizedUser :> "logout" :> Post '[JSON] ()

logout :: UserSession -> Application ()
logout session = do
    sessions <- getSessions
    Sessions.removeSession session sessions
    return ()


type GetCurrentUser = AuthorizedUser :> "user" :> Get '[JSON] User

getCurrentUser :: UserSession -> Application User
getCurrentUser (Session _ username) = do

    users <- fmap allUsers getEverything
    let mUser = List.find (\u -> userName u == username) users

    case mUser of
        Nothing -> throwError $ err500 {errReasonPhrase = "user not found; how odd!"}


type GetEntries = "entries" :> Get '[JSON] [Entry]

getEntries :: Application [Entry]
getEntries = fmap allEntries getEverything


type GetUsers = "users" :> Get '[JSON] [User]

getUsers :: Application [User]
getUsers = fmap allUsers getEverything


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