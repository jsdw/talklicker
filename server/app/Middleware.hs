{-# LANGUAGE DataKinds, TypeFamilies #-}

module Middleware (hasSessionHandler, Session(..), HasSession, isAdminHandler, AdminSession(..), IsAdmin) where

import Servant.Server.Experimental.Auth (mkAuthHandler, AuthServerData, AuthHandler)
import Network.Wai (Request, requestHeaders)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad (MonadPlus, mzero)

import qualified Sessions as Sessions
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.List as List
import qualified Database as Database

import Servant
import Types
import Application

--
-- our auth middleware. add the (AuthProtect "session") combinator to
-- a route to protect it with this and prevent access if need be.
--

hasSessionHandler :: AppState -> AuthHandler Request (Session User)
hasSessionHandler = mkAuthHandler . hasSession

data Session a = Session Sessions.Id a
type HasSession = AuthProtect "hasSession"

hasSession :: AppState -> Request -> Handler (Session User)
hasSession (AppState db sessions) req = do

    mSess <- runMaybeT $ do
        sessId'  <- liftMaybe $ List.lookup "Talklicker-Session" (requestHeaders req)
        let sessId = Bytes.unpack sessId'
        sessName <- liftMaybe =<< Sessions.get sessId sessions
        theDb   <- Database.read db
        user    <- liftMaybe $ List.find (\u -> userName u == sessName) (allUsers theDb)
        return (Session sessId user)

    case mSess of
        Just sess -> return sess
        Nothing -> throwError err401

type instance AuthServerData (AuthProtect "hasSession") = Session User


--
-- this middleware additionally checks whether the user session
-- is an admin, on top of the above
--

isAdminHandler :: AppState -> AuthHandler Request (AdminSession User)
isAdminHandler = mkAuthHandler . isAdmin

data AdminSession a = AdminSession Sessions.Id a
type IsAdmin = AuthProtect "isAdmin"

isAdmin :: AppState -> Request -> Handler (AdminSession User)
isAdmin appState req = do

    (Session sessId user) <- hasSession appState req
    if userType user == Admin
        then return (AdminSession sessId user)
        else throwError err401

-- what's returned from our (AuthProtect "session") combinator?
type instance AuthServerData (AuthProtect "isAdmin") = AdminSession User


--
-- Utils
--

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return