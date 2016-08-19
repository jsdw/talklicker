{-# LANGUAGE DataKinds, TypeFamilies #-}

module Middleware (

    maybeHasSessionHandler,
    Session(..),
    MaybeHasSession,

    hasSessionHandler,
    HasSession,

    isAdminHandler,
    AdminSession(..),
    IsAdmin ) where

import Servant.Server.Experimental.Auth (mkAuthHandler, AuthServerData, AuthHandler)
import Network.Wai (Request, requestHeaders)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad (MonadPlus, mzero)
import Data.Map (Map)

import qualified Sessions as Sessions
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.List as List
import qualified Database as Database
import qualified Data.Map as Map

import Servant
import Types
import Application

--
-- Does the user have a session? allows you to the route regardless, but passes
-- a maybe session so that we can then check it.
--

maybeHasSessionHandler :: AppState -> AuthHandler Request (Maybe (Session User))
maybeHasSessionHandler = mkAuthHandler . maybeHasSession

data Session a = Session Sessions.Id a
type MaybeHasSession = AuthProtect "maybeHasSession"

maybeHasSession :: AppState -> Request -> Handler (Maybe (Session User))
maybeHasSession (AppState db sessions) req = do

    mSess <- runMaybeT $ do

        let cookies = parseCookiesFromReq req
        sessId  <- liftMaybe $ Map.lookup "talklicker_session" cookies
        sessName <- liftMaybe =<< Sessions.get sessId sessions
        theDb   <- Database.read db
        user    <- liftMaybe $ List.find (\u -> userName u == sessName) (allUsers theDb)
        return (Session sessId user)

    case mSess of
        Just sess -> return (Just sess)
        Nothing -> return Nothing

type instance AuthServerData (AuthProtect "maybeHasSession") = Session (Maybe (Session User))

--
-- A stricter version of the above - if the usr doesn't have a session the route will fail.
--

hasSessionHandler :: AppState -> AuthHandler Request (Session User)
hasSessionHandler = mkAuthHandler . hasSession

type HasSession = AuthProtect "hasSession"

hasSession :: AppState -> Request -> Handler (Session User)
hasSession appState req = do

    mSess <- maybeHasSession appState req
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

parseCookiesFromReq :: Request -> Map String String
parseCookiesFromReq req = case List.lookup "Cookie" (requestHeaders req) of
    Nothing -> Map.empty
    Just bs -> parseCookies bs

parseCookies :: Bytes.ByteString -> Map String String
parseCookies bs =
  let
    parseOne str = case Bytes.split '=' (Bytes.dropWhile (== ' ') str) of
        (key : val : []) -> Map.singleton (Bytes.unpack key) (Bytes.unpack val)
        _ -> Map.empty
  in
    mconcat $ fmap parseOne (Bytes.split ';' bs)

