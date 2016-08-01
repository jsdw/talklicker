{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Main where

import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Experimental.Auth (mkAuthHandler, AuthServerData)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad (MonadPlus, mzero)
import System.Environment (getArgs)
import Sessions (Session(Session), AdminSession(AdminSession))

import qualified Data.ByteString.Char8 as Bytes
import qualified Data.List as List
import qualified Database as Database
import qualified Sessions as Sessions

import Servant
import Routes
import Application
import Types


main :: IO ()
main = do

    -- takes exactly ONE argument - the filename to write the DB to:
    [fileName] <- getArgs
    appState <- AppState <$> Database.init fileName <*> Sessions.empty

    let handlers = enter (appToHandler appState) routes
    let context = mkAuthHandler (isAdmin appState) :. mkAuthHandler (hasSession appState) :. EmptyContext
    let server = serveWithContext (Proxy :: Proxy Routes) context handlers

    run 8080 server

--
-- A couple of bits of middleware to endow a couple of AuthProtect
-- combinators with custom functionality
--

-- our auth middleware. add the (AuthProtect "session") combinator to
-- a route to protect it with this and prevent access if need be.
hasSession :: AppState -> Request -> Handler (Session User)
hasSession (AppState db sessions) req = do

    mSess <- runMaybeT $ do
        sessId  <- liftMaybe $ List.lookup "Talklicker-Session" (requestHeaders req)
        sess    <- liftMaybe =<< Sessions.get (Bytes.unpack sessId) sessions
        theDb   <- Database.read db
        user    <- liftMaybe $ List.find (\u -> userName u == Sessions.content sess) (allUsers theDb)
        return (Sessions.Session (Sessions.id sess) user)

    case mSess of
        Just sess -> return sess
        Nothing -> throwError err401

-- what's returned from our (AuthProtect "session") combinator?
type instance AuthServerData (AuthProtect "hasSession") = Session User

-- this middleware additionally checks whether the user session
-- is an admin, on top of the above
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