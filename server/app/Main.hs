{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Main where

import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Experimental.Auth (mkAuthHandler, AuthServerData)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad (MonadPlus, mzero)
import System.Environment (getArgs)

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
    let server = serveWithContext (Proxy :: Proxy Routes) (mkAuthHandler (authHandler appState) :. EmptyContext) handlers

    run 8080 server

-- our auth middleware. add the (AuthProtect "session") combinator to
-- a route to protect it with this and prevent access if need be.
authHandler :: AppState -> Request -> Handler (Sessions.Session User)
authHandler (AppState db sessions) req = do

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
type instance AuthServerData (AuthProtect "session") = Sessions.Session User

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return