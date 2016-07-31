{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Main where

import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Experimental.Auth (mkAuthHandler, AuthServerData)

import qualified Database as Database
import qualified Sessions as Sessions

import Servant
import Routes
import Application


main :: IO ()
main = do

    appState <- AppState
        <$> Database.init "testFile.json"
        <*> Sessions.empty

    let authHandler (req :: Request) = return ("hello" :: Sessions.Id) -- can access appState here to properly validate req!
    let handlers = enter (appToHandler appState) routes
    let server = serveWithContext (Proxy :: Proxy Routes) (mkAuthHandler authHandler :. EmptyContext) handlers

    run 8080 server

-- what's returned from our Auth combinator?
type instance AuthServerData (AuthProtect "session") = Sessions.Id
