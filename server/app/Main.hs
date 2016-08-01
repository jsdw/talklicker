{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Main where

import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)

import qualified Database as Database
import qualified Sessions as Sessions

import Servant
import Routes
import Application
import Middleware


main :: IO ()
main = do

    -- takes exactly ONE argument - the filename to write the DB to:
    [fileName] <- getArgs
    appState <- AppState <$> Database.init fileName <*> Sessions.empty

    let handlers = enter (appToHandler appState) routes
    let context = hasSessionHandler appState :. isAdminHandler appState :. EmptyContext
    let server = serveWithContext (Proxy :: Proxy Routes) context handlers

    run 8080 server
