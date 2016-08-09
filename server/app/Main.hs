{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Control.Applicative ((<|>))

import qualified Data.Map as Map
import qualified Database as Database
import qualified Sessions as Sessions
import qualified Args as Args

import Servant
import Routes
import Application
import Middleware

main :: IO ()
main = do

    args <- Args.parsed

    let arg key = Map.lookup key args
    let Just fileName = arg "database" <|> arg "db" <|> arg "d" <|> Just "talklicker.json"
    let Just staticDir = arg "static-files" <|> arg "static" <|> arg "s" <|> Just "static"

    appState <- AppState <$> Database.init fileName <*> Sessions.empty

    let handlers = enter (appToHandler appState) routes :<|> serveDirectory staticDir
    let context = hasSessionHandler appState :. isAdminHandler appState :. EmptyContext
    let server = serveWithContext (Proxy :: Proxy (Routes :<|> Raw)) context handlers

    run 8080 server

