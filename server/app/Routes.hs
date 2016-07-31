{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Routes (routes, Routes) where

import Control.Monad.Reader (ask)

import qualified Database
import Database (Database)

import qualified Sessions

import Types
import Servant
import Application


type Routes = GetEntries

routes = getEntries

type GetEntries = AuthProtect "session" :> "entries" :> Get '[JSON] [Entry]

getEntries :: Sessions.Id -> Application [Entry]
getEntries sessId = do
    appState <- ask
    throwError err301
    return []



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