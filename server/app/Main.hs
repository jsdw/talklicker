{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Main where

import qualified Data.Map as Map

import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (ReaderT, ask, runReaderT, MonadReader)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Trans (MonadIO, lift)
import Control.Concurrent (newMVar)
import Servant.Server.Experimental.Auth (mkAuthHandler, AuthHandler, AuthServerData)

import qualified Database as Database
import Database (Database)
import qualified Sessions
import Sessions (Sessions)

import Types
import Servant


main :: IO ()
main = do

    appState <- AppState
        <$> Database.init "testFile.json"
        <*> Sessions.empty

    let authHandler (req :: Request) = return ("hello" :: Sessions.Id) -- can access appState here to properly validate req!
    let handlers = enter (appToHandler appState) api
    let server = serveWithContext (Proxy :: Proxy Api) (mkAuthHandler authHandler :. EmptyContext) handlers

    run 8080 server



type Api = GetEntries

api = getEntries

type GetEntries = AuthProtect "session" :> "entries" :> Get '[JSON] [Entry]

getEntries :: Sessions.Id -> Application [Entry]
getEntries sessId = do
    appState <- ask
    throwError err301
    return []



type instance AuthServerData (AuthProtect "session") = Sessions.Id

-- describe how to transform our Application into a servant Handler.
-- this makes it possible for us to use it instead of servants type.
appToHandler :: AppState -> Application :~> Handler
appToHandler appState = Nat $ \r -> runReaderT (unApp r) appState

-- the monad our API will run under.
-- this makes our AppState readable anywhere in the app
-- without having to explicitly pass it about.
newtype Application a = Application { unApp :: ReaderT AppState Handler a }
    deriving (MonadError ServantErr, Functor, Applicative, Monad, MonadReader AppState, MonadIO)

-- this is all of our app state. It's available to
-- our various API calls.
data AppState = AppState
    { database :: Database Everything
    , sessions :: Sessions String
    }




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