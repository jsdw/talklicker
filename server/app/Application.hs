{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Application where

import Control.Monad.Reader (ReaderT, runReaderT, MonadReader)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO)

import Database
import Sessions
import Servant
import Types

type UserSessions = Sessions String
type UserSession = Session User

-- the monad our API will run under.
-- this makes our AppState readable anywhere in the app
-- without having to explicitly pass it about.
newtype Application a = Application { unApp :: ReaderT AppState Handler a }
    deriving (MonadError ServantErr, Functor, Applicative, Monad, MonadReader AppState, MonadIO)

-- this is all of our app state. It's available to
-- our various API calls.
data AppState = AppState
    { appDatabase :: Database Everything
    , appSessions :: UserSessions
    }

-- describe how to transform our Application into a servant Handler.
-- this makes it possible for us to use it instead of servants type.
appToHandler :: AppState -> Application :~> Handler
appToHandler appState = Nat $ \r -> runReaderT (unApp r) appState