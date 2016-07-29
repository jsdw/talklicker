{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Types (User(..), Entry(..), EntryType(..), Day(..), Everything(..), AppState(..)) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Control.Concurrent (MVar)
import Database (Database)
import Data.Map (Map)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)

data Id = Id String
    deriving (Show, Eq, Generic)

instance ToJSON Id
instance FromJSON Id

data User = User
    { userName :: String
    , fullName :: String
    , passHash :: String
    } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

data EntryType
    = Talk
    | Project
    deriving (Eq, Show, Generic)

instance ToJSON EntryType
instance FromJSON EntryType

data Entry = Entry
    { entryId          :: Id
    , entryDuration    :: Int
    , entryName        :: String
    , entryDescription :: String
    , entryType        :: EntryType
    , entryCreated     :: Int
    , entryModified    :: Int
    , entryOrder       :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON Entry
instance FromJSON Entry

data Day = Day
    { dayId     :: Id
    , dayTitle  :: String
    , dayDate   :: Int
    , dayEvents :: [Id]
    } deriving (Eq, Show, Generic)

instance ToJSON Day
instance FromJSON Day

data Everything = Everything
    { allEntries :: [Entry]
    , allUsers   :: [User]
    , allDays    :: [Day]
    } deriving (Eq, Show, Generic)

instance ToJSON Everything
instance FromJSON Everything

-- this is all of our app state. It's available to
-- our various API calls.
data AppState = AppState
    { database :: Database Everything
    , sessions :: MVar (Map String String)
    }
