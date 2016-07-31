{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Types (User(..), Entry(..), EntryType(..), Day(..), Everything(..), LoginInfo(..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Types (fieldLabelModifier)
import GHC.Generics (Generic)
import Data.Default (Default, def)

--
-- Our core types:
--

data Id = Id String
    deriving (Show, Eq, Generic)

instance ToJSON Id
instance FromJSON Id


data User = User
    { userName :: String
    , userFullName :: String
    , userPassHash :: String
    } deriving (Eq, Show, Generic)

instance ToJSON User where toJSON = toPrefix "user"
instance FromJSON User where parseJSON = fromPrefix "user"


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

instance ToJSON Entry where toJSON = toPrefix "entry"
instance FromJSON Entry where parseJSON = fromPrefix "entry"


data Day = Day
    { dayId     :: Id
    , dayTitle  :: String
    , dayDate   :: Int
    , dayEvents :: [Id]
    } deriving (Eq, Show, Generic)

instance ToJSON Day where toJSON = toPrefix "day"
instance FromJSON Day where parseJSON = fromPrefix "day"


data Everything = Everything
    { allEntries :: [Entry]
    , allUsers   :: [User]
    , allDays    :: [Day]
    } deriving (Eq, Show, Generic)

instance ToJSON Everything where toJSON = toPrefix "all"
instance FromJSON Everything where parseJSON = fromPrefix "all"

instance Default Everything where
    def = Everything [] [] []

--
-- Types for API request/responses.
--

data LoginInfo = LoginInfo
    { loginName :: String
    , loginPass :: String
    } deriving (Eq, Show, Generic)

instance ToJSON LoginInfo where toJSON = toPrefix "login"
instance FromJSON LoginInfo where parseJSON = fromPrefix "login"

--
-- handy JSON generic to/from funcs that strip or add prefixes, fixing
-- case to camelcase:
--
fromPrefix prefix = Aeson.genericParseJSON (Aeson.defaultOptions { fieldLabelModifier = stripPrefix prefix })
toPrefix prefix = Aeson.genericToJSON (Aeson.defaultOptions { fieldLabelModifier = stripPrefix prefix })

stripPrefix :: String -> String -> String
stripPrefix prefix s = case removePrefix prefix s of
    Nothing -> s
    Just [] -> []
    Just (s:ss) -> Char.toLower s : ss

removePrefix :: String -> String -> Maybe String
removePrefix [] str = Just str
removePrefix _ [] = Nothing
removePrefix (l:ls) (s:ss) = if l == s then removePrefix ls ss else Nothing
