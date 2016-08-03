{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Types.Types (
    ToJSON(..),
    FromJSON(..),

    Id(..),
    User(..),
    UserType(..),
    Entry(..),
    EntryType(..),
    Day(..),
    Everything(..),

    UserInput(..),
    DayInput(..),
    LoginInput(..),
    UserOutput(..),
    toUserOutput,

    fromPrefix,
    toPrefix) where

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Types (fieldLabelModifier)
import GHC.Generics (Generic)
import Data.Default (Default, def)
import Servant (FromHttpApiData(..))

--
-- Our core types:
--

data Id = Id String
    deriving (Show, Eq, Generic)

instance ToJSON Id
instance FromJSON Id
instance FromHttpApiData Id where parseUrlPiece = Right . Id . Text.unpack

data UserType
    = Admin
    | NormalUser
    deriving (Eq, Show, Generic)

instance ToJSON UserType
instance FromJSON UserType

data User = User
    { userName     :: String
    , userFullName :: String
    , userPassHash :: String
    , userType     :: UserType
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
-- Route specific types
--

data UserInput = UserInput
    { uiFullName :: Maybe String
    , uiPass     :: Maybe String
    } deriving (Show, Eq, Generic)

instance FromJSON UserInput where parseJSON = fromPrefix "ui"

data DayInput = DayInput
    { diId :: Id
    , diTitle :: Maybe String
    , diDate :: Maybe Int
    , diEvents :: Maybe [Id]
    } deriving (Show, Eq, Generic)

instance FromJSON DayInput where parseJSON = fromPrefix "di"

data LoginInput = LoginInput
    { loginName :: String
    , loginPass :: String
    } deriving (Eq, Show, Generic)

instance FromJSON LoginInput where parseJSON = fromPrefix "login"

data UserOutput = UserOutput
    { _uoUserName :: String
    , _uoUserFullName :: String
    } deriving (Eq, Show, Generic)

instance ToJSON UserOutput where toJSON = toPrefix "_uoUser"

toUserOutput :: User -> UserOutput
toUserOutput User{..} = UserOutput userName userFullName

--
-- handy JSON generic to/from funcs that strip prefixes, fixing
-- case to camelcase:
--
fromPrefix prefix = Aeson.genericParseJSON (Aeson.defaultOptions { fieldLabelModifier = stripPrefix prefix })
toPrefix prefix = Aeson.genericToJSON (Aeson.defaultOptions { fieldLabelModifier = stripPrefix prefix })

stripPrefix :: String -> String -> String
stripPrefix prefix fieldName = case removePrefix prefix fieldName of
    Nothing -> fieldName
    Just [] -> []
    Just (s:ss) -> Char.toLower s : ss

removePrefix :: String -> String -> Maybe String
removePrefix [] str = Just str
removePrefix _ [] = Nothing
removePrefix (l:ls) (s:ss) = if l == s then removePrefix ls ss else Nothing
