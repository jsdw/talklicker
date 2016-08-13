module Api.Entries exposing (get, Entry, EntryType(..))

import Json.Encode as Enc exposing (Value, null)
import Json.Decode as Dec exposing (Decoder, (:=))
import Task exposing (Task)
import String

import Api exposing (..)

--
-- Get entries:
--

get : Task Error (List Entry)
get = request Get "entries" Nothing entriesDecoder

type alias Entry =
    { id : String
    , user : String
    , duration : Int
    , name : String
    , description : String
    , entryType : EntryType
    , created : Int
    , modified : Int
    }

type EntryType = Talk | Project

entriesDecoder : Decoder (List Entry)
entriesDecoder = Dec.list entryDecoder

entryDecoder : Decoder Entry
entryDecoder = Dec.object8 Entry
    ("id" := Dec.string)
    ("user" := Dec.string)
    ("duration" := Dec.int)
    ("name" := Dec.string)
    ("description" := Dec.string)
    ("type" := entryTypeDecoder)
    ("created" := Dec.int)
    ("modified" := Dec.int)

entryTypeDecoder : Decoder EntryType
entryTypeDecoder = Dec.customDecoder Dec.string toEntryType

toEntryType : String -> Result String EntryType
toEntryType str =
    case String.toLower str of
        "talk" -> Ok Talk
        "project" -> Ok Project
        _ -> Err (str++" is not a valid EntryType")