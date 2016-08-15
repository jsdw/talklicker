module Api.Entries exposing (get, set, remove, add, Entry, EntrySettable, EntryAddable, EntryType(..))

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

--
-- Set entry (edit):
--

set : EntrySettable a -> Task Error Entry
set entry = request Post ("entries" :> entry.id) (Just <| addEntryEncoder entry) entryDecoder

entryTypeEncoder : EntryType -> Value
entryTypeEncoder e = Enc.string (fromEntryType e)

fromEntryType : EntryType -> String
fromEntryType e = case e of
    Talk -> "Talk"
    Project -> "Project"

type alias EntrySettable a =
    { a
    | id : String
    , user : String
    , duration : Int
    , name : String
    , description : String
    , entryType : EntryType
    }

--
-- Remove entry:
--

remove : String -> Task Error ()
remove id = request Delete ("entries" :> id) Nothing noResult

--
-- Add entry:
--

add : EntryAddable a -> Task Error Entry
add entry = request Post "entries" (Just <| addEntryEncoder entry) entryDecoder

addEntryEncoder : EntryAddable a -> Value
addEntryEncoder entry =
    Enc.object
        [ ("user", Enc.string entry.user)
        , ("duration", Enc.int entry.duration)
        , ("name", Enc.string entry.name)
        , ("description", Enc.string entry.description)
        , ("type", entryTypeEncoder entry.entryType) ]

type alias EntryAddable a =
    { a
    | user : String
    , duration : Int
    , name : String
    , description : String
    , entryType : EntryType
    }