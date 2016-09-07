module Api.Entries exposing (get, set, remove, add, order, Entry, EntryError(..), EntrySettable, EntryAddable, EntryType(..))

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

set : EntrySettable a -> Task EntryError Entry
set entry =
    request Post ("entries" :> entry.id) (Just <| addEntryEncoder entry) entryDecoder
        `Task.onError` handleError

handleError : Error -> Task EntryError a
handleError err = case err of
    ClientError 400 "BAD_NAME" -> Task.fail EntryBadName
    ClientError 400 "BAD_DESCRIPTION" -> Task.fail EntryBadDescription
    ClientError 400 "BAD_DURATION" -> Task.fail EntryBadDuration
    err -> Task.fail (EntryBadOther err)

entryTypeEncoder : EntryType -> Value
entryTypeEncoder e = Enc.string (fromEntryType e)

fromEntryType : EntryType -> String
fromEntryType e = case e of
    Talk -> "Talk"
    Project -> "Project"

type EntryError
    = EntryBadName
    | EntryBadDescription
    | EntryBadDuration
    | EntryBadOther Error

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

add : EntryAddable a -> Task EntryError Entry
add entry = request Post "entries" (Just <| addEntryEncoder entry) entryDecoder
    `Task.onError` handleError

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

--
-- Set entry order (provide list of entry IDs and they will be reordered)
--

order : List String -> Task Error ()
order ids = request Post ("entries" :> "order" :> "order") (Just <| Enc.list <| List.map Enc.string ids) noResult