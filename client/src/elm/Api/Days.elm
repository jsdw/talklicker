module Api.Days exposing (get, set, remove, add, Day, DayError(..), DayInput, DayAddable)

import Json.Encode as Enc exposing (Value, null)
import Json.Decode as Dec exposing (Decoder, (:=))
import Task exposing (Task)

import Api exposing (..)

--
-- Get days:
--

get : Task Error (List Day)
get = request Get "days" Nothing daysDecoder

type alias Day =
    { id : String
    , title : String
    , description : String
    , entries : List String
    , created : Int
    , modified : Int
    }

daysDecoder : Decoder (List Day)
daysDecoder = Dec.list dayDecoder

dayDecoder : Decoder Day
dayDecoder = Dec.object6 Day
    ("id" := Dec.string)
    ("title" := Dec.string)
    ("description" := Dec.string)
    ("entries" := Dec.list Dec.string)
    ("created" := Dec.int)
    ("modified" := Dec.int)

--
-- Set day:
--

set : DayInput a -> Task DayError Day
set day =
    request Post ("days" :> "set" :> day.id) (Just <| addDayEncoder day) dayDecoder
        `Task.onError` handleError

handleError : Error -> Task DayError a
handleError err = case err of
    ClientError 400 "BAD_TITLE" -> Task.fail DayBadTitle
    ClientError 400 "BAD_DESCRIPTION" -> Task.fail DayBadDescription
    err -> Task.fail (DayBadOther err)

type DayError
    = DayBadTitle
    | DayBadDescription
    | DayBadOther Error

type alias DayInput a =
    { a
    | id : String
    , title : String
    , description : String
    , entries : List String
    }

--
-- Add day:
--

type alias DayAddable a =
    { a
    | title : String
    , description : String
    , entries : List String
    }

add : DayAddable a -> Task DayError Day
add day =
    request Post ("days" :> "add") (Just <| addDayEncoder day) dayDecoder
        `Task.onError` handleError

addDayEncoder : DayAddable a -> Value
addDayEncoder day =
    Enc.object
        [ ("title", Enc.string day.title)
        , ("description", Enc.string day.description)
        , ("entries", Enc.list (List.map Enc.string day.entries)) ]

--
-- Remove day:
--

remove : String -> Task Error ()
remove id = request Delete ("days" :> id) Nothing noResult