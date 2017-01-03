module Api.Entries exposing (get, set, remove, add, move, order, Entry, EntryError(..), EntryPosition(..), EntrySettable, EntryAddable, EntryType(..))

import Json.Encode as Enc exposing (Value, null)
import Json.Decode as Dec exposing (Decoder, field)
import Task exposing (Task)
import String
import Api exposing (..)


--
-- Get entries:
--


get : Task Error (List Entry)
get =
    request Get "entries" Nothing entriesDecoder


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


type EntryType
    = Talk
    | Project


entriesDecoder : Decoder (List Entry)
entriesDecoder =
    Dec.list entryDecoder


entryDecoder : Decoder Entry
entryDecoder =
    Dec.map8 Entry
        (field "id" Dec.string)
        (field "user" Dec.string)
        (field "duration" Dec.int)
        (field "name" Dec.string)
        (field "description" Dec.string)
        (field "type" entryTypeDecoder)
        (field "created" Dec.int)
        (field "modified" Dec.int)


entryTypeDecoder : Decoder EntryType
entryTypeDecoder =
    Dec.string |>
        Dec.andThen toEntryType

toEntryType : String -> Decoder EntryType
toEntryType str =
    case String.toLower str of
        "talk" ->
            Dec.succeed Talk

        "project" ->
            Dec.succeed Project

        _ ->
            Dec.fail (str ++ " is not a valid EntryType")



--
-- Set entry (edit):
--


set : EntrySettable a -> Task EntryError Entry
set entry =
    request Post ("entries" :> "set" :> entry.id) (Just <| addEntryEncoder entry) entryDecoder
        |> Task.onError handleError


handleError : Error -> Task EntryError a
handleError err =
    case err of
        ClientError 400 "BAD_NAME" ->
            Task.fail EntryBadName

        ClientError 400 "BAD_DESCRIPTION" ->
            Task.fail EntryBadDescription

        ClientError 400 "BAD_DURATION" ->
            Task.fail EntryBadDuration

        err ->
            Task.fail (EntryBadOther err)


entryTypeEncoder : EntryType -> Value
entryTypeEncoder e =
    Enc.string (fromEntryType e)


fromEntryType : EntryType -> String
fromEntryType e =
    case e of
        Talk ->
            "Talk"

        Project ->
            "Project"


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
remove id =
    request Delete ("entries" :> id) Nothing noResult



--
-- Add entry:
--


add : EntryAddable a -> Task EntryError Entry
add entry =
    request Post ("entries" :> "add") (Just <| addEntryEncoder entry) entryDecoder
        |> Task.onError handleError


addEntryEncoder : EntryAddable a -> Value
addEntryEncoder entry =
    Enc.object
        [ ( "user", Enc.string entry.user )
        , ( "duration", Enc.int entry.duration )
        , ( "name", Enc.string entry.name )
        , ( "description", Enc.string entry.description )
        , ( "type", entryTypeEncoder entry.entryType )
        ]


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


type EntryPosition
    = AtBefore String
    | AtAfter String
    | AtBeginning
    | AtEnd


posToString : EntryPosition -> ( String, String )
posToString pos =
    case pos of
        AtAfter s ->
            ( "after", s )

        AtBefore s ->
            ( "before", s )

        AtEnd ->
            ( "before", "end" )

        AtBeginning ->
            ( "after", "beginning" )


move : String -> EntryPosition -> Task Error ()
move id pos =
    let
        ( rel, relId ) =
            posToString pos
    in
        request Post ("entries" :> "move" :> id :> rel :> relId) Nothing noResult


order : List String -> Task Error ()
order ids =
    request Post ("entries" :> "order") (Just <| Enc.list <| List.map Enc.string ids) noResult
