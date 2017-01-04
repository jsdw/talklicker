module Api.Days exposing (get, set, remove, add, Day, DayError(..), DayInput, DayAddable, DescriptionPart(..), descriptionPartsToString, descriptionStringToParts, descriptionPartsToEntryIds)

import Json.Encode as Enc exposing (Value, null)
import Json.Decode as Dec exposing (Decoder, field)
import Task exposing (Task)
import String
import Api exposing (..)
import Regex


--
-- Get days:
--


get : Task Error (List Day)
get =
    request Get "days" Nothing daysDecoder


type alias Day =
    { id : String
    , title : String
    , description : List DescriptionPart
    , created : Int
    , modified : Int
    }


daysDecoder : Decoder (List Day)
daysDecoder =
    Dec.list dayDecoder


dayDecoder : Decoder Day
dayDecoder =
    Dec.map5 Day
        (field "id" Dec.string)
        (field "title" Dec.string)
        (field "description" (Dec.map descriptionStringToParts Dec.string))
        (field "created" Dec.int)
        (field "modified" Dec.int)



-- day descriptions are read as a list of DayParts, where the part is either
-- some markdown text, or a list of entry IDs


type DescriptionPart
    = MarkdownPart String
    | EntriesPart (List String)



-- description parts can be turned into strings and vice versa.
-- This should always hold:
--   descriptionStringToParts (descriptionPartsToString s) == s


descriptionPartsToString : List DescriptionPart -> String
descriptionPartsToString parts =
    let
        fn =
            \part str ->
                case part of
                    MarkdownPart s ->
                        str ++ s

                    EntriesPart es ->
                        str ++ "@entries" ++ List.foldl (\e s -> s ++ ":" ++ e) "" es
    in
        List.foldl fn "" parts

descriptionStringToParts : String -> List DescriptionPart
descriptionStringToParts str =
    let
        entriesRegex =
            Regex.regex "@entries(:[a-zA-Z0-9]+)*"

        entryMatches =
            Regex.find Regex.All entriesRegex str

        toEntriesPart str =
            str |> String.split ":" |> List.drop 1 |> EntriesPart

        collapse = \m (lastIdx,out) ->
            let
                newIndex = m.index + String.length m.match
                newParts = [MarkdownPart (String.slice lastIdx m.index str), toEntriesPart m.match]
            in
                (newIndex, out ++ newParts)

        parts =
            let
                (lastIdx,out) = List.foldl collapse (0,[]) entryMatches
            in
                out ++ [ MarkdownPart (String.dropLeft lastIdx str) ]

    in
        parts

descriptionPartsToEntryIds : List DescriptionPart -> List String
descriptionPartsToEntryIds parts =
    let
        fn =
            \part out ->
                case part of
                    EntriesPart es ->
                        out ++ es

                    _ ->
                        out
    in
        List.foldl fn [] parts

--
-- Set day:
--


set : DayInput a -> Task DayError Day
set day =
    request Post ("days" :> "set" :> day.id) (Just <| addDayEncoder day) dayDecoder
        |> Task.onError handleError


handleError : Error -> Task DayError a
handleError err =
    case err of
        ClientError 400 "BAD_TITLE" ->
            Task.fail DayBadTitle

        ClientError 400 "BAD_DESCRIPTION" ->
            Task.fail DayBadDescription

        err ->
            Task.fail (DayBadOther err)


type DayError
    = DayBadTitle
    | DayBadDescription
    | DayBadOther Error


type alias DayInput a =
    { a
        | id : String
        , title : String
        , description : List DescriptionPart
    }



--
-- Add day:
--


type alias DayAddable a =
    { a
        | title : String
        , description : List DescriptionPart
    }


add : DayAddable a -> Task DayError Day
add day =
    request Post ("days" :> "add") (Just <| addDayEncoder day) dayDecoder
        |> Task.onError handleError


addDayEncoder : DayAddable a -> Value
addDayEncoder day =
    let
        entryIds =
            descriptionPartsToEntryIds day.description

        description =
            descriptionPartsToString day.description
    in
        Enc.object
            [ ( "title", Enc.string day.title )
            , ( "description", Enc.string description )
            , ( "entries", Enc.list (List.map Enc.string entryIds) )
            ]



--
-- Remove day:
--


remove : String -> Task Error ()
remove id =
    request Delete ("days" :> id) Nothing noResult
