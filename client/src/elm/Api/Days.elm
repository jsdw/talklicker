module Api.Days exposing (get, set, remove, add, Day, DayError(..), DayInput, DayAddable, DescriptionPart(..), descriptionPartsToString, descriptionStringToParts, descriptionPartsToEntryIds)

import Json.Encode as Enc exposing (Value, null)
import Json.Decode as Dec exposing (Decoder, field)
import Task exposing (Task)
import String
import Char
import Parser exposing (Parser)
import Api exposing (..)


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
    case Parser.parse partParser str of
        Ok res ->
            res

        Err _ ->
            []


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



-- parse a string into a list of DescriptionParts. A little messy but
-- does the trick (lack of do sugar shows here..)!


partParser : Parser (List DescriptionPart)
partParser =
    let
        entries =
            let
                toString =
                    \charList -> List.foldr (\c s -> String.cons c s) "" charList

                sep =
                    Parser.symbol ':'

                entryId =
                    Parser.some (Parser.satisfy (\c -> Char.isUpper c || Char.isLower c || Char.isDigit c)) |> Parser.map toString

                entryIds =
                    Parser.separatedBy entryId sep

                hasEntries =
                    Parser.map EntriesPart (sep |> Parser.andThen (\_ -> entryIds))
            in
                Parser.token "@entries" |> Parser.andThen (\_ -> Parser.optional hasEntries (EntriesPart []))

        loop str bits =
            let
                next =
                    \s bit -> Parser.recursively (\() -> loop s bit)

                foundEntries =
                    entries |> Parser.andThen (\es -> next "" (bits ++ [ MarkdownPart str, es ]))

                finishedSoReturnMarkdown =
                    Parser.end |> Parser.map (\_ -> bits ++ [ MarkdownPart str ])

                takeCharAndLoop =
                    Parser.satisfy (always True) |> Parser.andThen (\c -> next (str ++ String.fromChar c) bits)
            in
                Parser.choice [ foundEntries, finishedSoReturnMarkdown, takeCharAndLoop ]
    in
        loop "" []



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
