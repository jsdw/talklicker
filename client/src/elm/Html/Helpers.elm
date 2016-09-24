module Html.Helpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import String

inputRow : String -> Html a -> Html a
inputRow title html =
  let
    key = String.map (\c -> if c == ' ' then '-' else c) <| String.toLower title
  in
    tr [ class ("input-row input-row-"++key) ]
        [ td [ class ("input-name input-name-"++key) ] [ text title ]
        , td [ class "input-widget" ] [ html ]
        ]

isJust : Maybe a -> Bool
isJust m = case m of
    Nothing -> False
    Just _ -> True

infixl 1 ?
(?) : Bool -> Html a -> Html a
(?) b html = if b then html else noNode

(!?) : Bool -> Html a -> Html a
(!?) b html = if b then noNode else html

(??) : Maybe m -> Html a -> Html a
(??) m html = if isJust m then html else noNode

(!??) : Maybe m -> Html a -> Html a
(!??) m html = if isJust m then noNode else html

noNode : Html a
noNode = text ""