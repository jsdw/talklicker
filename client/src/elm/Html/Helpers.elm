module Html.Helpers exposing (..)

import Html exposing (node, Html)
import Html.Attributes exposing (style)

isJust : Maybe a -> Bool
isJust m = case m of
    Nothing -> False
    Just _ -> True

(?) : Bool -> Html a -> Html a
(?) b html = if b then html else noNode

(!?) : Bool -> Html a -> Html a
(!?) b html = if b then noNode else html

(??) : Maybe m -> Html a -> Html a
(??) m html = if isJust m then html else noNode

(!??) : Maybe m -> Html a -> Html a
(!??) m html = if isJust m then noNode else html

noNode : Html a
noNode = node "nothing" [ style [("position", "absolute"), ("display", "none")] ] []