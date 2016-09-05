module Dnd exposing (Model, model, Msg, update, sub, view)

import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)

import Array

import Mouse
import Json.Decode as JsonDec

--
-- our DnD state
--

type alias Model =
    { selectedId : Maybe String
    , dragPosition : Maybe DragPosition
    , dragInProgress : Bool
    , startXY : Mouse.Position
    , currentXY : Mouse.Position
    }

model : Model
model =
    { selectedId = Nothing
    , dragPosition = Nothing
    , dragInProgress = False
    , startXY = { x = 0, y = 0 }
    , currentXY = { x = 0, y = 0 }
    }

exceedsThreshold : Mouse.Position -> Mouse.Position -> Bool
exceedsThreshold posA posB =
  let
    x = posA.x - posB.x
    y = posA.y - posB.y
  in
    x*x + y*y > (15 ^ 2)

--
-- Update the state accoding to these events:
--

type Msg
    = DragStart String Mouse.Position
    | DragOver (Maybe DragPosition)
    | DragMove Mouse.Position
    | DragComplete

-- a tuple of the things we're between at present,
-- which could be Ids or beginning/end of list.
type alias DragPosition = (Position, Position)

type Position
    = AtBeginning
    | AtId String
    | AtEnd

update : Msg -> Model -> Model
update msg model = case msg of
    DragStart id pos ->
        { model | selectedId = Just id, startXY = pos, currentXY = pos, dragInProgress = False }
    DragOver mDragPos ->
        { model | dragPosition = mDragPos }
    DragMove pos ->
        { model | currentXY = pos, dragInProgress = if model.dragInProgress then True else exceedsThreshold model.startXY pos }
    DragComplete ->
      let
        selectedId = model.selectedId
        dragPosition = model.dragPosition
      in
        cancelDrag model -- notify the outside world here and reset. no dragPosition? failed.

cancelDrag : Model -> Model
cancelDrag model =
    { model | selectedId = Nothing, dragPosition = Nothing, dragInProgress = False }

--
-- Subscribe to mouseUps as necessary to keep DnD state consistent.
--

sub : Model -> Sub Msg
sub model = case model.selectedId of
    Nothing -> Sub.none
    Just _ -> Sub.batch
        [ Mouse.ups (always DragComplete)
        , Mouse.moves DragMove
        ]

--
-- View a list of draggable items paired with IDs we'll use to
-- identify them to the outside world.
--

view : Model -> (Msg -> parentMsg) -> List (String, Html parentMsg) -> Html parentMsg
view model pm items =
  let
    -- drag start occurs
    onDragStart id =
        onWithOptions
            "mousedown"
            { defaultOptions | preventDefault = True }
            (Mouse.position |> JsonDec.map (pm << DragStart id))
    -- the element that the mouse is over has changed
    onDragOver mDragPos =
        onWithOptions
            "mouseenter"
            { defaultOptions | preventDefault = True }
            (JsonDec.succeed (pm <| DragOver mDragPos))
    -- this event finishes the drag successfully when called:
    completeOnMouseUp =
        onWithOptions
            "mouseup"
            { defaultOptions | stopPropagation = True, preventDefault = True }
            (JsonDec.succeed (pm DragComplete))
    -- are we performing a drag at the mo?
    isDrag = case model.selectedId of
        Just _  -> True
        Nothing -> False
    -- wrap a list item so that we start drag onmousedown and react to drops.
    toDndItem lastPos (id,html) nextPos =
      let
        isBeingDragged = Just id == model.selectedId
      in
        div [ class ("dnd-item" ++ if isBeingDragged && model.dragInProgress then " being-dragged" else "")
            , onDragStart id
            ]
            [ html
            -- these will live above the element, covering top half and bottom half when drag is in porogress
            -- and otherwise keeping out of the way. Use them to figure out whether we're dragging above or
            -- below the current thing:
            , div [ class "dnd-item-before"
                  , onDragOver (if isBeingDragged then Nothing else Just (lastPos, AtId id))
                  , completeOnMouseUp
                  ] []
            , div [ class "dnd-item-after"
                  , onDragOver (if isBeingDragged then Nothing else Just (AtId id, nextPos))
                  , completeOnMouseUp
                  ] []
            ]
    -- these live between each item (and at the beginning and end) and expand when the drag is in their
    -- region or collapse otherwise, to make space for the drop.
    toDndSpacer ((a,b) as dragPosition) =
      let
        mDragId = Maybe.map AtId model.selectedId
        isNearDragged = Just a == mDragId || Just b == mDragId
      in
        div [ class
                (  "dnd-spacer"
                ++ (if model.dragPosition == Just dragPosition then " active" else "")
                ++ (if isNearDragged then " near-dragged" else "")
                )
            , onDragOver (Just dragPosition)
            , completeOnMouseUp
            ] []
    -- run through our items, inserting spacers between items, at the beginning and end,
    -- and wrapping items into dndItems:
    dndItems itemArr =
      let
        fn (idx,(itemId, itemHtml)) output =
          let
            lastPos = defaultFromArr (idx-1) (AtId << fst) AtBeginning itemArr
            nextPos = defaultFromArr (idx+1) (AtId << fst) AtEnd itemArr

            item1 rest = if lastPos == AtBeginning then toDndSpacer (AtBeginning,AtId itemId) :: rest else rest
            item2 rest = toDndItem lastPos (itemId, itemHtml) nextPos :: rest
            item3 rest = toDndSpacer (AtId itemId, nextPos) :: rest
          in
            item1 <| item2 <| item3 <| output
      in
        case Array.isEmpty itemArr of
            False -> Array.foldr fn [] (Array.indexedMap (\i a -> (i,a)) itemArr)
            True  -> [toDndSpacer (AtBeginning, AtEnd)]
  in
    -- our output is a dnd-items div wrapping spacer-separated draggable items:
    div [ class ("dnd-items" ++ if isDrag then " active" else "")
        , onMouseLeave (pm (DragOver Nothing))
        ]
        (dndItems (Array.fromList items))

defaultFromArr : Int -> (a -> b) -> b -> Array.Array a -> b
defaultFromArr idx fn def a = case Array.get idx a of
    Nothing -> def
    Just v -> fn v