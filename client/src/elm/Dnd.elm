module Dnd exposing (Model, model, draggedId, beingDragged, position, delta, Msg, Act(..), Position(..), update, sub, view)

import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)

import Array
import Mouse
import Keyboard
import Json.Decode as JsonDec

--
-- our DnD state
--

-- hide our model's internal state:
type Model = Model TheModel

type alias TheModel =
    { selectedId : Maybe String
    , dragPosition : Maybe DragPosition
    , dragInProgress : Bool
    , startXY : Mouse.Position
    , currentXY : Mouse.Position
    }

model : Model
model = Model
    { selectedId = Nothing
    , dragPosition = Nothing
    , dragInProgress = False
    , startXY = { x = 0, y = 0 }
    , currentXY = { x = 0, y = 0 }
    }

draggedId : Model -> String
draggedId (Model m) = case m.selectedId of
    Nothing -> ""
    Just i -> i

beingDragged : Model -> Bool
beingDragged (Model m) = m.dragInProgress

position : Model -> Mouse.Position
position (Model m) = m.currentXY

delta : Model -> Mouse.Position
delta (Model m) = { x = m.currentXY.x - m.startXY.x, y = m.currentXY.y - m.startXY.y }

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
    | DragCancel
    | Noop

type Act
    = MovedTo String DragPosition

-- a tuple of the things we're between at present,
-- which could be Ids or beginning/end of list.
type alias DragPosition = (Position, Position)

type Position
    = AtBeginning
    | AtId String
    | AtEnd

update : Msg -> Model -> (Model, Maybe Act)
update msg (Model model) = case msg of
    DragStart id pos ->
        { model | selectedId = Just id, startXY = pos, currentXY = pos, dragInProgress = False } !! Nothing
    DragOver mDragPos ->
        { model | dragPosition = mDragPos } !! Nothing
    DragMove pos ->
        { model | currentXY = pos, dragInProgress = if model.dragInProgress then True else exceedsThreshold model.startXY pos } !! Nothing
    DragComplete ->
      let
        act = case (model.selectedId, model.dragPosition) of
            (Just id, Just pos) -> Just (MovedTo id pos)
            _ -> Nothing
      in
        cancelDrag model !! act -- notify the outside world here and reset. no dragPosition? no notify.
    DragCancel ->
        cancelDrag model !! Nothing
    Noop ->
        model !! Nothing

(!!) : TheModel -> Maybe Act -> (Model, Maybe Act)
(!!) model act = (Model model, act)

cancelDrag : TheModel -> TheModel
cancelDrag model =
    { model | selectedId = Nothing, dragPosition = Nothing, dragInProgress = False }

--
-- Subscribe to mouseUps as necessary to keep DnD state consistent.
--

sub : Model -> Sub Msg
sub (Model model) = case model.selectedId of
    Nothing -> Sub.none
    Just _ -> Sub.batch
        [ Mouse.ups (always DragComplete)
        , Mouse.moves DragMove
        , Keyboard.downs (\code -> if code == 27 then DragCancel else Noop) -- listen for escape key to cancel drag
        ]

--
-- View a list of draggable items paired with IDs we'll use to
-- identify them to the outside world.
--

view : Model -> (Msg -> parentMsg) -> List (String, Html parentMsg) -> Html parentMsg
view (Model model) pm items =
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