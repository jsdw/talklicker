module Dnd exposing (Model, model, draggedId, beingDragged, position, delta, Msg, Act(..), Position(..), update, sub, view)

import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)

import Array
import Mouse
import Keyboard
import Json.Decode as JsonDec
import Html.Keyed as Keyed

--
-- our DnD state
--

-- hide our model's internal state:
type Model uniqueId = Model (TheModel uniqueId)

type alias TheModel uniqueId =
    { selectedId : Maybe uniqueId
    , dragPosition : Maybe (DragPosition uniqueId)
    , dragInProgress : Bool
    , startXY : Mouse.Position
    , currentXY : Mouse.Position
    }

model : Model uniqueId
model = Model
    { selectedId = Nothing
    , dragPosition = Nothing
    , dragInProgress = False
    , startXY = { x = 0, y = 0 }
    , currentXY = { x = 0, y = 0 }
    }

draggedId : Model uniqueId -> Maybe uniqueId
draggedId (Model m) = m.selectedId

beingDragged : Model uniqueId -> Bool
beingDragged (Model m) = m.dragInProgress

position : Model uniqueId -> Mouse.Position
position (Model m) = m.currentXY

delta : Model uniqueId -> Mouse.Position
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

type Msg uniqueId
    = DragStart uniqueId Mouse.Position
    | DragOver (Maybe (DragPosition uniqueId))
    | DragMove Mouse.Position
    | DragComplete
    | DragCancel
    | Noop

type Act uniqueId
    = MovedTo uniqueId (DragPosition uniqueId)

-- a tuple of the things we're between at present,
-- which could be Ids or beginning/end of list.
type alias DragPosition uniqueId = (Position uniqueId, Position uniqueId)

type Position uniqueId
    = AtBeginning
    | AtId uniqueId
    | AtEnd


update : Msg uniqueId -> Model uniqueId -> (Model uniqueId, Maybe (Act uniqueId))
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

(!!) : TheModel uniqueId -> Maybe (Act uniqueId) -> (Model uniqueId, Maybe (Act uniqueId))
(!!) model act = (Model model, act)

cancelDrag : TheModel uniqueId -> TheModel uniqueId
cancelDrag model =
    { model | selectedId = Nothing, dragPosition = Nothing, dragInProgress = False }

--
-- Subscribe to mouseUps as necessary to keep DnD state consistent.
--

sub : Model uniqueId -> Sub (Msg uniqueId)
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

type alias Options = Bool

view : Options -> Model uniqueId -> (Msg uniqueId -> parentMsg) -> List (uniqueId, Html parentMsg) -> Html parentMsg
view allowDnd (Model model) pm items =
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
    -- wrap a list item so that we start drag onmousedown and react to drops.
    toDndItem lastPos (id,html) nextPos =
      let
        mDragId = Maybe.map AtId model.selectedId
        isAboveDragged = Just nextPos == mDragId
        isBelowDragged = Just lastPos == mDragId
        isBeingDragged = Just id == model.selectedId
        el =
            div [ class ("dnd-item" ++ if isBeingDragged && model.dragInProgress then " being-dragged" else "")
                , if allowDnd then onDragStart id else noAttr
                ]
                [ html
                -- these will live above the element, covering top half and bottom half when drag is in porogress
                -- and otherwise keeping out of the way. Use them to figure out whether we're dragging above or
                -- below the current thing:
                , div [ class "dnd-item-before"
                    , onDragOver (if isBeingDragged || isBelowDragged then Nothing else Just (lastPos, AtId id))
                    ] []
                , div [ class "dnd-item-after"
                    , onDragOver (if isBeingDragged || isAboveDragged then Nothing else Just (AtId id, nextPos))
                    ] []
                ]
      in
        (toString id, el)
    -- these live between each item (and at the beginning and end) and expand when the drag is in their
    -- region or collapse otherwise, to make space for the drop.
    toDndSpacer ((a,b) as dragPosition) =
      let
        mDragId = Maybe.map AtId model.selectedId
        isAboveDragged = Just b == mDragId
        isBelowDragged = Just a == mDragId
        isNearDragged = isAboveDragged || isBelowDragged
        el =
            div [ class
                    (  "dnd-spacer"
                    ++ (if model.dragPosition == Just dragPosition && model.dragInProgress then " active" else "")
                    ++ (if isNearDragged then " near-dragged" else "")
                    ++ (if isAboveDragged then " above" else "")
                    ++ (if isBelowDragged then " below" else "")
                    )
                , onDragOver (Just dragPosition)
                ] []
      in
        (toString a ++ toString b, el)

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
    Keyed.node "div"
        [ class ("dnd-items" ++ if model.dragInProgress then " active" else "")
        , onMouseLeave (pm (DragOver Nothing))
        ]
        (dndItems (Array.fromList items))

defaultFromArr : Int -> (a -> b) -> b -> Array.Array a -> b
defaultFromArr idx fn def a = case Array.get idx a of
    Nothing -> def
    Just v -> fn v

noAttr : Attribute msg
noAttr = attribute "x" ""