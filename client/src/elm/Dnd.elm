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


type Model listId uniqueId
    = Model (TheModel listId uniqueId)


type alias TheModel listId uniqueId =
    { selectedId : Maybe uniqueId
    , listId : Maybe listId
    , dragPosition : Maybe (DragPosition listId uniqueId)
    , dragInProgress : Bool
    , startXY : Mouse.Position
    , currentXY : Mouse.Position
    }


model : Model listId uniqueId
model =
    Model
        { selectedId = Nothing
        , listId = Nothing
        , dragPosition = Nothing
        , dragInProgress = False
        , startXY = { x = 0, y = 0 }
        , currentXY = { x = 0, y = 0 }
        }


draggedId : Model listId uniqueId -> Maybe uniqueId
draggedId (Model m) =
    m.selectedId


beingDragged : Model listId uniqueId -> Bool
beingDragged (Model m) =
    m.dragInProgress


position : Model listId uniqueId -> Mouse.Position
position (Model m) =
    m.currentXY


delta : Model listId uniqueId -> Mouse.Position
delta (Model m) =
    { x = m.currentXY.x - m.startXY.x, y = m.currentXY.y - m.startXY.y }


exceedsThreshold : Mouse.Position -> Mouse.Position -> Bool
exceedsThreshold posA posB =
    let
        x =
            posA.x - posB.x

        y =
            posA.y - posB.y
    in
        x * x + y * y > (15 ^ 2)



--
-- Update the state accoding to these events:
--


type Msg listId uniqueId
    = DragStart listId uniqueId Mouse.Position
    | DragOver (Maybe (DragPosition listId uniqueId))
    | DragMove Mouse.Position
    | DragComplete
    | DragCancel
    | Noop


type Act listId uniqueId
    = MovedTo ( listId, uniqueId ) (DragPosition listId uniqueId)



-- a tuple of the things we're between at present,
-- which could be Ids or beginning/end of list.


type alias DragPosition listId uniqueId =
    ( listId, Position uniqueId, Position uniqueId )


type Position uniqueId
    = AtBeginning
    | AtId uniqueId
    | AtEnd


update : Msg listId uniqueId -> Model listId uniqueId -> ( Model listId uniqueId, Maybe (Act listId uniqueId) )
update msg (Model model) =
    case msg of
        DragStart listId id pos ->
            { model | listId = Just listId, selectedId = Just id, startXY = pos, currentXY = pos, dragInProgress = False } !! Nothing

        DragOver mDragPos ->
            { model | dragPosition = mDragPos } !! Nothing

        DragMove pos ->
          (if model.listId == Nothing then
            model
          else
            { model
                | currentXY = pos
                , dragInProgress =
                    if model.dragInProgress then
                        True
                    else
                        exceedsThreshold model.startXY pos
            })
                !! Nothing

        DragComplete ->
            let
                act =
                    case ( model.listId, model.selectedId, model.dragPosition ) of
                        ( Just listId, Just id, Just pos ) ->
                            Just (MovedTo ( listId, id ) pos)

                        _ ->
                            Nothing
            in
                cancelDrag model !! act

        -- notify the outside world here and reset. no dragPosition? no notify.
        DragCancel ->
            cancelDrag model !! Nothing

        Noop ->
            model !! Nothing


(!!) : TheModel listId uniqueId -> Maybe (Act listId uniqueId) -> ( Model listId uniqueId, Maybe (Act listId uniqueId) )
(!!) model act =
    ( Model model, act )


cancelDrag : TheModel listId uniqueId -> TheModel listId uniqueId
cancelDrag model =
    { model | listId = Nothing, selectedId = Nothing, dragPosition = Nothing, dragInProgress = False }



--
-- Subscribe to mouseUps as necessary to keep DnD state consistent.
--


sub : Model listId uniqueId -> Sub (Msg listId uniqueId)
sub (Model model) =
    case model.selectedId of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Mouse.ups (always DragComplete)
                , Mouse.moves DragMove
                , Keyboard.downs
                    (\code ->
                        if code == 27 then
                            DragCancel
                        else
                            Noop
                    )
                  -- listen for escape key to cancel drag
                ]



--
-- View a list of draggable items paired with IDs we'll use to
-- identify them to the outside world.
--


type alias Options listId =
    { listId : listId, enabled : Bool }


view : Options listId -> Model listId uniqueId -> (Msg listId uniqueId -> parentMsg) -> List ( uniqueId, Html parentMsg ) -> Html parentMsg
view options (Model model) pm items =
    let
        listId =
            options.listId

        -- drag start occurs
        onDragStart id =
            onWithOptions
                "mousedown"
                { defaultOptions | preventDefault = True }
                (Mouse.position |> JsonDec.map (pm << DragStart listId id))

        -- the element that the mouse is over has changed
        onDragOver mDragPos =
            onWithOptions
                "mouseenter"
                { defaultOptions | preventDefault = True }
                (JsonDec.succeed (pm <| DragOver mDragPos))

        -- wrap a list item so that we start drag onmousedown and react to drops.
        toDndItem lastPos ( id, html ) nextPos =
            let
                mDragId =
                    Maybe.map AtId model.selectedId

                isAboveDragged =
                    Just nextPos == mDragId

                isBelowDragged =
                    Just lastPos == mDragId

                isBeingDragged =
                    Just id == model.selectedId

                isDisabled =
                    not options.enabled

                el =
                    div
                        [ class
                            ("dnd-item"
                                ++ if isBeingDragged && model.dragInProgress then
                                    " being-dragged"
                                   else
                                    ""
                            )
                        , if options.enabled then
                            onDragStart id
                          else
                            noAttr
                        ]
                        [ html
                          -- these will live above the element, covering top half and bottom half when drag is in porogress
                          -- and otherwise keeping out of the way. Use them to figure out whether we're dragging above or
                          -- below the current thing:
                        , div
                            [ class "dnd-item-before"
                            , onDragOver
                                (if isDisabled || isBeingDragged || isBelowDragged then
                                    Nothing
                                 else
                                    Just ( listId, lastPos, AtId id )
                                )
                            ]
                            []
                        , div
                            [ class "dnd-item-after"
                            , onDragOver
                                (if isDisabled || isBeingDragged || isAboveDragged then
                                    Nothing
                                 else
                                    Just ( listId, AtId id, nextPos )
                                )
                            ]
                            []
                        ]
            in
                ( toString id, el )

        -- these live between each item (and at the beginning and end) and expand when the drag is in their
        -- region or collapse otherwise, to make space for the drop.
        toDndSpacer (( listId, a, b ) as dragPosition) =
            let
                mDragId =
                    Maybe.map AtId model.selectedId

                isAboveDragged =
                    Just b == mDragId

                isBelowDragged =
                    Just a == mDragId

                isNearDragged =
                    isAboveDragged || isBelowDragged

                isOnlySpacer =
                    a == AtBeginning && b == AtEnd

                el =
                    div
                        [ class
                            ("dnd-spacer"
                                ++ (if model.dragPosition == Just dragPosition && model.dragInProgress && options.enabled then
                                        " active"
                                    else
                                        ""
                                   )
                                ++ (if isNearDragged then
                                        " near-dragged"
                                    else
                                        ""
                                   )
                                ++ (if isAboveDragged then
                                        " above"
                                    else
                                        ""
                                   )
                                ++ (if isBelowDragged then
                                        " below"
                                    else
                                        ""
                                   )
                                ++ (if isOnlySpacer then
                                        " singleton"
                                    else
                                        ""
                                   )
                            )
                        , onDragOver
                            (if options.enabled then
                                Just dragPosition
                             else
                                Nothing
                            )
                        ]
                        []
            in
                ( toString a ++ toString b, el )

        -- run through our items, inserting spacers between items, at the beginning and end,
        -- and wrapping items into dndItems:
        dndItems itemArr =
            let
                fn ( idx, ( itemId, itemHtml ) ) output =
                    let
                        lastPos =
                            defaultFromArr (idx - 1) (AtId << Tuple.first) AtBeginning itemArr

                        nextPos =
                            defaultFromArr (idx + 1) (AtId << Tuple.first) AtEnd itemArr

                        item1 rest =
                            if lastPos == AtBeginning then
                                toDndSpacer ( listId, AtBeginning, AtId itemId ) :: rest
                            else
                                rest

                        item2 rest =
                            toDndItem lastPos ( itemId, itemHtml ) nextPos :: rest

                        item3 rest =
                            toDndSpacer ( listId, AtId itemId, nextPos ) :: rest
                    in
                        item1 <| item2 <| item3 <| output
            in
                case Array.isEmpty itemArr of
                    False ->
                        Array.foldr fn [] (Array.indexedMap (\i a -> ( i, a )) itemArr)

                    True ->
                        [ toDndSpacer ( listId, AtBeginning, AtEnd ) ]
    in
        -- our output is a dnd-items div wrapping spacer-separated draggable items:
        Keyed.node "div"
            [ class
                ("dnd-items"
                    ++ if model.dragInProgress && options.enabled then
                        " active"
                       else
                        ""
                )
            , onMouseLeave (pm (DragOver Nothing))
            ]
            (dndItems (Array.fromList items))


defaultFromArr : Int -> (a -> b) -> b -> Array.Array a -> b
defaultFromArr idx fn def a =
    case Array.get idx a of
        Nothing ->
            def

        Just v ->
            fn v


noAttr : Attribute msg
noAttr =
    attribute "x" ""
