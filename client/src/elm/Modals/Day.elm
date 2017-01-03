module Modals.Day exposing (model, prepareForAdd, prepareForEdit, Model, Msg, Act(..), update, addModal, editModal)

--import Html.App
--import Html.Events exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)
import Task.Extra exposing (perform)
import Material
import Material.Options as Options exposing (when, css, cs)
import Material.Icon as Icon
import Material.Button as Button
import Material.Textfield as Textfield
import Modals
import Api.Days as Days exposing (Day, DayError(..))
import Html.Helpers exposing (..)


--
-- Model
--


type alias Model =
    { id : String
    , title : String
    , description : String
    , saving : Bool
    , error :
        Maybe DayError
        -- is our remove submodal shown?
    , removing : Bool
    , mdl : Material.Model
    }


model : Model
model =
    { id = ""
    , title = ""
    , description = ""
    , saving = False
    , error =
        Nothing
        -- is our remove submodal shown?
    , removing = False
    , mdl = Material.model
    }


prepareForAdd : Model -> Model
prepareForAdd model =
    { model
        | id = ""
        , title = ""
        , description = ""
        , saving = False
        , error = Nothing
        , removing = False
    }


prepareForEdit : Day -> Model -> Model
prepareForEdit day model =
    { model
        | id = day.id
        , title = day.title
        , description = Days.descriptionPartsToString day.description
        , saving = False
        , error = Nothing
        , removing = False
    }



--
-- Update
--
-- messages we handle internally


type Msg
    = UpdateTitle String
    | UpdateDescription String
    | DoAddDay
    | DoEditDay
    | DayFailed DayError
    | AddDaySuccess Day
    | EditDaySuccess Day
    | ShowRemoveModal
    | DoRemove
    | CloseRemove
    | DoneRemove
    | Close
    | Mdl (Material.Msg Msg)



-- messages we pass back to the guy above us.


type Act
    = Added Day
    | Updated Day
    | Removed String
      --day ID
    | CloseMe


update : Msg -> Model -> ( Model, Maybe Act, Cmd Msg )
update msg model =
    case msg of
        UpdateTitle val ->
            { model | title = val } !! []

        UpdateDescription val ->
            { model | description = val } !! []

        DoAddDay ->
            { model | saving = True } !! [ doAddDay model ]

        DoEditDay ->
            { model | saving = True } !! [ doEditDay model ]

        DayFailed err ->
            { model | saving = False, error = Just err } !! []

        AddDaySuccess day ->
            ( { model | saving = False }, Added day ) ^!! []

        EditDaySuccess day ->
            ( { model | saving = False }, Updated day ) ^!! []

        ShowRemoveModal ->
            { model | removing = True } !! []

        DoRemove ->
            model !! [ doRemoveDay model.id ]

        CloseRemove ->
            { model | removing = False } !! []

        DoneRemove ->
            ( model, Removed model.id ) ^!! []

        Close ->
            ( model, CloseMe ) ^!! []

        Mdl msg_ ->
            let
                ( model2, cmd ) =
                    Material.update Mdl msg_ model
            in
                ( model2, Nothing, cmd )


(!!) : Model -> List (Cmd Msg) -> ( Model, Maybe Act, Cmd Msg )
(!!) model msgs =
    ( model, Nothing, Cmd.batch msgs )


(^!!) : ( Model, Act ) -> List (Cmd Msg) -> ( Model, Maybe Act, Cmd Msg )
(^!!) ( model, act ) msgs =
    ( model, Just act, Cmd.batch msgs )


doAddDay : Model -> Cmd Msg
doAddDay model =
    perform DayFailed AddDaySuccess (Days.add { model | description = Days.descriptionStringToParts model.description })


doEditDay : Model -> Cmd Msg
doEditDay model =
    perform DayFailed EditDaySuccess (Days.set { model | description = Days.descriptionStringToParts model.description })


doRemoveDay : String -> Cmd Msg
doRemoveDay dayId =
    perform (always DoneRemove) (always DoneRemove) (Days.remove dayId)



--
-- View
--


addModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
addModal =
    Modals.renderWith
        (\model ->
            { title = text "Add Day"
            , isLoading = model.saving
            , preventClose = False
            , hideClose = False
            , onClose = Close
            , mdl = Mdl
            , cover = []
            , content = dayModalHtml False model
            }
        )


editModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
editModal =
    Modals.renderWith
        (\model ->
            { title = text "Edit Day"
            , isLoading = model.saving
            , preventClose = False
            , hideClose = False
            , onClose = Close
            , mdl = Mdl
            , cover =
                if model.removing then
                    [ removeModal model ]
                else
                    []
            , content = dayModalHtml True model
            }
        )


removeModal : Model -> Html Msg
removeModal model =
    let
        opts =
            { title = "Remove Entry"
            , icon = "warning"
            , message = "Are you sure you want to remove this entry?"
            , onPerform = DoRemove
            , performText = "Remove"
            , onCancel = CloseRemove
            , cancelText = "Cancel"
            , hidePerform = False
            , hideCancel = False
            , mdl = Mdl
            }
    in
        Modals.choice opts model


dayModalHtml : Bool -> Model -> Html Msg
dayModalHtml isEditMode model =
    let
        errorString =
            case model.error of
                Just DayBadTitle ->
                    "Day title required"

                Just DayBadDescription ->
                    "Day description required"

                _ ->
                    "Something eery happened!"
    in
        div [ class "day-modal" ]
            [ table [ class "inputs" ]
                [ inputRow "Title" <|
                    Textfield.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Textfield.label "A Day Title"
                        , Options.onInput UpdateTitle
                        , Textfield.value model.title
                        ]
                        []
                , inputRow "Description" <|
                    Textfield.render Mdl
                        [ 1 ]
                        model.mdl
                        [ Textfield.label "More detail"
                        , Textfield.textarea
                        , Textfield.rows 6
                        , Options.onInput UpdateDescription
                        , Textfield.value model.description
                        ]
                        []
                ]
            , div [ class "bottom-row" ]
                [ Button.render Mdl
                    [ 5 ]
                    model.mdl
                    [ Button.raised
                    , Button.colored
                    , when (model.title == "" || model.description == "") Button.disabled
                    , Options.onClick
                        (if isEditMode then
                            DoEditDay
                         else
                            DoAddDay
                        )
                    , cs "add-day-button"
                    ]
                    [ text
                        (if isEditMode then
                            "Save Changes"
                         else
                            "Add Day"
                        )
                    ]
                , if isJust model.error then
                    div [ class "error" ] [ text errorString ]
                  else
                    div [ class "space-filler" ] []
                , isEditMode
                    ? div [ class "delete" ]
                        [ Button.render Mdl
                            [ 6 ]
                            model.mdl
                            [ Button.icon
                            , Button.ripple
                            , Options.onClick ShowRemoveModal
                            ]
                            [ Icon.i "delete" ]
                        ]
                ]
            ]
