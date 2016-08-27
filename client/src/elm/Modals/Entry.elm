module Modals.Entry exposing (model, prepareForAdd, prepareForEdit, Model, Msg, Act(..), update, addModal, editModal)

--import Html.App
--import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task

import Material
import Material.Options as Options exposing (when, css, cs)
import Material.Icon as Icon
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Menu as Menu

import Modals

import Api.Entries as Entries exposing (Entry, EntryType(..), EntryError(..))
import Api.Users as Users exposing (User, UserType(..), LoginError(..))
import Html.Helpers exposing (..)

--
-- Model
--

type alias Model =
    { entryId : String
    , entryUser : String
    , entryDuration : Int
    , entryName : String
    , entryDescription : String
    , entryType : EntryType
    , entrySaving : Bool
    , entryError : Maybe EntryError

    -- is our remove submodal shown?
    , entryRemoving : Bool

    , mdl : Material.Model
    }

model : Model
model =
    { entryId = ""
    , entryUser = ""
    , entryDuration = 3600000
    , entryName = ""
    , entryDescription = ""
    , entryType = Talk
    , entrySaving = False
    , entryError = Nothing

    , entryRemoving = False

    , mdl = Material.model
    }

prepareForAdd : User -> Model -> Model
prepareForAdd user model =
    { model
    | entryUser = user.name
    , entryDuration = 3600000
    , entryName = ""
    , entryDescription = ""
    , entryType = Talk
    , entryError = Nothing
    , entryRemoving = False
    }

prepareForEdit : Entry -> Model -> Model
prepareForEdit entry model =
    { model
    | entryId = entry.id
    , entryUser = entry.user
    , entryDuration = entry.duration
    , entryName = entry.name
    , entryDescription = entry.description
    , entryType = entry.entryType
    , entryError = Nothing
    , entryRemoving = False
    }

--
-- Update
--

-- messages we handle internally
type Msg
    = RemoveModal
    | UpdateEntryName String
    | UpdateEntryDescription String
    | UpdateEntryType EntryType
    | UpdateEntryDuration Int
    | DoAddEntry
    | AddEntryFailed EntryError
    | AddEntrySuccess Entry
    | DoEditEntry
    | EditEntryFailed EntryError
    | EditEntrySuccess Entry
    | Close

    | DoRemoveEntry
    | CloseRemoveEntry
    | DoneRemoveEntry

    | Mdl (Material.Msg Msg)

-- messages we pass back to the guy above us.
type Act
    = Added Entry
    | Updated Entry
    | Removed String --entry ID
    | CloseMe

update : Msg -> Model -> (Model, Maybe Act, Cmd Msg)
update msg model = case msg of
    RemoveModal ->
        { model | entryRemoving = True } !! []
    UpdateEntryName val ->
        { model | entryName = val } !! []
    UpdateEntryDescription val ->
        { model | entryDescription = val } !! []
    UpdateEntryType val ->
        { model | entryType = val } !! []
    UpdateEntryDuration val ->
        { model | entryDuration = val } !! []
    DoAddEntry ->
        { model | entrySaving = True } !! [doAddEntry model]
    AddEntrySuccess entry ->
        ({ model | entrySaving = False }, Added entry) ^!! []
    AddEntryFailed err ->
        { model | entrySaving = False, entryError = Just err } !! []
    DoEditEntry ->
        { model | entrySaving = True } !! [doEditEntry model]
    EditEntrySuccess entry ->
        ({ model | entrySaving = False }, Updated entry) ^!! []
    EditEntryFailed err ->
        { model | entrySaving = False, entryError = Just err } !! []
    CloseRemoveEntry ->
        { model | entryRemoving = False } !! []
    Close ->
        (model, CloseMe) ^!! []
    DoRemoveEntry ->
        (model, Removed model.entryId) ^!! [doRemoveEntry model.entryId]
    DoneRemoveEntry ->
        model !! [] -- do nothing at the mo.
    Mdl msg' ->
        let (model,cmd) = Material.update msg' model
        in (model, Nothing, cmd)

(!!) : Model -> List (Cmd Msg) -> (Model, Maybe Act, Cmd Msg)
(!!) model msgs = (model, Nothing, Cmd.batch msgs)

(^!!) : (Model, Act) -> List (Cmd Msg) -> (Model, Maybe Act, Cmd Msg)
(^!!) (model,act) msgs = (model, Just act, Cmd.batch msgs)

doAddEntry : Model -> Cmd Msg
doAddEntry model =
  let
    entry = entryishFromModel model
  in
    Task.perform AddEntryFailed AddEntrySuccess (Entries.add entry)

doEditEntry : Model -> Cmd Msg
doEditEntry model =
  let
    entry = entryishFromModel model
  in
    Task.perform EditEntryFailed EditEntrySuccess (Entries.set entry)

doRemoveEntry : String -> Cmd Msg
doRemoveEntry entryId =
  Task.perform (always DoneRemoveEntry) (always DoneRemoveEntry) (Entries.remove entryId)

entryishFromModel : Model -> Entries.EntrySettable {}
entryishFromModel model =
    { id = model.entryId
    , user = model.entryUser
    , duration = model.entryDuration
    , name = model.entryName
    , description = model.entryDescription
    , entryType = model.entryType
    }

--
-- View
--

addModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
addModal = Modals.renderWith (\model ->
    { title = text "Add Entry"
    , isLoading = model.entrySaving
    , preventClose = False
    , hideClose = False
    , onClose = Close
    , mdl = Mdl
    , cover = []
    , content = entryModalHtml False model
    })

editModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
editModal = Modals.renderWith (\model ->
    { title = text "Edit Entry"
    , isLoading = model.entrySaving
    , preventClose = False
    , hideClose = False
    , onClose = Close
    , mdl = Mdl
    , cover = if model.entryRemoving then [ removeModal model ] else []
    , content = entryModalHtml True model
    })

removeModal : Model -> Html Msg
removeModal model =
  let
    opts =
        { title = "Remove Entry"
        , icon = "warning"
        , message = "Are you sure you want to remove this entry?"
        , onPerform = DoRemoveEntry
        , performText = "Remove"
        , onCancel = CloseRemoveEntry
        , cancelText = "Cancel"
        , hidePerform = False
        , hideCancel = False
        , mdl = Mdl
        }
  in
    Modals.choice opts model

entryModalHtml : Bool -> Model -> Html Msg
entryModalHtml isEditMode model =
  let
    errorString = case model.entryError of
        Just EntryBadName -> "Entry name required"
        Just EntryBadDescription -> "Entry description required"
        Just EntryBadDuration -> "Entry duration required"
        _ -> "Something unsettling happened!"
    durationString =
        toString (toFloat model.entryDuration / 3600000) ++ "h"
  in
    div [ class "entry-modal" ]
        [ table [ class "inputs" ]
            [ inputRow "Title" <|
                Textfield.render Mdl [0] model.mdl
                    [ Textfield.label "My talk or project"
                    , Textfield.onInput UpdateEntryName
                    , Textfield.value model.entryName
                    ]
            , inputRow "Description" <|
                Textfield.render Mdl [1] model.mdl
                    [ Textfield.label "More detail"
                    , Textfield.textarea
                    , Textfield.rows 6
                    , Textfield.onInput UpdateEntryDescription
                    , Textfield.value model.entryDescription
                    ]
            , inputRow "Type" <|
                div [ class "type-inputs" ]
                    [ Toggles.radio Mdl [2] model.mdl
                        [ Toggles.value (model.entryType == Talk)
                        , Toggles.group "EntryType"
                        , Toggles.ripple
                        , Toggles.onClick (UpdateEntryType Talk)
                        ]
                        [ text "Talk" ]
                    , Toggles.radio Mdl [3] model.mdl
                        [ Toggles.value (model.entryType == Project)
                        , Toggles.group "EntryType"
                        , Toggles.ripple
                        , Toggles.onClick (UpdateEntryType Project)
                        ]
                        [ text "Project" ]
                    ]
            , inputRow "Duration" <|
                div [ class "duration-input" ]
                    [ text durationString
                    , Menu.render Mdl [4] model.mdl
                        [ Menu.ripple, Menu.bottomRight ]
                        [ Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (1 * 1800000)) ]
                            [ text "30 minutes" ]
                        , Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (1 * 3600000)) ]
                            [ text "1 hour" ]
                        , Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (2 * 3600000)) ]
                            [ text "2 hours" ]
                        , Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (4 * 3600000)) ]
                            [ text "4 hours" ]
                        , Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (1 * 8 * 3600000)) ]
                            [ text "1 day" ]
                        , Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (2 * 8 * 3600000)) ]
                            [ text "2 days" ]
                        ]
                    ]
            ]
        , div [ class "bottom-row" ]
            [ Button.render Mdl [5] model.mdl
                [ Button.raised
                , Button.colored
                , Button.disabled `when` (model.entryName == "" || model.entryDescription == "")
                , Button.onClick (if isEditMode then DoEditEntry else DoAddEntry)
                , cs "add-entry-button"
                ]
                [ text (if isEditMode then "Save Changes" else "Add Entry") ]
            , if isJust model.entryError
                then div [ class "error" ] [ text errorString ]
                else div [ class "space-filler" ] []
            , isEditMode ?
                div [ class "delete" ]
                    [ Button.render Mdl [6] model.mdl
                        [ Button.icon
                        , Button.ripple
                        , Button.onClick RemoveModal
                        ]
                        [ Icon.i "delete"]
                    ]
            ]
        ]