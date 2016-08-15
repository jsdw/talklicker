import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task
import Debug

--import Material
--import Material.Scheme
--import Material.List as Lists
--import Material.Options as Options exposing (when, css)
--import Material.Icon as Icon

import Api
import Api.Entries as Entries exposing (Entry, EntryType(..))
import Api.Users as Users exposing (User)

--
-- Model
--

model : Model
model =
    { entries = []
    , modals = []
    , error = ""
    , user = Nothing

    -- login modal:
    , loginUserName = ""
    , loginPassword = ""

    -- entry stuff for add/edit:
    , entryId = ""
    , entryUser = ""
    , entryDuration = 3600000
    , entryName = ""
    , entryDescription = ""
    , entryType = Talk
    , entrySaving = False

    --, mdl = Material.model
    }

type alias Model =
    { entries : List Entry
    , modals : List ModalName
    , error : String
    , user : Maybe User

    -- login modal:
    , loginUserName : String
    , loginPassword : String

    -- entry stuff for add/edit:
    , entryId : String
    , entryUser : String
    , entryDuration : Int
    , entryName : String
    , entryDescription : String
    , entryType : EntryType
    , entrySaving : Bool

    -- , mdl : Material.Model

    }

--
-- Update
--

type Msg
    = ApiError Api.Error
    | UpdateEntries (List Entry)
    | CurrentUser (Maybe User)
    | LogOut

    -- login modal:
    | ShowLoginModal
    | LoginUserName String
    | LoginPassword String
    | DoLogin

    -- add/edit entry modal:
    | ShowAddEntryModal
    | ShowEditEntryModal Entry
    | UpdateEntryName String
    | UpdateEntryDescription String
    | UpdateEntryType EntryType
    | UpdateEntryDuration Int
    | DoAddEntry
    | DoneAddEntry Entry
    | DoEditEntry
    | DoneEditEntry Entry

    -- remove entry alert/action
    | ShowRemoveEntryModal Entry
    | DoRemoveEntry Entry
    | DoneRemoveEntry

    | CloseTopModal

    -- | Mdl (Material.Msg Msg)

    | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case Debug.log "update" msg of

    ApiError error ->
        { model | error = toString error } ! []
    CurrentUser mUser ->
        { model | user = mUser } ! []
    UpdateEntries es ->
        { model | entries = es } ! []
    LogOut ->
        { model | user = Nothing } ! []

    -- login modal:
    ShowLoginModal ->
        { model | modals = model.modals ++ [LoginModal] } ! []
    LoginUserName str ->
        { model | loginUserName = str } ! []
    LoginPassword str ->
        { model | loginPassword = str } ! []
    DoLogin ->
        { model | loginUserName = "", loginPassword = "" } ! [doLogin model]

    -- add/edit entry modals:
    ShowAddEntryModal ->
        prepareModelForAddEntry { model | modals = model.modals ++ [AddEntryModal] } ! []
    ShowEditEntryModal entry ->
        prepareModelForEditEntry entry { model | modals = model.modals ++ [EditEntryModal] } ! []
    UpdateEntryName val ->
        { model | entryName = val } ! []
    UpdateEntryDescription val ->
        { model | entryDescription = val } ! []
    UpdateEntryType val ->
        { model | entryType = val } ! []
    UpdateEntryDuration val ->
        { model | entryDuration = val } ! []
    DoAddEntry ->
        { model | entrySaving = True } ! [doAddEntry model]
    DoneAddEntry entry ->
        closeTopModal { model | entries = model.entries ++ [entry], entrySaving = False } ! []
    DoEditEntry ->
        { model | entrySaving = True } ! [doEditEntry model]
    DoneEditEntry entry ->
      let
        entries' = List.map (\e -> if e.id == entry.id then entry else e) model.entries
      in
        closeTopModal { model | entries = entries', entrySaving = False } ! []

    -- remove an entry
    ShowRemoveEntryModal entry ->
        prepareModelForEditEntry entry { model | modals = model.modals ++ [RemoveEntryModal] } ! []
    DoRemoveEntry entry ->
      let
        entries' = List.filter (\e -> e.id /= entry.id) model.entries
      in
        { model | entries = entries' } ! [doRemoveEntry model]
    DoneRemoveEntry ->
        model ! [] -- Do nothing at the mo.
    CloseTopModal ->
        closeTopModal model ! []

    --Mdl msg' ->
    --    Material.update msg' model

    Noop -> (model, Cmd.none)

prepareModelForAddEntry : Model -> Model
prepareModelForAddEntry model =
  let
    doUpdate user =
        { model
        | entryUser = user.name
        , entryDuration = 3600000
        , entryName = ""
        , entryDescription = ""
        , entryType = Talk
        }
  in
    case model.user of
        Nothing -> model
        Just u -> doUpdate u

prepareModelForEditEntry : Entry -> Model -> Model
prepareModelForEditEntry entry model =
    { model
    | entryUser = entry.user
    , entryDuration = entry.duration
    , entryName = entry.name
    , entryDescription = entry.description
    , entryType = entry.entryType
    }

closeTopModal : Model -> Model
closeTopModal model =
  let
    dropEnd l = case l of
        (a :: b :: []) -> a :: [b]
        (a :: b) -> a :: dropEnd b
        [] -> []
  in
    { model
    | modals = dropEnd model.modals
    }

doLogin : Model -> Cmd Msg
doLogin model =
  let
    login = Users.login model.loginUserName model.loginPassword |> Task.map Just
  in
    Task.perform ApiError CurrentUser login

entryishFromModel model =
    { id = model.entryId
    , user = model.entryUser
    , duration = model.entryDuration
    , name = model.entryName
    , description = model.entryDescription
    , entryType = model.entryType
    }

doAddEntry : Model -> Cmd Msg
doAddEntry model =
  let
    entry = entryishFromModel model
  in
    Task.perform ApiError DoneAddEntry (Entries.add entry)

doEditEntry : Model -> Cmd Msg
doEditEntry model =
  let
    entry = entryishFromModel model
  in
    Task.perform ApiError DoneEditEntry (Entries.set entry)

doRemoveEntry : Model -> Cmd Msg
doRemoveEntry model =
  Task.perform ApiError (\_ -> DoneRemoveEntry) (Entries.remove model.entryId)

--
-- View
--

--type alias Mdl =
--  Material.Model

view : Model -> Html Msg
view model =
  let
    isLoggedIn =
        isJust model.user
  in
    div [ class "content" ]
        [ div [ class "top" ]
            [ div [ class "left" ]
                [
                ]
            , div [ class "right" ]
                [ isLoggedIn ?
                    button' [ onClick LogOut, class "logout-button" ]
                        [ text "Log Out" ]
                , not isLoggedIn ?
                    button' [ onClick ShowLoginModal, class "login-button" ]
                        [ text "Log In" ]
                ]
            ]
        , isLoggedIn ?
            div [ class "add-entry-area" ]
                [ button' [ onClick ShowAddEntryModal, class "add-entry-button" ]
                    [ text "Add Entry" ]
                ]
        , div [ class "entries" ] (entries model)
        , div [ class "modals" ] (List.map (showModal model) model.modals)
        , text (toString model.error)
        ]

entries : Model -> List (Html Msg)
entries model =
  let
    entry e = div [ class "entry" ] [ text e.name ]
  in
    List.map entry model.entries

button' : List (Attribute a) -> List (Html a) -> Html a
button' attrs children =
    div ([ class "button" ] ++ attrs) children

showModal : Model -> ModalName -> Html Msg
showModal model modalName =
  let
    makeModal (title, content) =
        div [ class "modal-background" ]
            [ div [ class "modal-inner" ]
                [ div [ class "title" ]
                    [ div [ class "title-inner" ] [ title ]
                    , div [ class "closer", onClick CloseTopModal ] []
                    ]
                , div [ class "content" ]
                    [ content ]
                ]
            ]
  in
    case modalName of
        ErrorModal -> makeModal (errorModal model)
        LoginModal -> makeModal (loginModal model)
        AddEntryModal -> makeModal (addEntryModal model)
        EditEntryModal -> makeModal (editEntryModal model)
        RemoveEntryModal -> makeModal (removeEntryModal model)

type ModalName
    = ErrorModal
    | LoginModal
    | AddEntryModal
    | EditEntryModal
    | RemoveEntryModal

errorModal : Model -> (Html Msg, Html Msg)
errorModal model =
  let
    title =
        text "Error"
    content =
        div [ class "error-modal" ]
            [ text (toString model.error) ]
  in
    (title, content)

loginModal : Model -> (Html Msg, Html Msg)
loginModal model =
  let
    title =
        text "Login"
    content =
        div [ class "login-modal" ]
            [ div [ class "inputs" ]
                [ input [ placeholder "username", onInput LoginUserName, defaultValue model.loginUserName ] []
                , input [ placeholder "password", type' "password", onInput LoginPassword, defaultValue model.loginPassword ] []
                ]
            , div [ class "login-button" ]
                [ button' [ onClick DoLogin ] [ text "Login" ]
                ]
            ]
  in
    (title, content)

addEntryModal : Model -> (Html Msg, Html Msg)
addEntryModal model =
  let
    title =
        text "Add an entry"
    content =
        div [ class "add-entry-modal" ] []
  in
    (title, content)

editEntryModal : Model -> (Html Msg, Html Msg)
editEntryModal model =
  let
    title =
        text ("Edit entry " ++ model.entryName)
    content =
        div [ class "edit-entry-modal" ] []
  in
    (title, content)

removeEntryModal : Model -> (Html Msg, Html Msg)
removeEntryModal model =
  let
    title =
        text ("Remove " ++ model.entryName)
    content =
        div [ class "remove-entry-modal" ] []
  in
    (title, content)

--
-- Main
--

main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \model -> Sub.batch
        [ ]
    }

init : (Model, Cmd Msg)
init =
  let
    currentUser = Task.perform ApiError CurrentUser Users.current
    getEntries = Task.perform ApiError UpdateEntries Entries.get
  in
    (model, Cmd.batch [getEntries, currentUser])

--
-- Utils
--

isJust : Maybe a -> Bool
isJust m = case m of
    Nothing -> False
    Just _ -> True

(?) : Bool -> Html a -> Html a
(?) b html = if b then html else Html.node "nothing" [ style [("position", "absolute"), ("display", "none")] ] []