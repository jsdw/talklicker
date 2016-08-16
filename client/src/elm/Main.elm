import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task
import Debug

import Material
--import Material.Scheme
--import Material.List as Lists
import Material.Options as Options exposing (when, css)
import Material.Icon as Icon
import Material.Button as Button
import Material.Textfield as Textfield

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
    , loggingIn = False
    , loginFailed = False

    -- entry stuff for add/edit:
    , entryId = ""
    , entryUser = ""
    , entryDuration = 3600000
    , entryName = ""
    , entryDescription = ""
    , entryType = Talk
    , entrySaving = False

    , mdl = Material.model
    }

type alias Model =
    { entries : List Entry
    , modals : List ModalName
    , error : String
    , user : Maybe User

    -- login modal:
    , loginUserName : String
    , loginPassword : String
    , loggingIn : Bool
    , loginFailed : Bool

    -- entry stuff for add/edit:
    , entryId : String
    , entryUser : String
    , entryDuration : Int
    , entryName : String
    , entryDescription : String
    , entryType : EntryType
    , entrySaving : Bool

    , mdl : Material.Model

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
    | LoginFailed
    | LoginSuccess User
    | ClearLoginDetails

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

    -- perform several actions at once
    | All (List Msg)

    | CloseTopModal

    | Mdl (Material.Msg Msg)

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
        { model | loggingIn = True } ! [doLogin model]
    LoginFailed ->
        { model | loggingIn = False, loginFailed = True } ! []
    LoginSuccess user ->
        (resetLoginState <| closeTopModal { model | user = Just user }) ! []
    ClearLoginDetails ->
        resetLoginState model ! []

    -- add/edit entry modals:
    ShowAddEntryModal ->
        prepareModelForAddEntry (showModal model AddEntryModal) ! []
    ShowEditEntryModal entry ->
        prepareModelForEditEntry entry (showModal model EditEntryModal) ! []
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
        prepareModelForEditEntry entry (showModal model RemoveEntryModal) ! []
    DoRemoveEntry entry ->
      let
        entries' = List.filter (\e -> e.id /= entry.id) model.entries
      in
        { model | entries = entries' } ! [doRemoveEntry model]
    DoneRemoveEntry ->
        model ! [] -- Do nothing at the mo.
    CloseTopModal ->
        closeTopModal model ! []

    -- perform several actions eg cloing modal and logging in.
    -- done one after the other.
    All ms ->
      let
        folder msg (model,cmds) =
          let (newModel, cmd) = update msg model
          in (newModel, cmd :: cmds)
        (newModel, cmds) = List.foldl folder (model,[]) ms
      in
        (newModel, Cmd.batch cmds)


    Mdl msg' ->
        Material.update msg' model

    Noop ->
        (model, Cmd.none)

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

showModal : Model -> ModalName -> Model
showModal model modal =
    { model | modals = model.modals ++ [modal] }

closeTopModal : Model -> Model
closeTopModal model =
  let
    dropEnd l = case l of
        (b :: []) -> []
        (a :: b) -> a :: dropEnd b
        [] -> []
  in
    { model
    | modals = dropEnd model.modals
    }

doLogin : Model -> Cmd Msg
doLogin model =
  let
    login = Users.login model.loginUserName model.loginPassword
  in
    Task.perform (\_ -> LoginFailed) LoginSuccess login

resetLoginState : Model -> Model
resetLoginState model =
    { model
    | loginUserName = ""
    , loginPassword = ""
    , loginFailed = False
    , loggingIn = False
    }

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

type alias Mdl =
  Material.Model

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
        , div [ class "modals" ] (List.map (renderModal model) model.modals)
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
    div [ class "button" ]
        [ div attrs children ]

errorModal : Model -> String -> ModalOptions Msg Model
errorModal model err =
    { defaultModalOptions
    | title = text "Error"
    , content =
        div [ class "error-modal" ]
            [ text err ]
    }

loginModal : Model -> ModalOptions Msg Model
loginModal model =
  let
    invalid = model.loginUserName == "" || model.loginPassword == ""
  in
    { defaultModalOptions
    | title = text "Login"
    , onClose = Just ClearLoginDetails
    , preventClose = \model -> model.loggingIn
    , content =
        div [ class "login-modal" ]
            [ div [ class "inputs" ]
                [ Textfield.render Mdl [70,0] model.mdl
                    [ Textfield.label "Username"
                    , Textfield.floatingLabel
                    , Textfield.onInput LoginUserName
                    , Textfield.value model.loginUserName
                    ]
                , Textfield.render Mdl [70,1] model.mdl
                    [ Textfield.label "Password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    , Textfield.onInput LoginPassword
                    , Textfield.value model.loginPassword
                    ]
                ]
            , div [ class "login-button" ]
                [ Button.render Mdl [0] model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.disabled `when` invalid
                    , Button.onClick (All [ClearLoginDetails, DoLogin])
                    ]
                    [ text "Login" ]
                ]
            ]
    }

addEntryModal : Model -> ModalOptions Msg Model
addEntryModal model =
    { defaultModalOptions
    | title = text "Add Entry"
    , content =
        div [ class "add-entry-modal" ]
            [ ]
    }


editEntryModal : Model -> ModalOptions Msg Model
editEntryModal model =
    { defaultModalOptions
    | title = text "Edit Entry"
    , content =
        div [ class "edit-entry-modal" ]
            [ ]
    }

removeEntryModal : Model -> ModalOptions Msg Model
removeEntryModal model =
    { defaultModalOptions
    | title = text "Error"
    , content =
        div [ class "remove-entry-modal" ]
            [ ]
    }

--
-- View - Modal maker
--

renderModal : Model -> ModalName -> Html Msg
renderModal model modalName =
  let
    makeModal {title,content,onClose,preventClose} =
      let
        closeMsgs = All ([CloseTopModal] ++ case onClose of
            Nothing -> []
            Just msg -> [msg])
      in
        div [ class "modal-background" ]
            [ div [ class "modal-inner" ]
                [ div [ class "title" ]
                    [ div [ class "title-inner" ] [ title ]
                    , Button.render Mdl [100,0] model.mdl
                        [ Button.icon
                        , Button.plain
                        , Button.onClick closeMsgs
                        , Button.disabled `when` preventClose model
                        ]
                        [ Icon.i "close" ]
                    ]
                , div [ class "content" ]
                    [ content ]
                ]
            ]
  in
    case modalName of
        ErrorModal str -> makeModal (errorModal model str)
        LoginModal -> makeModal (loginModal model)
        AddEntryModal -> makeModal (addEntryModal model)
        EditEntryModal -> makeModal (editEntryModal model)
        RemoveEntryModal -> makeModal (removeEntryModal model)

type ModalName
    = ErrorModal String
    | LoginModal
    | AddEntryModal
    | EditEntryModal
    | RemoveEntryModal

type alias ModalOptions msg model =
    { title : Html msg
    , content : Html msg
    , preventClose : model -> Bool
    , onClose : Maybe msg
    }

defaultModalOptions : ModalOptions Msg Model
defaultModalOptions =
    { title = text ""
    , content = div [] []
    , preventClose = \_ -> False
    , onClose = Nothing
    }

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