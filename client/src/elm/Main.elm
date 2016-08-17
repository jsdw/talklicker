import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task exposing (Task)
import Debug
import Dict exposing (Dict)

import Material
--import Material.Scheme
--import Material.List as Lists
import Material.Options as Options exposing (when, css, cs)
import Material.Icon as Icon
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Progress as Loading

import Api
import Api.Entries as Entries exposing (Entry, EntryType(..))
import Api.Users as Users exposing (User, LoginError(..))

--
-- Model
--

model : Model
model =
    { loading = True

    , entries = []
    , users = Dict.empty
    , modals = []
    , error = ""
    , user = Nothing

    -- login modal:
    , loginUserName = ""
    , loginPassword = ""
    , loggingIn = False
    , loginError = Nothing

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
    { loading : Bool

    , entries : List Entry
    , users : Dict String User
    , modals : List ModalName
    , error : String
    , user : Maybe User

    -- login modal:
    , loginUserName : String
    , loginPassword : String
    , loggingIn : Bool
    , loginError : Maybe LoginError

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
    | UpdateCoreDetails CoreDetails
    | LogOut

    -- login modal:
    | ShowLoginModal
    | LoginUserName String
    | LoginPassword String
    | DoLogin
    | LoginFailed LoginError
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
        { model | loading = False, error = toString error } ! []
    UpdateCoreDetails core ->
        { model | loading = False, user = core.currentUser, entries = core.entries, users = core.users } ! []
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
        { model | loggingIn = True, loginError = Nothing } ! [doLogin model]
    LoginFailed err ->
        { model | loggingIn = False, loginError = Just err } ! []
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
    Task.perform LoginFailed LoginSuccess login

resetLoginState : Model -> Model
resetLoginState model =
    { model
    | loginUserName = ""
    , loginPassword = ""
    , loginError = Nothing
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
            , model.loading ?
                div [ class "loading-overlay" ]
                    [ Loading.indeterminate
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
    entry e =
      let
        entryIcon = case e.entryType of
            Talk -> Icon.i "insert_emoticon"
            Project -> Icon.i "keyboard"
        entryUser = case Dict.get e.user model.users of
            Nothing -> "Unknown User"
            Just u -> u.fullName
        entryHours = round (toFloat e.duration / 3600000)
      in
        div [ class "entry" ]
            [ div [ class "title" ]
                [ div [ class "icon" ] [ entryIcon ]
                , div [ class "text" ] [ text e.name ]
                ]
            , div [ class "description" ] [ text e.description ]
            , div [ class "user"] [ text entryUser ]
            , div [ class "duration" ] [ text <| (toString entryHours)++"h" ]
            ]
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
    loginErrorString = case model.loginError of
        Just LoginBadUser -> "Wrong username"
        Just LoginBadPassword -> "Wrong password"
        _ -> "Network error"
  in
    { defaultModalOptions
    | title = text "Login"
    , onClose = Just ClearLoginDetails
    , preventClose = \model -> model.loggingIn
    , isLoading = \model -> model.loggingIn
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
            , div [ class "bottom-row" ]
                [ Button.render Mdl [0] model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.disabled `when` invalid
                    , Button.onClick DoLogin
                    , cs "login-button"
                    ]
                    [ text "Login" ]
                , model.loginError ??
                    div [ class "error" ]
                        [ text loginErrorString ]
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
    makeModal {title,content,onClose,preventClose,isLoading} =
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
                    , isLoading model ?
                        div [ class "loading-overlay" ]
                            [ Loading.indeterminate
                            ]
                    ]
                , div [ class "content" ]
                    [ content
                    ]
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
    , isLoading : model -> Bool
    , onClose : Maybe msg
    }

defaultModalOptions : ModalOptions Msg Model
defaultModalOptions =
    { title = text ""
    , content = div [] []
    , preventClose = \_ -> False
    , isLoading = \_ -> False
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
init = (model, updateEverything)

type alias CoreDetails =
    { currentUser : Maybe User, entries : List Entry, users : Dict String User }

updateEverything : Cmd Msg
updateEverything = Task.perform ApiError UpdateCoreDetails getEverything

getEverything : Task Api.Error CoreDetails
getEverything =
  let
    fn = \curr entries users ->
        { currentUser = curr, entries = entries, users = users }
  in
    Task.map3 fn Users.current Entries.get Users.get

--
-- Utils
--

isJust : Maybe a -> Bool
isJust m = case m of
    Nothing -> False
    Just _ -> True

(?) : Bool -> Html a -> Html a
(?) b html = if b then html else noNode

(??) : Maybe m -> Html a -> Html a
(??) m html = if isJust m then html else noNode

noNode = Html.node "nothing" [ style [("position", "absolute"), ("display", "none")] ] []
