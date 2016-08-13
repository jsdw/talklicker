import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task
import Debug

import Material
import Material.Scheme
import Material.List as Lists
import Material.Options as Options exposing (when, css)
import Material.Icon as Icon

import Api
import Api.Entries as Entries exposing (Entry)
import Api.Users as Users exposing (User)

--
-- Model
--

model : Model
model =
    { mdl = Material.model
    , entries = []
    , modals = []
    , error = ""
    , user = Nothing

    -- login modal:
    , loginUserName = ""
    , loginPassword = ""
    }

type alias Model =
    { mdl : Material.Model
    , entries : List Entry
    , modals : List ModalName
    , error : String
    , user : Maybe User

    -- login modal:
    , loginUserName : String
    , loginPassword : String
    }

--
-- Update
--

type Msg
    = Mdl (Material.Msg Msg)
    | ApiError Api.Error
    | UpdateEntries (List Entry)
    | CurrentUser (Maybe User)
    | LogOut

    -- login modal:
    | ShowLoginModal
    | LoginUserName String
    | LoginPassword String
    | AttemptLogin

    | ShowAddEntryModal
    | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case Debug.log "update" msg of
    Mdl msg' -> Material.update msg' model

    ApiError error -> ({ model | error = toString error }, Cmd.none)
    CurrentUser mUser -> ({ model | user = mUser }, Cmd.none)
    UpdateEntries es -> ({ model | entries = es }, Cmd.none)
    LogOut -> ({ model | user = Nothing }, Cmd.none)

    -- login modal:
    ShowLoginModal -> ({ model | modals = model.modals ++ [LoginModal] }, Cmd.none)
    LoginUserName str -> ({ model | loginUserName = str }, Cmd.none)
    LoginPassword str -> ({ model | loginPassword = str }, Cmd.none)
    AttemptLogin -> ({ model | loginUserName = "", loginPassword = "" }, doLogin model.loginUserName model.loginPassword)

    ShowAddEntryModal -> ({ model | modals = model.modals ++ [AddEntryModal] }, Cmd.none)

    Noop -> (model, Cmd.none)


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
    makeModal html =
        div [ class "modal-background" ]
            [ div [ class "modal-inner" ]
                [ html
                ]
            ]
  in
    case modalName of
        ErrorModal -> makeModal (errorModal model)
        LoginModal -> makeModal (loginModal model)
        AddEntryModal -> makeModal (addEntryModal model)

type ModalName
    = ErrorModal
    | LoginModal
    | AddEntryModal

errorModal : Model -> Html Msg
errorModal model =
    div [ class "error-modal" ]
        [ text (toString model.error) ]

loginModal : Model -> Html Msg
loginModal model =
    div [ class "login-modal" ]
        [ div [ class "inputs" ]
            [ input [ placeholder "username", onInput LoginUserName, defaultValue model.loginUserName ] []
            , input [ placeholder "password", type' "password", onInput LoginPassword, defaultValue model.loginPassword ] []
            ]
        , div [ class "login-button" ]
            [ button' [ onClick AttemptLogin ] [ text "Login" ]
            ]
        ]

addEntryModal : Model -> Html Msg
addEntryModal model = div [ class "add-entry-modal" ] []

doLogin : String -> String -> Cmd Msg
doLogin name pass =
  let
    login = Users.login name pass |> Task.map Just
  in
    Task.perform ApiError CurrentUser login
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