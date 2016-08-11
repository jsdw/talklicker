import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task

import Api
import Api.Entries as Entries exposing (Entry)
import Api.Users as Users exposing (User)

type alias Model =
    { entries : List Entry
    , err : String
    , user : Maybe User
    }

type Msg
    = UpdateEntries (List Entry)
    | CurrentUser (Maybe User)
    | ApiError Api.Error
    | Noop

view : Model -> Html Msg
view model =
  let
    isLoggedIn = isJust model.user
  in
    div [ ]
        [ text model.err
        , text (toString model.entries)
        , text (if isLoggedIn then "Logged In" else "Not Logged In")
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Noop -> (model, Cmd.none)
    ApiError err -> ({model | err = toString err}, Cmd.none)
    CurrentUser user -> ({model | user = user}, Cmd.none)
    UpdateEntries es -> ({model | entries = es}, Cmd.none)

init : (Model, Cmd Msg)
init =
  let
    currentUser = Task.perform ApiError CurrentUser Users.current
    getEntries = Task.perform ApiError UpdateEntries Entries.get
  in
    (Model [] "" Nothing, Cmd.batch [getEntries, currentUser])

main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \model -> Sub.batch
        [ ]
    }

isJust : Maybe a -> Bool
isJust m = case m of
    Nothing -> False
    Just _ -> True