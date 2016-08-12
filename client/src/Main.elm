import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task

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
    , err = ""
    , user = Nothing
    }

type alias Model =
    { mdl : Material.Model
    , entries : List Entry
    , err : String
    , user : Maybe User
    }

--
-- Update
--

type Msg
    = Mdl (Material.Msg Msg)
    | UpdateEntries (List Entry)
    | CurrentUser (Maybe User)
    | ApiError Api.Error
    | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Noop -> (model, Cmd.none)
    ApiError err -> ({model | err = toString err}, Cmd.none)
    CurrentUser mUser -> ({model | user = mUser}, Cmd.none)
    UpdateEntries es -> ({model | entries = es}, Cmd.none)
    Mdl msg' -> Material.update msg' model

--
-- View
--

type alias Mdl =
  Material.Model

view : Model -> Html Msg
view model =
  let
    isLoggedIn = isJust model.user
  in
    div [ ]
        [ div [] []
        , div [] [ entries model ]
        , text model.err
        , text (toString model.entries)
        , text (if isLoggedIn then "Logged In" else "Not Logged In")
        ]

entries : Model -> Html Msg
entries m =
  Lists.ul [ css "margin" "0", css "padding" "0" ]
    [ Lists.li []
        [ Lists.content []
            [ Lists.icon "inbox" []
            , text "Inbox"
            ]
        ]
    , Lists.li []
        [ Lists.content []
            [ Lists.icon "send" []
            , text "Sent mail"
            ]
        ]
    , Lists.li []
        [ Lists.content []
            [ Lists.icon "delete" []
            , text "Trash"
            ]
        ]
    ]

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

isJust : Maybe a -> Bool
isJust m = case m of
    Nothing -> False
    Just _ -> True