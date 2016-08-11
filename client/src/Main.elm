import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task

import Api
import Api.Entries as Entries exposing (Entry)

type alias Model =
    { entries : List Entry
    , err : String
    }


type Msg
    = UpdateEntries (List Entry)
    | FailedGetEntries Api.Error
    | Noop

view : Model -> Html Msg
view model =
    div [ ]
        [ text model.err
        , text (toString model.entries)
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Noop -> (model, Cmd.none)
    FailedGetEntries err -> ({model | err = toString err}, Cmd.none)
    UpdateEntries es -> ({model | entries = es}, Cmd.none)

init : (Model, Cmd Msg)
init =
  let
    cmds = Task.perform FailedGetEntries UpdateEntries Entries.get
  in
    (Model [] "hello world!", cmds)

main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \model -> Sub.batch
        [ ]
    }

