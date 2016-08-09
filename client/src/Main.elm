import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)

type alias Model =
    { test : String
    }


type Msg = Text String | Noop

view : Model -> Html Msg
view model =
    div [ ]
        [ text model.test
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Noop -> (model, Cmd.none)
    Text str -> ({model | test = str}, Cmd.none)

init : (Model, Cmd Msg)
init =
  let
    cmds = Cmd.none
  in
    (Model "hello world!", cmds)

main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \model -> Sub.batch
        [ ]
    }
