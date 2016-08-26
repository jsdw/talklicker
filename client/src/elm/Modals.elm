module Modals exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)

import Material.Options as Options exposing (when, css, cs)
import Material.Icon as Icon
import Material.Button as Button
import Material.Progress as Loading
import Material

import Html.Helpers exposing ((?))

--
-- Render a basic modal overlay:
--

render : (Material.Msg msg -> msg) -> { a | mdl: Material.Model } -> RenderOptions msg { a | mdl: Material.Model } -> Html msg
render mdl model {title,content,onClose,preventClose,hideClose,isLoading} =
    div [ class "modal-background" ]
        [ div [ class "modal-inner" ]
            [ div [ class "title" ]
                [ div [ class "title-inner" ] [ title ]
                , not (hideClose model) ?
                    Button.render mdl [100,0] model.mdl
                        [ Button.icon
                        , Button.plain
                        , Button.onClick onClose
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

type alias RenderOptions msg model =
    { title : Html msg
    , content : Html msg
    , preventClose : model -> Bool
    , hideClose : model -> Bool
    , isLoading : model -> Bool
    , onClose : msg
    }

--
-- A more specific version of general modals aimed
-- at showing alerts/warnings/confirms. provide the choice options
-- to choice and then the rest can be used as render.
--

type alias ChoiceOptions msg =
    { title : String
    , icon : String
    , message : String
    , onCancel : msg
    , onPerform : msg
    , cancelText : String
    , performText : String
    , hidePerform : Bool
    , hideCancel : Bool
    }

choiceOptions : msg -> msg -> ChoiceOptions msg
choiceOptions onCancel onPerform =
    { title = "Warning"
    , icon = "warning"
    , message = "Are you sure you want to do this?"
    , onCancel = onCancel
    , cancelText = "Dismiss"
    , hideCancel = False
    , onPerform = onPerform
    , performText = "Perform"
    , hidePerform = False
    }

choice : ChoiceOptions msg -> (Material.Msg msg -> msg) -> { a | mdl: Material.Model } -> RenderOptions msg { a | mdl: Material.Model }
choice opts mdl model =
    { title = text opts.title
    , preventClose = always False
    , onClose = opts.onCancel
    , hideClose = always False
    , isLoading = always False
    , content =
        div [ class "choice-modal" ]
            [ div [ class "content" ]
                [ div [ class "icon" ]
                    [ Icon.view opts.icon [ Icon.size48 ] ]
                , div [ class "message" ]
                    [ text opts.message ]
                ]
            , div [ class "buttons" ]
                [ not opts.hideCancel ?
                    Button.render mdl [200,0] model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.onClick opts.onCancel
                        , cs "cancel-button"
                        ]
                        [ text opts.cancelText ]
                , not opts.hidePerform ?
                    Button.render mdl [200,1] model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.onClick opts.onPerform
                        , cs "perform-button"
                        ]
                        [ text opts.performText ]
                ]
            ]
    }