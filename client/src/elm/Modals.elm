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

render : RenderOptions msg -> { a | mdl: Material.Model } -> Html msg
render {title,content,onClose,preventClose,hideClose,isLoading,mdl} model =
    div [ class "modal-background" ]
        [ div [ class "modal-inner" ]
            [ div [ class "title" ]
                [ div [ class "title-inner" ] [ title ]
                , not hideClose ?
                    Button.render mdl [100,0] model.mdl
                        [ Button.icon
                        , Button.plain
                        , Button.onClick onClose
                        , Button.disabled `when` preventClose
                        ]
                        [ Icon.i "close" ]
                , isLoading ?
                    div [ class "loading-overlay" ]
                        [ Loading.indeterminate
                        ]
                ]
            , div [ class "content" ]
                [ content
                ]
            ]
        ]

type alias RenderOptions msg =
    { title : Html msg
    , content : Html msg
    , preventClose : Bool
    , hideClose : Bool
    , isLoading : Bool
    , onClose : msg
    , mdl : Material.Msg msg -> msg
    }

--
-- A more specific version of general modals aimed
-- at showing alerts/warnings/confirms. choice converts
-- ChoiceOptions to RenderOptions so it can be rendered
-- as above.
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
    , mdl : Material.Msg msg -> msg
    }

choice : ChoiceOptions msg -> { a | mdl: Material.Model } -> RenderOptions msg
choice opts model =
    { title = text opts.title
    , preventClose = False
    , onClose = opts.onCancel
    , hideClose = False
    , isLoading = False
    , mdl = opts.mdl
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
                    Button.render opts.mdl [200,0] model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.onClick opts.onCancel
                        , cs "cancel-button"
                        ]
                        [ text opts.cancelText ]
                , not opts.hidePerform ?
                    Button.render opts.mdl [200,1] model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.onClick opts.onPerform
                        , cs "perform-button"
                        ]
                        [ text opts.performText ]
                ]
            ]
    }