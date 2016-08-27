module Modals exposing (renderWith, render, RenderOptions, choice, ChoiceOptions, choiceToRenderOptions)

import Html.App
import Html.Attributes exposing (..)
import Html exposing (..)

import Material.Options as Options exposing (when, css, cs)
import Material.Icon as Icon
import Material.Button as Button
import Material.Progress as Loading
import Material

import Html.Helpers exposing ((?))

--
-- Render a basic modal overlay, but take care of transforming the
-- input model and the output message so that we can wrap modals that
-- rely on some subpart of the model/messages to render in the parent.
-- Sample usage:
--
-- let myAddModal = renderWith addModal .addModal AddModal
-- in div [ myAddModal model ]
--
-- here, addModal uses parentModel.addModel as its model,
-- and wraps all of its output messages up into the AddModal
-- type.
--
renderWith : (WithMdl childModel -> RenderOptions childMsg)
          -> (parentModel -> WithMdl childModel)
          -> (childMsg -> parentMsg)
          -> (parentModel -> Html parentMsg)
renderWith modal toModel fromMsg = \parentModel ->
    let thisModel = toModel parentModel
    in render (modal thisModel) thisModel |> Html.App.map fromMsg

type alias WithMdl model = { model | mdl: Material.Model }

--
-- Render a basic modal overlay given options and a model that
-- has a .mdl thing inside it.
--

render : RenderOptions msg -> WithMdl a -> Html msg
render {title,content,onClose,preventClose,hideClose,isLoading,mdl,cover} model =
    div [ class "modal-background" ]
        ([ div [ class "modal-inner" ]
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
        ] ++ cover)

type alias RenderOptions msg =
    { title : Html msg
    , content : Html msg
    , preventClose : Bool
    , hideClose : Bool
    , isLoading : Bool
    , onClose : msg
    , cover : List (Html msg)
    , mdl : Material.Msg msg -> msg
    }

--
-- A more specific version of general modals aimed
-- at showing alerts/warnings/confirms.
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

choiceToRenderOptions : ChoiceOptions msg -> WithMdl a -> RenderOptions msg
choiceToRenderOptions opts model =
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
    , cover = []
    }

choice : ChoiceOptions msg -> WithMdl a -> Html msg
choice opts model = render (choiceToRenderOptions opts model) model