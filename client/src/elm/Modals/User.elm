module Modals.User exposing (model, prepareForAdd, prepareForEdit, prepareSetPass, Model, Msg(SetPasswordFirst, SetPasswordSecond), Act(..), update, addModal, editModal, profileModal, setPasswordModal)

--import Html.Events exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)
import Task.Extra exposing (perform)
import String
import Material
import Material.Options as Options exposing (when, css, cs)
import Material.Icon as Icon
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Modals
import Api.Users as Users exposing (User, UserType(..), LoginError(..))
import Html.Helpers exposing (..)


--
-- Model
--


type alias Model =
    { userName : String
    , userFullName : String
    , userType : UserType
    , userSaving : Bool
    , userError : Bool
    , setPasswordFirst : String
    , setPasswordSecond : String
    , setPasswordSaving :
        Bool
        -- current logged in user
    , currentUser :
        Maybe User
        -- is our remove submodal shown?
    , userRemoving :
        Bool
        -- is password reset submodal shown?
    , passwordResetting :
        Bool
        -- is password set submodal shown?
    , setPassword : Bool
    , mdl : Material.Model
    }


model : Model
model =
    { userName = ""
    , userFullName = ""
    , userType = NormalUser
    , userSaving = False
    , userError = False
    , setPasswordFirst = ""
    , setPasswordSecond = ""
    , setPasswordSaving = False
    , currentUser = Nothing
    , userRemoving = False
    , passwordResetting = False
    , setPassword = False
    , mdl = Material.model
    }


prepareForAdd : Maybe User -> Model -> Model
prepareForAdd currentUser model =
    { model
        | userName = ""
        , userFullName = ""
        , userType = NormalUser
        , userSaving = False
        , userError = False
        , userRemoving = False
        , passwordResetting = False
        , currentUser = currentUser
    }


prepareForEdit : Maybe User -> User -> Model -> Model
prepareForEdit currentUser user model =
    { model
        | userName = user.name
        , userFullName = user.fullName
        , userType = user.userType
        , userSaving = False
        , userError = False
        , userRemoving = False
        , passwordResetting = False
        , currentUser = currentUser
    }


prepareSetPass : User -> Model -> Model
prepareSetPass user model =
    { model
        | userName = user.name
        , setPasswordFirst = ""
        , setPasswordSecond = ""
        , setPasswordSaving = False
    }



--
-- Update
--
-- messages we handle internally


type Msg
    = ShowSetPassword
    | ShowResetPassword
    | DoResetPassword
    | DoneResetPassword
    | UpdateUserName String
    | UpdateUserFullName String
    | UpdateUserType UserType
    | DoEditUser
    | DoAddUser
    | EditUserSuccess User
    | AddUserSuccess User
    | EditUserFailed
      -- set password modal
    | SetPasswordFirst String
    | SetPasswordSecond String
    | DoSetPassword Bool
    | SetPasswordSuccess Bool
    | SetPasswordFailed
      -- remove a user (from add user modal)
    | ShowRemoveUserModal
    | DoRemoveUser
    | DoneRemoveUser
    | Close
    | Mdl (Material.Msg Msg)
    | Noop



-- messages we pass back to the guy above us.


type Act
    = Added User
    | Updated User
    | Removed String
      --user ID
    | SetPasswordDone
      -- for externally launched version of the modal.
    | CloseMe


update : Msg -> Model -> ( Model, Maybe Act, Cmd Msg )
update msg model =
    case msg of
        ShowSetPassword ->
            { model | setPassword = True } !! []

        ShowResetPassword ->
            { model | passwordResetting = True } !! []

        DoResetPassword ->
            model !! [ doResetPassword model.userName ]

        DoneResetPassword ->
            { model | passwordResetting = False } !! []

        UpdateUserName str ->
            { model | userName = str } !! []

        UpdateUserFullName str ->
            { model | userFullName = str } !! []

        UpdateUserType ty ->
            { model | userType = ty } !! []

        DoEditUser ->
            { model | userSaving = True, userError = False } !! [ doEditUser model ]

        DoAddUser ->
            { model | userSaving = True, userError = False } !! [ doAddUser model ]

        EditUserSuccess user ->
            ( { model | userSaving = False }, Updated user ) ^!! []

        AddUserSuccess user ->
            ( { model | userSaving = False }, Added user ) ^!! []

        EditUserFailed ->
            { model | userSaving = False, userError = True } !! []

        -- set password modal:
        SetPasswordFirst str ->
            { model | setPasswordFirst = str } !! []

        SetPasswordSecond str ->
            { model | setPasswordSecond = str } !! []

        DoSetPassword launchedExternally ->
            { model | setPasswordSaving = True } !! [ doSetPassword model launchedExternally ]

        SetPasswordSuccess launchedExternally ->
            ( { model | setPassword = False, setPasswordSaving = False, setPasswordFirst = "", setPasswordSecond = "" }
            , if launchedExternally then
                Just SetPasswordDone
              else
                Nothing
            , Cmd.none
            )

        SetPasswordFailed ->
            { model | setPassword = False, setPasswordSaving = False, setPasswordFirst = "", setPasswordSecond = "" } !! []

        -- remove user
        ShowRemoveUserModal ->
            { model | userRemoving = True } !! []

        DoRemoveUser ->
            model !! [ doRemoveUser model ]

        DoneRemoveUser ->
            ( model, Removed model.userName ) ^!! []

        Close ->
            ( model, CloseMe ) ^!! []

        Mdl msg_ ->
            let
                ( model2, cmd ) =
                    Material.update Mdl msg_ model
            in
                ( model2, Nothing, cmd )

        Noop ->
            model !! []


(!!) : Model -> List (Cmd Msg) -> ( Model, Maybe Act, Cmd Msg )
(!!) model msgs =
    ( model, Nothing, Cmd.batch msgs )


(^!!) : ( Model, Act ) -> List (Cmd Msg) -> ( Model, Maybe Act, Cmd Msg )
(^!!) ( model, act ) msgs =
    ( model, Just act, Cmd.batch msgs )


doResetPassword : String -> Cmd Msg
doResetPassword username =
    let
        resetPass =
            Users.set username { fullName = Nothing, userType = Nothing, pass = Just "" }
    in
        perform (always DoneResetPassword) (always DoneResetPassword) resetPass


doSetPassword : Model -> Bool -> Cmd Msg
doSetPassword model launchedExternally =
    let
        setPass =
            Users.set model.userName { fullName = Nothing, userType = Nothing, pass = Just model.setPasswordFirst }
    in
        Debug.log (toString model) <| perform (always SetPasswordFailed) (always (SetPasswordSuccess launchedExternally)) setPass


doEditUser : Model -> Cmd Msg
doEditUser model =
    let
        editUser =
            Users.set model.userName { fullName = Just model.userFullName, userType = Just model.userType, pass = Nothing }
    in
        perform (always EditUserFailed) EditUserSuccess editUser


doAddUser : Model -> Cmd Msg
doAddUser model =
    let
        addUser =
            Users.add { name = model.userName, fullName = model.userFullName, userType = model.userType, pass = "" }
    in
        perform (always EditUserFailed) AddUserSuccess addUser


doRemoveUser : Model -> Cmd Msg
doRemoveUser model =
    perform (always DoneRemoveUser) (always DoneRemoveUser) (Users.remove model.userName)

--
-- Views (external interfaces)
--


addModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
addModal =
    Modals.renderWith
        (\model ->
            { title = text "Add User"
            , isLoading = model.userSaving
            , preventClose = False
            , hideClose = False
            , onClose = Close
            , mdl = Mdl
            , cover = showSubModals model
            , content = userModalHtml False model
            }
        )


editModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
editModal =
    Modals.renderWith
        (\model ->
            { title = text "Edit User"
            , isLoading = model.userSaving
            , preventClose = False
            , hideClose = False
            , onClose = Close
            , mdl = Mdl
            , cover = showSubModals model
            , content = userModalHtml True model
            }
        )


profileModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
profileModal =
    Modals.renderWith
        (\model ->
            { title = text "Profile"
            , isLoading = model.userSaving
            , preventClose = False
            , hideClose = False
            , onClose = Close
            , mdl = Mdl
            , cover = showSubModals model
            , content = userModalHtml True model
            }
        )


setPasswordModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
setPasswordModal fromParentModel toParentMsg =
    \model -> setPasswordModal_ True (fromParentModel model) |> Html.map toParentMsg


showSubModals : Model -> List (Html Msg)
showSubModals model =
    let
        resetPassModal =
            if model.passwordResetting then
                [ resetPasswordModal model ]
            else
                []

        removeModal =
            if model.userRemoving then
                [ removeUserModal model ]
            else
                []

        setPassModal =
            if model.setPassword then
                [ setPasswordModal_ False model ]
            else
                []
    in
        resetPassModal ++ removeModal ++ setPassModal



--
-- Internal-only views:
--


removeUserModal : Model -> Html Msg
removeUserModal model =
    let
        opts =
            { title = "Remove User"
            , icon = "warning"
            , message = "Are you sure you want to remove this user? This will also delete any Entries associated with them."
            , onPerform = DoRemoveUser
            , performText = "Remove"
            , onCancel = Close
            , cancelText = "Cancel"
            , hidePerform = False
            , hideCancel = False
            , mdl = Mdl
            }
    in
        Modals.choice opts model


userModalHtml : Bool -> Model -> Html Msg
userModalHtml isEditMode model =
    let
        isAdmin =
            Maybe.map .userType model.currentUser == Just Admin

        isMe =
            isEditMode && Maybe.map .name model.currentUser == Just model.userName

        userType =
            case model.userType of
                Admin ->
                    "Administrator"

                NormalUser ->
                    "Normal User"
    in
        div [ class "user-modal" ]
            [ table [ class "inputs" ]
                [ inputRow "Username" <|
                    if isEditMode then
                        text model.userName
                    else
                        Textfield.render Mdl
                            [ 10, 0 ]
                            model.mdl
                            [ Options.onInput UpdateUserName
                            , Textfield.value model.userName
                            ]
                            []
                , inputRow "Display Name" <|
                    Textfield.render Mdl
                        [ 10, 1 ]
                        model.mdl
                        [ Options.onInput UpdateUserFullName
                        , Textfield.value model.userFullName
                        ]
                        []
                , (not isMe && (isAdmin || not isEditMode))
                    ? (inputRow "Type" <|
                        div [ class "type-inputs" ]
                            [ Toggles.radio Mdl
                                [ 10, 2 ]
                                model.mdl
                                [ Toggles.value (model.userType == Admin)
                                , Toggles.group "UserType"
                                , Toggles.ripple
                                , Options.onClick (UpdateUserType Admin)
                                ]
                                [ text "Administrator" ]
                            , Toggles.radio Mdl
                                [ 10, 3 ]
                                model.mdl
                                [ Toggles.value (model.userType == NormalUser)
                                , Toggles.group "UserType"
                                , Toggles.ripple
                                , Options.onClick (UpdateUserType NormalUser)
                                ]
                                [ text "Normal User" ]
                            ]
                      )
                , (isEditMode && isMe)
                    ? (inputRow "Password" <|
                        Button.render Mdl
                            [ 10, 4 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , Options.onClick ShowSetPassword
                            , cs "set-password-button"
                            ]
                            [ text "Set Password" ]
                      )
                , (isEditMode && isAdmin && not isMe)
                    ? (inputRow "Reset Password" <|
                        Button.render Mdl
                            [ 10, 5 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , Options.onClick ShowResetPassword
                            , cs "set-password-button"
                            ]
                            [ text "Reset Password" ]
                      )
                ]
            , div [ class "bottom-row" ]
                [ Button.render Mdl
                    [ 10, 6 ]
                    model.mdl
                    [ Button.raised
                    , Button.colored
                    , when (model.userName == "" || model.userFullName == "") Button.disabled
                    , Options.onClick
                        (if isEditMode then
                            DoEditUser
                         else
                            DoAddUser
                        )
                    , cs "add-user-button"
                    ]
                    [ text
                        (if isMe then
                            "Save"
                         else if isEditMode then
                            "Update User"
                         else
                            "Add User"
                        )
                    ]
                , if model.userError then
                    div [ class "error" ] [ text "Username taken" ]
                  else
                    div [ class "space-filler" ] []
                , (isEditMode && not isMe)
                    ? div [ class "delete" ]
                        [ Button.render Mdl
                            [ 10, 7 ]
                            model.mdl
                            [ Button.icon
                            , Button.ripple
                            , Options.onClick ShowRemoveUserModal
                            ]
                            [ Icon.i "delete" ]
                        ]
                ]
            ]


resetPasswordModal : Model -> Html Msg
resetPasswordModal model =
    let
        opts =
            { title = "Reset Password"
            , icon = "warning"
            , message = "Are you sure you want to reset this users password?"
            , onPerform = DoResetPassword
            , performText = "Reset"
            , onCancel = Close
            , cancelText = "Cancel"
            , hidePerform = False
            , hideCancel = False
            , mdl = Mdl
            }
    in
        Modals.choice opts model


setPasswordModal_ : Bool -> Model -> Html Msg
setPasswordModal_ launchedExternally model =
    let
        invalid =
            model.setPasswordFirst /= model.setPasswordSecond || String.length model.setPasswordFirst == 0

        opts =
            { title = text "Set Password"
            , preventClose = False
            , isLoading = model.setPasswordSaving
            , hideClose = launchedExternally
            , onClose = SetPasswordFailed
            , mdl = Mdl
            , cover = []
            , content =
                div [ class "set-password-modal" ]
                    [ launchedExternally
                        ? div [ class "needs-setting-text" ]
                            [ text "You have not yet set a password. Please do so now." ]
                    , div [ class "inputs" ]
                        [ Textfield.render Mdl
                            [ 71, 1 ]
                            model.mdl
                            [ Textfield.label "New Password"
                            , Textfield.floatingLabel
                            , Textfield.password
                            , Options.onInput SetPasswordFirst
                            , Textfield.value model.setPasswordFirst
                            ]
                            []
                        , Textfield.render Mdl
                            [ 71, 2 ]
                            model.mdl
                            [ Textfield.label "New Password Again"
                            , Textfield.floatingLabel
                            , Textfield.password
                            , Options.onInput SetPasswordSecond
                            , Textfield.value model.setPasswordSecond
                            ]
                            []
                        ]
                    , div [ class "bottom-row" ]
                        [ Button.render Mdl
                            [ 71, 3 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , when invalid Button.disabled
                            , Options.onClick (DoSetPassword launchedExternally)
                            , cs "set-password-button"
                            ]
                            [ text "Set Password" ]
                        , (invalid && model.setPasswordSecond /= "")
                            ? div [ class "error" ]
                                [ text "Passwords do not match" ]
                        ]
                    ]
            }
    in
        Modals.render opts model
