module Modals.User exposing (model, prepareForAdd, prepareForEdit, Model, Msg, Act(..), update, addModal, editModal, profileModal)

--import Html.App
--import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task

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

    -- current logged in user
    , currentUser : Maybe User
    -- is our remove submodal shown?
    , userRemoving : Bool
    -- is password reset submodal shown?
    , passwordResetting : Bool

    , mdl : Material.Model
    }

model : Model
model =
    { userName = ""
    , userFullName = ""
    , userType = NormalUser
    , userSaving = False
    , userError = False

    , currentUser = Nothing
    , userRemoving = False
    , passwordResetting = False

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
    | Removed String --user ID
    | ShowSetPasswordModal
    | CloseMe

update : Msg -> Model -> (Model, Maybe Act, Cmd Msg)
update msg model = case msg of
    ShowSetPassword ->
        (model, ShowSetPasswordModal) ^!! []
    ShowResetPassword ->
        { model | passwordResetting = True } !! []
    DoResetPassword ->
        model !! [doResetPassword model.userName]
    DoneResetPassword ->
        { model | passwordResetting = False } !! []
    UpdateUserName str ->
        { model | userName = str } !! []
    UpdateUserFullName str ->
        { model | userFullName = str } !! []
    UpdateUserType ty ->
        { model | userType = ty } !! []
    DoEditUser ->
        { model | userSaving = True, userError = False } !! [doEditUser model]
    DoAddUser ->
        { model | userSaving = True, userError = False } !! [doAddUser model]
    EditUserSuccess user ->
        ({ model | userSaving = False}, Updated user) ^!! []
    AddUserSuccess user ->
        ({ model | userSaving = False}, Added user) ^!! []
    EditUserFailed ->
        { model | userSaving = False, userError = True } !! []

    -- remove user
    ShowRemoveUserModal ->
        { model | userRemoving = True } !! []
    DoRemoveUser ->
        model !! [doRemoveUser model]
    DoneRemoveUser ->
        (model, Removed model.userName) ^!! []

    Close ->
        (model, CloseMe) ^!! []
    Mdl msg' ->
        let (model,cmd) = Material.update msg' model
        in (model, Nothing, cmd)
    Noop ->
        model !! []

(!!) : Model -> List (Cmd Msg) -> (Model, Maybe Act, Cmd Msg)
(!!) model msgs = (model, Nothing, Cmd.batch msgs)

(^!!) : (Model, Act) -> List (Cmd Msg) -> (Model, Maybe Act, Cmd Msg)
(^!!) (model,act) msgs = (model, Just act, Cmd.batch msgs)

doResetPassword : String -> Cmd Msg
doResetPassword username =
  let
    resetPass = Users.set username { fullName = Nothing, userType = Nothing, pass = Just "" }
  in
    Task.perform (always DoneResetPassword) (always DoneResetPassword) resetPass

doEditUser : Model -> Cmd Msg
doEditUser model =
  let
    editUser = Users.set model.userName { fullName = Just model.userFullName, userType = Just model.userType, pass = Nothing }
  in
    Task.perform (always EditUserFailed) EditUserSuccess editUser

doAddUser : Model -> Cmd Msg
doAddUser model =
  let
    addUser = Users.add { name = model.userName, fullName = model.userFullName, userType = model.userType, pass = "" }
  in
    Task.perform (always EditUserFailed) AddUserSuccess addUser

doRemoveUser : Model -> Cmd Msg
doRemoveUser model = Task.perform (always DoneRemoveUser) (always DoneRemoveUser) (Users.remove model.userName)

--
-- View
--

addModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
addModal = Modals.renderWith (\model ->
    { title = text "Add User"
    , isLoading = model.userSaving
    , preventClose = False
    , hideClose = False
    , onClose = Close
    , mdl = Mdl
    , cover = showSubModals model
    , content = userModalHtml False model
    })

editModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
editModal = Modals.renderWith (\model ->
    { title = text "Edit User"
    , isLoading = model.userSaving
    , preventClose = False
    , hideClose = False
    , onClose = Close
    , mdl = Mdl
    , cover = showSubModals model
    , content = userModalHtml True model
    })

profileModal : (parentModel -> Model) -> (Msg -> parentMsg) -> (parentModel -> Html parentMsg)
profileModal = Modals.renderWith (\model ->
    { title = text "Profile"
    , isLoading = model.userSaving
    , preventClose = False
    , hideClose = False
    , onClose = Close
    , mdl = Mdl
    , cover = showSubModals model
    , content = userModalHtml True model
    })

showSubModals : Model -> List (Html Msg)
showSubModals model =
  let
    resetModal = if model.passwordResetting then [ resetPasswordModal model ] else []
    removeModal = if model.userRemoving then [ removeUserModal model ] else []
  in
    resetModal ++ removeModal

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
    isAdmin = Maybe.map .userType model.currentUser == Just Admin
    isMe = isEditMode && Maybe.map .name model.currentUser == Just model.userName
    userType = case model.userType of
        Admin -> "Administrator"
        NormalUser -> "Normal User"
  in
    div [ class "user-modal" ]
        [ table [ class "inputs" ]
            [ inputRow "Username" <|
                if isEditMode
                then
                    text model.userName
                else
                    Textfield.render Mdl [10,0] model.mdl
                        [ Textfield.onInput UpdateUserName
                        , Textfield.value model.userName
                        ]
            , inputRow "Display Name" <|
                Textfield.render Mdl [10,1] model.mdl
                    [ Textfield.onInput UpdateUserFullName
                    , Textfield.value model.userFullName
                    ]
            , (not isMe && (isAdmin || not isEditMode)) ?
                (inputRow "Type" <|
                    div [ class "type-inputs" ]
                        [ Toggles.radio Mdl [10,2] model.mdl
                            [ Toggles.value (model.userType == Admin)
                            , Toggles.group "UserType"
                            , Toggles.ripple
                            , Toggles.onClick (UpdateUserType Admin)
                            ]
                            [ text "Administrator" ]
                        , Toggles.radio Mdl [10,3] model.mdl
                            [ Toggles.value (model.userType == NormalUser)
                            , Toggles.group "UserType"
                            , Toggles.ripple
                            , Toggles.onClick (UpdateUserType NormalUser)
                            ]
                            [ text "Normal User" ]
                        ])
            , (isEditMode && isMe) ?
                (inputRow "Password" <|
                    Button.render Mdl [10,4] model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.onClick ShowSetPassword
                        , cs "set-password-button"
                        ]
                        [ text "Set Password" ])
            , (isEditMode && isAdmin && not isMe) ?
                (inputRow "Reset Password" <|
                    Button.render Mdl [10,5] model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.onClick ShowResetPassword
                        , cs "set-password-button"
                        ]
                        [ text "Reset Password" ])
            ]
        , div [ class "bottom-row" ]
            [ Button.render Mdl [10,6] model.mdl
                [ Button.raised
                , Button.colored
                , Button.disabled `when` (model.userName == "" || model.userFullName == "")
                , Button.onClick (if isEditMode then DoEditUser else DoAddUser)
                , cs "add-user-button"
                ]
                [ text (if isMe then "Save" else if isEditMode then "Update User" else "Add User") ]
            , if model.userError
                then div [ class "error" ] [ text "Username taken" ]
                else div [ class "space-filler" ] []
            , (isEditMode && not isMe) ?
                div [ class "delete" ]
                    [ Button.render Mdl [10,7] model.mdl
                        [ Button.icon
                        , Button.ripple
                        , Button.onClick ShowRemoveUserModal
                        ]
                        [ Icon.i "delete"]
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