import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task exposing (Task)
import Debug
import Dict exposing (Dict)
import List
import String

import Material
--import Material.Scheme
--import Material.List as Lists
import Material.Options as Options exposing (when, css, cs)
import Material.Icon as Icon
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Progress as Loading
import Material.Toggles as Toggles
import Material.Menu as Menu
import Material.Tabs as Tabs

import Modals
import Modals.Entry as EntryModal
import Html.Helpers exposing (..)

import Api
import Api.Entries as Entries exposing (Entry, EntryType(..), EntryError(..))
import Api.Users as Users exposing (User, UserType(..), LoginError(..))

--
-- Model
--

model : Model
model =
    { loading = True

    , tab = EntriesTab
    , entries = []
    , users = Dict.empty
    , modals = []
    , error = ""
    , user = Nothing

    -- login modal:
    , loginUserName = ""
    , loginPassword = ""
    , loggingIn = False
    , loginError = Nothing

    -- set password modal:
    , setPasswordFirst = ""
    , setPasswordSecond = ""
    , setPasswordSaving = False

    -- add/edit user modal:
    , userName = ""
    , userFullName = ""
    , userType = NormalUser
    , userSaving = False
    , userError = False

    -- entry stuff for add/edit:
    , entryModal = EntryModal.model

    , mdl = Material.model
    }

type alias Model =
    { loading : Bool

    , tab : Tab
    , entries : List Entry
    , users : Dict String User
    , modals : List (TheModel -> Html Msg)
    , error : String
    , user : Maybe User

    -- login modal:
    , loginUserName : String
    , loginPassword : String
    , loggingIn : Bool
    , loginError : Maybe LoginError

    -- set password modal:
    , setPasswordFirst : String
    , setPasswordSecond : String
    , setPasswordSaving : Bool

    -- add/edit user modal:
    , userName : String
    , userFullName : String
    , userType : UserType
    , userSaving : Bool
    , userError : Bool

    -- entry stuff for add/edit:
    , entryModal : EntryModal.Model

    , mdl : Material.Model

    }

-- allow Model alias to reference itself by
-- hiding itself inside a real type:
type TheModel = TheModel Model

type Tab = EntriesTab | UsersTab

--
-- Update
--

type Msg
    = SelectTab Tab
    | ApiError Api.Error
    | UpdateCoreDetails CoreDetails
    | LogOut
    | DoLogout

    -- login modal:
    | ShowLoginModal
    | LoginUserName String
    | LoginPassword String
    | DoLogin
    | LoginFailed LoginError
    | LoginSuccess User
    | ClearLoginDetails

    -- set password modal:
    | SetPasswordFirst String
    | SetPasswordSecond String
    | DoSetPassword
    | SetPasswordSuccess
    | SetPasswordFailed

    -- add/edit user modal:
    | ShowEditCurrentUserModal
    | ShowAddUserModal
    | ShowEditUserModal User
    | ShowSetPasswordModal
    | ShowResetPassword
    | DoResetPassword
    | UpdateUserName String
    | UpdateUserFullName String
    | UpdateUserType UserType
    | DoEditUser
    | EditUserSuccess User
    | EditUserFailed
    | DoAddUser

    -- remove a user (from add user modal)
    | ShowRemoveUserModal
    | DoRemoveUser
    | DoneRemoveUser

    -- add/edit/remove entry modal:
    | ShowAddEntryModal
    | ShowEditEntryModal Entry
    | EntryModal EntryModal.Msg

    -- perform several actions at once
    | All (List Msg)

    | CloseTopModal

    | Mdl (Material.Msg Msg)

    | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case logMsg msg of

    SelectTab tab ->
        { model | tab = tab } ! []

    ApiError error ->
        { model | loading = False, error = toString error } ! []
    UpdateCoreDetails core ->
        showSetPasswordIfNeeded { model | loading = False, user = core.currentUser, entries = core.entries, users = core.users } ! []
    LogOut ->
        showModal logoutModal model ! []
    DoLogout ->
        { model | user = Nothing, tab = EntriesTab } ! [Task.perform (always Noop) (always Noop) Users.logout]

    -- login modal:
    ShowLoginModal ->
        showModal loginModal model ! []
    LoginUserName str ->
        { model | loginUserName = str } ! []
    LoginPassword str ->
        { model | loginPassword = str } ! []
    DoLogin ->
        { model | loggingIn = True, loginError = Nothing } ! [doLogin model]
    LoginFailed err ->
        { model | loggingIn = False, loginError = Just err } ! []
    LoginSuccess user ->
        (showSetPasswordIfNeeded <| resetLoginState <| closeTopModal { model | user = Just user }) ! []
    ClearLoginDetails ->
        resetLoginState model ! []

    -- set password modal:
    SetPasswordFirst str ->
        { model | setPasswordFirst = str } ! []
    SetPasswordSecond str ->
        { model | setPasswordSecond = str } ! []
    DoSetPassword ->
        { model | setPasswordSaving = True } ! [doSetPassword model]
    SetPasswordSuccess ->
        closeTopModal { model | setPasswordSaving = False, setPasswordFirst = "", setPasswordSecond = "" } ! []
    SetPasswordFailed ->
        closeTopModal { model | setPasswordSaving = False, setPasswordFirst = "", setPasswordSecond = "" } ! []

    -- add/edit user modal:
    ShowEditCurrentUserModal ->
        showModal editCurrentUserModal (prepareEditCurrentUser model) ! []
    ShowEditUserModal user ->
        showModal editUserModal (prepareEditUser user model) ! []
    ShowSetPasswordModal ->
        showModal (setPasswordModal False) { model | setPasswordFirst = "", setPasswordSecond = "" } ! []
    ShowResetPassword ->
        showModal resetPasswordModal model ! []
    DoResetPassword ->
        model ! [doResetPassword model.userName]
    ShowAddUserModal ->
        showModal addUserModal (prepareAddUser model) ! []
    UpdateUserName str ->
        { model | userName = str } ! []
    UpdateUserFullName str ->
        { model | userFullName = str } ! []
    UpdateUserType ty ->
        { model | userType = ty } ! []
    DoEditUser ->
        { model | userSaving = True, userError = False } ! [doEditUser model]
    DoAddUser ->
        { model | userSaving = True, userError = False } ! [doAddUser model]
    EditUserSuccess user ->
        (closeTopModal <| updateModelWithUser user { model | userSaving = False } ) ! []
    EditUserFailed ->
        { model | userSaving = False, userError = True }  ! []

    -- remove user
    ShowRemoveUserModal ->
        showModal removeUserModal model ! []
    DoRemoveUser ->
      let
        users' = Dict.filter (\name _ -> name /= model.userName) model.users
      in
        { model | users = users' } ! [doRemoveUser model]
    DoneRemoveUser ->
        model ! [ updateEverything ] -- make sure everything is uptodate.

    -- add/edit entry modals:
    ShowAddEntryModal ->
        case model.user of
            Nothing -> model ! []
            Just u -> showModal (EntryModal.addModal .entryModal EntryModal) { model | entryModal = EntryModal.prepareForAdd u model.entryModal } ! []
    ShowEditEntryModal entry ->
        showModal (EntryModal.editModal .entryModal EntryModal) { model | entryModal = EntryModal.prepareForEdit entry model.entryModal } ! []

    -- handle entry modal updates:
    EntryModal msg ->
      let
        (entryModal', act, cmd) = EntryModal.update msg model.entryModal
        actedModel = case act of
            Just (EntryModal.Added entry) ->
                closeTopModal { model | entries = model.entries ++ [entry] }
            Just (EntryModal.Updated entry) ->
                closeTopModal { model | entries = List.map (\e -> if e.id == entry.id then entry else e) model.entries }
            Just (EntryModal.Removed entryId) ->
                closeTopModal <| closeTopModal <| { model | entries = List.filter (\e -> e.id /= entryId) model.entries }
            Just EntryModal.ShowRemoveEntryModal ->
                showModal (EntryModal.removeModal .entryModal EntryModal) model
            Just EntryModal.CloseMe ->
                closeTopModal model
            _ -> model
      in
        { actedModel | entryModal = entryModal' } ! [Cmd.map EntryModal cmd]

    CloseTopModal ->
        closeTopModal model ! []

    -- perform several actions eg cloing modal and logging in.
    -- done one after the other.
    All ms ->
      let
        folder msg (model,cmds) =
          let (newModel, cmd) = update msg model
          in (newModel, cmd :: cmds)
        (newModel, cmds) = List.foldl folder (model,[]) ms
      in
        (newModel, Cmd.batch cmds)


    Mdl msg' ->
        Material.update msg' model

    Noop ->
        (model, Cmd.none)

-- logs Msg's but hides sensitive information on a case by case:
logMsg : Msg -> Msg
logMsg msg =
  let
    log = Debug.log "update:"
    pwLog pw = String.map (\_ -> '*') pw
    doLog = case msg of
      LoginPassword p -> log (LoginPassword <| pwLog p)
      SetPasswordFirst p -> log (SetPasswordFirst <| pwLog p)
      SetPasswordSecond p -> log (SetPasswordSecond <| pwLog p)
      a -> log a
  in
    msg

-- if the user doesn't have a password set, this shows the set password modal:
showSetPasswordIfNeeded : Model -> Model
showSetPasswordIfNeeded model =
    if Maybe.map .hasPass model.user == Just False
        then showModal (setPasswordModal True) model
        else model

prepareEditCurrentUser : Model -> Model
prepareEditCurrentUser model =
  let
    update u =
        { model
        | userName = u.name
        , userFullName = u.fullName
        , userType = u.userType
        }
  in
    case model.user of
        Nothing -> model
        Just u -> update u

prepareAddUser : Model -> Model
prepareAddUser model =
    { model
    | userName = ""
    , userFullName = ""
    , userType = NormalUser
    , userSaving = False
    , userError = False
    }

prepareEditUser : User -> Model -> Model
prepareEditUser user model =
    { model
    | userName = user.name
    , userFullName = user.fullName
    , userType = user.userType
    , userSaving = False
    , userError = False}

showModal : (Model -> Html Msg) -> Model -> Model
showModal modalFn model =
  let
    modalShower theModel = case theModel of
        TheModel m -> modalFn m
  in
    { model | modals = model.modals ++ [modalShower] }

closeTopModal : Model -> Model
closeTopModal model =
  let
    dropEnd l = case l of
        (b :: []) -> []
        (a :: b) -> a :: dropEnd b
        [] -> []
  in
    { model
    | modals = dropEnd model.modals
    }

doLogin : Model -> Cmd Msg
doLogin model =
  let
    login = Users.login model.loginUserName model.loginPassword
  in
    Task.perform LoginFailed LoginSuccess login

doSetPassword : Model -> Cmd Msg
doSetPassword model =
  let
    setPass u = Users.set u.name { fullName = Nothing, userType = Nothing, pass = Just model.setPasswordFirst }
  in
    case model.user of
        Nothing -> Cmd.none
        Just u -> Task.perform (always SetPasswordFailed) (always SetPasswordSuccess) (setPass u)

doResetPassword : String -> Cmd Msg
doResetPassword username =
  let
    resetPass = Users.set username { fullName = Nothing, userType = Nothing, pass = Just "" }
  in
    Task.perform (always Noop) (always Noop) resetPass

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
    Task.perform (always EditUserFailed) EditUserSuccess addUser

doRemoveUser : Model -> Cmd Msg
doRemoveUser model = Task.perform (always DoneRemoveUser) (always DoneRemoveUser) (Users.remove model.userName)

resetLoginState : Model -> Model
resetLoginState model =
    { model
    | loginUserName = ""
    , loginPassword = ""
    , loginError = Nothing
    , loggingIn = False
    }

updateModelWithUser : User -> Model -> Model
updateModelWithUser user model =
  let
    updateCurrUser m = case m.user of
        Nothing -> m
        Just u -> { m | user = if u.name == user.name then Just user else Just u }
    updateUserList m =
        { m | users = Dict.map (\_ u -> if u.name == user.name then user else u) m.users }
    appendToUserList m =
        { m | users = if Dict.member user.name m.users then m.users else Dict.insert user.name user m.users }
  in
    model |> updateCurrUser |> updateUserList |> appendToUserList

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
    details = case model.user of
        Nothing -> { fullName = "Unknown User", userType = NormalUser }
        Just u -> { fullName = u.fullName, userType = u.userType }
    isAdmin = details.userType == Admin

    -- entries tab with list of entries
    entriesTab =
        div [ class "entries-tab" ]
            [ isLoggedIn ?
                div [ class "add-entry-area" ]
                    [ Button.render Mdl [0,2] model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.ripple
                        , Button.onClick ShowAddEntryModal
                        ]
                        [ text "Add Entry"]
                    ]
            , div [ class "entries" ] <|
                if model.entries == []
                then [ div [ class "no-entries" ] [ text "No entries have been added yet." ] ]
                else List.map (renderEntry model) model.entries
            ]

    -- admin tab with list of users
    usersTab =
        div [ class "users-tab" ]
            [ div [ class "add-user-area" ]
                [ Button.render Mdl [0,3] model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Button.onClick ShowAddUserModal
                    ]
                    [ text "Add User"]
                ]
            , div [ class "users" ]
                (Dict.foldr (\k v users -> renderUser model v :: users) [] model.users)
            ]

  in
    div [ class "content" ]
        [ div [ class "top" ]
            [ div [ class "left" ]
                [ span [ class "logo" ]
                    [ text "TalkLicker"
                    , img [ src "static/logo.svg", class "logo-image" ] []
                    ]
                ]
            , div [ class "right" ]
                [ isLoggedIn ?
                    text details.fullName
                , isLoggedIn ?
                    profileMenu model
                , not isLoggedIn ?
                    Button.render Mdl [0,1] model.mdl
                        [ Button.colored
                        , Button.ripple
                        , Button.onClick ShowLoginModal
                        , cs "login-button"
                        ]
                        [ text "Log In"]
                ]
            , model.loading ?
                div [ class "loading-overlay" ]
                    [ Loading.indeterminate
                    ]
            ]

            , isAdmin ?
                Tabs.render Mdl [0,10] model.mdl
                    [ Tabs.ripple
                    , Tabs.onSelectTab (\i -> SelectTab (if i == 0 then EntriesTab else UsersTab))
                    , Tabs.activeTab (if model.tab == EntriesTab then 0 else 1)
                    , cs "main"
                    ]
                    [ Tabs.label
                        [ Options.center ]
                        [ Icon.i "list"
                        , Options.span [ css "width" "4px" ] []
                        , text "Entries"
                        ]
                    , Tabs.label
                        [ Options.center ]
                        [ Icon.i "group"
                        , Options.span [ css "width" "4px" ] []
                        , text "Users"
                        ]
                    ]
                    [ case model.tab of
                        EntriesTab -> entriesTab
                        UsersTab -> usersTab
                    ]
            , not isAdmin ?
                div [ class "main" ]
                    [ entriesTab ]
        , div [ class "modals" ] (List.map (\modalFunc -> modalFunc (TheModel model)) model.modals)
        ]

profileMenu : Model -> Html Msg
profileMenu model =
  let
    i name =
        Icon.view name [ css "width" "40px" ]
    padding =
        css "padding-right" "24px"
  in
    Menu.render Mdl [1,1] model.mdl
        [ Menu.ripple, Menu.bottomRight ]
        [ Menu.item
            [ Menu.onSelect ShowEditCurrentUserModal, padding ]
            [ i "person", text "Profile" ]
        , Menu.item
            [ Menu.onSelect LogOut, padding ]
            [ i "lock", text "Log out" ]
        ]

renderEntry : Model -> Entry -> Html Msg
renderEntry model e =
  let
    isMine = Maybe.map .name model.user == Just e.user
    isAdmin = Maybe.map .userType model.user == Just Admin
    entryClass = case e.entryType of
        Talk -> "entry-talk"
        Project -> "entry-project"
    entryIcon = case e.entryType of
        Talk -> Icon.i "insert_emoticon"
        Project -> Icon.i "keyboard"
    entryUser = case Dict.get e.user model.users of
        Nothing -> "an Unknown User"
        Just u -> u.fullName
    entryHours = toFloat e.duration / 3600000
  in
    div [ class ("entry " ++ entryClass) ]
        [ div [ class "title" ]
            [ entryIcon
            , if isAdmin || isMine
                then a [ class "text link", onClick (ShowEditEntryModal e) ] [ text e.name ]
                else div [ class "text" ] [ text e.name ]
            ]
        , div [ class "description" ] [ text e.description ]
        , div [ class "user"] [ text ("By " ++ entryUser) ]
        , div [ class "duration" ] [ span [] [ text <| (toString entryHours)++"h" ] ]
        ]

renderUser : Model -> User -> Html Msg
renderUser model user =
  let
    isAdmin = user.userType == Admin
  in
    div [ class ("user " ++ if isAdmin then "user-admin" else "user-normal") ]
        [ div [ class "title" ]
            [ Icon.i (if isAdmin then "star" else "person")
            , a [ class "text link", onClick (ShowEditUserModal user) ] [ text user.name ]
            ]
        , div [ class "name" ] [ text user.fullName ]
        , div [ class "type"] [ text (if isAdmin then "Administrator" else "Normal User") ]
        , not user.hasPass ? div [ class "is-new" ] [ text "New" ]
        ]

loginModal : Model -> Html Msg
loginModal model =
  let
    invalid = model.loginUserName == ""
    loginErrorString = case model.loginError of
        Just LoginBadUser -> "Wrong username"
        Just LoginBadPassword -> "Wrong password"
        _ -> "Something Untoward Transpired"
    opts =
        { title = text "Login"
        , preventClose = model.loggingIn
        , hideClose = False
        , isLoading = model.loggingIn
        , onClose = All [CloseTopModal, ClearLoginDetails]
        , mdl = Mdl
        , content =
            div [ class "login-modal" ]
            [ div [ class "inputs" ]
                [ Textfield.render Mdl [70,0] model.mdl
                    [ Textfield.label "Username"
                    , Textfield.floatingLabel
                    , Textfield.onInput LoginUserName
                    , Textfield.value model.loginUserName
                    ]
                , Textfield.render Mdl [70,1] model.mdl
                    [ Textfield.label "Password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    , Textfield.onInput LoginPassword
                    , Textfield.value model.loginPassword
                    ]
                ]
            , div [ class "bottom-row" ]
                [ Button.render Mdl [0] model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.disabled `when` invalid
                    , Button.onClick DoLogin
                    , cs "login-button"
                    ]
                    [ text "Login" ]
                , model.loginError ??
                    div [ class "error" ]
                        [ text loginErrorString ]
                ]
            ]
        }
  in
    Modals.render opts model

setPasswordModal : Bool -> Model -> Html Msg
setPasswordModal bNeedsSetting model =
  let
    invalid = model.setPasswordFirst /= model.setPasswordSecond || String.length model.setPasswordFirst == 0
    opts =
        { title = text "Set Password"
        , preventClose = False
        , isLoading = model.setPasswordSaving
        , hideClose = bNeedsSetting
        , onClose = CloseTopModal
        , mdl = Mdl
        , content =
            div [ class "set-password-modal" ]
                [ bNeedsSetting ?
                    div [ class "needs-setting-text" ]
                        [ text "You have not yet set a password. Please do so now." ]
                , div [ class "inputs" ]
                    [ Textfield.render Mdl [71,1] model.mdl
                        [ Textfield.label "New Password"
                        , Textfield.floatingLabel
                        , Textfield.password
                        , Textfield.onInput SetPasswordFirst
                        , Textfield.value model.setPasswordFirst
                        ]
                    , Textfield.render Mdl [71,2] model.mdl
                        [ Textfield.label "New Password Again"
                        , Textfield.floatingLabel
                        , Textfield.password
                        , Textfield.onInput SetPasswordSecond
                        , Textfield.value model.setPasswordSecond
                        ]
                    ]
                , div [ class "bottom-row" ]
                    [ Button.render Mdl [71,3] model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.disabled `when` invalid
                        , Button.onClick DoSetPassword
                        , cs "set-password-button"
                        ]
                        [ text "Set Password" ]
                    , (invalid && model.setPasswordSecond /= "") ?
                        div [ class "error" ]
                            [ text "Passwords do not match" ]
                    ]
                ]
        }
  in
    Modals.render opts model

logoutModal : Model -> Html Msg
logoutModal model =
  let
    opts =
        { title = "Logout"
        , icon = "lock"
        , message = "Are you sure you want to log out?"
        , onPerform = All [CloseTopModal, DoLogout]
        , performText = "Log out"
        , onCancel = CloseTopModal
        , cancelText = "Cancel"
        , hidePerform = False
        , hideCancel = False
        , mdl = Mdl
        }
  in
    Modals.choice opts model

addUserModal : Model -> Html Msg
addUserModal model =
  let
    opts =
        { title = text "Add User"
        , isLoading = model.userSaving
        , preventClose = False
        , hideClose = False
        , onClose = CloseTopModal
        , mdl = Mdl
        , content = userModalHtml False model
        }
  in
    Modals.render opts model

editUserModal : Model -> Html Msg
editUserModal model =
  let
    opts =
        { title = text "Edit User"
        , isLoading = model.userSaving
        , preventClose = False
        , hideClose = False
        , onClose = CloseTopModal
        , mdl = Mdl
        , content = userModalHtml True model
        }
  in
    Modals.render opts model

editCurrentUserModal : Model -> Html Msg
editCurrentUserModal model =
  let
    opts =
        { title = text "Profile"
        , isLoading = model.userSaving
        , preventClose = False
        , hideClose = False
        , onClose = CloseTopModal
        , mdl = Mdl
        , content = userModalHtml True model
        }
  in
    Modals.render opts model

removeUserModal : Model -> Html Msg
removeUserModal model =
  let
    opts =
        { title = "Remove User"
        , icon = "warning"
        , message = "Are you sure you want to remove this user? This will also delete any Entries associated with them."
        , onPerform = All [CloseTopModal, CloseTopModal, DoRemoveUser] -- close the "Edit user" modal we came from as well.
        , performText = "Remove"
        , onCancel = CloseTopModal
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
    isAdmin = Maybe.map .userType model.user == Just Admin
    isMe = isEditMode && Maybe.map .name model.user == Just model.userName
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
                        , Button.onClick ShowSetPasswordModal
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
        , onPerform = All [CloseTopModal, DoResetPassword]
        , performText = "Reset"
        , onCancel = CloseTopModal
        , cancelText = "Cancel"
        , hidePerform = False
        , hideCancel = False
        , mdl = Mdl
        }
  in
    Modals.choice opts model

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
init = (model, updateEverything)

type alias CoreDetails =
    { currentUser : Maybe User, entries : List Entry, users : Dict String User }

updateEverything : Cmd Msg
updateEverything = Task.perform ApiError UpdateCoreDetails getEverything

getEverything : Task Api.Error CoreDetails
getEverything =
  let
    fn = \curr entries users ->
        { currentUser = curr, entries = entries, users = users }
  in
    Task.map3 fn Users.current Entries.get Users.get
