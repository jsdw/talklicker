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
    , entryId = ""
    , entryUser = ""
    , entryDuration = 3600000
    , entryName = ""
    , entryDescription = ""
    , entryType = Talk
    , entrySaving = False
    , entryError = Nothing

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
    , entryId : String
    , entryUser : String
    , entryDuration : Int
    , entryName : String
    , entryDescription : String
    , entryType : EntryType
    , entrySaving : Bool
    , entryError : Maybe EntryError

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

    -- add/edit entry modal:
    | ShowAddEntryModal
    | ShowEditEntryModal Entry
    | UpdateEntryName String
    | UpdateEntryDescription String
    | UpdateEntryType EntryType
    | UpdateEntryDuration Int
    | DoAddEntry
    | AddEntryFailed EntryError
    | AddEntrySuccess Entry
    | DoEditEntry
    | EditEntryFailed EntryError
    | EditEntrySuccess Entry

    -- remove entry alert/action (from add/entry modal)
    | ShowRemoveEntryModal
    | DoRemoveEntry
    | DoneRemoveEntry

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

    -- add/edit entry modals:
    ShowAddEntryModal ->
        showModal addEntryModal (prepareModelForAddEntry model) ! []
    ShowEditEntryModal entry ->
        showModal editEntryModal (prepareModelForEditEntry entry model) ! []
    UpdateEntryName val ->
        { model | entryName = val } ! []
    UpdateEntryDescription val ->
        { model | entryDescription = val } ! []
    UpdateEntryType val ->
        { model | entryType = val } ! []
    UpdateEntryDuration val ->
        { model | entryDuration = val } ! []
    DoAddEntry ->
        { model | entrySaving = True } ! [doAddEntry model]
    AddEntrySuccess entry ->
        closeTopModal { model | entries = model.entries ++ [entry], entrySaving = False } ! []
    AddEntryFailed err ->
        { model | entrySaving = False, entryError = Just err } ! []
    DoEditEntry ->
        { model | entrySaving = True } ! [doEditEntry model]
    EditEntrySuccess entry ->
      let
        entries' = List.map (\e -> if e.id == entry.id then entry else e) model.entries
      in
        closeTopModal { model | entries = entries', entrySaving = False } ! []
    EditEntryFailed err ->
        { model | entrySaving = False, entryError = Just err } ! []

    -- remove an entry
    ShowRemoveEntryModal ->
        showModal removeEntryModal model ! []
    DoRemoveEntry ->
      let
        entries' = List.filter (\e -> e.id /= model.entryId) model.entries
      in
        { model | entries = entries' } ! [doRemoveEntry model]
    DoneRemoveEntry ->
        model ! [] -- Do nothing at the mo.
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

prepareModelForAddEntry : Model -> Model
prepareModelForAddEntry model =
  let
    doUpdate user =
        { model
        | entryUser = user.name
        , entryDuration = 3600000
        , entryName = ""
        , entryDescription = ""
        , entryType = Talk
        , entryError = Nothing
        }
  in
    case model.user of
        Nothing -> model
        Just u -> doUpdate u

prepareModelForEditEntry : Entry -> Model -> Model
prepareModelForEditEntry entry model =
    { model
    | entryId = entry.id
    , entryUser = entry.user
    , entryDuration = entry.duration
    , entryName = entry.name
    , entryDescription = entry.description
    , entryType = entry.entryType
    , entryError = Nothing
    }

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

showModal : (Model -> ModalOptions Msg Model) -> Model -> Model
showModal modal model =
  let
    modalShower theModel = case theModel of
        TheModel m -> renderModal m (modal m)
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

resetLoginState : Model -> Model
resetLoginState model =
    { model
    | loginUserName = ""
    , loginPassword = ""
    , loginError = Nothing
    , loggingIn = False
    }

entryishFromModel : Model -> Entries.EntrySettable {}
entryishFromModel model =
    { id = model.entryId
    , user = model.entryUser
    , duration = model.entryDuration
    , name = model.entryName
    , description = model.entryDescription
    , entryType = model.entryType
    }

doAddEntry : Model -> Cmd Msg
doAddEntry model =
  let
    entry = entryishFromModel model
  in
    Task.perform AddEntryFailed AddEntrySuccess (Entries.add entry)

doEditEntry : Model -> Cmd Msg
doEditEntry model =
  let
    entry = entryishFromModel model
  in
    Task.perform EditEntryFailed EditEntrySuccess (Entries.set entry)

updateModelWithUser : User -> Model -> Model
updateModelWithUser user model =
  let
    updateCurrUser m = case m.user of
        Nothing -> m
        Just u -> { m | user = if u.name == user.name then Just user else Just u }
    updateUserList m =
        { m | users = Dict.map (\_ u -> if u.name == user.name then user else u) m.users }
  in
    model |> updateCurrUser |> updateUserList

doRemoveEntry : Model -> Cmd Msg
doRemoveEntry model =
  Task.perform ApiError (\_ -> DoneRemoveEntry) (Entries.remove model.entryId)

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
                    [ text "TalkLicker" ]
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
                entriesTab
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
    entryHours = round (toFloat e.duration / 3600000)
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
        ]

loginModal : Model -> ModalOptions Msg Model
loginModal model =
  let
    invalid = model.loginUserName == ""
    loginErrorString = case model.loginError of
        Just LoginBadUser -> "Wrong username"
        Just LoginBadPassword -> "Wrong password"
        _ -> "Network error"
  in
    { defaultModalOptions
    | title = text "Login"
    , onClose = Just ClearLoginDetails
    , preventClose = \model -> model.loggingIn
    , isLoading = \model -> model.loggingIn
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

setPasswordModal : Bool -> Model -> ModalOptions Msg Model
setPasswordModal bNeedsSetting model =
  let
    invalid = model.setPasswordFirst /= model.setPasswordSecond || String.length model.setPasswordFirst == 0
  in
    { defaultModalOptions
    | title = text "Set Password"
    , isLoading = \model -> model.setPasswordSaving
    , hideClose = always bNeedsSetting
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

logoutModal : Model -> ModalOptions Msg Model
logoutModal model =
  let
    opts =
        { defaultWarningModalOptions
        | title = "Logout"
        , icon = "lock"
        , message = "Are you sure you want to log out?"
        , onPerform = Just DoLogout
        , performText = "Log out"
        }
  in
    choiceModal opts model

editUserModal : Model -> ModalOptions Msg Model
editUserModal model =
    { defaultModalOptions
    | title = text "Edit User"
    , isLoading = \model -> model.userSaving
    , content = userModalHtml True model
    }

editCurrentUserModal : Model -> ModalOptions Msg Model
editCurrentUserModal model =
    { defaultModalOptions
    | title = text "Profile"
    , isLoading = \model -> model.userSaving
    , content = userModalHtml True model
    }

addUserModal : Model -> ModalOptions Msg Model
addUserModal model =
    { defaultModalOptions
    | title = text "Add User"
    , isLoading = \model -> model.userSaving
    , content = userModalHtml False model
    }

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
            , (isAdmin && not isMe) ?
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
            , model.userError ?
                div [ class "error" ]
                    [ text "Username taken" ]
            ]
        ]

addEntryModal : Model -> ModalOptions Msg Model
addEntryModal model =
    { defaultModalOptions
    | title = text "Add Entry"
    , isLoading = \model -> model.entrySaving
    , content = entryModalHtml False model
    }

editEntryModal : Model -> ModalOptions Msg Model
editEntryModal model =
    { defaultModalOptions
    | title = text "Edit Entry"
    , isLoading = \model -> model.entrySaving
    , content = entryModalHtml True model
    }

entryModalHtml : Bool -> Model -> Html Msg
entryModalHtml isEditMode model =
  let
    errorString = case model.entryError of
        Just EntryBadName -> "Entry name required"
        Just EntryBadDescription -> "Entry description required"
        Just EntryBadDuration -> "Entry duration required"
        _ -> "Something unsettling happened!"
    durationString =
      let
        hours = round (toFloat model.entryDuration / (60 * 60 * 1000))
      in
        if hours == 1 then "1 hour"
        else if hours < 8 then toString hours ++ " hours"
        else if hours == 8 then "1 day"
        else toString (toFloat hours / 8) ++ " days"
  in
    div [ class "entry-modal" ]
        [ table [ class "inputs" ]
            [ inputRow "Title" <|
                Textfield.render Mdl [7,0] model.mdl
                    [ Textfield.label "My talk or project"
                    , Textfield.onInput UpdateEntryName
                    , Textfield.value model.entryName
                    ]
            , inputRow "Description" <|
                Textfield.render Mdl [7,1] model.mdl
                    [ Textfield.label "More detail"
                    , Textfield.textarea
                    , Textfield.rows 6
                    , Textfield.onInput UpdateEntryDescription
                    , Textfield.value model.entryDescription
                    ]
            , inputRow "Type" <|
                div [ class "type-inputs" ]
                    [ Toggles.radio Mdl [7,2] model.mdl
                        [ Toggles.value (model.entryType == Talk)
                        , Toggles.group "EntryType"
                        , Toggles.ripple
                        , Toggles.onClick (UpdateEntryType Talk)
                        ]
                        [ text "Talk" ]
                    , Toggles.radio Mdl [7,3] model.mdl
                        [ Toggles.value (model.entryType == Project)
                        , Toggles.group "EntryType"
                        , Toggles.ripple
                        , Toggles.onClick (UpdateEntryType Project)
                        ]
                        [ text "Project" ]
                    ]
            , inputRow "Duration" <|
                div [ class "duration-input" ]
                    [ text durationString
                    , Menu.render Mdl [0] model.mdl
                        [ Menu.ripple, Menu.bottomRight ]
                        [ Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (1 * 3600000)) ]
                            [ text "1 hour" ]
                        , Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (2 * 3600000)) ]
                            [ text "2 hours" ]
                        , Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (4 * 3600000)) ]
                            [ text "4 hours" ]
                        , Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (1 * 8 * 3600000)) ]
                            [ text "1 day" ]
                        , Menu.item
                            [ Menu.onSelect (UpdateEntryDuration (2 * 8 * 3600000)) ]
                            [ text "2 days" ]
                        ]
                    ]
            ]
        , div [ class "bottom-row" ]
            [ Button.render Mdl [0] model.mdl
                [ Button.raised
                , Button.colored
                , Button.disabled `when` (model.entryName == "" || model.entryDescription == "")
                , Button.onClick (if isEditMode then DoEditEntry else DoAddEntry)
                , cs "add-entry-button"
                ]
                [ text (if isEditMode then "Save Changes" else "Add Entry") ]
            , if isJust model.entryError
                then div [ class "error" ] [ text errorString ]
                else div [ class "space-filler" ] []
            , isEditMode ?
                div [ class "delete" ]
                    [ Button.render Mdl [0] model.mdl
                        [ Button.icon
                        , Button.ripple
                        , Button.onClick ShowRemoveEntryModal
                        ]
                        [ Icon.i "delete"]
                    ]
            ]
        ]

inputRow : String -> Html a -> Html a
inputRow title html =
  let
    key = String.map (\c -> if c == ' ' then '-' else c) <| String.toLower title
  in
    tr [ class ("input-row input-row-"++key) ]
        [ td [ class ("input-name input-name-"++key) ] [ text title ]
        , td [ class "input-widget" ] [ html ]
        ]

resetPasswordModal : Model -> ModalOptions Msg Model
resetPasswordModal model =
  let
    opts =
        { defaultWarningModalOptions
        | title = "Reset Password"
        , message = "Are you sure you want to reset this users password?"
        , onPerform = Just DoResetPassword
        , performText = "Reset"
        }
  in
    choiceModal opts model

removeEntryModal : Model -> ModalOptions Msg Model
removeEntryModal model =
  let
    opts =
        { defaultWarningModalOptions
        | title = "Remove Entry"
        , message = "Are you sure you want to remove this entry?"
        , onPerform = Just (All [CloseTopModal, DoRemoveEntry]) -- close the "Edit entry" modal we came from as well.
        , performText = "Remove"
        }
  in
    choiceModal opts model

--
-- A more specific version of general modals aimed
-- at showing alerts/warnings/confirms:
--

type alias ChoiceModalOptions msg =
    { title : String
    , icon : String
    , message : String
    , onCancel : Maybe msg
    , onPerform : Maybe msg
    , cancelText : String
    , performText : String
    , hidePerform : Bool
    , hideCancel : Bool
    }

defaultWarningModalOptions : ChoiceModalOptions Msg
defaultWarningModalOptions =
    { title = "Warning"
    , icon = "warning"
    , message = "Are you sure you want to do this?"
    , onCancel = Nothing
    , cancelText = "Dismiss"
    , hideCancel = False
    , onPerform = Nothing
    , performText = "Perform"
    , hidePerform = False
    }

choiceModal : ChoiceModalOptions Msg -> Model -> ModalOptions Msg Model
choiceModal opts model =
  let
    performMsg = All ([CloseTopModal] ++ case opts.onPerform of
        Nothing -> []
        Just msg -> [msg])
    cancelMsg = All ([CloseTopModal] ++ case opts.onCancel of
        Nothing -> []
        Just msg -> [msg])
  in
    { defaultModalOptions
    | title = text opts.title
    , onClose = opts.onCancel
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
                    Button.render Mdl [200,0] model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.onClick cancelMsg
                        , cs "cancel-button"
                        ]
                        [ text opts.cancelText ]
                , not opts.hidePerform ?
                    Button.render Mdl [200,1] model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.onClick performMsg
                        , cs "perform-button"
                        ]
                        [ text opts.performText ]
                ]
            ]
    }

--
-- View - Modal maker
--

renderModal : Model -> ModalOptions Msg Model -> Html Msg
renderModal model {title,content,onClose,preventClose,hideClose,isLoading} =
  let
    closeMsgs = All ([CloseTopModal] ++ case onClose of
        Nothing -> []
        Just msg -> [msg])
  in
    div [ class "modal-background" ]
        [ div [ class "modal-inner" ]
            [ div [ class "title" ]
                [ div [ class "title-inner" ] [ title ]
                , not (hideClose model) ?
                    Button.render Mdl [100,0] model.mdl
                        [ Button.icon
                        , Button.plain
                        , Button.onClick closeMsgs
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

type alias ModalOptions msg model =
    { title : Html msg
    , content : Html msg
    , preventClose : model -> Bool
    , hideClose : model -> Bool
    , isLoading : model -> Bool
    , onClose : Maybe msg
    }

defaultModalOptions : ModalOptions Msg Model
defaultModalOptions =
    { title = text ""
    , content = div [] []
    , preventClose = always False
    , hideClose = always False
    , isLoading = always False
    , onClose = Nothing
    }

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

--
-- Utils
--

isJust : Maybe a -> Bool
isJust m = case m of
    Nothing -> False
    Just _ -> True

(?) : Bool -> Html a -> Html a
(?) b html = if b then html else noNode

(!?) : Bool -> Html a -> Html a
(!?) b html = if b then noNode else html

(??) : Maybe m -> Html a -> Html a
(??) m html = if isJust m then html else noNode

(!??) : Maybe m -> Html a -> Html a
(!??) m html = if isJust m then noNode else html

noNode : Html a
noNode = Html.node "nothing" [ style [("position", "absolute"), ("display", "none")] ] []