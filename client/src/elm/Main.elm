import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task exposing (Task)
import Debug
import Dict exposing (Dict)
import List
import String
import Markdown
--import Process
--import Time

import Material
--import Material.Scheme
--import Material.List as Lists
import Material.Options as Options exposing (when, css, cs)
import Material.Icon as Icon
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Progress as Loading
--import Material.Toggles as Toggles
import Material.Menu as Menu
import Material.Tabs as Tabs

import Modals
import Modals.Entry as EntryModal
import Modals.User as UserModal
import Html.Helpers exposing (..)

import Dnd

import Api
import Api.Entries as Entries exposing (Entry, EntryType(..), EntryError(..))
import Api.Users as Users exposing (User, UserType(..), LoginError(..))

--
-- Model
--

initialModel : Model
initialModel =
    { loading = True
    , isAdminMode = False

    , tab = EntriesTab
    , entries = []
    , entriesDnd = Dnd.model
    , users = Dict.empty
    , modals = []
    , error = ""
    , user = Nothing

    -- login modal:
    , loginUserName = ""
    , loginPassword = ""
    , loggingIn = False
    , loginError = Nothing

    -- add/edit user modal:
    , userModal = UserModal.model

    -- entry stuff for add/edit:
    , entryModal = EntryModal.model

    , mdl = Material.model
    }

type alias Model =
    { loading : Bool
    , isAdminMode : Bool

    , tab : Tab
    , entries : List Entry
    , entriesDnd : Dnd.Model
    , users : Dict String User
    , modals : List (TheModel -> Html Msg)
    , error : String
    , user : Maybe User

    -- login modal:
    , loginUserName : String
    , loginPassword : String
    , loggingIn : Bool
    , loginError : Maybe LoginError

    -- add/edit user modal:
    , userModal : UserModal.Model

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

    | EntriesDnd Dnd.Msg

    -- admin mode
    | ToggleAdminMode

    -- login modal:
    | ShowLoginModal
    | LoginUserName String
    | LoginPassword String
    | DoLogin
    | LoginFailed LoginError
    | LoginSuccess User
    | ClearLoginDetails

    -- add/edit user modal:
    | ShowEditCurrentUserModal
    | ShowAddUserModal
    | ShowEditUserModal User
    | UserModal UserModal.Msg

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
        showSetPasswordIfNeeded { model | loading = False, user = core.currentUser, entries = core.entries, users = core.users }
            ! [] -- [ Task.perform ApiError UpdateCoreDetails <| Process.sleep (30 * Time.second) `Task.andThen` \_ -> getEverything ]
    LogOut ->
        showModal logoutModal model ! []
    DoLogout ->
        { model | user = Nothing, tab = EntriesTab } ! [Task.perform (always Noop) (always Noop) Users.logout]

    -- admin mode:
    ToggleAdminMode ->
        { model | isAdminMode = not model.isAdminMode } ! []

    -- handle DND output:
    EntriesDnd msg ->
      let
        (dnd, mAct) = Dnd.update msg model.entriesDnd
        (actedModel, actedCmd) = case mAct of
            Just (Dnd.MovedTo id (before, after)) -> moveEntryTo id before after model
            Nothing -> model ! []
      in
        { actedModel | entriesDnd = dnd } ! [ actedCmd ]

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
        case model.user of
            Nothing -> model ! []
            Just u ->
              showModal
              (UserModal.profileModal .userModal UserModal)
              { model | userModal = UserModal.prepareForEdit model.user u model.userModal } ! []
    ShowEditUserModal user ->
        showModal (UserModal.editModal .userModal UserModal) { model | userModal = UserModal.prepareForEdit model.user user model.userModal } ! []
    ShowAddUserModal ->
        showModal (UserModal.addModal .userModal UserModal) { model | userModal = UserModal.prepareForAdd model.user model.userModal }  ! []
    UserModal msg ->
        handleUserUpdate msg model

    -- add/edit entry modals:
    ShowAddEntryModal ->
        case model.user of
            Nothing -> model ! []
            Just u -> showModal (EntryModal.addModal .entryModal EntryModal) { model | entryModal = EntryModal.prepareForAdd u model.entryModal } ! []
    ShowEditEntryModal entry ->
        showModal (EntryModal.editModal .entryModal EntryModal) { model | entryModal = EntryModal.prepareForEdit entry model.entryModal } ! []
    EntryModal msg ->
        handleEntryUpdate msg model

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

-- handle moving an entry given DND finish
moveEntryTo : String -> Dnd.Position -> Dnd.Position -> Model -> (Model, Cmd Msg)
moveEntryTo id before after model =
  let
    entrySingleton = List.filter (\e -> e.id == id) model.entries
    entries = List.filter (\e -> e.id /= id) model.entries
    tryAddBeforeId entry id =
      let newList = List.foldr (\e out -> if e.id == id then entry :: e :: out else e :: out) [] entries
      in if List.length newList /= List.length entries + 1 then model.entries else newList
    moved entry = case (before, after) of
        (_, Dnd.AtEnd)  -> (entries ++ [entry], Entries.AtEnd)
        (_, Dnd.AtId b) -> (tryAddBeforeId entry b, Entries.AtBefore b)
        _               -> Debug.crash ("impossible position "++toString (before,after))
  in
    case entrySingleton of
        [entry] ->
          let
            (entries', ePos) = moved entry
            orderApi = Task.perform (always Noop) (always Noop) (Entries.move id ePos)
          in
            { model | entries = entries' } ! [ orderApi ]
        _ -> model ! [] -- entry not found

-- handle updates to the userModal
handleUserUpdate : UserModal.Msg -> Model -> (Model, Cmd Msg)
handleUserUpdate msg model =
  let
    (userModal', act, cmd) = UserModal.update msg model.userModal
    (actedModel, aCmd) = case act of
        Just (UserModal.Added user) ->
            closeTopModal { model | users = Dict.insert user.name user model.users } ! []
        Just (UserModal.Updated user) ->
            closeTopModal
                { model
                | users = Dict.insert user.name user model.users
                , user = if Maybe.map .name model.user == Just user.name then Just user else model.user
                } ! []
        Just (UserModal.Removed userId) ->
            closeTopModal { model | users = Dict.remove userId model.users } ! [updateEverything]
        Just UserModal.CloseMe ->
            closeTopModal model ! []
        Just UserModal.SetPasswordDone ->
            closeTopModal model ! []
        Nothing ->
            model ! []
  in
    { actedModel | userModal = userModal' } ! [ aCmd, Cmd.map UserModal cmd ]

handleEntryUpdate : EntryModal.Msg -> Model -> (Model, Cmd Msg)
handleEntryUpdate msg model =
  let
    (entryModal', act, cmd) = EntryModal.update msg model.entryModal
    actedModel = case act of
        Just (EntryModal.Added entry) ->
            closeTopModal { model | entries = model.entries ++ [entry] }
        Just (EntryModal.Updated entry) ->
            closeTopModal { model | entries = List.map (\e -> if e.id == entry.id then entry else e) model.entries }
        Just (EntryModal.Removed entryId) ->
            closeTopModal { model | entries = List.filter (\e -> e.id /= entryId) model.entries }
        Just EntryModal.CloseMe ->
            closeTopModal model
        Nothing ->
            model
  in
    { actedModel | entryModal = entryModal' } ! [Cmd.map EntryModal cmd]

-- logs Msg's but hides sensitive information on a case by case:
logMsg : Msg -> Msg
logMsg msg =
  let
    log = Debug.log "update:"
    pwLog pw = String.map (\_ -> '*') pw
    doLog = case msg of
      LoginPassword p -> log (LoginPassword <| pwLog p)
      UserModal (UserModal.SetPasswordFirst p) -> log (UserModal <| UserModal.SetPasswordFirst <| pwLog p)
      UserModal (UserModal.SetPasswordSecond p) -> log (UserModal <| UserModal.SetPasswordSecond <| pwLog p)
      a -> log a
  in
    msg

-- if the user doesn't have a password set, this shows the set password modal:
showSetPasswordIfNeeded : Model -> Model
showSetPasswordIfNeeded model = case model.user of
    Just u ->
        if u.hasPass == False
        then showModal
             (UserModal.setPasswordModal .userModal UserModal)
             { model | userModal = UserModal.prepareSetPass u model.userModal }
        else model
    Nothing -> model

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

resetLoginState : Model -> Model
resetLoginState model =
    { model
    | loginUserName = ""
    , loginPassword = ""
    , loginError = Nothing
    , loggingIn = False
    }

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
                else if isLoggedIn
                    then [ Dnd.view model.entriesDnd EntriesDnd <| List.map (\e -> (e.id, renderEntry model e)) model.entries ]
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
        [ div [ class ("top"++if model.isAdminMode then " is-admin-mode" else "") ]
            [ div [ class "left" ]
                [ span [ class "logo" ]
                    [ text "TalkLicker"
                    , img [ src "static/logo.svg", class "logo-image" ] []
                    ]
                ]
            , div [ class "right" ]
                [ isLoggedIn && isAdmin && not model.isAdminMode ?
                    div [ class "admin-mode", onClick ToggleAdminMode ] [ text "Admin Mode" ]
                , isLoggedIn && isAdmin && model.isAdminMode ?
                    div [ class "admin-mode enabled", onClick ToggleAdminMode ] [ text "Admin Mode" ]
                , isLoggedIn ?
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

            , model.isAdminMode ?
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
            , not model.isAdminMode ?
                div [ class "main" ]
                    [ entriesTab ]
        , div [ class "modals" ] (List.map (\modalFunc -> modalFunc (TheModel model)) model.modals)
        , if Dnd.beingDragged model.entriesDnd
            then draggingEntry model
            else text ""
        ]

draggingEntry : Model -> Html Msg
draggingEntry model =
  let
    draggedId = Dnd.draggedId model.entriesDnd
    entry = case List.filter (\e -> e.id == draggedId) model.entries of
        [e] -> renderEntry model e
        _ -> text ""
    pos = Dnd.position model.entriesDnd
    css =
        [ ("position","absolute")
        , ("top", toString pos.y ++ "px")
        , ("left", toString pos.x ++ "px") ]
  in
    div [ class "dragging-entry"
        , style css ]
        [ entry ]

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
    markdownOpts = let d = Markdown.defaultOptions in { d | sanitize = True }
  in
    div [ class ("entry " ++ entryClass) ]
        [ div [ class "title" ]
            [ entryIcon
            , if model.isAdminMode || isMine
                then a [ class "text link", onClick (ShowEditEntryModal e) ] [ text e.name ]
                else div [ class "text" ] [ text e.name ]
            ]
        , div [ class "description" ] [ Markdown.toHtmlWith markdownOpts [class "markdown"] e.description ]
        , div [ class "user"] [ text ("By " ++ entryUser) ]
        , div [ class "duration" ] [ span [] [ text <| (toString entryHours)++"h" ] ]
        ]

renderUser : Model -> User -> Html Msg
renderUser model user =
    div [ class ("user " ++ if model.isAdminMode then "user-admin" else "user-normal") ]
        [ div [ class "title" ]
            [ Icon.i (if model.isAdminMode then "star" else "person")
            , a [ class "text link", onClick (ShowEditUserModal user) ] [ text user.name ]
            ]
        , div [ class "name" ] [ text user.fullName ]
        , div [ class "type"] [ text (if model.isAdminMode then "Administrator" else "Normal User") ]
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
        , cover = []
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
        [
            Sub.map EntriesDnd (Dnd.sub model.entriesDnd)
        ]
    }

init : (Model, Cmd Msg)
init = (initialModel, updateEverything)

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
