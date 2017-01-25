module Main exposing (..)

import Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Task exposing (Task)
import Task.Extra exposing (perform)
import Debug
import Dict exposing (Dict)
import List
import String
import Markdown
import Set exposing (Set)


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
import Modals.Day as DayModal
import Modals.User as UserModal
import Html.Helpers exposing (..)
import Dnd
import Api
import Api.Entries as Entries exposing (Entry, EntryType(..), EntryError(..))
import Api.Users as Users exposing (User, UserType(..), LoginError(..))
import Api.Days as Days exposing (Day)


--
-- Model
--


initialModel : Model
initialModel =
    { loading = True
    , isAdminMode = False
    , tab = EntriesTab
    , entries = Dict.empty
    , entryIds = []
    , days = []
    , entriesDnd = Dnd.model
    , users = Dict.empty
    , modals = []
    , error = ""
    , user =
        Nothing
        -- login modal:
    , loginUserName = ""
    , loginPassword = ""
    , loggingIn = False
    , loginError =
        Nothing
        -- add/edit user modal:
    , userModal =
        UserModal.model
        -- entry stuff for add/edit:
    , entryModal =
        EntryModal.model
        -- day stuff:
    , dayModal = DayModal.model
    , mdl = Material.model
    }


type alias Model =
    { loading : Bool
    , isAdminMode : Bool
    , tab : Tab
    , entryIds : List String
    , entries : Dict String Entry
    , days : List Day
    , entriesDnd : Dnd.Model DnDLocation EntryDndKey
    , users : Dict String User
    , modals : List (TheModel -> Html Msg)
    , error : String
    , user :
        Maybe User
        -- login modal:
    , loginUserName : String
    , loginPassword : String
    , loggingIn : Bool
    , loginError :
        Maybe LoginError
        -- add/edit user modal:
    , userModal :
        UserModal.Model
        -- entry stuff for add/edit:
    , entryModal :
        EntryModal.Model
        -- day stuff:
    , dayModal : DayModal.Model
    , mdl : Material.Model
    }



-- allow Model alias to reference itself by
-- hiding itself inside a real type:


type TheModel
    = TheModel Model



-- This is what we use to identify indicidual DND items;
-- their IDs.


type alias EntryDndKey =
    String



-- where is the DnD item? in the entries list, or
-- in a day with some id and at some index corresponding to
-- a list of entries in the day


type DnDLocation
    = InEntriesList
    | InDay String Int



-- what page are we viewing?


type Tab
    = EntriesTab
    | UsersTab



--
-- Update
--


type Msg
    = SelectTab Tab
    | ApiError Api.Error
    | UpdateCoreDetails CoreDetails
    | LogOut
    | DoLogout
    | EntriesDnd (Dnd.Msg DnDLocation EntryDndKey)
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
      -- add/edit/remove day:
    | ShowAddDayModal
    | ShowEditDayModal Day
    | DayModal DayModal.Msg
    | ShowDayCompleteModal Day
    | DoCompleteDay Day
    | DoneCompleteDay Day
      -- perform several actions at once
    | All (List Msg)
    | CloseTopModal
    | Mdl (Material.Msg Msg)
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case logMsg msg of
        SelectTab tab ->
            { model | tab = tab } ! []

        ApiError error ->
            { model | loading = False, error = toString error } ! []

        UpdateCoreDetails core ->
            showSetPasswordIfNeeded
                (setEntries core.entries { model | loading = False, user = core.currentUser, users = core.users, days = List.reverse core.days })
                ! []

        -- [ perform ApiError UpdateCoreDetails <| Process.sleep (30 * Time.second) `Task.andThen` \_ -> getEverything ]
        LogOut ->
            showModal logoutModal model ! []

        DoLogout ->
            { model | isAdminMode = False, user = Nothing, tab = EntriesTab } ! [ perform (always Noop) (always Noop) Users.logout ]

        -- admin mode:
        ToggleAdminMode ->
            { model | isAdminMode = not model.isAdminMode } ! []

        -- handle DND output:
        EntriesDnd msg ->
            let
                ( dnd, mAct ) =
                    Dnd.update msg model.entriesDnd

                ( actedModel, actedCmd ) =
                    case mAct of
                        Just (Dnd.MovedTo from to) ->
                            moveEntryTo from to model

                        Nothing ->
                            model ! []
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
            { model | loggingIn = True, loginError = Nothing } ! [ doLogin model ]

        LoginFailed err ->
            { model | loggingIn = False, loginError = Just err } ! []

        LoginSuccess user ->
            (showSetPasswordIfNeeded <| resetLoginState <| closeTopModal { model | user = Just user }) ! []

        ClearLoginDetails ->
            resetLoginState model ! []

        -- add/edit user modal:
        ShowEditCurrentUserModal ->
            case model.user of
                Nothing ->
                    model ! []

                Just u ->
                    showModal
                        (UserModal.profileModal .userModal UserModal)
                        { model | userModal = UserModal.prepareForEdit model.user u model.userModal }
                        ! []

        ShowEditUserModal user ->
            showModal (UserModal.editModal .userModal UserModal) { model | userModal = UserModal.prepareForEdit model.user user model.userModal } ! []

        ShowAddUserModal ->
            showModal (UserModal.addModal .userModal UserModal) { model | userModal = UserModal.prepareForAdd model.user model.userModal } ! []

        UserModal msg ->
            handleUserUpdate msg model

        -- add/edit entry modals:
        ShowAddEntryModal ->
            case model.user of
                Nothing ->
                    model ! []

                Just u ->
                    showModal (EntryModal.addModal .entryModal EntryModal) { model | entryModal = EntryModal.prepareForAdd u model.entryModal } ! []

        ShowEditEntryModal entry ->
            showModal (EntryModal.editModal .entryModal EntryModal) { model | entryModal = EntryModal.prepareForEdit entry model.entryModal } ! []

        EntryModal msg ->
            handleEntryUpdate msg model

        CloseTopModal ->
            closeTopModal model ! []

        -- add/edit day modals:
        ShowAddDayModal ->
            showModal (DayModal.addModal .dayModal DayModal) { model | dayModal = DayModal.prepareForAdd model.dayModal } ! []

        ShowEditDayModal day ->
            showModal (DayModal.editModal .dayModal DayModal) { model | dayModal = DayModal.prepareForEdit day model.dayModal } ! []

        DayModal msg ->
            handleDayUpdate msg model

        ShowDayCompleteModal day ->
            showModal (completeDayModal day) model ! []

        DoCompleteDay day ->
            model ! [ doCompleteDay day ]

        DoneCompleteDay day ->
            closeTopModal { model | days = List.map (\d -> if d.id == day.id then day else d) model.days } ! []

        -- perform several actions eg cloing modal and logging in.
        -- done one after the other.
        All ms ->
            let
                folder msg ( model, cmds ) =
                    let
                        ( newModel, cmd ) =
                            update msg model
                    in
                        ( newModel, cmd :: cmds )

                ( newModel, cmds ) =
                    List.foldl folder ( model, [] ) ms
            in
                ( newModel, Cmd.batch cmds )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        Noop ->
            ( model, Cmd.none )



-- handle moving an entry given DND finish


moveEntryTo : ( DnDLocation, String ) -> ( DnDLocation, Dnd.Position EntryDndKey, Dnd.Position EntryDndKey ) -> Model -> ( Model, Cmd Msg )
moveEntryTo ( fromListId, id ) ( toListId, before, after ) model =
    let
        -- turn task to cmd, ignore results
        noopCmd task =
            perform (always Noop) (always Noop) task

        -- update backend knowledge of dayId in days list.
        setDayCmd dayId days =
            let
                mDay =
                    List.foldl
                        (\d m ->
                            if d.id == dayId then
                                Just d
                            else
                                m
                        )
                        Nothing
                        days
            in
                case mDay of
                    Nothing ->
                        Debug.log ("Tried to tell backend about day " ++ dayId ++ " but it doesnt exist") Cmd.none

                    Just day ->
                        noopCmd <| Days.set day

        -- remove entry from the list given.
        removeFromEntries entryIds =
            List.filter (\eid -> eid /= id) entryIds

        -- add entry to list given based on before and after; will try to use
        -- either of these (incase one is the entry being moved!) and will output
        -- a suitable command for the backend on completion.
        addToEntries entryIds =
            case ( before, after ) of
                ( Dnd.AtBeginning, _ ) ->
                    ( id :: entryIds, noopCmd <| Entries.move id Entries.AtBeginning )

                ( _, Dnd.AtEnd ) ->
                    ( entryIds ++ [ id ], noopCmd <| Entries.move id Entries.AtEnd )

                ( Dnd.AtId id1, Dnd.AtId id2 ) ->
                    addBetween id1 id2 entryIds

                badPosition ->
                    Debug.log ("impossible DND position: " ++ toString badPosition) ( entryIds, Cmd.none )

        -- given some entry IDs, add the id moved between the first and second id given.
        -- copes with either of these not actually existing in the list.
        addBetween id1 id2 entryIds =
            let
                fn =
                    \eid ( bAdded, ids, cmd ) ->
                        if bAdded then
                            ( True, eid :: ids, cmd )
                        else if eid == id1 then
                            ( True, eid :: id :: ids, noopCmd <| Entries.move id <| Entries.AtAfter eid )
                        else if eid == id2 then
                            ( True, id :: eid :: ids, noopCmd <| Entries.move id <| Entries.AtBefore eid )
                        else
                            ( False, eid :: ids, cmd )

                ( _, newEntries, cmd ) =
                    List.foldr fn ( False, [], Cmd.none ) entryIds
            in
                ( newEntries, cmd )

        -- modify entries in the day pointed at by ID given list of days using the function given,
        -- and return the modified list of days back.
        modifyEntriesInDay dayId idx modifyFn days =
            let
                modifyAtPart partIdx part =
                    if partIdx /= idx then
                        part
                    else
                        case part of
                            Days.EntriesPart es ->
                                Days.EntriesPart (modifyFn es)

                            other ->
                                Debug.log ("asked to modify entries in day but index wrong " ++ toString other) other

                doModify day =
                    { day | description = List.indexedMap modifyAtPart day.description }
            in
                List.map
                    (\day ->
                        if day.id == dayId then
                            doModify day
                        else
                            day
                    )
                    days

        -- remove or add entries from days using the modify function above, returning the new days.
        removeFromDay dayId idx days =
            modifyEntriesInDay dayId idx removeFromEntries days

        addToDay dayId idx days =
            modifyEntriesInDay dayId idx (addToEntries >> Tuple.first) days
    in
        -- perform drop action depending on where thing  was moved from and to, ie moved within
        -- entries list, or moved into a day or between days or out of a day.
        case ( fromListId, toListId ) of
            ( InEntriesList, InEntriesList ) ->
                let
                    ( newEntryIds, moveCmd ) =
                        model.entryIds |> removeFromEntries |> addToEntries
                in
                    { model | entryIds = newEntryIds } ! [ moveCmd ]

            ( InEntriesList, InDay dayId idx ) ->
                let
                    newEntryIds =
                        removeFromEntries model.entryIds

                    newDays =
                        addToDay dayId idx model.days
                in
                    { model | entryIds = newEntryIds, days = newDays } ! [ setDayCmd dayId newDays ]

            ( InDay dayId idx, InEntriesList ) ->
                let
                    newDays =
                        removeFromDay dayId idx model.days

                    ( newEntryIds, addEntryCmd ) =
                        addToEntries model.entryIds
                in
                    { model | entryIds = newEntryIds, days = newDays } ! [ addEntryCmd, setDayCmd dayId newDays ]

            ( InDay dayId1 idx1, InDay dayId2 idx2 ) ->
                let
                    newDays =
                        removeFromDay dayId1 idx1 model.days |> addToDay dayId2 idx2

                    dayCmds =
                        if dayId1 == dayId2 then
                            setDayCmd dayId1 newDays
                        else
                            Cmd.batch [ setDayCmd dayId1 newDays, setDayCmd dayId2 newDays ]
                in
                    { model | days = newDays } ! [ dayCmds ]



-- handle updates to the userModal


handleUserUpdate : UserModal.Msg -> Model -> ( Model, Cmd Msg )
handleUserUpdate msg model =
    let
        ( userModal_, act, cmd ) =
            UserModal.update msg model.userModal

        ( actedModel, aCmd ) =
            case act of
                Just (UserModal.Added user) ->
                    closeTopModal { model | users = Dict.insert user.name user model.users } ! []

                Just (UserModal.Updated user) ->
                    closeTopModal
                        { model
                            | users = Dict.insert user.name user model.users
                            , user =
                                if Maybe.map .name model.user == Just user.name then
                                    Just user
                                else
                                    model.user
                        }
                        ! []

                Just (UserModal.Removed userId) ->
                    closeTopModal { model | users = Dict.remove userId model.users } ! [ updateEverything ]

                Just (UserModal.CloseMe) ->
                    closeTopModal model ! []

                Just (UserModal.SetPasswordDone) ->
                    closeTopModal model ! []

                Nothing ->
                    model ! []
    in
        { actedModel | userModal = userModal_ } ! [ aCmd, Cmd.map UserModal cmd ]


handleEntryUpdate : EntryModal.Msg -> Model -> ( Model, Cmd Msg )
handleEntryUpdate msg model =
    let
        ( entryModal_, act, cmd ) =
            EntryModal.update msg model.entryModal

        actedModel =
            case act of
                Just (EntryModal.Added entry) ->
                    closeTopModal (addEntry entry model)

                Just (EntryModal.Updated entry) ->
                    closeTopModal (updateEntry entry model)

                Just (EntryModal.Removed entryId) ->
                    closeTopModal (removeEntry entryId model)

                Just (EntryModal.CloseMe) ->
                    closeTopModal model

                Nothing ->
                    model
    in
        { actedModel | entryModal = entryModal_ } ! [ Cmd.map EntryModal cmd ]


setEntries : List Entry -> Model -> Model
setEntries entryList model =
    let
        entryIdsInDays =
            List.foldl (\day s -> Days.descriptionPartsToEntryIds day.description |> Set.fromList |> Set.union s) Set.empty model.days

        ( entryMap, entryIds ) =
            List.foldr folder ( Dict.empty, [] ) entryList

        folder =
            \entry ( entryMap_, entryIds_ ) ->
                let
                    entryIds =
                        if Set.member entry.id entryIdsInDays then
                            entryIds_
                        else
                            entry.id :: entryIds_

                    entryMap =
                        Dict.insert entry.id entry entryMap_
                in
                    ( entryMap, entryIds )
    in
        { model | entries = entryMap, entryIds = entryIds }


removeEntry : String -> Model -> Model
removeEntry id model =
    { model | entries = Dict.remove id model.entries, entryIds = List.filter (\eid -> eid /= id) model.entryIds }


updateEntry : Entry -> Model -> Model
updateEntry entry model =
    { model | entries = Dict.insert entry.id entry model.entries }


addEntry : Entry -> Model -> Model
addEntry entry model =
    { model | entries = Dict.insert entry.id entry model.entries, entryIds = model.entryIds ++ [ entry.id ] }


handleDayUpdate : DayModal.Msg -> Model -> ( Model, Cmd Msg )
handleDayUpdate msg model =
    let
        ( dayModal_, act, cmd ) =
            DayModal.update msg model.dayModal

        actedModel =
            case act of
                Just (DayModal.Added day) ->
                    closeTopModal { model | days = model.days ++ [ day ] }

                Just (DayModal.Updated day) ->
                    closeTopModal
                        { model
                            | days =
                                List.map
                                    (\d ->
                                        if d.id == day.id then
                                            day
                                        else
                                            d
                                    )
                                    model.days
                        }

                Just (DayModal.Removed dayId) ->
                    closeTopModal { model | days = List.filter (\d -> d.id /= dayId) model.days }

                Just (DayModal.CloseMe) ->
                    closeTopModal model

                Nothing ->
                    model
    in
        { actedModel | dayModal = dayModal_ } ! [ Cmd.map DayModal cmd ]



-- logs Msg's but hides sensitive information on a case by case:


logMsg : Msg -> Msg
logMsg msg =
    let
        log =
            Debug.log "update:"

        pwLog pw =
            String.map (\_ -> '*') pw

        doLog =
            case msg of
                LoginPassword p ->
                    log (LoginPassword <| pwLog p)

                UserModal (UserModal.SetPasswordFirst p) ->
                    log (UserModal <| UserModal.SetPasswordFirst <| pwLog p)

                UserModal (UserModal.SetPasswordSecond p) ->
                    log (UserModal <| UserModal.SetPasswordSecond <| pwLog p)

                a ->
                    log a
    in
        msg



-- if the user doesn't have a password set, this shows the set password modal:


showSetPasswordIfNeeded : Model -> Model
showSetPasswordIfNeeded model =
    case model.user of
        Just u ->
            if u.hasPass == False then
                showModal
                    (UserModal.setPasswordModal .userModal UserModal)
                    { model | userModal = UserModal.prepareSetPass u model.userModal }
            else
                model

        Nothing ->
            model


showModal : (Model -> Html Msg) -> Model -> Model
showModal modalFn model =
    let
        modalShower theModel =
            case theModel of
                TheModel m ->
                    modalFn m
    in
        { model | modals = model.modals ++ [ modalShower ] }


closeTopModal : Model -> Model
closeTopModal model =
    let
        dropEnd l =
            case l of
                b :: [] ->
                    []

                a :: b ->
                    a :: dropEnd b

                [] ->
                    []
    in
        { model
            | modals = dropEnd model.modals
        }


doLogin : Model -> Cmd Msg
doLogin model =
    let
        login =
            Users.login model.loginUserName model.loginPassword
    in
        perform LoginFailed LoginSuccess login


resetLoginState : Model -> Model
resetLoginState model =
    { model
        | loginUserName = ""
        , loginPassword = ""
        , loginError = Nothing
        , loggingIn = False
    }

doCompleteDay : Day -> Cmd Msg
doCompleteDay day =
    let
        newDay =
            { day | completed = not day.completed }
    in
        perform (always Noop) (always <| DoneCompleteDay newDay) (Days.set newDay)
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

        details =
            case model.user of
                Nothing ->
                    { fullName = "Unknown User", userType = NormalUser }

                Just u ->
                    { fullName = u.fullName, userType = u.userType }

        isAdmin =
            details.userType == Admin

        isDays =
            daysToRender /= []

        isEntries =
            model.entryIds /= []

        daysToRender =
            if model.isAdminMode
                then model.days
                else List.filter (not << .completed) model.days

        -- entries tab with list of entries
        entriesTab =
            div [ class "entries-tab" ]
                [ isLoggedIn
                    ? div [ class "add-entry-area" ]
                        [ model.isAdminMode
                            ? Button.render Mdl
                                [ 0, 2 ]
                                model.mdl
                                [ Button.raised
                                , Button.colored
                                , Button.ripple
                                , Options.onClick ShowAddDayModal
                                ]
                                [ text "Add Day" ]
                        , Button.render Mdl
                            [ 0, 2 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , Button.ripple
                            , Options.onClick ShowAddEntryModal
                            ]
                            [ text "Add Entry" ]
                        ]
                , div [ class "scrollable" ]
                    [ div [ class "scrollable-inner" ]
                        [ isDays
                            ? div [ class "days" ]
                                [ h2 [ class "days-title" ] [ text "Days" ]
                                , div [ class "days-inner" ] (List.map (renderDay model) daysToRender)
                                ]
                        , div [ class "entries" ]
                            [ isDays
                                ? h2 [ class "entries-title" ] [ text "Entries" ]
                            , if not isEntries && not model.loading then
                                div [ class "no-entries" ] [ text "No entries have been added yet." ]
                              else
                                Dnd.view { enabled = isLoggedIn, listId = InEntriesList } model.entriesDnd EntriesDnd <|
                                    List.map (\eid -> ( eid, renderEntry model eid )) model.entryIds
                            ]
                        ]
                    ]
                ]

        -- admin tab with list of users
        usersTab =
            div [ class "users-tab" ]
                [ div [ class "add-user-area" ]
                    [ Button.render Mdl
                        [ 0, 3 ]
                        model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.ripple
                        , Options.onClick ShowAddUserModal
                        ]
                        [ text "Add User" ]
                    ]
                , div [ class "scrollable" ]
                    [ div [ class "scrollable-inner" ]
                        (Dict.foldr (\k v users -> renderUser model v :: users) [] model.users)
                    ]
                ]
    in
        div [ class "content" ]
            [ div
                [ class
                    ("top"
                        ++ if model.isAdminMode then
                            " is-admin-mode"
                           else
                            ""
                    )
                ]
                [ div [ class "left" ]
                    [ span [ class "logo" ]
                        [ text "TalkLicker"
                        , img [ src "static/logo.svg", class "logo-image" ] []
                        ]
                    ]
                , div [ class "right" ]
                    [ isLoggedIn
                        && isAdmin
                        && not model.isAdminMode
                        ? div [ class "admin-mode", onClick ToggleAdminMode ] [ text "Admin Mode" ]
                    , isLoggedIn
                        && isAdmin
                        && model.isAdminMode
                        ? div [ class "admin-mode enabled", onClick ToggleAdminMode ] [ text "Admin Mode" ]
                    , isLoggedIn
                        ? text details.fullName
                    , isLoggedIn
                        ? profileMenu model
                    , not isLoggedIn
                        ? Button.render Mdl
                            [ 0, 1 ]
                            model.mdl
                            [ Button.colored
                            , Button.ripple
                            , Options.onClick ShowLoginModal
                            , cs "login-button"
                            ]
                            [ text "Log In" ]
                    ]
                , model.loading
                    ? div [ class "loading-overlay" ]
                        [ Loading.indeterminate
                        ]
                ]
            , model.isAdminMode
                ? Tabs.render Mdl
                    [ 0, 10 ]
                    model.mdl
                    [ Tabs.ripple
                    , Tabs.onSelectTab
                        (\i ->
                            SelectTab
                                (if i == 0 then
                                    EntriesTab
                                 else
                                    UsersTab
                                )
                        )
                    , Tabs.activeTab
                        (if model.tab == EntriesTab then
                            0
                         else
                            1
                        )
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
                        EntriesTab ->
                            entriesTab

                        UsersTab ->
                            usersTab
                    ]
            , not model.isAdminMode
                ? div [ class "main" ] [ entriesTab ]
            , div [ class "modals" ] (List.map (\modalFunc -> modalFunc (TheModel model)) model.modals)
            , if Dnd.beingDragged model.entriesDnd then
                draggingEntry model
              else
                text ""
            ]


draggingEntry : Model -> Html Msg
draggingEntry model =
    let
        draggedId =
            Dnd.draggedId model.entriesDnd |> Maybe.withDefault ""

        entry =
            renderEntry model draggedId

        pos =
            Dnd.position model.entriesDnd

        css =
            [ ( "position", "absolute" )
            , ( "top", toString pos.y ++ "px" )
            , ( "left", toString pos.x ++ "px" )
            ]
    in
        div
            [ class "dragging-entry"
            , style css
            ]
            [ entry ]


profileMenu : Model -> Html Msg
profileMenu model =
    let
        i name =
            Icon.view name [ css "width" "40px" ]

        padding =
            css "padding-right" "24px"
    in
        Menu.render Mdl
            [ 1, 1 ]
            model.mdl
            [ Menu.ripple, Menu.bottomRight ]
            [ Menu.item
                [ Menu.onSelect ShowEditCurrentUserModal, padding ]
                [ i "person", text "Profile" ]
            , Menu.item
                [ Menu.onSelect LogOut, padding ]
                [ i "lock", text "Log out" ]
            ]


renderDay : Model -> Day -> Html Msg
renderDay model d =
    let
        tryRenderDndEntry idx entryId arr =
            case Dict.get entryId model.entries of
                Nothing ->
                    arr

                Just entry ->
                    ( entryId, renderEntry model entryId ) :: arr

        renderPart idx part =
            case part of
                Days.MarkdownPart str ->
                    markdown [ class "markdown" ] str

                Days.EntriesPart entryIds ->
                    Dnd.view { enabled = model.isAdminMode && not d.completed, listId = InDay d.id idx } model.entriesDnd EntriesDnd <|
                        List.foldr (tryRenderDndEntry idx) [] entryIds

        completeDayButton =
            Button.render Mdl
                [ 5, 5 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Options.onClick (ShowDayCompleteModal d)
                , cs "complete-day-button"
                ]
                [ text
                    (if d.completed then
                        "Uncomplete"
                    else
                        "Complete"
                    )
                ]

        dayClasses =
            "day" ++ if d.completed then " completed" else ""

    in
        div [ class dayClasses ]
            [ div [ class "title" ]
                [ if model.isAdminMode && not d.completed
                    then a [ class "text link", onClick (ShowEditDayModal d) ] [ text d.title ]
                    else div [ class "text" ] [ text d.title ]
                , if model.isAdminMode
                    then completeDayButton
                    else text ""
                ]
            , if not d.completed
                then div [ class "description" ] (List.indexedMap renderPart d.description)
                else text ""
            ]


renderEntry : Model -> String -> Html Msg
renderEntry model entryId =
    let
        entry =
            Dict.get entryId model.entries
                |> Maybe.withDefault
                    { id = ""
                    , user = "Nobody"
                    , duration = 0
                    , name = "Invalid Entry: " ++ entryId
                    , description = "The entry ID asked to be turned into an entry here does not exist."
                    , entryType = Talk
                    , created = 0
                    , modified = 0
                    }

        isMine =
            Maybe.map .name model.user == Just entry.user

        entryClass =
            case entry.entryType of
                Talk ->
                    "entry-talk"

                Project ->
                    "entry-project"

        entryIcon =
            case entry.entryType of
                Talk ->
                    Icon.i "insert_emoticon"

                Project ->
                    Icon.i "keyboard"

        entryUser =
            case Dict.get entry.user model.users of
                Nothing ->
                    "an Unknown User"

                Just u ->
                    u.fullName

        entryHours =
            toFloat entry.duration / 3600000

        markdownOpts =
            let
                d =
                    Markdown.defaultOptions
            in
                { d | sanitize = True }
    in
        div [ class ("entry " ++ entryClass) ]
            [ div [ class "title" ]
                [ entryIcon
                , if model.isAdminMode || isMine then
                    a [ class "text link", onClick (ShowEditEntryModal entry) ] [ text entry.name ]
                  else
                    div [ class "text" ] [ text entry.name ]
                ]
            , div [ class "description" ] [ markdown [ class "markdown" ] entry.description ]
            , div [ class "user" ] [ text ("By " ++ entryUser) ]
            , div [ class "duration" ] [ span [] [ text <| (toString entryHours) ++ "h" ] ]
            ]


renderUser : Model -> User -> Html Msg
renderUser model user =
    let
        isUserAdmin =
            user.userType == Admin
    in
        div
            [ class
                ("user "
                    ++ if isUserAdmin then
                        "user-admin"
                       else
                        "user-normal"
                )
            ]
            [ div [ class "title" ]
                [ Icon.i
                    (if isUserAdmin then
                        "star"
                     else
                        "person"
                    )
                , a [ class "text link", onClick (ShowEditUserModal user) ] [ text user.name ]
                ]
            , div [ class "name" ] [ text user.fullName ]
            , div [ class "type" ]
                [ text
                    (if isUserAdmin then
                        "Administrator"
                     else
                        "Normal User"
                    )
                ]
            , not user.hasPass ? div [ class "is-new" ] [ text "New" ]
            ]


loginModal : Model -> Html Msg
loginModal model =
    let
        invalid =
            model.loginUserName == ""

        loginErrorString =
            case model.loginError of
                Just LoginBadUser ->
                    "Wrong username"

                Just LoginBadPassword ->
                    "Wrong password"

                _ ->
                    "Something Untoward Transpired"

        opts =
            { title = text "Login"
            , preventClose = model.loggingIn
            , hideClose = False
            , isLoading = model.loggingIn
            , onClose = All [ CloseTopModal, ClearLoginDetails ]
            , mdl = Mdl
            , cover = []
            , content =
                div [ class "login-modal" ]
                    [ div [ class "inputs" ]
                        [ Textfield.render Mdl
                            [ 70, 0 ]
                            model.mdl
                            [ Textfield.label "Username"
                            , Textfield.floatingLabel
                            , Options.onInput LoginUserName
                            , Textfield.value model.loginUserName
                            ]
                            []
                        , Textfield.render Mdl
                            [ 70, 1 ]
                            model.mdl
                            [ Textfield.label "Password"
                            , Textfield.floatingLabel
                            , Textfield.password
                            , Options.onInput LoginPassword
                            , Textfield.value model.loginPassword
                            ]
                            []
                        ]
                    , div [ class "bottom-row" ]
                        [ Button.render Mdl
                            [ 0 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , when invalid Button.disabled
                            , Options.onClick DoLogin
                            , cs "login-button"
                            ]
                            [ text "Login" ]
                        , model.loginError
                            ?? div [ class "error" ]
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
            , onPerform = All [ CloseTopModal, DoLogout ]
            , performText = "Log out"
            , onCancel = CloseTopModal
            , cancelText = "Cancel"
            , hidePerform = False
            , hideCancel = False
            , mdl = Mdl
            }
    in
        Modals.choice opts model


completeDayModal : Day -> Model -> Html Msg
completeDayModal day model =
    let
        msg = if day.completed
            then "Are you sure you no longer want to mark this day as complete?"
            else "Are you sure you want to mark this day as complete?"
        btnMsg = if day.completed
            then "Uncomplete"
            else "Complete"
        opts =
            { title = "Complete Day"
            , icon = "done"
            , message = msg
            , onPerform = All [ CloseTopModal, DoCompleteDay day ]
            , performText = btnMsg
            , onCancel = CloseTopModal
            , cancelText = "Cancel"
            , hidePerform = False
            , hideCancel = False
            , mdl = Mdl
            }
    in
        Modals.choice opts model

markdown : List (Attribute Msg) -> String -> Html Msg
markdown attrs txt =
    let
        markdownOpts =
            let
                d =
                    Markdown.defaultOptions
            in
                { d | sanitize = True }
    in
        Markdown.toHtmlWith markdownOpts attrs txt



--
-- Main
--


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \model ->
                Sub.batch
                    [ Sub.map EntriesDnd (Dnd.sub model.entriesDnd)
                    ]
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, updateEverything )


type alias CoreDetails =
    { currentUser : Maybe User, entries : List Entry, users : Dict String User, days : List Day }


updateEverything : Cmd Msg
updateEverything =
    perform ApiError UpdateCoreDetails getEverything


getEverything : Task Api.Error CoreDetails
getEverything =
    let
        fn =
            \curr entries users days ->
                { currentUser = curr, entries = entries, users = users, days = days }
    in
        Task.map4 fn
            Users.current
            Entries.get
            Users.get
            Days.get
