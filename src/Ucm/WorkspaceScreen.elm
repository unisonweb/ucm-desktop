module Ucm.WorkspaceScreen exposing (..)

import Browser
import Code.BranchRef as BranchRef
import Code.CodebaseTree as CodebaseTree
import Code.Config
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import RemoteData exposing (RemoteData(..))
import UI.AnchoredOverlay as AnchoredOverlay
import UI.Button as Button
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut(..))
import UI.KeyboardShortcut.Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent
import UI.Modal as Modal
import Ucm.AppContext as AppContext exposing (AppContext)
import Ucm.CommandPalette as CommandPalette
import Ucm.SwitchBranch as SwitchBranch
import Ucm.SwitchProject as SwitchProject
import Ucm.UcmConnectivity as UcmConnectivity
import Ucm.Workspace.WorkspaceContext as WorkspaceContext exposing (WorkspaceContext)
import Ucm.Workspace.WorkspacePanes as WorkspacePanes
import Window


type WorkspaceScreenModal
    = NoModal
    | CommandPaletteModal CommandPalette.Model


type alias Model =
    { workspaceContext : WorkspaceContext
    , codebaseTree : CodebaseTree.Model
    , config : Code.Config.Config
    , window : Window.Model
    , panes : WorkspacePanes.Model
    , switchProject : SwitchProject.Model
    , switchBranch : SwitchBranch.Model
    , modal : WorkspaceScreenModal
    , sidebarVisible : Bool
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : AppContext -> WorkspaceContext -> ( Model, Cmd Msg )
init appContext workspaceContext =
    let
        config =
            AppContext.toCodeConfig appContext workspaceContext

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init config

        ( panes, panesCmd ) =
            WorkspacePanes.init appContext workspaceContext
    in
    ( { workspaceContext = workspaceContext
      , codebaseTree = codebaseTree
      , config = config
      , window = Window.init
      , panes = panes
      , switchProject = SwitchProject.init
      , switchBranch = SwitchBranch.init
      , modal = NoModal
      , sidebarVisible = True
      , keyboardShortcut = KeyboardShortcut.init appContext.operatingSystem
      }
    , Cmd.batch
        [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
        , Cmd.map WorkspacePanesMsg panesCmd
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | WindowMsg Window.Msg
    | ShowCommandPalette
    | CloseModal
    | ToggleSidebar
    | ToggleRightPane
    | Keydown KeyboardEvent.KeyboardEvent
    | CodebaseTreeMsg CodebaseTree.Msg
    | WorkspacePanesMsg WorkspacePanes.Msg
    | SwitchProjectMsg SwitchProject.Msg
    | SwitchBranchMsg SwitchBranch.Msg
    | CommandPaletteMsg CommandPalette.Msg
    | KeyboardShortcutMsg KeyboardShortcut.Msg


type OutMsg
    = None


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext msg model =
    case msg of
        CodebaseTreeMsg codebaseTreeMsg ->
            let
                ( codebaseTree, codebaseTreeCmd, outMsg ) =
                    CodebaseTree.update model.config codebaseTreeMsg model.codebaseTree

                ( model_, cmd_ ) =
                    ( { model | codebaseTree = codebaseTree }
                    , Cmd.map CodebaseTreeMsg codebaseTreeCmd
                    )

                ( m, c ) =
                    case outMsg of
                        CodebaseTree.OpenDefinition ref ->
                            let
                                ( panes, panesCmd ) =
                                    WorkspacePanes.openDefinition model.config model.panes ref
                            in
                            ( { model_ | panes = panes }
                            , Cmd.batch [ cmd_, Cmd.map WorkspacePanesMsg panesCmd ]
                            )

                        _ ->
                            ( model_, cmd_ )
            in
            ( m, c, None )

        WindowMsg wMsg ->
            let
                ( window, wCmd ) =
                    Window.update wMsg model.window
            in
            ( { model | window = window }, Cmd.map WindowMsg wCmd, None )

        CommandPaletteMsg cpMsg ->
            case model.modal of
                CommandPaletteModal m ->
                    let
                        ( palette, cpCmd, out ) =
                            CommandPalette.update appContext model.config cpMsg m

                        ( panes, panesCmd, modal ) =
                            case out of
                                CommandPalette.CloseRequest ->
                                    ( model.panes, Cmd.none, NoModal )

                                CommandPalette.SelectDefinition ref ->
                                    let
                                        ( panes_, panesCmd_ ) =
                                            WorkspacePanes.openDefinition model.config model.panes ref
                                    in
                                    ( panes_, panesCmd_, NoModal )

                                _ ->
                                    ( model.panes, Cmd.none, CommandPaletteModal palette )
                    in
                    ( { model | panes = panes, modal = modal }
                    , Cmd.batch [ Cmd.map CommandPaletteMsg cpCmd, Cmd.map WorkspacePanesMsg panesCmd ]
                    , None
                    )

                _ ->
                    ( model, Cmd.none, None )

        ToggleSidebar ->
            ( { model | sidebarVisible = not model.sidebarVisible }, Cmd.none, None )

        ToggleRightPane ->
            let
                panes =
                    WorkspacePanes.toggleRightPane model.panes
            in
            ( { model | panes = panes }, Cmd.none, None )

        Keydown event ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.collect model.keyboardShortcut event.key

                shortcut =
                    KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut event

                model_ =
                    { model | keyboardShortcut = keyboardShortcut }

                showCommandPalette =
                    ( { model_ | modal = CommandPaletteModal (CommandPalette.init appContext CommandPalette.NoContext) }, Cmd.none )

                toggleSidebar =
                    ( { model_ | sidebarVisible = not model_.sidebarVisible }, Cmd.none )

                focusLeft =
                    ( { model_ | panes = WorkspacePanes.focusLeft model_.panes }, Cmd.none )

                focusRight =
                    ( { model_ | panes = WorkspacePanes.focusRight model_.panes }, Cmd.none )

                ( nextModel, cmd ) =
                    case ( model_.modal, shortcut ) of
                        ( NoModal, Chord Ctrl (K _) ) ->
                            showCommandPalette

                        ( NoModal, Chord Meta (K _) ) ->
                            showCommandPalette

                        ( NoModal, Sequence _ ForwardSlash ) ->
                            showCommandPalette

                        ( NoModal, Sequence (Just (W _)) (S _) ) ->
                            toggleSidebar

                        ( NoModal, Chord Meta (B _) ) ->
                            toggleSidebar

                        ( NoModal, Chord Ctrl (B _) ) ->
                            toggleSidebar

                        ( NoModal, Sequence (Just (W _)) ArrowLeft ) ->
                            focusLeft

                        ( NoModal, Sequence (Just (W _)) (H _) ) ->
                            focusLeft

                        ( NoModal, Sequence (Just (W _)) ArrowRight ) ->
                            focusRight

                        ( NoModal, Sequence (Just (W _)) (L _) ) ->
                            focusRight

                        ( NoModal, Sequence (Just (W _)) (P _) ) ->
                            let
                                ( switchProject, switchProjectCmd ) =
                                    SwitchProject.toggleSheet appContext model.switchProject
                            in
                            ( { model_ | switchProject = switchProject }, Cmd.map SwitchProjectMsg switchProjectCmd )

                        ( NoModal, Sequence (Just (W _)) (B _) ) ->
                            let
                                ( switchBranch, switchBranchCmd ) =
                                    SwitchBranch.toggleSheet appContext model.workspaceContext.projectName model.switchBranch
                            in
                            ( { model_ | switchBranch = switchBranch }, Cmd.map SwitchBranchMsg switchBranchCmd )

                        _ ->
                            ( model_, Cmd.none )
            in
            ( nextModel, Cmd.batch [ Cmd.map KeyboardShortcutMsg kCmd, cmd ], None )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd, None )

        ShowCommandPalette ->
            ( { model | modal = CommandPaletteModal (CommandPalette.init appContext CommandPalette.NoContext) }
            , Cmd.none
            , None
            )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none, None )

        SwitchProjectMsg switchProjectMsg ->
            let
                ( switchProject, switchProjectCmd, out ) =
                    SwitchProject.update
                        appContext
                        switchProjectMsg
                        model.switchProject

                ( model_, cmd_ ) =
                    case out of
                        SwitchProject.None ->
                            ( model, Cmd.none )

                        SwitchProject.SwitchProjectRequest pn ->
                            let
                                workspaceContext =
                                    { projectName = pn, branchRef = BranchRef.main_ }

                                config =
                                    AppContext.toCodeConfig appContext workspaceContext

                                ( codebaseTree, codebaseTreeCmd ) =
                                    CodebaseTree.init config

                                ( panes, panesCmd ) =
                                    WorkspacePanes.init appContext workspaceContext
                            in
                            ( { model
                                | workspaceContext = workspaceContext
                                , codebaseTree = codebaseTree
                                , config = config
                                , panes = panes
                              }
                            , Cmd.batch
                                [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
                                , Cmd.map WorkspacePanesMsg panesCmd
                                , WorkspaceContext.save workspaceContext
                                ]
                            )
            in
            ( { model_ | switchProject = switchProject }
            , Cmd.batch
                [ Cmd.map SwitchProjectMsg switchProjectCmd
                , cmd_
                ]
            , None
            )

        SwitchBranchMsg switchBranchMsg ->
            let
                ( switchBranch, switchBranchCmd, out ) =
                    SwitchBranch.update
                        appContext
                        model.workspaceContext.projectName
                        switchBranchMsg
                        model.switchBranch

                ( model_, cmd_ ) =
                    case out of
                        SwitchBranch.None ->
                            ( model, Cmd.none )

                        SwitchBranch.SwitchToBranchRequest br ->
                            let
                                workspaceContext =
                                    { projectName = model.workspaceContext.projectName, branchRef = br }

                                config =
                                    AppContext.toCodeConfig appContext workspaceContext

                                ( codebaseTree, codebaseTreeCmd ) =
                                    CodebaseTree.init config

                                ( panes, panesCmd ) =
                                    WorkspacePanes.init appContext workspaceContext
                            in
                            ( { model
                                | workspaceContext = workspaceContext
                                , codebaseTree = codebaseTree
                                , config = config
                                , panes = panes
                              }
                            , Cmd.batch
                                [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
                                , Cmd.map WorkspacePanesMsg panesCmd
                                , WorkspaceContext.save workspaceContext
                                ]
                            )
            in
            ( { model_ | switchBranch = switchBranch }, Cmd.batch [ Cmd.map SwitchBranchMsg switchBranchCmd, cmd_ ], None )

        WorkspacePanesMsg wspMsg ->
            let
                ( panes, panesCmd ) =
                    WorkspacePanes.update model.config wspMsg model.panes
            in
            ( { model | panes = panes }, Cmd.map WorkspacePanesMsg panesCmd, None )

        NoOp ->
            ( model, Cmd.none, None )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        activeSub =
            case model.modal of
                NoModal ->
                    Sub.map WorkspacePanesMsg (WorkspacePanes.subscriptions model.panes)

                CommandPaletteModal m ->
                    Sub.map CommandPaletteMsg (CommandPalette.subscriptions m)
    in
    Sub.batch
        [ Sub.map WindowMsg (Window.subscriptions model.window)
        , KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown
        , activeSub
        ]



-- VIEW


titlebarLeft : Model -> List (Html Msg)
titlebarLeft { switchProject, switchBranch, workspaceContext } =
    [ SwitchProject.toAnchoredOverlay workspaceContext.projectName switchProject
        |> AnchoredOverlay.map SwitchProjectMsg
        |> AnchoredOverlay.view
    , SwitchBranch.toAnchoredOverlay workspaceContext.branchRef switchBranch
        |> AnchoredOverlay.map SwitchBranchMsg
        |> AnchoredOverlay.view
    ]


titlebarRight : List (Html Msg)
titlebarRight =
    [ Button.icon ShowCommandPalette Icon.search
        |> Button.small
        |> Button.subdued
        |> Button.view
    , Button.icon ToggleRightPane Icon.windowSplit
        |> Button.small
        |> Button.subdued
        |> Button.view
    ]


viewLeftSidebar : CodebaseTree.Model -> List (Html Msg)
viewLeftSidebar codebaseTree =
    -- TODO: this class should be controlled by Window
    [ div [ class "window-sidebar_inner-sidebar" ]
        [ Html.map CodebaseTreeMsg
            (CodebaseTree.view { withPerspective = False } codebaseTree)
        ]
    ]


view : AppContext -> Model -> Browser.Document Msg
view appContext model =
    let
        window =
            Window.window "workspace-screen"

        ( sidebarIcon, window_ ) =
            if model.sidebarVisible then
                ( Icon.leftSidebarOff
                , Window.withLeftSidebar (viewLeftSidebar model.codebaseTree) window
                )

            else
                ( Icon.leftSidebarOn, window )

        footerLeft =
            [ Button.icon ToggleSidebar sidebarIcon
                |> Button.small
                |> Button.subdued
                |> Button.view
            ]

        footerRight =
            [ UcmConnectivity.view appContext.ucmConnectivity ]

        window__ =
            case model.modal of
                NoModal ->
                    window_

                CommandPaletteModal m ->
                    Window.withModal
                        (m |> CommandPalette.view |> Modal.map CommandPaletteMsg)
                        window_

        content =
            [ Html.map WorkspacePanesMsg (WorkspacePanes.view model.panes) ]
    in
    window__
        |> Window.withTitlebarLeft (titlebarLeft model)
        |> Window.withTitlebarRight titlebarRight
        |> Window.withFooterLeft footerLeft
        |> Window.withFooterRight footerRight
        |> Window.withContent content
        |> Window.view WindowMsg model.window
