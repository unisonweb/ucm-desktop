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
import UI.Modal as Modal
import Ucm.AppContext as AppContext exposing (AppContext)
import Ucm.CommandPalette as CommandPalette
import Ucm.SwitchBranch as SwitchBranch
import Ucm.SwitchProject as SwitchProject
import Ucm.Workspace.WorkspaceContext as WorkspaceContext exposing (WorkspaceContext)
import Ucm.Workspace.WorkspacePane as WorkspacePane
import Window


type WorkspaceScreenModal
    = NoModal
    | CommandPaletteModal CommandPalette.Model


type alias Model =
    { workspaceContext : WorkspaceContext
    , codebaseTree : CodebaseTree.Model
    , config : Code.Config.Config
    , window : Window.Model
    , leftPane : WorkspacePane.Model
    , rightPane : Maybe WorkspacePane.Model
    , switchProject : SwitchProject.Model
    , switchBranch : SwitchBranch.Model
    , modal : WorkspaceScreenModal
    , sidebarVisible : Bool
    }


init : AppContext -> WorkspaceContext -> ( Model, Cmd Msg )
init appContext workspaceContext =
    let
        config =
            AppContext.toCodeConfig appContext workspaceContext

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init config

        ( leftPane, leftPaneCmd ) =
            WorkspacePane.init appContext workspaceContext
    in
    ( { workspaceContext = workspaceContext
      , codebaseTree = codebaseTree
      , config = config
      , window = Window.init
      , leftPane = leftPane
      , rightPane = Nothing
      , switchProject = SwitchProject.init
      , switchBranch = SwitchBranch.init
      , modal = NoModal
      , sidebarVisible = True
      }
    , Cmd.batch
        [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
        , Cmd.map LeftPaneMsg leftPaneCmd
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | WindowMsg Window.Msg
    | ShowCommandPalette
    | CloseModal
    | ToggleSidebar
    | CodebaseTreeMsg CodebaseTree.Msg
    | LeftPaneMsg WorkspacePane.Msg
    | SwitchProjectMsg SwitchProject.Msg
    | SwitchBranchMsg SwitchBranch.Msg
    | CommandPaletteMsg CommandPalette.Msg


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
                                ( leftPane, paneCmd ) =
                                    WorkspacePane.openDefinition
                                        model.config
                                        model_.leftPane
                                        ref
                            in
                            ( { model_ | leftPane = leftPane }
                            , Cmd.batch [ cmd_, Cmd.map LeftPaneMsg paneCmd ]
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

                        ( leftPane, paneCmd, modal ) =
                            case out of
                                CommandPalette.CloseRequest ->
                                    ( model.leftPane, Cmd.none, NoModal )

                                CommandPalette.SelectDefinition ref ->
                                    let
                                        ( leftPane_, paneCmd_ ) =
                                            WorkspacePane.openDefinition
                                                model.config
                                                model.leftPane
                                                ref
                                    in
                                    ( leftPane_, Cmd.map LeftPaneMsg paneCmd_, NoModal )

                                _ ->
                                    ( model.leftPane, Cmd.none, CommandPaletteModal palette )
                    in
                    ( { model | leftPane = leftPane, modal = modal }
                    , Cmd.batch [ Cmd.map CommandPaletteMsg cpCmd, paneCmd ]
                    , None
                    )

                _ ->
                    ( model, Cmd.none, None )

        ToggleSidebar ->
            ( { model | sidebarVisible = not model.sidebarVisible }, Cmd.none, None )

        LeftPaneMsg workspacePaneMsg ->
            let
                ( pane, paneCmd ) =
                    WorkspacePane.update
                        model.config
                        workspacePaneMsg
                        model.leftPane
            in
            ( { model | leftPane = pane }, Cmd.map LeftPaneMsg paneCmd, None )

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

                                ( leftPane, leftPaneCmd ) =
                                    WorkspacePane.init appContext workspaceContext
                            in
                            ( { model
                                | workspaceContext = workspaceContext
                                , codebaseTree = codebaseTree
                                , config = config
                                , leftPane = leftPane
                              }
                            , Cmd.batch
                                [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
                                , Cmd.map LeftPaneMsg leftPaneCmd
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

                                ( leftPane, leftPaneCmd ) =
                                    WorkspacePane.init appContext workspaceContext
                            in
                            ( { model
                                | workspaceContext = workspaceContext
                                , codebaseTree = codebaseTree
                                , config = config
                                , leftPane = leftPane
                              }
                            , Cmd.batch
                                [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
                                , Cmd.map LeftPaneMsg leftPaneCmd
                                , WorkspaceContext.save workspaceContext
                                ]
                            )
            in
            ( { model_ | switchBranch = switchBranch }, Cmd.batch [ Cmd.map SwitchBranchMsg switchBranchCmd, cmd_ ], None )

        NoOp ->
            ( model, Cmd.none, None )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map WindowMsg (Window.subscriptions model.window)



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
    ]


viewLeftSidebar : CodebaseTree.Model -> List (Html Msg)
viewLeftSidebar codebaseTree =
    -- TODO: this class should be controlled by Window
    [ div [ class "window-sidebar_inner-sidebar" ]
        [ Html.map CodebaseTreeMsg
            (CodebaseTree.view { withPerspective = False } codebaseTree)
        ]
    ]


view : Model -> Browser.Document Msg
view model =
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
            []

        window__ =
            case model.modal of
                NoModal ->
                    window_

                CommandPaletteModal m ->
                    Window.withModal
                        (m |> CommandPalette.view |> Modal.map CommandPaletteMsg)
                        window_
    in
    window__
        |> Window.withFooterLeft footerLeft
        |> Window.withFooterRight footerRight
        |> Window.withTitlebarLeft (titlebarLeft model)
        |> Window.withTitlebarRight titlebarRight
        |> Window.withContent [ Html.map LeftPaneMsg (WorkspacePane.view model.leftPane) ]
        |> Window.view WindowMsg model.window
