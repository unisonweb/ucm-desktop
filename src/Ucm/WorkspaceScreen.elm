module Ucm.WorkspaceScreen exposing (..)

import Browser
import Code.BranchRef as BranchRef
import Code.CodebaseTree as CodebaseTree
import Code.Config
import Code.ProjectName exposing (ProjectName)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import RemoteData exposing (RemoteData(..), WebData)
import UI.AnchoredOverlay as AnchoredOverlay
import UI.Button as Button
import UI.Icon as Icon
import Ucm.AppContext as AppContext exposing (AppContext)
import Ucm.SwitchBranch as SwitchBranch
import Ucm.SwitchProject as SwitchProject
import Ucm.Workspace.WorkspaceContext as WorkspaceContext exposing (WorkspaceContext)
import Ucm.Workspace.WorkspacePane as WorkspacePane
import Window


type WorkspaceScreenModal
    = NoModal
    | SwitchProjectModal (WebData { query : String, projects : List ProjectName })


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
    | CodebaseTreeMsg CodebaseTree.Msg
    | LeftPaneMsg WorkspacePane.Msg
    | SwitchProjectMsg SwitchProject.Msg
    | SwitchBranchMsg SwitchBranch.Msg
    | FetchProjectsFinished (WebData (List ProjectName))
    | CloseModal
    | ToggleSidebar


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
                                ( leftPane, paneMsg ) =
                                    WorkspacePane.openDefinition
                                        model.config
                                        model_.leftPane
                                        ref
                            in
                            ( { model_ | leftPane = leftPane }
                            , Cmd.batch [ cmd_, Cmd.map LeftPaneMsg paneMsg ]
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

        FetchProjectsFinished projects ->
            case model.modal of
                SwitchProjectModal _ ->
                    let
                        modalData =
                            projects
                                |> RemoteData.map (\ps -> { projects = ps, query = "" })
                    in
                    ( { model | modal = SwitchProjectModal modalData }, Cmd.none, None )

                _ ->
                    ( model, Cmd.none, None )

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
    [ Button.icon NoOp Icon.search
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
    in
    window_
        |> Window.withFooterLeft footerLeft
        |> Window.withFooterRight footerRight
        |> Window.withTitlebarLeft (titlebarLeft model)
        |> Window.withTitlebarRight titlebarRight
        |> Window.withContent [ Html.map LeftPaneMsg (WorkspacePane.view model.leftPane) ]
        |> Window.view WindowMsg model.window
