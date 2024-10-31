module Ucm.WorkspaceScreen exposing (..)

import Browser
import Code.CodebaseTree as CodebaseTree
import Code.Config
import Code.ProjectName as ProjectName
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import UI.AnchoredOverlay as AnchoredOverlay
import UI.Button as Button
import UI.Icon as Icon
import Ucm.AppContext as AppContext exposing (AppContext)
import Ucm.SwitchBranch as SwitchBranch
import Ucm.Workspace.WorkspaceContext exposing (WorkspaceContext)
import Ucm.Workspace.WorkspacePane as WorkspacePane
import Window


type alias Model =
    { workspaceContext : WorkspaceContext
    , codebaseTree : CodebaseTree.Model
    , config : Code.Config.Config
    , window : Window.Model
    , leftPane : WorkspacePane.Model
    , rightPane : Maybe WorkspacePane.Model
    , switchBranch : SwitchBranch.Model
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
      , switchBranch = SwitchBranch.init
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
    | ShowChooseProject
    | SwitchBranchMsg SwitchBranch.Msg


type OutMsg
    = None
    | ShowWelcomeScreen


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

        LeftPaneMsg workspacePaneMsg ->
            let
                ( pane, paneCmd ) =
                    WorkspacePane.update
                        model.config
                        workspacePaneMsg
                        model.leftPane
            in
            ( { model | leftPane = pane }, Cmd.map LeftPaneMsg paneCmd, None )

        ShowChooseProject ->
            ( model, Cmd.none, ShowWelcomeScreen )

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


titlebarLeft : SwitchBranch.Model -> WorkspaceContext -> List (Html Msg)
titlebarLeft switchBranch workspaceContext =
    let
        projectName =
            ProjectName.toString workspaceContext.projectName
    in
    [ Button.iconThenLabel ShowChooseProject Icon.pencilRuler projectName
        |> Button.small
        |> Button.view
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
    , Button.icon NoOp Icon.windowSplit
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
        footerLeft =
            [ Button.icon NoOp Icon.leftSidebarOff
                |> Button.small
                |> Button.subdued
                |> Button.view
            ]

        footerRight =
            []
    in
    Window.window "workspace-screen"
        |> Window.withTitlebarLeft (titlebarLeft model.switchBranch model.workspaceContext)
        |> Window.withTitlebarRight titlebarRight
        |> Window.withFooterLeft footerLeft
        |> Window.withFooterRight footerRight
        |> Window.withLeftSidebar (viewLeftSidebar model.codebaseTree)
        |> Window.withContent [ Html.map LeftPaneMsg (WorkspacePane.view model.leftPane) ]
        |> Window.view WindowMsg model.window
