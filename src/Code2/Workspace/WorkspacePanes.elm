module Code2.Workspace.WorkspacePanes exposing (..)

import Code.Config exposing (Config)
import Code.Definition.Reference exposing (Reference)
import Code.FullyQualifiedName exposing (FQN)
import Code2.Workspace.WorkspaceItemRef exposing (WorkspaceItemRef)
import Code2.Workspace.WorkspacePane as WorkspacePane
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Lib.OperatingSystem exposing (OperatingSystem)
import SplitPane.SplitPane as SplitPane


type FocusedPane
    = LeftPaneFocus { rightPaneVisible : Bool }
    | RightPaneFocus


type alias Model =
    { left : WorkspacePane.Model
    , right : WorkspacePane.Model
    , focusedPane : FocusedPane
    , splitPane : SplitPane.State
    }


init : OperatingSystem -> ( Model, Cmd Msg )
init os =
    let
        ( leftPane, leftPaneCmd ) =
            WorkspacePane.init os

        ( rightPane, rightPaneCmd ) =
            WorkspacePane.init os

        splitPane =
            SplitPane.init SplitPane.Horizontal
                |> SplitPane.configureSplitter (SplitPane.percentage 0.5 Nothing)

        panes =
            { left = leftPane
            , right = rightPane
            , focusedPane = LeftPaneFocus { rightPaneVisible = False }
            , splitPane = splitPane
            }
    in
    ( panes
    , Cmd.batch
        [ Cmd.map LeftPaneMsg leftPaneCmd
        , Cmd.map RightPaneMsg
            rightPaneCmd
        ]
    )



-- UPDATE


type Msg
    = LeftPaneMsg WorkspacePane.Msg
    | RightPaneMsg WorkspacePane.Msg
    | SplitPaneMsg SplitPane.Msg


type OutMsg
    = NoOut
    | Focused WorkspaceItemRef
    | Emptied
    | ChangePerspectiveToSubNamespace Reference FQN
    | ShowFinderRequest FQN


update : Config -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update config msg model =
    case msg of
        LeftPaneMsg workspacePaneMsg ->
            let
                ( leftPane, leftPaneCmd, leftPaneOut ) =
                    WorkspacePane.update config "workspace-pane_left" workspacePaneMsg model.left

                ( model_, out ) =
                    case leftPaneOut of
                        WorkspacePane.RequestPaneFocus ->
                            case model.focusedPane of
                                RightPaneFocus ->
                                    ( { model | focusedPane = LeftPaneFocus { rightPaneVisible = True } }, NoOut )

                                _ ->
                                    ( model, NoOut )

                        WorkspacePane.FocusOn wsRef ->
                            ( model, Focused wsRef )

                        WorkspacePane.RequestFindInNamespace fqn ->
                            ( model, ShowFinderRequest fqn )

                        WorkspacePane.Emptied ->
                            ( model, Emptied )

                        WorkspacePane.RequestChangePerspective ref fqn ->
                            ( model, ChangePerspectiveToSubNamespace ref fqn )

                        _ ->
                            ( model, NoOut )
            in
            ( { model_ | left = leftPane }
            , Cmd.map LeftPaneMsg leftPaneCmd
            , out
            )

        RightPaneMsg workspacePaneMsg ->
            let
                ( rightPane, rightPaneCmd, rightPaneOut ) =
                    WorkspacePane.update config "workspace-pane_right" workspacePaneMsg model.left

                ( model_, out ) =
                    case rightPaneOut of
                        WorkspacePane.RequestPaneFocus ->
                            case model.focusedPane of
                                RightPaneFocus ->
                                    ( { model | focusedPane = RightPaneFocus }, NoOut )

                                _ ->
                                    ( model, NoOut )

                        WorkspacePane.FocusOn wsRef ->
                            ( model, Focused wsRef )

                        WorkspacePane.RequestFindInNamespace fqn ->
                            ( model, ShowFinderRequest fqn )

                        WorkspacePane.Emptied ->
                            ( model, Emptied )

                        _ ->
                            ( model, NoOut )
            in
            ( { model_ | right = rightPane }
            , Cmd.map RightPaneMsg rightPaneCmd
            , out
            )

        SplitPaneMsg paneMsg ->
            ( { model
                | splitPane =
                    SplitPane.update
                        paneMsg
                        model.splitPane
              }
            , Cmd.none
            , NoOut
            )


toggleRightPane : Model -> Model
toggleRightPane model =
    let
        focus =
            case model.focusedPane of
                LeftPaneFocus _ ->
                    RightPaneFocus

                RightPaneFocus ->
                    LeftPaneFocus { rightPaneVisible = False }
    in
    { model | focusedPane = focus }


focusRight : Model -> Model
focusRight model =
    let
        focus =
            case model.focusedPane of
                LeftPaneFocus { rightPaneVisible } ->
                    if rightPaneVisible then
                        RightPaneFocus

                    else
                        model.focusedPane

                _ ->
                    model.focusedPane
    in
    { model | focusedPane = focus }


focusLeft : Model -> Model
focusLeft model =
    let
        focus =
            case model.focusedPane of
                LeftPaneFocus _ ->
                    model.focusedPane

                RightPaneFocus ->
                    LeftPaneFocus { rightPaneVisible = True }
    in
    { model | focusedPane = focus }


openDefinition : Config -> Model -> Reference -> ( Model, Cmd Msg )
openDefinition config model ref =
    case model.focusedPane of
        LeftPaneFocus _ ->
            let
                -- TODO: deal with the out msg and routes
                ( leftPane, leftPaneCmd, _ ) =
                    WorkspacePane.openDefinition config "workspace-pane_left" model.left ref
            in
            ( { model | left = leftPane }, Cmd.map LeftPaneMsg leftPaneCmd )

        RightPaneFocus ->
            let
                ( rightPane, rightPaneCmd, _ ) =
                    WorkspacePane.openDefinition config "workspace-pane_right" model.right ref
            in
            ( { model | right = rightPane }, Cmd.map RightPaneMsg rightPaneCmd )


openDependentsOf : Config -> Model -> Reference -> ( Model, Cmd Msg )
openDependentsOf config model ref =
    case model.focusedPane of
        LeftPaneFocus _ ->
            let
                -- TODO: deal with the out msg and routes
                ( leftPane, leftPaneCmd, _ ) =
                    WorkspacePane.openDependents config "workspace-pane_left" model.left ref
            in
            ( { model | left = leftPane }, Cmd.map LeftPaneMsg leftPaneCmd )

        RightPaneFocus ->
            let
                ( rightPane, rightPaneCmd, _ ) =
                    WorkspacePane.openDependents config "workspace-pane_right" model.right ref
            in
            ( { model | right = rightPane }, Cmd.map RightPaneMsg rightPaneCmd )


openDependenciesOf : Config -> Model -> Reference -> ( Model, Cmd Msg )
openDependenciesOf config model ref =
    case model.focusedPane of
        LeftPaneFocus _ ->
            let
                -- TODO: deal with the out msg and routes
                ( leftPane, leftPaneCmd, _ ) =
                    WorkspacePane.openDependencies config "workspace-pane_left" model.left ref
            in
            ( { model | left = leftPane }, Cmd.map LeftPaneMsg leftPaneCmd )

        RightPaneFocus ->
            let
                ( rightPane, rightPaneCmd, _ ) =
                    WorkspacePane.openDependencies config "workspace-pane_right" model.right ref
            in
            ( { model | right = rightPane }, Cmd.map RightPaneMsg rightPaneCmd )


currentlyOpenReferences : Model -> List Reference
currentlyOpenReferences model =
    WorkspacePane.currentlyOpenReferences model.left
        ++ WorkspacePane.currentlyOpenReferences model.left


currentlyOpenFqns : Model -> List FQN
currentlyOpenFqns model =
    WorkspacePane.currentlyOpenFqns model.left
        ++ WorkspacePane.currentlyOpenFqns model.right



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        focusSub =
            case model.focusedPane of
                LeftPaneFocus _ ->
                    Sub.map LeftPaneMsg (WorkspacePane.subscriptions model.left)

                RightPaneFocus ->
                    Sub.map RightPaneMsg (WorkspacePane.subscriptions model.right)
    in
    Sub.batch
        [ focusSub
        , Sub.map SplitPaneMsg (SplitPane.subscriptions model.splitPane)
        ]



-- VIEW


type alias PanesConfig =
    { operatingSystem : OperatingSystem
    , withFocusedPaneIndicator : Bool
    , withNamespaceDropdown : Bool
    , withMinimap : Bool
    }


view : PanesConfig -> Model -> Html Msg
view cfg model =
    let
        paneConfig paneId isFocused =
            { operatingSystem = cfg.operatingSystem
            , paneId = paneId
            , isFocused = isFocused
            , withFocusedPaneIndicator = cfg.withFocusedPaneIndicator
            , withNamespaceDropdown = cfg.withNamespaceDropdown
            , withMinimap = cfg.withMinimap
            }

        left isFocused =
            Html.map LeftPaneMsg
                (WorkspacePane.view
                    (paneConfig "workspace-pane_left" isFocused)
                    model.left
                )

        right isFocused =
            Html.map RightPaneMsg
                (WorkspacePane.view
                    (paneConfig "workspace-pane_right" isFocused)
                    model.right
                )

        splitConfig =
            SplitPane.createViewConfig
                { toMsg = SplitPaneMsg
                , customSplitter =
                    Just (SplitPane.createCustomSplitter SplitPaneMsg splitter)
                }

        splitter =
            { attributes = [ class "workspace-panes_resize-handle" ]
            , children = []
            }
    in
    case model.focusedPane of
        LeftPaneFocus { rightPaneVisible } ->
            if rightPaneVisible then
                div [ class "workspace-panes" ] [ SplitPane.view splitConfig (left True) (right False) model.splitPane ]

            else
                div [ class "workspace-panes_single-pane" ] [ left True ]

        RightPaneFocus ->
            div [ class "workspace-panes" ] [ SplitPane.view splitConfig (left False) (right True) model.splitPane ]
