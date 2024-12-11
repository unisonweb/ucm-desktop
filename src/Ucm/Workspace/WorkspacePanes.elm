module Ucm.Workspace.WorkspacePanes exposing (..)

import Code.Config exposing (Config)
import Code.Definition.Reference exposing (Reference)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import SplitPane.SplitPane as SplitPane
import Ucm.AppContext exposing (AppContext)
import Ucm.Workspace.WorkspaceContext exposing (WorkspaceContext)
import Ucm.Workspace.WorkspacePane as WorkspacePane


type FocusedPane
    = LeftPaneFocus { rightPaneVisible : Bool }
    | RightPaneFocus


type alias Model =
    { left : WorkspacePane.Model
    , right : WorkspacePane.Model
    , focusedPane : FocusedPane
    , splitPane : SplitPane.State
    }


init : AppContext -> WorkspaceContext -> ( Model, Cmd Msg )
init appContext workspaceContext =
    let
        ( leftPane, leftPaneCmd ) =
            WorkspacePane.init appContext workspaceContext

        ( rightPane, rightPaneCmd ) =
            WorkspacePane.init appContext workspaceContext

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


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        LeftPaneMsg workspacePaneMsg ->
            let
                ( leftPane, leftPaneCmd ) =
                    WorkspacePane.update config workspacePaneMsg model.left
            in
            ( { model | left = leftPane }, Cmd.map LeftPaneMsg leftPaneCmd )

        RightPaneMsg workspacePaneMsg ->
            let
                ( rightPane, rightPaneCmd ) =
                    WorkspacePane.update config workspacePaneMsg model.right
            in
            ( { model | right = rightPane }, Cmd.map RightPaneMsg rightPaneCmd )

        SplitPaneMsg paneMsg ->
            ( { model
                | splitPane =
                    SplitPane.update
                        paneMsg
                        model.splitPane
              }
            , Cmd.none
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
                ( leftPane, leftPaneCmd ) =
                    WorkspacePane.openDefinition config model.left ref
            in
            ( { model | left = leftPane }, Cmd.map LeftPaneMsg leftPaneCmd )

        RightPaneFocus ->
            let
                ( rightPane, rightPaneCmd ) =
                    WorkspacePane.openDefinition config model.right ref
            in
            ( { model | right = rightPane }, Cmd.map RightPaneMsg rightPaneCmd )



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


view : Model -> Html Msg
view model =
    let
        left isFocused =
            Html.map LeftPaneMsg (WorkspacePane.view isFocused model.left)

        right isFocused =
            Html.map RightPaneMsg (WorkspacePane.view isFocused model.right)

        paneConfig =
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
                div [ class "workspace-panes" ] [ SplitPane.view paneConfig (left True) (right False) model.splitPane ]

            else
                div [ class "workspace-panes_single-pane" ] [ left True ]

        RightPaneFocus ->
            div [ class "workspace-panes" ] [ SplitPane.view paneConfig (left False) (right True) model.splitPane ]
