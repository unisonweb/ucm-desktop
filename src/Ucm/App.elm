module Ucm.App exposing (..)

import Browser
import Html
import Ucm.AppContext exposing (AppContext)
import Ucm.WelcomeScreen as WelcomeScreen exposing (OutMsg(..))
import Ucm.Workspace.WorkspaceContext exposing (WorkspaceContext)
import Ucm.WorkspaceScreen as WorkspaceScreen
import Url exposing (Url)


type Screen
    = WelcomeScreen WelcomeScreen.Model
    | WorkspaceScreen WorkspaceScreen.Model


type alias Model =
    { appContext : AppContext
    , screen : Screen
    }


init : AppContext -> Maybe WorkspaceContext -> Url -> ( Model, Cmd Msg )
init appContext workspaceContext _ =
    let
        ( screen, cmd ) =
            case workspaceContext of
                Just wsc ->
                    let
                        ( workspace, wsCmd ) =
                            WorkspaceScreen.init appContext wsc
                    in
                    ( WorkspaceScreen workspace, Cmd.map WorkspaceScreenMsg wsCmd )

                Nothing ->
                    let
                        ( welcome, welcomeCmd ) =
                            WelcomeScreen.init appContext
                    in
                    ( WelcomeScreen welcome, Cmd.map WelcomeScreenMsg welcomeCmd )
    in
    ( { appContext = appContext, screen = screen }
    , cmd
    )



-- UPDATE


type Msg
    = NoOp
    | UrlRequest Browser.UrlRequest
    | UrlChange Url
    | WelcomeScreenMsg WelcomeScreen.Msg
    | WorkspaceScreenMsg WorkspaceScreen.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WelcomeScreenMsg wMsg ->
            case model.screen of
                WelcomeScreen welcome ->
                    let
                        ( newWelcome, welcomeCmd, welcomeOut ) =
                            WelcomeScreen.update wMsg welcome

                        ( screen, newScreenCmd ) =
                            case welcomeOut of
                                WelcomeScreen.None ->
                                    ( WelcomeScreen newWelcome, Cmd.none )

                                ChangeScreenToWorkspace wsc ->
                                    let
                                        ( workspace, workspaceCmd ) =
                                            WorkspaceScreen.init model.appContext wsc
                                    in
                                    ( WorkspaceScreen workspace
                                    , Cmd.map WorkspaceScreenMsg workspaceCmd
                                    )
                    in
                    ( { model | screen = screen }
                    , Cmd.batch [ Cmd.map WelcomeScreenMsg welcomeCmd, newScreenCmd ]
                    )

                _ ->
                    ( model, Cmd.none )

        WorkspaceScreenMsg workspaceMsg ->
            case model.screen of
                WorkspaceScreen workspace ->
                    let
                        ( newWorkspace, workspaceCmd, workspaceOut ) =
                            WorkspaceScreen.update model.appContext workspaceMsg workspace

                        ( screen, screenCmd ) =
                            case workspaceOut of
                                WorkspaceScreen.None ->
                                    ( WorkspaceScreen newWorkspace, Cmd.map WorkspaceScreenMsg workspaceCmd )
                    in
                    ( { model | screen = screen }, screenCmd )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        WelcomeScreen welcome ->
            Sub.map WelcomeScreenMsg (WelcomeScreen.subscriptions welcome)

        WorkspaceScreen workspace ->
            Sub.map WorkspaceScreenMsg (WorkspaceScreen.subscriptions workspace)



-- VIEW


mapDocument : (msgA -> msgB) -> Browser.Document msgA -> Browser.Document msgB
mapDocument f document =
    { title = document.title
    , body = List.map (Html.map f) document.body
    }


view : Model -> Browser.Document Msg
view model =
    case model.screen of
        WelcomeScreen m ->
            m
                |> WelcomeScreen.view
                |> mapDocument WelcomeScreenMsg

        WorkspaceScreen m ->
            m
                |> WorkspaceScreen.view
                |> mapDocument WorkspaceScreenMsg
