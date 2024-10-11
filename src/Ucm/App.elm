module Ucm.App exposing (..)

import Browser
import Ucm.AppContext exposing (AppContext)
import Ucm.WelcomeScreen as WelcomeScreen exposing (OutMsg(..))
import Ucm.WorkspaceScreen as WorkspaceScreen
import Url exposing (Url)
import Window


type Screen
    = WelcomeScreen WelcomeScreen.Model
    | WorkspaceScreen WorkspaceScreen.Model


type alias Model =
    { appContext : AppContext
    , screen : Screen
    }


init : AppContext -> Url -> ( Model, Cmd Msg )
init appContext _ =
    let
        ( welcome, welcomeCmd ) =
            WelcomeScreen.init appContext
    in
    ( { appContext = appContext, screen = WelcomeScreen welcome }
    , Cmd.map WelcomeScreenMsg welcomeCmd
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
                            WorkspaceScreen.update workspaceMsg workspace

                        screen =
                            case workspaceOut of
                                WorkspaceScreen.None ->
                                    WorkspaceScreen newWorkspace
                    in
                    ( { model | screen = screen }
                    , Cmd.map WorkspaceScreenMsg workspaceCmd
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view { screen } =
    case screen of
        WelcomeScreen m ->
            WelcomeScreen.view m
                |> Window.map WelcomeScreenMsg
                |> Window.view

        WorkspaceScreen m ->
            WorkspaceScreen.view m
                |> Window.map WorkspaceScreenMsg
                |> Window.view
