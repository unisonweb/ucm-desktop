module Ucm.App exposing (..)

import Browser
import Html exposing (br, code, div, h2, p, text)
import Html.Attributes exposing (class)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import UI.Button as Button
import UI.Icon as Icon
import UI.StatusBanner as StatusBanner
import UI.StatusIndicator as StatusIndicator
import Ucm.Api as Api
import Ucm.AppContext exposing (AppContext)
import Ucm.UcmConnectivity exposing (UcmConnectivity(..))
import Ucm.WelcomeScreen as WelcomeScreen exposing (OutMsg(..))
import Ucm.Workspace.WorkspaceContext exposing (WorkspaceContext)
import Ucm.WorkspaceScreen as WorkspaceScreen


type Screen
    = WelcomeScreen WelcomeScreen.Model
    | WorkspaceScreen WorkspaceScreen.Model


type alias Model =
    { appContext : AppContext
    , screen : Screen
    }


init : AppContext -> Maybe WorkspaceContext -> ( Model, Cmd Msg )
init appContext workspaceContext =
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
    , Cmd.batch [ cmd, checkInitialUcmConnectivity appContext ]
    )



-- UPDATE


type Msg
    = NoOp
    | WelcomeScreenMsg WelcomeScreen.Msg
    | WorkspaceScreenMsg WorkspaceScreen.Msg
    | ReCheckUCMConnectivity
    | PerformReCheckUCMConnectivity
    | InitialUCMConnectivityCheckFinished (HttpResult ())
    | CheckUCMConnectivity
    | UCMConnectivityCheckFinished (HttpResult ())


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

        ReCheckUCMConnectivity ->
            let
                appContext =
                    model.appContext

                appContext_ =
                    { appContext | ucmConnectivity = Connecting }
            in
            -- Delay the actual check to give the user feedback when they click the button
            ( { model | appContext = appContext_ }, Util.delayMsg 750 PerformReCheckUCMConnectivity )

        PerformReCheckUCMConnectivity ->
            let
                appContext =
                    model.appContext

                appContext_ =
                    { appContext | ucmConnectivity = Connecting }
            in
            ( { model | appContext = appContext_ }, checkInitialUcmConnectivity appContext_ )

        InitialUCMConnectivityCheckFinished res ->
            let
                appContext =
                    model.appContext

                ( appContext_, screen, cmd ) =
                    case res of
                        Ok _ ->
                            let
                                ( screen_, screenCmd ) =
                                    case model.screen of
                                        WelcomeScreen _ ->
                                            let
                                                ( welcome, welcomeCmd ) =
                                                    WelcomeScreen.init appContext
                                            in
                                            ( WelcomeScreen welcome, Cmd.map WelcomeScreenMsg welcomeCmd )

                                        WorkspaceScreen wss ->
                                            let
                                                ( workspace, wsCmd ) =
                                                    WorkspaceScreen.init appContext wss.workspaceContext
                                            in
                                            ( WorkspaceScreen workspace, Cmd.map WorkspaceScreenMsg wsCmd )
                            in
                            ( { appContext | ucmConnectivity = Connected }
                            , screen_
                            , Cmd.batch [ Util.delayMsg 5000 CheckUCMConnectivity, screenCmd ]
                            )

                        Err e ->
                            ( { appContext | ucmConnectivity = NeverConnected e }, model.screen, Cmd.none )
            in
            ( { model | appContext = appContext_, screen = screen }, cmd )

        CheckUCMConnectivity ->
            ( model, checkUcmConnectivity model.appContext )

        UCMConnectivityCheckFinished res ->
            let
                appContext =
                    model.appContext

                appContext_ =
                    case res of
                        Ok _ ->
                            { appContext | ucmConnectivity = Connected }

                        Err e ->
                            { appContext | ucmConnectivity = LostConnection e }
            in
            ( { model | appContext = appContext_ }, Util.delayMsg 5000 CheckUCMConnectivity )

        _ ->
            ( model, Cmd.none )



-- EFFECTS


checkInitialUcmConnectivity : AppContext -> Cmd Msg
checkInitialUcmConnectivity appContext =
    Api.projects Nothing
        |> HttpApi.toRequestWithEmptyResponse InitialUCMConnectivityCheckFinished
        |> HttpApi.perform appContext.api


checkUcmConnectivity : AppContext -> Cmd Msg
checkUcmConnectivity appContext =
    Api.projects Nothing
        |> HttpApi.toRequestWithEmptyResponse UCMConnectivityCheckFinished
        |> HttpApi.perform appContext.api



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
    let
        view_ =
            case model.screen of
                WelcomeScreen m ->
                    m
                        |> WelcomeScreen.view model.appContext
                        |> mapDocument WelcomeScreenMsg

                WorkspaceScreen m ->
                    m
                        |> WorkspaceScreen.view model.appContext
                        |> mapDocument WorkspaceScreenMsg
    in
    case model.appContext.ucmConnectivity of
        Connecting ->
            { title = "UCM Desktop | Connecting..."
            , body = [ div [ class "connecting" ] [ StatusBanner.working "Connecting..." ] ]
            }

        NeverConnected _ ->
            let
                apiUrl =
                    HttpApi.baseApiUrl model.appContext.api
                        |> String.replace "/api/" ""
            in
            { title = "UCM Desktop | Couldn't connect to the UCM CLI"
            , body =
                [ div []
                    [ div [ class "app-message" ]
                        [ h2 [] [ StatusIndicator.view StatusIndicator.working, text "Waiting on the UCM CLI" ]
                        , p []
                            [ text "Please make sure UCM is running on the right port like so: "
                            , br [] []
                            , code [] [ text apiUrl ]
                            ]
                        , Button.iconThenLabel ReCheckUCMConnectivity Icon.plug "Connect to the UCM CLI"
                            |> Button.large
                            |> Button.view
                        ]
                    ]
                ]
            }

        Connected ->
            view_

        LostConnection _ ->
            view_
