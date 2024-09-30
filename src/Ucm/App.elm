module Ucm.App exposing (..)

import Browser
import Html
    exposing
        ( Html
        , article
        , div
        , footer
        , header
        , main_
        )
import Html.Attributes exposing (attribute, class, classList)
import UI
import UI.Button as Button
import UI.Icon as Icon
import Ucm.AppContext exposing (AppContext)
import Ucm.WelcomeScreen as WelcomeScreen
import Url exposing (Url)


type Screen
    = WelcomeScreen WelcomeScreen.Model
    | ProjectScreen


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
    | ChangeScreen Screen
    | WelcomeScreenMsg WelcomeScreen.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeScreen s ->
            ( { model | screen = s }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewScreenContent : Screen -> Html Msg
viewScreenContent screen =
    case screen of
        WelcomeScreen m ->
            article [ class "screen-content welcome-screen" ]
                [ Html.map WelcomeScreenMsg (div [] (WelcomeScreen.view m)) ]

        ProjectScreen ->
            article [ class "screen-content project-screen" ] []


viewWindowTitlebar : Screen -> Html Msg
viewWindowTitlebar screen =
    let
        ( left, right, transparentTitlebar ) =
            case screen of
                WelcomeScreen _ ->
                    ( [], [], True )

                ProjectScreen ->
                    ( [ Button.iconThenLabelThenIcon NoOp Icon.pencilRuler "@unison/base" Icon.caretDown
                            |> Button.small
                            |> Button.view
                      , Button.iconThenLabelThenIcon NoOp Icon.branch "/main" Icon.caretDown
                            |> Button.small
                            |> Button.view
                      , Button.iconThenLabelThenIcon NoOp Icon.pencilRuler "History" Icon.caretDown
                            |> Button.small
                            |> Button.view
                      ]
                    , [ Button.icon NoOp Icon.search
                            |> Button.small
                            |> Button.subdued
                            |> Button.view
                      , Button.icon NoOp Icon.largePlus
                            |> Button.small
                            |> Button.subdued
                            |> Button.view
                      , Button.icon NoOp Icon.windowSplit
                            |> Button.small
                            |> Button.subdued
                            |> Button.view
                      , Button.iconThenLabel NoOp Icon.unisonMark "Sign-in to Unison Share"
                            |> Button.small
                            |> Button.decorativePurple
                            |> Button.view
                      ]
                    , False
                    )
    in
    header
        [ attribute "data-tauri-drag-region" "1"
        , class "window-control-bar window-titlebar"
        , classList [ ( "window-titlebar_transparent", transparentTitlebar ) ]
        ]
        [ div [ class "window-control-bar-group" ] left
        , div [ class "window-control-bar-group" ] right
        ]


viewWindowFooter : Screen -> Html Msg
viewWindowFooter screen =
    case screen of
        WelcomeScreen _ ->
            UI.nothing

        ProjectScreen ->
            footer [ class "window-control-bar window-footer" ]
                [ div [ class "window-control-bar-group" ]
                    [ Button.icon NoOp Icon.leftSidebarOff
                        |> Button.small
                        |> Button.subdued
                        |> Button.view
                    ]
                , div [ class "window-control-bar-group" ]
                    [ Button.icon NoOp Icon.cli
                        |> Button.small
                        |> Button.subdued
                        |> Button.view
                    ]
                ]


view : Model -> Browser.Document Msg
view { screen } =
    { title = "@unison/base UCM"
    , body =
        [ viewWindowTitlebar screen
        , main_ [] [ viewScreenContent screen ]
        , viewWindowFooter screen
        ]
    }
