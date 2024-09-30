module Ucm.App exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, article, div, footer, h1, h2, header, main_, p, text)
import Html.Attributes exposing (attribute, class)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Divider as Divider
import UI.Form.TextField as TextField
import UI.Icon as Icon
import Url exposing (Url)


type alias WelcomeScreenModel =
    { searchQuery : String
    }


type Screen
    = WelcomeScreen WelcomeScreenModel
    | ProjectScreen


type alias Model =
    { screen : Screen
    }


type alias Flags =
    ()


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { screen = WelcomeScreen { searchQuery = "" }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | UrlRequest Browser.UrlRequest
    | UrlChange Url
    | ChangeScreen Screen


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


viewWelcomeScreen : WelcomeScreenModel -> List (Html Msg)
viewWelcomeScreen model =
    let
        viewProjectOption p =
            div [ class "project-option" ] [ text p, Icon.view Icon.chevronRight ]
    in
    [ div [ class "welcome-column" ]
        [ header []
            [ Icon.view Icon.unisonMark
            , h1 [] [ text "Unison" ]
            , p [ class "subdued" ] [ text "Version: release/0.5.26 (built on 2024-09-05)" ]
            , div [ class "actions" ]
                [ Button.iconThenLabel NoOp Icon.docs "Unison Docs"
                    |> Button.small
                    |> Button.view
                , Button.iconThenLabel NoOp Icon.largePlus "New project"
                    |> Button.small
                    |> Button.view
                , Button.iconThenLabel NoOp Icon.download "Clone project"
                    |> Button.small
                    |> Button.view
                ]
            ]
        , Divider.divider |> Divider.small |> Divider.view
        , h2 [] [ Icon.view Icon.pencilRuler, text "Select a project to get started" ]
        , TextField.fieldWithoutLabel (always NoOp) "Search projects" model.searchQuery
            |> TextField.view
        , Card.card
            [ viewProjectOption "@unison/base"
            , viewProjectOption "@unison/cloud"
            , viewProjectOption "@hojberg/html"
            , viewProjectOption "@hojberg/svg"
            ]
            |> Card.asContained
            |> Card.withClassName "select-project"
            |> Card.view
        ]
    ]


viewScreenContent : Screen -> Html Msg
viewScreenContent screen =
    case screen of
        WelcomeScreen m ->
            article [ class "screen-content welcome-screen" ] (viewWelcomeScreen m)

        ProjectScreen ->
            article [ class "screen-content project-screen" ] []


viewWindowTitlebar : Screen -> Html Msg
viewWindowTitlebar screen =
    let
        ( left, right ) =
            case screen of
                WelcomeScreen _ ->
                    ( [], [] )

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
                    )
    in
    header [ attribute "data-tauri-drag-region" "1", class "window-control-bar window-titlebar" ]
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
