module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, article, div, footer, header, main_, text)
import Html.Attributes exposing (attribute, class)
import UI.Button as Button
import UI.Icon as Icon
import Url exposing (Url)


type Screen
    = HomeScreen


type alias Model =
    { screen : Screen
    }


type alias Flags =
    ()


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { screen = HomeScreen
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


viewScreenContent : Screen -> Html Msg
viewScreenContent _ =
    article [ class "screen-content" ] []


viewWindowTitlebar : Html Msg
viewWindowTitlebar =
    header [ attribute "data-tauri-drag-region" "1", class "window-control-bar window-titlebar" ]
        [ div [ class "window-control-bar-group" ]
            [ Button.iconThenLabelThenIcon NoOp Icon.pencilRuler "@unison/base" Icon.caretDown
                |> Button.small
                |> Button.view
            , Button.iconThenLabelThenIcon NoOp Icon.branch "/main" Icon.caretDown
                |> Button.small
                |> Button.view
            , Button.iconThenLabelThenIcon NoOp Icon.pencilRuler "History" Icon.caretDown
                |> Button.small
                |> Button.view
            ]
        , div [ class "window-control-bar-group" ]
            [ Button.icon NoOp Icon.search
                |> Button.small
                |> Button.subdued
                |> Button.view
            , Button.icon NoOp Icon.branch
                |> Button.small
                |> Button.subdued
                |> Button.view
            , Button.icon NoOp Icon.pencilRuler
                |> Button.small
                |> Button.subdued
                |> Button.view
            , Button.iconThenLabel NoOp Icon.pencilRuler "Sign-in"
                |> Button.small
                |> Button.decorativePurple
                |> Button.view
            ]
        ]


viewWindowFooter : Html Msg
viewWindowFooter =
    footer [ class "window-control-bar window-footer" ]
        [ Button.icon NoOp Icon.leftSidebarOff
            |> Button.small
            |> Button.subdued
            |> Button.view
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "@unison/base UCM"
    , body =
        [ viewWindowTitlebar
        , main_ []
            [ viewScreenContent model.screen
            ]
        , viewWindowFooter
        ]
    }



-- PROGRAM


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }
