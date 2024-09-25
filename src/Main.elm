module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, article, h1, main_, text)
import Html.Attributes exposing (class)
import UI.Button as Button
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
viewScreenContent screen =
    let
        title t =
            h1 [] [ text t ]

        ( title_, content ) =
            case screen of
                HomeScreen ->
                    ( title "UCM", text "Hello World" )
    in
    article [ class "screen-content" ] [ title_, content ]


view : Model -> Browser.Document Msg
view model =
    { title = "UCM"
    , body =
        [ main_ []
            [ viewScreenContent model.screen
            , Button.view (Button.button NoOp "Click me")
            ]
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
