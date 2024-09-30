module Ucm.WelcomeScreen exposing (..)

import Html
    exposing
        ( Html
        , div
        , h2
        , header
        , img
        , p
        , text
        )
import Html.Attributes exposing (alt, class, src)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import RemoteData exposing (WebData)
import UI.Button as Button
import UI.Card as Card
import UI.Divider as Divider
import UI.Form.TextField as TextField
import UI.Icon as Icon
import Ucm.Api as Api
import Ucm.AppContext exposing (AppContext)
import Ucm.ProjectName as ProjectName exposing (ProjectName)



-- MODEL


type alias Model =
    { searchQuery : String
    }


init : AppContext -> ( Model, Cmd Msg )
init appContext =
    ( { searchQuery = "" }
    , fetchProjects
        |> HttpApi.perform appContext.api
    )



-- UPDATE


type Msg
    = NoOp
    | UpdateSearchQuery String
    | FetchProjectsFinished (WebData (List ProjectName))


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



--EFFECTS


fetchProjects : HttpApi.ApiRequest (List ProjectName) Msg
fetchProjects =
    let
        decodeProjectList =
            Decode.list <|
                Decode.field "projectName" ProjectName.decode
    in
    Api.projects
        |> HttpApi.toRequest decodeProjectList (RemoteData.fromResult >> FetchProjectsFinished)



-- VIEW


view : Model -> List (Html Msg)
view model =
    let
        viewProjectOption p =
            div [ class "project-option" ] [ text p, Icon.view Icon.chevronRight ]
    in
    [ div [ class "welcome-column" ]
        [ header []
            [ div [ class "app-logo" ]
                [ img [ src "/src/assets/app-icon.png", alt "UCM App Icon", class "app-icon" ] []
                , div []
                    [ h2 [] [ text "Unison Codebase Manager" ]
                    , p [ class "unison-version" ] [ text "Version: release/0.5.26 (built on 2024-09-05)" ]
                    ]
                ]
            , div [ class "actions" ]
                [ Button.iconThenLabel NoOp Icon.graduationCap "Unison Docs"
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
