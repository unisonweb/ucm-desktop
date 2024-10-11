module Ucm.WelcomeScreen exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Html
    exposing
        ( div
        , h2
        , header
        , img
        , p
        , section
        , text
        )
import Html.Attributes exposing (alt, class, src)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.Form.TextField as TextField
import UI.Icon as Icon
import Ucm.Api as Api
import Ucm.AppContext exposing (AppContext)
import Ucm.Link as Link
import Ucm.ProjectName as ProjectName exposing (ProjectName)
import Ucm.WorkspaceContext exposing (WorkspaceContext)
import Window exposing (Window)



-- MODEL


type alias Model =
    { searchQuery : String
    , projects : WebData (List ProjectName)
    }


init : AppContext -> ( Model, Cmd Msg )
init appContext =
    ( { searchQuery = "", projects = Loading }
    , fetchProjects appContext
    )



-- UPDATE


type Msg
    = FetchProjectsFinished (WebData (List ProjectName))
    | UpdateSearchQuery String
    | SelectProject ProjectName BranchRef


type OutMsg
    = None
    | ChangeScreenToWorkspace WorkspaceContext


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        FetchProjectsFinished projects ->
            ( { model | projects = projects }, Cmd.none, None )

        UpdateSearchQuery q ->
            ( { model | searchQuery = q }, Cmd.none, None )

        SelectProject projectName branchRef ->
            let
                workspaceContext =
                    { projectName = projectName, branchRef = branchRef }
            in
            ( model, Cmd.none, ChangeScreenToWorkspace workspaceContext )



--EFFECTS


fetchProjects : AppContext -> Cmd Msg
fetchProjects appContext =
    let
        decodeProjectList : Decode.Decoder (List ProjectName)
        decodeProjectList =
            Decode.list <|
                Decode.field "projectName" ProjectName.decode
    in
    Api.projects
        |> HttpApi.toRequest decodeProjectList (RemoteData.fromResult >> FetchProjectsFinished)
        |> HttpApi.perform appContext.api



-- VIEW


view : Model -> Window Msg
view model =
    let
        viewProjectOption p =
            Click.onClick (SelectProject p BranchRef.main_)
                |> Click.view [ class "project-option" ]
                    [ Card.card [ text (ProjectName.toString p) ]
                        |> Card.asContained
                        |> Card.view
                    ]

        selectProject =
            case model.projects of
                NotAsked ->
                    div [ class "projects" ] [ text "Loading" ]

                Loading ->
                    div [ class "projects" ] [ text "Loading" ]

                Failure _ ->
                    div [ class "projects" ] [ text "Error" ]

                Success projects ->
                    section [ class "projects" ]
                        [ div [ class "project-cards" ]
                            (List.map viewProjectOption projects)
                        ]

        welcomeHeader =
            header [ class "welcome-header" ]
                [ div [ class "app-logo" ]
                    [ img [ src "/src/assets/app-icon.png", alt "UCM App Icon", class "app-icon" ] []
                    , div []
                        [ h2 [] [ text "Unison Codebase Manager" ]
                        , p [ class "unison-version" ] [ text "Version: release/0.5.26 (built on 2024-09-05)" ]
                        ]
                    ]
                , TextField.fieldWithoutLabel UpdateSearchQuery "Search projects" model.searchQuery
                    |> TextField.withIcon Icon.search
                    |> TextField.view
                ]

        windowContent =
            [ welcomeHeader
            , selectProject
            ]
    in
    Window.window "welcome-screen"
        |> Window.withTitlebarRight
            [ Button.iconThenLabel_ Link.docs Icon.graduationCap "Unison Docs"
                |> Button.small
                |> Button.view
            , Button.iconThenLabel_ Link.share Icon.browse "Find libraries on Unison Share"
                |> Button.small
                |> Button.view
            ]
        |> Window.withoutTitlebarBorder
        |> Window.withContent windowContent
