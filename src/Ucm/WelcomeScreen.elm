module Ucm.WelcomeScreen exposing (..)

import Browser
import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.ProjectName as ProjectName exposing (ProjectName(..))
import Code.ProjectNameListing as ProjectNameListing
import Code.ProjectSlug as ProjectSlug
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
import Lib.UserHandle as UserHandle
import RemoteData exposing (RemoteData(..), WebData)
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.Form.TextField as TextField
import UI.Icon as Icon
import Ucm.Api as Api
import Ucm.AppContext exposing (AppContext)
import Ucm.Link as Link
import Ucm.Workspace.WorkspaceContext as WorkspaceContext exposing (WorkspaceContext)
import Window



-- MODEL


type alias Model =
    { searchQuery : String
    , projects : WebData (List ProjectName)
    , window : Window.Model
    }


init : AppContext -> ( Model, Cmd Msg )
init appContext =
    ( { searchQuery = ""
      , projects = Loading
      , window = Window.init
      }
    , fetchProjects appContext
    )



-- UPDATE


type Msg
    = FetchProjectsFinished (WebData (List ProjectName))
    | UpdateSearchQuery String
    | SelectProject ProjectName BranchRef
    | WindowMsg Window.Msg


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
            ( model, WorkspaceContext.save workspaceContext, ChangeScreenToWorkspace workspaceContext )

        WindowMsg wMsg ->
            let
                ( window, wCmd ) =
                    Window.update wMsg model.window
            in
            ( { model | window = window }, Cmd.map WindowMsg wCmd, None )



--EFFECTS


fetchProjects : AppContext -> Cmd Msg
fetchProjects appContext =
    let
        decodeProjectList : Decode.Decoder (List ProjectName)
        decodeProjectList =
            Decode.list <|
                Decode.field "projectName" ProjectName.decode
    in
    Api.projects Nothing
        |> HttpApi.toRequest decodeProjectList (RemoteData.fromResult >> FetchProjectsFinished)
        |> HttpApi.perform appContext.api



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map WindowMsg (Window.subscriptions model.window)



-- VIEW


isMatch : String -> ProjectName -> Bool
isMatch s (ProjectName handle slug) =
    let
        removeAt s_ =
            String.replace "@" "" s_

        handle_ =
            handle
                |> Maybe.map UserHandle.toString
                |> Maybe.map removeAt
                |> Maybe.withDefault ""

        slug_ =
            ProjectSlug.toString slug
    in
    String.startsWith (removeAt s) handle_ || String.startsWith s slug_


view : AppContext -> Model -> Browser.Document Msg
view _ model =
    let
        viewProjectOption p =
            Click.onClick (SelectProject p BranchRef.main_)
                |> Click.view [ class "project-option" ]
                    [ Card.card
                        [ ProjectNameListing.projectNameListing p
                            |> ProjectNameListing.view
                        ]
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
                    let
                        projects_ =
                            if not (String.isEmpty model.searchQuery) then
                                projects
                                    |> List.filter (isMatch model.searchQuery)

                            else
                                projects
                    in
                    section [ class "projects" ]
                        [ div [ class "project-cards" ]
                            (List.map viewProjectOption projects_)
                        ]

        welcomeHeader =
            header [ class "welcome-header" ]
                [ div [ class "app-logo" ]
                    [ img [ src "/src/assets/app-icon.png", alt "UCM App Icon", class "app-icon" ] []
                    , div []
                        [ h2 [] [ text "Unison Codebase Manager" ]

                        -- TODO: insert UCM version
                        -- , p [ class "unison-version" ] [ text "Version: release/0.5.26 (built on 2024-09-05)" ]
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
        |> Window.view WindowMsg model.window
