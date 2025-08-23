module Ucm.SearchProjectSheet exposing (..)

import Code.ProjectName as ProjectName exposing (ProjectName)
import Html exposing (Html, article, div, footer, h2, h3, h4, p, section, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Search as Search exposing (Search)
import Lib.SearchResults as SearchResults
import RemoteData exposing (WebData)
import UI
import UI.Click as Click
import UI.Divider as Divider
import UI.EmptyState as EmptyState
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.Tag as Tag
import Ucm.Api as UcmApi
import Ucm.AppContext exposing (AppContext)



-- MODEL


type alias Model =
    { search : Search ProjectName
    }


init : Model
init =
    { search = Search.empty
    }



-- UPDATE


type Msg
    = UpdateSearchQuery String
    | PerformSearch String
    | ClearSearch
    | FetchSearchResultsFinished String (HttpResult (List ProjectName))
    | SelectProject ProjectName
    | NoOp


type OutMsg
    = NoOutMsg
    | SelectProjectRequest ProjectName


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext msg model =
    case msg of
        UpdateSearchQuery q ->
            let
                newModel =
                    { model | search = Search.withQuery q model.search }

                ( sheet, cmd ) =
                    if Search.queryGreaterThan 1 model.search then
                        ( newModel, Search.debounce (PerformSearch q) )

                    else
                        ( newModel, Cmd.none )
            in
            ( sheet, cmd, NoOutMsg )

        PerformSearch query ->
            if Search.queryEquals query model.search then
                ( { model | search = Search.Searching query Nothing }
                , fetchProjects query appContext
                , NoOutMsg
                )

            else
                ( model, Cmd.none, NoOutMsg )

        ClearSearch ->
            ( { model | search = Search.reset model.search }, Cmd.none, NoOutMsg )

        FetchSearchResultsFinished query projects ->
            if Search.queryEquals query model.search then
                ( { model | search = Search.fromResult model.search projects }, Cmd.none, NoOutMsg )

            else
                ( model, Cmd.none, NoOutMsg )

        NoOp ->
            ( model, Cmd.none, NoOutMsg )

        SelectProject project ->
            ( model, Cmd.none, SelectProjectRequest project )



-- EFFECTS


fetchProjects : String -> AppContext -> Cmd Msg
fetchProjects query appContext =
    let
        decode =
            Decode.list <|
                Decode.field "projectName" ProjectName.decode
    in
    UcmApi.projects (Just query)
        |> HttpApi.toRequest decode (FetchSearchResultsFinished query)
        |> HttpApi.perform appContext.api



-- VIEW
-- really this should be a<projectRef>, but alas no higher kinded types


type alias Suggestions a =
    { data : WebData a
    , view : a -> List (Html Msg)
    }


viewProject : ProjectName -> Html Msg
viewProject projectName =
    Tag.tag (ProjectName.toString projectName)
        |> Tag.large
        |> Tag.withClick (Click.onClick (SelectProject projectName))
        |> Tag.view


viewProjectList : String -> List ProjectName -> Html Msg
viewProjectList title projects =
    section [ class "search-projects switch-project_project-list" ]
        [ h3 [] [ text title ]
        , div [ class "search-project-sheet_project-list_items" ]
            (projects |> List.take 8 |> List.map viewProject)
        ]


view : String -> Suggestions a -> Maybe (Html Msg) -> Model -> Html Msg
view title suggestions footer_ model =
    let
        shape_ length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.tiny

        shape length =
            shape_ length
                |> Placeholder.subdued
                |> Placeholder.view

        shapeBright length =
            shape_ length
                |> Placeholder.view

        loading =
            [ div [ class "search-project-sheet_recent-projects_loading" ]
                [ div [ class "search-project-sheet_projects_loading-section" ]
                    [ shape Placeholder.Small
                    , div [ class "search-project-sheet_projects_loading-list" ]
                        [ shapeBright Placeholder.Tiny
                        , shapeBright Placeholder.Medium
                        , shapeBright Placeholder.Small
                        ]
                    ]
                ]
            ]

        viewSuggestions data =
            case suggestions.view data of
                [] ->
                    []

                xs ->
                    [ div [ class "search-project-sheet_suggestions" ] xs ]

        ( content, isSearching, query ) =
            case ( model.search, suggestions.data ) of
                ( Search.NotAsked q, RemoteData.Success data ) ->
                    ( viewSuggestions data
                    , False
                    , q
                    )

                ( Search.NotAsked q, RemoteData.Failure _ ) ->
                    ( [ StatusBanner.bad "Something broke on our end and we couldn't load the recent projects.\nPlease try again." ], False, q )

                ( Search.NotAsked q, RemoteData.NotAsked ) ->
                    ( loading, False, q )

                ( Search.NotAsked q, RemoteData.Loading ) ->
                    ( loading, False, q )

                ( Search.Searching q _, RemoteData.Success data ) ->
                    ( viewSuggestions data
                        ++ [ div [ class "search-project-sheet_searching" ] [] ]
                    , True
                    , q
                    )

                ( Search.Searching q _, _ ) ->
                    ( [ div [ class "search-project-sheet_searching" ]
                            [ div [ class "search-project-sheet_projects_loading-list" ]
                                [ shapeBright Placeholder.Medium
                                , shapeBright Placeholder.Tiny
                                , shapeBright Placeholder.Small
                                ]
                            ]
                      ]
                    , True
                    , q
                    )

                ( Search.Success q sr, suggestions_ ) ->
                    let
                        results =
                            if SearchResults.isEmpty sr then
                                if q == "" then
                                    case suggestions_ of
                                        RemoteData.Success data ->
                                            viewSuggestions data

                                        _ ->
                                            []

                                else
                                    [ EmptyState.search
                                        |> EmptyState.withContent
                                            [ div [ class "search-project-sheet_no-results_message" ]
                                                [ h4 [] [ text "No matches" ]
                                                , p [] [ text ("We looked everywhere, but couldn't find any projects matching \"" ++ q ++ "\".") ]
                                                ]
                                            ]
                                        |> EmptyState.view
                                    ]

                            else
                                [ viewProjectList "Search results" (SearchResults.toList sr) ]
                    in
                    ( results, False, q )

                ( Search.Failure q _, _ ) ->
                    ( [ StatusBanner.bad "Something broke on our end and we couldn't perform the search. Please try again."
                      ]
                    , False
                    , q
                    )

        content_ =
            if List.isEmpty content then
                []

            else
                [ Divider.divider
                    |> Divider.small
                    |> Divider.withoutMargin
                    |> Divider.view
                , div [ class "search-project-sheet_projects" ] content
                ]

        -- Currently this exists to prevent other keyboard centric interactions
        -- to take over from writing in the input field.
        keyboardEvent =
            KeyboardEvent.on KeyboardEvent.Keydown (always NoOp)
                |> KeyboardEvent.stopPropagation
                |> KeyboardEvent.attach

        footer__ =
            footer_
                |> Maybe.map (\f -> footer [ class "search-project-sheet_more-link" ] [ f ])
                |> Maybe.withDefault UI.nothing
    in
    article [ class "search-project-sheet", keyboardEvent ]
        [ h2 [] [ text title ]
        , Html.node "search"
            []
            ((TextField.fieldWithoutLabel UpdateSearchQuery "Search projects" query
                |> TextField.withIconOrWorking Icon.search isSearching
                |> TextField.withClear ClearSearch
                |> TextField.withAutofocus
                |> TextField.view
             )
                :: content_
            )
        , footer__
        ]
