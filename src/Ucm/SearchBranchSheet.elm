module Ucm.SearchBranchSheet exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.ProjectName exposing (ProjectName)
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
    { search : Search BranchRef
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
    | FetchSearchResultsFinished String (HttpResult (List BranchRef))
    | SelectBranch BranchRef
    | NoOp


type OutMsg
    = NoOutMsg
    | SelectBranchRequest BranchRef


update : AppContext -> ProjectName -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectName msg model =
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
                , fetchBranches query appContext projectName
                , NoOutMsg
                )

            else
                ( model, Cmd.none, NoOutMsg )

        ClearSearch ->
            ( { model | search = Search.reset model.search }, Cmd.none, NoOutMsg )

        FetchSearchResultsFinished query branches ->
            if Search.queryEquals query model.search then
                ( { model | search = Search.fromResult model.search branches }, Cmd.none, NoOutMsg )

            else
                ( model, Cmd.none, NoOutMsg )

        NoOp ->
            ( model, Cmd.none, NoOutMsg )

        SelectBranch branch ->
            ( model, Cmd.none, SelectBranchRequest branch )



-- EFFECTS


fetchBranches : String -> AppContext -> ProjectName -> Cmd Msg
fetchBranches query appContext projectName =
    let
        decode =
            Decode.list <|
                Decode.field "branchName" BranchRef.decode
    in
    UcmApi.projectBranches projectName (Just query)
        |> HttpApi.toRequest decode (FetchSearchResultsFinished query)
        |> HttpApi.perform appContext.api



-- VIEW
-- really this should be a<BranchRef>, but alas no higher kinded types


type alias Suggestions a =
    { data : WebData a
    , view : a -> List (Html Msg)
    }


viewBranch : BranchRef -> Html Msg
viewBranch branchRef =
    BranchRef.toTag branchRef
        |> Tag.large
        |> Tag.withClick (Click.onClick (SelectBranch branchRef))
        |> Tag.view


viewBranchList : String -> List BranchRef -> Html Msg
viewBranchList title branches =
    section [ class "search-branch switch-branch_branch-list" ]
        [ h3 [] [ text title ]
        , div [ class "search-branch-sheet_branch-list_items" ]
            (branches |> List.take 8 |> List.map viewBranch)
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
            [ div [ class "search-branch-sheet_recent-branches_loading" ]
                [ div [ class "search-branch-sheet_branches_loading-section" ]
                    [ shape Placeholder.Small
                    , div [ class "search-branch-sheet_branches_loading-list" ]
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
                    [ div [ class "search-branch-sheet_suggestions" ] xs ]

        ( content, isSearching, query ) =
            case ( model.search, suggestions.data ) of
                ( Search.NotAsked q, RemoteData.Success data ) ->
                    ( viewSuggestions data
                    , False
                    , q
                    )

                ( Search.NotAsked q, RemoteData.Failure _ ) ->
                    ( [ StatusBanner.bad "Something broke on our end and we couldn't load the recent branches.\nPlease try again." ], False, q )

                ( Search.NotAsked q, RemoteData.NotAsked ) ->
                    ( loading, False, q )

                ( Search.NotAsked q, RemoteData.Loading ) ->
                    ( loading, False, q )

                ( Search.Searching q _, RemoteData.Success data ) ->
                    ( viewSuggestions data
                        ++ [ div [ class "search-branch-sheet_searching" ] [] ]
                    , True
                    , q
                    )

                ( Search.Searching q _, _ ) ->
                    ( [ div [ class "search-branch-sheet_searching" ]
                            [ div [ class "search-branch-sheet_branches_loading-list" ]
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
                                        |> EmptyState.onDark
                                        |> EmptyState.withContent
                                            [ div [ class "search-branch-sheet_no-results_message" ]
                                                [ h4 [] [ text "No matches" ]
                                                , p [] [ text ("We looked everywhere, but couldn't find any branches matching \"" ++ q ++ "\".") ]
                                                ]
                                            ]
                                        |> EmptyState.view
                                    ]

                            else
                                [ viewBranchList "Search results" (SearchResults.toList sr) ]
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
                    |> Divider.onDark
                    |> Divider.withoutMargin
                    |> Divider.view
                , div [ class "search-branch-sheet_branches" ] content
                ]

        -- Currently this exists to prevent other keyboard centric interactions
        -- to take over from writing in the input field.
        keyboardEvent =
            KeyboardEvent.on KeyboardEvent.Keydown (always NoOp)
                |> KeyboardEvent.stopPropagation
                |> KeyboardEvent.attach

        footer__ =
            footer_
                |> Maybe.map (\f -> footer [ class "search-branch-sheet_more-link" ] [ f ])
                |> Maybe.withDefault UI.nothing
    in
    article [ class "search-branch-sheet", keyboardEvent ]
        [ h2 [] [ text title ]
        , Html.node "search"
            []
            ((TextField.fieldWithoutLabel UpdateSearchQuery "Search Branches" query
                |> TextField.withHelpText "Find a contributor branch by prefixing their handle, ex: \"@unison\"."
                |> TextField.withIconOrWorking Icon.search isSearching
                |> TextField.withClear ClearSearch
                |> TextField.withAutofocus
                |> TextField.view
             )
                :: content_
            )
        , footer__
        ]
