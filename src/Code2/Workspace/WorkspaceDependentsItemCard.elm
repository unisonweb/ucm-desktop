module Code2.Workspace.WorkspaceDependentsItemCard exposing (..)

import Code.Definition.Reference as Reference exposing (Reference)
import Code.Definition.Term as Term exposing (Term(..))
import Code.Definition.Type as Type exposing (Type(..))
import Code.DefinitionSummaryTooltip as DefinitionSummaryTooltip
import Code.FullyQualifiedName as FQN
import Code.Syntax.SyntaxSegment as SyntaxSegment
import Code2.Workspace.DefinitionItem as DefinitionItem exposing (DefinitionItem)
import Code2.Workspace.DefinitionMatch as DefinitionMatch exposing (DefinitionMatch(..))
import Code2.Workspace.DependentsWorkspaceItemState exposing (DependentsItemTab(..), DependentsWorkspaceItemState)
import Code2.Workspace.WorkspaceCard as WorkspaceCard exposing (WorkspaceCard)
import Code2.Workspace.WorkspaceItemRef exposing (WorkspaceItemRef)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class)
import Lib.String.Helpers exposing (pluralize)
import Maybe.Extra as MaybeE
import RemoteData
import UI
import UI.Click as Click
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.TabList as TabList exposing (tab, tabList)
import UI.Tooltip as Tooltip


type alias ViewConfig msg =
    { wsRef : WorkspaceItemRef
    , dependentsOfRef : Reference
    , item : DefinitionItem
    , dependents : List DefinitionMatch
    , updateQuery : String -> msg
    , closeItem : msg
    , openDefinition : Reference -> msg
    , state : DependentsWorkspaceItemState
    , changeTab : DependentsItemTab -> msg
    }


type alias GroupedDependents =
    { terms : List DefinitionMatch
    , types : List DefinitionMatch
    , abilities : List DefinitionMatch
    , tests : List DefinitionMatch
    , docs : List DefinitionMatch
    }


groupDependents : List DefinitionMatch -> GroupedDependents
groupDependents dependents =
    let
        f dep acc =
            case dep of
                TermMatch { category } ->
                    case category of
                        Term.PlainTerm ->
                            { acc | terms = dep :: acc.terms }

                        Term.TestTerm ->
                            { acc | tests = dep :: acc.tests }

                        Term.DocTerm ->
                            { acc | docs = dep :: acc.docs }

                TypeMatch { category } ->
                    case category of
                        Type.DataType ->
                            { acc | types = dep :: acc.types }

                        Type.AbilityType ->
                            { acc | abilities = dep :: acc.abilities }

                AbilityConstructorMatch _ ->
                    { acc | abilities = dep :: acc.abilities }

                DataConstructorMatch _ ->
                    { acc | types = dep :: acc.types }
    in
    List.foldl f { terms = [], types = [], abilities = [], docs = [], tests = [] } dependents


viewDependent : (Reference -> msg) -> DefinitionMatch -> Html msg
viewDependent openDefinition match =
    let
        ( name, ref, defSum ) =
            case match of
                TermMatch { displayName, hash, category, fqn, summary } ->
                    let
                        termSum =
                            Term hash
                                category
                                { fqn = fqn
                                , name = displayName
                                , namespace = Nothing
                                , signature = summary
                                }
                    in
                    ( displayName
                    , Reference.fromFQN Reference.TermReference fqn
                    , DefinitionSummaryTooltip.TermHover termSum
                    )

                TypeMatch { hash, category, summary, displayName, fqn } ->
                    let
                        typeSum =
                            Type hash
                                category
                                { fqn = fqn
                                , name = displayName
                                , namespace = Nothing
                                , source = summary
                                }
                    in
                    ( displayName
                    , Reference.fromFQN Reference.TypeReference fqn
                    , DefinitionSummaryTooltip.TypeHover typeSum
                    )

                DataConstructorMatch { hash, category, summary, displayName, fqn } ->
                    let
                        termSum =
                            Term hash
                                category
                                { fqn = fqn
                                , name = displayName
                                , namespace = Nothing
                                , signature = summary
                                }
                    in
                    ( displayName
                    , Reference.fromFQN Reference.TypeReference fqn
                    , DefinitionSummaryTooltip.TermHover termSum
                    )

                AbilityConstructorMatch { hash, category, summary, displayName, fqn } ->
                    let
                        termSum =
                            Term hash
                                category
                                { fqn = fqn
                                , name = displayName
                                , namespace = Nothing
                                , signature = summary
                                }
                    in
                    ( displayName
                    , Reference.fromFQN Reference.TypeReference fqn
                    , DefinitionSummaryTooltip.TermHover termSum
                    )

        tooltipContent =
            DefinitionSummaryTooltip.viewSummary (RemoteData.Success defSum)

        content =
            case tooltipContent of
                Just c ->
                    c
                        |> Tooltip.tooltip
                        |> Tooltip.below
                        |> Tooltip.withArrow Tooltip.Start
                        |> Tooltip.view
                            (SyntaxSegment.viewFQN name)

                Nothing ->
                    SyntaxSegment.viewFQN name
    in
    Click.onClick (openDefinition ref)
        |> Click.stopPropagation
        |> Click.view [ class "dependent fqn" ] [ content ]


viewDependents : String -> (Reference -> msg) -> List DefinitionMatch -> Html msg
viewDependents className openDefinition dependents =
    let
        dependents_ =
            dependents
                |> List.sortBy (DefinitionMatch.displayName >> FQN.toString >> String.toLower)
                |> List.map (viewDependent openDefinition)
    in
    div [ class "dependents_column rich" ]
        [ div [ class ("dependents_items syntax " ++ className) ]
            dependents_
        ]


blankIfEmpty : Html msg -> List a -> Html msg
blankIfEmpty html xs =
    if List.isEmpty xs then
        UI.nothing

    else
        html


dependentsItemTab :
    (DependentsItemTab -> msg)
    -> GroupedDependents
    ->
        { terms : Maybe (TabList.Tab msg)
        , types : Maybe (TabList.Tab msg)
        , abilities : Maybe (TabList.Tab msg)
        , docs : Maybe (TabList.Tab msg)
        , tests : Maybe (TabList.Tab msg)
        }
dependentsItemTab changeTab group =
    let
        mkTab title msg dependents =
            if List.isEmpty dependents then
                Nothing

            else
                Just (tab title (Click.onClick (changeTab msg)) |> TabList.withCount (List.length dependents))
    in
    { terms = mkTab "Terms" TermsTab group.terms
    , types = mkTab "Types" TypesTab group.types
    , abilities = mkTab "Abilities" AbilitiesTab group.abilities
    , docs = mkTab "Docs" DocsTab group.docs
    , tests = mkTab "Tests" TestsTab group.tests
    }


withTabList : ViewConfig msg -> GroupedDependents -> WorkspaceCard msg -> WorkspaceCard msg
withTabList cfg group card =
    let
        tabs =
            dependentsItemTab cfg.changeTab group

        tabOrCard before tab_ after =
            case tab_ of
                Just t ->
                    card
                        |> WorkspaceCard.withTabList
                            (tabList (MaybeE.values before) t (MaybeE.values after))

                Nothing ->
                    card
    in
    if isSearching cfg then
        card

    else
        case cfg.state.activeTab of
            TermsTab ->
                tabOrCard [] tabs.terms [ tabs.types, tabs.abilities, tabs.docs, tabs.tests ]

            TypesTab ->
                tabOrCard [ tabs.terms ] tabs.types [ tabs.abilities, tabs.docs, tabs.tests ]

            AbilitiesTab ->
                tabOrCard [ tabs.terms, tabs.types ] tabs.abilities [ tabs.docs, tabs.tests ]

            DocsTab ->
                tabOrCard [ tabs.terms, tabs.types, tabs.abilities ] tabs.docs [ tabs.tests ]

            TestsTab ->
                tabOrCard [ tabs.terms, tabs.types, tabs.abilities, tabs.docs ] tabs.tests []


activeTab : GroupedDependents -> DependentsItemTab -> Maybe DependentsItemTab
activeTab { terms, types, tests, abilities, docs } tab =
    let
        tabWithItems t items =
            if List.isEmpty items then
                Nothing

            else
                Just t

        tabs_ =
            [ tabWithItems TermsTab terms
            , tabWithItems TypesTab types
            , tabWithItems AbilitiesTab abilities
            , tabWithItems DocsTab docs
            , tabWithItems TestsTab tests
            ]
                |> MaybeE.values
    in
    if List.member tab tabs_ then
        Just tab

    else
        List.head tabs_


dependentsByQuery : String -> List DefinitionMatch -> List DefinitionMatch
dependentsByQuery query deps =
    List.filter
        (DefinitionMatch.fqn
            >> FQN.toString
            >> String.toLower
            >> String.contains query
        )
        deps


viewSearchResults : ViewConfig msg -> Html msg
viewSearchResults cfg =
    let
        { terms, types, tests, abilities, docs } =
            cfg.dependents
                |> dependentsByQuery cfg.state.searchQuery
                |> groupDependents

        results =
            [ blankIfEmpty (viewDependents "term-reference" cfg.openDefinition terms) terms
            , blankIfEmpty (viewDependents "type-reference" cfg.openDefinition types) types
            , blankIfEmpty (viewDependents "type-reference" cfg.openDefinition abilities) abilities
            , blankIfEmpty (viewDependents "term-reference" cfg.openDefinition docs) docs
            , blankIfEmpty (viewDependents "term-reference" cfg.openDefinition tests) tests
            ]
    in
    div [ class "search-results" ] results


isSearching : ViewConfig msg -> Bool
isSearching cfg =
    not (String.isEmpty cfg.state.searchQuery)


view : ViewConfig msg -> WorkspaceCard msg
view cfg =
    let
        lib =
            cfg.item
                |> DefinitionItem.toLib
                |> Maybe.map WorkspaceCard.viewLibraryTag
                |> Maybe.withDefault UI.nothing

        ({ terms, types, tests, abilities, docs } as group) =
            groupDependents cfg.dependents

        itemContent =
            let
                content =
                    if isSearching cfg then
                        viewSearchResults cfg

                    else
                        case activeTab group cfg.state.activeTab of
                            Just TermsTab ->
                                viewDependents "term-reference" cfg.openDefinition terms

                            Just TypesTab ->
                                viewDependents "type-reference" cfg.openDefinition types

                            Just AbilitiesTab ->
                                viewDependents "type-reference" cfg.openDefinition abilities

                            Just DocsTab ->
                                viewDependents "term-reference" cfg.openDefinition docs

                            Just TestsTab ->
                                viewDependents "term-reference" cfg.openDefinition tests

                            Nothing ->
                                div [ class "empty-state" ]
                                    [ FQN.view (DefinitionItem.name cfg.item)
                                    , text "has no direct dependents."
                                    ]
            in
            div [ class "workspace-dependents-item-card_content" ]
                [ content
                ]

        numDeps =
            List.length cfg.dependents

        dependentsOfDefinition =
            Click.onClick (cfg.openDefinition cfg.dependentsOfRef)
                |> Click.view [] [ FQN.view (DefinitionItem.name cfg.item) ]

        search =
            div [ class "search-dependents" ]
                [ TextField.fieldWithoutLabel cfg.updateQuery "Search dependents" cfg.state.searchQuery
                    |> TextField.withIcon Icon.search
                    |> TextField.view
                ]

        withSearch card =
            if List.length cfg.dependents > 5 then
                WorkspaceCard.withSubtitleBar search card

            else
                card
    in
    WorkspaceCard.empty
        |> WorkspaceCard.withTitlebarLeft
            [ lib
            , dependentsOfDefinition
            , strong [ class "subdued" ]
                [ text
                    (String.fromInt numDeps
                        ++ " "
                        ++ pluralize "direct dependent"
                            "direct dependents"
                            numDeps
                    )
                ]
            ]
        |> WorkspaceCard.withClose cfg.closeItem
        |> withTabList cfg group
        |> withSearch
        |> WorkspaceCard.withContent
            [ itemContent
            ]
