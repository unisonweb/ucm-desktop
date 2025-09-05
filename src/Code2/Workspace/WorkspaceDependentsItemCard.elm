module Code2.Workspace.WorkspaceDependentsItemCard exposing (view)

import Code.Definition.Reference as Reference exposing (Reference)
import Code.FullyQualifiedName as FQN
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Code2.Workspace.DefinitionItem as DefinitionItem exposing (DefinitionItem)
import Code2.Workspace.DefinitionMatch exposing (DefinitionMatch(..))
import Code2.Workspace.WorkspaceCard as WorkspaceCard exposing (WorkspaceCard)
import Code2.Workspace.WorkspaceItemRef exposing (WorkspaceItemRef)
import Html exposing (Html, div, header, strong, text)
import Html.Attributes exposing (class)
import Lib.String.Helpers exposing (pluralize)
import UI
import UI.Click as Click
import UI.Icon as Icon exposing (Icon)


type alias ViewConfig msg =
    { wsRef : WorkspaceItemRef
    , item : DefinitionItem
    , dependents : List DefinitionMatch
    , syntaxConfig : SyntaxConfig.SyntaxConfig msg
    , closeItem : msg
    , openDefinition : Reference -> msg
    }


type alias GroupedDependents =
    { terms : List DefinitionMatch
    , types : List DefinitionMatch
    , abilities : List DefinitionMatch
    , docs : List DefinitionMatch
    }


groupDependents : List DefinitionMatch -> GroupedDependents
groupDependents dependents =
    let
        f dep acc =
            case dep of
                TermMatch _ ->
                    { acc | terms = dep :: acc.terms }

                TypeMatch _ ->
                    { acc | types = dep :: acc.types }

                AbilityConstructorMatch _ ->
                    { acc | abilities = dep :: acc.abilities }

                DataConstructorMatch _ ->
                    { acc | docs = dep :: acc.docs }
    in
    List.foldl f { terms = [], types = [], abilities = [], docs = [] } dependents


viewDependent : (Reference -> msg) -> DefinitionMatch -> Html msg
viewDependent openDefinition match =
    let
        ( name, ref ) =
            case match of
                TermMatch { displayName, fqn } ->
                    ( FQN.view displayName, Reference.fromFQN Reference.TermReference fqn )

                TypeMatch { displayName, fqn } ->
                    ( FQN.view displayName, Reference.fromFQN Reference.TypeReference fqn )

                DataConstructorMatch { displayName, fqn } ->
                    ( FQN.view displayName, Reference.fromFQN Reference.TypeReference fqn )

                AbilityConstructorMatch { displayName, fqn } ->
                    ( FQN.view displayName, Reference.fromFQN Reference.TypeReference fqn )
    in
    Click.onClick (openDefinition ref)
        |> Click.stopPropagation
        |> Click.view [ class "dependent" ] [ name ]


viewDependents : (Reference -> msg) -> Icon msg -> String -> String -> List DefinitionMatch -> Html msg
viewDependents openDefinition icon titleSingular titlePlural dependents =
    let
        numDeps =
            List.length dependents
    in
    div [ class "dependents_column" ]
        [ header []
            [ Icon.view icon
            , text (String.fromInt numDeps ++ " " ++ pluralize titleSingular titlePlural numDeps)
            ]
        , div [ class "dependents_items" ] (List.map (viewDependent openDefinition) dependents)
        ]


viewTerms : (Reference -> msg) -> List DefinitionMatch -> Html msg
viewTerms openDefinition terms =
    viewDependents openDefinition Icon.term "Term" "Terms" terms


viewTypes : (Reference -> msg) -> List DefinitionMatch -> Html msg
viewTypes openDefinition types =
    viewDependents openDefinition Icon.type_ "Type" "Types" types


viewAbilities : (Reference -> msg) -> List DefinitionMatch -> Html msg
viewAbilities openDefinition abilities =
    viewDependents openDefinition Icon.ability "Ability" "Abilities" abilities


viewDocs : (Reference -> msg) -> List DefinitionMatch -> Html msg
viewDocs openDefinition docs =
    viewDependents openDefinition Icon.doc "Doc" "Docs" docs


blankIfEmpty : Html msg -> List a -> Html msg
blankIfEmpty html xs =
    if List.isEmpty xs then
        UI.nothing

    else
        html


view : ViewConfig msg -> WorkspaceCard msg
view cfg =
    let
        lib =
            cfg.item
                |> DefinitionItem.toLib
                |> Maybe.map WorkspaceCard.viewLibraryTag
                |> Maybe.withDefault UI.nothing

        itemContent =
            case cfg.item of
                DefinitionItem.TermItem _ ->
                    div [ class "workspace-dependents-item-card_content" ]
                        [ viewTerms cfg.openDefinition cfg.dependents
                        ]

                _ ->
                    let
                        { terms, types, abilities, docs } =
                            groupDependents cfg.dependents
                    in
                    div [ class "workspace-dependents-item-card_content" ]
                        [ blankIfEmpty (viewTerms cfg.openDefinition terms) terms
                        , blankIfEmpty (viewTypes cfg.openDefinition types) types
                        , blankIfEmpty (viewAbilities cfg.openDefinition abilities) abilities
                        , blankIfEmpty (viewDocs cfg.openDefinition docs) docs
                        ]

        numDeps =
            List.length cfg.dependents
    in
    WorkspaceCard.empty
        |> WorkspaceCard.withTitlebarLeft
            [ lib
            , strong [] [ text (FQN.toString (DefinitionItem.name cfg.item)) ]
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
        |> WorkspaceCard.withContent [ itemContent ]
