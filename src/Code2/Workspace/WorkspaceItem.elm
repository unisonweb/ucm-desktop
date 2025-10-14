module Code2.Workspace.WorkspaceItem exposing (..)

import Code.Definition.AbilityConstructor exposing (AbilityConstructor(..))
import Code.Definition.DataConstructor exposing (DataConstructor(..))
import Code.Definition.Info as Info
import Code.Definition.Reference as Reference exposing (Reference)
import Code.Definition.Term exposing (Term(..))
import Code.Definition.Type exposing (Type(..))
import Code.FullyQualifiedName exposing (FQN)
import Code2.Workspace.DefinitionItem exposing (DefinitionItem(..))
import Code2.Workspace.DefinitionMatch exposing (DefinitionMatch)
import Code2.Workspace.DefinitionMatchesState exposing (DefinitionMatchesState)
import Code2.Workspace.DefinitionWorkspaceItemState exposing (DefinitionWorkspaceItemState)
import Code2.Workspace.WorkspaceItemRef exposing (SearchResultsRef, WorkspaceItemRef(..))
import Http
import Maybe.Extra as MaybeE


type alias SearchResultsItem =
    { ref : SearchResultsRef }


type LoadedWorkspaceItem
    = DefinitionWorkspaceItem Reference DefinitionWorkspaceItemState DefinitionItem
    | SearchResultsWorkspaceItem SearchResultsItem
    | DependentsWorkspaceItem Reference DefinitionMatchesState DefinitionItem (List DefinitionMatch)
    | DependenciesWorkspaceItem Reference DefinitionMatchesState DefinitionItem (List DefinitionMatch)


type WorkspaceItem
    = Loading WorkspaceItemRef
    | Failure WorkspaceItemRef Http.Error
    | Success WorkspaceItemRef LoadedWorkspaceItem


reference : WorkspaceItem -> WorkspaceItemRef
reference item =
    case item of
        Loading ref ->
            ref

        Failure ref _ ->
            ref

        Success ref _ ->
            ref


definitionReference : WorkspaceItem -> Maybe Reference
definitionReference item =
    let
        iRef =
            reference item
    in
    case iRef of
        SearchResultsItemRef _ ->
            Nothing

        DefinitionItemRef ref ->
            Just ref

        DependentsItemRef ref ->
            Just ref

        DependenciesItemRef ref ->
            Just ref


allFqns : WorkspaceItem -> List FQN
allFqns item =
    let
        fromRef =
            MaybeE.values
                [ item
                    |> definitionReference
                    |> Maybe.andThen Reference.fqn
                ]
    in
    case item of
        Success _ loadedItem ->
            case loadedItem of
                DefinitionWorkspaceItem _ _ (TermItem (Term _ _ { info })) ->
                    Info.allFqns info

                DefinitionWorkspaceItem _ _ (TypeItem (Type _ _ { info })) ->
                    Info.allFqns info

                DefinitionWorkspaceItem _ _ (AbilityConstructorItem (AbilityConstructor _ { info })) ->
                    Info.allFqns info

                DefinitionWorkspaceItem _ _ (DataConstructorItem (DataConstructor _ { info })) ->
                    Info.allFqns info

                _ ->
                    fromRef

        _ ->
            fromRef


isSameRef : WorkspaceItem -> WorkspaceItemRef -> Bool
isSameRef item ref =
    reference item == ref


isSameByRef : WorkspaceItem -> WorkspaceItem -> Bool
isSameByRef a b =
    reference a == reference b
