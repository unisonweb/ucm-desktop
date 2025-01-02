module Ucm.Workspace.WorkspaceItemRef exposing (..)

import Code.Definition.Reference as Reference exposing (Reference)
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Maybe.Extra as MaybeE


type SearchResultsRef
    = SearchResultsRef String


type WorkspaceItemRef
    = DefinitionItemRef Reference
    | SearchResultsItemRef SearchResultsRef


toString : WorkspaceItemRef -> String
toString ref =
    case ref of
        DefinitionItemRef r ->
            Reference.toString r

        SearchResultsItemRef (SearchResultsRef r) ->
            r


toHumanString : WorkspaceItemRef -> String
toHumanString ref =
    case ref of
        DefinitionItemRef r ->
            let
                fqn =
                    Reference.fqn r

                hash =
                    Reference.hash r
            in
            fqn
                |> Maybe.map FQN.toString
                |> MaybeE.or (Maybe.map Hash.toShortString hash)
                |> Maybe.withDefault (Reference.toHumanString r)

        SearchResultsItemRef (SearchResultsRef r) ->
            r
