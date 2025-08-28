module Code2.Workspace.WorkspaceItemRef exposing (..)

import Code.Definition.Reference as Reference exposing (Reference)
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Maybe.Extra as MaybeE


type SearchResultsRef
    = SearchResultsRef String


type WorkspaceItemRef
    = DefinitionItemRef Reference
    | SearchResultsItemRef SearchResultsRef
    | DependentsItemRef Reference


definitionReference : WorkspaceItemRef -> Maybe Reference
definitionReference ref =
    case ref of
        DefinitionItemRef r ->
            Just r

        DependentsItemRef r ->
            Just r

        _ ->
            Nothing


toString : WorkspaceItemRef -> String
toString ref =
    case ref of
        DefinitionItemRef r ->
            Reference.toString r

        SearchResultsItemRef (SearchResultsRef r) ->
            r

        DependentsItemRef r ->
            Reference.toString r


toDomString : WorkspaceItemRef -> String
toDomString ref =
    ref |> toString |> String.replace "." "__"


toHumanString : WorkspaceItemRef -> String
toHumanString ref =
    let
        defRefToString r =
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
    in
    case ref of
        DefinitionItemRef r ->
            defRefToString r

        SearchResultsItemRef (SearchResultsRef r) ->
            r

        DependentsItemRef r ->
            defRefToString r
