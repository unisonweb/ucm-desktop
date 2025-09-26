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


equals : WorkspaceItemRef -> WorkspaceItemRef -> Bool
equals a b =
    a == b


{-| Like equals, but focused on hash equality, so even if one WorkspaceItemRef
has an fqn+hash when compared to an WorkspaceItemRef that only has a hash, the
hash comparison wins.
-}
same : WorkspaceItemRef -> WorkspaceItemRef -> Bool
same a b =
    case ( a, b ) of
        ( DefinitionItemRef a_r, DefinitionItemRef b_r ) ->
            Reference.same a_r b_r

        ( DependentsItemRef a_r, DependentsItemRef b_r ) ->
            Reference.same a_r b_r

        _ ->
            a == b


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
            "Definition: " ++ Reference.toString r

        SearchResultsItemRef (SearchResultsRef r) ->
            "Search results:" ++ r

        DependentsItemRef r ->
            "Dependents of :" ++ Reference.toString r


toDomString : WorkspaceItemRef -> String
toDomString ref =
    ref
        |> toString
        |> String.toLower
        |> String.replace ": " "___"
        |> String.replace " " "-"
        |> String.replace "." "__"


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


toUrlPath : WorkspaceItemRef -> List String
toUrlPath wsRef =
    case wsRef of
        DefinitionItemRef r ->
            Reference.toUrlPath r

        DependentsItemRef r ->
            "dependents-of" :: Reference.toUrlPath r

        _ ->
            -- TODO
            [ "search" ]
