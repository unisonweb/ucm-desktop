module Ucm.Workspace.WorkspaceItemRef exposing (..)

import Code.Definition.Reference as Reference exposing (Reference)


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
            Reference.toHumanString r

        SearchResultsItemRef (SearchResultsRef r) ->
            r
