module Code2.Workspace.WorkspaceDependenciesItemCard exposing (..)

import Code.Definition.Reference exposing (Reference)
import Code2.Workspace.DefinitionItem exposing (DefinitionItem)
import Code2.Workspace.DefinitionMatch exposing (DefinitionMatch)
import Code2.Workspace.DefinitionMatchesState exposing (DefinitionMatchesCardTab, DefinitionMatchesState)
import Code2.Workspace.WorkspaceCard exposing (WorkspaceCard)
import Code2.Workspace.WorkspaceDefinitionMatchesCard as WorkspaceDefinitionMatchesCard
import Code2.Workspace.WorkspaceItemRef exposing (WorkspaceItemRef)
import Lib.String.Helpers exposing (pluralize)


type alias ViewConfig msg =
    { wsRef : WorkspaceItemRef
    , dependenciesOfRef : Reference
    , item : DefinitionItem
    , dependencies : List DefinitionMatch
    , updateQuery : String -> msg
    , closeItem : msg
    , openDefinition : Reference -> msg
    , state : DefinitionMatchesState
    , changeTab : DefinitionMatchesCardTab -> msg
    }


view : ViewConfig msg -> WorkspaceCard msg
view cfg =
    let
        toSubTitle numDeps =
            String.fromInt numDeps
                ++ " "
                ++ pluralize "direct dependencies"
                    "direct dependencies"
                    numDeps

        cfg_ =
            { wsRef = cfg.wsRef
            , contextItem = cfg.item
            , contextItemRef = cfg.dependenciesOfRef
            , toSubTitle = toSubTitle
            , searchPlaceholder = "Search dependencies"
            , emptyStateMessage = "has no direct dependencies."
            , matches = cfg.dependencies
            , updateQuery = cfg.updateQuery
            , closeItem = cfg.closeItem
            , openDefinition = cfg.openDefinition
            , state = cfg.state
            , changeTab = cfg.changeTab
            }
    in
    WorkspaceDefinitionMatchesCard.view cfg_
