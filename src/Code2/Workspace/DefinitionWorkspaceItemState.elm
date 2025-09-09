module Code2.Workspace.DefinitionWorkspaceItemState exposing (..)

import Code.Definition.Doc exposing (DocFoldToggles)


type DefinitionItemTab
    = CodeTab
    | DocsTab DocFoldToggles


type alias DefinitionWorkspaceItemState =
    { activeTab : DefinitionItemTab
    , namespaceDropdownIsOpen : Bool
    }


init : DefinitionItemTab -> DefinitionWorkspaceItemState
init tab =
    { activeTab = tab, namespaceDropdownIsOpen = False }
