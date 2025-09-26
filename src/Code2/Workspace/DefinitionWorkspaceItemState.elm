module Code2.Workspace.DefinitionWorkspaceItemState exposing (..)

import Code.Definition.Doc as Doc exposing (DocFoldToggles)


type DefinitionItemTab
    = CodeTab
    | DocsTab


type alias DefinitionWorkspaceItemState =
    { activeTab : DefinitionItemTab
    , namespaceDropdownIsOpen : Bool
    , docFoldToggles : DocFoldToggles
    , isCodeFolded : Bool
    }


init : DefinitionItemTab -> DefinitionWorkspaceItemState
init tab =
    { activeTab = tab
    , namespaceDropdownIsOpen = False
    , docFoldToggles = Doc.emptyDocFoldToggles
    , isCodeFolded = True
    }
