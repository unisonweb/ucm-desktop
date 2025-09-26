module Code2.Workspace.DependentsWorkspaceItemState exposing (..)


type DependentsItemTab
    = TermsTab
    | TypesTab
    | AbilitiesTab
    | DocsTab
    | TestsTab


type alias DependentsWorkspaceItemState =
    { activeTab : DependentsItemTab
    , searchQuery : String
    }


init : DependentsItemTab -> DependentsWorkspaceItemState
init tab =
    { activeTab = tab, searchQuery = "" }
