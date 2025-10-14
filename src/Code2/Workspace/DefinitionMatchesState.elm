module Code2.Workspace.DefinitionMatchesState exposing (..)


type DefinitionMatchesCardTab
    = TermsTab
    | TypesTab
    | AbilitiesTab
    | DocsTab
    | TestsTab


type alias DefinitionMatchesState =
    { activeTab : DefinitionMatchesCardTab
    , searchQuery : String
    }


init : DefinitionMatchesCardTab -> DefinitionMatchesState
init tab =
    { activeTab = tab, searchQuery = "" }
