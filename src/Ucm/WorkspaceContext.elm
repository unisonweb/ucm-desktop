module Ucm.WorkspaceContext exposing (..)

import Code.BranchRef as BranchRef
import Ucm.ProjectName exposing (ProjectName)


type alias WorkspaceContext =
    { projectName : ProjectName, branchRef : BranchRef.BranchRef }
