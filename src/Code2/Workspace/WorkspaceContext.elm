port module Code2.Workspace.WorkspaceContext exposing (WorkspaceContext, decode, save)

import Code.BranchRef as BranchRef
import Code.ProjectName as ProjectName exposing (ProjectName)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias WorkspaceContext =
    { projectName : ProjectName, branchRef : BranchRef.BranchRef }


save : WorkspaceContext -> Cmd msg
save context =
    let
        json =
            Encode.object
                [ ( "projectName", context.projectName |> ProjectName.toString |> Encode.string )
                , ( "branchRef", context.branchRef |> BranchRef.toString |> Encode.string )
                ]
    in
    saveWorkspaceContext json


port saveWorkspaceContext : Encode.Value -> Cmd msg


decode : Decode.Decoder WorkspaceContext
decode =
    Decode.succeed WorkspaceContext
        |> required "projectName" ProjectName.decode
        |> required "branchRef" BranchRef.decode
