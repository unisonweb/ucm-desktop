module Ucm.Project exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Json.Decode as Decode exposing (nullable)
import Json.Decode.Pipeline exposing (required)
import UI.DateTime as DateTime exposing (DateTime)
import Ucm.ProjectName as ProjectName exposing (ProjectName)



-- A Unison Local Project is not the same as a Unison Share Project.
-- There's no reference (instead there's a ProjectName), there's no favs or
-- weekly downloads, summary, or tags.


type alias Project =
    { id : String
    , name : ProjectName
    , defaultBranch : Maybe BranchRef
    , lastActiveBranch : Maybe BranchRef
    , createdAt : DateTime
    , updatedAt : DateTime
    }



-- DECODE


decode : Decode.Decoder Project
decode =
    let
        makeProject id name defaultBranch lastActiveBranch createdAt updatedAt =
            { id = id
            , name = name
            , defaultBranch = defaultBranch
            , lastActiveBranch = lastActiveBranch
            , createdAt = createdAt
            , updatedAt = updatedAt
            }
    in
    Decode.succeed makeProject
        |> required "id" Decode.string
        |> required "name" ProjectName.decode
        |> required "defaultBranch" (nullable BranchRef.decode)
        |> required "lastActiveBranch" (nullable BranchRef.decode)
        |> required "createdAt" DateTime.decode
        |> required "updatedAt" DateTime.decode
