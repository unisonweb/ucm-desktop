module Ucm.Command exposing (..)

import UI.Icon as Icon exposing (Icon)
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut)


type WorkspaceCommand
    = OpenDefinition
    | CloseDefinition
    | CollapseCard
    | ClearFocus
    | MoveDefinitionUp
    | MoveDefinitionDown
    | MoveDefinitionRight
    | MoveDefinitionLeft


type Project
    = SwitchProject
    | SwitchBranch


type CommandGroup
    = Definition
    | Workspace
    | Project
    | Branch


type alias CommandDetail msg =
    { icon : Icon msg
    , label : String
    , group : CommandGroup
    , keyboardShortcut : KeyboardShortcut
    }
