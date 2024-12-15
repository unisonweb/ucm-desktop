module Ucm.Command exposing (..)

import Html exposing (text)
import UI
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut)
import Ucm.CommandPalette.CommandPaletteItem as CommandPaletteItem exposing (CommandPaletteItem)
import Ucm.WorkspaceScreen exposing (Msg(..))


type Command
    = Definition DefinitionCommand
    | Workspace WorkspaceCommand
    | Project ProjectCommand


type WorkspaceCommand
    = OpenDefinition
    | CloseDefinition
    | ToggleCardExpansion { isExpanded : Bool }
    | ClearFocus
    | MoveDefinitionUp
    | MoveDefinitionDown
    | MoveDefinitionRight
    | MoveDefinitionLeft
    | ToggleSplitPane { isOn : Bool }
    | MoveFocusUp
    | MoveFocusDown
    | MoveFocusRight
    | MoveFocusLeft
    | ToggleProjectPane { isOn : Bool }


type ProjectCommand
    = SwitchProject
    | SwitchBranch


type DefinitionCommand
    = Edit


toCommandPaletteItem : (Command -> msg) -> Command -> CommandPaletteItem msg
toCommandPaletteItem selectMsg cmd =
    let
        toItem icon label =
            CommandPaletteItem.item_ icon (text label)
    in
    case cmd of
        Definition Edit ->
            toItem Icon.writingPad "Edit"

        Project SwitchProject ->
            toItem Icon.pencilRuler "Switch project"

        Project SwitchBranch ->
            toItem Icon.branch "Switch branch"

        Workspace OpenDefinition ->
            toItem Icon.browse "Open definition"

        Workspace CloseDefinition ->
            toItem Icon.x "Close definition"

        Workspace (ToggleCardExpansion { isExpanded } ->
          if isExpanded then
              toItem Icon.collapseUp "Collapse card"
          else
            toItem Icon.collapseUp "Expand card"

        Workspace ClearFocus ->
            toItem Icon.x "Clearn focus"

        Workspace MoveDefinitionUp ->
            toItem Icon.writingPad "Move definition up"

        Workspace MoveDefinitionDown ->
            toItem Icon.writingPad "Move definition down"

        Workspace MoveDefinitionRight ->
            toItem Icon.writingPad "Move definition right"

        Workspace MoveDefinitionLeft ->
            toItem Icon.writingPad "Move definition left"

        Workspace (ToggleSplitPane { isOn })->
          if isOn then
            toItem Icon.windowSplit "Hide split pane"
          else isOn
              toItem Icon.windowSplit "Show split pane"

        Workspace MoveFocusUp ->
            toItem Icon.arrowUp "Move focus up"

        Workspace MoveFocusDown ->
            toItem Icon.arrowDown "Move focus down"

        Workspace MoveFocusRight ->
            toItem Icon.arrowRight "Move focus right"

        Workspace MoveFocusLeft ->
            toItem Icon.arrowLeft "Move focus left"

        Workspace (ToggleProjectPane { isOn })->
          if isOn then
            toItem Icon.leftSidebarOn "Hide project pane"
          else
            toItem Icon.leftSidebarOn "Show project pane"
