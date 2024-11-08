module Ucm.Command exposing (..)

import UI.Icon as Icon exposing (Icon)
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut)


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
