module Ucm.CommandPalette.CommandPaletteItem exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import UI.Click as Click exposing (Click)
import UI.Icon as Icon exposing (Icon)
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut)


type RightSide msg
    = None
    | Custom (List (Html msg))
    | Shortcut KeyboardShortcut


type alias CommandPaletteItem msg =
    { icon : Icon msg
    , label : Html msg
    , rightSide : RightSide msg
    , click : Click msg
    }


item : Icon msg -> Html msg -> Click msg -> CommandPaletteItem msg
item icon label click =
    { icon = icon
    , label = label
    , rightSide = None
    , click = click
    }


withKeyboardShortcut : KeyboardShortcut -> CommandPaletteItem msg -> CommandPaletteItem msg
withKeyboardShortcut shortcut item_ =
    { item_ | rightSide = Shortcut shortcut }


command : Icon msg -> String -> KeyboardShortcut -> Click msg -> CommandPaletteItem msg
command icon label shortcut click =
    item icon (text label) click
        |> withKeyboardShortcut shortcut


view : KeyboardShortcut.Model -> CommandPaletteItem msg -> Html msg
view keyboardShortcut { icon, label, rightSide, click } =
    let
        rightSide_ =
            case rightSide of
                None ->
                    []

                Custom right ->
                    right

                Shortcut shortcut ->
                    [ KeyboardShortcut.view keyboardShortcut shortcut ]

        content =
            [ div [ class "command-palette-item_left-side" ]
                [ Icon.view icon
                , label
                ]
            , div [ class "command-palette-item_right-side" ]
                rightSide_
            ]
    in
    Click.view [ class "command-palette-item" ] content click
