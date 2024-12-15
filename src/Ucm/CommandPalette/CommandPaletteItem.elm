module Ucm.CommandPalette.CommandPaletteItem exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
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
    , click : Maybe (Click msg)
    }


item : Icon msg -> Html msg -> Click msg -> CommandPaletteItem msg
item icon label click =
    { icon = icon
    , label = label
    , rightSide = None
    , click = Just click
    }


item_ : Icon msg -> Html msg -> CommandPaletteItem msg
item_ icon label =
    { icon = icon
    , label = label
    , rightSide = None
    , click = Nothing
    }


withClick : Click msg -> CommandPaletteItem msg -> CommandPaletteItem msg
withClick click i =
    { i | click = Just click }


withKeyboardShortcut : KeyboardShortcut -> CommandPaletteItem msg -> CommandPaletteItem msg
withKeyboardShortcut shortcut i =
    { i | rightSide = Shortcut shortcut }


command : Icon msg -> String -> KeyboardShortcut -> Click msg -> CommandPaletteItem msg
command icon label shortcut click =
    item icon (text label) click
        |> withKeyboardShortcut shortcut


view : KeyboardShortcut.Model -> CommandPaletteItem msg -> Bool -> Html msg
view keyboardShortcut { icon, label, rightSide, click } isSelected =
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
    case click of
        Just c ->
            Click.view
                [ class "command-palette-item"
                , classList [ ( "selected", isSelected ) ]
                ]
                content
                c

        Nothing ->
            div
                [ class "command-palette-item"
                , classList [ ( "selected", isSelected ) ]
                ]
                content
