module Ucm.CommandPalette.CommandPaletteModal exposing (..)

import Html exposing (Html, div, header, text)
import Html.Attributes exposing (class)
import OrderedDict exposing (OrderedDict)
import UI
import UI.Form.TextField as TextField
import UI.Icon as Icon exposing (Icon)
import UI.Modal as Modal
import Ucm.CommandPalette.CommandPaletteItem exposing (CommandPaletteItem)


type alias Filter msg =
    { icon : Icon msg, label : String }


type alias CommandPaletteModal msg =
    { filters : List (Filter msg)
    , query : String
    , items : OrderedDict String (List (CommandPaletteItem msg))
    }


viewFilter : Filter msg -> Html msg
viewFilter filter =
    div [ class "command-palette_filter" ]
        [ Icon.view filter.icon
        , text filter.label
        ]


type alias ViewConfig msg =
    { updateQueryMsg : String -> msg
    , closeMsg : msg
    }


view : ViewConfig msg -> CommandPaletteModal msg -> Html msg
view viewConfig palette =
    let
        filters =
            if not (List.isEmpty palette.filters) then
                div [ class "command-palette_filters" ]
                    (List.map viewFilter palette.filters)

            else
                UI.nothing

        content =
            div [ class "command-palette-modal" ]
                [ header [ class "command-palette_query" ]
                    [ filters
                    , TextField.fieldWithoutLabel viewConfig.updateQueryMsg
                        " Type a command or search definitions..."
                        palette.query
                        |> TextField.view
                    ]
                ]
    in
    Modal.modal "command-palette-modal" viewConfig.closeMsg (Modal.content content)
        |> Modal.view
