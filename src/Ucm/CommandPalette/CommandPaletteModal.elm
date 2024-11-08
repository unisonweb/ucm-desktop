module Ucm.CommandPalette.CommandPaletteModal exposing (..)

import Html exposing (Html, div, header, text)
import Html.Attributes exposing (class)
import Lib.SearchResults as SearchResults exposing (SearchResults)
import OrderedDict exposing (OrderedDict)
import UI
import UI.Form.TextField as TextField
import UI.Icon as Icon exposing (Icon)
import UI.KeyboardShortcut as KeyboardShortcut
import UI.Modal as Modal exposing (Modal)
import Ucm.CommandPalette.CommandPaletteItem as CommandPaletteItem exposing (CommandPaletteItem)


type alias Filter msg =
    { icon : Icon msg, label : String }


type alias SearchResultItems msg =
    SearchResults (CommandPaletteItem msg)


type alias CommandPaletteModal msg =
    { filters : List (Filter msg)
    , query : String
    , commandItems : OrderedDict String (List (CommandPaletteItem msg))
    , searchResultItems : Maybe (SearchResultItems msg)
    }



-- CREATE


empty : CommandPaletteModal msg
empty =
    { filters = []
    , query = ""
    , commandItems = OrderedDict.empty
    , searchResultItems = Nothing
    }


withCommandItems : String -> List (CommandPaletteItem msg) -> CommandPaletteModal msg -> CommandPaletteModal msg
withCommandItems groupName items palette =
    { palette
        | commandItems = OrderedDict.insert groupName items palette.commandItems
    }


withSearchResultItems : SearchResultItems msg -> CommandPaletteModal msg -> CommandPaletteModal msg
withSearchResultItems results palette =
    withSearchResultItems_ (Just results) palette


withSearchResultItems_ : Maybe (SearchResultItems msg) -> CommandPaletteModal msg -> CommandPaletteModal msg
withSearchResultItems_ results palette =
    { palette | searchResultItems = results }


withQuery : String -> CommandPaletteModal msg -> CommandPaletteModal msg
withQuery q palette =
    { palette | query = q }



-- VIEW


viewFilter : Filter msg -> Html msg
viewFilter filter =
    div [ class "command-palette_filter" ]
        [ Icon.view filter.icon
        , text filter.label
        ]


type alias ViewConfig msg =
    { updateQueryMsg : String -> msg
    , closeMsg : msg
    , keyboardShortcut : KeyboardShortcut.Model
    }


view : ViewConfig msg -> CommandPaletteModal msg -> Modal msg
view viewConfig palette =
    let
        filters =
            if not (List.isEmpty palette.filters) then
                div [ class "command-palette_filters" ]
                    (List.map viewFilter palette.filters)

            else
                UI.nothing

        view_ =
            CommandPaletteItem.view viewConfig.keyboardShortcut

        sheet =
            case palette.searchResultItems of
                Just results_ ->
                    case results_ of
                        SearchResults.Empty ->
                            UI.nothing

                        SearchResults.SearchResults matches ->
                            div [ class "command-palette_sheet" ]
                                (SearchResults.mapMatchesToList view_ matches)

                Nothing ->
                    UI.nothing

        content =
            div [ class "command-palette-modal" ]
                [ header [ class "command-palette_query" ]
                    [ filters
                    , TextField.fieldWithoutLabel viewConfig.updateQueryMsg
                        "Type a command or search definitions..."
                        palette.query
                        |> TextField.withAutofocus
                        |> TextField.view
                    ]
                , sheet
                ]
    in
    Modal.modal "command-palette-modal" viewConfig.closeMsg (Modal.content content)
