module Code2.Workspace.WorkspaceMinimap exposing (Minimap, view)

import Code.Definition.AbilityConstructor exposing (AbilityConstructor(..))
import Code.Definition.Category as Category exposing (Category)
import Code.Definition.DataConstructor exposing (DataConstructor(..))
import Code.Definition.Term exposing (Term(..))
import Code.Definition.Type as Type exposing (Type(..))
import Code.FullyQualifiedName as FQN
import Code2.Workspace.DefinitionItem as DefinitionItem exposing (DefinitionItem(..))
import Code2.Workspace.WorkspaceItem as WorkspaceItem exposing (LoadedWorkspaceItem(..), WorkspaceItem(..))
import Code2.Workspace.WorkspaceItemRef exposing (WorkspaceItemRef)
import Code2.Workspace.WorkspaceItems as WorkspaceItems exposing (WorkspaceItems, focus, mapToList)
import Html exposing (Html, div, header, small, text)
import Html.Attributes exposing (class, classList, hidden)
import Html.Events exposing (onClick)
import UI
import UI.Button as Button
import UI.Click as Click
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut(..))
import UI.KeyboardShortcut.Key as Key
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner


type alias Minimap msg =
    { keyboardShortcut : KeyboardShortcut.Model
    , workspaceItems : WorkspaceItems
    , selectItemMsg : WorkspaceItemRef -> msg
    , closeAllMsg : msg
    , closeItemMsg : WorkspaceItemRef -> msg
    , isToggled : Bool
    , toggleMinimapMsg : msg
    }


view : Minimap msg -> Html msg
view model =
    div
        [ classList [ ( "workspace-minimap", True ), ( "workspace-minimap_toggled", model.isToggled ) ] ]
        [ viewExpanded model
        , viewCollapsed model
        ]


viewCollapsed : Minimap msg -> Html msg
viewCollapsed model =
    let
        viewContent : WorkspaceItem -> List (Html msg)
        viewContent item =
            let
                focusIndex =
                    model.workspaceItems
                        |> WorkspaceItems.focusIndex
                        |> Maybe.withDefault 0
            in
            [ Button.icon model.toggleMinimapMsg Icon.unfoldedMap
                |> Button.small
                |> Button.view
            , viewItem model focusIndex ( item, True )
            ]

        content =
            model.workspaceItems
                |> focus
                |> Maybe.map viewContent
                |> Maybe.withDefault []
    in
    div [ class "workspace-minimap_collapsed" ] content


viewExpanded : Minimap msg -> Html msg
viewExpanded model =
    let
        header =
            viewHeader model.toggleMinimapMsg model.closeAllMsg

        entries =
            model.workspaceItems
                |> mapToList Tuple.pair
                |> List.indexedMap (viewItem model)
                |> div [ class "workspace-minimap_entries" ]
    in
    div
        [ class "workspace-minimap_expanded" ]
        [ header, entries ]


viewHeader : msg -> msg -> Html msg
viewHeader toggleMinimapMsg closeAllMsg =
    header
        [ class "workspace-minimap_header" ]
        [ Click.view
            [ class "workspace-minimap_header_title" ]
            [ Icon.view Icon.caretDown, text "MAP" ]
            (Click.onClick toggleMinimapMsg)
        , Click.view
            [ class "workspace-minimap_close" ]
            [ text "Close all" ]
            (Click.onClick closeAllMsg)
        ]


viewItem : Minimap msg -> Int -> ( WorkspaceItem, Bool ) -> Html msg
viewItem { selectItemMsg, closeItemMsg, keyboardShortcut } index ( item, focused ) =
    let
        wsRef =
            WorkspaceItem.reference item

        content =
            case item of
                Loading _ ->
                    div
                        [ class "workspace-minimap_item_content" ]
                        [ Placeholder.text
                            |> Placeholder.view
                        ]

                Failure _ _ ->
                    div
                        [ class "workspace-minimap_item_content" ]
                        [ StatusBanner.bad "Couldn't fetch item" ]

                Success _ (DefinitionWorkspaceItem _ _ defItem) ->
                    let
                        ( info, category ) =
                            case defItem of
                                TermItem (Term _ category_ detail) ->
                                    ( detail.info, Category.Term category_ )

                                TypeItem (Type _ category_ detail) ->
                                    ( detail.info, Category.Type category_ )

                                DataConstructorItem (DataConstructor _ detail) ->
                                    ( detail.info, Category.Type Type.DataType )

                                AbilityConstructorItem (AbilityConstructor _ detail) ->
                                    ( detail.info, Category.Type Type.AbilityType )
                    in
                    viewItemContent category info.name

                Success _ (DependentsWorkspaceItem _ _ defItem _) ->
                    div
                        [ class "workspace-minimap_item_content dependents" ]
                        [ Icon.view Icon.dependents
                        , FQN.view (DefinitionItem.name defItem)
                        , small [] [ text "dependents" ]
                        ]

                Success _ (DependenciesWorkspaceItem _ _ defItem _) ->
                    div
                        [ class "workspace-minimap_item_content dependencies" ]
                        [ Icon.view Icon.dependencies
                        , FQN.view (DefinitionItem.name defItem)
                        , small [] [ text "dependencies" ]
                        ]

                _ ->
                    UI.nothing
    in
    div
        [ classList [ ( "workspace-minimap_item", True ), ( "focused", focused ) ]
        , onClick (selectItemMsg wsRef)
        ]
        [ content
        , Button.icon (closeItemMsg wsRef) Icon.x |> Button.small |> Button.stopPropagation |> Button.subdued |> Button.view
        , div
            [ hidden True ]
            -- currently hidden as feature is not supported yet
            [ viewItemKeyboardShortcut keyboardShortcut index ]
        ]


viewItemContent : Category -> FQN.FQN -> Html msg
viewItemContent category name =
    div
        [ class "workspace-minimap_item_content" ]
        [ Icon.view (Category.icon category)
        , FQN.view name
        ]


viewItemKeyboardShortcut : KeyboardShortcut.Model -> Int -> Html msg
viewItemKeyboardShortcut model index =
    KeyboardShortcut.view model (Chord (Key.fromString "J") (Key.fromString (String.fromInt index)))
