module Code2.Workspace.WorkspaceDefinitionItemCard exposing (..)

import Code.Definition.Doc as Doc
import Code.Definition.Source as Source
import Code.Definition.Term as Term
import Code.Definition.Type as Type
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.Source.SourceViewConfig as SourceViewConfig
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Code2.Workspace.DefinitionItem as DefinitionItem exposing (DefinitionItem(..))
import Code2.Workspace.DefinitionWorkspaceItemState exposing (DefinitionItemTab(..), DefinitionWorkspaceItemState)
import Code2.Workspace.WorkspaceCard as WorkspaceCard exposing (WorkspaceCard)
import Code2.Workspace.WorkspaceCardTitlebarButton as TitlebarButton exposing (titlebarButton)
import Code2.Workspace.WorkspaceItemRef exposing (WorkspaceItemRef)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import UI
import UI.Click as Click
import UI.CopyOnClick as CopyOnClick
import UI.Icon as Icon
import UI.TabList as TabList
import UI.Tooltip as Tooltip


type alias WorkspaceDefinitionItemCardConfig msg =
    { wsRef : WorkspaceItemRef
    , toggleDocFold : Doc.FoldId -> msg
    , closeItem : msg
    , isFolded : Bool
    , toggleFold : msg
    , state : DefinitionWorkspaceItemState
    , item : DefinitionItem
    , changeTab : DefinitionItemTab -> msg
    , syntaxConfig : SyntaxConfig.SyntaxConfig msg
    , showDependents : msg
    , withDependents : Bool
    , withDependencies : Bool
    }


rawSource : DefinitionItem -> Maybe String
rawSource defItem =
    case defItem of
        TermItem detail ->
            Term.rawSource detail

        TypeItem detail ->
            Type.rawSource detail

        _ ->
            Nothing


viewDefinitionItemSource : SyntaxConfig.SyntaxConfig msg -> DefinitionItem -> Html msg
viewDefinitionItemSource syntaxConfig defItem =
    let
        sourceViewConfig =
            SourceViewConfig.rich syntaxConfig
    in
    case defItem of
        TermItem (Term.Term _ _ { info, source }) ->
            Source.viewTermSource sourceViewConfig info.name source

        TypeItem (Type.Type _ _ { source }) ->
            Source.viewTypeSource sourceViewConfig source

        _ ->
            UI.nothing


definitionItemTabs : (DefinitionItemTab -> msg) -> { code : TabList.Tab msg, docs : TabList.Tab msg }
definitionItemTabs changeTab =
    { code =
        TabList.tab "Code"
            (Click.onClick (changeTab CodeTab))
    , docs =
        TabList.tab "Docs"
            (Click.onClick (changeTab (DocsTab Doc.emptyDocFoldToggles)))
    }


view : WorkspaceDefinitionItemCardConfig msg -> WorkspaceCard msg
view cfg =
    let
        tabs =
            definitionItemTabs cfg.changeTab

        withTabList c =
            if DefinitionItem.hasDocs cfg.item then
                case cfg.state.activeTab of
                    CodeTab ->
                        c |> WorkspaceCard.withTabList (TabList.tabList [] tabs.code [ tabs.docs ])

                    DocsTab _ ->
                        c |> WorkspaceCard.withTabList (TabList.tabList [ tabs.code ] tabs.docs [])

            else
                c

        lib =
            cfg.item
                |> DefinitionItem.toLib
                |> Maybe.map WorkspaceCard.viewLibraryTag
                |> Maybe.withDefault UI.nothing

        itemContent =
            case ( cfg.state.activeTab, DefinitionItem.docs cfg.item ) of
                ( DocsTab docFoldToggles, Just docs ) ->
                    Doc.view cfg.syntaxConfig
                        cfg.toggleDocFold
                        docFoldToggles
                        docs

                _ ->
                    viewDefinitionItemSource cfg.syntaxConfig cfg.item

        dependentsButton =
            -- Feature flag dependents (which aren't ready in UCM yet, but exist in Share)
            if cfg.withDependents then
                titlebarButton cfg.showDependents Icon.dependents
                    |> TitlebarButton.withLeftOfTooltip (text "View direct dependents")
                    |> TitlebarButton.view

            else
                UI.nothing

        copySourceToClipboard =
            case rawSource cfg.item of
                Just source ->
                    div [ class "copy-code" ]
                        [ Tooltip.tooltip (Tooltip.text "Copy source")
                            |> Tooltip.below
                            |> Tooltip.withArrow Tooltip.Start
                            |> Tooltip.view
                                (CopyOnClick.view source
                                    (div [ class "button small subdued content-icon" ]
                                        [ Icon.view Icon.clipboard ]
                                    )
                                    (Icon.view Icon.checkmark)
                                )
                        ]

                Nothing ->
                    UI.nothing

        defHash =
            div [ class "definition-hash" ]
                [ Tooltip.tooltip (Tooltip.text "Copy full definition hash")
                    |> Tooltip.below
                    |> Tooltip.withArrow Tooltip.End
                    |> Tooltip.view
                        (CopyOnClick.view (Hash.toUnprefixedString (DefinitionItem.hash cfg.item))
                            (Hash.view (DefinitionItem.hash cfg.item))
                            (Icon.view Icon.checkmark)
                        )
                ]
    in
    WorkspaceCard.empty
        |> WorkspaceCard.withClassName "workspace-definition-item-card"
        |> WorkspaceCard.withTitlebarLeft
            [ lib
            , FQN.view (DefinitionItem.name cfg.item)
            , copySourceToClipboard
            ]
        |> WorkspaceCard.withTitlebarRight
            [ defHash
            , dependentsButton
            ]
        |> WorkspaceCard.withClose cfg.closeItem
        |> WorkspaceCard.withToggleFold cfg.toggleFold
        |> WorkspaceCard.withIsFolded cfg.isFolded
        |> withTabList
        |> WorkspaceCard.withContent [ itemContent ]
