module Code2.Workspace.WorkspaceDefinitionItemCard exposing (..)

import Code.Definition.Doc as Doc
import Code.Definition.Source as Source
import Code.Definition.Term as Term
import Code.Definition.Type as Type
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.Source.SourceViewConfig as SourceViewConfig
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Code2.Workspace.DefinitionWorkspaceItemState exposing (DefinitionItemTab(..), DefinitionWorkspaceItemState)
import Code2.Workspace.WorkspaceCard as WorkspaceCard exposing (WorkspaceCard)
import Code2.Workspace.WorkspaceItem as WorkspaceItem exposing (DefinitionItem)
import Code2.Workspace.WorkspaceItemRef exposing (WorkspaceItemRef)
import Html exposing (Html, div)
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
    }


rawSource : WorkspaceItem.DefinitionItem -> Maybe String
rawSource defItem =
    case defItem of
        WorkspaceItem.TermItem detail ->
            Term.rawSource detail

        WorkspaceItem.TypeItem detail ->
            Type.rawSource detail

        _ ->
            Nothing


viewDefinitionItemSource : SyntaxConfig.SyntaxConfig msg -> WorkspaceItem.DefinitionItem -> Html msg
viewDefinitionItemSource syntaxConfig defItem =
    let
        sourceViewConfig =
            SourceViewConfig.rich syntaxConfig
    in
    case defItem of
        WorkspaceItem.TermItem (Term.Term _ _ { info, source }) ->
            Source.viewTermSource sourceViewConfig info.name source

        WorkspaceItem.TypeItem (Type.Type _ _ { source }) ->
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
            if WorkspaceItem.hasDocs cfg.item then
                case cfg.state.activeTab of
                    CodeTab ->
                        c |> WorkspaceCard.withTabList (TabList.tabList [] tabs.code [ tabs.docs ])

                    DocsTab _ ->
                        c |> WorkspaceCard.withTabList (TabList.tabList [ tabs.code ] tabs.docs [])

            else
                c

        lib =
            cfg.item
                |> WorkspaceItem.definitionItemToLib
                |> Maybe.map WorkspaceCard.viewLibraryTag
                |> Maybe.withDefault UI.nothing

        itemContent =
            case ( cfg.state.activeTab, WorkspaceItem.docs cfg.item ) of
                ( DocsTab docFoldToggles, Just docs ) ->
                    Doc.view cfg.syntaxConfig
                        cfg.toggleDocFold
                        docFoldToggles
                        docs

                _ ->
                    viewDefinitionItemSource cfg.syntaxConfig cfg.item

        {-
           showDependentsButton =
             titlebarButton cfg.showDependentsButton Icon.dependents
              |> TitlebarButton.withLeftOfTooltip (text "View direct dependents")
              |> TitlebarButton.view
        -}
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
    in
    WorkspaceCard.empty
        |> WorkspaceCard.withClassName "workspace-definition-item-card"
        |> WorkspaceCard.withTitlebarLeft
            [ lib
            , FQN.view (WorkspaceItem.definitionItemName cfg.item)
            , copySourceToClipboard
            ]
        |> WorkspaceCard.withTitlebarRight
            [ -- showDependentsButton
              Hash.view (WorkspaceItem.definitionItemHash cfg.item)
            ]
        |> WorkspaceCard.withClose cfg.closeItem
        |> WorkspaceCard.withToggleFold cfg.toggleFold
        |> WorkspaceCard.withIsFolded cfg.isFolded
        |> withTabList
        |> WorkspaceCard.withContent [ itemContent ]
