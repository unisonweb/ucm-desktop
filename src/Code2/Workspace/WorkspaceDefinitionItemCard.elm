module Code2.Workspace.WorkspaceDefinitionItemCard exposing (..)

import Code.Definition.Doc as Doc
import Code.Definition.Source as Source
import Code.Definition.Term as Term
import Code.Definition.Type as Type
import Code.FullyQualifiedName as FQN
import Code.Source.SourceViewConfig as SourceViewConfig
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Code2.Workspace.DefinitionWorkspaceItemState exposing (DefinitionItemTab(..), DefinitionWorkspaceItemState)
import Code2.Workspace.WorkspaceCard as WorkspaceCard exposing (WorkspaceCard)
import Code2.Workspace.WorkspaceItem as WorkspaceItem exposing (DefinitionItem)
import Code2.Workspace.WorkspaceItemRef exposing (WorkspaceItemRef)
import Html exposing (Html, strong, text)
import UI
import UI.Click as Click
import UI.TabList as TabList


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
               Button.icon cfg.showDependents Icon.dependents
                   |> Button.stopPropagation
                   |> Button.subdued
                   |> Button.small
                   |> Button.view
        -}
    in
    WorkspaceCard.empty
        |> WorkspaceCard.withTitlebarLeft
            [ lib
            , strong []
                [ text (FQN.toString (WorkspaceItem.definitionItemName cfg.item))
                ]
            ]
        |> WorkspaceCard.withTitlebarRight
            [-- showDependentsButton
            ]
        |> WorkspaceCard.withClose cfg.closeItem
        |> WorkspaceCard.withToggleFold cfg.toggleFold
        |> WorkspaceCard.withIsFolded cfg.isFolded
        |> withTabList
        |> WorkspaceCard.withContent [ itemContent ]
