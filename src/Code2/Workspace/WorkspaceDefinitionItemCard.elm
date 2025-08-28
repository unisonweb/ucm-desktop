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
view { state, item, toggleDocFold, syntaxConfig, changeTab, closeItem } =
    let
        tabs =
            definitionItemTabs changeTab

        withTabList c =
            if WorkspaceItem.hasDocs item then
                case state.activeTab of
                    CodeTab ->
                        c |> WorkspaceCard.withTabList (TabList.tabList [] tabs.code [ tabs.docs ])

                    DocsTab _ ->
                        c |> WorkspaceCard.withTabList (TabList.tabList [ tabs.code ] tabs.docs [])

            else
                c

        lib =
            item
                |> WorkspaceItem.definitionItemToLib
                |> Maybe.map WorkspaceCard.viewLibraryTag
                |> Maybe.withDefault UI.nothing

        itemContent =
            case ( state.activeTab, WorkspaceItem.docs item ) of
                ( DocsTab docFoldToggles, Just docs ) ->
                    Doc.view syntaxConfig
                        toggleDocFold
                        docFoldToggles
                        docs

                _ ->
                    viewDefinitionItemSource syntaxConfig item

        {-
           showDependentsButton =
               Button.icon showDependents Icon.dependents
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
                [ text (FQN.toString (WorkspaceItem.definitionItemName item))
                ]
            ]
        |> WorkspaceCard.withTitlebarRight
            [-- showDependentsButton
            ]
        |> WorkspaceCard.withClose closeItem
        |> withTabList
        |> WorkspaceCard.withContent [ itemContent ]
