module Code2.Workspace.WorkspaceDefinitionItemCard exposing (..)

import Code.Definition.AbilityConstructor as AbilityConstructor
import Code.Definition.DataConstructor as DataConstructor
import Code.Definition.Doc as Doc
import Code.Definition.Source as Source
import Code.Definition.Term as Term
import Code.Definition.Type as Type
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.ProjectDependency as ProjectDependency exposing (ProjectDependency)
import Code.Source.SourceViewConfig as SourceViewConfig
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Code2.Workspace.DefinitionWorkspaceItemState exposing (DefinitionItemTab(..), DefinitionWorkspaceItemState)
import Code2.Workspace.WorkspaceCard as WorkspaceCard exposing (WorkspaceCard)
import Code2.Workspace.WorkspaceItem as WorkspaceItem exposing (DefinitionItem(..), LoadedWorkspaceItem(..))
import Code2.Workspace.WorkspaceItemRef exposing (WorkspaceItemRef(..))
import Html exposing (Html, strong, text)
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import UI
import UI.Button as Button
import UI.Click as Click
import UI.ContextualTag as ContextualTag
import UI.Icon as Icon
import UI.KeyboardShortcut exposing (KeyboardShortcut(..))
import UI.KeyboardShortcut.Key exposing (Key(..))
import UI.TabList as TabList


type alias WorkspaceDefinitionItemCardConfig msg =
    { wsRef : WorkspaceItemRef
    , toggleDocFold : Doc.FoldId -> msg
    , closeItem : msg
    , state : DefinitionWorkspaceItemState
    , item : DefinitionItem
    , changeTab : DefinitionItemTab -> msg
    , syntaxConfig : SyntaxConfig.SyntaxConfig msg
    }


definitionItemName : WorkspaceItem.DefinitionItem -> FQN
definitionItemName defItem =
    case defItem of
        WorkspaceItem.TermItem (Term.Term _ _ { info }) ->
            info.name

        WorkspaceItem.TypeItem (Type.Type _ _ { info }) ->
            info.name

        WorkspaceItem.AbilityConstructorItem (AbilityConstructor.AbilityConstructor _ { info }) ->
            info.name

        WorkspaceItem.DataConstructorItem (DataConstructor.DataConstructor _ { info }) ->
            info.name


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


viewLibraryTag : ProjectDependency -> Html msg
viewLibraryTag dep =
    ContextualTag.contextualTag Icon.book (ProjectDependency.toString dep)
        |> ContextualTag.decorativePurple
        |> ContextualTag.withTooltipText "Library dependency"
        |> ContextualTag.view


definitionItemTabs : (DefinitionItemTab -> msg) -> { code : TabList.Tab msg, docs : TabList.Tab msg }
definitionItemTabs changeTab =
    { code =
        TabList.tab "Code"
            (Click.onClick (changeTab CodeTab))
    , docs =
        TabList.tab "Docs"
            (Click.onClick (changeTab (DocsTab Doc.emptyDocFoldToggles)))
    }


hasDocs : DefinitionItem -> Bool
hasDocs defItem =
    MaybeE.isJust (WorkspaceItem.docs defItem)


definitionItemToLib : WorkspaceItem.DefinitionItem -> Maybe ProjectDependency
definitionItemToLib defItem =
    let
        fqnToLib fqn =
            case fqn |> FQN.segments |> NEL.toList of
                "lib" :: _ :: "lib" :: _ ->
                    Nothing

                "lib" :: libName :: _ ->
                    Just (ProjectDependency.fromString libName)

                _ ->
                    Nothing

        toLib info =
            case info.namespace of
                Just n ->
                    fqnToLib n

                Nothing ->
                    let
                        f n acc =
                            if MaybeE.isJust acc then
                                acc

                            else
                                fqnToLib n
                    in
                    List.foldl f Nothing info.otherNames
    in
    case defItem of
        WorkspaceItem.TermItem (Term.Term _ _ { info }) ->
            toLib info

        WorkspaceItem.TypeItem (Type.Type _ _ { info }) ->
            toLib info

        WorkspaceItem.AbilityConstructorItem (AbilityConstructor.AbilityConstructor _ { info }) ->
            toLib info

        WorkspaceItem.DataConstructorItem (DataConstructor.DataConstructor _ { info }) ->
            toLib info


view : WorkspaceDefinitionItemCardConfig msg -> WorkspaceCard msg
view { state, item, toggleDocFold, syntaxConfig, changeTab, closeItem } =
    let
        tabs =
            definitionItemTabs changeTab

        withTabList c =
            if hasDocs item then
                case state.activeTab of
                    CodeTab ->
                        c |> WorkspaceCard.withTabList (TabList.tabList [] tabs.code [ tabs.docs ])

                    DocsTab _ ->
                        c |> WorkspaceCard.withTabList (TabList.tabList [ tabs.code ] tabs.docs [])

            else
                c

        lib =
            item
                |> definitionItemToLib
                |> Maybe.map viewLibraryTag
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
    in
    WorkspaceCard.empty
        |> WorkspaceCard.withTitlebarLeft
            [ lib
            , strong []
                [ text (FQN.toString (definitionItemName item))
                ]
            ]
        |> WorkspaceCard.withTitlebarRight
            [ Button.icon closeItem Icon.x
                |> Button.subdued
                |> Button.small
                |> Button.view
            ]
        |> withTabList
        |> WorkspaceCard.withContent [ itemContent ]
