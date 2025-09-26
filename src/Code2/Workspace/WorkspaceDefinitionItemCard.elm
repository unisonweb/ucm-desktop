module Code2.Workspace.WorkspaceDefinitionItemCard exposing (..)

import Code.Definition.Doc as Doc
import Code.Definition.Reference exposing (Reference)
import Code.Definition.Source as Source
import Code.Definition.Term as Term
import Code.Definition.Type as Type
import Code.FullyQualifiedName as FQN exposing (FQN)
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
import UI.ActionMenu as ActionMenu
import UI.Button as Button
import UI.Click as Click
import UI.CopyOnClick as CopyOnClick
import UI.Divider as Divider
import UI.FoldToggle as FoldToggle
import UI.Icon as Icon exposing (Icon)
import UI.TabList as TabList
import UI.Tag as Tag
import UI.Tooltip as Tooltip


type alias NamespaceDropdown msg =
    { toggle : msg
    , findWithinNamespace : FQN -> msg
    , changePerspective : FQN -> msg
    }


type CodeAndDocsViewMode msg
    = WithTabs { changeTab : DefinitionItemTab -> msg }
    | MixedCodeAndDocs { toggleCodeFold : msg }


type alias WorkspaceDefinitionItemCardConfig msg =
    { wsRef : WorkspaceItemRef
    , definitionRef : Reference
    , toggleDocFold : Doc.FoldId -> msg
    , closeItem : msg
    , isFolded : Bool
    , toggleFold : msg
    , state : DefinitionWorkspaceItemState
    , item : DefinitionItem
    , codeAndDocsViewMode : CodeAndDocsViewMode msg
    , syntaxConfig : SyntaxConfig.SyntaxConfig msg
    , showDependents : msg
    , withDependents : Bool
    , withDependencies : Bool
    , namespaceDropdown : Maybe (NamespaceDropdown msg)
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


viewDefinitionItemSource : WorkspaceDefinitionItemCardConfig msg -> Html msg
viewDefinitionItemSource cfg =
    let
        sourceViewConfig =
            SourceViewConfig.rich cfg.syntaxConfig

        source_ =
            case cfg.item of
                TermItem (Term.Term _ _ { info, source }) ->
                    let
                        fullSource =
                            Source.viewTermSource sourceViewConfig info.name source
                    in
                    case cfg.codeAndDocsViewMode of
                        MixedCodeAndDocs { toggleCodeFold } ->
                            let
                                viewFoldableSource renderedSource =
                                    div [ class "foldable-source" ]
                                        [ FoldToggle.view foldToggle, renderedSource ]

                                foldToggle =
                                    FoldToggle.foldToggle toggleCodeFold
                                        |> FoldToggle.isOpen (not cfg.state.isCodeFolded)

                                signature =
                                    Source.viewNamedTermSignature sourceViewConfig info.name (Term.termSignature source)

                                foldedOrUnfoldedSource =
                                    if cfg.state.isCodeFolded then
                                        signature

                                    else
                                        fullSource
                            in
                            if DefinitionItem.isBuiltin cfg.item || not (DefinitionItem.hasDocs cfg.item) then
                                fullSource

                            else
                                viewFoldableSource foldedOrUnfoldedSource

                        _ ->
                            fullSource

                TypeItem (Type.Type _ _ { source }) ->
                    Source.viewTypeSource sourceViewConfig source

                _ ->
                    UI.nothing
    in
    div [ class "definition-source" ] [ source_ ]


definitionItemTabs : (DefinitionItemTab -> msg) -> { code : TabList.Tab msg, docs : TabList.Tab msg }
definitionItemTabs changeTab =
    { code =
        TabList.tab "Code"
            (Click.onClick (changeTab CodeTab))
    , docs =
        TabList.tab "Docs"
            (Click.onClick (changeTab DocsTab))
    }


categoryIcon : DefinitionItem -> Icon msg
categoryIcon item =
    case item of
        TermItem (Term.Term _ cat _) ->
            case cat of
                Term.PlainTerm ->
                    Icon.term

                Term.DocTerm ->
                    Icon.doc

                Term.TestTerm ->
                    Icon.test

        TypeItem (Type.Type _ cat _) ->
            case cat of
                Type.DataType ->
                    Icon.type_

                Type.AbilityType ->
                    Icon.ability

        AbilityConstructorItem _ ->
            Icon.abilityConstructor

        DataConstructorItem _ ->
            Icon.dataConstructor


viewItemContent : WorkspaceDefinitionItemCardConfig msg -> Html msg
viewItemContent cfg =
    case cfg.codeAndDocsViewMode of
        WithTabs _ ->
            case ( cfg.state.activeTab, DefinitionItem.docs cfg.item ) of
                ( DocsTab, Just docs ) ->
                    Doc.view cfg.syntaxConfig
                        cfg.toggleDocFold
                        cfg.state.docFoldToggles
                        docs

                _ ->
                    viewDefinitionItemSource cfg

        MixedCodeAndDocs _ ->
            case DefinitionItem.docs cfg.item of
                Just docs ->
                    let
                        sourceAndDivider =
                            if not (DefinitionItem.isBuiltin cfg.item) || DefinitionItem.isTerm cfg.item then
                                [ viewDefinitionItemSource cfg
                                , Divider.viewSimple
                                ]

                            else
                                []
                    in
                    div [ class "mixed-docs-and-code" ]
                        (sourceAndDivider
                            ++ [ Doc.view cfg.syntaxConfig
                                    cfg.toggleDocFold
                                    cfg.state.docFoldToggles
                                    docs
                               ]
                        )

                _ ->
                    viewDefinitionItemSource cfg


viewNamespaceDropdown : WorkspaceDefinitionItemCardConfig msg -> Html msg
viewNamespaceDropdown cfg =
    case ( cfg.namespaceDropdown, DefinitionItem.namespace cfg.item ) of
        ( Just dropdown, Just fqn ) ->
            let
                ns =
                    FQN.toString fqn

                label =
                    case DefinitionItem.toLibFqn fqn of
                        Just libFqn ->
                            FQN.toString (FQN.stripPrefix libFqn fqn)

                        _ ->
                            ns
            in
            -- When stripping lib prefix resulting an empty FQN we don't
            -- want to render a dropdown
            if label == "." then
                UI.nothing

            else
                div [ class "namespace-dropdown" ]
                    [ ActionMenu.items
                        (ActionMenu.optionItem
                            Icon.browse
                            ("Find within " ++ ns)
                            (Click.onClick (dropdown.findWithinNamespace fqn))
                        )
                        [ ActionMenu.optionItem
                            Icon.intoFolder
                            ("Change perspective to " ++ ns)
                            (Click.onClick (dropdown.changePerspective fqn))
                        ]
                        |> ActionMenu.fromButton dropdown.toggle label
                        |> ActionMenu.extendingRight
                        |> ActionMenu.withButtonColor Button.Outlined
                        |> ActionMenu.shouldBeOpen cfg.state.namespaceDropdownIsOpen
                        |> ActionMenu.view
                    ]

        _ ->
            UI.nothing


titlebarLeft : WorkspaceDefinitionItemCardConfig msg -> List (Html msg)
titlebarLeft cfg =
    let
        lib =
            cfg.item
                |> DefinitionItem.toLib
                |> Maybe.map WorkspaceCard.viewLibraryTag
                |> Maybe.withDefault UI.nothing

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

        builtin =
            if DefinitionItem.isBuiltin cfg.item then
                Tooltip.tooltip
                    (Tooltip.text
                        (FQN.toString (DefinitionItem.name cfg.item) ++ " is a built-in definition provided by the Unison runtime.")
                    )
                    |> Tooltip.below
                    |> Tooltip.withArrow Tooltip.Start
                    |> Tooltip.view
                        (Tag.tag "Built-in"
                            |> Tag.withIcon Icon.unisonMark
                            |> Tag.view
                        )

            else
                UI.nothing
    in
    [ div [ class "category-icon" ] [ Icon.view (categoryIcon cfg.item) ]
    , lib
    , viewNamespaceDropdown cfg
    , FQN.view (DefinitionItem.name cfg.item)
    , builtin
    , copySourceToClipboard
    ]


titlebarRight : WorkspaceDefinitionItemCardConfig msg -> List (Html msg)
titlebarRight cfg =
    let
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

        dependentsButton =
            -- Feature flag dependents (which aren't ready in UCM yet, but exist in Share)
            if cfg.withDependents && not (DefinitionItem.isBuiltin cfg.item) then
                titlebarButton cfg.showDependents Icon.dependents
                    |> TitlebarButton.withLeftOfTooltip (text "View direct dependents")
                    |> TitlebarButton.view

            else
                UI.nothing

        otherNames_ =
            DefinitionItem.otherNames cfg.item

        otherNames =
            if not (List.isEmpty otherNames_) then
                let
                    viewOtherName n =
                        div [ class "other-name" ]
                            [ Icon.view Icon.boldDot
                            , div [ class "fully-qualified-name" ] [ FQN.view n ]
                            ]

                    otherNamesTooltipContent =
                        Tooltip.rich
                            (div [ class "workspace-definition-item-card_other-names_list" ]
                                (div [ class "aka" ] [ text "Also known as" ] :: List.map viewOtherName otherNames_)
                            )
                in
                div [ class "workspace-definition-item-card_other-names" ]
                    [ Tooltip.tooltip otherNamesTooltipContent
                        |> Tooltip.below
                        |> Tooltip.withArrow Tooltip.End
                        |> Tooltip.view (div [ class "workspace-definition-item-card_other-names_button" ] [ Icon.view Icon.tags ])
                    ]

            else
                UI.nothing
    in
    [ defHash
    , otherNames
    , dependentsButton
    ]


view : WorkspaceDefinitionItemCardConfig msg -> WorkspaceCard msg
view cfg =
    let
        withTabList c =
            case cfg.codeAndDocsViewMode of
                WithTabs { changeTab } ->
                    let
                        tabs =
                            definitionItemTabs changeTab
                    in
                    if DefinitionItem.hasDocs cfg.item then
                        case cfg.state.activeTab of
                            CodeTab ->
                                c |> WorkspaceCard.withTabList (TabList.tabList [] tabs.code [ tabs.docs ])

                            DocsTab ->
                                c |> WorkspaceCard.withTabList (TabList.tabList [ tabs.code ] tabs.docs [])

                    else
                        c

                _ ->
                    c
    in
    WorkspaceCard.empty
        |> WorkspaceCard.withClassName "workspace-definition-item-card"
        |> WorkspaceCard.withTitlebarLeft (titlebarLeft cfg)
        |> WorkspaceCard.withTitlebarRight (titlebarRight cfg)
        |> WorkspaceCard.withClose cfg.closeItem
        |> WorkspaceCard.withToggleFold cfg.toggleFold
        |> WorkspaceCard.withIsFolded cfg.isFolded
        |> withTabList
        |> WorkspaceCard.withContent [ viewItemContent cfg ]
