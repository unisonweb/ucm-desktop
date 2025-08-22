module Code2.Workspace.WorkspacePane exposing (..)

import Code.CodebaseApi as CodebaseApi
import Code.Config exposing (Config)
import Code.Definition.AbilityConstructor as AbilityConstructor
import Code.Definition.DataConstructor as DataConstructor
import Code.Definition.Doc as Doc
import Code.Definition.Reference exposing (Reference)
import Code.Definition.Source as Source
import Code.Definition.Term as Term
import Code.Definition.Type as Type
import Code.DefinitionSummaryTooltip as DefinitionSummaryTooltip
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.ProjectDependency as ProjectDependency exposing (ProjectDependency)
import Code.Source.SourceViewConfig as SourceViewConfig
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Code2.Workspace.DefinitionWorkspaceItemState exposing (DefinitionItemTab(..))
import Code2.Workspace.WorkspaceCard as WorkspaceCard
import Code2.Workspace.WorkspaceItem as WorkspaceItem exposing (DefinitionItem(..), LoadedWorkspaceItem(..), WorkspaceItem)
import Code2.Workspace.WorkspaceItemRef as WorkspaceItemRef exposing (WorkspaceItemRef(..))
import Code2.Workspace.WorkspaceItems as WorkspaceItems exposing (WorkspaceItems)
import Html exposing (Html, div, span, strong, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Lib.HttpApi as HttpApi exposing (ApiRequest, HttpResult)
import Lib.OperatingSystem exposing (OperatingSystem)
import Lib.ScrollTo as ScrollTo
import Lib.Util as Util
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import UI
import UI.Button as Button
import UI.Click as Click
import UI.ContextualTag as ContextualTag
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut(..))
import UI.KeyboardShortcut.Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent
import UI.Placeholder as Placeholder
import UI.TabList as TabList



-- MODEL


type alias Model =
    { workspaceItems : WorkspaceItems
    , definitionSummaryTooltip : DefinitionSummaryTooltip.Model
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : OperatingSystem -> ( Model, Cmd Msg )
init os =
    ( { workspaceItems = WorkspaceItems.init Nothing
      , definitionSummaryTooltip = DefinitionSummaryTooltip.init
      , keyboardShortcut = KeyboardShortcut.init os
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | PaneFocus
    | FetchDefinitionItemFinished Reference (HttpResult DefinitionItem)
    | Refetch WorkspaceItemRef
    | CloseWorkspaceItem WorkspaceItemRef
    | ChangeDefinitionItemTab WorkspaceItemRef DefinitionItemTab
    | OpenDependency Reference
    | ToggleDocFold WorkspaceItemRef Doc.FoldId
    | Keydown KeyboardEvent.KeyboardEvent
    | SetFocusedItem WorkspaceItemRef
    | DefinitionSummaryTooltipMsg DefinitionSummaryTooltip.Msg
    | KeyboardShortcutMsg KeyboardShortcut.Msg


type OutMsg
    = NoOut
    | RequestPaneFocus
    | FocusOn WorkspaceItemRef


update : Config -> String -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update config paneId msg model =
    case msg of
        PaneFocus ->
            ( model, Cmd.none, RequestPaneFocus )

        Refetch ref ->
            let
                ( model_, cmd ) =
                    case ref of
                        SearchResultsItemRef _ ->
                            ( model, Cmd.none )

                        DefinitionItemRef dRef ->
                            let
                                nextWorkspaceItems =
                                    WorkspaceItems.replace model.workspaceItems ref (WorkspaceItem.Loading ref)
                            in
                            ( { model | workspaceItems = nextWorkspaceItems }
                            , HttpApi.perform config.api (fetchDefinition config dRef)
                            )
            in
            ( model_, cmd, NoOut )

        FetchDefinitionItemFinished dRef (Ok defItem) ->
            let
                workspaceItemRef =
                    WorkspaceItemRef.DefinitionItemRef dRef

                activeTab =
                    if WorkspaceItem.isDoc defItem then
                        DocsTab Doc.emptyDocFoldToggles

                    else
                        CodeTab
            in
            ( { model
                | workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        workspaceItemRef
                        (WorkspaceItem.Success workspaceItemRef
                            (WorkspaceItem.DefinitionWorkspaceItem
                                { activeTab = activeTab }
                                defItem
                            )
                        )
              }
            , Cmd.none
            , NoOut
            )

        FetchDefinitionItemFinished dRef (Err e) ->
            let
                workspaceItemRef =
                    WorkspaceItemRef.DefinitionItemRef dRef
            in
            ( { model
                | workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        workspaceItemRef
                        (WorkspaceItem.Failure workspaceItemRef e)
              }
            , Cmd.none
            , NoOut
            )

        CloseWorkspaceItem ref ->
            ( { model
                | workspaceItems =
                    WorkspaceItems.remove model.workspaceItems ref
              }
            , Cmd.none
            , NoOut
            )

        ChangeDefinitionItemTab wsRef newTab ->
            let
                workspaceItems_ =
                    WorkspaceItems.updateDefinitionItemState
                        (\_ -> { activeTab = newTab })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }, Cmd.none, NoOut )

        OpenDependency r ->
            let
                ( m, c, out ) =
                    case WorkspaceItems.focus model.workspaceItems of
                        Just item ->
                            openReference config
                                paneId
                                model
                                (WorkspaceItem.reference item)
                                (WorkspaceItemRef.DefinitionItemRef r)

                        Nothing ->
                            openDefinition config paneId model r
            in
            ( m, c, out )

        ToggleDocFold wsRef foldId ->
            let
                updateState state =
                    case state.activeTab of
                        DocsTab toggles ->
                            { activeTab =
                                DocsTab (Doc.toggleFold toggles foldId)
                            }

                        _ ->
                            state

                workspaceItems_ =
                    WorkspaceItems.updateDefinitionItemState
                        updateState
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }, Cmd.none, NoOut )

        SetFocusedItem wsRef ->
            ( { model | workspaceItems = WorkspaceItems.focusOn model.workspaceItems wsRef }
            , Cmd.none
            , FocusOn wsRef
            )

        Keydown event ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.collect model.keyboardShortcut event.key

                shortcut =
                    KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut event

                ( nextModel, cmd ) =
                    handleKeyboardShortcut paneId
                        { model | keyboardShortcut = keyboardShortcut }
                        shortcut
            in
            ( nextModel, Cmd.batch [ cmd, Cmd.map KeyboardShortcutMsg kCmd ], NoOut )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd, NoOut )

        DefinitionSummaryTooltipMsg tMsg ->
            let
                ( definitionSummaryTooltip, tCmd ) =
                    DefinitionSummaryTooltip.update config tMsg model.definitionSummaryTooltip
            in
            ( { model | definitionSummaryTooltip = definitionSummaryTooltip }
            , Cmd.map DefinitionSummaryTooltipMsg tCmd
            , NoOut
            )

        _ ->
            ( model, Cmd.none, NoOut )



-- HELPERS


openDefinition : Config -> String -> Model -> Reference -> ( Model, Cmd Msg, OutMsg )
openDefinition config paneId model ref =
    openItem config paneId model Nothing (DefinitionItemRef ref)


open : Config -> String -> Model -> WorkspaceItemRef -> ( Model, Cmd Msg, OutMsg )
open config paneId model ref =
    openItem config paneId model Nothing ref


openReference : Config -> String -> Model -> WorkspaceItemRef -> WorkspaceItemRef -> ( Model, Cmd Msg, OutMsg )
openReference config paneId model relativeToRef ref =
    openItem config paneId model (Just relativeToRef) ref


openItem : Config -> String -> Model -> Maybe WorkspaceItemRef -> WorkspaceItemRef -> ( Model, Cmd Msg, OutMsg )
openItem config paneId ({ workspaceItems } as model) relativeToRef ref =
    case ref of
        SearchResultsItemRef _ ->
            ( model, Cmd.none, FocusOn ref )

        DefinitionItemRef dRef ->
            -- We don't want to refetch or replace any already open definitions, but we
            -- do want to focus and scroll to it (unless its already currently focused)
            if WorkspaceItems.includesItem workspaceItems ref then
                if not (WorkspaceItems.isFocused workspaceItems ref) then
                    let
                        nextWorkspaceItems =
                            WorkspaceItems.focusOn workspaceItems ref
                    in
                    ( { model | workspaceItems = nextWorkspaceItems }
                    , scrollToItem paneId ref
                    , FocusOn ref
                    )

                else
                    ( model, Cmd.none, NoOut )

            else
                let
                    toInsert =
                        WorkspaceItem.Loading ref

                    nextWorkspaceItems =
                        case relativeToRef of
                            Nothing ->
                                WorkspaceItems.prependWithFocus workspaceItems toInsert

                            Just r ->
                                WorkspaceItems.insertWithFocusBefore workspaceItems r toInsert
                in
                ( { model | workspaceItems = nextWorkspaceItems }
                , Cmd.batch [ HttpApi.perform config.api (fetchDefinition config dRef), scrollToItem paneId ref ]
                , FocusOn ref
                )


currentlyOpenReferences : Model -> List Reference
currentlyOpenReferences model =
    WorkspaceItems.definitionReferences model.workspaceItems


currentlyOpenFqns : Model -> List FQN
currentlyOpenFqns model =
    WorkspaceItems.fqns model.workspaceItems


handleKeyboardShortcut : String -> Model -> KeyboardShortcut -> ( Model, Cmd Msg )
handleKeyboardShortcut paneId model shortcut =
    let
        scrollToCmd =
            WorkspaceItems.focus
                >> Maybe.map WorkspaceItem.reference
                >> Maybe.map (scrollToItem paneId)
                >> Maybe.withDefault Cmd.none

        nextDefinition =
            let
                next =
                    WorkspaceItems.next model.workspaceItems
            in
            ( { model | workspaceItems = next }, scrollToCmd next )

        prevDefinitions =
            let
                prev =
                    WorkspaceItems.prev model.workspaceItems
            in
            ( { model | workspaceItems = prev }, scrollToCmd prev )

        moveDown =
            let
                next =
                    WorkspaceItems.moveDown model.workspaceItems
            in
            ( { model | workspaceItems = next }, scrollToCmd next )

        moveUp =
            let
                next =
                    WorkspaceItems.moveUp model.workspaceItems
            in
            ( { model | workspaceItems = next }, scrollToCmd next )
    in
    case shortcut of
        Chord Shift ArrowDown ->
            moveDown

        Chord Shift ArrowUp ->
            moveUp

        Chord Shift (J _) ->
            moveDown

        Chord Shift (K _) ->
            moveUp

        Chord Shift (X _) ->
            ( { model | workspaceItems = WorkspaceItems.empty }
            , Cmd.none
            )

        Sequence _ ArrowDown ->
            nextDefinition

        Sequence _ (J _) ->
            nextDefinition

        Sequence _ ArrowUp ->
            prevDefinitions

        Sequence _ (K _) ->
            prevDefinitions

        Sequence _ (X _) ->
            let
                without =
                    model.workspaceItems
                        |> WorkspaceItems.focus
                        |> Maybe.map (WorkspaceItem.reference >> WorkspaceItems.remove model.workspaceItems)
                        |> Maybe.withDefault model.workspaceItems
            in
            ( { model | workspaceItems = without }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- EFFECTS


fetchDefinition : Config -> Reference -> ApiRequest DefinitionItem Msg
fetchDefinition config ref =
    let
        endpoint =
            CodebaseApi.Definition
                { perspective = config.perspective
                , ref = ref
                }
    in
    endpoint
        |> config.toApiEndpoint
        |> HttpApi.toRequest
            (WorkspaceItem.decodeDefinitionItem ref)
            (FetchDefinitionItemFinished ref)


scrollToItem : String -> WorkspaceItemRef -> Cmd Msg
scrollToItem paneId ref =
    let
        targetId =
            "workspace-card_" ++ WorkspaceItemRef.toDomString ref
    in
    -- Annoying magic number, but this is 0.75rem AKA 12px for scroll margin
    -- `scroll-margin-top` does not work with Elm's way of setting the viewport
    ScrollTo.scrollTo_ NoOp paneId targetId 12



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown



-- VIEW


syntaxConfig : DefinitionSummaryTooltip.Model -> SyntaxConfig.SyntaxConfig Msg
syntaxConfig definitionSummaryTooltip =
    SyntaxConfig.default
        (OpenDependency >> Click.onClick)
        (DefinitionSummaryTooltip.tooltipConfig
            DefinitionSummaryTooltipMsg
            definitionSummaryTooltip
        )
        |> SyntaxConfig.withSyntaxHelp


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


viewDefinitionItemSource : DefinitionSummaryTooltip.Model -> WorkspaceItem.DefinitionItem -> Html Msg
viewDefinitionItemSource definitionSummaryTooltip defItem =
    let
        sourceViewConfig =
            SourceViewConfig.rich (syntaxConfig definitionSummaryTooltip)
    in
    case defItem of
        WorkspaceItem.TermItem (Term.Term _ _ { info, source }) ->
            Source.viewTermSource sourceViewConfig info.name source

        WorkspaceItem.TypeItem (Type.Type _ _ { source }) ->
            Source.viewTypeSource sourceViewConfig source

        _ ->
            UI.nothing


hasDocs : WorkspaceItem.DefinitionItem -> Bool
hasDocs defItem =
    MaybeE.isJust (WorkspaceItem.docs defItem)


definitionItemTabs : WorkspaceItemRef -> { code : TabList.Tab Msg, docs : TabList.Tab Msg }
definitionItemTabs wsRef =
    { code =
        TabList.tab "Code"
            (Click.onClick (ChangeDefinitionItemTab wsRef CodeTab))
    , docs =
        TabList.tab "Docs"
            (Click.onClick (ChangeDefinitionItemTab wsRef (DocsTab Doc.emptyDocFoldToggles)))
    }


viewLibraryTag : ProjectDependency -> Html msg
viewLibraryTag dep =
    ContextualTag.contextualTag Icon.book (ProjectDependency.toString dep)
        |> ContextualTag.decorativePurple
        |> ContextualTag.withTooltipText "Library dependency"
        |> ContextualTag.view


viewItem : DefinitionSummaryTooltip.Model -> WorkspaceItem -> Bool -> Html Msg
viewItem definitionSummaryTooltip item isFocused =
    let
        cardBase =
            WorkspaceCard.empty

        domId =
            "workspace-card_" ++ (item |> WorkspaceItem.reference |> WorkspaceItemRef.toDomString)

        card =
            case item of
                WorkspaceItem.Loading _ ->
                    cardBase
                        |> WorkspaceCard.withTitlebarLeft
                            [ Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
                            ]
                        |> WorkspaceCard.withContent
                            [ div [ class "workspace-card_loading" ]
                                [ Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
                                , Placeholder.text |> Placeholder.withLength Placeholder.Huge |> Placeholder.view
                                , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.view
                                , Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
                                , Placeholder.text |> Placeholder.withLength Placeholder.Small |> Placeholder.view
                                ]
                            ]

                WorkspaceItem.Success wsRef (WorkspaceItem.DefinitionWorkspaceItem state defItem) ->
                    let
                        tabs =
                            definitionItemTabs wsRef

                        withTabList c =
                            if hasDocs defItem then
                                case state.activeTab of
                                    CodeTab ->
                                        c |> WorkspaceCard.withTabList (TabList.tabList [] tabs.code [ tabs.docs ])

                                    DocsTab _ ->
                                        c |> WorkspaceCard.withTabList (TabList.tabList [ tabs.code ] tabs.docs [])

                            else
                                c

                        lib =
                            defItem
                                |> definitionItemToLib
                                |> Maybe.map viewLibraryTag
                                |> Maybe.withDefault UI.nothing

                        itemContent =
                            case ( state.activeTab, WorkspaceItem.docs defItem ) of
                                ( DocsTab docFoldToggles, Just docs ) ->
                                    Doc.view (syntaxConfig definitionSummaryTooltip)
                                        (ToggleDocFold wsRef)
                                        docFoldToggles
                                        docs

                                _ ->
                                    viewDefinitionItemSource definitionSummaryTooltip defItem
                    in
                    cardBase
                        |> WorkspaceCard.withTitlebarLeft [ lib, strong [] [ text (FQN.toString (definitionItemName defItem)) ] ]
                        |> WorkspaceCard.withTitlebarRight
                            [ Button.icon (CloseWorkspaceItem wsRef) Icon.x
                                |> Button.subdued
                                |> Button.small
                                |> Button.view
                            ]
                        |> withTabList
                        |> WorkspaceCard.withContent [ itemContent ]

                WorkspaceItem.Success _ _ ->
                    {- TODO -}
                    cardBase
                        |> WorkspaceCard.withContent []

                WorkspaceItem.Failure wsRef e ->
                    cardBase
                        |> WorkspaceCard.withTitle ("Failed to load definition: " ++ WorkspaceItemRef.toHumanString wsRef)
                        |> WorkspaceCard.withTitlebarRight
                            [ Button.icon (CloseWorkspaceItem wsRef) Icon.x
                                |> Button.subdued
                                |> Button.small
                                |> Button.view
                            ]
                        |> WorkspaceCard.withContent
                            [ div [ class "workspace-card_error" ]
                                [ span [ class "error" ]
                                    [ span [ class "error_icon" ] [ Icon.view Icon.warn ]
                                    , text (Util.httpErrorToString e)
                                    ]
                                , Button.iconThenLabel (Refetch wsRef) Icon.refresh "Try again"
                                    |> Button.view
                                ]
                            ]
    in
    card
        |> WorkspaceCard.withFocus isFocused
        |> WorkspaceCard.withDomId domId
        |> WorkspaceCard.withClick
            (Click.onClick (SetFocusedItem (WorkspaceItem.reference item)))
        |> WorkspaceCard.view


view : String -> Bool -> Model -> Html Msg
view paneId isFocused model =
    div
        [ onClick PaneFocus
        , class "workspace-pane"
        , id paneId
        , classList [ ( "workspace-pane_focused", isFocused ) ]
        ]
        (model.workspaceItems
            |> WorkspaceItems.mapToList (viewItem model.definitionSummaryTooltip)
        )
