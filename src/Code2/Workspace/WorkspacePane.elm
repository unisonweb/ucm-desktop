module Code2.Workspace.WorkspacePane exposing (..)

import Code.CodebaseApi as CodebaseApi
import Code.Config exposing (Config)
import Code.Definition.Doc as Doc
import Code.Definition.Reference exposing (Reference)
import Code.DefinitionSummaryTooltip as DefinitionSummaryTooltip
import Code.FullyQualifiedName exposing (FQN)
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Code2.Workspace.DefinitionItem as DefinitionItem exposing (DefinitionItem)
import Code2.Workspace.DefinitionMatch as DefinitionMatch exposing (DefinitionMatch)
import Code2.Workspace.DefinitionMatchesState as DefinitionMatchesState exposing (DefinitionMatchesCardTab)
import Code2.Workspace.DefinitionWorkspaceItemState as DefinitionWorkspaceItemState exposing (DefinitionItemTab)
import Code2.Workspace.WorkspaceCard as WorkspaceCard
import Code2.Workspace.WorkspaceDefinitionItemCard as WorkspaceDefinitionItemCard
import Code2.Workspace.WorkspaceDependenciesItemCard as WorkspaceDependenciesItemCard
import Code2.Workspace.WorkspaceDependentsItemCard as WorkspaceDependentsItemCard
import Code2.Workspace.WorkspaceItem as WorkspaceItem exposing (WorkspaceItem)
import Code2.Workspace.WorkspaceItemRef as WorkspaceItemRef exposing (WorkspaceItemRef(..))
import Code2.Workspace.WorkspaceItems as WorkspaceItems exposing (WorkspaceItems)
import Code2.Workspace.WorkspaceMinimap as WorkspaceMinimap
import Html exposing (Html, div, p, pre, strong, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (ApiRequest, HttpResult)
import Lib.OperatingSystem exposing (OperatingSystem)
import Lib.ScrollTo as ScrollTo
import Lib.Util as Util
import Maybe.Extra as MaybeE
import Set exposing (Set)
import Set.Extra as SetE
import Task
import UI
import UI.Button as Button
import UI.Click as Click
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut(..))
import UI.KeyboardShortcut.Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent
import UI.Placeholder as Placeholder
import UI.StatusIndicator as StatusIndicator



-- MODEL


type alias Model =
    { workspaceItems : WorkspaceItems
    , definitionSummaryTooltip : DefinitionSummaryTooltip.Model
    , collapsedItems : Set String -- Serialized WorkspaceItemRef
    , keyboardShortcut : KeyboardShortcut.Model
    , isMinimapToggled : Bool
    }


init : OperatingSystem -> ( Model, Cmd Msg )
init os =
    ( { workspaceItems = WorkspaceItems.init Nothing
      , definitionSummaryTooltip = DefinitionSummaryTooltip.init
      , collapsedItems = Set.empty
      , keyboardShortcut = KeyboardShortcut.init os
      , isMinimapToggled = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | PaneFocus
    | FetchDefinitionItemFinished Reference (HttpResult DefinitionItem)
    | Refetch WorkspaceItemRef
    | CloseAll
    | CloseWorkspaceItem WorkspaceItemRef
    | ToggleMinimap
    | ChangeDefinitionItemTab WorkspaceItemRef DefinitionItemTab
    | ToggleCodeFold WorkspaceItemRef
    | ChangeDependentsItemTab WorkspaceItemRef DefinitionMatchesCardTab
    | ShowDependentsOf WorkspaceItemRef
    | UpdateDependentsSearchQuery WorkspaceItemRef String
    | FetchDependentsFinished WorkspaceItemRef Reference (HttpResult ( DefinitionItem, List DefinitionMatch ))
    | ChangeDependenciesItemTab WorkspaceItemRef DefinitionMatchesCardTab
    | ShowDependenciesOf WorkspaceItemRef
    | UpdateDependenciesSearchQuery WorkspaceItemRef String
    | FetchDependenciesFinished WorkspaceItemRef Reference (HttpResult ( DefinitionItem, List DefinitionMatch ))
    | FindWithinNamespace WorkspaceItemRef FQN
    | ChangePerspective WorkspaceItemRef Reference FQN
    | OpenDefinition Reference
    | ToggleDocFold WorkspaceItemRef Doc.FoldId
    | ToggleNamespaceDropdown WorkspaceItemRef
    | ToggleFold WorkspaceItemRef
    | Keydown KeyboardEvent.KeyboardEvent
    | SetFocusedItem WorkspaceItemRef
    | DefinitionSummaryTooltipMsg DefinitionSummaryTooltip.Msg
    | KeyboardShortcutMsg KeyboardShortcut.Msg


type OutMsg
    = NoOut
    | RequestPaneFocus
    | FocusOn WorkspaceItemRef
    | RequestFindInNamespace FQN
    | RequestChangePerspective Reference FQN
    | Emptied


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

                        DependentsItemRef _ ->
                            ( model, Cmd.none )

                        DependenciesItemRef _ ->
                            ( model, Cmd.none )
            in
            ( model_, cmd, NoOut )

        FetchDefinitionItemFinished dRef (Ok defItem) ->
            let
                workspaceItemRef =
                    WorkspaceItemRef.DefinitionItemRef dRef

                activeTab =
                    if DefinitionItem.isDoc defItem then
                        DefinitionWorkspaceItemState.DocsTab

                    else
                        DefinitionWorkspaceItemState.CodeTab

                workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        workspaceItemRef
                        (WorkspaceItem.Success workspaceItemRef
                            (WorkspaceItem.DefinitionWorkspaceItem
                                dRef
                                (DefinitionWorkspaceItemState.init activeTab)
                                defItem
                            )
                        )
            in
            ( { model | workspaceItems = workspaceItems }, Cmd.none, NoOut )

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

        CloseAll ->
            ( { model | workspaceItems = WorkspaceItems.empty }
            , Cmd.none
            , Emptied
            )

        CloseWorkspaceItem ref ->
            let
                workspaceItems =
                    WorkspaceItems.remove model.workspaceItems ref

                out =
                    workspaceItems
                        |> WorkspaceItems.focusedReference
                        |> MaybeE.unwrap Emptied FocusOn
            in
            ( { model | workspaceItems = workspaceItems }, Cmd.none, out )

        ToggleMinimap ->
            ( { model | isMinimapToggled = not model.isMinimapToggled }, Cmd.none, NoOut )

        ToggleFold ref ->
            ( { model
                | collapsedItems =
                    SetE.toggle
                        (WorkspaceItemRef.toString ref)
                        model.collapsedItems
              }
            , Cmd.none
            , NoOut
            )

        ChangeDefinitionItemTab wsRef newTab ->
            let
                workspaceItems_ =
                    WorkspaceItems.updateDefinitionItemState
                        (\s -> { s | activeTab = newTab })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }, Cmd.none, NoOut )

        ToggleCodeFold wsRef ->
            let
                workspaceItems_ =
                    WorkspaceItems.updateDefinitionItemState
                        (\s -> { s | isCodeFolded = not s.isCodeFolded })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }, Cmd.none, NoOut )

        ChangeDependentsItemTab wsRef newTab ->
            let
                workspaceItems_ =
                    WorkspaceItems.updateDependentsItemState
                        (\s -> { s | activeTab = newTab })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }, Cmd.none, NoOut )

        ShowDependentsOf defRef ->
            case defRef of
                WorkspaceItemRef.DefinitionItemRef r ->
                    openDependents config paneId model r

                _ ->
                    ( model, Cmd.none, NoOut )

        FetchDependentsFinished depRef defRef (Ok ( defItem, dependents )) ->
            let
                workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        depRef
                        (WorkspaceItem.Success
                            depRef
                            (WorkspaceItem.DependentsWorkspaceItem
                                defRef
                                (DefinitionMatchesState.init DefinitionMatchesState.TermsTab)
                                defItem
                                dependents
                            )
                        )
            in
            ( { model | workspaceItems = workspaceItems }, Cmd.none, NoOut )

        FetchDependentsFinished depRef _ (Err e) ->
            ( { model
                | workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        depRef
                        (WorkspaceItem.Failure depRef e)
              }
            , Cmd.none
            , NoOut
            )

        ChangeDependenciesItemTab wsRef newTab ->
            let
                workspaceItems_ =
                    WorkspaceItems.updateDependenciesItemState
                        (\s -> { s | activeTab = newTab })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }, Cmd.none, NoOut )

        ShowDependenciesOf defRef ->
            case defRef of
                WorkspaceItemRef.DefinitionItemRef r ->
                    openDependencies config paneId model r

                _ ->
                    ( model, Cmd.none, NoOut )

        FetchDependenciesFinished depRef defRef (Ok ( defItem, dependents )) ->
            let
                workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        depRef
                        (WorkspaceItem.Success
                            depRef
                            (WorkspaceItem.DependenciesWorkspaceItem
                                defRef
                                (DefinitionMatchesState.init DefinitionMatchesState.TermsTab)
                                defItem
                                dependents
                            )
                        )
            in
            ( { model | workspaceItems = workspaceItems }, Cmd.none, NoOut )

        FetchDependenciesFinished depRef _ (Err e) ->
            ( { model
                | workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        depRef
                        (WorkspaceItem.Failure depRef e)
              }
            , Cmd.none
            , NoOut
            )

        OpenDefinition r ->
            openDefinition config paneId model r

        ToggleDocFold wsRef foldId ->
            let
                updateState state =
                    { state | docFoldToggles = Doc.toggleFold state.docFoldToggles foldId }

                workspaceItems_ =
                    WorkspaceItems.updateDefinitionItemState
                        updateState
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }, Cmd.none, NoOut )

        ToggleNamespaceDropdown wsRef ->
            let
                updateState state =
                    { state
                        | namespaceDropdownIsOpen =
                            not state.namespaceDropdownIsOpen
                    }

                workspaceItems_ =
                    WorkspaceItems.updateDefinitionItemState
                        updateState
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }, Cmd.none, NoOut )

        SetFocusedItem wsRef ->
            ( { model | workspaceItems = WorkspaceItems.focusOn model.workspaceItems wsRef }
            , scrollToItem paneId wsRef
            , FocusOn wsRef
            )

        FindWithinNamespace wsRef fqn ->
            let
                workspaceItems_ =
                    WorkspaceItems.updateDefinitionItemState
                        (\s -> { s | namespaceDropdownIsOpen = False })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }
            , Cmd.none
            , RequestFindInNamespace fqn
            )

        ChangePerspective wsRef ref fqn ->
            let
                workspaceItems_ =
                    WorkspaceItems.updateDefinitionItemState
                        (\s -> { s | namespaceDropdownIsOpen = False })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }
            , Cmd.none
            , RequestChangePerspective ref fqn
            )

        UpdateDependentsSearchQuery wsRef query ->
            let
                workspaceItems_ =
                    WorkspaceItems.updateDependentsItemState
                        (\s -> { s | searchQuery = query })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }
            , Cmd.none
            , NoOut
            )

        UpdateDependenciesSearchQuery wsRef query ->
            let
                workspaceItems_ =
                    WorkspaceItems.updateDependenciesItemState
                        (\s -> { s | searchQuery = query })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }
            , Cmd.none
            , NoOut
            )

        Keydown event ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.collect model.keyboardShortcut event.key

                shortcut =
                    KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut event

                ( nextModel, cmd, out ) =
                    handleKeyboardShortcut paneId
                        { model | keyboardShortcut = keyboardShortcut }
                        shortcut
            in
            ( nextModel, Cmd.batch [ cmd, Cmd.map KeyboardShortcutMsg kCmd ], out )

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
openDefinition config paneId ({ workspaceItems } as model) ref =
    let
        wsRef =
            WorkspaceItemRef.DefinitionItemRef ref
    in
    -- openItem config paneId model (Just relativeToRef) ref
    -- We don't want to refetch or replace any already open definitions, but we
    -- do want to focus and scroll to it (unless its already currently focused)
    if WorkspaceItems.includesItem workspaceItems wsRef then
        focusOpenedItem paneId model wsRef

    else
        let
            toInsert =
                WorkspaceItem.Loading wsRef

            nextWorkspaceItems =
                case workspaceItems |> WorkspaceItems.focus |> Maybe.map WorkspaceItem.reference of
                    Nothing ->
                        WorkspaceItems.prependWithFocus workspaceItems toInsert

                    Just r ->
                        WorkspaceItems.insertWithFocusBefore workspaceItems r toInsert
        in
        ( { model | workspaceItems = nextWorkspaceItems }
        , Cmd.batch [ HttpApi.perform config.api (fetchDefinition config ref), scrollToItem paneId wsRef ]
        , FocusOn wsRef
        )


focusOpenedItem : String -> Model -> WorkspaceItemRef -> ( Model, Cmd Msg, OutMsg )
focusOpenedItem paneId ({ workspaceItems } as model) wsRef =
    if not (WorkspaceItems.isFocused workspaceItems wsRef) then
        let
            nextWorkspaceItems =
                WorkspaceItems.focusOn workspaceItems wsRef
        in
        ( { model | workspaceItems = nextWorkspaceItems }
        , scrollToItem paneId wsRef
        , FocusOn wsRef
        )

    else
        ( model, Cmd.none, NoOut )


openDependents : Config -> String -> Model -> Reference -> ( Model, Cmd Msg, OutMsg )
openDependents config paneId ({ workspaceItems } as model) dependentsOfRef =
    let
        depRef =
            WorkspaceItemRef.DependentsItemRef dependentsOfRef
    in
    if WorkspaceItems.includesItem workspaceItems depRef then
        focusOpenedItem paneId model depRef

    else
        let
            nextWorkspaceItems =
                WorkspaceItems.insertWithFocusBefore
                    workspaceItems
                    (WorkspaceItemRef.DefinitionItemRef dependentsOfRef)
                    (WorkspaceItem.Loading depRef)
        in
        ( { model | workspaceItems = nextWorkspaceItems }
        , Cmd.batch
            [ fetchDependents config depRef dependentsOfRef
            , scrollToItem paneId depRef
            ]
        , FocusOn depRef
        )


openDependencies : Config -> String -> Model -> Reference -> ( Model, Cmd Msg, OutMsg )
openDependencies config paneId ({ workspaceItems } as model) dependenciesOfRef =
    let
        depRef =
            WorkspaceItemRef.DependenciesItemRef dependenciesOfRef
    in
    if WorkspaceItems.includesItem workspaceItems depRef then
        focusOpenedItem paneId model depRef

    else
        let
            nextWorkspaceItems =
                WorkspaceItems.insertWithFocusBefore
                    workspaceItems
                    (WorkspaceItemRef.DefinitionItemRef dependenciesOfRef)
                    (WorkspaceItem.Loading depRef)
        in
        ( { model | workspaceItems = nextWorkspaceItems }
        , Cmd.batch
            [ fetchDependencies config depRef dependenciesOfRef
            , scrollToItem paneId depRef
            ]
        , FocusOn depRef
        )


currentlyOpenReferences : Model -> List Reference
currentlyOpenReferences model =
    WorkspaceItems.definitionReferences model.workspaceItems


currentlyOpenFqns : Model -> List FQN
currentlyOpenFqns model =
    WorkspaceItems.fqns model.workspaceItems


handleKeyboardShortcut : String -> Model -> KeyboardShortcut -> ( Model, Cmd Msg, OutMsg )
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

                out =
                    next
                        |> WorkspaceItems.focusedReference
                        |> Maybe.map FocusOn
                        |> Maybe.withDefault NoOut
            in
            ( { model | workspaceItems = next }, scrollToCmd next, out )

        prevDefinition =
            let
                prev =
                    WorkspaceItems.prev model.workspaceItems

                out =
                    prev
                        |> WorkspaceItems.focusedReference
                        |> Maybe.map FocusOn
                        |> Maybe.withDefault NoOut
            in
            ( { model | workspaceItems = prev }, scrollToCmd prev, out )

        moveDown =
            let
                next =
                    WorkspaceItems.moveDown model.workspaceItems
            in
            ( { model | workspaceItems = next }, scrollToCmd next, NoOut )

        moveUp =
            let
                next =
                    WorkspaceItems.moveUp model.workspaceItems
            in
            ( { model | workspaceItems = next }, scrollToCmd next, NoOut )
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
            , Emptied
            )

        Sequence _ ArrowDown ->
            nextDefinition

        Sequence _ (J _) ->
            nextDefinition

        Sequence _ ArrowUp ->
            prevDefinition

        Sequence _ (K _) ->
            prevDefinition

        Sequence _ (X _) ->
            let
                without =
                    model.workspaceItems
                        |> WorkspaceItems.focus
                        |> Maybe.map (WorkspaceItem.reference >> WorkspaceItems.remove model.workspaceItems)
                        |> Maybe.withDefault model.workspaceItems

                out =
                    without
                        |> WorkspaceItems.focusedReference
                        |> Maybe.map FocusOn
                        |> Maybe.withDefault Emptied
            in
            ( { model | workspaceItems = without }
            , Cmd.none
            , out
            )

        Sequence _ (Z _) ->
            let
                collapsedItems =
                    model.workspaceItems
                        |> WorkspaceItems.focus
                        |> Maybe.map WorkspaceItem.reference
                        |> Maybe.map WorkspaceItemRef.toString
                        |> Maybe.map (\r -> SetE.toggle r model.collapsedItems)
                        |> Maybe.withDefault model.collapsedItems
            in
            ( { model | collapsedItems = collapsedItems }
            , Cmd.none
            , NoOut
            )

        Chord Shift (Z _) ->
            let
                collapsedItems =
                    if Set.isEmpty model.collapsedItems then
                        model.workspaceItems
                            |> WorkspaceItems.toList
                            |> List.map WorkspaceItem.reference
                            |> List.map WorkspaceItemRef.toString
                            |> Set.fromList

                    else
                        Set.empty
            in
            ( { model | collapsedItems = collapsedItems }
            , Cmd.none
            , NoOut
            )

        _ ->
            ( model, Cmd.none, NoOut )



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
            (DefinitionItem.decode ref)
            (FetchDefinitionItemFinished ref)


fetchDependents : Config -> WorkspaceItemRef -> Reference -> Cmd Msg
fetchDependents config depRef defRef =
    let
        deps =
            CodebaseApi.Dependents { ref = defRef }
                |> config.toApiEndpoint
                |> HttpApi.toTask config.api.url
                    (Decode.field "results" DefinitionMatch.decodeList)

        def =
            CodebaseApi.Definition
                { perspective = config.perspective
                , ref = defRef
                }
                |> config.toApiEndpoint
                |> HttpApi.toTask config.api.url
                    (DefinitionItem.decode defRef)

        t =
            Task.map2 Tuple.pair def deps
    in
    Task.attempt (FetchDependentsFinished depRef defRef) t


fetchDependencies : Config -> WorkspaceItemRef -> Reference -> Cmd Msg
fetchDependencies config depRef defRef =
    let
        deps =
            CodebaseApi.Dependencies { ref = defRef }
                |> config.toApiEndpoint
                |> HttpApi.toTask config.api.url
                    (Decode.field "results" DefinitionMatch.decodeList)

        def =
            CodebaseApi.Definition
                { perspective = config.perspective
                , ref = defRef
                }
                |> config.toApiEndpoint
                |> HttpApi.toTask config.api.url
                    (DefinitionItem.decode defRef)

        t =
            Task.map2 Tuple.pair def deps
    in
    Task.attempt (FetchDependenciesFinished depRef defRef) t


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


type alias PaneConfig =
    { paneId : String
    , operatingSystem : OperatingSystem
    , isFocused : Bool
    , withFocusedPaneIndicator : Bool
    , withNamespaceDropdown : Bool
    , withMinimap : Bool
    }


syntaxConfig : DefinitionSummaryTooltip.Model -> SyntaxConfig.SyntaxConfig Msg
syntaxConfig definitionSummaryTooltip =
    SyntaxConfig.default
        (OpenDefinition >> Click.onClick)
        (DefinitionSummaryTooltip.tooltipConfig
            DefinitionSummaryTooltipMsg
            definitionSummaryTooltip
        )
        |> SyntaxConfig.withSyntaxHelp


viewItem : PaneConfig -> Set String -> DefinitionSummaryTooltip.Model -> WorkspaceItem -> Bool -> Html Msg
viewItem cfg collapsedItems definitionSummaryTooltip item isFocused =
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

                WorkspaceItem.Success wsRef (WorkspaceItem.DefinitionWorkspaceItem definitionRef state defItem) ->
                    let
                        namespaceDropdown =
                            if cfg.withNamespaceDropdown then
                                Just
                                    { toggle = ToggleNamespaceDropdown wsRef
                                    , findWithinNamespace = FindWithinNamespace wsRef
                                    , changePerspective = ChangePerspective wsRef definitionRef
                                    }

                            else
                                Nothing

                        config =
                            { wsRef = wsRef
                            , definitionRef = definitionRef
                            , state = state
                            , item = defItem
                            , syntaxConfig = syntaxConfig definitionSummaryTooltip
                            , closeItem = CloseWorkspaceItem wsRef
                            , codeAndDocsViewMode =
                                WorkspaceDefinitionItemCard.MixedCodeAndDocs { toggleCodeFold = ToggleCodeFold wsRef }
                            , toggleDocFold = ToggleDocFold wsRef
                            , isFolded =
                                Set.member
                                    (WorkspaceItemRef.toString wsRef)
                                    collapsedItems
                            , toggleFold = ToggleFold wsRef
                            , showDependents = ShowDependentsOf wsRef
                            , showDependencies = ShowDependenciesOf wsRef
                            , namespaceDropdown = namespaceDropdown
                            }
                    in
                    WorkspaceDefinitionItemCard.view config

                WorkspaceItem.Success wsRef (WorkspaceItem.DependentsWorkspaceItem dependentsOfRef state defItem dependents) ->
                    let
                        config =
                            { wsRef = wsRef
                            , dependentsOfRef = dependentsOfRef
                            , item = defItem
                            , state = state
                            , updateQuery = UpdateDependentsSearchQuery wsRef
                            , changeTab = ChangeDependentsItemTab wsRef
                            , dependents = dependents
                            , closeItem = CloseWorkspaceItem wsRef
                            , openDefinition = OpenDefinition
                            }
                    in
                    WorkspaceDependentsItemCard.view config

                WorkspaceItem.Success wsRef (WorkspaceItem.DependenciesWorkspaceItem dependenciesOfRef state defItem dependencies) ->
                    let
                        config =
                            { wsRef = wsRef
                            , dependenciesOfRef = dependenciesOfRef
                            , item = defItem
                            , state = state
                            , updateQuery = UpdateDependenciesSearchQuery wsRef
                            , changeTab = ChangeDependenciesItemTab wsRef
                            , dependencies = dependencies
                            , closeItem = CloseWorkspaceItem wsRef
                            , openDefinition = OpenDefinition
                            }
                    in
                    WorkspaceDependenciesItemCard.view config

                WorkspaceItem.Success _ _ ->
                    {- TODO -}
                    cardBase
                        |> WorkspaceCard.withContent []

                WorkspaceItem.Failure wsRef e ->
                    let
                        ( title, subTitle ) =
                            case wsRef of
                                WorkspaceItemRef.DefinitionItemRef _ ->
                                    ( WorkspaceItemRef.toHumanString wsRef
                                    , "failed to load definition"
                                    )

                                WorkspaceItemRef.DependentsItemRef _ ->
                                    ( WorkspaceItemRef.toHumanString wsRef
                                    , "failed to load dependents"
                                    )

                                _ ->
                                    ( WorkspaceItemRef.toHumanString wsRef
                                    , "failed to load"
                                    )
                    in
                    cardBase
                        |> WorkspaceCard.withTitlebarLeft
                            [ StatusIndicator.bad |> StatusIndicator.view
                            , strong [] [ text title ]
                            , strong [ class "subdued" ] [ text subTitle ]
                            ]
                        |> WorkspaceCard.withTitlebarRight
                            [ Button.icon (CloseWorkspaceItem wsRef) Icon.x
                                |> Button.subdued
                                |> Button.small
                                |> Button.view
                            ]
                        |> WorkspaceCard.withContent
                            [ div [ class "workspace-card_error" ]
                                [ p [ class "error" ]
                                    [ pre [] [ text (Util.httpErrorToString e) ]
                                    ]
                                , Button.iconThenLabel (Refetch wsRef) Icon.refresh "Try again"
                                    |> Button.small
                                    |> Button.view
                                ]
                            ]
    in
    card
        |> WorkspaceCard.withFocus isFocused
        |> WorkspaceCard.withDomId domId
        |> WorkspaceCard.withClick
            (Click.onClick (SetFocusedItem (WorkspaceItem.reference item)))
        |> WorkspaceCard.view cfg.operatingSystem


view : PaneConfig -> Model -> Html Msg
view cfg model =
    let
        minimap =
            if cfg.withMinimap && WorkspaceItems.length model.workspaceItems > 1 then
                WorkspaceMinimap.view
                    { keyboardShortcut = model.keyboardShortcut
                    , workspaceItems = model.workspaceItems
                    , selectItemMsg = SetFocusedItem
                    , closeAllMsg = CloseAll
                    , closeItemMsg = CloseWorkspaceItem
                    , isToggled = model.isMinimapToggled
                    , toggleMinimapMsg = ToggleMinimap
                    }

            else
                UI.nothing
    in
    div
        [ onClick PaneFocus
        , class "workspace-pane"
        , id cfg.paneId
        , classList
            [ ( "workspace-pane_focused", cfg.isFocused )
            , ( "workspace-pane_focused-pane-indicator", cfg.withFocusedPaneIndicator )
            ]
        ]
        (minimap
            :: (model.workspaceItems
                    |> WorkspaceItems.mapToList (viewItem cfg model.collapsedItems model.definitionSummaryTooltip)
               )
        )
