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
import Code2.Workspace.DefinitionWorkspaceItemState exposing (DefinitionItemTab(..))
import Code2.Workspace.WorkspaceCard as WorkspaceCard
import Code2.Workspace.WorkspaceDefinitionItemCard as WorkspaceDefinitionItemCard
import Code2.Workspace.WorkspaceDependentsItemCard as WorkspaceDependentsItemCard
import Code2.Workspace.WorkspaceItem as WorkspaceItem exposing (WorkspaceItem)
import Code2.Workspace.WorkspaceItemRef as WorkspaceItemRef exposing (WorkspaceItemRef(..))
import Code2.Workspace.WorkspaceItems as WorkspaceItems exposing (WorkspaceItems)
import Html exposing (Html, div, p, strong, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Lib.HttpApi as HttpApi exposing (ApiRequest, HttpResult)
import Lib.OperatingSystem exposing (OperatingSystem)
import Lib.ScrollTo as ScrollTo
import Lib.Util as Util
import Set exposing (Set)
import Set.Extra as SetE
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
    }


init : OperatingSystem -> ( Model, Cmd Msg )
init os =
    ( { workspaceItems = WorkspaceItems.init Nothing
      , definitionSummaryTooltip = DefinitionSummaryTooltip.init
      , collapsedItems = Set.empty
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
    | OpenDefinition Reference
    | ShowDependentsOf { defRef : WorkspaceItemRef, defItem : DefinitionItem }
    | FetchDependentsFinished { depRef : WorkspaceItemRef, defItem : DefinitionItem } (HttpResult (List DefinitionMatch))
    | ToggleDocFold WorkspaceItemRef Doc.FoldId
    | ToggleFold WorkspaceItemRef
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

                        DependentsItemRef _ ->
                            ( model, Cmd.none )
            in
            ( model_, cmd, NoOut )

        FetchDefinitionItemFinished dRef (Ok defItem) ->
            let
                workspaceItemRef =
                    WorkspaceItemRef.DefinitionItemRef dRef

                activeTab =
                    if DefinitionItem.isDoc defItem then
                        DocsTab Doc.emptyDocFoldToggles

                    else
                        CodeTab

                workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        workspaceItemRef
                        (WorkspaceItem.Success workspaceItemRef
                            (WorkspaceItem.DefinitionWorkspaceItem
                                { activeTab = activeTab }
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

        CloseWorkspaceItem ref ->
            ( { model
                | workspaceItems =
                    WorkspaceItems.remove model.workspaceItems ref
              }
            , Cmd.none
            , NoOut
            )

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
                        (\_ -> { activeTab = newTab })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }, Cmd.none, NoOut )

        OpenDefinition r ->
            openDefinition config paneId model r

        ShowDependentsOf { defRef, defItem } ->
            case defRef of
                WorkspaceItemRef.DefinitionItemRef r ->
                    openDependents config paneId model r defItem

                _ ->
                    ( model, Cmd.none, NoOut )

        FetchDependentsFinished { depRef, defItem } (Ok dependents) ->
            let
                workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        depRef
                        (WorkspaceItem.Success
                            depRef
                            (WorkspaceItem.DependentsWorkspaceItem defItem dependents)
                        )
            in
            ( { model | workspaceItems = workspaceItems }, Cmd.none, NoOut )

        FetchDependentsFinished { depRef } (Err e) ->
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


openDependents : Config -> String -> Model -> Reference -> DefinitionItem -> ( Model, Cmd Msg, OutMsg )
openDependents config paneId ({ workspaceItems } as model) dependentsOfRef dependentsOfItem =
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
            [ HttpApi.perform config.api
                (fetchDependents config depRef dependentsOfRef dependentsOfItem)
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
            (DefinitionItem.decode ref)
            (FetchDefinitionItemFinished ref)


fetchDependents : Config -> WorkspaceItemRef -> Reference -> DefinitionItem -> ApiRequest (List DefinitionMatch) Msg
fetchDependents config depRef defRef defItem =
    let
        endpoint =
            CodebaseApi.Dependents { ref = defRef }
    in
    endpoint
        |> config.toApiEndpoint
        |> HttpApi.toRequest
            DefinitionMatch.decodeList
            (FetchDependentsFinished { depRef = depRef, defItem = defItem })


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
    , withDependents : Bool
    , withDependencies : Bool
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

                WorkspaceItem.Success wsRef (WorkspaceItem.DefinitionWorkspaceItem state defItem) ->
                    let
                        config =
                            { wsRef = wsRef
                            , state = state
                            , item = defItem
                            , syntaxConfig = syntaxConfig definitionSummaryTooltip
                            , closeItem = CloseWorkspaceItem wsRef
                            , changeTab = ChangeDefinitionItemTab wsRef
                            , toggleDocFold = ToggleDocFold wsRef
                            , isFolded =
                                Set.member
                                    (WorkspaceItemRef.toString wsRef)
                                    collapsedItems
                            , toggleFold = ToggleFold wsRef
                            , withDependents = cfg.withDependents
                            , withDependencies = cfg.withDependencies
                            , showDependents = ShowDependentsOf { defRef = wsRef, defItem = defItem }
                            }
                    in
                    WorkspaceDefinitionItemCard.view config

                WorkspaceItem.Success wsRef (WorkspaceItem.DependentsWorkspaceItem defItem dependents) ->
                    let
                        config =
                            { wsRef = wsRef
                            , item = defItem
                            , dependents = dependents
                            , syntaxConfig = syntaxConfig definitionSummaryTooltip
                            , closeItem = CloseWorkspaceItem wsRef
                            , openDefinition = OpenDefinition
                            }
                    in
                    WorkspaceDependentsItemCard.view config

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
                                    [ text (Util.httpErrorToString e)
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
    div
        [ onClick PaneFocus
        , class "workspace-pane"
        , id cfg.paneId
        , classList [ ( "workspace-pane_focused", cfg.isFocused ) ]
        ]
        (model.workspaceItems
            |> WorkspaceItems.mapToList (viewItem cfg model.collapsedItems model.definitionSummaryTooltip)
        )
