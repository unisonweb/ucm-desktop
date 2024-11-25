module Ucm.Workspace.WorkspacePane exposing (..)

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
import Html exposing (Html, div, span, strong, text)
import Html.Attributes exposing (class, classList)
import Lib.HttpApi as HttpApi exposing (ApiRequest, HttpResult)
import Lib.ScrollTo as ScrollTo
import Lib.Util as Util
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import UI
import UI.Button as Button
import UI.Click as Click
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut(..))
import UI.KeyboardShortcut.Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent
import UI.TabList as TabList
import Ucm.AppContext exposing (AppContext)
import Ucm.ContextualTag as ContextualTag
import Ucm.Workspace.WorkspaceCard as WorkspaceCard
import Ucm.Workspace.WorkspaceContext exposing (WorkspaceContext)
import Ucm.Workspace.WorkspaceItem as WorkspaceItem exposing (DefinitionItem(..), LoadedWorkspaceItem(..), WorkspaceItem)
import Ucm.Workspace.WorkspaceItemRef as WorkspaceItemRef exposing (WorkspaceItemRef(..))
import Ucm.Workspace.WorkspaceItems as WorkspaceItems exposing (WorkspaceItems)



-- MODEL


type alias Model =
    { workspaceItems : WorkspaceItems
    , definitionSummaryTooltip : DefinitionSummaryTooltip.Model
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : AppContext -> WorkspaceContext -> ( Model, Cmd Msg )
init appContext _ =
    ( { workspaceItems = WorkspaceItems.init Nothing
      , definitionSummaryTooltip = DefinitionSummaryTooltip.init
      , keyboardShortcut = KeyboardShortcut.init appContext.operatingSystem
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | FetchDefinitionItemFinished Reference (HttpResult DefinitionItem)
    | CloseWorkspaceItem WorkspaceItemRef
    | ChangeDefinitionItemTab WorkspaceItemRef WorkspaceItem.DefinitionItemTab
    | OpenDependency Reference
    | Keydown KeyboardEvent.KeyboardEvent
    | DefinitionSummaryTooltipMsg DefinitionSummaryTooltip.Msg
    | KeyboardShortcutMsg KeyboardShortcut.Msg


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        FetchDefinitionItemFinished dRef (Ok defItem) ->
            let
                workspaceItemRef =
                    WorkspaceItemRef.DefinitionItemRef dRef

                activeTab =
                    if WorkspaceItem.isDoc defItem then
                        WorkspaceItem.DocsTab Doc.emptyDocFoldToggles

                    else
                        WorkspaceItem.CodeTab
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
            )

        CloseWorkspaceItem ref ->
            ( { model
                | workspaceItems =
                    WorkspaceItems.remove model.workspaceItems ref
              }
            , Cmd.none
            )

        ChangeDefinitionItemTab wsRef newTab ->
            let
                workspaceItems_ =
                    WorkspaceItems.updateDefinitionItemState
                        (\_ -> { activeTab = newTab })
                        wsRef
                        model.workspaceItems
            in
            ( { model | workspaceItems = workspaceItems_ }, Cmd.none )

        OpenDependency r ->
            case WorkspaceItems.focus model.workspaceItems of
                Just item ->
                    openReference config
                        model
                        (WorkspaceItem.reference item)
                        (WorkspaceItemRef.DefinitionItemRef r)

                Nothing ->
                    openDefinition config model r

        Keydown event ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.collect model.keyboardShortcut event.key

                shortcut =
                    KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut event

                ( nextModel, cmd ) =
                    handleKeyboardShortcut
                        { model | keyboardShortcut = keyboardShortcut }
                        shortcut
            in
            ( nextModel, Cmd.batch [ cmd, Cmd.map KeyboardShortcutMsg kCmd ] )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd )

        DefinitionSummaryTooltipMsg tMsg ->
            let
                ( definitionSummaryTooltip, tCmd ) =
                    DefinitionSummaryTooltip.update config tMsg model.definitionSummaryTooltip
            in
            ( { model | definitionSummaryTooltip = definitionSummaryTooltip }
            , Cmd.map DefinitionSummaryTooltipMsg tCmd
            )

        _ ->
            ( model, Cmd.none )



-- HELPERS


openDefinition : Config -> Model -> Reference -> ( Model, Cmd Msg )
openDefinition config model ref =
    openItem config model Nothing (DefinitionItemRef ref)


open : Config -> Model -> WorkspaceItemRef -> ( Model, Cmd Msg )
open config model ref =
    openItem config model Nothing ref


openReference : Config -> Model -> WorkspaceItemRef -> WorkspaceItemRef -> ( Model, Cmd Msg )
openReference config model relativeToRef ref =
    openItem config model (Just relativeToRef) ref


openItem : Config -> Model -> Maybe WorkspaceItemRef -> WorkspaceItemRef -> ( Model, Cmd Msg )
openItem config ({ workspaceItems } as model) relativeToRef ref =
    case ref of
        SearchResultsItemRef _ ->
            ( model, Cmd.none )

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
                    , scrollToItem ref
                    )

                else
                    ( model, Cmd.none )

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
                , Cmd.batch [ HttpApi.perform config.api (fetchDefinition config dRef), scrollToItem ref ]
                )


handleKeyboardShortcut : Model -> KeyboardShortcut -> ( Model, Cmd Msg )
handleKeyboardShortcut model shortcut =
    let
        scrollToCmd =
            WorkspaceItems.focus
                >> Maybe.map WorkspaceItem.reference
                >> Maybe.map scrollToItem
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


scrollToItem : WorkspaceItemRef -> Cmd Msg
scrollToItem ref =
    let
        targetId =
            "workspac-item-" ++ WorkspaceItemRef.toString ref
    in
    ScrollTo.scrollTo NoOp "pane" targetId



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
            (Click.onClick (ChangeDefinitionItemTab wsRef WorkspaceItem.CodeTab))
    , docs =
        TabList.tab "Docs"
            (Click.onClick (ChangeDefinitionItemTab wsRef (WorkspaceItem.DocsTab Doc.emptyDocFoldToggles)))
    }


viewLibraryTag : ProjectDependency -> Html msg
viewLibraryTag dep =
    ContextualTag.contextualTag Icon.book (ProjectDependency.toString dep)
        |> ContextualTag.decorativePurple
        |> ContextualTag.view


viewItem : DefinitionSummaryTooltip.Model -> WorkspaceItem -> Bool -> Html Msg
viewItem definitionSummaryTooltip item isFocused =
    let
        cardBase =
            WorkspaceCard.empty

        card =
            case item of
                WorkspaceItem.Loading _ ->
                    Nothing

                WorkspaceItem.Success wsRef (WorkspaceItem.DefinitionWorkspaceItem state defItem) ->
                    let
                        tabs =
                            definitionItemTabs wsRef

                        withTabList c =
                            if hasDocs defItem then
                                case state.activeTab of
                                    WorkspaceItem.CodeTab ->
                                        c |> WorkspaceCard.withTabList (TabList.tabList [] tabs.code [ tabs.docs ])

                                    WorkspaceItem.DocsTab _ ->
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
                                ( WorkspaceItem.DocsTab docFoldToggles, Just docs ) ->
                                    Doc.view (syntaxConfig definitionSummaryTooltip)
                                        (always NoOp)
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
                        |> Just

                WorkspaceItem.Failure wsRef e ->
                    cardBase
                        |> WorkspaceCard.withTitle ("Failed to load definition: " ++ WorkspaceItemRef.toHumanString wsRef)
                        |> WorkspaceCard.withContent
                            [ span [ class "error" ]
                                [ span [ class "error_icon" ] [ Icon.view Icon.warn ]
                                , text (Util.httpErrorToString e)
                                ]
                            ]
                        |> Just

                _ ->
                    cardBase
                        |> WorkspaceCard.withContent []
                        |> Just
    in
    card
        |> Maybe.map (WorkspaceCard.withFocus isFocused)
        |> Maybe.map WorkspaceCard.view
        |> Maybe.withDefault UI.nothing


view : Bool -> Model -> Html Msg
view isFocused model =
    div [ class "workspace-pane", classList [ ( "workspace-pane_focused", isFocused ) ] ]
        (model.workspaceItems
            |> WorkspaceItems.mapToList (viewItem model.definitionSummaryTooltip)
        )
