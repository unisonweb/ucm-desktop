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
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Source.SourceViewConfig as SourceViewConfig
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Lib.HttpApi as HttpApi exposing (ApiRequest, HttpResult)
import Lib.ScrollTo as ScrollTo
import Lib.Util as Util
import Maybe.Extra as MaybeE
import UI
import UI.Button as Button
import UI.Click as Click
import UI.Icon as Icon
import UI.TabList as TabList
import Ucm.AppContext exposing (AppContext)
import Ucm.Workspace.WorkspaceCard as WorkspaceCard
import Ucm.Workspace.WorkspaceContext exposing (WorkspaceContext)
import Ucm.Workspace.WorkspaceItem as WorkspaceItem exposing (DefinitionItem(..), WorkspaceItem)
import Ucm.Workspace.WorkspaceItemRef as WorkspaceItemRef exposing (WorkspaceItemRef(..))
import Ucm.Workspace.WorkspaceItems as WorkspaceItems exposing (WorkspaceItems)



-- MODEL


type alias Model =
    { workspaceItems : WorkspaceItems
    }


init : AppContext -> WorkspaceContext -> ( Model, Cmd Msg )
init _ _ =
    ( { workspaceItems = WorkspaceItems.init Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | FetchDefinitionItemFinished Reference (HttpResult DefinitionItem)
    | CloseWorkspaceItem WorkspaceItemRef
    | ChangeDefinitionItemTab WorkspaceItemRef WorkspaceItem.DefinitionItemTab


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FetchDefinitionItemFinished dRef (Ok defItem) ->
            let
                workspaceItemRef =
                    WorkspaceItemRef.DefinitionItemRef dRef
            in
            ( { workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        workspaceItemRef
                        (WorkspaceItem.Success workspaceItemRef
                            (WorkspaceItem.DefinitionWorkspaceItem { activeTab = WorkspaceItem.CodeTab } defItem)
                        )
              }
            , Cmd.none
            )

        FetchDefinitionItemFinished dRef (Err e) ->
            let
                workspaceItemRef =
                    WorkspaceItemRef.DefinitionItemRef dRef
            in
            ( { workspaceItems =
                    WorkspaceItems.replace
                        model.workspaceItems
                        workspaceItemRef
                        (WorkspaceItem.Failure workspaceItemRef e)
              }
            , Cmd.none
            )

        CloseWorkspaceItem ref ->
            ( { workspaceItems =
                    WorkspaceItems.remove model.workspaceItems ref
              }
            , Cmd.none
            )

        ChangeDefinitionItemTab wsRef newTab ->
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



-- VIEW


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


viewDefinitionItemSource : WorkspaceItem.DefinitionItem -> Html msg
viewDefinitionItemSource defItem =
    case defItem of
        WorkspaceItem.TermItem (Term.Term _ _ { info, source }) ->
            Source.viewTermSource (SourceViewConfig.rich SyntaxConfig.empty) info.name source

        WorkspaceItem.TypeItem (Type.Type _ _ { source }) ->
            Source.viewTypeSource (SourceViewConfig.rich SyntaxConfig.empty) source

        _ ->
            UI.nothing


hasDocs : WorkspaceItem.DefinitionItem -> Bool
hasDocs defItem =
    case defItem of
        WorkspaceItem.TermItem (Term.Term _ _ { doc }) ->
            MaybeE.isJust doc

        WorkspaceItem.TypeItem (Type.Type _ _ { doc }) ->
            MaybeE.isJust doc

        _ ->
            False


definitionItemTabs : WorkspaceItemRef -> { code : TabList.Tab Msg, docs : TabList.Tab Msg, tests : TabList.Tab Msg }
definitionItemTabs wsRef =
    { code =
        TabList.tab "Code"
            (Click.onClick (ChangeDefinitionItemTab wsRef WorkspaceItem.CodeTab))
    , docs =
        TabList.tab "Docs"
            (Click.onClick (ChangeDefinitionItemTab wsRef (WorkspaceItem.DocsTab Doc.emptyDocFoldToggles)))
    , tests =
        TabList.tab "Tests"
            (Click.onClick (ChangeDefinitionItemTab wsRef WorkspaceItem.TestsTab))
    }


viewItem : WorkspaceItem -> Bool -> Html Msg
viewItem item isFocused =
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
                                        c |> WorkspaceCard.withTabList (TabList.tabList [] tabs.code [ tabs.docs, tabs.tests ])

                                    WorkspaceItem.DocsTab _ ->
                                        c |> WorkspaceCard.withTabList (TabList.tabList [ tabs.code ] tabs.docs [ tabs.tests ])

                                    WorkspaceItem.TestsTab ->
                                        c |> WorkspaceCard.withTabList (TabList.tabList [ tabs.code, tabs.docs ] tabs.tests [])

                            else
                                c
                    in
                    cardBase
                        |> WorkspaceCard.withTitle (FQN.toString (definitionItemName defItem))
                        |> WorkspaceCard.withTitlebarRight
                            [ Button.icon (CloseWorkspaceItem wsRef) Icon.x
                                |> Button.subdued
                                |> Button.small
                                |> Button.view
                            ]
                        |> withTabList
                        |> WorkspaceCard.withContent [ viewDefinitionItemSource defItem ]
                        |> Just

                WorkspaceItem.Failure _ e ->
                    cardBase
                        |> WorkspaceCard.withContent [ text (Util.httpErrorToString e) ]
                        |> Just

                _ ->
                    cardBase
                        |> WorkspaceCard.withContent [ text "TODO" ]
                        |> Just
    in
    card
        |> Maybe.map (WorkspaceCard.withFocus isFocused)
        |> Maybe.map WorkspaceCard.view
        |> Maybe.withDefault UI.nothing


view : Model -> Html Msg
view model =
    div [ class "workspace-pane" ]
        (model.workspaceItems
            |> WorkspaceItems.mapToList viewItem
        )
