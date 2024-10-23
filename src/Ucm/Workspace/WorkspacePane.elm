module Ucm.Workspace.WorkspacePane exposing (..)

import Code.CodebaseApi as CodebaseApi
import Code.Config exposing (Config)
import Code.Definition.AbilityConstructor as AbilityConstructor
import Code.Definition.DataConstructor as DataConstructor
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
import UI
import UI.Click as Click
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
                            (WorkspaceItem.DefinitionWorkspaceItem defItem)
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
            if WorkspaceItems.member workspaceItems ref then
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

        _ ->
            UI.nothing


viewItem : WorkspaceItem -> Bool -> Html Msg
viewItem item isFocused =
    let
        cardBase =
            WorkspaceCard.empty

        card =
            case item of
                WorkspaceItem.Loading _ ->
                    cardBase
                        |> WorkspaceCard.withContent [ text "Loading..." ]

                WorkspaceItem.Success _ (WorkspaceItem.DefinitionWorkspaceItem defItem) ->
                    let
                        tabList =
                            TabList.tabList [] (TabList.tab "Code" (Click.onClick NoOp)) []
                    in
                    cardBase
                        |> WorkspaceCard.withTitle (FQN.toString (definitionItemName defItem))
                        -- |> WorkspaceCard.withTabList tabList
                        |> WorkspaceCard.withContent [ viewDefinitionItemSource defItem ]

                WorkspaceItem.Failure _ e ->
                    cardBase
                        |> WorkspaceCard.withContent [ text (Util.httpErrorToString e) ]

                _ ->
                    cardBase
                        |> WorkspaceCard.withContent [ text "TODO" ]
    in
    card
        |> WorkspaceCard.withFocus isFocused
        |> WorkspaceCard.view


view : Model -> Html Msg
view model =
    div [ class "workspace-pane" ]
        (model.workspaceItems
            |> WorkspaceItems.mapToList viewItem
        )
