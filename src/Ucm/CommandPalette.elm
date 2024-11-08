module Ucm.CommandPalette exposing (..)

import Code.CodebaseApi as CodebaseApi
import Code.Config exposing (Config)
import Code.Definition.AbilityConstructor exposing (AbilityConstructor(..))
import Code.Definition.DataConstructor exposing (DataConstructor(..))
import Code.Definition.Reference as Reference exposing (Reference)
import Code.Definition.Term exposing (Term(..))
import Code.Definition.Type exposing (Type(..))
import Code.Finder.FinderMatch as FinderMatch exposing (FinderItem(..), FinderMatch)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Syntax as Syntax
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Search as Search exposing (Search(..))
import Lib.SearchResults as SearchResults
import UI.Click as Click
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut
import UI.Modal exposing (Modal)
import Ucm.AppContext exposing (AppContext)
import Ucm.CommandPalette.CommandPaletteItem as CommandPaletteItem exposing (CommandPaletteItem)
import Ucm.CommandPalette.CommandPaletteModal as CommandPaletteModal



-- MODEL


type CommandPaletteContext
    = NoContext
    | Definition FQN


type alias Model =
    { context : CommandPaletteContext
    , search : Search FinderMatch
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : AppContext -> CommandPaletteContext -> Model
init appContext context =
    { context = context
    , search = Search.empty
    , keyboardShortcut = KeyboardShortcut.init appContext.operatingSystem
    }



-- UPDATE


type Msg
    = UpdateQuery String
    | FetchMatchesFinished String (HttpResult (List FinderMatch))
    | SelectMatch Reference
    | PerformSearch
    | Close


type OutMsg
    = NoOut
    | CloseRequest
    | SelectDefinition Reference


update : AppContext -> Config -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext config msg model =
    case msg of
        UpdateQuery q ->
            let
                search =
                    Search.withQuery q model.search

                cmd =
                    if Search.hasSubstantialQuery search then
                        Search.debounce PerformSearch

                    else
                        Cmd.none
            in
            ( { model | search = search }
            , cmd
            , NoOut
            )

        PerformSearch ->
            ( { model | search = Search.toSearching model.search }
            , fetchDefinitionMatches appContext config (Search.query model.search)
            , NoOut
            )

        FetchMatchesFinished query results ->
            if Search.queryEquals query model.search then
                let
                    search =
                        case results of
                            Ok r ->
                                Search.toSuccess (SearchResults.fromList r) model.search

                            Err e ->
                                Search.toFailure e model.search
                in
                ( { model | search = search }, Cmd.none, NoOut )

            else
                ( model, Cmd.none, NoOut )

        SelectMatch ref ->
            ( model, Cmd.none, SelectDefinition ref )

        Close ->
            ( model, Cmd.none, CloseRequest )



-- EFFECTS


fetchDefinitionMatches : AppContext -> Config -> String -> Cmd Msg
fetchDefinitionMatches appContext config query =
    let
        limit =
            9

        sourceWidth =
            Syntax.Width 100
    in
    CodebaseApi.Find
        { perspective = config.perspective
        , withinFqn = Nothing
        , limit = limit
        , sourceWidth = sourceWidth
        , query = query
        }
        |> config.toApiEndpoint
        |> HttpApi.toRequest FinderMatch.decodeMatches (FetchMatchesFinished query)
        |> HttpApi.perform appContext.api



-- SUBSCRIPTIONS
-- VIEW


matchToItem : FinderMatch -> CommandPaletteItem Msg
matchToItem match =
    let
        ( icon, name_, ref ) =
            case match.item of
                FinderMatch.TermItem (Term _ _ { name, fqn }) ->
                    ( Icon.term, name, Reference.fromFQN Reference.TermReference fqn )

                FinderMatch.TypeItem (Type _ _ { name, fqn }) ->
                    ( Icon.type_, name, Reference.fromFQN Reference.TypeReference fqn )

                FinderMatch.AbilityConstructorItem (AbilityConstructor _ { name, fqn }) ->
                    ( Icon.abilityConstructor, name, Reference.fromFQN Reference.AbilityConstructorReference fqn )

                FinderMatch.DataConstructorItem (DataConstructor _ { name, fqn }) ->
                    ( Icon.dataConstructor, name, Reference.fromFQN Reference.DataConstructorReference fqn )
    in
    CommandPaletteItem.item
        icon
        (FQN.view name_)
        (Click.onClick (SelectMatch ref))


view : Model -> Modal Msg
view model =
    let
        viewConfig =
            { updateQueryMsg = UpdateQuery
            , closeMsg = Close
            , keyboardShortcut = model.keyboardShortcut
            }

        searchResultItems =
            model.search
                |> Search.searchResults
                |> Maybe.map (SearchResults.mapMatches matchToItem)
    in
    CommandPaletteModal.empty
        |> CommandPaletteModal.withQuery (Search.query model.search)
        |> CommandPaletteModal.withSearchResultItems_ searchResultItems
        |> CommandPaletteModal.view viewConfig
