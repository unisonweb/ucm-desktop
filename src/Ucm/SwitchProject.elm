module Ucm.SwitchProject exposing (..)

import Code.ProjectName as ProjectName exposing (ProjectName)
import Html exposing (Html)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Maybe.Extra as MaybeE
import RemoteData exposing (RemoteData(..), WebData)
import UI.AnchoredOverlay as AnchoredOverlay exposing (AnchoredOverlay)
import UI.Button as Button
import UI.Icon as Icon
import Ucm.Api as UcmApi
import Ucm.AppContext exposing (AppContext)
import Ucm.SearchProjectSheet as SearchProjectSheet



-- MODEL


type alias Sheet =
    { sheet : SearchProjectSheet.Model
    , projectSuggestions : WebData (List ProjectName)
    }


type Model
    = Closed
    | Open Sheet


init : Model
init =
    Closed



-- UPDATE


type Msg
    = ToggleSheet
    | CloseSheet
    | FetchProjectsFinished (HttpResult (List ProjectName))
    | SearchProjectSheetMsg SearchProjectSheet.Msg


type OutMsg
    = None
    | SwitchProjectRequest ProjectName


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext msg model =
    case ( msg, model ) of
        ( ToggleSheet, Closed ) ->
            let
                sheet =
                    { sheet = SearchProjectSheet.init
                    , projectSuggestions = Loading
                    }
            in
            ( Open sheet, fetchProjects appContext, None )

        ( ToggleSheet, Open _ ) ->
            ( Closed, Cmd.none, None )

        ( CloseSheet, _ ) ->
            ( Closed, Cmd.none, None )

        ( FetchProjectsFinished projects, Open s ) ->
            let
                sheet =
                    { s | projectSuggestions = RemoteData.fromResult projects }
            in
            ( Open sheet, Cmd.none, None )

        ( SearchProjectSheetMsg sbsMsg, Open sheet ) ->
            let
                ( newSheet, cmd, sbsOut ) =
                    SearchProjectSheet.update appContext sbsMsg sheet.sheet
            in
            case sbsOut of
                SearchProjectSheet.NoOutMsg ->
                    ( Open { sheet | sheet = newSheet }, Cmd.map SearchProjectSheetMsg cmd, None )

                SearchProjectSheet.SelectProjectRequest projectName ->
                    ( Closed, Cmd.map SearchProjectSheetMsg cmd, SwitchProjectRequest projectName )

        _ ->
            ( model, Cmd.none, None )


toggleSheet : AppContext -> Model -> ( Model, Cmd Msg )
toggleSheet appContext model =
    case model of
        Closed ->
            let
                sheet =
                    { sheet = SearchProjectSheet.init
                    , projectSuggestions = Loading
                    }
            in
            ( Open sheet, fetchProjects appContext )

        Open _ ->
            ( Closed, Cmd.none )



-- EFFECTS


fetchProjects : AppContext -> Cmd Msg
fetchProjects appContext =
    let
        decode =
            Decode.list <|
                Decode.field "projectName" ProjectName.decode
    in
    UcmApi.projects Nothing
        |> HttpApi.toRequest decode FetchProjectsFinished
        |> HttpApi.perform appContext.api



-- VIEW


viewSuggestions : List ProjectName -> List (Html SearchProjectSheet.Msg)
viewSuggestions projects =
    let
        suggestions =
            if List.isEmpty projects then
                Nothing

            else
                Just (SearchProjectSheet.viewProjectList "Projects" (List.take 8 projects))
    in
    MaybeE.values [ suggestions ]


viewSheet : Sheet -> Html Msg
viewSheet sheet =
    let
        suggestions =
            { data = sheet.projectSuggestions
            , view = viewSuggestions
            }
    in
    Html.map SearchProjectSheetMsg
        (SearchProjectSheet.view
            "Switch Project"
            suggestions
            Nothing
            sheet.sheet
        )


toAnchoredOverlay : ProjectName -> Model -> AnchoredOverlay Msg
toAnchoredOverlay projectName model =
    let
        button caret =
            Button.iconThenLabel ToggleSheet Icon.pencilRuler (ProjectName.toString projectName)
                |> Button.withIconAfterLabel caret
                |> Button.small
                |> Button.stopPropagation
                |> Button.view

        ao_ =
            AnchoredOverlay.anchoredOverlay CloseSheet
    in
    case model of
        Closed ->
            ao_ (button Icon.caretDown)

        Open sheet ->
            ao_ (button Icon.caretUp)
                |> AnchoredOverlay.withSheetPosition AnchoredOverlay.BottomLeft
                |> AnchoredOverlay.withSheet (AnchoredOverlay.sheet (viewSheet sheet))
