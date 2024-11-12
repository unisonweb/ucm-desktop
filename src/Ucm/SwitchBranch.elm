module Ucm.SwitchBranch exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.ProjectName exposing (ProjectName)
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
import Ucm.SearchBranchSheet as SearchBranchSheet



-- MODEL


type alias Sheet =
    { sheet : SearchBranchSheet.Model
    , branchSuggestions : WebData (List BranchRef)
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
    | FetchProjectBranchesFinished (HttpResult (List BranchRef))
    | SearchBranchSheetMsg SearchBranchSheet.Msg


type OutMsg
    = None
    | SwitchToBranchRequest BranchRef


update : AppContext -> ProjectName -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectName msg model =
    case ( msg, model ) of
        ( ToggleSheet, Closed ) ->
            let
                sheet =
                    { sheet = SearchBranchSheet.init
                    , branchSuggestions = Loading
                    }
            in
            ( Open sheet
            , fetchBranches appContext projectName
            , None
            )

        ( ToggleSheet, Open _ ) ->
            ( Closed, Cmd.none, None )

        ( CloseSheet, _ ) ->
            ( Closed, Cmd.none, None )

        ( FetchProjectBranchesFinished branches, Open s ) ->
            let
                sheet =
                    { s | branchSuggestions = RemoteData.fromResult branches }
            in
            ( Open sheet, Cmd.none, None )

        ( SearchBranchSheetMsg sbsMsg, Open sheet ) ->
            let
                ( newSheet, cmd, sbsOut ) =
                    SearchBranchSheet.update appContext projectName sbsMsg sheet.sheet
            in
            case sbsOut of
                SearchBranchSheet.NoOutMsg ->
                    ( Open { sheet | sheet = newSheet }, Cmd.map SearchBranchSheetMsg cmd, None )

                SearchBranchSheet.SelectBranchRequest br ->
                    ( Closed, Cmd.map SearchBranchSheetMsg cmd, SwitchToBranchRequest br )

        _ ->
            ( model, Cmd.none, None )


toggleSheet : AppContext -> ProjectName -> Model -> ( Model, Cmd Msg )
toggleSheet appContext projectName model =
    case model of
        Closed ->
            let
                sheet =
                    { sheet = SearchBranchSheet.init
                    , branchSuggestions = Loading
                    }
            in
            ( Open sheet
            , fetchBranches appContext projectName
            )

        Open _ ->
            ( Closed, Cmd.none )



-- EFFECTS


fetchBranches : AppContext -> ProjectName -> Cmd Msg
fetchBranches appContext projectName =
    let
        decode =
            Decode.list <|
                Decode.field "branchName" BranchRef.decode
    in
    UcmApi.projectBranches projectName Nothing
        |> HttpApi.toRequest decode FetchProjectBranchesFinished
        |> HttpApi.perform appContext.api



-- VIEW


viewSuggestions : List BranchRef -> List (Html SearchBranchSheet.Msg)
viewSuggestions branches =
    let
        suggestions =
            if List.isEmpty branches then
                Nothing

            else
                Just (SearchBranchSheet.viewBranchList "Branches" (List.take 8 branches))
    in
    MaybeE.values [ suggestions ]


viewSheet : Sheet -> Html Msg
viewSheet sheet =
    let
        suggestions =
            { data = sheet.branchSuggestions
            , view = viewSuggestions
            }
    in
    Html.map SearchBranchSheetMsg
        (SearchBranchSheet.view
            "Switch Branch"
            suggestions
            Nothing
            sheet.sheet
        )


toAnchoredOverlay : BranchRef -> Model -> AnchoredOverlay Msg
toAnchoredOverlay branchRef model =
    let
        button caret =
            Button.iconThenLabel ToggleSheet Icon.branch (BranchRef.toString branchRef)
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
