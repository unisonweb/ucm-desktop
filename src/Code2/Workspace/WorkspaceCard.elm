module Code2.Workspace.WorkspaceCard exposing (..)

import Html exposing (Html, div, header, section, span, text)
import Html.Attributes exposing (class)
import UI
import UI.Card as Card
import UI.Click as Click exposing (Click)
import UI.TabList as TabList exposing (TabList)


type alias WorkspaceCard msg =
    { titleLeft : List (Html msg)
    , titleRight : List (Html msg)
    , tabList : Maybe (TabList msg)
    , content : List (Html msg)
    , hasFocus : Bool
    , domId : Maybe String
    , click : Click msg
    }



-- CREATE


empty : WorkspaceCard msg
empty =
    { titleLeft = []
    , titleRight = []
    , tabList = Nothing
    , content = []
    , hasFocus = False
    , domId = Nothing
    , click = Click.disabled
    }


card : List (Html msg) -> WorkspaceCard msg
card content =
    empty
        |> withContent content



-- MODIFY


withTitle : String -> WorkspaceCard msg -> WorkspaceCard msg
withTitle title card_ =
    { card_
        | titleLeft =
            [ span [ class "workspace-card_title" ] [ text title ] ]
    }


withTitlebar : { left : List (Html msg), right : List (Html msg) } -> WorkspaceCard msg -> WorkspaceCard msg
withTitlebar { left, right } card_ =
    { card_ | titleLeft = left, titleRight = right }


withTitlebarLeft : List (Html msg) -> WorkspaceCard msg -> WorkspaceCard msg
withTitlebarLeft left card_ =
    { card_ | titleLeft = left }


withTitlebarRight : List (Html msg) -> WorkspaceCard msg -> WorkspaceCard msg
withTitlebarRight right card_ =
    { card_ | titleRight = right }


withContent : List (Html msg) -> WorkspaceCard msg -> WorkspaceCard msg
withContent content card_ =
    { card_ | content = content }


withDomId : String -> WorkspaceCard msg -> WorkspaceCard msg
withDomId domId card_ =
    { card_ | domId = Just domId }


withClick : Click msg -> WorkspaceCard msg -> WorkspaceCard msg
withClick click card_ =
    { card_ | click = click }


withTabList : TabList msg -> WorkspaceCard msg -> WorkspaceCard msg
withTabList tabList card_ =
    { card_ | tabList = Just tabList }


withFocus : Bool -> WorkspaceCard msg -> WorkspaceCard msg
withFocus hasFocus card_ =
    { card_ | hasFocus = hasFocus }


focus : WorkspaceCard msg -> WorkspaceCard msg
focus card_ =
    { card_ | hasFocus = True }


blur : WorkspaceCard msg -> WorkspaceCard msg
blur card_ =
    { card_ | hasFocus = False }



-- MAP


map : (fromMsg -> toMsg) -> WorkspaceCard fromMsg -> WorkspaceCard toMsg
map f card_ =
    let
        map_ =
            List.map (Html.map f)
    in
    { titleLeft = map_ card_.titleLeft
    , titleRight = map_ card_.titleRight
    , tabList = Maybe.map (TabList.map f) card_.tabList
    , content = map_ card_.content
    , hasFocus = card_.hasFocus
    , domId = card_.domId
    , click = Click.map f card_.click
    }



-- VIEW


view : WorkspaceCard msg -> Html msg
view { titleLeft, titleRight, tabList, content, hasFocus, domId, click } =
    let
        className =
            if hasFocus then
                "workspace-card focused"

            else
                "workspace-card"

        titlebar =
            header [ class "workspace-card_titlebar" ]
                [ div [ class "workspace-card_titlebar_left" ] titleLeft
                , div [ class "workspace-card_titlebar_right" ] titleRight
                ]

        cardContent =
            [ titlebar
            , tabList |> Maybe.map TabList.view |> Maybe.withDefault UI.nothing
            , section [ class "workspace-card_main-content" ] content
            ]

        card_ =
            case domId of
                Just domId_ ->
                    Card.card cardContent
                        |> Card.withDomId domId_

                Nothing ->
                    Card.card cardContent

        html =
            card_
                |> Card.asContained
                |> Card.withClassName className
                |> Card.view
    in
    case click of
        Click.Disabled ->
            html

        _ ->
            Click.view [] [ html ] click
