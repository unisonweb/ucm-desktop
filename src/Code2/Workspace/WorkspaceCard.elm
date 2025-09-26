module Code2.Workspace.WorkspaceCard exposing (..)

import Code.ProjectDependency as ProjectDependency exposing (ProjectDependency)
import Code2.Workspace.WorkspaceCardTitlebarButton as TitlebarButton exposing (titlebarButton)
import Html exposing (Html, div, header, section, span, text)
import Html.Attributes exposing (class)
import Lib.OperatingSystem exposing (OperatingSystem)
import UI
import UI.Card as Card
import UI.Click as Click exposing (Click)
import UI.ContextualTag as ContextualTag
import UI.Icon as Icon exposing (Icon)
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut(..), single)
import UI.KeyboardShortcut.Key as Key exposing (letter)
import UI.TabList as TabList exposing (TabList)


type alias WorkspaceCard msg =
    { titleLeft : List (Html msg)
    , titleRight : List (Html msg)
    , subtitleBar : Maybe (Html msg)
    , tabList : Maybe (TabList msg)
    , content : List (Html msg)
    , hasFocus : Bool
    , domId : Maybe String
    , click : Click msg
    , close : Maybe msg
    , isFolded : Bool
    , toggleFold : Maybe msg
    , className : String
    }



-- CREATE


empty : WorkspaceCard msg
empty =
    { titleLeft = []
    , titleRight = []
    , subtitleBar = Nothing
    , tabList = Nothing
    , content = []
    , hasFocus = False
    , domId = Nothing
    , click = Click.disabled
    , close = Nothing
    , isFolded = False
    , toggleFold = Nothing
    , className = ""
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


withClassName : String -> WorkspaceCard msg -> WorkspaceCard msg
withClassName className card_ =
    { card_ | className = className }


withTitlebar : { left : List (Html msg), right : List (Html msg) } -> WorkspaceCard msg -> WorkspaceCard msg
withTitlebar { left, right } card_ =
    { card_ | titleLeft = left, titleRight = right }


withTitlebarLeft : List (Html msg) -> WorkspaceCard msg -> WorkspaceCard msg
withTitlebarLeft left card_ =
    { card_ | titleLeft = left }


withTitlebarRight : List (Html msg) -> WorkspaceCard msg -> WorkspaceCard msg
withTitlebarRight right card_ =
    { card_ | titleRight = right }


withSubtitle : String -> WorkspaceCard msg -> WorkspaceCard msg
withSubtitle subtitle card_ =
    withSubtitleBar (span [ class "subdued" ] [ text subtitle ]) card_


withSubtitleBar : Html msg -> WorkspaceCard msg -> WorkspaceCard msg
withSubtitleBar subtitle card_ =
    { card_ | subtitleBar = Just subtitle }


withContent : List (Html msg) -> WorkspaceCard msg -> WorkspaceCard msg
withContent content card_ =
    { card_ | content = content }


withDomId : String -> WorkspaceCard msg -> WorkspaceCard msg
withDomId domId card_ =
    { card_ | domId = Just domId }


withClick : Click msg -> WorkspaceCard msg -> WorkspaceCard msg
withClick click card_ =
    { card_ | click = click }


withClose : msg -> WorkspaceCard msg -> WorkspaceCard msg
withClose close card_ =
    { card_ | close = Just close }


withToggleFold : msg -> WorkspaceCard msg -> WorkspaceCard msg
withToggleFold toggleFold card_ =
    { card_ | toggleFold = Just toggleFold }


withIsFolded : Bool -> WorkspaceCard msg -> WorkspaceCard msg
withIsFolded isFolded card_ =
    { card_ | isFolded = isFolded }


fold : WorkspaceCard msg -> WorkspaceCard msg
fold card_ =
    { card_ | isFolded = True }


unfold : WorkspaceCard msg -> WorkspaceCard msg
unfold card_ =
    { card_ | isFolded = False }


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
    , subtitleBar = Maybe.map (Html.map f) card_.subtitleBar
    , tabList = Maybe.map (TabList.map f) card_.tabList
    , content = map_ card_.content
    , hasFocus = card_.hasFocus
    , domId = card_.domId
    , click = Click.map f card_.click
    , close = Maybe.map f card_.close
    , isFolded = card_.isFolded
    , toggleFold = Maybe.map f card_.toggleFold
    , className = card_.className
    }



-- RELATED VIEW HELPERS


viewLibraryTag : ProjectDependency -> Html msg
viewLibraryTag dep =
    ContextualTag.contextualTag Icon.book (ProjectDependency.toString dep)
        |> ContextualTag.decorativePurple
        |> ContextualTag.withTooltipText "Library dependency"
        |> ContextualTag.view



-- VIEW


toggleFoldedIcon : Bool -> Icon msg
toggleFoldedIcon isFolded =
    if isFolded then
        Icon.collapseUp

    else
        Icon.expandDown


consIf : a -> Bool -> List a -> List a
consIf x isTrue xs =
    if isTrue then
        x :: xs

    else
        xs


view : OperatingSystem -> WorkspaceCard msg -> Html msg
view os wsCard =
    let
        { titleLeft, titleRight, subtitleBar, tabList, content, hasFocus, domId, click, close, isFolded, toggleFold } =
            wsCard

        className =
            [ "workspace-card", wsCard.className ]
                |> consIf "focused" hasFocus
                |> consIf "folded" isFolded
                |> String.join " "

        close_ =
            case close of
                Nothing ->
                    []

                Just closeMsg ->
                    [ titlebarButton closeMsg
                        Icon.x
                        |> TitlebarButton.withLeftOfTooltip
                            (div [ class "tooltip-with-shortcut" ]
                                [ text "Close"
                                , KeyboardShortcut.viewSimple os (single (letter Key.X))
                                , text "Close all:"
                                , KeyboardShortcut.viewSimple os (Chord Key.Shift (letter Key.X))
                                ]
                            )
                        |> TitlebarButton.view
                    ]

        toggleFold_ =
            case toggleFold of
                Nothing ->
                    []

                Just toggle ->
                    [ titlebarButton toggle
                        (toggleFoldedIcon isFolded)
                        |> TitlebarButton.withLeftOfTooltip
                            (div [ class "tooltip-with-shortcut" ]
                                [ text "Toggle fold"
                                , KeyboardShortcut.viewSimple os (single (letter Key.Z))
                                , text "Toggle all:"
                                , KeyboardShortcut.viewSimple os (Chord Key.Shift (letter Key.Z))
                                ]
                            )
                        |> TitlebarButton.view
                    ]

        titleRight_ =
            div [ class "workspace-card_titlebar_right" ] (titleRight ++ toggleFold_ ++ close_)

        titlebar =
            header [ class "workspace-card_titlebar" ]
                [ div [ class "workspace-card_titlebar_left" ] titleLeft
                , titleRight_
                ]

        subtitleBar_ =
            case subtitleBar of
                Just stb ->
                    div [ class "workspace-card_subtitlebar" ] [ stb ]

                Nothing ->
                    UI.nothing

        cardContent =
            [ titlebar
            , subtitleBar_
            , div [ class "workspace-card_foldable-content" ]
                [ tabList |> Maybe.map TabList.view |> Maybe.withDefault UI.nothing
                , section [ class "workspace-card_main-content" ] content
                ]
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
            if hasFocus then
                html

            else
                Click.view [ class "clickable-workspace-card" ] [ html ] click
