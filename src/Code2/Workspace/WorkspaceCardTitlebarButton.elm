module Code2.Workspace.WorkspaceCardTitlebarButton exposing (..)

import Html exposing (Html)
import UI.Button as Button
import UI.Icon exposing (Icon)
import UI.Tooltip as Tooltip


type TitlebarTooltip msg
    = NoTooltip
    | LeftOf (Html msg)
    | RightOf (Html msg)


type alias TitlebarButton msg =
    { icon : Icon msg
    , onClick : msg
    , tooltip : TitlebarTooltip msg
    }


titlebarButton : msg -> Icon msg -> TitlebarButton msg
titlebarButton onClick icon =
    { onClick = onClick, icon = icon, tooltip = NoTooltip }


withLeftOfTooltip : Html msg -> TitlebarButton msg -> TitlebarButton msg
withLeftOfTooltip content titlebarButton_ =
    { titlebarButton_ | tooltip = LeftOf content }


withRightOfTooltip : Html msg -> TitlebarButton msg -> TitlebarButton msg
withRightOfTooltip content titlebarButton_ =
    { titlebarButton_ | tooltip = RightOf content }


view : TitlebarButton msg -> Html msg
view { icon, onClick, tooltip } =
    let
        button =
            Button.icon onClick icon
                |> Button.stopPropagation
                |> Button.subdued
                |> Button.small
                |> Button.view
    in
    case tooltip of
        NoTooltip ->
            button

        LeftOf tooltipContent ->
            Tooltip.rich tooltipContent
                |> Tooltip.tooltip
                |> Tooltip.below
                |> Tooltip.withArrow Tooltip.End
                |> Tooltip.view button

        RightOf tooltipContent ->
            Tooltip.rich tooltipContent
                |> Tooltip.tooltip
                |> Tooltip.below
                |> Tooltip.withArrow Tooltip.Start
                |> Tooltip.view button
