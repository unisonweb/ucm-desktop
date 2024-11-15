module Ucm.UcmConnectivity exposing (..)

import Html exposing (Html, div, h3, p, text)
import Html.Attributes exposing (class)
import Http
import UI
import UI.Icon as Icon
import UI.Tooltip as Tooltip


type UcmConnectivity
    = Connected
    | NotConnected Http.Error


view : UcmConnectivity -> Html msg
view ucmConnectivity =
    case ucmConnectivity of
        Connected ->
            UI.nothing

        NotConnected err ->
            let
                ( errorTitle, errorMessage ) =
                    case err of
                        Http.Timeout ->
                            ( "UCM Timeout", "The connection to UCM timed out" )

                        Http.NetworkError ->
                            ( "UCM Network error"
                            , "There was a network error when trying to connect to UCM. Make sure its running."
                            )

                        Http.BadStatus status ->
                            ( "Unexpected response from UCM", "Bad status: " ++ String.fromInt status )

                        Http.BadBody resp ->
                            ( "Unexpected response from UCM: ", resp )

                        Http.BadUrl url ->
                            ( "Malformed URL ", url )

                tooltipContent =
                    Tooltip.rich (div [] [ h3 [] [ text errorTitle ], p [] [ text errorMessage ] ])

                icon =
                    div
                        [ class "ucm-connectivity ucm-connectivity_error" ]
                        [ Icon.view Icon.warn ]
            in
            Tooltip.tooltip tooltipContent
                |> Tooltip.withPosition Tooltip.Above
                |> Tooltip.withArrow Tooltip.End
                |> Tooltip.view icon
