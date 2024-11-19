module Ucm.ContextualTag exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import UI.Icon as Icon exposing (Icon)


type TagColor
    = Subdued
    | DecorativePurple
    | DecorativeBlue


type alias ContextualTag msg =
    { icon : Icon msg
    , label : Html msg
    , color : TagColor
    }



-- CREATE


contextualTag : Icon msg -> String -> ContextualTag msg
contextualTag icon label =
    contextualTag_ icon (text label)


contextualTag_ : Icon msg -> Html msg -> ContextualTag msg
contextualTag_ icon label =
    { icon = icon
    , label = label
    , color = Subdued
    }



-- MODIFY


subdued : ContextualTag msg -> ContextualTag msg
subdued tag =
    { tag | color = Subdued }


decorativePurple : ContextualTag msg -> ContextualTag msg
decorativePurple tag =
    { tag | color = DecorativePurple }


decorativeBlue : ContextualTag msg -> ContextualTag msg
decorativeBlue tag =
    { tag | color = DecorativeBlue }



-- VIEW


view : ContextualTag msg -> Html msg
view tag =
    let
        color =
            case tag.color of
                Subdued ->
                    "subdued"

                DecorativePurple ->
                    "decorative-purple"

                DecorativeBlue ->
                    "decorative-blue"
    in
    div
        [ class "contextual-tag", class color ]
        [ Icon.view tag.icon, tag.label ]
