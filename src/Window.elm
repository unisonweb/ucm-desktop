module Window exposing (..)

import Browser
import Html
    exposing
        ( Attribute
        , Html
        , aside
        , div
        , footer
        , header
        , main_
        , text
        )
import Html.Attributes exposing (attribute, class, classList, id)
import SplitPane.SplitPane as SplitPane
import UI
import UI.Modal as Modal exposing (Modal)



-- MODEL


type alias Model =
    { splitPane : SplitPane.State
    }


init : Model
init =
    { splitPane =
        SplitPane.init SplitPane.Horizontal
            |> SplitPane.configureSplitter (SplitPane.px 256 Nothing)
    }



-- UPDATE


type Msg
    = SplitPaneMsg SplitPane.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SplitPaneMsg paneMsg ->
            ( { model
                | splitPane =
                    SplitPane.update
                        paneMsg
                        model.splitPane
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SplitPaneMsg (SplitPane.subscriptions model.splitPane)



-- WINDOW


type WindowTitlebar msg
    = HiddenWindowTitlebar
    | WindowTitlebar
        { left : List (Html msg)
        , center : List (Html msg)
        , right : List (Html msg)
        , border : Bool
        }
    | TextWindowTitlebar { label : String, border : Bool }


type WindowFooter msg
    = NoWindowFooter
    | WindowFooter
        { left : List (Html msg)
        , center : List (Html msg)
        , right : List (Html msg)
        }


type WindowSidebar msg
    = NoWindowSidebar
    | WindowSidebar
        { content : List (Html msg)
        }


type alias Window msg =
    { id : String
    , titlebar : WindowTitlebar msg
    , leftSidebar : WindowSidebar msg
    , content : List (Html msg)
    , footer : WindowFooter msg
    , modal : Maybe (Modal msg)
    }



-- CREATE


window : String -> Window msg
window id_ =
    { id = id_
    , titlebar = HiddenWindowTitlebar
    , leftSidebar = NoWindowSidebar
    , content = []
    , footer = NoWindowFooter
    , modal = Nothing
    }


titlebar : WindowTitlebar msg
titlebar =
    WindowTitlebar titlebarConfig


titlebarConfig : { left : List (Html msg), center : List (Html msg), right : List (Html msg), border : Bool }
titlebarConfig =
    { left = [], center = [], right = [], border = True }



-- TRANSFORM


withTitlebarLeft : List (Html msg) -> Window msg -> Window msg
withTitlebarLeft left win =
    let
        titlebar_ =
            case win.titlebar of
                HiddenWindowTitlebar ->
                    WindowTitlebar { titlebarConfig | left = left }

                WindowTitlebar t ->
                    WindowTitlebar { t | left = left }

                TextWindowTitlebar cfg ->
                    WindowTitlebar
                        { titlebarConfig
                            | center = [ text cfg.label ]
                            , border = cfg.border
                            , left = left
                        }
    in
    { win | titlebar = titlebar_ }


withTitlebarCenter : List (Html msg) -> Window msg -> Window msg
withTitlebarCenter center win =
    let
        titlebar_ =
            case win.titlebar of
                HiddenWindowTitlebar ->
                    WindowTitlebar { titlebarConfig | center = center }

                WindowTitlebar t ->
                    WindowTitlebar { t | center = center }

                TextWindowTitlebar cfg ->
                    WindowTitlebar
                        { titlebarConfig
                            | center = center
                            , border = cfg.border
                        }
    in
    { win | titlebar = titlebar_ }


withTitlebarRight : List (Html msg) -> Window msg -> Window msg
withTitlebarRight right win =
    let
        titlebar_ =
            case win.titlebar of
                HiddenWindowTitlebar ->
                    WindowTitlebar { titlebarConfig | right = right }

                WindowTitlebar t ->
                    WindowTitlebar { t | right = right }

                TextWindowTitlebar cfg ->
                    WindowTitlebar
                        { titlebarConfig
                            | center = [ text cfg.label ]
                            , right = right
                            , border = cfg.border
                        }
    in
    { win | titlebar = titlebar_ }


withTitlebarLabel : String -> Window msg -> Window msg
withTitlebarLabel label win =
    let
        titlebar_ =
            case win.titlebar of
                HiddenWindowTitlebar ->
                    TextWindowTitlebar { label = label, border = True }

                WindowTitlebar { border } ->
                    TextWindowTitlebar { label = label, border = border }

                TextWindowTitlebar cfg ->
                    TextWindowTitlebar { cfg | label = label }
    in
    { win | titlebar = titlebar_ }


withTitlebarBorder : Window msg -> Window msg
withTitlebarBorder win =
    let
        titlebar_ =
            case win.titlebar of
                HiddenWindowTitlebar ->
                    HiddenWindowTitlebar

                WindowTitlebar t ->
                    WindowTitlebar { t | border = True }

                TextWindowTitlebar cfg ->
                    TextWindowTitlebar { cfg | border = True }
    in
    { win | titlebar = titlebar_ }


withoutTitlebarBorder : Window msg -> Window msg
withoutTitlebarBorder win =
    let
        titlebar_ =
            case win.titlebar of
                HiddenWindowTitlebar ->
                    HiddenWindowTitlebar

                WindowTitlebar t ->
                    WindowTitlebar { t | border = False }

                TextWindowTitlebar cfg ->
                    TextWindowTitlebar { cfg | border = False }
    in
    { win | titlebar = titlebar_ }


withTitlebar : WindowTitlebar msg -> Window msg -> Window msg
withTitlebar titlebar_ win =
    { win | titlebar = titlebar_ }


withContent : List (Html msg) -> Window msg -> Window msg
withContent content win =
    { win | content = content }


withFooter : WindowFooter msg -> Window msg -> Window msg
withFooter footer win =
    { win | footer = footer }


withFooterLeft : List (Html msg) -> Window msg -> Window msg
withFooterLeft left win =
    let
        footer_ =
            case win.footer of
                NoWindowFooter ->
                    WindowFooter { left = left, center = [], right = [] }

                WindowFooter { right, center } ->
                    WindowFooter { left = left, center = center, right = right }
    in
    { win | footer = footer_ }


withFooterCenter : List (Html msg) -> Window msg -> Window msg
withFooterCenter center win =
    let
        footer_ =
            case win.footer of
                NoWindowFooter ->
                    WindowFooter { left = [], center = center, right = [] }

                WindowFooter { left, right } ->
                    WindowFooter { left = left, center = center, right = right }
    in
    { win | footer = footer_ }


withFooterRight : List (Html msg) -> Window msg -> Window msg
withFooterRight right win =
    let
        footer_ =
            case win.footer of
                NoWindowFooter ->
                    WindowFooter { left = [], center = [], right = right }

                WindowFooter { left, center } ->
                    WindowFooter { left = left, center = center, right = right }
    in
    { win | footer = footer_ }


withLeftSidebar : List (Html msg) -> Window msg -> Window msg
withLeftSidebar sidebar win =
    { win | leftSidebar = WindowSidebar { content = sidebar } }


withModal : Modal msg -> Window msg -> Window msg
withModal m win =
    { win | modal = Just m }


map : (fromMsg -> toMsg) -> Window fromMsg -> Window toMsg
map f win =
    let
        map_ =
            List.map (Html.map f)

        newTitlebar =
            case win.titlebar of
                HiddenWindowTitlebar ->
                    HiddenWindowTitlebar

                WindowTitlebar cfg ->
                    WindowTitlebar
                        { left = map_ cfg.left
                        , center = map_ cfg.center
                        , right = map_ cfg.right
                        , border = cfg.border
                        }

                TextWindowTitlebar label ->
                    TextWindowTitlebar label

        newLeftSidebar =
            case win.leftSidebar of
                NoWindowSidebar ->
                    NoWindowSidebar

                WindowSidebar sb ->
                    WindowSidebar { content = map_ sb.content }

        newFooter =
            case win.footer of
                NoWindowFooter ->
                    NoWindowFooter

                WindowFooter cfg ->
                    WindowFooter
                        { left = map_ cfg.left
                        , center = map_ cfg.center
                        , right = map_ cfg.right
                        }
    in
    { id = win.id
    , titlebar = newTitlebar
    , leftSidebar = newLeftSidebar
    , content = map_ win.content
    , footer = newFooter
    , modal = Maybe.map (Modal.map f) win.modal
    }



-- HELPERS


{-| A HTML attribute to enable window draggability for Tauri.
Used for titlebars
-}
windowDraggability : Attribute msg
windowDraggability =
    attribute "data-tauri-drag-region" "1"


{-| Normally, this would happen automatically, but when the Elm app mounts it
removes all previous HTML from <body> (where the Decorum plugin would normally
add a titlebar for Windows)
-}
windowControls : Attribute msg
windowControls =
    attribute "data-tauri-decorum-tb" "1"



-- VIEW


viewWindowTitlebar : String -> WindowTitlebar msg -> Html msg
viewWindowTitlebar id_ titlebar_ =
    let
        { left, center, right, transparent, border } =
            case titlebar_ of
                HiddenWindowTitlebar ->
                    { left = []
                    , center = []
                    , right = []
                    , transparent = True
                    , border = False
                    }

                WindowTitlebar cfg ->
                    { left = cfg.left
                    , center = cfg.center
                    , right = cfg.right
                    , transparent = False
                    , border = cfg.border
                    }

                TextWindowTitlebar cfg ->
                    { left = []
                    , center = [ text cfg.label ]
                    , right = []
                    , transparent = False
                    , border = cfg.border
                    }
    in
    header
        [ id (id_ ++ "_window-titlebar")
        , windowDraggability
        , windowControls
        , class "window-control-bar window-titlebar"
        , classList
            [ ( "window-titlebar_transparent", transparent )
            , ( "window-titlebar_borderless", not border )
            ]
        ]
        [ div [ class "window-control-bar-group" ] left
        , div [ class "window-control-bar-group" ] center
        , div [ class "window-control-bar-group" ] right
        ]


viewWindowFooter : String -> WindowFooter msg -> Html msg
viewWindowFooter id_ footer_ =
    case footer_ of
        NoWindowFooter ->
            UI.nothing

        WindowFooter { left, center, right } ->
            footer
                [ id (id_ ++ "_window-footer")
                , class "window-control-bar window-footer"
                ]
                [ div [ class "window-control-bar-group" ] left
                , div [ class "window-control-bar-group" ] center
                , div [ class "window-control-bar-group" ] right
                ]


view : (Msg -> msg) -> Model -> Window msg -> Browser.Document msg
view toMsg model win =
    let
        mainContent =
            case win.leftSidebar of
                NoWindowSidebar ->
                    main_
                        [ id (win.id ++ "_window-content")
                        , class "window-content"
                        ]
                        win.content

                WindowSidebar sb ->
                    let
                        sidebarPane =
                            aside [ class "window-sidebar" ]
                                sb.content

                        mainPane =
                            div
                                [ id (win.id ++ "_window-content")
                                , class "window-content"
                                ]
                                win.content

                        paneConfig =
                            SplitPane.createViewConfig
                                { toMsg = SplitPaneMsg >> toMsg
                                , customSplitter =
                                    Just (SplitPane.createCustomSplitter (SplitPaneMsg >> toMsg) splitter)
                                }

                        splitter =
                            { attributes = [ class "window-sidebar_resize-handle" ]
                            , children =
                                [ div [ class "window-sidebar_resize-handle_main-pane-side" ] []
                                ]
                            }

                        panes =
                            SplitPane.view paneConfig sidebarPane mainPane model.splitPane
                    in
                    main_ [ class "window-content-shell" ] [ panes ]

        modal =
            case win.modal of
                Just m ->
                    Modal.view m

                Nothing ->
                    UI.nothing
    in
    { title = "UCM"
    , body =
        [ viewWindowTitlebar win.id win.titlebar
        , mainContent
        , viewWindowFooter win.id win.footer
        , modal
        ]
    }
