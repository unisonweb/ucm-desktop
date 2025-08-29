port module Window exposing (..)

import Browser
import Html
    exposing
        ( Html
        , aside
        , div
        , footer
        , h2
        , h3
        , header
        , img
        , main_
        , p
        , text
        )
import Html.Attributes exposing (alt, class, classList, id, src)
import SplitPane.SplitPane as SplitPane
import UI
import UI.ActionMenu as ActionMenu
import UI.Button as Button
import UI.Click as Click
import UI.Divider as Divider
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut
import UI.KeyboardShortcut.Key as Key exposing (letter)
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent
import UI.Modal as Modal exposing (Modal)
import UI.Tooltip as Tooltip
import Ucm.AppContext exposing (AppContext)
import Ucm.Link as Link



-- MODEL


type WindowModal
    = NoModal
    | AboutModal
    | KeyboardShortcutsModal


type alias Model =
    { splitPane : SplitPane.State
    , isSettingsMenuOpen : Bool
    , modal : WindowModal
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : AppContext -> Model
init appContext =
    { splitPane =
        SplitPane.init SplitPane.Horizontal
            |> SplitPane.configureSplitter (SplitPane.px 256 Nothing)
    , isSettingsMenuOpen = False
    , modal = NoModal
    , keyboardShortcut = KeyboardShortcut.init appContext.operatingSystem
    }



-- UPDATE


type Msg
    = SplitPaneMsg SplitPane.Msg
    | ToggleSettingsMenu
    | ChangeTheme String
    | ReloadApp
    | ResetToFactorySettings
    | ShowAboutModal
    | ShowKeyboardShortcutsModal
    | CloseModal
    | Keydown KeyboardEvent.KeyboardEvent
    | KeyboardShortcutMsg KeyboardShortcut.Msg


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

        ToggleSettingsMenu ->
            ( { model | isSettingsMenuOpen = not model.isSettingsMenuOpen }, Cmd.none )

        ChangeTheme theme ->
            ( { model | isSettingsMenuOpen = False }, saveTheme theme )

        ReloadApp ->
            ( model, reloadApp () )

        ResetToFactorySettings ->
            ( { model | isSettingsMenuOpen = False }, clearSettings () )

        ShowKeyboardShortcutsModal ->
            ( { model | modal = KeyboardShortcutsModal, isSettingsMenuOpen = False }, Cmd.none )

        ShowAboutModal ->
            ( { model | modal = AboutModal, isSettingsMenuOpen = False }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        Keydown event ->
            let
                shortcut =
                    KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut event
            in
            case shortcut of
                KeyboardShortcut.Sequence _ Key.Escape ->
                    ( { model | modal = NoModal, isSettingsMenuOpen = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd )



-- PORTS


port saveTheme : String -> Cmd msg


port reloadApp : () -> Cmd msg


port clearSettings : () -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SplitPaneMsg (SplitPane.subscriptions model.splitPane)
        , KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown
        ]



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



-- VIEW


viewWindowTitlebar : Html msg -> String -> WindowTitlebar msg -> Html msg
viewWindowTitlebar settingsMenu id_ titlebar_ =
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
                    , right =
                        cfg.right
                            ++ [ Tooltip.text "Settings"
                                    |> Tooltip.tooltip
                                    |> Tooltip.below
                                    |> Tooltip.withArrow Tooltip.End
                                    |> Tooltip.view
                                        settingsMenu
                               ]
                    , transparent = False
                    , border = cfg.border
                    }

                TextWindowTitlebar cfg ->
                    { left = []
                    , center = [ text cfg.label ]
                    , right = [ settingsMenu ]
                    , transparent = False
                    , border = cfg.border
                    }
    in
    header
        [ id (id_ ++ "_window-titlebar")
        , class "window-control-bar window-titlebar"
        , classList
            [ ( "window-titlebar_transparent", transparent )
            , ( "window-titlebar_borderless", not border )
            ]
        ]
        [ div [ class "window-control-bar-group window-titlebar_left" ] left
        , div [ class "window-control-bar-group window-titlebar_center" ] center
        , div [ class "window-control-bar-group window-titlebar_right" ] right
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
                [ div [ class "window-control-bar-group window-footer_left" ] left
                , div [ class "window-control-bar-group window-footer_center" ] center
                , div [ class "window-control-bar-group window-footer_right" ] right
                ]


aboutModal : AppContext -> Modal Msg
aboutModal appContext =
    let
        content =
            div [ class "about" ]
                [ img [ src appContext.assets.appIcon, alt "UCM App Icon", class "app-icon" ] []
                , h2 [] [ text "UCM Desktop" ]
                , p [] [ text "A companion app to the Unison programming language" ]
                , div [ class "info" ] [ text ("Version: " ++ appContext.version) ]
                , Divider.divider |> Divider.small |> Divider.view
                , Button.button CloseModal "Close" |> Button.small |> Button.emphasized |> Button.view
                , div [ class "copyright" ] [ text "© 2025 Unison Computing, PBC" ]
                ]
    in
    Modal.modal "about-modal" CloseModal (Modal.content content)


keyboardShortcutsModal : AppContext -> Modal Msg
keyboardShortcutsModal appContext =
    let
        -- Why not use the one from Model? Well, we don't
        -- really want this to light up sequence shortcuts
        -- when viewing the shortcuts modal.
        keyboardShortcut =
            KeyboardShortcut.init appContext.operatingSystem

        group title commands =
            div [ class "group" ] [ h3 [] [ text title ], div [ class "commands" ] commands ]

        command title shortcuts =
            div [ class "command" ]
                [ text title
                , div [ class "instructions" ]
                    [ KeyboardShortcut.viewShortcuts keyboardShortcut shortcuts
                    ]
                ]

        single key1 =
            KeyboardShortcut.single key1

        win k =
            KeyboardShortcut.Sequence (Just (Key.W Key.Lower)) k

        chord key1 key2 =
            KeyboardShortcut.Chord key1 key2

        windowNav =
            group "Window navigation"
                [ command "Toggle sidebar" [ win (letter Key.S), chord Key.Meta (letter Key.B), chord Key.Ctrl (letter Key.B) ]
                , command "Show project picker" [ win (letter Key.P) ]
                , command "Show branch picker" [ win (letter Key.B) ]
                , command "Toggle right pane" [ win (letter Key.R) ]
                , command "Focus right pane" [ win (letter Key.L), win Key.ArrowRight ]
                , command "Focus left pane" [ win (letter Key.H), win Key.ArrowLeft ]
                ]

        paneNav =
            group "Pane navigation"
                [ command "Move focus up" [ single Key.ArrowUp, single (letter Key.K) ]
                , command "Move focus down" [ single Key.ArrowDown, single (letter Key.J) ]
                , command "Move card up" [ chord Key.Shift Key.ArrowUp, chord Key.Shift (letter Key.K) ]
                , command "Move card down" [ chord Key.Shift Key.ArrowDown, chord Key.Shift (letter Key.J) ]
                , command "Toggle fold for focused" [ single (letter Key.Z) ]
                , command "Toggle fold for all cards" [ chord Key.Shift (letter Key.Z) ]
                , command "Close focused card" [ single (letter Key.X) ]
                , command "Close all cards" [ chord Key.Shift (letter Key.X) ]
                ]

        other =
            group "Other"
                [ command "Command palette / Search" [ single Key.ForwardSlash, chord Key.Meta (letter Key.K), chord Key.Ctrl (letter Key.K) ]
                ]

        content =
            div [ class "inner-content" ]
                [ div [ class "all-instructions" ]
                    [ windowNav
                    , paneNav
                    , other
                    ]
                ]
    in
    Modal.modal "keyboard-shortcuts-modal" CloseModal (Modal.content content)
        |> Modal.withHeader "Keyboard shortcuts"


view : AppContext -> (Msg -> msg) -> Model -> Window msg -> Browser.Document msg
view appContext toMsg model win =
    let
        settingsMenu =
            ActionMenu.items
                (ActionMenu.titleItem "Theme")
                [ ActionMenu.optionItem Icon.computer "System" (Click.onClick (ChangeTheme "system"))
                , ActionMenu.optionItem Icon.sun "Unison Light" (Click.onClick (ChangeTheme "unison-light"))
                , ActionMenu.optionItem Icon.moon "Unison Dark" (Click.onClick (ChangeTheme "unison-dark"))
                , ActionMenu.dividerItem
                , ActionMenu.titleItem "Resources"
                , ActionMenu.optionItem Icon.graduationCap "Unison Docs" Link.docs
                , ActionMenu.optionItem Icon.browse "Unison Share (libraries)" Link.share
                , ActionMenu.optionItem Icon.keyboard "Keyboard shortcuts" (Click.onClick ShowKeyboardShortcutsModal)
                , ActionMenu.optionItem Icon.unisonMark "About" (Click.onClick ShowAboutModal)
                , ActionMenu.dividerItem
                , ActionMenu.titleItem "Debug"
                , ActionMenu.optionItem Icon.restartCircle "Restart app" (Click.onClick ReloadApp)
                , ActionMenu.optionItem Icon.factory "Reset to factory settings" (Click.onClick ResetToFactorySettings)
                ]
                |> ActionMenu.fromIconButton ToggleSettingsMenu Icon.cog
                |> ActionMenu.withButtonColor Button.Subdued
                |> ActionMenu.shouldBeOpen model.isSettingsMenuOpen
                |> ActionMenu.view
                |> Html.map toMsg

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
            case ( model.modal, win.modal ) of
                ( AboutModal, _ ) ->
                    aboutModal appContext
                        |> Modal.map toMsg
                        |> Modal.view

                ( KeyboardShortcutsModal, _ ) ->
                    keyboardShortcutsModal appContext
                        |> Modal.map toMsg
                        |> Modal.view

                ( _, Just m ) ->
                    Modal.view m

                _ ->
                    UI.nothing
    in
    { title = "Unison Codebase Manager"
    , body =
        [ viewWindowTitlebar settingsMenu win.id win.titlebar
        , mainContent
        , viewWindowFooter win.id win.footer
        , modal
        ]
    }
