module Ucm.WorkspaceScreen exposing (..)

import Browser
import Code.BranchRef as BranchRef
import Code.CodebaseTree as CodebaseTree
import Code.Config
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import UI.Button as Button
import UI.Icon as Icon
import Ucm.AppContext as AppContext exposing (AppContext)
import Ucm.ProjectName as ProjectName
import Ucm.WorkspaceContext exposing (WorkspaceContext)
import Window



{-
   type alias Config =
       { operatingSystem : OperatingSystem
       , perspective : Perspective
       , toApiEndpoint : ToApiEndpoint
       , api : HttpApi
     }
-}


type alias Model =
    { workspaceContext : WorkspaceContext
    , codebaseTree : CodebaseTree.Model
    , config : Code.Config.Config
    , window : Window.Model
    }


init : AppContext -> WorkspaceContext -> ( Model, Cmd Msg )
init appContext workspaceContext =
    let
        config =
            AppContext.toCodeConfig appContext workspaceContext

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init config
    in
    ( { workspaceContext = workspaceContext
      , codebaseTree = codebaseTree
      , config = config
      , window = Window.init
      }
    , Cmd.map CodebaseTreeMsg codebaseTreeCmd
    )



-- UPDATE


type Msg
    = NoOp
    | WindowMsg Window.Msg
    | CodebaseTreeMsg CodebaseTree.Msg


type OutMsg
    = None


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        CodebaseTreeMsg codebaseTreeMsg ->
            let
                ( codebaseTree, codebaseTreeCmd, _ ) =
                    CodebaseTree.update model.config codebaseTreeMsg model.codebaseTree

                ( m, cmd_ ) =
                    ( { model | codebaseTree = codebaseTree }
                    , Cmd.map CodebaseTreeMsg codebaseTreeCmd
                    )
            in
            ( m, cmd_, None )

        {-
           case outMsg of
                  CodebaseTree.OpenDefinition ref ->
                      let
                          navCmd =
                              navigateToCode appContext context (Route.definition model.config.perspective ref)

                          -- Close the sidebar when opening items on mobile
                          m_ =
                              if m.sidebarToggled then
                                  { m | sidebarToggled = False }

                              else
                                  m
                      in
                      ( m_, Cmd.batch [ cmd, cmd_, navCmd ] )
               _ ->
                 ( m, cmd_ )
        -}
        WindowMsg wMsg ->
            let
                ( window, wCmd ) =
                    Window.update wMsg model.window
            in
            ( { model | window = window }, Cmd.map WindowMsg wCmd, None )

        _ ->
            ( model, Cmd.none, None )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map WindowMsg (Window.subscriptions model.window)



-- VIEW


titlebarLeft : WorkspaceContext -> List (Html Msg)
titlebarLeft workspaceContext =
    let
        projectName =
            ProjectName.toString workspaceContext.projectName

        branchRef =
            BranchRef.toString workspaceContext.branchRef
    in
    [ Button.iconThenLabelThenIcon NoOp Icon.pencilRuler projectName Icon.caretDown
        |> Button.small
        |> Button.view
    , Button.iconThenLabelThenIcon NoOp Icon.branch branchRef Icon.caretDown
        |> Button.small
        |> Button.view
    ]


titlebarRight : List (Html Msg)
titlebarRight =
    [ Button.icon NoOp Icon.search
        |> Button.small
        |> Button.subdued
        |> Button.view
    , Button.icon NoOp Icon.windowSplit
        |> Button.small
        |> Button.subdued
        |> Button.view
    ]


viewLeftSidebar : CodebaseTree.Model -> List (Html Msg)
viewLeftSidebar codebaseTree =
    [ div [ class "inner-sidebar" ]
        [ Html.map CodebaseTreeMsg
            (CodebaseTree.view { withPerspective = False } codebaseTree)
        ]
    ]


view : Model -> Browser.Document Msg
view model =
    let
        footerLeft =
            [ Button.icon NoOp Icon.leftSidebarOff
                |> Button.small
                |> Button.subdued
                |> Button.view
            ]

        footerRight =
            []
    in
    Window.window "workspace-screen"
        |> Window.withTitlebarLeft (titlebarLeft model.workspaceContext)
        |> Window.withTitlebarRight titlebarRight
        |> Window.withFooterLeft footerLeft
        |> Window.withFooterRight footerRight
        |> Window.withLeftSidebar (viewLeftSidebar model.codebaseTree)
        |> Window.withContent [ text "Pick a definition" ]
        |> Window.view WindowMsg model.window
