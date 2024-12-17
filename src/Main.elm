module Main exposing (..)

import Browser
import Json.Decode as Decode
import Ucm.App as App
import Ucm.AppContext as AppContext
import Ucm.Workspace.WorkspaceContext as WorkspaceContext



-- PROGRAM


appInit : AppContext.Flags -> ( App.Model, Cmd App.Msg )
appInit flags =
    let
        workspaceContext =
            flags.workspaceContext
                |> Decode.decodeValue (Decode.nullable WorkspaceContext.decode)
                |> Result.withDefault Nothing
    in
    App.init
        (AppContext.init flags)
        workspaceContext


main : Program AppContext.Flags App.Model App.Msg
main =
    Browser.document
        { init = appInit
        , view = App.view
        , update = App.update
        , subscriptions = App.subscriptions
        }
