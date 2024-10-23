module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Json.Decode as Decode
import Ucm.App as App
import Ucm.AppContext as AppContext
import Ucm.Workspace.WorkspaceContext as WorkspaceContext
import Url exposing (Url)



-- PROGRAM


appInit : AppContext.Flags -> Url -> Nav.Key -> ( App.Model, Cmd App.Msg )
appInit flags url navKey =
    let
        workspaceContext =
            flags.workspaceContext
                |> Decode.decodeValue (Decode.nullable WorkspaceContext.decode)
                |> Result.withDefault Nothing
    in
    App.init
        (AppContext.init flags navKey)
        workspaceContext
        url


main : Program AppContext.Flags App.Model App.Msg
main =
    Browser.application
        { init = appInit
        , update = App.update
        , view = App.view
        , subscriptions = App.subscriptions
        , onUrlRequest = App.UrlRequest
        , onUrlChange = App.UrlChange
        }
