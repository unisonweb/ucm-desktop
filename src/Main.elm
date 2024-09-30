module Main exposing (..)

import Browser
import Ucm.App as App
import Ucm.AppContext as AppContext



-- PROGRAM


main : Program AppContext.Flags App.Model App.Msg
main =
    let
        appInit flags url navKey =
            App.init (AppContext.init flags navKey) url
    in
    Browser.application
        { init = appInit
        , update = App.update
        , view = App.view
        , subscriptions = App.subscriptions
        , onUrlRequest = App.UrlRequest
        , onUrlChange = App.UrlChange
        }
