module Main exposing (..)

import Browser
import Ucm.App as App



-- PROGRAM


main : Program App.Flags App.Model App.Msg
main =
    Browser.application
        { init = App.init
        , update = App.update
        , view = App.view
        , subscriptions = App.subscriptions
        , onUrlRequest = App.UrlRequest
        , onUrlChange = App.UrlChange
        }
