module Ucm.AppContext exposing (..)

import Code.Config
import Code.Perspective as Perspective
import Http
import Json.Decode exposing (Value)
import Lib.HttpApi as HttpApi exposing (HttpApi)
import Lib.OperatingSystem as OS exposing (OperatingSystem)
import Ucm.Api as Api
import Ucm.Workspace.WorkspaceContext exposing (WorkspaceContext)


type UCMConnectivity
    = Connected
    | NotConnected Http.Error


type alias AppContext =
    { operatingSystem : OperatingSystem
    , basePath : String
    , api : HttpApi
    , ucmConnected : UCMConnectivity
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiUrl : String
    , workspaceContext : Value
    }


init : Flags -> AppContext
init flags =
    { operatingSystem = OS.fromString flags.operatingSystem
    , basePath = flags.basePath
    , api = HttpApi.httpApi False flags.apiUrl Nothing
    , ucmConnected = Connected
    }


toCodeConfig : AppContext -> WorkspaceContext -> Code.Config.Config
toCodeConfig appContext workspaceContext =
    { operatingSystem = appContext.operatingSystem
    , perspective = Perspective.relativeRootPerspective
    , toApiEndpoint = Api.codebaseApiEndpointToEndpoint workspaceContext
    , api = appContext.api
    }
