module Ucm.AppContext exposing (..)

import Code.Config
import Code.Perspective as Perspective
import Json.Decode exposing (Value)
import Lib.HttpApi as HttpApi exposing (HttpApi)
import Lib.OperatingSystem as OS exposing (OperatingSystem)
import Ucm.Api as Api
import Ucm.UcmConnectivity exposing (UcmConnectivity(..))
import Ucm.Workspace.WorkspaceContext exposing (WorkspaceContext)


type alias Assets =
    { appIcon : String
    }


type alias AppContext =
    { operatingSystem : OperatingSystem
    , basePath : String
    , api : HttpApi
    , ucmConnectivity : UcmConnectivity
    , theme : String
    , assets : Assets
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiUrl : String
    , workspaceContext : Value
    , theme : String
    , assets : Assets
    }


init : Flags -> AppContext
init flags =
    { operatingSystem = OS.fromString flags.operatingSystem
    , basePath = flags.basePath
    , api = HttpApi.httpApi False flags.apiUrl Nothing
    , ucmConnectivity = Connecting
    , theme = flags.theme
    , assets = flags.assets
    }


toCodeConfig : AppContext -> WorkspaceContext -> Code.Config.Config
toCodeConfig appContext workspaceContext =
    { operatingSystem = appContext.operatingSystem
    , perspective = Perspective.relativeRootPerspective
    , toApiEndpoint = Api.codebaseApiEndpointToEndpoint workspaceContext
    , api = appContext.api
    }
