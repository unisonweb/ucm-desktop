module Ucm.AppContext exposing (..)

import Browser.Navigation as Nav
import Code.Config
import Code.Perspective as Perspective
import Json.Decode exposing (Value)
import Lib.HttpApi as HttpApi exposing (HttpApi)
import Lib.OperatingSystem as OS exposing (OperatingSystem)
import Ucm.Api as Api
import Ucm.Workspace.WorkspaceContext exposing (WorkspaceContext)


type alias AppContext =
    { operatingSystem : OperatingSystem
    , basePath : String
    , api : HttpApi
    , navKey : Nav.Key
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiUrl : String
    , workspaceContext : Value
    }


init : Flags -> Nav.Key -> AppContext
init flags navKey =
    { operatingSystem = OS.fromString flags.operatingSystem
    , basePath = flags.basePath
    , api = HttpApi.httpApi False flags.apiUrl Nothing
    , navKey = navKey
    }


toCodeConfig : AppContext -> WorkspaceContext -> Code.Config.Config
toCodeConfig appContext workspaceContext =
    { operatingSystem = appContext.operatingSystem
    , perspective = Perspective.relativeRootPerspective
    , toApiEndpoint = Api.codebaseApiEndpointToEndpoint workspaceContext
    , api = appContext.api
    }
