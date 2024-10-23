module Ucm.Api exposing (codebaseApiEndpointToEndpoint, projectBranches, projects)

import Code.BranchRef as BranchRef
import Code.CodebaseApi as CodebaseApi
import Code.Definition.Reference as Reference
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.HashQualified as HQ
import Code.Namespace.NamespaceRef as NamespaceRef
import Code.Perspective as Perspective exposing (Perspective(..))
import Code.Syntax as Syntax
import Code.Version as Version
import Lib.HttpApi exposing (Endpoint(..))
import Maybe.Extra as MaybeE
import Regex
import Ucm.ProjectName as ProjectName exposing (ProjectName)
import Ucm.Workspace.WorkspaceContext exposing (WorkspaceContext)
import Url.Builder exposing (QueryParameter, int, string)


projects : Endpoint
projects =
    GET
        { path = [ "projects" ]
        , queryParams = []
        }


projectBranches : ProjectName -> Endpoint
projectBranches projectName =
    GET
        { path = [ "projects", ProjectName.toApiString projectName, "branches" ]
        , queryParams = []
        }


codebaseApiEndpointToEndpoint : WorkspaceContext -> CodebaseApi.CodebaseEndpoint -> Endpoint
codebaseApiEndpointToEndpoint context cbEndpoint =
    let
        base =
            baseCodePathFromContext context
    in
    case cbEndpoint of
        CodebaseApi.Find { perspective, withinFqn, limit, sourceWidth, query } ->
            let
                params =
                    case withinFqn of
                        Just fqn ->
                            [ toRootBranch (Perspective.rootPerspective perspective)
                            , Just (relativeTo fqn)
                            ]
                                |> MaybeE.values

                        Nothing ->
                            perspectiveToQueryParams perspective

                width =
                    case sourceWidth of
                        Syntax.Width w ->
                            w
            in
            GET
                { path = base ++ [ "find" ]
                , queryParams =
                    [ int "limit" limit
                    , int "renderWidth" width
                    , string "query" query
                    ]
                        ++ params
                }

        CodebaseApi.Browse { perspective, ref } ->
            let
                namespace_ =
                    ref
                        |> Maybe.map NamespaceRef.toString
                        |> Maybe.map (string "namespace")
                        |> Maybe.map (\qp -> [ qp ])
            in
            GET
                { path = base ++ [ "list" ]
                , queryParams = Maybe.withDefault [] namespace_ ++ perspectiveToQueryParams perspective
                }

        CodebaseApi.Definition { perspective, ref } ->
            let
                constructorSuffixRegex =
                    Maybe.withDefault Regex.never (Regex.fromString "#[ad]\\d$")

                withoutConstructorSuffix h =
                    h
                        |> Hash.toString
                        |> Regex.replace constructorSuffixRegex (always "")

                refToString r =
                    case Reference.hashQualified r of
                        HQ.NameOnly fqn ->
                            -- Using plain `toString` here because percentEncoded is added in elm/url's query param builder below
                            fqn |> FQN.toString

                        HQ.HashOnly h ->
                            withoutConstructorSuffix h

                        HQ.HashQualified _ h ->
                            withoutConstructorSuffix h
            in
            [ refToString ref ]
                |> List.map (string "names")
                |> (\names -> GET { path = base ++ [ "getDefinition" ], queryParams = names ++ perspectiveToQueryParams perspective })

        CodebaseApi.Summary { perspective, ref } ->
            let
                hqPath hq =
                    case hq of
                        HQ.NameOnly fqn ->
                            -- TODO: Not really valid...
                            ( [ "by-name", FQN.toApiUrlString fqn ], [] )

                        HQ.HashOnly h ->
                            ( [ "by-hash", Hash.toApiUrlString h ], [] )

                        HQ.HashQualified fqn h ->
                            ( [ "by-hash", Hash.toApiUrlString h ], [ string "name" (FQN.toApiUrlString fqn) ] )

                ( path, query ) =
                    case ref of
                        Reference.TermReference hq ->
                            let
                                ( p, q ) =
                                    hqPath hq
                            in
                            ( [ "definitions", "terms" ] ++ p ++ [ "summary" ], q )

                        Reference.TypeReference hq ->
                            let
                                ( p, q ) =
                                    hqPath hq
                            in
                            ( [ "definitions", "types" ] ++ p ++ [ "summary" ], q )

                        Reference.AbilityConstructorReference hq ->
                            let
                                ( p, q ) =
                                    hqPath hq
                            in
                            ( [ "definitions", "terms" ] ++ p ++ [ "summary" ], q )

                        Reference.DataConstructorReference hq ->
                            let
                                ( p, q ) =
                                    hqPath hq
                            in
                            ( [ "definitions", "terms" ] ++ p ++ [ "summary" ], q )
            in
            GET
                { path = base ++ path
                , queryParams = query ++ perspectiveToQueryParams perspective
                }


baseCodePathFromContext : WorkspaceContext -> List String
baseCodePathFromContext { projectName, branchRef } =
    let
        name =
            ProjectName.toApiString projectName
    in
    case branchRef of
        BranchRef.ReleaseBranchRef v ->
            [ "projects", name, "releases", Version.toString v ]

        _ ->
            [ "projects", name, "branches", BranchRef.toApiUrlString branchRef ]



-- QUERY PARAMS ---------------------------------------------------------------


perspectiveToQueryParams : Perspective -> List QueryParameter
perspectiveToQueryParams perspective =
    case perspective of
        Root p ->
            MaybeE.values [ toRootBranch p.root ]

        Namespace d ->
            [ toRootBranch d.root, Just (relativeTo d.fqn) ] |> MaybeE.values


toRootBranch : Perspective.RootPerspective -> Maybe QueryParameter
toRootBranch rootPerspective =
    case rootPerspective of
        Perspective.Relative ->
            Nothing

        Perspective.Absolute h ->
            Just (rootBranch h)


rootBranch : Hash -> QueryParameter
rootBranch hash =
    string "rootBranch" (hash |> Hash.toString)


relativeTo : FQN -> QueryParameter
relativeTo fqn =
    string "relativeTo" (fqn |> FQN.toString)
