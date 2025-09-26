module Ucm.Api exposing (codebaseApiEndpointToEndpoint, pingUcm, projectBranches, projects)

import Code.BranchRef as BranchRef
import Code.CodebaseApi as CodebaseApi
import Code.Definition.Reference as Reference
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.HashQualified as HQ
import Code.Namespace.NamespaceRef as NamespaceRef
import Code.Perspective as Perspective exposing (Perspective(..))
import Code.ProjectName as ProjectName exposing (ProjectName)
import Code.Syntax as Syntax
import Code.Version as Version
import Code2.Workspace.WorkspaceContext exposing (WorkspaceContext)
import Lib.HttpApi exposing (Endpoint(..))
import Maybe.Extra as MaybeE
import Regex
import Url.Builder exposing (QueryParameter, int, string)


{-| Check connectivity to UCM by using the projects endpoint
-}
pingUcm : Endpoint
pingUcm =
    GET
        { path = [ "projects" ]
        , queryParams = []
        }


projects : Maybe String -> Endpoint
projects query =
    let
        queryParams =
            case query of
                Just q ->
                    [ string "query" q ]

                Nothing ->
                    []
    in
    GET
        { path = [ "projects" ]
        , queryParams = queryParams
        }


projectBranches : ProjectName -> Maybe String -> Endpoint
projectBranches projectName query =
    let
        queryParams =
            case query of
                Just q ->
                    [ string "query" q ]

                Nothing ->
                    []
    in
    GET
        { path = [ "projects", ProjectName.toApiString projectName, "branches" ]
        , queryParams = queryParams
        }


codebaseApiEndpointToEndpoint : WorkspaceContext -> CodebaseApi.CodebaseEndpoint -> Endpoint
codebaseApiEndpointToEndpoint context cbEndpoint =
    let
        base =
            baseCodePathFromContext context

        constructorSuffixRegex =
            Maybe.withDefault Regex.never (Regex.fromString "#[ad]\\d$")

        withoutConstructorSuffix h =
            h
                |> Hash.toString
                |> Regex.replace constructorSuffixRegex (always "")

        refQueryParams ref =
            case Reference.hashQualified ref of
                HQ.NameOnly fqn ->
                    [ string "name" (FQN.toApiUrlString fqn) ]

                HQ.HashOnly h ->
                    [ string "name" (withoutConstructorSuffix h) ]

                HQ.HashQualified _ h ->
                    [ string "name" (withoutConstructorSuffix h) ]

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

        CodebaseApi.Dependencies { ref } ->
            GET
                { path = base ++ [ "getDefinitionDependencies" ]
                , queryParams = refQueryParams ref
                }

        CodebaseApi.Dependents { ref } ->
            GET
                { path = base ++ [ "getDefinitionDependents" ]
                , queryParams = refQueryParams ref
                }

        CodebaseApi.Definition { perspective, ref } ->
            [ string "names" (refToString ref) ]
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
