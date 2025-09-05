module Code2.Workspace.DefinitionItem exposing (..)

import Code.Definition.AbilityConstructor exposing (AbilityConstructor(..), AbilityConstructorDetail)
import Code.Definition.DataConstructor exposing (DataConstructor(..), DataConstructorDetail)
import Code.Definition.Doc as Doc exposing (Doc)
import Code.Definition.Info as Info
import Code.Definition.Reference exposing (Reference)
import Code.Definition.Term as Term exposing (Term(..), TermCategory, TermDetail, TermSource)
import Code.Definition.Type as Type exposing (Type(..), TypeCategory, TypeDetail, TypeSource)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.ProjectDependency as ProjectDependency exposing (ProjectDependency)
import Json.Decode as Decode exposing (field, index)
import Lib.Decode.Helpers as DecodeH
import List.Nonempty as NEL
import Maybe.Extra as MaybeE


type alias WithDoc =
    { doc : Maybe Doc }


type alias TermDetailWithDoc =
    TermDetail WithDoc


type alias TypeDetailWithDoc =
    TypeDetail WithDoc


type DefinitionItem
    = TermItem TermDetailWithDoc
    | TypeItem TypeDetailWithDoc
      -- TODO: DataConstructorItem and AbilityConstructorItem are currently not
      -- rendered separate from TypeItem
    | DataConstructorItem DataConstructorDetail
    | AbilityConstructorItem AbilityConstructorDetail


toLib : DefinitionItem -> Maybe ProjectDependency
toLib defItem =
    let
        fqnToLib fqn =
            case fqn |> FQN.segments |> NEL.toList of
                "lib" :: _ :: "lib" :: _ ->
                    Nothing

                "lib" :: libName :: _ ->
                    Just (ProjectDependency.fromString libName)

                _ ->
                    Nothing

        toLib_ info =
            case info.namespace of
                Just n ->
                    fqnToLib n

                Nothing ->
                    let
                        f n acc =
                            if MaybeE.isJust acc then
                                acc

                            else
                                fqnToLib n
                    in
                    List.foldl f Nothing info.otherNames
    in
    case defItem of
        TermItem (Term.Term _ _ { info }) ->
            toLib_ info

        TypeItem (Type.Type _ _ { info }) ->
            toLib_ info

        AbilityConstructorItem (AbilityConstructor _ { info }) ->
            toLib_ info

        DataConstructorItem (DataConstructor _ { info }) ->
            toLib_ info


name : DefinitionItem -> FQN
name defItem =
    case defItem of
        TermItem (Term.Term _ _ { info }) ->
            info.name

        TypeItem (Type.Type _ _ { info }) ->
            info.name

        AbilityConstructorItem (AbilityConstructor _ { info }) ->
            info.name

        DataConstructorItem (DataConstructor _ { info }) ->
            info.name


hash : DefinitionItem -> Hash
hash defItem =
    case defItem of
        TermItem (Term.Term h _ _) ->
            h

        TypeItem (Type.Type h _ _) ->
            h

        AbilityConstructorItem (AbilityConstructor h _) ->
            h

        DataConstructorItem (DataConstructor h _) ->
            h


docs : DefinitionItem -> Maybe Doc
docs defItem =
    case defItem of
        TermItem (Term.Term _ _ { doc }) ->
            doc

        TypeItem (Type.Type _ _ { doc }) ->
            doc

        _ ->
            Nothing


hasDocs : DefinitionItem -> Bool
hasDocs defItem =
    MaybeE.isJust (docs defItem)


isDoc : DefinitionItem -> Bool
isDoc defItem =
    case defItem of
        TermItem (Term.Term _ Term.DocTerm _) ->
            True

        _ ->
            False



-- JSON DECODERS


decodeDocs : String -> Decode.Decoder (Maybe Doc)
decodeDocs fieldName =
    Decode.oneOf
        [ Decode.map Just (field fieldName (index 0 (index 2 Doc.decode)))
        , Decode.succeed Nothing
        ]


decodeTypeDetails :
    Decode.Decoder
        { category : TypeCategory
        , name : FQN
        , otherNames : NEL.Nonempty FQN
        , source : TypeSource
        , doc : Maybe Doc
        }
decodeTypeDetails =
    let
        make cat name_ otherNames source doc =
            { category = cat
            , doc = doc
            , name = name_
            , otherNames = otherNames
            , source = source
            }
    in
    Decode.map5 make
        (Type.decodeTypeCategory [ "defnTypeTag" ])
        (field "bestTypeName" FQN.decode)
        (field "typeNames" (DecodeH.nonEmptyList FQN.decode))
        (Type.decodeTypeSource [ "typeDefinition", "tag" ] [ "typeDefinition", "contents" ])
        (decodeDocs "typeDocs")


decodeTypes : Reference -> Decode.Decoder (List TypeDetailWithDoc)
decodeTypes ref =
    let
        makeType ( hash_, d ) =
            hash_
                |> Hash.fromString
                |> Maybe.map
                    (\h ->
                        Type h
                            d.category
                            { doc = d.doc
                            , info = Info.makeInfo ref d.name d.otherNames
                            , source = d.source
                            }
                    )

        buildTypes =
            List.map makeType >> MaybeE.values
    in
    Decode.keyValuePairs decodeTypeDetails |> Decode.map buildTypes


decodeTermDetails :
    Decode.Decoder
        { category : TermCategory
        , name : FQN
        , otherNames : NEL.Nonempty FQN
        , source : TermSource
        , doc : Maybe Doc
        }
decodeTermDetails =
    let
        make cat name_ otherNames source doc =
            { category = cat
            , name = name_
            , otherNames = otherNames
            , source = source
            , doc = doc
            }
    in
    Decode.map5 make
        (Term.decodeTermCategory [ "defnTermTag" ])
        (field "bestTermName" FQN.decode)
        (field "termNames" (DecodeH.nonEmptyList FQN.decode))
        (Term.decodeTermSource
            [ "termDefinition", "tag" ]
            [ "signature" ]
            [ "termDefinition", "contents" ]
        )
        (decodeDocs "termDocs")


decodeTerms : Reference -> Decode.Decoder (List TermDetailWithDoc)
decodeTerms ref =
    let
        makeTerm ( hash_, d ) =
            hash_
                |> Hash.fromString
                |> Maybe.map
                    (\h ->
                        Term h
                            d.category
                            { doc = d.doc
                            , info = Info.makeInfo ref d.name d.otherNames
                            , source = d.source
                            }
                    )

        buildTerms =
            List.map makeTerm >> MaybeE.values
    in
    Decode.keyValuePairs decodeTermDetails |> Decode.map buildTerms


{-| The server returns a list, but we only query for a single WorkspaceItem at a time.
-}
decodeList : Reference -> Decode.Decoder (List DefinitionItem)
decodeList ref =
    Decode.map2 List.append
        (Decode.map (List.map TermItem) (field "termDefinitions" (decodeTerms ref)))
        (Decode.map (List.map TypeItem) (field "typeDefinitions" (decodeTypes ref)))


decode : Reference -> Decode.Decoder DefinitionItem
decode ref =
    Decode.map List.head (decodeList ref)
        |> Decode.andThen
            (Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Empty list")
            )
