module Code2.Workspace.DefinitionMatch exposing (..)

import Code.Definition.Term as Term exposing (TermCategory, TermSignature)
import Code.Definition.Type as Type exposing (TypeCategory, TypeSource)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (at, field)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Decode.Helpers exposing (whenKindIs)


type alias MatchSummary cat sum =
    { displayName : FQN
    , fqn : FQN
    , category : cat
    , hash : Hash
    , summary : sum
    }


type alias TermMatchSummary =
    MatchSummary TermCategory TermSignature


type alias TypeMatchSummary =
    MatchSummary TypeCategory TypeSource


type DefinitionMatch
    = TermMatch TermMatchSummary
    | TypeMatch TypeMatchSummary
    | DataConstructorMatch TermMatchSummary
    | AbilityConstructorMatch TermMatchSummary



-- HELPERS


displayName : DefinitionMatch -> FQN
displayName defItem =
    case defItem of
        TermMatch sum ->
            sum.displayName

        TypeMatch sum ->
            sum.displayName

        DataConstructorMatch sum ->
            sum.displayName

        AbilityConstructorMatch sum ->
            sum.displayName


fqn : DefinitionMatch -> FQN
fqn defItem =
    case defItem of
        TermMatch sum ->
            sum.fqn

        TypeMatch sum ->
            sum.fqn

        DataConstructorMatch sum ->
            sum.fqn

        AbilityConstructorMatch sum ->
            sum.fqn



-- JSON DECODERS


decodeMatch_ :
    (MatchSummary cat sum -> DefinitionMatch)
    -> (List String -> Decode.Decoder cat)
    -> Decode.Decoder sum
    -> Decode.Decoder DefinitionMatch
decodeMatch_ ctor catDecoder summaryDecoder =
    let
        make hash name_ fqn_ category summary =
            ctor
                { hash = hash
                , fqn = fqn_
                , displayName = name_
                , category = category
                , summary = summary
                }
    in
    Decode.succeed make
        |> requiredAt [ "definition", "hash" ] Hash.decode
        |> requiredAt [ "definition", "displayName" ] FQN.decode
        |> required "fqn" FQN.decode
        |> requiredAt [ "definition" ] (catDecoder [ "tag" ])
        |> requiredAt [ "definition", "summary" ] summaryDecoder


decode : Decode.Decoder DefinitionMatch
decode =
    let
        termTypeByHash hash =
            if Hash.isAbilityConstructorHash hash then
                "AbilityConstructor"

            else if Hash.isDataConstructorHash hash then
                "DataConstructor"

            else
                "Term"

        decodeConstructorSuffix =
            Decode.map termTypeByHash (at [ "definition", "hash" ] Hash.decode)

        decodeTypeMatch =
            decodeMatch_ TypeMatch Type.decodeTypeCategory (Type.decodeTypeSource [ "tag" ] [ "contents" ])

        decodeTermMatch =
            decodeMatch_ TermMatch Term.decodeTermCategory (Term.decodeSignature [ "contents" ])

        decodeAbilityConstructorMatch =
            decodeMatch_ TermMatch Term.decodeTermCategory (Term.decodeSignature [ "contents" ])

        decodeDataConstructorMatch =
            decodeMatch_ TermMatch Term.decodeTermCategory (Term.decodeSignature [ "contents" ])
    in
    Decode.oneOf
        [ when decodeConstructorSuffix ((==) "AbilityConstructor") (field "definition" decodeAbilityConstructorMatch)
        , when decodeConstructorSuffix ((==) "DataConstructor") (field "definition" decodeDataConstructorMatch)
        , whenKindIs "term" decodeTermMatch
        , whenKindIs "type" decodeTypeMatch
        ]


decodeList : Decode.Decoder (List DefinitionMatch)
decodeList =
    Decode.list decode
