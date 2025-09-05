module Code2.Workspace.DefinitionMatch exposing (..)

import Code.Definition.Term as Term exposing (TermSignature)
import Code.Definition.Type as Type exposing (TypeSource)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (at, field)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Decode.Helpers exposing (whenKindIs)


type alias MatchSummary sum =
    { displayName : FQN, fqn : FQN, hash : Hash, summary : sum }


type alias TermMatchSummary =
    MatchSummary TermSignature


type alias TypeMatchSummary =
    MatchSummary TypeSource


type DefinitionMatch
    = TermMatch TermMatchSummary
    | TypeMatch TypeMatchSummary
    | DataConstructorMatch TermMatchSummary
    | AbilityConstructorMatch TermMatchSummary



-- JSON DECODERS


decodeMatch_ :
    (MatchSummary sum -> DefinitionMatch)
    -> (List String -> Decode.Decoder c)
    -> (List String -> Decode.Decoder sum)
    -> Decode.Decoder DefinitionMatch
decodeMatch_ ctor catDecoder summaryDecoder =
    let
        make hash name_ fqn _ summary =
            ctor
                { hash = hash
                , fqn = fqn
                , displayName = name_
                , summary = summary
                }
    in
    Decode.succeed make
        |> requiredAt [ "definition", "hash" ] Hash.decode
        |> requiredAt [ "definition", "displayName" ] FQN.decode
        |> required "fqn" FQN.decode
        |> requiredAt [ "definition" ] (catDecoder [ "tag" ])
        |> requiredAt [ "definition" ] (summaryDecoder [ "summary" ])


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
            Decode.map termTypeByHash (at [ "contents", "namedTerm", "termHash" ] Hash.decode)

        decodeTypeMatch =
            -- TODO
            decodeMatch_ TypeMatch Type.decodeTypeCategory (\path -> Type.decodeTypeSource path [])

        decodeTermMatch =
            decodeMatch_ TermMatch Term.decodeTermCategory Term.decodeSignature

        decodeAbilityConstructorMatch =
            decodeMatch_ TermMatch Term.decodeTermCategory Term.decodeSignature

        decodeDataConstructorMatch =
            decodeMatch_ TermMatch Term.decodeTermCategory Term.decodeSignature
    in
    Decode.oneOf
        [ when decodeConstructorSuffix ((==) "AbilityConstructor") (field "contents" decodeAbilityConstructorMatch)
        , when decodeConstructorSuffix ((==) "DataConstructor") (field "contents" decodeDataConstructorMatch)
        , whenKindIs "term" (field "contents" decodeTermMatch)
        , whenKindIs "type" (field "contents" decodeTypeMatch)
        ]


decodeList : Decode.Decoder (List DefinitionMatch)
decodeList =
    Decode.list decode
