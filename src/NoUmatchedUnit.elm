module NoUmatchedUnit exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when a Unit (`()`) is not matched in a pattern.

    config =
        [ NoUmatchedUnit.rule
        ]


## Fail

    foo : () -> Int
    foo _ =
        3


## Success

    foo : () -> Int
    foo () =
        3


## When (not) to enable this rule

  - Nobody on your team thinks explicitly matching against the Unit makes the code
    more readable (as far as showing that there wasn't a "real" value being ignored).
  - When you feel that Unit is often used as a placeholder for a value that will
    soon be filled in later.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template mthadley/elm-review-unit/example --rules NoUmatchedUnit
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUmatchedUnit" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, declaration } ->
            case Maybe.map Node.value signature of
                Just { typeAnnotation } ->
                    visitDeclarationAndType typeAnnotation (Node.value declaration |> .arguments)

                Nothing ->
                    []

        _ ->
            []


visitDeclarationAndType : Node TypeAnnotation -> List (Node Pattern) -> List (Error {})
visitDeclarationAndType typeAnnotation arguments =
    case ( Node.value typeAnnotation, arguments ) of
        ( TypeAnnotation.Unit, [ node ] ) ->
            case Node.value node of
                Pattern.AllPattern ->
                    [ reportUnitPatternError (Node.range node) ]

                _ ->
                    []

        ( TypeAnnotation.Unit, _ ) ->
            []

        ( TypeAnnotation.Tupled typeAnnotations, [ node ] ) ->
            case Node.value node of
                Pattern.TuplePattern patterns ->
                    List.map2 (\ta p -> visitDeclarationAndType ta [ p ])
                        typeAnnotations
                        patterns
                        |> List.concat

                _ ->
                    []

        ( TypeAnnotation.FunctionTypeAnnotation leftArg rightArg, leftPattern :: restPatterns ) ->
            visitDeclarationAndType leftArg [ leftPattern ]
                ++ visitDeclarationAndType rightArg restPatterns

        ( TypeAnnotation.FunctionTypeAnnotation _ _, [] ) ->
            []

        ( TypeAnnotation.Tupled _, _ ) ->
            []

        ( TypeAnnotation.GenericType _, _ ) ->
            []

        ( TypeAnnotation.Typed _ _, _ ) ->
            []

        ( TypeAnnotation.Record _, _ ) ->
            []

        ( TypeAnnotation.GenericRecord _ _, _ ) ->
            []


reportUnitPatternError : Range -> Error {}
reportUnitPatternError range =
    Rule.errorWithFix
        { message = "Use the Unit pattern \"()\" instead of ignoring it with \"_\"."
        , details =
            [ "Being explicit about matching the Unit pattern makes it easier for readers to see that there's no other expected value for the argument."
            , "It also will cause a compile error if the parameter is changed to another type, at which point you probably want to do something with it, or now ignore it."
            ]
        }
        range
        [ Fix.replaceRangeBy range "()" ]
