module NoUnmatchedUnit exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, ModuleRuleSchema, Rule)


{-| Reports when a Unit (`()`) is not matched in a pattern.

    config =
        [ NoUnmatchedUnit.rule
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
elm-review --template mthadley/elm-review-unit/example --rules NoUnmatchedUnit
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnmatchedUnit" initProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = \_ -> identity
            }
        |> Rule.withDependenciesProjectVisitor
            (\deps _ -> ( [], { qualifiedNameToType = collectDeps deps } ))
        |> Rule.fromProjectRuleSchema


moduleVisitor :
    ModuleRuleSchema {} ModuleContext
    -> ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withExpressionEnterVisitor
            (\node context -> ( expressionVisitor node context, context ))
        |> Rule.withSimpleDeclarationVisitor declarationVisitor


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , qualifiedNameToType : Dict String Elm.Type.Type
    }


type alias ProjectContext =
    { qualifiedNameToType : Dict String Elm.Type.Type
    }


initProjectContext : ProjectContext
initProjectContext =
    { qualifiedNameToType = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable { qualifiedNameToType } ->
            { lookupTable = lookupTable
            , qualifiedNameToType = qualifiedNameToType
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\_ -> { qualifiedNameToType = Dict.empty })


expressionVisitor : Node Expression -> ModuleContext -> List (Error {})
expressionVisitor node context =
    case Node.value node of
        Expression.OperatorApplication "<|" _ left right ->
            case Node.value (removeParensFromExpr left) of
                Expression.Application (fn :: args) ->
                    reportUnitsInFunctionCall context fn (args ++ [ right ])

                _ ->
                    reportUnitsInFunctionCall context left [ right ]

        Expression.OperatorApplication "|>" _ left right ->
            case Node.value (removeParensFromExpr right) of
                Expression.Application (fn :: args) ->
                    reportUnitsInFunctionCall context fn (args ++ [ left ])

                _ ->
                    reportUnitsInFunctionCall context right [ left ]

        Expression.Application (fn :: args) ->
            reportUnitsInFunctionCall context fn args

        _ ->
            []


reportUnitsInFunctionCall : ModuleContext -> Node Expression -> List (Node Expression) -> List (Error {})
reportUnitsInFunctionCall context fn args =
    case Node.value fn of
        Expression.FunctionOrValue _ name ->
            case lookupTypeFromNode context name fn of
                Just tipe ->
                    expressionVisitorWithType tipe (List.map removeParensFromExpr args)

                Nothing ->
                    []

        _ ->
            []


expressionVisitorWithType : Elm.Type.Type -> List (Node Expression) -> List (Error {})
expressionVisitorWithType tipe args =
    case ( tipe, args ) of
        ( Elm.Type.Lambda left ((Elm.Type.Lambda _ _) as right), (Node _ (Expression.LambdaExpression lambda)) :: restNodes ) ->
            patternVisitorWithType left lambda.args
                ++ expressionVisitorWithType right restNodes

        ( Elm.Type.Lambda _ ((Elm.Type.Lambda _ _) as right), _ :: restNodes ) ->
            expressionVisitorWithType right restNodes

        ( Elm.Type.Lambda left _, (Node _ (Expression.LambdaExpression lambda)) :: _ ) ->
            patternVisitorWithType left lambda.args

        ( Elm.Type.Lambda _ _, _ :: _ ) ->
            []

        _ ->
            []


patternVisitorWithType : Elm.Type.Type -> List (Node Pattern) -> List (Error {})
patternVisitorWithType tipe argPatterns =
    case ( tipe, List.map removeParensFromPattern argPatterns ) of
        ( Elm.Type.Tuple [], ((Node _ Pattern.AllPattern) as node) :: _ ) ->
            [ reportUnitPatternError (Node.range node) ]

        ( Elm.Type.Tuple tupleArgs, (Node _ (Pattern.TuplePattern patterns)) :: _ ) ->
            List.map2 (\ta p -> patternVisitorWithType ta [ p ])
                tupleArgs
                patterns
                |> List.concat

        ( Elm.Type.Tuple _, _ ) ->
            []

        ( Elm.Type.Type _ typeArgs, (Node _ (Pattern.NamedPattern _ patterns)) :: _ ) ->
            List.map2 (\ta p -> patternVisitorWithType ta [ p ]) typeArgs patterns
                |> List.concat

        ( Elm.Type.Lambda left ((Elm.Type.Lambda _ _) as right), node :: restNodes ) ->
            patternVisitorWithType left [ node ]
                ++ patternVisitorWithType right restNodes

        ( Elm.Type.Lambda left _, node :: _ ) ->
            patternVisitorWithType left [ node ]

        ( Elm.Type.Lambda _ _, [] ) ->
            []

        ( Elm.Type.Type _ _, _ ) ->
            []

        ( Elm.Type.Record _ _, _ ) ->
            []

        ( Elm.Type.Var _, _ ) ->
            []


lookupTypeFromNode : ModuleContext -> String -> Node Expression -> Maybe Elm.Type.Type
lookupTypeFromNode context name node =
    ModuleNameLookupTable.moduleNameFor context.lookupTable node
        |> Maybe.andThen (lookupTypeFromName context name)


lookupTypeFromName : ModuleContext -> String -> List String -> Maybe Elm.Type.Type
lookupTypeFromName context name modulePath =
    Dict.get (String.join "." modulePath ++ "." ++ name) context.qualifiedNameToType


collectDeps : Dict String Dependency -> Dict String Elm.Type.Type
collectDeps rawDeps =
    let
        collectModule : Elm.Docs.Module -> Dict String Elm.Type.Type -> Dict String Elm.Type.Type
        collectModule mod acc =
            List.foldl
                (\value subAcc ->
                    case value.tipe of
                        Elm.Type.Lambda _ _ ->
                            Dict.insert
                                (mod.name ++ "." ++ value.name)
                                value.tipe
                                subAcc

                        _ ->
                            subAcc
                )
                acc
                mod.values

        collectHelp : a -> Dependency -> Dict String Elm.Type.Type -> Dict String Elm.Type.Type
        collectHelp _ dep deps =
            Dependency.modules dep
                |> List.foldl collectModule deps
    in
    Dict.foldl collectHelp Dict.empty rawDeps


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
        ( TypeAnnotation.Unit, ((Node _ Pattern.AllPattern) as node) :: _ ) ->
            [ reportUnitPatternError (Node.range node) ]

        ( TypeAnnotation.Unit, _ ) ->
            []

        ( TypeAnnotation.Tupled typeAnnotations, (Node _ (Pattern.TuplePattern patterns)) :: _ ) ->
            List.map2 (\ta p -> visitDeclarationAndType ta [ p ]) typeAnnotations patterns
                |> List.concat

        ( TypeAnnotation.FunctionTypeAnnotation left right, pattern :: restPatterns ) ->
            visitDeclarationAndType left [ pattern ]
                ++ visitDeclarationAndType right restPatterns

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


removeParensFromExpr : Node Expression -> Node Expression
removeParensFromExpr node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            removeParensFromExpr expr

        _ ->
            node


removeParensFromPattern : Node Pattern -> Node Pattern
removeParensFromPattern node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            removeParensFromPattern pattern

        _ ->
            node
