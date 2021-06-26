module NoUmatchedUnit exposing (rule)

{-|

@docs rule

-}

import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ NoUmatchedUnit.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template mthadley/elm-review-unit/example --rules NoUmatchedUnit
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUmatchedUnit" ()
        -- Add your visitors
        |> Rule.fromModuleRuleSchema
