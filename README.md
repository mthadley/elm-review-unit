# elm-review-unit

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to usage of Elm's "Unit" type, also known as `()`.


## Provided rules

- [`NoUnmatchedUnit`](https://package.elm-lang.org/packages/mthadley/elm-review-unit/2.0.1/NoUnmatchedUnit) - Reports when a `()` is ignored (`_`) instead of being matched.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoUnmatchedUnit
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoUnmatchedUnit.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template mthadley/elm-review-unit/example
```
