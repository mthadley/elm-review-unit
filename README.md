# elm-review-unit

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`NoUmatchedUnit`](https://package.elm-lang.org/packages/mthadley/elm-review-unit/1.0.0/NoUmatchedUnit) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoUmatchedUnit
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoUmatchedUnit.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template mthadley/elm-review-unit/example
```
