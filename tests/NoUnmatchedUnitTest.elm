module NoUnmatchedUnitTest exposing (all)

import Elm.Type as Type
import NoUnmatchedUnit exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Test exposing (ExpectedError)
import Review.Test.Dependencies
import Test exposing (Test, describe, test)
import Test.Extra


all : Test
all =
    describe "NoUnmatchedUnit"
        [ test "should not report an error when a () is matched" <|
            \() ->
                [ """
module A exposing (..)
foo : () -> Int
foo () =
    3
    """, """
module B exposing (..)
foo : () -> Int -> Int
foo () x =
    x
    """, """
module C exposing (..)
foo : ((), Int) -> Int
foo ((), x) =
    x
    """ ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a () is not matched" <|
            \() ->
                """
module A exposing (..)
foo : () -> Int
foo _ =
    3
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ expectedErrorWithFix """
module A exposing (..)
foo : () -> Int
foo () =
    3
"""
                        ]
        , test "should report an error when a () is not matched with multiple args" <|
            \() ->
                """
module A exposing (..)
foo3 : () -> () -> Int -> Int
foo3 _ _ x =
    x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ expectedErrorWithFix """
module A exposing (..)
foo3 : () -> () -> Int -> Int
foo3 () _ x =
    x
"""
                            |> Review.Test.atExactly
                                { start = { row = 4, column = 6 }
                                , end = { row = 4, column = 7 }
                                }
                        , expectedErrorWithFix """
module A exposing (..)
foo3 : () -> () -> Int -> Int
foo3 _ () x =
    x
"""
                            |> Review.Test.atExactly
                                { start = { row = 4, column = 8 }
                                , end = { row = 4, column = 9 }
                                }
                        ]
        , test "should report an error when a () is not matched within tuples" <|
            \() ->
                """
module A exposing (..)
foo2 : ((), Int) -> Int
foo (_, x) =
    x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ expectedErrorWithFix """
module A exposing (..)
foo2 : ((), Int) -> Int
foo ((), x) =
    x
"""
                        ]
        , test "should report when _ is used with a dependency that takes a lazy lambda" <|
            \() ->
                """
module A exposing (..)
import Test exposing (test)
tests =
    describe
        [ test "1 + 1 = 2" (\\_ -> Expect.equal 2 (1 + 1))
        ]
"""
                    |> Review.Test.runWithProjectData testProject rule
                    |> Review.Test.expectErrors
                        [ expectedErrorWithFix
                            """
module A exposing (..)
import Test exposing (test)
tests =
    describe
        [ test "1 + 1 = 2" (\\() -> Expect.equal 2 (1 + 1))
        ]
"""
                        ]
        , test "should report when _ is used with a left pizza and dependency that takes a lazy lambda" <|
            \() ->
                """
module A exposing (..)
import Test exposing (test)
tests =
    describe
        [ test "1 + 1 = 2" <|
            \\_ -> Expect.equal 2 (1 + 1)
        ]
"""
                    |> Review.Test.runWithProjectData testProject rule
                    |> Review.Test.expectErrors
                        [ expectedErrorWithFix
                            """
module A exposing (..)
import Test exposing (test)
tests =
    describe
        [ test "1 + 1 = 2" <|
            \\() -> Expect.equal 2 (1 + 1)
        ]
"""
                        ]
        , test "should report when _ is used with a right pizza and dependency that takes a lazy lambda" <|
            \() ->
                """
module A exposing (..)
import Test exposing (test)
tests =
    describe
        [ (\\_ -> Expect.equal 2 (1 + 1))
           |> test "1 + 1 = 2"
        ]
"""
                    |> Review.Test.runWithProjectData testProject rule
                    |> Review.Test.expectErrors
                        [ expectedErrorWithFix
                            """
module A exposing (..)
import Test exposing (test)
tests =
    describe
        [ (\\() -> Expect.equal 2 (1 + 1))
           |> test "1 + 1 = 2"
        ]
"""
                        ]
        ]


expectedErrorWithFix : String -> ExpectedError
expectedErrorWithFix whenFixed =
    Review.Test.error
        { message = "Use the Unit pattern \"()\" instead of ignoring it with \"_\"."
        , details =
            [ "Being explicit about matching the Unit pattern makes it easier for readers to see that there's no other expected value for the argument."
            , "It also will cause a compile error if the parameter is changed to another type, at which point you probably want to do something with it, or now ignore it."
            ]
        , under = "_"
        }
        |> Review.Test.whenFixed whenFixed


testProject : Project
testProject =
    Review.Test.Dependencies.projectWithElmCore
        |> Project.addDependency elmTestDependency


elmTestDependency : Dependency
elmTestDependency =
    Dependency.create "elm-explorations/test"
        (Test.Extra.createElmJson elmTestJson)
        [ { name = "Test"
          , comment = ""
          , unions = []
          , aliases = []
          , values =
                [ { name = "test"
                  , comment = ""
                  , tipe =
                        Type.Lambda
                            (Type.Type "String" [])
                            (Type.Lambda
                                (Type.Lambda (Type.Tuple []) (Type.Type "Expectation" []))
                                (Type.Type "Test" [])
                            )
                  }
                ]
          , binops = []
          }
        ]


elmTestJson : String
elmTestJson =
    """
{
  "type": "package",
  "name": "elm-explorations/test",
  "summary": "Summary",
  "license": "MIT",
  "version": "1.0.0",
  "exposed-modules": [
    "Test"
  ],
  "elm-version": "0.19.0 <= v < 0.20.0",
  "dependencies": {
    "elm/core": "1.0.0 <= v < 2.0.0"
  },
  "test-dependencies": {}
}
"""
