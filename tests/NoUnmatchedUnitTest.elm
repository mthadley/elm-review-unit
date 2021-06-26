module NoUnmatchedUnitTest exposing (all)

import NoUnmatchedUnit exposing (rule)
import Review.Test exposing (ExpectedError)
import Test exposing (Test, describe, test)


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
                        [ expectedError """
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
                        [ expectedError """
module A exposing (..)
foo3 : () -> () -> Int -> Int
foo3 () _ x =
    x
"""
                            |> Review.Test.atExactly
                                { start = { row = 4, column = 6 }
                                , end = { row = 4, column = 7 }
                                }
                        , expectedError """
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
                        [ expectedError """
module A exposing (..)
foo2 : ((), Int) -> Int
foo ((), x) =
    x
"""
                        ]
        ]


expectedError : String -> ExpectedError
expectedError whenFixed =
    Review.Test.error
        { message = "Use the Unit pattern \"()\" instead of ignoring it with \"_\"."
        , details =
            [ "Being explicit about matching the Unit pattern makes it easier for readers to see that there's no other expected value for the argument."
            , "It also will cause a compile error if the parameter is changed to another type, at which point you probably want to do something with it, or now ignore it."
            ]
        , under = "_"
        }
        |> Review.Test.whenFixed whenFixed
