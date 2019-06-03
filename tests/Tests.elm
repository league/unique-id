module Tests exposing (tests)

import Debug exposing (toString)
import Expect exposing (equal, false, notEqual)
import Test exposing (Test, describe, test)
import Unique exposing (..)


tests : Test
tests =
    describe "Unique IDs"
        [ test "Constant is preserved" <|
            \() ->
                equal 91 <|
                    run <|
                        return 91
        , test "Two uniques in the same run are different" <|
            \() ->
                (\( x, y ) -> notEqual x y) <|
                    run <|
                        map2 Tuple.pair unique unique
        , test "But uniques in different runs are the same" <|
            \() -> equal (run unique) (run unique)
        , test "map toString works on IDs" <|
            \() ->
                false "string shouldn't be empty" <|
                    String.isEmpty <|
                        run <|
                            map toString unique
        , test "use andThen to join values" <|
            \() ->
                equal "(Id 0,15,Id 1)" <|
                    toString <|
                        run <|
                            (unique |> andThen (\x -> unique |> andThen (\y -> return ( x, 15, y ))))
        , test "Use map3 to join values" <|
            \() ->
                equal "[Id 0,Id 1,Id 2,Id 1]" <|
                    toString <|
                        run <|
                            map3 (\x y z -> [ x, y, z, y ]) unique unique unique
        , test "Use replicate to create list of uniques" <|
            \() ->
                equal "[Id 0,Id 1,Id 2,Id 3,Id 4]" <|
                    toString <|
                        run <|
                            replicate 5 unique
        , test "Use replicate to create list of constants" <|
            \() ->
                equal "[9,9,9,9,9]" <|
                    toString <|
                        run <|
                            replicate 5 <|
                                return 9
        , test "Use sequence to run through a list" <|
            \() ->
                equal [ "Ok", "Id 0", "Now", "Id 1" ] <|
                    run <|
                        sequence
                            [ return "Ok"
                            , map toString unique
                            , return "Now"
                            , map toString unique
                            ]
        ]
