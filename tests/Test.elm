module Main exposing (..)

import ElmTest exposing (..)
import String
import Unique exposing (..)

tests : Test
tests = suite "Unique IDs"
        [ test "Constant is preserved"
          <| assertEqual 91
          <| run
          <| return 91

        , test "Two uniques in the same run are different"
          <| uncurry assertNotEqual
          <| run
          <| map2 (,) unique unique

        , test "But uniques in different runs are the same"
          <| assertEqual (run unique) (run unique)

        , test "map toString works on IDs"
          <| assert <| not <| String.isEmpty
          <| run <| map toString unique

        , test "use andThen to join values"
          <| assertEqual "(Id 0,15,Id 1)"
          <| toString
          <| run
          <| unique `andThen` \x -> unique `andThen` \y -> return (x,15,y)
        ]

main =
  runSuite tests
