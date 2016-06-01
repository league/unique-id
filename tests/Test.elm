module Main exposing (..)

import ElmTest exposing (..)
import Html exposing (..)
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

        , test "Use map3 to join values"
          <| assertEqual "[Id 0,Id 1,Id 2,Id 1]"
          <| toString
          <| run
          <| map3 (\x y z -> [x,y,z,y]) unique unique unique

        , test "Use replicate to create list of uniques"
          <| assertEqual "[Id 0,Id 1,Id 2,Id 3,Id 4]"
          <| toString
          <| run
          <| replicate 5 unique

        , test "Use replicate to create list of constants"
          <| assertEqual "[9,9,9,9,9]"
          <| toString
          <| run
          <| replicate 5 <| return 9

        , test "Use sequence to run through a list"
          <| assertEqual ["Ok","Id 0","Now","Id 1"]
          <| run
          <| sequence [return "Ok", map toString unique,
                       return "Now", map toString unique]
        ]

main = runSuite tests

--  pre [] [text (stringRunner tests)]
