module Main exposing (..)

import ElmTest exposing (..)

tests : Test
tests = suite "Unique IDs"
        [ test "test the tests"
          <| assertEqual 4
          <| 2+2
        ]

main =
  runSuite tests
