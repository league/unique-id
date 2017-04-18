module Home exposing (..)

import Main exposing (tests)
import Test.Runner.Html exposing (run, TestProgram)

main : TestProgram
main = run tests
