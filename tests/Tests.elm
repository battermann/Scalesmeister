module Tests exposing (all)

import Expect
import Libs.Ratio
import Test exposing (..)
import Types.Formula
import Types.Interval
import Types.Line
import Types.Note
import Types.Octave
import Types.Orchestration
import Types.Pitch
import Types.PitchClass
import Types.Range
import Types.Scale
import Types.Switch
import Types.TimeSignature



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        ]
