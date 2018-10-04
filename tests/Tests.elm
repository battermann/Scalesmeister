module Tests exposing (all)

import Audio
import Expect
import Libs.Ratio
import Libs.SelectList
import Main
import Ports.In
import Ports.Out
import Score
import State
import Test exposing (..)
import Types
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
    describe "This test import all modules from the project"
        [ test "This should compile and succeed" <|
            \_ ->
                Expect.equal 1 1
        ]
