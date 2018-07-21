module IntervalTests exposing (..)

import Test exposing (..)
import Expect
import Types.PitchClass exposing (..)
import Types.Interval exposing (..)
import Types.Octave exposing (..)


all : Test
all =
    describe "Interval operations"
        [ test "addIntervalSizeToLetter C Second should be D " <|
            \_ ->
                Expect.equal (addIntervalSizeToLetter C Second) (Just D)
        , test "addIntervalSizeToLetter G Sixth should be E " <|
            \_ ->
                Expect.equal (addIntervalSizeToLetter G Sixth) (Just E)
        , test "semitoneDistanceToTargetLetter C Sharp to D should be 1" <|
            \_ ->
                Expect.equal (noteLetterDistance (PitchClass C Sharp) D) 1
        , test "semitoneDistanceToTargetLetter A Flat to E should be 8" <|
            \_ ->
                Expect.equal (noteLetterDistance (PitchClass A Flat) E) 8
        , test "addIntervalToNote C Perfect Fifth should be G" <|
            \_ ->
                Expect.equal (transpose perfectFifth (PitchClass C Natural)) (Just (PitchClass G Natural))
        , test "addIntervalToNote G Perfect Fifth should be D" <|
            \_ ->
                Expect.equal (transpose perfectFifth (PitchClass G Natural)) (Just (PitchClass D Natural))
        , test "addIntervalToNote G# Minor Third should be B" <|
            \_ ->
                Expect.equal (transpose minorThird (PitchClass G Sharp)) (Just (PitchClass B Natural))
        , test "addIntervalToNote C Minor Third should be Eb" <|
            \_ ->
                Expect.equal (transpose minorThird (PitchClass C Natural)) (Just (PitchClass E Flat))
        , test "addIntervalToNote C# Minor Third should be E" <|
            \_ ->
                Expect.equal (transpose minorThird (PitchClass C Sharp)) (Just (PitchClass E Natural))
        , test "addIntervalToNote Db Minor Third should be Fb" <|
            \_ ->
                Expect.equal (transpose minorThird (PitchClass D Flat)) (Just (PitchClass F Flat))
        , test "addIntervalToNote Gb Minor Second should Abb" <|
            \_ ->
                Expect.equal (transpose minorSecond (PitchClass G Flat)) (Just (PitchClass A DoubleFlat))
        , test "addIntervalToNote Eb diminished fifth should Bbb" <|
            \_ ->
                Expect.equal (transpose diminishedFifth (PitchClass E Flat)) (Just (PitchClass B DoubleFlat))
        ]
