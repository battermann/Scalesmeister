module IntervalTests exposing (..)

import Test exposing (..)
import Expect
import Types.Note exposing (..)
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
                Expect.equal (noteLetterDistance (Note C Sharp) D) 1
        , test "semitoneDistanceToTargetLetter A Flat to E should be 8" <|
            \_ ->
                Expect.equal (noteLetterDistance (Note A Flat) E) 8
        , test "addIntervalToNote C Perfect Fifth should be G" <|
            \_ ->
                Expect.equal (addIntervalToNote (Note C Natural) perfectFifth) (Just (Note G Natural))
        , test "addIntervalToNote G Perfect Fifth should be D" <|
            \_ ->
                Expect.equal (addIntervalToNote (Note G Natural) perfectFifth) (Just (Note D Natural))
        , test "addIntervalToNote G# Minor Third should be B" <|
            \_ ->
                Expect.equal (addIntervalToNote (Note G Sharp) minorThird) (Just (Note B Natural))
        , test "addIntervalToNote C Minor Third should be Eb" <|
            \_ ->
                Expect.equal (addIntervalToNote (Note C Natural) minorThird) (Just (Note E Flat))
        , test "addIntervalToNote C# Minor Third should be E" <|
            \_ ->
                Expect.equal (addIntervalToNote (Note C Sharp) minorThird) (Just (Note E Natural))
        , test "addIntervalToNote Db Minor Third should be Fb" <|
            \_ ->
                Expect.equal (addIntervalToNote (Note D Flat) minorThird) (Just (Note F Flat))
        , test "addIntervalToNote Gb Minor Second should Abb" <|
            \_ ->
                Expect.equal (addIntervalToNote (Note G Flat) minorSecond) (Just (Note A DoubleFlat))
        ]
