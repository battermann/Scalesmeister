module Types.Scale
    exposing
        ( ScaleDef
        , intervals
        , notes
        , Scale(..)
        , root
        , minorPentatonic
        , majorPentatonic
        , minorSevenDiminishedFifthPentatonic
        , minorSixthPentatonic
        , majorMinorSecondPentatonic
        , majorMinorSixthPentatonic
        , ionian
        )

import Types.PitchClass as PitchClass exposing (PitchClass(..))
import Types.Interval exposing (Interval, minorThird, perfectFourth, perfectFifth, minorSeventh, majorSixth, majorSeventh, majorThird, majorSecond, minorSecond, diminishedFifth, minorSixth)


type ScaleDef
    = ScaleDef (List Interval)


intervals : ScaleDef -> List Interval
intervals (ScaleDef intervals) =
    intervals


minorPentatonic : ScaleDef
minorPentatonic =
    [ minorThird, perfectFourth, perfectFifth, minorSeventh ]
        |> ScaleDef


majorPentatonic : ScaleDef
majorPentatonic =
    [ majorSecond, majorThird, perfectFifth, majorSixth ]
        |> ScaleDef


minorSixthPentatonic : ScaleDef
minorSixthPentatonic =
    [ minorThird, perfectFourth, perfectFifth, majorSixth ]
        |> ScaleDef


majorMinorSixthPentatonic : ScaleDef
majorMinorSixthPentatonic =
    [ majorSecond, majorThird, perfectFifth, minorSixth ]
        |> ScaleDef


minorSevenDiminishedFifthPentatonic : ScaleDef
minorSevenDiminishedFifthPentatonic =
    [ minorThird, perfectFourth, diminishedFifth, minorSeventh ]
        |> ScaleDef


majorMinorSecondPentatonic : ScaleDef
majorMinorSecondPentatonic =
    [ minorSecond, majorThird, perfectFifth, majorSixth ]
        |> ScaleDef


ionian : ScaleDef
ionian =
    [ majorSecond, majorThird, perfectFourth, perfectFifth, majorSixth, majorSeventh ]
        |> ScaleDef



--wholeTonePentatonic : ScaleDef
--wholeTonePentatonic =
--    [ majorThird, augmentedFourth, minorSixth, minorSeventh ]
--        |> ScaleDef


type Scale
    = Scale PitchClass ScaleDef


root : Scale -> PitchClass
root (Scale rootNote _) =
    rootNote


notes : Scale -> List PitchClass
notes (Scale note (ScaleDef intervals)) =
    note :: (intervals |> List.filterMap (\interval -> PitchClass.transpose interval note))
