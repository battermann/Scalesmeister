module Types.Scale exposing
    ( Scale(..)
    , ScaleDef
    , intervals
    , ionian
    , majorMinorSecondPentatonic
    , majorMinorSixthPentatonic
    , majorPentatonic
    , minorPentatonic
    , minorSevenDiminishedFifthPentatonic
    , minorSixthPentatonic
    , notes
    , root
    )

import Types.Interval exposing (Interval, diminishedFifth, majorSecond, majorSeventh, majorSixth, majorThird, minorSecond, minorSeventh, minorSixth, minorThird, perfectFifth, perfectFourth)
import Types.PitchClass as PitchClass exposing (PitchClass(..))


type ScaleDef
    = ScaleDef (List Interval)


intervals : ScaleDef -> List Interval
intervals (ScaleDef xs) =
    xs


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
notes (Scale note (ScaleDef xs)) =
    note :: (xs |> List.filterMap (\interval -> PitchClass.transpose interval note))
