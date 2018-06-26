module Types.Scale exposing (ScaleDef, intervals, notes, Scale(..), minorPentatonic, majorPentatonic, minorSevenDiminishedFifthPentatonic, minorSixthPentatonic, majorMinorSecondPentatonic, majorMinorSixthPentatonic)

import Types.Note as Note exposing (..)
import Types.Interval exposing (..)
import Types.Pitch as Pitch exposing (..)
import List.Extra exposing (..)


type ScaleDef
    = ScaleDef (List Interval)


intervals : ScaleDef -> List Interval
intervals (ScaleDef intervals) =
    intervals


minorPentatonic : ScaleDef
minorPentatonic =
    [ minorThird, perfectFourth, perfectFifth, minorSeventh ]
        |> ScaleDef


minorSevenDiminishedFifthPentatonic : ScaleDef
minorSevenDiminishedFifthPentatonic =
    [ minorThird, perfectFourth, diminishedFifth, minorSeventh ]
        |> ScaleDef


minorSixthPentatonic : ScaleDef
minorSixthPentatonic =
    [ minorThird, perfectFourth, perfectFifth, minorSixth ]
        |> ScaleDef


majorPentatonic : ScaleDef
majorPentatonic =
    [ majorSecond, majorThird, perfectFifth, majorSixth ]
        |> ScaleDef


majorMinorSecondPentatonic : ScaleDef
majorMinorSecondPentatonic =
    [ minorSecond, majorThird, perfectFifth, majorSixth ]
        |> ScaleDef


majorMinorSixthPentatonic : ScaleDef
majorMinorSixthPentatonic =
    [ majorSecond, majorThird, perfectFifth, minorSixth ]
        |> ScaleDef



--wholeTonePentatonic : ScaleDef
--wholeTonePentatonic =
--    [ majorThird, augmentedFourth, minorSixth, minorSeventh ]
--        |> ScaleDef


type Scale
    = Scale Note ScaleDef


root : Scale -> Note
root (Scale rootNote _) =
    rootNote


notes : Scale -> List Note
notes (Scale note (ScaleDef intervals)) =
    note :: (intervals |> List.filterMap (addIntervalToNote note))
