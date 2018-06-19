module Types.Scale exposing (ScaleDef, intervals, notes, Scale(..), minorPentatonic)

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


type Scale
    = Scale Note ScaleDef


root : Scale -> Note
root (Scale rootNote _) =
    rootNote


notes : Scale -> List Note
notes (Scale note (ScaleDef intervals)) =
    note :: (intervals |> List.filterMap (addIntervalToNote note))
