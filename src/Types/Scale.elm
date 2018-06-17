module Types.Scale exposing (ScaleDef, intervals)

import Types.Pitch exposing (..)
import Types.Interval exposing (..)


type ScaleDef
    = ScaleDef (List Interval)


intervals : ScaleDef -> List Interval
intervals (ScaleDef intervals) =
    intervals


minorPentatonic : ScaleDef
minorPentatonic =
    [ minorThird, perfectFourth, perfectFifth, minorSeventh ]
        |> ScaleDef
