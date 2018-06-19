module Types.Line exposing (..)

import List.Extra
import Types.Scale exposing (Scale, notes)
import Types.Pitch as Pitch exposing (..)
import Types.Note as Note exposing (..)
import Types.Range as Range exposing (..)


type alias Line =
    List Pitch


fromScale : Scale -> Line
fromScale scale =
    fromScalaWithinRange Range.piano scale


fromScalaWithinRange : Range -> Scale -> Line
fromScalaWithinRange range scale =
    Pitch.all
        |> List.filter (\pitch -> (range |> contains pitch) && (scale |> notes |> List.member (note pitch)))
