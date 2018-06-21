module Types.Formula exposing (..)


type alias Formula =
    List Int


type Direction
    = Ascending
    | Descending
    | Static


direction : Formula -> Direction
direction formula =
    let
        sum =
            List.sum formula
    in
        if sum > 0 then
            Ascending
        else if sum < 0 then
            Descending
        else
            Static
