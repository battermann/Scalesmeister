module Types.Formula exposing
    ( Direction(..)
    , Formula
    , direction
    , formula1
    , formula2
    , formula3
    , formula4
    , formula5
    , formulas
    , fromString
    , invert
    , serialize
    , toString
    )

import Maybe.Extra


type alias Formula =
    List Int


formulas : List Formula
formulas =
    [ [ 1 ]
    , [ -1 ]
    , [ 2 ]
    , [ -2 ]
    , formula1
    , formula1 |> invert
    , formula2
    , formula2 |> invert
    , formula3
    , formula3 |> invert
    , formula4
    , formula4 |> invert
    , formula5
    , formula5 |> invert
    ]


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


invert : Formula -> Formula
invert formula =
    formula |> List.map ((*) -1)


formula1 : Formula
formula1 =
    [ -2, -1, 2, -1 ]


formula2 : Formula
formula2 =
    [ -1, 2, -1, -2 ]


formula3 : Formula
formula3 =
    [ 2, -1, -2, -1 ]


formula4 : Formula
formula4 =
    [ -1, -2, -1, 2 ]


formula5 : Formula
formula5 =
    [ 3, -2, -2, 3 ]


formulaPartToString : String -> String -> Int -> String
formulaPartToString up down n =
    if n > 0 then
        up ++ String.fromInt (abs n)

    else
        down ++ String.fromInt (abs n)


toStringWithSymbol : String -> String -> String -> Formula -> String
toStringWithSymbol sep up down formula =
    formula
        |> List.map (formulaPartToString up down)
        |> String.join sep


toString : Formula -> String
toString =
    toStringWithSymbol " " "↑" "↓"


serialize : Formula -> String
serialize =
    toStringWithSymbol "" "+" "-"


fromString : String -> Maybe Formula
fromString str =
    let
        toStep =
            String.fromChar >> String.toInt >> Maybe.Extra.toList

        go acc chars =
            case chars of
                [] ->
                    acc

                '+' :: c :: tail ->
                    go (acc ++ toStep c) tail

                '-' :: c :: tail ->
                    go (acc ++ (toStep c |> List.map ((*) -1))) tail

                c :: tail ->
                    go (acc ++ toStep c) tail
    in
    case go [] (String.toList str |> List.filter (\c -> Char.isDigit c || c == '+' || c == '-')) of
        [] ->
            Nothing

        nonEmpty ->
            if (nonEmpty |> List.length) > 8 then
                Nothing

            else
                Just nonEmpty
