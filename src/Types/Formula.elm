module Types.Formula exposing
    ( Direction(..)
    , Formula
    , direction
    , filter
    , formula1
    , formula2
    , formula3
    , formula4
    , formula5
    , formulas
    , fromInputString
    , fromUrlString
    , invert
    , isValidChar
    , toInputString
    , toString
    , toUrlString
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


toInputString : Formula -> String
toInputString =
    toStringWithSymbol "" "+" "-"


toUrlString : Formula -> String
toUrlString =
    toStringWithSymbol "-" "up" "down"


isValidChar : Char -> Bool
isValidChar c =
    Char.isDigit c || c == '+' || c == '-'


filter : String -> String
filter inputString =
    let
        cleaned =
            inputString |> String.toList |> List.filter isValidChar |> String.fromList
    in
    if (cleaned |> String.endsWith "-") || (cleaned |> String.endsWith "+") then
        cleaned

    else
        cleaned |> fromInputString |> Maybe.map (List.take 8 >> toInputString) |> Maybe.withDefault cleaned


fromUrlString : String -> Maybe Formula
fromUrlString =
    String.toLower
        >> String.replace "-" ""
        >> String.replace "up" "+"
        >> String.replace "down" "-"
        >> fromInputString


fromInputString : String -> Maybe Formula
fromInputString str =
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
    case go [] (String.toList str |> List.filter isValidChar) of
        [] ->
            Nothing

        nonEmpty ->
            Just (nonEmpty |> List.take 8)
