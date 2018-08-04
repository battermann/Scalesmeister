module Types.Line exposing (..)

import List.Extra exposing (..)
import Types.Scale exposing (Scale, notes)
import Types.Pitch as Pitch exposing (Pitch(..))
import Types.PitchClass exposing (..)
import Types.Range as Range exposing (..)
import Types.Formula exposing (..)
import Maybe.Extra


type alias Line =
    List Pitch


fromScale : Scale -> Line
fromScale scale =
    fromScaleWithinRange Range.piano scale


fromScaleWithinRange : Range -> Scale -> Line
fromScaleWithinRange range scale =
    Pitch.all
        |> List.filter (\pitch -> (range |> contains pitch) && (scale |> notes |> List.member (Pitch.note pitch)))


fitFormula : Int -> List Int -> List a -> Maybe (List a)
fitFormula startingIndex formula line =
    formula
        |> List.scanl (+) startingIndex
        |> List.tail
        |> Maybe.withDefault []
        |> List.map ((!!) line)
        |> Maybe.Extra.combine


possibleStartingIndices : Direction -> PitchClass -> Line -> List Int
possibleStartingIndices direction note line =
    let
        indices =
            line |> findIndices (\(Pitch n _) -> n == note)
    in
        case direction of
            Ascending ->
                indices

            Static ->
                indices

            Descending ->
                indices |> List.reverse


fitFormulaRecursively : List Int -> List a -> List a -> Int -> List a
fitFormulaRecursively formula line acc startingIndex =
    case ( fitFormula startingIndex formula line, direction formula ) of
        ( Just nextPart, Static ) ->
            acc ++ nextPart

        ( Just nextPart, _ ) ->
            fitFormulaRecursively formula line (acc ++ nextPart) ((formula |> List.sum) + startingIndex)

        ( Nothing, _ ) ->
            acc


applyFormulaFromFirstViableIndex : List Int -> List a -> List Int -> List a
applyFormulaFromFirstViableIndex formula line startingIndices =
    case startingIndices of
        [] ->
            []

        head :: tail ->
            case fitFormulaRecursively formula line [] head of
                [] ->
                    applyFormulaFromFirstViableIndex formula line tail

                resultingLine ->
                    ((line !! head) |> Maybe.Extra.toList) ++ resultingLine


applyFormula : PitchClass -> Formula -> Line -> Line
applyFormula startingNote formula line =
    possibleStartingIndices (direction formula) startingNote (line |> List.sortBy Pitch.semitoneOffset)
        |> applyFormulaFromFirstViableIndex formula line
