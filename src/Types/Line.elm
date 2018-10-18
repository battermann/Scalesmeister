module Types.Line exposing
    ( Line
    , applyFormula
    , fromScaleWithinRange
    )

import List.Extra
import Maybe.Extra
import MusicTheory.PitchClass exposing (PitchClass)
import MusicTheory.Scale exposing (Scale, toList)
import Set
import Types.Formula as Formula exposing (Direction(..), Formula)
import Types.Pitch as Pitch exposing (Pitch(..))
import Types.Range as Range exposing (Range)


type alias Line =
    List Pitch


fromScaleWithinRange : Range -> Scale -> Line
fromScaleWithinRange range scale =
    Pitch.all
        |> List.filter (\pitch -> (range |> Range.contains pitch) && (scale |> toList |> List.member (Pitch.pitchClass pitch)))


fitFormula : Int -> List Int -> List a -> Maybe (List a)
fitFormula startingIndex formula line =
    formula
        |> List.Extra.scanl (+) startingIndex
        |> List.tail
        |> Maybe.withDefault []
        |> List.map (\i -> List.Extra.getAt i line)
        |> Maybe.Extra.combine


possibleStartingIndices : Direction -> PitchClass -> Line -> List Int
possibleStartingIndices direction pitchClass line =
    let
        indices =
            line |> List.Extra.findIndices (\(Pitch n _) -> n == pitchClass)
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
    case ( fitFormula startingIndex formula line, Formula.direction formula ) of
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
                    ((line |> List.Extra.getAt head) |> Maybe.Extra.toList) ++ resultingLine


applyFormula : PitchClass -> Formula -> Line -> Line
applyFormula startingNote formula line =
    possibleStartingIndices (Formula.direction formula) startingNote (line |> List.sortBy Pitch.semitones)
        |> applyFormulaFromFirstViableIndex formula line
