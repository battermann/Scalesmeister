module Types.TimeSignature
    exposing
        ( BeatDuration(..)
        , TimeSignature(..)
        , NumberOfBeats(..)
        , durationsPerBar
        , numberOfBeatsToInt
        , beatDurationToInt
        , grouping
        , setDuration
        , setNumberOfBeats
        , numberOfBeats
        , beatDuration
        , timeSignatureToString
        , durationGte
        )

import Types.Note as Note exposing (Note(..), Duration(..), Rest(..), Altered(..))
import Ratio exposing (over, Rational(..), divide, split, divideIntBy)


type BeatDuration
    = Half
    | Quarter
    | Eighth


type TimeSignature
    = TimeSignature NumberOfBeats BeatDuration


type NumberOfBeats
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Nine
    | Twelve


setNumberOfBeats : NumberOfBeats -> TimeSignature -> TimeSignature
setNumberOfBeats numBeats (TimeSignature _ duration) =
    TimeSignature numBeats duration


setDuration : BeatDuration -> TimeSignature -> TimeSignature
setDuration duration (TimeSignature numBeats _) =
    TimeSignature numBeats duration


durationsPerBar : TimeSignature -> Duration -> Maybe Int
durationsPerBar timeSignature duration =
    case divide (sixteenthPerBar2 timeSignature) (Note.toSixteenthNotes duration) |> split of
        ( x, 1 ) ->
            Just x

        _ ->
            Nothing


numberOfBeats : TimeSignature -> NumberOfBeats
numberOfBeats (TimeSignature numBeats _) =
    numBeats


beatDuration : TimeSignature -> BeatDuration
beatDuration (TimeSignature _ duration) =
    duration


numberOfBeatsToInt : NumberOfBeats -> Int
numberOfBeatsToInt numberOfBeats =
    case numberOfBeats of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Nine ->
            9

        Twelve ->
            12


beatDurationToInt : BeatDuration -> Int
beatDurationToInt beatDuration =
    case beatDuration of
        Half ->
            2

        Quarter ->
            4

        Eighth ->
            8


numberOfSixteenth : BeatDuration -> Int
numberOfSixteenth beatDuration =
    case beatDuration of
        Half ->
            8

        Quarter ->
            4

        Eighth ->
            2


sixteenthPerBar : TimeSignature -> Int
sixteenthPerBar (TimeSignature numBeats beatDuration) =
    numberOfBeatsToInt numBeats * numberOfSixteenth beatDuration


sixteenthPerBar2 : TimeSignature -> Rational
sixteenthPerBar2 (TimeSignature numBeats beatDuration) =
    over (numberOfSixteenth beatDuration * numberOfBeatsToInt numBeats) 1


durationGte : BeatDuration -> BeatDuration -> Bool
durationGte lhs rhs =
    numberOfSixteenth lhs >= numberOfSixteenth rhs


timeSignatureToString : TimeSignature -> String
timeSignatureToString (TimeSignature numBeats duration) =
    (numBeats |> numberOfBeatsToInt |> toString) ++ "/" ++ (duration |> beatDurationToInt |> toString)


grouping : TimeSignature -> Duration -> List Int
grouping (TimeSignature numBeats beatDuration) duration =
    case ( numBeats, beatDuration, duration ) of
        ( Four, Quarter, Note.Eighth None ) ->
            List.repeat 2 4

        ( Three, Eighth, Note.Eighth None ) ->
            [ 3 ]

        ( Six, Eighth, Note.Eighth None ) ->
            List.repeat 2 3

        ( Nine, Eighth, Note.Eighth None ) ->
            List.repeat 3 3

        ( Twelve, Eighth, Note.Eighth None ) ->
            List.repeat 4 3

        ( Five, Eighth, Note.Eighth None ) ->
            [ 3, 2 ]

        ( Seven, Eighth, Note.Eighth None ) ->
            [ 4, 3 ]

        ( _, Quarter, Note.Eighth None ) ->
            List.repeat (numberOfBeatsToInt numBeats) 2

        ( _, Quarter, Note.Eighth Triplet ) ->
            List.repeat (numberOfBeatsToInt numBeats) 3

        ( _, Half, Note.Eighth None ) ->
            List.repeat (numberOfBeatsToInt numBeats * 2) 4

        ( _, _, Note.Eighth None ) ->
            List.repeat (numberOfBeatsToInt numBeats * 4) 2

        ( _, Quarter, Note.Sixteenth ) ->
            List.repeat (numberOfBeatsToInt numBeats) 4

        ( _, Half, Note.Sixteenth ) ->
            List.repeat (numberOfBeatsToInt numBeats * 2) 4

        _ ->
            List.repeat (divideIntBy (sixteenthPerBar (TimeSignature numBeats beatDuration)) (Note.toSixteenthNotes duration) |> Ratio.round) 1
