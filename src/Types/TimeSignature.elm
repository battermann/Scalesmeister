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
        )

import Types.Note as Note exposing (..)


type BeatDuration
    = Half
    | Quarter
    | Eighth
    | Sixteenth


type TimeSignature
    = TimeSignature NumberOfBeats BeatDuration


type NumberOfBeats
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven


setNumberOfBeats : NumberOfBeats -> TimeSignature -> TimeSignature
setNumberOfBeats numBeats (TimeSignature _ duration) =
    TimeSignature numBeats duration


setDuration : BeatDuration -> TimeSignature -> TimeSignature
setDuration duration (TimeSignature numBeats _) =
    TimeSignature numBeats duration


durationsPerBar : TimeSignature -> Duration -> Maybe Int
durationsPerBar timeSignature duration =
    sixteenthPerBar timeSignature /// (Note.numberOfSixteenth duration)


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


beatDurationToInt : BeatDuration -> Int
beatDurationToInt beatDuration =
    case beatDuration of
        Half ->
            2

        Quarter ->
            4

        Eighth ->
            8

        Sixteenth ->
            16


numberOfSixteenth : BeatDuration -> Int
numberOfSixteenth beatDuration =
    case beatDuration of
        Half ->
            8

        Quarter ->
            4

        Eighth ->
            2

        Sixteenth ->
            1


sixteenthPerBar : TimeSignature -> Int
sixteenthPerBar (TimeSignature numBeats beatDuration) =
    (numberOfBeatsToInt numBeats) * (numberOfSixteenth beatDuration)


div : Int -> Int -> Maybe Int
div a b =
    case a % b of
        0 ->
            Just (a // b)

        _ ->
            Nothing


(///) : Int -> Int -> Maybe Int
(///) =
    div


grouping : TimeSignature -> Duration -> List Int
grouping (TimeSignature numBeats beatDuration) duration =
    case ( numBeats, beatDuration, duration ) of
        ( Four, Quarter, Note.Eighth ) ->
            List.repeat 2 4

        ( _, Quarter, Note.Eighth ) ->
            List.repeat (numberOfBeatsToInt numBeats) 2

        _ ->
            List.repeat ((sixteenthPerBar (TimeSignature numBeats beatDuration) |> toFloat) / (Note.numberOfSixteenth duration |> toFloat) |> round) 1
