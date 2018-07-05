module Types.Pitch exposing (..)

import Types.Octave as Octave exposing (..)
import Types.Note as Note exposing (..)
import Types.Interval as Interval exposing (..)
import List.Extra
import Maybe.Extra


type Pitch
    = Pitch Note Octave


type alias PitchNotation =
    String


note : Pitch -> Note
note (Pitch note _) =
    note


accidental : Pitch -> Accidental
accidental (Pitch note _) =
    Note.accidental note


all : List Pitch
all =
    Octave.all
        |> List.concatMap (\octave -> Note.all |> List.map (\note -> Pitch note octave))


semitoneOffset : Pitch -> Semitones
semitoneOffset (Pitch note octave) =
    Note.semitoneOffset note + (Octave.number octave) * 12


type alias Direction =
    Note -> Note -> Octave -> Maybe Octave


up : Note -> Note -> Octave -> Maybe Octave
up startingNote targetNote octave =
    if Note.semitoneOffset targetNote > Note.semitoneOffset startingNote then
        Just octave
    else
        octave |> Octave.add 1


down : Note -> Note -> Octave -> Maybe Octave
down startingNote targetNote octave =
    if Note.semitoneOffset targetNote > Note.semitoneOffset startingNote then
        octave |> Octave.add -1
    else
        Just octave


transpose : Pitch -> Direction -> Interval -> Maybe Pitch
transpose (Pitch note octave) dir interval =
    Note.transpose interval note
        |> Maybe.andThen
            (\targetNote ->
                dir note targetNote octave |> Maybe.map (Pitch targetNote)
            )


enharmonicEquivalent : (Pitch -> Direction -> Interval -> Maybe Pitch) -> (Pitch -> Direction -> Interval -> Maybe Pitch)
enharmonicEquivalent f =
    (\pitch dir interval -> toBestEnharmonicEquivalent pitch |> Maybe.andThen (\p -> transpose p dir interval) |> Maybe.andThen toBestEnharmonicEquivalent)



-- this is broken, needs to be fixed


toBestEnharmonicEquivalent : Pitch -> Maybe Pitch
toBestEnharmonicEquivalent (Pitch (Note letter acc) octave) =
    case acc of
        Natural ->
            Just (Pitch (Note letter acc) octave)

        Flat ->
            Just (Pitch (Note letter acc) octave)

        Sharp ->
            Just (Pitch (Note letter acc) octave)

        DoubleFlat ->
            all
                |> List.Extra.find
                    (\pitch ->
                        semitoneOffset pitch == semitoneOffset (Pitch (Note letter acc) octave) && accidental pitch == Natural
                    )
                |> Maybe.Extra.orElseLazy
                    (\() ->
                        all
                            |> List.Extra.find
                                (\pitch ->
                                    semitoneOffset pitch == semitoneOffset (Pitch (Note letter acc) octave) && accidental pitch == Flat
                                )
                    )

        DoubleSharp ->
            all
                |> List.Extra.find
                    (\pitch ->
                        semitoneOffset pitch == semitoneOffset (Pitch (Note letter acc) octave) && ((accidental pitch == Natural || accidental pitch == Sharp))
                    )
