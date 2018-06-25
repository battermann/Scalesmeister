module Types.Note exposing (..)


type Accidental
    = DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp


type Letter
    = C
    | D
    | E
    | F
    | G
    | A
    | B


type Note
    = Note Letter Accidental


type alias Semitones =
    Int


letters : List Letter
letters =
    [ C, D, E, F, G, A, B ]


all : List Note
all =
    letters
        |> List.concatMap (\letter -> [ Note letter Flat, Note letter Natural, Note letter Sharp ])


semitoneOffset : Note -> Semitones
semitoneOffset (Note letter accidental) =
    (letterSemitoneOffset letter) + (accidentalSemitoneOffset accidental)


letterSemitoneOffset : Letter -> Semitones
letterSemitoneOffset letter =
    case letter of
        C ->
            0

        D ->
            2

        E ->
            4

        F ->
            5

        G ->
            7

        A ->
            9

        B ->
            11


accidentalSemitoneOffset : Accidental -> Semitones
accidentalSemitoneOffset accidental =
    case accidental of
        DoubleFlat ->
            -2

        Flat ->
            -1

        Natural ->
            0

        Sharp ->
            1

        DoubleSharp ->
            2
