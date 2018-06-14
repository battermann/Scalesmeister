module Types exposing (..)

import Array exposing (Array)


type Accidental
    = Sharp
    | Flat


type alias Octave =
    Int


type Letter
    = C
    | D
    | E
    | F
    | G
    | A
    | B


type Pitch
    = Pitch Letter (Maybe Accidental) Octave


type alias PitchNotation =
    String


type alias SampleUrl =
    String


type Row
    = Stopped (Array Pitch)
    | Playing (Array Pitch)


type alias Model =
    Maybe Row


middleOctave : Int
middleOctave =
    4


chromaticScale : Octave -> Array Pitch
chromaticScale octave =
    Array.fromList
        [ Pitch C Nothing octave
        , Pitch C (Just Sharp) octave
        , Pitch D Nothing octave
        , Pitch D (Just Sharp) octave
        , Pitch E Nothing octave
        , Pitch F Nothing octave
        , Pitch F (Just Sharp) octave
        , Pitch G Nothing octave
        , Pitch G (Just Sharp) octave
        , Pitch A Nothing octave
        , Pitch A (Just Sharp) octave
        , Pitch B Nothing octave
        ]


type alias MidiNumber =
    Int


toMidiNumber : Pitch -> MidiNumber
toMidiNumber (Pitch letter accidental octave) =
    (octave + 1) * 12 + (letterToInt letter) + (accidentalToInt accidental)


letterToInt : Letter -> Int
letterToInt letter =
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


accidentalToInt : Maybe Accidental -> Int
accidentalToInt accidental =
    case accidental of
        Just Sharp ->
            1

        Just Flat ->
            -1

        Nothing ->
            0
