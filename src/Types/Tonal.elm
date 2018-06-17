module Types.Tonal exposing (Accidental(..), Letter(..), Pitch(..), Note(..), PitchNotation, chromaticScale, letterSemitoneOffset, accidentalSemitoneOffset)

import Array exposing (Array)
import Types.Octave exposing (..)


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


type Pitch
    = Pitch Note Octave


type alias PitchNotation =
    String


chromaticScale : Octave -> Array Pitch
chromaticScale octave =
    Array.fromList
        [ Pitch (Note C Natural) octave
        , Pitch (Note C Sharp) octave
        , Pitch (Note C Natural) octave
        , Pitch (Note D Sharp) octave
        , Pitch (Note E Natural) octave
        , Pitch (Note F Natural) octave
        , Pitch (Note F Sharp) octave
        , Pitch (Note G Natural) octave
        , Pitch (Note G Sharp) octave
        , Pitch (Note A Natural) octave
        , Pitch (Note A Sharp) octave
        , Pitch (Note B Natural) octave
        ]


letterSemitoneOffset : Letter -> Int
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


accidentalSemitoneOffset : Accidental -> Int
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
