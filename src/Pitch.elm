module Pitch exposing (Accidental(..), Letter(..), Pitch(..), PitchNotation, chromaticScale, toPitchNotation, toPitchNotationCustomNatural)

import Array exposing (Array)
import Octave exposing (..)


type Accidental
    = Sharp
    | Flat


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


toPitchNotationCustomNatural : String -> Pitch -> PitchNotation
toPitchNotationCustomNatural natSymbol (Pitch note accidental octave) =
    let
        acc =
            case accidental of
                Just Sharp ->
                    "#"

                Just Flat ->
                    "b"

                Nothing ->
                    natSymbol
    in
        (toString note) ++ acc ++ (toString (number octave))


toPitchNotation : Pitch -> PitchNotation
toPitchNotation pitch =
    toPitchNotationCustomNatural "" pitch
