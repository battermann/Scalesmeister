module Types.Range exposing (Range, setLowest, setHighest, highest, lowest, contains, piano)

import Types.Pitch as Pitch exposing (Pitch(..))
import Types.Octave as Octave
import Types.PitchClass exposing (PitchClass(..), Letter(..), Accidental(..))


type Range
    = Range Pitch Pitch


highest : Range -> Pitch
highest (Range _ h) =
    h


lowest : Range -> Pitch
lowest (Range l _) =
    l


contains : Pitch -> Range -> Bool
contains pitch (Range l h) =
    Pitch.semitoneOffset l <= Pitch.semitoneOffset pitch && Pitch.semitoneOffset h >= Pitch.semitoneOffset pitch


setLowest : Pitch -> Range -> Range
setLowest pitch (Range l h) =
    if Pitch.semitoneOffset pitch >= Pitch.semitoneOffset h || not (contains pitch piano) then
        Range l h
    else
        Range pitch h


setHighest : Pitch -> Range -> Range
setHighest pitch (Range l h) =
    if Pitch.semitoneOffset pitch <= Pitch.semitoneOffset l || not (contains pitch piano) then
        Range l h
    else
        Range l pitch


piano : Range
piano =
    Range (Pitch (PitchClass A Natural) Octave.zero) (Pitch (PitchClass C Natural) Octave.eight)
