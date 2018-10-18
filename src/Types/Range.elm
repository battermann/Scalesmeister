module Types.Range exposing (Range, contains, highest, lowest, piano, setHighest, setLowest)

import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (natural)
import Types.Octave as Octave
import Types.Pitch as Pitch exposing (Pitch(..))


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
    Pitch.semitones l <= Pitch.semitones pitch && Pitch.semitones h >= Pitch.semitones pitch


setLowest : Pitch -> Range -> Range
setLowest pitch (Range l h) =
    if Pitch.semitones pitch >= Pitch.semitones h || not (contains pitch piano) then
        Range l h

    else
        Range pitch h


setHighest : Pitch -> Range -> Range
setHighest pitch (Range l h) =
    if Pitch.semitones pitch <= Pitch.semitones l || not (contains pitch piano) then
        Range l h

    else
        Range l pitch


piano : Range
piano =
    Range (Pitch (PitchClass.pitchClass A natural) Octave.zero) (Pitch (PitchClass.pitchClass C natural) Octave.eight)
