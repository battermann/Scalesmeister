module MusicTheory.Internals.PitchClass.Enharmonic exposing (NaturalOrSingleAccidental(..), semitonesToNaturalOrAccidental)

import MusicTheory.Internals.PitchClass as Internal
import MusicTheory.Letter as Letter exposing (Letter(..))


type NaturalOrSingleAccidental
    = Nat Letter
    | SharpFlat Letter Letter


semitonesToNaturalOrAccidental : Int -> NaturalOrSingleAccidental
semitonesToNaturalOrAccidental semitones =
    if semitones == -3 then
        Nat A

    else if semitones == -2 then
        SharpFlat A B

    else if semitones == -1 then
        Nat B

    else if semitones == 0 then
        Nat C

    else if semitones == 1 then
        SharpFlat C D

    else if semitones == 2 then
        Nat D

    else if semitones == 3 then
        SharpFlat D E

    else if semitones == 4 then
        Nat E

    else if semitones == 5 then
        Nat F

    else if semitones == 6 then
        SharpFlat F G

    else if semitones == 7 then
        Nat G

    else if semitones == 8 then
        SharpFlat G A

    else if semitones == 9 then
        Nat A

    else if semitones == 10 then
        SharpFlat A B

    else if semitones == 11 then
        Nat B

    else if semitones == 12 then
        Nat C

    else if semitones == 13 then
        SharpFlat C D

    else if semitones == 14 then
        Nat D

    else if semitones > 14 then
        semitonesToNaturalOrAccidental (semitones - 12)

    else
        semitonesToNaturalOrAccidental (semitones + 12)
