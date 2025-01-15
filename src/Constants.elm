module Constants exposing (roots, scaleToDisplayName, scales)

import Libs.SelectList as SelectList exposing (SelectList)
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.ScaleClass as ScaleClass exposing (ScaleClass)
import String.Extra


scales : SelectList ( String, ScaleClass )
scales =
    SelectList.fromLists []
        ( "minor-pentatonic", ScaleClass.minorPentatonic )
        [ ( "aeolian", ScaleClass.aeolian )
        , ( "altered", ScaleClass.altered )
        , ( "augmented", ScaleClass.augmented )
        , ( "blues", ScaleClass.blues )
        , ( "diminished-half-whole", ScaleClass.diminishedHalfToneWholeTone )
        , ( "diminished-whole-half", ScaleClass.diminishedWholeToneHalfTone )
        , ( "dorian-flat-9", ScaleClass.dorianFlat9 )
        , ( "dorian-sharp-11", ScaleClass.dorianSharp11 )
        , ( "dorian", ScaleClass.dorian )
        , ( "double-harmonic-minor", ScaleClass.doubleHarmonicMinor )
        , ( "harmonic-minor", ScaleClass.harmonicMinor )
        , ( "ionian-sharp-5", ScaleClass.ionianSharp5 )
        , ( "ionian", ScaleClass.ionian )
        , ( "leading-whole-tone", ScaleClass.leadingWholeTone )
        , ( "locrian-nat-13", ScaleClass.locrianNatural13 )
        , ( "locrian-nat-9", ScaleClass.locrianNatural9 )
        , ( "locrian", ScaleClass.locrian )
        , ( "lydian-augmented", ScaleClass.lydianAugmented )
        , ( "lydian-diminished", ScaleClass.lydianDiminished )
        , ( "lydian-dominant", ScaleClass.lydianDominant )
        , ( "lydian-minor", ScaleClass.lydianMinor )
        , ( "lydian-sharp-9", ScaleClass.lydianSharp9 )
        , ( "lydian", ScaleClass.lydian )
        , ( "major", ScaleClass.major )
        , ( "major-flat-2-pentatonic", ScaleClass.majorFlat2Pentatonic )
        , ( "major-flat-6-pentatonic", ScaleClass.majorFlat6Pentatonic )
        , ( "major-pentatonic", ScaleClass.majorPentatonic )
        , ( "melodic-minor", ScaleClass.melodicMinor )
        , ( "minor-6-pentatonic", ScaleClass.minor6Pentatonic )
        , ( "minor-7-flat-5-pentatonic", ScaleClass.minorFlat5Pentatonic )
        , ( "minor", ScaleClass.minor )
        , ( "mixolydian-flat-13", ScaleClass.mixolydianFlat13 )
        , ( "mixolydian-flat-9-flat-13", ScaleClass.mixolydianFlat9Flat13 )
        , ( "mixolydian", ScaleClass.mixolydian )
        , ( "phrygian", ScaleClass.phrygian )
        , ( "six-tone-symmetrical", ScaleClass.sixToneSymmetrical )
        , ( "whole-tone", ScaleClass.wholeTone )
        ]


scaleToDisplayName : String -> String
scaleToDisplayName =
    String.replace "flat-" "♭"
        >> String.replace "sharp-" "♯"
        >> String.replace "nat-" "♮"
        >> String.replace "-" " "
        >> String.Extra.toTitleCase


roots : SelectList PitchClass
roots =
    SelectList.fromLists
        []
        (PitchClass.pitchClass C PitchClass.natural)
        [ PitchClass.pitchClass C PitchClass.sharp
        , PitchClass.pitchClass D PitchClass.flat
        , PitchClass.pitchClass D PitchClass.natural
        , PitchClass.pitchClass D PitchClass.sharp
        , PitchClass.pitchClass E PitchClass.flat
        , PitchClass.pitchClass E PitchClass.natural
        , PitchClass.pitchClass F PitchClass.natural
        , PitchClass.pitchClass F PitchClass.sharp
        , PitchClass.pitchClass G PitchClass.flat
        , PitchClass.pitchClass G PitchClass.natural
        , PitchClass.pitchClass G PitchClass.sharp
        , PitchClass.pitchClass A PitchClass.flat
        , PitchClass.pitchClass A PitchClass.natural
        , PitchClass.pitchClass A PitchClass.sharp
        , PitchClass.pitchClass B PitchClass.flat
        , PitchClass.pitchClass B PitchClass.natural
        ]
