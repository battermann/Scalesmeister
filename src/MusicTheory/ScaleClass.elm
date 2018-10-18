module MusicTheory.ScaleClass exposing
    ( ScaleClass
    , aeolian
    , altered
    , alteredDoubleFlat7
    , arabian
    , augmented
    , balinese
    , blues
    , byzantine
    , chinese
    , diminishedHalfToneWholeTone
    , diminishedWholeToneHalfTone
    , dorian
    , dorianFlat9
    , dorianSharp11
    , doubleHarmonicMinor
    , egyptian
    , eightToneSpanish
    , enigmatic
    , harmonicMinor
    , hirajoshi
    , hungarianMajor
    , ichikosucho
    , ionian
    , ionianSharp5
    , kumoi
    , leadingWholeTone
    , locrian
    , locrianNatural13
    , locrianNatural9
    , lydian
    , lydianAugmented
    , lydianDiminished
    , lydianDominant
    , lydianMinor
    , lydianSharp9
    , major
    , majorFlat2Pentatonic
    , majorFlat6Pentatonic
    , majorPentatonic
    , melodicMinor
    , minor
    , minor6Pentatonic
    , minorFlat5Pentatonic
    , minorPentatonic
    , mixolydian
    , mixolydianFlat13
    , mixolydianFlat9Flat13
    , neapolitan
    , neapolitanMajor
    , neapolitanMinor
    , pelog
    , persian
    , phrygian
    , prometheus
    , prometheusNeopolitan
    , purviTheta
    , sixToneSymmetrical
    , todiTheta
    , wholeTone
    )

import MusicTheory.Internals.ScaleClass as Internal
import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)


type alias ScaleClass =
    Internal.ScaleClass



-- Modes of major


major : ScaleClass
major =
    ionian


minor : ScaleClass
minor =
    aeolian


ionian : ScaleClass
ionian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


dorian : ScaleClass
dorian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


phrygian : ScaleClass
phrygian =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


lydian : ScaleClass
lydian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


mixolydian : ScaleClass
mixolydian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


aeolian : ScaleClass
aeolian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


locrian : ScaleClass
locrian =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }



-- Modes of melodic minor


melodicMinor : ScaleClass
melodicMinor =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


dorianFlat9 : ScaleClass
dorianFlat9 =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


lydianAugmented : ScaleClass
lydianAugmented =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


lydianDominant : ScaleClass
lydianDominant =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


mixolydianFlat13 : ScaleClass
mixolydianFlat13 =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


locrianNatural9 : ScaleClass
locrianNatural9 =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


altered : ScaleClass
altered =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }



-- Modes of harmonic minor
-- (NOTE: add Superlocrian)


harmonicMinor : ScaleClass
harmonicMinor =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


locrianNatural13 : ScaleClass
locrianNatural13 =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorThird
        , sixth = Interval.minorSecond
        }


ionianSharp5 : ScaleClass
ionianSharp5 =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


dorianSharp11 : ScaleClass
dorianSharp11 =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


mixolydianFlat9Flat13 : ScaleClass
mixolydianFlat9Flat13 =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


lydianSharp9 : ScaleClass
lydianSharp9 =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }



-- Other dominant scales


blues : ScaleClass
blues =
    Internal.Hexatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorThird
        }


diminishedWholeToneHalfTone : ScaleClass
diminishedWholeToneHalfTone =
    Internal.Octatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        , seventh = Interval.majorSecond
        }


diminishedHalfToneWholeTone : ScaleClass
diminishedHalfToneWholeTone =
    Internal.Octatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        , seventh = Interval.minorSecond
        }


wholeTone : ScaleClass
wholeTone =
    Internal.Hexatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        }



-- Western pentatonic scales


majorPentatonic : ScaleClass
majorPentatonic =
    Internal.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }


minorPentatonic : ScaleClass
minorPentatonic =
    Internal.Pentatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        }



-- Synthetic scales


augmented : ScaleClass
augmented =
    Internal.Hexatonic
        { first = Interval.minorThird
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.minorThird
        }


leadingWholeTone : ScaleClass
leadingWholeTone =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


enigmatic : ScaleClass
enigmatic =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


sixToneSymmetrical : ScaleClass
sixToneSymmetrical =
    Internal.Hexatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        }


prometheus : ScaleClass
prometheus =
    Internal.Hexatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        }


prometheusNeopolitan : ScaleClass
prometheusNeopolitan =
    Internal.Hexatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        }



-- Non-Western scales


arabian : ScaleClass
arabian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


balinese : ScaleClass
balinese =
    Internal.Pentatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }


byzantine : ScaleClass
byzantine =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


chinese : ScaleClass
chinese =
    Internal.Pentatonic
        { first = Interval.majorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorThird
        }


egyptian : ScaleClass
egyptian =
    Internal.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        }


eightToneSpanish : ScaleClass
eightToneSpanish =
    Internal.Octatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        , seventh = Interval.majorSecond
        }


hirajoshi : ScaleClass
hirajoshi =
    Internal.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorThird
        , fourth = Interval.minorSecond
        }


hungarianMajor : ScaleClass
hungarianMajor =
    Internal.Heptatonic
        { first = Interval.minorThird
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


ichikosucho : ScaleClass
ichikosucho =
    Internal.Octatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        , seventh = Interval.majorSecond
        }


kumoi : ScaleClass
kumoi =
    Internal.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorThird
        , fourth = Interval.majorSecond
        }


pelog : ScaleClass
pelog =
    Internal.Pentatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorThird
        , fourth = Interval.minorSecond
        }


persian : ScaleClass
persian =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorThird
        }


purviTheta : ScaleClass
purviTheta =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


todiTheta : ScaleClass
todiTheta =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }



-- Others
-- NOTE: categorize


alteredDoubleFlat7 : ScaleClass
alteredDoubleFlat7 =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


lydianDiminished : ScaleClass
lydianDiminished =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


lydianMinor : ScaleClass
lydianMinor =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


doubleHarmonicMinor : ScaleClass
doubleHarmonicMinor =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


neapolitan : ScaleClass
neapolitan =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


neapolitanMajor : ScaleClass
neapolitanMajor =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


neapolitanMinor : ScaleClass
neapolitanMinor =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


minor6Pentatonic : ScaleClass
minor6Pentatonic =
    Internal.Pentatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        }


majorFlat6Pentatonic : ScaleClass
majorFlat6Pentatonic =
    Internal.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        }


minorFlat5Pentatonic : ScaleClass
minorFlat5Pentatonic =
    Internal.Pentatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorThird
        }


majorFlat2Pentatonic : ScaleClass
majorFlat2Pentatonic =
    Internal.Pentatonic
        { first = Interval.minorSecond
        , second = Interval.augmentedSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }
