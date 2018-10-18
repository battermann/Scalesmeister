module MusicTheory.Key exposing
    ( Key
    , MajorOrMinor(..)
    , major
    , minor
    , scale
    , tonic
    )

import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass exposing (PitchClass, pitchClass)
import MusicTheory.Scale as Scale exposing (Scale)
import MusicTheory.ScaleClass as ScaleClass


type Key
    = Key MajorOrMinor Scale


type MajorOrMinor
    = Major
    | Minor


major : PitchClass -> Key
major root =
    Key Major <| Scale.scale root ScaleClass.major


minor : PitchClass -> Key
minor root =
    Key Minor <| Scale.scale root ScaleClass.minor


scale : Key -> Scale
scale (Key _ keyScale) =
    keyScale


tonic : Key -> PitchClass
tonic key =
    Scale.root <| scale key
