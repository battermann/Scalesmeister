module Pitches exposing (..)

import Types exposing (..)


chromaticScaleFromC4ToB : List Pitch
chromaticScaleFromC4ToB =
    [ c4
    , cSharp4
    , d4
    , dSharp4
    , e4
    , f4
    , fSharp4
    , g4
    , gSharp4
    , a4
    , aSharp4
    , b4
    ]


c4 : Pitch
c4 =
    { note = C
    , accidental = Nothing
    , octave = 4
    }


cSharp4 : Pitch
cSharp4 =
    { note = C
    , accidental = Just Sharp
    , octave = 4
    }


d4 : Pitch
d4 =
    { note = D
    , accidental = Nothing
    , octave = 4
    }


dSharp4 : Pitch
dSharp4 =
    { note = D
    , accidental = Just Sharp
    , octave = 4
    }


e4 : Pitch
e4 =
    { note = E
    , accidental = Nothing
    , octave = 4
    }


f4 : Pitch
f4 =
    { note = F
    , accidental = Nothing
    , octave = 4
    }


fSharp4 : Pitch
fSharp4 =
    { note = F
    , accidental = Just Sharp
    , octave = 4
    }


g4 : Pitch
g4 =
    { note = G
    , accidental = Nothing
    , octave = 4
    }


gSharp4 : Pitch
gSharp4 =
    { note = G
    , accidental = Just Sharp
    , octave = 4
    }


a4 : Pitch
a4 =
    { note = A
    , accidental = Nothing
    , octave = 4
    }


aSharp4 : Pitch
aSharp4 =
    { note = A
    , accidental = Just Sharp
    , octave = 4
    }


b4 : Pitch
b4 =
    { note = B
    , accidental = Nothing
    , octave = 4
    }
