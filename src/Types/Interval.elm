module Types.Interval
    exposing
        ( IntervalNumber(..)
        , IntervalQuality(..)
        , Interval
        , perfectUnison
        , minorSecond
        , majorSecond
        , minorThird
        , majorThird
        , perfectFourth
        , augmentedFourth
        , diminishedFifth
        , perfectFifth
        , augmentedFifth
        , minorSixth
        , majorSixth
        , minorSeventh
        , majorSeventh
        , quality
        , number
        , semitones
        , complementary
        , octave
        )


type IntervalNumber
    = Unison
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh
    | Octave


type IntervalQuality
    = Diminished
    | Minor
    | Perfect
    | Major
    | Augmented


type Interval
    = Interval IntervalQuality IntervalNumber Int


quality : Interval -> IntervalQuality
quality (Interval q _ _) =
    q


number : Interval -> IntervalNumber
number (Interval _ n _) =
    n


semitones : Interval -> Int
semitones (Interval _ _ n) =
    n


complementaryIntervalNumber : IntervalNumber -> IntervalNumber
complementaryIntervalNumber interval =
    case interval of
        Unison ->
            Octave

        Second ->
            Seventh

        Third ->
            Sixth

        Fourth ->
            Fifth

        Fifth ->
            Fourth

        Sixth ->
            Third

        Seventh ->
            Second

        Octave ->
            Unison


complementaryIntervalQuality : IntervalQuality -> IntervalQuality
complementaryIntervalQuality intervalQuality =
    case intervalQuality of
        Diminished ->
            Augmented

        Minor ->
            Major

        Perfect ->
            Perfect

        Major ->
            Minor

        Augmented ->
            Diminished


complementary : Interval -> Interval
complementary (Interval quality number semitones) =
    Interval (complementaryIntervalQuality quality) (complementaryIntervalNumber number) (12 - semitones)


perfectUnison : Interval
perfectUnison =
    Interval Perfect Unison 0


minorSecond : Interval
minorSecond =
    Interval Minor Second 1


majorSecond : Interval
majorSecond =
    Interval Major Second 2


minorThird : Interval
minorThird =
    Interval Minor Third 3


majorThird : Interval
majorThird =
    Interval Major Third 4


perfectFourth : Interval
perfectFourth =
    Interval Perfect Fourth 5


augmentedFourth : Interval
augmentedFourth =
    Interval Augmented Fourth 6


diminishedFifth : Interval
diminishedFifth =
    Interval Diminished Fifth 6


perfectFifth : Interval
perfectFifth =
    Interval Perfect Fifth 7


augmentedFifth : Interval
augmentedFifth =
    Interval Augmented Fifth 8


minorSixth : Interval
minorSixth =
    Interval Minor Sixth 8


majorSixth : Interval
majorSixth =
    Interval Major Sixth 9


minorSeventh : Interval
minorSeventh =
    Interval Minor Seventh 10


majorSeventh : Interval
majorSeventh =
    Interval Major Seventh 11


octave : Interval
octave =
    Interval Perfect Octave 12
