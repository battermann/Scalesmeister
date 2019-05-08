module Types.Orchestration exposing
    ( Bar(..)
    , Beamed
    , Clef(..)
    , DisplayNote(..)
    , Orchestration(..)
    , orchestrate
    )

import Dict.Any as Dict exposing (AnyDict)
import List.Extra
import MusicTheory.Letter as Letter exposing (Letter)
import MusicTheory.Octave exposing (Octave)
import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.Pitch.Spelling as Spelling exposing (PitchSpelling)
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import Types.Line exposing (Line)
import Types.Note exposing (Duration(..), Note(..), Rest(..), addDurations)
import Types.TimeSignature exposing (NumberOfBeats(..), TimeSignature(..), durationsPerBar, grouping)
import Util exposing (Either(..))


type Clef
    = Treble
    | Bass


type DisplayNote
    = DisplayNote Bool Note


type alias Beamed =
    List (Either DisplayNote Rest)


type Bar
    = Bar (Maybe Clef) (List Beamed)


type Orchestration
    = Orchestration TimeSignature (List Bar)


averagePitchOfLine : Line -> Float
averagePitchOfLine line =
    let
        sum =
            line
                |> List.map Pitch.semitones
                |> List.sum
                |> toFloat

        length =
            line |> List.length |> toFloat
    in
    sum / length


averagePitch : List Beamed -> Float
averagePitch beamedList =
    beamedList
        |> List.map
            (List.filterMap
                (\n ->
                    case n of
                        Left (DisplayNote _ (Note pitch _)) ->
                            Just pitch

                        _ ->
                            Nothing
                )
            )
        |> List.concat
        |> averagePitchOfLine


prettify : Beamed -> Beamed
prettify beamed =
    beamed
        |> List.foldr
            (\noteOrRest acc ->
                case acc of
                    [] ->
                        [ noteOrRest ]

                    h :: [] ->
                        case ( noteOrRest, h ) of
                            ( Right (Rest d1), Right (Rest d2) ) ->
                                addDurations d1 d2
                                    |> Maybe.map (\d -> [ Right (Rest d) ])
                                    |> Maybe.withDefault [ noteOrRest, h ]

                            ( Left (DisplayNote showAccidental (Note pitch d1)), Right (Rest d2) ) ->
                                addDurations d1 d2
                                    |> Maybe.map (\d -> [ Left <| DisplayNote showAccidental <| Note pitch d ])
                                    |> Maybe.withDefault [ noteOrRest, h ]

                            _ ->
                                [ noteOrRest, h ]

                    xs ->
                        noteOrRest :: xs
            )
            []


type alias PreviousAccidentals =
    AnyDict String Letter ( Accidental, Octave )


type IsEqual
    = IsEqual Bool


type SameOctave
    = SameOctave Bool


type PreviousAccidental
    = DoesNotExist
    | Exists IsEqual SameOctave


type CheckPreviousAccidentalsResult
    = IsNatural PreviousAccidental
    | SharpOrFlat PreviousAccidental


checkPreviousAccidentals : PreviousAccidentals -> PitchSpelling -> CheckPreviousAccidentalsResult
checkPreviousAccidentals dict { letter, accidental, octave } =
    case ( accidental == Natural, dict |> Dict.get letter ) of
        ( True, Just ( a, o ) ) ->
            IsNatural <| Exists (IsEqual <| a == accidental) (SameOctave <| o == octave)

        ( True, Nothing ) ->
            IsNatural DoesNotExist

        ( False, Just ( a, o ) ) ->
            SharpOrFlat <| Exists (IsEqual <| a == accidental) (SameOctave <| o == octave)

        ( False, Nothing ) ->
            SharpOrFlat DoesNotExist


pitchesToNotes : Duration -> List Pitch -> List (Either DisplayNote Rest)
pitchesToNotes duration pitches =
    let
        f prevAccs pitch =
            let
                spelling =
                    pitch |> Spelling.simple
            in
            spelling
                |> Result.map
                    (\sp ->
                        case checkPreviousAccidentals prevAccs sp of
                            IsNatural DoesNotExist ->
                                ( prevAccs |> Dict.insert sp.letter ( sp.accidental, sp.octave )
                                , Left <| DisplayNote False (Note pitch duration)
                                )

                            SharpOrFlat DoesNotExist ->
                                ( prevAccs |> Dict.insert sp.letter ( sp.accidental, sp.octave )
                                , Left <| DisplayNote True (Note pitch duration)
                                )

                            IsNatural (Exists (IsEqual False) _) ->
                                ( prevAccs |> Dict.insert sp.letter ( sp.accidental, sp.octave )
                                , Left <| DisplayNote True (Note pitch duration)
                                )

                            IsNatural (Exists (IsEqual True) (SameOctave False)) ->
                                ( prevAccs |> Dict.insert sp.letter ( sp.accidental, sp.octave )
                                , Left <| DisplayNote False (Note pitch duration)
                                )

                            SharpOrFlat (Exists (IsEqual True) (SameOctave False)) ->
                                ( prevAccs |> Dict.insert sp.letter ( sp.accidental, sp.octave )
                                , Left <| DisplayNote True (Note pitch duration)
                                )

                            SharpOrFlat (Exists (IsEqual False) _) ->
                                ( prevAccs |> Dict.insert sp.letter ( sp.accidental, sp.octave )
                                , Left <| DisplayNote True (Note pitch duration)
                                )

                            IsNatural (Exists (IsEqual True) (SameOctave True)) ->
                                ( prevAccs
                                , Left <| DisplayNote False (Note pitch duration)
                                )

                            SharpOrFlat (Exists (IsEqual True) (SameOctave True)) ->
                                ( prevAccs
                                , Left <| DisplayNote False (Note pitch duration)
                                )
                    )
                |> Result.withDefault ( prevAccs, Right <| Rest duration )
    in
    pitches
        |> List.Extra.mapAccuml f (Dict.empty Letter.toString)
        |> Tuple.second


mkBeamed : TimeSignature -> Int -> Duration -> Line -> List Beamed
mkBeamed timeSignature max dur pitches =
    List.repeat (max - (pitches |> List.length)) (Right (Rest dur))
        |> List.append (pitchesToNotes dur pitches)
        |> List.Extra.groupsOfVarying (grouping timeSignature dur)
        |> List.map prettify


mkBar : Clef -> List Beamed -> ( Clef, Bar )
mkBar currentClef beamed =
    let
        clef =
            if (beamed |> averagePitch) > 40.0 then
                Treble

            else
                Bass
    in
    beamed
        |> Bar
            (if clef == currentClef then
                Nothing

             else
                Just clef
            )
        |> (\b -> ( clef, b ))


orchestrate : TimeSignature -> Duration -> Line -> Maybe Orchestration
orchestrate timeSignature duration line =
    durationsPerBar timeSignature duration
        |> Maybe.map
            (\n ->
                line
                    |> List.Extra.greedyGroupsOf n
                    |> List.map (mkBeamed timeSignature n duration)
                    |> List.Extra.mapAccuml mkBar Treble
                    |> Tuple.second
                    |> Orchestration timeSignature
            )
        |> Maybe.andThen
            (\o ->
                case o of
                    Orchestration _ [] ->
                        Nothing

                    _ ->
                        Just o
            )
