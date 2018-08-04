module Types.Orchestration exposing (..)

import Types.PitchClass exposing (..)
import Types.Note exposing (..)
import Types.Line exposing (..)
import List.Extra
import Types.TimeSignature exposing (TimeSignature(..), NumberOfBeats(..), durationsPerBar, grouping)
import Helpers exposing (Either(..))
import Types.Pitch as Pitch


type Clef
    = Treble
    | Bass


type alias Key =
    PitchClass


type alias Beamed =
    List (Either Note Rest)


type Bar
    = Bar (Maybe Clef) (List Beamed)


type Orchestration
    = Orchestration TimeSignature (List Bar)


averagePitchOfLine : Line -> Float
averagePitchOfLine line =
    (line
        |> List.map Pitch.semitoneOffset
        |> List.sum
        |> toFloat
    )
        / (line |> List.length |> toFloat)


averagePitch : List Beamed -> Float
averagePitch beamedList =
    beamedList
        |> List.map
            (List.filterMap
                (\n ->
                    case n of
                        Left (Note pitch _) ->
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

                            ( Left (Note pitch d1), Right (Rest d2) ) ->
                                addDurations d1 d2
                                    |> Maybe.map (\d -> [ Left (Note pitch d) ])
                                    |> Maybe.withDefault [ noteOrRest, h ]

                            _ ->
                                [ noteOrRest, h ]

                    xs ->
                        noteOrRest :: xs
            )
            []


orchestrate : TimeSignature -> Duration -> Line -> Maybe Orchestration
orchestrate timeSignature duration line =
    let
        mkBeamed : Int -> Duration -> Line -> List Beamed
        mkBeamed max duration_ line_ =
            List.repeat (max - (line_ |> List.length)) (Right (Rest duration_))
                |> List.append (line_ |> List.map (\pitch -> Left (Note pitch duration_)))
                |> List.Extra.groupsOfVarying (grouping timeSignature duration_)
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
                    |> (,) clef
    in
        durationsPerBar timeSignature duration
            |> Maybe.map
                (\n ->
                    line
                        |> List.Extra.greedyGroupsOf n
                        |> List.map (mkBeamed n duration)
                        |> List.Extra.mapAccuml mkBar Treble
                        |> Tuple.second
                        |> Orchestration timeSignature
                )
