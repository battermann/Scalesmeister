module Types.Orchestration exposing
    ( Bar(..)
    , Beamed
    , Clef(..)
    , Orchestration(..)
    , orchestrate
    )

import List.Extra
import Types.Line exposing (Line)
import Types.Note exposing (Duration(..), Note(..), Rest(..), addDurations)
import Types.Pitch as Pitch
import Types.TimeSignature exposing (NumberOfBeats(..), TimeSignature(..), durationsPerBar, grouping)
import Util exposing (Either(..))


type Clef
    = Treble
    | Bass


type alias Beamed =
    List (Either Note Rest)


type Bar
    = Bar (Maybe Clef) (List Beamed)


type Orchestration
    = Orchestration TimeSignature (List Bar)


averagePitchOfLine : Line -> Float
averagePitchOfLine line =
    (line
        |> List.map Pitch.semitones
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
                |> (\b -> ( clef, b ))
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
