module Types.Orchestration exposing (..)

import Types.PitchClass exposing (..)
import Types.Note as Note exposing (..)
import Types.Line exposing (..)
import List.Extra
import Types.TimeSignature as TimeSignature exposing (TimeSignature(..), NumberOfBeats(..), durationsPerBar, grouping)
import Helpers exposing (Either(..))


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


orchestrate : TimeSignature -> Duration -> Line -> Maybe Orchestration
orchestrate timeSignature duration line =
    let
        mkBar : Int -> Duration -> Line -> Bar
        mkBar max duration line =
            List.repeat (max - (line |> List.length)) (Right (Rest duration))
                |> List.append (line |> List.map (\pitch -> Left (Note pitch duration)))
                |> List.Extra.groupsOfVarying (grouping timeSignature duration)
                |> Bar Nothing
    in
        durationsPerBar timeSignature duration
            |> Maybe.map
                (\n ->
                    line
                        |> List.Extra.greedyGroupsOf n
                        |> List.map (mkBar n duration)
                        |> Orchestration timeSignature
                )
