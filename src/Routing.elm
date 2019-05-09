module Routing exposing (Route(..), extractRoute, generateRandomUrl, mkUrl, routeParser)

import Constants
import Libs.SelectList as SelectList
import List.Extra
import Maybe.Extra
import MusicTheory.Letter as Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.PitchClass.Enharmonic as Enharmonic
import MusicTheory.PitchClass.Spelling as Spelling exposing (Accidental(..))
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass exposing (ScaleClass)
import Random
import Types
import Types.Formula as Formula exposing (Formula)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), Parser, map, parse)


type Route
    = Main PitchClass ( String, ScaleClass ) Formula PitchClass


pitchClassFromString : String -> Maybe PitchClass
pitchClassFromString str =
    let
        parseAccidental chars =
            case chars |> String.fromList of
                "-flat" ->
                    Just PitchClass.flat

                "-sharp" ->
                    Just PitchClass.sharp

                "" ->
                    Just PitchClass.natural

                _ ->
                    Nothing

        maybePitchClass =
            case str |> String.toLower |> String.toList of
                'c' :: tail ->
                    parseAccidental tail |> Maybe.map (PitchClass.pitchClass C)

                'd' :: tail ->
                    parseAccidental tail |> Maybe.map (PitchClass.pitchClass D)

                'e' :: tail ->
                    parseAccidental tail |> Maybe.map (PitchClass.pitchClass E)

                'f' :: tail ->
                    parseAccidental tail |> Maybe.map (PitchClass.pitchClass F)

                'g' :: tail ->
                    parseAccidental tail |> Maybe.map (PitchClass.pitchClass G)

                'a' :: tail ->
                    parseAccidental tail |> Maybe.map (PitchClass.pitchClass A)

                'b' :: tail ->
                    parseAccidental tail |> Maybe.map (PitchClass.pitchClass B)

                _ ->
                    Nothing
    in
    maybePitchClass |> Maybe.map Enharmonic.simple


scaleClass : Parser (( String, ScaleClass ) -> a) a
scaleClass =
    Url.Parser.custom "SCALECLASS"
        (\str ->
            Constants.scales
                |> SelectList.toList
                |> List.Extra.find (Tuple.first >> (==) str)
        )


pitchClass : Parser (PitchClass -> a) a
pitchClass =
    Url.Parser.custom "PITCHCLASS" pitchClassFromString


formula : Parser (Formula -> a) a
formula =
    Url.Parser.custom "FORMULA" Formula.fromUrlString


routeParser : Parser (Route -> a) a
routeParser =
    map Main (pitchClass </> scaleClass </> formula </> pitchClass)


extractRoute : Url -> Maybe Route
extractRoute =
    parse routeParser
        >> Maybe.andThen
            (\(Main root sc f startingNote) ->
                if Types.validStartingNote root (Tuple.second sc) startingNote then
                    Just (Main root sc f startingNote)

                else
                    Just (Main root sc f root)
            )


mkUrl : PitchClass -> String -> Formula -> PitchClass -> String
mkUrl rootPc sc f startingNotePc =
    Url.Builder.absolute [ pitchClassToUrlString rootPc, sc, Formula.toUrlString f, pitchClassToUrlString startingNotePc ] []


pitchClassToUrlString : PitchClass -> String
pitchClassToUrlString pc =
    let
        spelling =
            pc |> Spelling.simple

        serializeAccidental a =
            case a of
                Sharp ->
                    Just "sharp"

                Natural ->
                    Nothing

                Flat ->
                    Just "flat"

        toString { letter, accidental } =
            (letter
                |> Letter.toString
                |> String.toLower
            )
                ++ (serializeAccidental accidental |> Maybe.Extra.unwrap "" ((++) "-"))
    in
    toString spelling


randomStep : Random.Generator Int
randomStep =
    Random.weighted ( 50, 1 )
        [ ( 40, 2 )
        , ( 30, 3 )
        , ( 20, 4 )
        , ( 10, 5 )
        ]
        |> Random.andThen (\n -> Random.uniform -1 [ 1 ] |> Random.map ((*) n))


randomFormula : Random.Generator Formula
randomFormula =
    Random.weighted ( 5, 1 )
        [ ( 10, 2 )
        , ( 40, 3 )
        , ( 50, 4 )
        , ( 40, 5 )
        , ( 10, 6 )
        , ( 5, 7 )
        , ( 2, 8 )
        ]
        |> Random.andThen (\n -> Random.list n randomStep)


randomUrl : Random.Generator String
randomUrl =
    Random.pair
        (Random.uniform (SelectList.selected Constants.roots) (SelectList.before Constants.roots ++ SelectList.after Constants.roots))
        (Random.uniform (SelectList.selected Constants.scales) (SelectList.before Constants.scales ++ SelectList.after Constants.scales))
        |> Random.andThen
            (\( r, sc ) ->
                Random.uniform r (Scale.scale r (Tuple.second sc) |> Scale.toList |> List.filter (not << (==) r))
                    |> Random.map (\sn -> ( r, sc, sn ))
            )
        |> Random.andThen
            (\( r, sc, sn ) ->
                randomFormula
                    |> Random.map (\f -> mkUrl r (Tuple.first sc) f sn)
            )


generateRandomUrl : (String -> msg) -> Cmd msg
generateRandomUrl msg =
    Random.generate msg randomUrl
