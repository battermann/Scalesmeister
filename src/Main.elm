module Main exposing (..)

import Html exposing (Html, text, div, h1, button, p, i)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Html.Attributes exposing (class)
import Ports exposing (..)
import Types exposing (..)
import Random exposing (generate)
import Array exposing (Array, fromList, toList, map)
import Random.Array exposing (shuffle)


---- MODEL ----


toPitchNotation : Pitch -> PitchNotation
toPitchNotation (Pitch note accidental octave) =
    let
        acc =
            case accidental of
                Just Sharp ->
                    "#"

                Just Flat ->
                    "b"

                Nothing ->
                    ""
    in
        (toString note) ++ acc ++ (toString octave)


pitchToSampleUrlMapping : Pitch -> ( PitchNotation, SampleUrl )
pitchToSampleUrlMapping (Pitch note accidental octave) =
    let
        acc =
            Maybe.map toString accidental
                |> Maybe.withDefault ""

        url =
            "samples/" ++ (toString note) ++ acc ++ (toString octave) ++ ".mp3"
    in
        ( toPitchNotation (Pitch note accidental octave), url )


loadPianoSamples : Cmd msg
loadPianoSamples =
    [ Pitch C Nothing 4
    , Pitch D (Just Sharp) 4
    , Pitch F (Just Sharp) 4
    , Pitch A Nothing 4
    ]
        |> List.map pitchToSampleUrlMapping
        |> loadSamples


generate12ToneRow : Cmd Msg
generate12ToneRow =
    Random.generate RowGenerated (Random.Array.shuffle (chromaticScale 4))


init : ( Model, Cmd Msg )
init =
    let
        cmd =
            Cmd.batch [ loadPianoSamples, generate12ToneRow ]
    in
        ( Nothing, cmd )



---- UPDATE ----


type Msg
    = NoteOn Pitch
    | NoteOff Pitch
    | RowGenerated (Array Pitch)
    | GenerateNew12ToneRow
    | TogglePlay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoteOn pitch ->
            ( model, noteOn (toPitchNotation pitch) )

        NoteOff pitch ->
            ( model, noteOff (toPitchNotation pitch) )

        RowGenerated row ->
            ( Just (Stopped row), Cmd.none )

        GenerateNew12ToneRow ->
            ( Nothing, Cmd.batch [ stopSequence (), generate12ToneRow ] )

        TogglePlay ->
            case model of
                Nothing ->
                    ( model, Cmd.none )

                Just (Stopped row) ->
                    ( Just (Playing row), startSequence (Array.map toPitchNotation row) )

                Just (Playing row) ->
                    ( Just (Stopped row), stopSequence () )



---- VIEW ----


rowView : Array Pitch -> Html Msg
rowView row =
    div [] (row |> Array.map (\p -> button [ onMouseDown (NoteOn p), onMouseUp (NoteOff p) ] [ text (toPitchNotation p) ]) |> Array.toList)


generateButton : Html Msg
generateButton =
    button [ onClick GenerateNew12ToneRow ] [ text "Generate new row" ]


rowWithControls : Array Pitch -> String -> Html Msg
rowWithControls row icon =
    div []
        [ rowView row
        , p []
            [ button [ onClick TogglePlay ] [ i [ class icon ] [] ]
            , generateButton
            ]
        ]


maybeRowWithControls : Model -> Html Msg
maybeRowWithControls model =
    case model of
        Just (Stopped row) ->
            rowWithControls row "fas fa-play"

        Just (Playing row) ->
            rowWithControls row "fas fa-stop"

        Nothing ->
            div [] [ generateButton ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "luigi" ]
        , maybeRowWithControls model
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
