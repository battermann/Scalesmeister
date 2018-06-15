module Main exposing (..)

import Html exposing (Html, text, div, h1, button, p, i, a)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Html.Attributes exposing (class, href, download, downloadAs, id)
import Ports exposing (..)
import Types exposing (..)
import MidiConversions exposing (toBase64EncodedMidi, toDataString)
import Random exposing (generate)
import Array exposing (Array, fromList, toList, map)
import Random.Array exposing (shuffle)


---- MODEL ----


toPitchNotation_ : String -> Pitch -> PitchNotation
toPitchNotation_ nat (Pitch note accidental octave) =
    let
        acc =
            case accidental of
                Just Sharp ->
                    "#"

                Just Flat ->
                    "b"

                Nothing ->
                    nat
    in
        (toString note) ++ acc ++ (toString octave)


toPitchNotation : Pitch -> PitchNotation
toPitchNotation pitch =
    toPitchNotation_ "" pitch


toEasyScorePitchNotation : Pitch -> PitchNotation
toEasyScorePitchNotation pitch =
    (toPitchNotation_ "n" pitch) ++ "/q"


toEasyScoreNotation : Array Pitch -> String
toEasyScoreNotation row =
    row |> Array.map toEasyScorePitchNotation |> Array.toList |> String.join ", "


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
    [ Pitch C Nothing middleOctave
    , Pitch D (Just Sharp) middleOctave
    , Pitch F (Just Sharp) middleOctave
    , Pitch A Nothing middleOctave
    ]
        |> List.map pitchToSampleUrlMapping
        |> loadSamples


generate12ToneRow : Cmd Msg
generate12ToneRow =
    Random.generate RowGenerated (Random.Array.shuffle (chromaticScale 4))


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.batch [ loadPianoSamples, generate12ToneRow ] )



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
            ( Just (Stopped row), renderScore ( scoreElementId, toEasyScoreNotation row ) )

        GenerateNew12ToneRow ->
            ( model, Cmd.batch [ stopSequence (), generate12ToneRow ] )

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
            , a [ href (toBase64EncodedMidi row |> toDataString), downloadAs "luigi.midi", class "button" ] [ i [ class "fas fa-download" ] [] ]
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
        , div [ id scoreElementId ] []
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
