module MidiConversions exposing (toBase64EncodedMidi, toDataString, toMidiNumber)

import Types exposing (..)
import Array exposing (Array, toList)
import Midi.Types as Midi
import Midi.Generate exposing (recording)
import BinaryBase64 exposing (encode)


type alias ContentType =
    String


type alias ContentTransferEncoding =
    String


type alias MidiAsBase64EncodedString =
    String


type alias MidiEncoding =
    { contentType : ContentType
    , contentTransferEncoding : ContentTransferEncoding
    , midiData : MidiAsBase64EncodedString
    }


toDataString : MidiEncoding -> String
toDataString midi =
    "data:" ++ midi.contentType ++ ";" ++ midi.contentTransferEncoding ++ "," ++ midi.midiData


toMidiNumber : Pitch -> MidiNumber
toMidiNumber (Pitch letter accidental octave) =
    (octave + 1) * 12 + (letterToInt letter) + (accidentalToInt accidental)


letterToInt : Letter -> Int
letterToInt letter =
    case letter of
        C ->
            0

        D ->
            2

        E ->
            4

        F ->
            5

        G ->
            7

        A ->
            9

        B ->
            11


accidentalToInt : Maybe Accidental -> Int
accidentalToInt accidental =
    case accidental of
        Just Sharp ->
            1

        Just Flat ->
            -1

        Nothing ->
            0


flatten : List (List a) -> List a
flatten list =
    List.foldr (++) [] list


toMidi : Array Pitch -> List Midi.Byte
toMidi row =
    row
        |> Array.toList
        |> List.map toMidiNumber
        |> List.map
            (\midiNumber ->
                [ ( 0, Midi.NoteOn 0 midiNumber 64 )
                , ( 2, Midi.NoteOff 0 midiNumber 0 )
                ]
            )
        |> flatten
        |> Midi.SingleTrack 4
        |> recording


toBase64EncodedMidi : Array Pitch -> MidiEncoding
toBase64EncodedMidi bytes =
    { contentType = "audio/midi"
    , contentTransferEncoding = "base64"
    , midiData = (encode (toMidi bytes))
    }
