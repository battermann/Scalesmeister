module MidiConversions exposing (toMidiNumber, createDataLink)

import Pitch exposing (..)
import Array exposing (Array, toList)
import Midi.Types
import Midi.Generate exposing (recording)
import BinaryBase64
import Octave exposing (..)
import ListUtils exposing (flatten)


toMidiNumber : Pitch -> Int
toMidiNumber (Pitch letter accidental octave) =
    ((number octave) + 1) * 12 + (letterOffset letter) + (accidentalOffset accidental)


letterOffset : Letter -> Int
letterOffset letter =
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


accidentalOffset : Maybe Accidental -> Int
accidentalOffset accidental =
    case accidental of
        Just Sharp ->
            1

        Just Flat ->
            -1

        Nothing ->
            0


toMidi : Array Pitch -> List Midi.Types.Byte
toMidi row =
    row
        |> Array.toList
        |> List.map toMidiNumber
        |> List.map
            (\midiNumber ->
                [ ( 0, Midi.Types.NoteOn 0 midiNumber 64 )
                , ( 2, Midi.Types.NoteOff 0 midiNumber 0 )
                ]
            )
        |> flatten
        |> Midi.Types.SingleTrack 4
        |> recording


concatParts : String -> String -> String -> String
concatParts contentType contentTransferEncoding midiData =
    "data:" ++ contentType ++ ";" ++ contentTransferEncoding ++ "," ++ midiData


createDataLink : Array Pitch -> String
createDataLink =
    toMidi >> BinaryBase64.encode >> concatParts "audio/midi" "base64"
