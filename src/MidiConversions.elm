module MidiConversions exposing (toMidiNumber, createDataLink)

import Types.Pitch exposing (..)
import Types.Note exposing (..)
import Array exposing (Array, toList)
import Midi.Types
import Midi.Generate exposing (recording)
import BinaryBase64
import Types.Octave exposing (..)
import ListUtils exposing (flatten)


toMidiNumber : Pitch -> Int
toMidiNumber (Pitch (Note letter accidental) octave) =
    ((number octave) + 1) * 12 + (letterSemitoneOffset letter) + (accidentalSemitoneOffset accidental)


toMidi : Array Pitch -> List Midi.Types.Byte
toMidi pitches =
    pitches
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
