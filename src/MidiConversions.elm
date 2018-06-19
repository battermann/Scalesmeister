module MidiConversions exposing (toMidiNumber, createDataLink)

import Types.Pitch as Pitch exposing (..)
import Array exposing (Array, toList)
import Midi.Types
import Midi.Generate exposing (recording)
import BinaryBase64
import Types.Octave exposing (..)


toMidiNumber : Pitch -> Int
toMidiNumber =
    Pitch.semitoneOffset >> ((+) 12)


toMidi : Array Pitch -> List Midi.Types.Byte
toMidi pitches =
    pitches
        |> Array.toList
        |> List.map toMidiNumber
        |> List.concatMap
            (\midiNumber ->
                [ ( 0, Midi.Types.NoteOn 0 midiNumber 64 )
                , ( 2, Midi.Types.NoteOff 0 midiNumber 0 )
                ]
            )
        |> Midi.Types.SingleTrack 4
        |> recording


concatParts : String -> String -> String -> String
concatParts contentType contentTransferEncoding midiData =
    "data:" ++ contentType ++ ";" ++ contentTransferEncoding ++ "," ++ midiData


createDataLink : Array Pitch -> String
createDataLink =
    toMidi >> BinaryBase64.encode >> concatParts "audio/midi" "base64"
