module MidiConversions exposing (toMidiNumber, createDataLink)

import Types.Pitch as Pitch exposing (..)
import Midi.Types
import Midi.Generate exposing (recording)
import BinaryBase64


toMidiNumber : Pitch -> Int
toMidiNumber =
    Pitch.semitoneOffset >> ((+) 12)


toMidi : List Pitch -> List Midi.Types.Byte
toMidi pitches =
    pitches
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


createDataLink : List Pitch -> String
createDataLink =
    toMidi >> BinaryBase64.encode >> concatParts "audio/midi" "base64"
