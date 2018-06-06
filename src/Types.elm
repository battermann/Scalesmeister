module Types exposing (..)

type Note
    = C
    | DSharp
    | FSharp
    | A

type alias NoteAlias = String

type alias NoteSampleUrl = String

type alias Model = Maybe Note
