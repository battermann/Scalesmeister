port module Ports exposing (..)

import Types exposing (..)

port noteOn : NoteAlias -> Cmd msg

port noteOff : () -> Cmd msg

port loadSample : ( NoteAlias, NoteSampleUrl ) -> Cmd msg
