port module Ports.In exposing (samplesLoaded)


port samplesLoaded : (() -> msg) -> Sub msg
