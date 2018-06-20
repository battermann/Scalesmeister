module View exposing (view)

import Html exposing (Html, text, div, h1, button, p, i, a)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Html.Attributes exposing (class, href, download, downloadAs, id)
import Types.Pitch exposing (..)
import Types.Note exposing (..)
import MidiConversions
import Score
import Model exposing (..)
import Types.Octave exposing (..)


displayPitch : Pitch -> Html msg
displayPitch (Pitch (Note letter accidental) octave) =
    let
        acc =
            case accidental of
                DoubleFlat ->
                    "bb"

                Flat ->
                    "b"

                Natural ->
                    ""

                Sharp ->
                    "#"

                DoubleSharp ->
                    "##"
    in
        (toString letter) ++ (octave |> number |> toString) ++ acc |> text


lineView : List Pitch -> Html Msg
lineView line =
    let
        _ =
            Debug.log (toString line)
    in
        div [] (line |> List.map (\p -> button [ onMouseDown (NoteOn p), onMouseUp (NoteOff p) ] [ displayPitch p ]))


lineWithControls : List Pitch -> String -> Html Msg
lineWithControls line icon =
    div []
        [ lineView line
        , p []
            [ button [ onClick TogglePlay ] [ i [ class icon ] [] ]
            ]
        , p []
            [ a [ href (MidiConversions.createDataLink line), downloadAs "luigi.midi", class "button" ] [ i [ class "fas fa-download" ] [], text " MIDI" ]
            , button [ onClick DownloadPdf ] [ i [ class "fas fa-download" ] [], text " PDF" ]
            ]
        ]


controlPanel : Model -> Html Msg
controlPanel model =
    case model of
        Stopped line ->
            lineWithControls line "fas fa-play"

        Playing line ->
            lineWithControls line "fas fa-stop"


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "luigi" ]
        , controlPanel model
        , div [ id Score.elementId ] []
        ]
