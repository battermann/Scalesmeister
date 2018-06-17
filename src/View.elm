module View exposing (view)

import Html exposing (Html, text, div, h1, button, p, i, a)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Html.Attributes exposing (class, href, download, downloadAs, id)
import Array exposing (Array)
import Types.Tonal exposing (..)
import MidiConversions
import Types.Octave as Octave
import Score
import Model exposing (..)


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
        (toString letter) ++ acc |> text


rowView : Array Pitch -> Html Msg
rowView row =
    div [] (row |> Array.map (\p -> button [ onMouseDown (NoteOn p), onMouseUp (NoteOff p) ] [ displayPitch p ]) |> Array.toList)


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
            ]
        , p []
            [ a [ href (MidiConversions.createDataLink row), downloadAs "luigi.midi", class "button" ] [ i [ class "fas fa-download" ] [], text " MIDI" ]
            , button [ onClick DownloadPdf ] [ i [ class "fas fa-download" ] [], text " PDF" ]
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
        , div [ id Score.elementId ] []
        ]
