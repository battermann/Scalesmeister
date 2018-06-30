module View exposing (view)

import Html exposing (i, div, Html)
import Html.Attributes exposing (class)
import Model exposing (..)
import Styles exposing (..)
import Element exposing (..)
import Styles exposing (..)
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import Score
import Types.Note exposing (..)


displayNote (Note letter accidental) =
    let
        acc =
            case accidental of
                DoubleFlat ->
                    "♭♭"

                Flat ->
                    "♭"

                Natural ->
                    ""

                Sharp ->
                    "♯"

                DoubleSharp ->
                    "♯♯"
    in
        (toString letter) ++ acc |> text


playAndDownload line =
    let
        icon =
            case line of
                Stopped _ ->
                    "fas fa-play"

                Playing _ ->
                    "fas fa-stop"
    in
        row None
            [ spacing 10, alignBottom ]
            [ button Button
                [ onClick TogglePlay
                , padding 10
                , userSelectNone
                ]
                (html (i [ Html.Attributes.class icon ] []))
            , button Button
                [ onClick DownloadPdf
                , padding 10
                , userSelectNone
                ]
                (html (div [] [ i [ Html.Attributes.class "fas fa-download" ] [], Html.text " PDF" ]))
            ]


settings model =
    column None
        [ spacing 10 ]
        [ button Root
            [ padding 10
            , alignLeft
            , userSelectNone
            , onClick OpenSelectRootDialog
            ]
            (displayNote model.root)
        ]


notePicker note =
    button Root [ padding 10, userSelectNone, onClick (RootSelected note) ] (displayNote note)


selectRoot model =
    modal Dialog
        [ width fill
        , height fill
        , padding 100
        , onClick CloseDialog
        ]
        (el DialogBox
            [ center
            , padding 20
            ]
            (column None
                [ spacing 10 ]
                [ row None [ spacing 10 ] [ notePicker (Note C Natural), notePicker (Note D Flat) ]
                , row None [ spacing 10 ] [ notePicker (Note E Natural) ]
                , row None [ spacing 10 ] []
                ]
            )
        )


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        (column Page
            [ spacing 20, padding 20 ]
            [ h1 Title [ padding 10, center ] (text "luigi")
            , paragraph Subtitle [ center ] [ (text "Generate lines for jazz improvisation based on scales and formulas.") ]
            , column None [ spacing 30 ] [ settings model, playAndDownload model.line ]
            , el Score [ id Score.elementId ] empty
            , footer Footer [] (text "created with Elm")
            , when (model.dialog == Just SelectRoot) (selectRoot model)
            ]
        )
