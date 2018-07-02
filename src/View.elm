module View exposing (view)

import Html exposing (i, div, Html)
import Html.Attributes exposing (class)
import Types exposing (..)
import Styles exposing (..)
import Element exposing (..)
import Styles exposing (..)
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import Score
import Types.Note exposing (..)
import State
import SelectList exposing (SelectList)
import List.Extra
import Types.Scale exposing (ScaleDef)
import Types.Formula exposing (Formula)


displayNote : Note -> Element style variation msg
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


playAndDownload : PlayingState -> Element MyStyles variation Msg
playAndDownload line =
    let
        icon =
            case line of
                Stopped ->
                    "fas fa-play"

                Playing ->
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


settings : Model -> Element MyStyles variation Msg
settings model =
    row None
        [ spacing 10 ]
        [ button LargeFontButton
            [ padding 10
            , alignLeft
            , userSelectNone
            , onClick (Open SelectRoot)
            , width (px 70)
            , height (px 70)
            ]
            (displayNote (SelectList.selected model.roots))
        , button LargeFontButton
            [ padding 10
            , onClick (Open SelectScale)
            ]
            (text (model.scales |> SelectList.selected |> Tuple.first))
        , button LargeFontButton
            [ padding 10
            , onClick (Open SelectFormula)
            ]
            (text (model.formulas |> SelectList.selected |> toString))
        ]


notePicker : Note -> Element MyStyles variation Msg
notePicker note =
    button LargeFontButton [ padding 10, userSelectNone, onClick (RootSelected note), width (px 70), height (px 70) ] (displayNote note)


scalePicker : ( String, ScaleDef ) -> Element MyStyles variation Msg
scalePicker ( name, scale ) =
    button LargeFontButton [ padding 10, userSelectNone, onClick (ScaleSelected scale) ] (text name)


formulaPicker : Formula -> Element MyStyles variation Msg
formulaPicker formula =
    button Button [ padding 10, userSelectNone, onClick (FormulaSelected formula), width (px 200) ] (text (toString formula))


selectScale : Model -> Element MyStyles variation Msg
selectScale model =
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
                (SelectList.toList model.scales |> List.map scalePicker)
            )
        )


selectRoot : Model -> Element MyStyles variation Msg
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
                (SelectList.toList model.roots |> List.map notePicker |> List.Extra.greedyGroupsOf 3 |> List.map (row None [ spacing 10 ]))
            )
        )


selectFormula : Model -> Element MyStyles variation Msg
selectFormula model =
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
                (SelectList.toList model.formulas |> List.map formulaPicker |> List.Extra.greedyGroupsOf 2 |> List.map (row None [ spacing 10 ]))
            )
        )


dialog : Model -> Element MyStyles variation Msg
dialog model =
    case model.dialog of
        Just SelectRoot ->
            selectRoot model

        Just SelectScale ->
            selectScale model

        Just SelectFormula ->
            selectFormula model

        Nothing ->
            empty


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        (column Page
            [ spacing 20, padding 20 ]
            [ h1 Title [ padding 10, center ] (text "luigi")
            , paragraph Subtitle [ center ] [ (text "Generate lines for jazz improvisation based on scales and formulas.") ]
            , column None [ spacing 30 ] [ settings model, playAndDownload model.playingState ]
            , el Score [ id Score.elementId ] empty
            , footer Footer [] (text "created with Elm")
            , dialog model
            ]
        )
