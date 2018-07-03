module View exposing (view)

import Html exposing (i, div, Html)
import Types exposing (..)
import Styles exposing (..)
import Element exposing (..)
import Styles exposing (..)
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import Score
import Types.Note exposing (..)
import SelectList exposing (SelectList)
import List.Extra
import Types.Scale exposing (ScaleDef)
import Types.Formula exposing (Formula)
import View.FontAwesome as Icons


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


formulaPartToString n =
    case n < 0 of
        False ->
            text ("↑" ++ (toString (abs n)))

        True ->
            text ("↓" ++ (toString (abs n)))


displayFormula : Formula -> Element MyStyles variation msg
displayFormula formula =
    formula
        |> List.map formulaPartToString
        |> List.intersperse (text "  ")
        |> (row None [])


playAndDownload : PlayingState -> Element MyStyles variation Msg
playAndDownload line =
    let
        icon =
            case line of
                Stopped ->
                    Icons.play

                Playing ->
                    Icons.stop
    in
        row None
            [ spacing 10, alignBottom, center ]
            [ button Button
                [ onClick TogglePlay
                , padding 10
                , userSelectNone
                ]
                icon
            , button Button
                [ onClick DownloadPdf
                , padding 10
                , userSelectNone
                ]
                (row None [] [ Icons.download, text " PDF" ])
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
            , width (px 60)
            , height (px 60)
            ]
            (displayNote (SelectList.selected model.roots))
        , button LargeFontButton
            [ padding 10
            , onClick (Open SelectScale)
            ]
            (text (model.scales |> SelectList.selected |> Tuple.first))
        , button SkipsNSteps
            [ padding 10
            , onClick (Open SelectFormula)
            ]
            (displayFormula (model.formulas |> SelectList.selected))
        ]


modalDialog : Model -> Element MyStyles variation Msg -> Element MyStyles variation Msg
modalDialog model element =
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
            element
        )


selectNoteButton : Note -> Element MyStyles variation Msg
selectNoteButton note =
    button LargeFontButton [ userSelectNone, onClick (RootSelected note), width (px 65), height (px 65) ] (displayNote note)


selectScaleButton : ( String, ScaleDef ) -> Element MyStyles variation Msg
selectScaleButton ( name, scale ) =
    button LargeFontButton [ padding 10, userSelectNone, onClick (ScaleSelected scale) ] (text name)


selectFormulaButton : Formula -> Element MyStyles variation Msg
selectFormulaButton formula =
    button Button [ padding 10, userSelectNone, onClick (FormulaSelected formula), width (px 150) ] (displayFormula formula)


selectScaleDialog : Model -> Element MyStyles variation Msg
selectScaleDialog model =
    modalDialog model
        (column None
            [ spacing 10 ]
            ((h2 H2 [ center ] (text "Scale"))
                :: (SelectList.toList model.scales |> List.map selectScaleButton)
            )
        )


selectRootDialog : Model -> Element MyStyles variation Msg
selectRootDialog model =
    modalDialog model
        (column None
            [ spacing 10 ]
            ((h2 H2 [ center ] (text "Root Note"))
                :: (SelectList.toList model.roots |> List.map selectNoteButton |> List.Extra.greedyGroupsOf 3 |> List.map (row None [ spacing 10 ]))
            )
        )


selectFormulaDialog : Model -> Element MyStyles variation Msg
selectFormulaDialog model =
    modalDialog model
        (column None
            [ spacing 10 ]
            ((h2 H2 [ center ] (text "Formula"))
                :: (SelectList.toList model.formulas |> List.map selectFormulaButton |> List.Extra.greedyGroupsOf 2 |> List.map (row None [ spacing 10 ]))
            )
        )


chooseDialog : Model -> Element MyStyles variation Msg
chooseDialog model =
    case model.dialog of
        Just SelectRoot ->
            selectRootDialog model

        Just SelectScale ->
            selectScaleDialog model

        Just SelectFormula ->
            selectFormulaDialog model

        Nothing ->
            empty


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        (column Page
            [ spacing 40, padding 20 ]
            [ h1 H1 [ padding 10, center ] (text "luigi")
            , paragraph Subtitle [ center ] [ (text "Generate lines for jazz improvisation based on scales and formulas.") ]
            , el None [ center ] (column None [ spacing 30 ] [ settings model, playAndDownload model.playingState ])
            , el Score [ id Score.elementId ] empty
            , column Footer
                [ spacing 5 ]
                [ row None
                    [ center ]
                    [ text "created with "
                    , link "http://elm-lang.org/" <| el Link [] (text "Elm")
                    ]
                , row None
                    [ center ]
                    [ text "sound samples from "
                    , link "https://archive.org/details/SalamanderGrandPianoV3" <| el Link [] (text "Salamander Grand Piano")
                    ]
                , el GitHubIcon [ center ] (link "https://github.com/battermann/Luigi" <| Icons.github)
                ]
            , chooseDialog model
            ]
        )
