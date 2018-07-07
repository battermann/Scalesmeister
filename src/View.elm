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
import Types.Range as Range exposing (Range)
import Types.Pitch exposing (..)
import Types.Octave as Octave
import Types.Scale as Scale exposing (Scale(..))


accidentalToString : Accidental -> String
accidentalToString accidental =
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


displayPitch : Pitch -> String
displayPitch (Pitch note octave) =
    (displayNote note) ++ (Octave.number octave |> toString)


displayNote : Note -> String
displayNote (Note letter accidental) =
    (toString letter) ++ (accidental |> accidentalToString)


rangeView : Range -> Element MyStyles variation Msg
rangeView range =
    column None
        [ width fill, spacing 2, userSelectNone ]
        [ el SmallText [] (text "Range")
        , row None
            [ spacing 2 ]
            [ button Page [ width fill, padding 10, onClick RangeMinSkipDown ] Icons.doubleAngleLeft
            , button Page [ width fill, padding 10, onClick RangeMinStepDown ] Icons.angleLeft
            , button Page [ width fill, padding 10, onClick RangeMinStepUp ] Icons.angleRight
            , button Page [ width fill, padding 10, onClick RangeMinSkipUp ] Icons.doubleAngleRight
            , column Page
                [ verticalCenter, center, padding 10, spacing 2, width (px 120) ]
                [ row None
                    [ spacing 10 ]
                    [ text (displayPitch (Range.lowest range))
                    , text "-"
                    , text (displayPitch (Range.highest range))
                    ]
                ]
            , button Page [ width fill, padding 10, onClick RangeMaxSkipDown ] Icons.doubleAngleLeft
            , button Page [ width fill, padding 10, onClick RangeMaxStepDown ] Icons.angleLeft
            , button Page [ width fill, padding 10, onClick RangeMaxStepUp ] Icons.angleRight
            , button Page [ width fill, padding 10, onClick RangeMaxSkipUp ] Icons.doubleAngleRight
            ]
        ]


formulaPartToString : Int -> Element style variation msg
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
            [ spacing 2, alignBottom ]
            [ button LightButton
                [ onClick TogglePlay
                , padding 10
                , userSelectNone
                , height (px 60)
                , width (px 60)
                ]
                icon
            , button Page
                [ onClick DownloadPdf
                , padding 10
                , userSelectNone
                , verticalCenter
                ]
                (row None [] [ Icons.download, text " PDF" ])
            ]


settings : Model -> Element MyStyles variation Msg
settings model =
    row None
        [ spacing 2 ]
        [ column None
            [ width fill, spacing 2 ]
            [ el SmallText [] (text "Root")
            , button Page
                [ padding 10
                , alignLeft
                , userSelectNone
                , onClick (Open SelectRoot)
                , width fill
                ]
                (displayNote (SelectList.selected model.roots) |> text)
            ]
        , column None
            [ width fill, spacing 2 ]
            [ el SmallText [] (text "Scale")
            , button Page
                [ padding 10
                , onClick (Open SelectScale)
                , width fill
                ]
                (text (model.scales |> SelectList.selected |> Tuple.first))
            ]
        , column None
            [ width fill, spacing 2 ]
            [ el SmallText [] (text "Formula")
            , button Page
                [ padding 10
                , onClick (Open SelectFormula)
                , width fill
                ]
                (displayFormula (model.formulas |> SelectList.selected))
            ]
        , column None
            [ width fill, spacing 2 ]
            [ el SmallText [] (text "Starting note")
            , button Page
                [ padding 10
                , width fill
                , onClick (Open SelectStartingNote)
                ]
                (displayNote model.startingNote |> text)
            ]
        ]


modalDialog : Model -> Element MyStyles variation Msg -> Element MyStyles variation Msg
modalDialog model element =
    modal Dialog
        [ width fill
        , height fill
        , padding 200
        , onClick CloseDialog
        ]
        (el Page
            [ center
            , padding 20
            ]
            element
        )


selectNoteButton : (Note -> Msg) -> Note -> Element MyStyles variation Msg
selectNoteButton event note =
    button DarkButton [ userSelectNone, onClick (event note), padding 10, width fill ] (displayNote note |> text)


selectScaleButton : ( String, ScaleDef ) -> Element MyStyles variation Msg
selectScaleButton ( name, scale ) =
    button DarkButton [ padding 10, userSelectNone, onClick (ScaleSelected scale) ] (text name)


selectFormulaButton : Formula -> Element MyStyles variation Msg
selectFormulaButton formula =
    button DarkButton [ padding 10, userSelectNone, onClick (FormulaSelected formula), width fill ] (displayFormula formula)


selectScaleDialog : Model -> Element MyStyles variation Msg
selectScaleDialog model =
    modalDialog model
        (column None
            [ spacing 2 ]
            ((h2 H2 [ center ] (text "Scale"))
                :: (SelectList.toList model.scales |> List.map selectScaleButton)
            )
        )


selectRootDialog : Model -> Element MyStyles variation Msg
selectRootDialog model =
    modalDialog model
        (column None
            [ spacing 2, width (px 400) ]
            ((h2 H2 [ center ] (text "Root"))
                :: (SelectList.toList model.roots |> List.map (selectNoteButton RootSelected) |> List.Extra.greedyGroupsOf 3 |> List.map (row None [ spacing 2 ]))
            )
        )


selectStartingNoteDialog : Model -> Element MyStyles variation Msg
selectStartingNoteDialog model =
    modalDialog model
        (column None
            [ spacing 2, width (px 400) ]
            ((h2 H2 [ center ] (text "Starting note"))
                :: (SelectList.selected model.scales |> (Tuple.second >> (Scale (SelectList.selected model.roots)) >> Scale.notes) |> List.map (selectNoteButton StartingNoteSelected) |> List.Extra.greedyGroupsOf 3 |> List.map (row None [ spacing 2 ]))
            )
        )


selectFormulaDialog : Model -> Element MyStyles variation Msg
selectFormulaDialog model =
    modalDialog model
        (column None
            [ spacing 2 ]
            ((h2 H2 [ center ] (text "Formula"))
                :: (SelectList.toList model.formulas |> List.map selectFormulaButton |> List.Extra.greedyGroupsOf 2 |> List.map (row None [ spacing 2 ]))
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

        Just SelectStartingNote ->
            selectStartingNoteDialog model

        Nothing ->
            empty


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        (column Page
            [ spacing 40, padding 20, paddingTop 100 ]
            [ h1 H1 [ padding 10, center ] (text "Luigi")
            , paragraph Subtitle [ center, paddingBottom 40 ] [ (text "Generate lines for jazz improvisation based on scales and formulas.") ]
            , column None
                [ spacing 2 ]
                [ column None
                    []
                    [ row None
                        [ center, width fill ]
                        [ column None
                            [ spacing 2, width (px 800) ]
                            [ playAndDownload model.playingState
                            , column Settings
                                [ padding 20, spacing 6 ]
                                [ settings model
                                , rangeView model.range
                                ]
                            ]
                        ]
                    ]
                , row Score [ center ] [ el Score [ id Score.elementId, center ] empty ]
                ]
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
