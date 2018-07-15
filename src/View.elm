module View exposing (view)

import Html exposing (i, div, Html)
import Types exposing (..)
import Styles exposing (..)
import Element exposing (..)
import Styles exposing (..)
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import Score
import Types.PitchClass exposing (..)
import SelectList exposing (SelectList)
import List.Extra
import Types.Scale exposing (ScaleDef)
import Types.Formula exposing (Formula)
import View.FontAwesome as Icons
import Types.Range as Range exposing (Range)
import Types.Pitch exposing (..)
import Types.Scale as Scale exposing (Scale(..))


rangeView : Model -> Element AppStyles variation Msg
rangeView model =
    let
        myLayout =
            if model.device.phone || (model.device.tablet && model.device.portrait) then
                column
            else
                row
    in
        column None
            [ width fill, spacing 2, userSelectNone ]
            [ el SmallText [] (text "Range")
            , myLayout None
                [ spacing 2 ]
                [ row None
                    [ spacing 2 ]
                    [ button Page [ width fill, padding 10, onClick RangeMinSkipDown ] Icons.doubleAngleLeft
                    , button Page [ width fill, padding 10, onClick RangeMinStepDown ] Icons.angleLeft
                    , button Page [ width fill, padding 10, onClick RangeMinStepUp ] Icons.angleRight
                    , button Page [ width fill, padding 10, onClick RangeMinSkipUp ] Icons.doubleAngleRight
                    ]
                , column Page
                    [ verticalCenter, center, padding 10, spacing 2, width fill ]
                    [ row None
                        [ spacing 10 ]
                        [ text (displayPitch (Range.lowest model.range))
                        , text "-"
                        , text (displayPitch (Range.highest model.range))
                        ]
                    ]
                , row None
                    [ spacing 2 ]
                    [ button Page [ width fill, padding 10, onClick RangeMaxSkipDown ] Icons.doubleAngleLeft
                    , button Page [ width fill, padding 10, onClick RangeMaxStepDown ] Icons.angleLeft
                    , button Page [ width fill, padding 10, onClick RangeMaxStepUp ] Icons.angleRight
                    , button Page [ width fill, padding 10, onClick RangeMaxSkipUp ] Icons.doubleAngleRight
                    ]
                ]
            ]


formulaPartToString : Int -> Element style variation msg
formulaPartToString n =
    case n < 0 of
        False ->
            text ("↑" ++ (toString (abs n)))

        True ->
            text ("↓" ++ (toString (abs n)))


displayFormula : Formula -> Element AppStyles variation msg
displayFormula formula =
    formula
        |> List.map formulaPartToString
        |> List.intersperse (text "  ")
        |> (row None [])


playAndDownload : Model -> Element AppStyles variation Msg
playAndDownload model =
    let
        ( icon, control, event ) =
            case ( model.playingState, model.samplesLoaded ) of
                ( _, False ) ->
                    ( column None [ center, verticalCenter, spacing 4 ] [ Icons.spinner, el VerySmallText [] (text "loading…") ], el, [] )

                ( Stopped, _ ) ->
                    ( Icons.play, button, [ onClick TogglePlay ] )

                ( Playing, _ ) ->
                    ( Icons.stop, button, [ onClick TogglePlay ] )
    in
        row None
            [ spacing 2, alignBottom ]
            [ control LightButton
                ([ padding 10
                 , userSelectNone
                 , height (px 60)
                 , width (px 60)
                 ]
                    ++ event
                )
                icon
            , button Page
                [ onClick DownloadPdf
                , padding 10
                , userSelectNone
                , verticalCenter
                ]
                (row None [] [ Icons.download, text " PDF" ])
            ]


settings : Model -> Element AppStyles variation Msg
settings model =
    let
        columns =
            if model.device.phone || model.device.tablet then
                2
            else
                4
    in
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
                (noteToString (SelectList.selected model.roots) |> text)
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
                (noteToString model.startingNote |> text)
            ]
        ]
            |> List.Extra.greedyGroupsOf columns
            |> List.map (row None [ spacing 2 ])
            |> (column None [ spacing 6 ])


modalDialog : Model -> Element AppStyles variation Msg -> Element AppStyles variation Msg
modalDialog model element =
    modal Dialog
        [ width fill
        , height fill
        , onClick CloseDialog
        , paddingTop 100
        , scrollbars
        ]
        (el Page
            [ center
            , padding 20
            ]
            element
        )


selectNoteButton : (PitchClass -> Msg) -> PitchClass -> Element AppStyles variation Msg
selectNoteButton event note =
    button DarkButton [ userSelectNone, onClick (event note), padding 10, width fill ] (noteToString note |> text)


selectScaleButton : ( String, ScaleDef ) -> Element AppStyles variation Msg
selectScaleButton ( name, scale ) =
    button DarkButton [ padding 10, userSelectNone, onClick (ScaleSelected scale) ] (text name)


selectFormulaButton : Formula -> Element AppStyles variation Msg
selectFormulaButton formula =
    button DarkButton [ padding 10, userSelectNone, onClick (FormulaSelected formula), width fill ] (displayFormula formula)


selectScaleDialog : Model -> Element AppStyles variation Msg
selectScaleDialog model =
    modalDialog model
        (column None
            [ spacing 2 ]
            ((h2 H2 [ center ] (text "Scale"))
                :: (SelectList.toList model.scales |> List.map selectScaleButton)
            )
        )


selectRootDialog : Model -> Element AppStyles variation Msg
selectRootDialog model =
    modalDialog model
        (column None
            [ spacing 2, width (px 220) ]
            ((h2 H2 [ center ] (text "Root"))
                :: (SelectList.toList model.roots |> List.map (selectNoteButton RootSelected) |> List.Extra.greedyGroupsOf 3 |> List.map (row None [ spacing 2 ]))
            )
        )


selectStartingNoteDialog : Model -> Element AppStyles variation Msg
selectStartingNoteDialog model =
    modalDialog model
        (column None
            [ spacing 2, width (px 220) ]
            ((h2 H2 [ center ] (text "Starting note"))
                :: (SelectList.selected model.scales |> (Tuple.second >> (Scale (SelectList.selected model.roots)) >> Scale.notes) |> List.map (selectNoteButton StartingNoteSelected) |> List.Extra.greedyGroupsOf 3 |> List.map (row None [ spacing 2 ]))
            )
        )


selectFormulaDialog : Model -> Element AppStyles variation Msg
selectFormulaDialog model =
    modalDialog model
        (column None
            [ spacing 2 ]
            ((h2 H2 [ center ] (text "Formula"))
                :: (SelectList.toList model.formulas |> List.map selectFormulaButton |> List.Extra.greedyGroupsOf 2 |> List.map (row None [ spacing 2 ]))
            )
        )


chooseDialog : Model -> Element AppStyles variation Msg
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
    let
        ( scoreLayout, pagePaddingTop, settingsWidth ) =
            if model.device.phone || model.device.tablet then
                ( row Score [ xScrollbar ] [ el Score [ id Score.elementId, center, width (percent 100) ] empty ]
                , paddingTop 20
                , percent 100
                )
            else
                ( row Score [ center ] [ el Score [ id Score.elementId, center ] empty ]
                , paddingTop 100
                , percent 70
                )
    in
        Element.viewport stylesheet <|
            (column Page
                [ spacing 40, paddingXY 10 10, pagePaddingTop ]
                [ h1 H1 [ center ] (text "Luigi")
                , paragraph Subtitle [ paddingBottom 40, center ] [ text "Generate lines for jazz improvisation based on scales and formulas." ]
                , column None
                    [ spacing 2 ]
                    [ column None
                        []
                        [ row None
                            [ center, width (percent 100) ]
                            [ column None
                                [ spacing 2, width settingsWidth ]
                                [ playAndDownload model
                                , column Settings
                                    [ padding 20, spacing 6 ]
                                    [ settings model
                                    , rangeView model
                                    ]
                                ]
                            ]
                        ]
                    , scoreLayout
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
