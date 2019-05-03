module View exposing (view)

import Element as Element exposing (Attribute, Element, alignBottom, alignRight, centerX, centerY, column, el, fill, height, image, inFront, link, minimum, padding, paddingEach, paddingXY, paragraph, px, row, scrollbars, spacing, text, width)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Libs.SelectList as SelectList exposing (SelectList)
import List.Extra
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.Scale as Scale
import Score
import Types exposing (Dialog(..), Model, Msg(..), PlayingState(..))
import Types.Formula as Formula exposing (Formula)
import Types.Note as Note
import Types.Range as Range
import Types.Switch as Switch
import Types.TimeSignature as TimeSignature exposing (BeatDuration(..), NumberOfBeats(..), TimeSignature(..))
import View.FontAwesome as Icons
import View.Styles as Styles


smallSpacing : Attribute msg
smallSpacing =
    spacing 2


standardPadding : Attribute msg
standardPadding =
    padding 10


id : String -> Attribute msg
id value =
    Element.htmlAttribute <| Html.Attributes.id value


viewControlWithLabel : List (Attribute msg) -> String -> Element msg -> Element msg
viewControlWithLabel attributes label control =
    column
        (smallSpacing :: attributes)
        [ el Styles.smallText (text label)
        , control
        ]


viewNoteDurationControls : Model -> Element Msg
viewNoteDurationControls model =
    let
        attributes duration =
            if duration == model.noteDuration then
                standardPadding :: Styles.lightButton

            else
                standardPadding :: Styles.page

        fileName duration baseName =
            if duration == model.noteDuration then
                baseName ++ ".svg"

            else
                baseName ++ "-light" ++ ".svg"
    in
    row
        [ smallSpacing ]
        [ Input.button
            (Note.Eighth Note.None |> attributes)
            { label = image [ height (px 20) ] { src = fileName (Note.Eighth Note.None) "eighthnotes", description = "" }
            , onPress = Just ToggleNoteValue
            }
        , if [ Quarter, Half ] |> List.member (model.timeSignature |> TimeSignature.beatDuration) then
            Input.button
                (Note.Eighth Note.Triplet |> attributes)
                { label = image [ height (px 20) ] { src = fileName (Note.Eighth Note.Triplet) "triplet", description = "" }
                , onPress = Just ToggleNoteValue
                }

          else
            el (Note.Eighth Note.Triplet |> attributes) <|
                image [ Styles.opacity 0.2, height (px 20) ] { src = fileName (Note.Eighth Note.Triplet) "triplet", description = "" }
        ]


viewTempoSlider : Model -> Element Msg
viewTempoSlider model =
    row
        [ spacing 15, width fill ]
        [ Input.slider
            [ spacing 10
            , padding 2
            , width fill
            , height (px 38)
            , Element.behindContent <|
                Element.el (Styles.rangeTrack ++ [ width fill, height (px 2), centerY ]) Element.none
            ]
            { onChange = UpdateTempo
            , label = Input.labelHidden ""
            , min = 60.0
            , max = 280.0
            , value = model.tempo
            , thumb = Input.defaultThumb
            , step = Just 1.0
            }
            |> viewControlWithLabel [ width fill ] ("Tempo: " ++ String.fromFloat model.tempo ++ " bpm")
        , Input.button
            ([ standardPadding, width (px 40) ] ++ Switch.fold Styles.lightButton Styles.page model.clickTrack)
            { label = el [] (model.clickTrack |> Switch.fold Icons.volumeUp Icons.volumeOff), onPress = Just ToggleClick }
            |> viewControlWithLabel [] "Click"
        ]


viewTimeSignatureControls : Model -> Element Msg
viewTimeSignatureControls model =
    let
        attributes ts =
            if ts == model.timeSignature then
                Styles.lightButton

            else
                Styles.page

        rowLength =
            if model.device.phone && model.device.portrait then
                5

            else
                10

        buttons =
            [ TimeSignature Three Quarter
            , TimeSignature Four Quarter
            , TimeSignature Five Quarter
            , TimeSignature Six Quarter
            , TimeSignature Three Eighth
            , TimeSignature Five Eighth
            , TimeSignature Six Eighth
            , TimeSignature Seven Eighth
            , TimeSignature Nine Eighth
            , TimeSignature Twelve Eighth
            ]
                |> List.map (\ts -> Input.button (attributes ts ++ [ width fill, standardPadding ]) { label = ts |> TimeSignature.toString |> text, onPress = Just (SetTimeSignature ts) })
                |> List.Extra.greedyGroupsOf rowLength
                |> List.map (row [ smallSpacing, width fill ])
                |> column [ spacing 6, width fill ]
    in
    buttons |> viewControlWithLabel [ width fill, Styles.userSelectNone ] "Time Signature"


viewRangeControls : Model -> Element Msg
viewRangeControls model =
    let
        colOrRow =
            if model.device.phone || (model.device.tablet && model.device.portrait) then
                column

            else
                row

        buttonAttributes =
            Styles.page ++ [ width fill, standardPadding, Styles.userSelectNone ]
    in
    colOrRow
        [ smallSpacing, width fill ]
        [ row
            [ smallSpacing, width fill ]
            [ Input.button buttonAttributes { label = Icons.doubleAngleLeft, onPress = Just RangeMinSkipDown }
            , Input.button buttonAttributes { label = Icons.angleLeft, onPress = Just RangeMinStepDown }
            , Input.button buttonAttributes { label = Icons.angleRight, onPress = Just RangeMinStepUp }
            , Input.button buttonAttributes { label = Icons.doubleAngleRight, onPress = Just RangeMinSkipUp }
            ]
        , el (Styles.page ++ [ width fill, standardPadding, smallSpacing, Styles.userSelectNone ]) <|
            row [ spacing 10, centerX, centerY ]
                [ text (Pitch.toString (Range.lowest model.range))
                , text "-"
                , text (Pitch.toString (Range.highest model.range))
                ]
        , row
            [ smallSpacing, width fill ]
            [ Input.button buttonAttributes { label = Icons.doubleAngleLeft, onPress = Just RangeMaxSkipDown }
            , Input.button buttonAttributes { label = Icons.angleLeft, onPress = Just RangeMaxStepDown }
            , Input.button buttonAttributes { label = Icons.angleRight, onPress = Just RangeMaxStepUp }
            , Input.button buttonAttributes { label = Icons.doubleAngleRight, onPress = Just RangeMaxSkipUp }
            ]
        ]
        |> viewControlWithLabel [ width fill, smallSpacing, Styles.userSelectNone ] "Range"


viewPlayControl : Model -> Element Msg
viewPlayControl model =
    let
        attributes =
            Styles.lightButton ++ [ alignBottom, standardPadding, Styles.userSelectNone, height (px 60), width (px 60), id "play-button" ]
    in
    case ( model.playingState, model.samplesLoaded ) of
        ( _, False ) ->
            column attributes [ Icons.spinner, el Styles.verySmallText (text "loading…") ]

        ( Stopped, _ ) ->
            Input.button attributes { label = Icons.play, onPress = Just TogglePlay }

        ( Playing, _ ) ->
            Input.button attributes { label = Icons.stop, onPress = Just TogglePlay }


viewMainSettingsControls : Model -> Element Msg
viewMainSettingsControls model =
    let
        columns =
            if model.device.phone || model.device.tablet then
                2

            else
                4

        buttonAttributes =
            Styles.page ++ [ standardPadding, Styles.userSelectNone, width fill ]
    in
    [ Input.button
        buttonAttributes
        { label = PitchClass.toString (SelectList.selected model.roots) |> text, onPress = Just <| Open SelectRoot }
        |> viewControlWithLabel [ width fill ] "Root"
    , Input.button
        buttonAttributes
        { label = text (model.scales |> SelectList.selected |> Tuple.first), onPress = Just <| Open SelectScale }
        |> viewControlWithLabel [ width fill ] "Scale"
    , Input.button
        buttonAttributes
        { label = model.formula |> Formula.toString |> text, onPress = Just <| Open SelectFormula }
        |> viewControlWithLabel [ width fill ] "Formula"
    , Input.button
        buttonAttributes
        { label = PitchClass.toString model.startingNote |> text, onPress = Just <| Open SelectStartingNote }
        |> viewControlWithLabel [ width fill ] "Starting Note"
    ]
        |> List.Extra.greedyGroupsOf columns
        |> List.map (row [ smallSpacing, width fill ])
        |> column [ spacing 6, width fill ]


viewModalDialog : String -> Element Msg -> Element Msg
viewModalDialog heading element =
    el
        (Styles.dialog
            ++ [ Element.htmlAttribute (Html.Attributes.style "z-index" "20")
               , width fill
               , height fill
               , onClick CloseDialog
               , paddingEach { top = 100, left = 0, bottom = 0, right = 0 }
               , scrollbars
               ]
        )
    <|
        el (Styles.page ++ [ centerX, padding 20 ]) (column [ spacing 20 ] [ el [ Element.alignRight, Styles.userSelectNone ] Icons.times, el (centerX :: Styles.h2) (text heading), element ])


viewModalDialog2 : String -> Element Msg -> Element Msg
viewModalDialog2 heading element =
    el
        (Styles.dialog
            ++ [ Element.htmlAttribute (Html.Attributes.style "z-index" "20")
               , width fill
               , height fill
               , paddingEach { top = 100, left = 0, bottom = 0, right = 0 }
               , scrollbars
               ]
        )
    <|
        el (Styles.page ++ [ centerX, padding 20 ]) (column [ spacing 20 ] [ el [ Element.alignRight, Styles.userSelectNone, onClick CloseDialog ] Icons.times, el (centerX :: Styles.h2) (text heading), element ])


darkButtonAttributes : List (Attribute msg)
darkButtonAttributes =
    Styles.darkButton ++ [ Styles.userSelectNone, standardPadding, width (fill |> minimum 100) ]


lightButtonAttributes : List (Attribute msg)
lightButtonAttributes =
    Styles.lightButton ++ [ Styles.userSelectNone, standardPadding, width (fill |> minimum 100) ]


viewSelectNoteButton : List (Attribute Msg) -> (PitchClass -> Msg) -> PitchClass -> Element Msg
viewSelectNoteButton attrs event pitchClass =
    Input.button attrs { label = PitchClass.toString pitchClass |> text, onPress = Just <| event pitchClass }


viewSelectScaleButton : List (Attribute Msg) -> String -> Element Msg
viewSelectScaleButton attrs name =
    Input.button attrs { label = text name, onPress = Just <| ScaleSelected name }


viewSelectFormulaButton : List (Attribute Msg) -> Formula -> Element Msg
viewSelectFormulaButton attrs formula =
    Input.button attrs { label = formula |> Formula.toString |> text, onPress = Just <| FormulaSelected formula }


viewSelectScaleDialog : Model -> Element Msg
viewSelectScaleDialog model =
    viewModalDialog "Scale" <|
        column
            [ smallSpacing ]
            (List.append (SelectList.before model.scales) (SelectList.after model.scales)
                |> List.sortBy Tuple.first
                |> List.map (Tuple.first >> viewSelectScaleButton darkButtonAttributes)
                |> (::) (SelectList.selected model.scales |> Tuple.first >> viewSelectScaleButton lightButtonAttributes)
            )


viewSelectRootDialog : Model -> Element Msg
viewSelectRootDialog model =
    viewModalDialog "Root" <|
        column
            [ smallSpacing ]
            (SelectList.toList model.roots
                |> List.map (\root -> viewSelectNoteButton (matchWithSelected darkButtonAttributes lightButtonAttributes model.roots root) RootSelected root)
                |> List.Extra.greedyGroupsOf 3
                |> List.map (row [ smallSpacing, width fill ])
            )


viewSelectStartingNoteDialog : Model -> Element Msg
viewSelectStartingNoteDialog model =
    viewModalDialog "Starting Note" <|
        column
            [ smallSpacing ]
            (SelectList.selected model.scales
                |> (Tuple.second >> Scale.scale (SelectList.selected model.roots) >> Scale.toList)
                |> List.map
                    (\startingNote ->
                        viewSelectNoteButton
                            (if model.startingNote == startingNote then
                                lightButtonAttributes

                             else
                                darkButtonAttributes
                            )
                            StartingNoteSelected
                            startingNote
                    )
                |> List.Extra.greedyGroupsOf 3
                |> List.map (row [ smallSpacing, width fill ])
            )


viewSelectFormulaDialog : Model -> Element Msg
viewSelectFormulaDialog model =
    viewModalDialog2 "Formula" <|
        column [ Element.spacing 16 ]
            [ Input.text [ Font.color Styles.darkGray, Element.htmlAttribute (Html.Attributes.id "formula-input") ]
                { onChange = FormulaInput
                , text = model.formulaInput
                , placeholder = Just <| Input.placeholder [] <| text "example: +2-1-2+1"
                , label = Input.labelHidden "formula"
                }
            , el (onClick CloseDialog :: width fill :: Styles.userSelectNone :: padding 10 :: Styles.largeText) (text <| Formula.toString model.formula)
            , column
                [ smallSpacing ]
                (Formula.formulas
                    |> List.map (viewSelectFormulaButton darkButtonAttributes)
                    |> List.Extra.greedyGroupsOf 2
                    |> List.map (row [ smallSpacing, width fill ])
                )
            ]


viewSelectedDialog : Model -> Element Msg
viewSelectedDialog model =
    case model.dialog of
        Just SelectRoot ->
            viewSelectRootDialog model

        Just SelectScale ->
            viewSelectScaleDialog model

        Just SelectFormula ->
            viewSelectFormulaDialog model

        Just SelectStartingNote ->
            viewSelectStartingNoteDialog model

        Nothing ->
            Element.none


matchWithSelected : b -> b -> SelectList a -> a -> b
matchWithSelected onNotSelected onSelected list a =
    if SelectList.selected list == a then
        onSelected

    else
        onNotSelected


viewAdvancedControls : Model -> Element Msg
viewAdvancedControls model =
    if model.advancedControls then
        column (Styles.settings ++ [ padding 20, spacing 6, width fill ])
            [ row [ spacing 10, alignRight, Styles.userSelectNone, onClick ToggleAdvancedControls ] [ Icons.angleUp ]
            , viewRangeControls model
            , viewTimeSignatureControls model
            , viewNoteDurationControls model
            ]

    else
        column (Styles.settings ++ [ padding 20, spacing 6, width fill ])
            [ row [ spacing 10, alignRight, Styles.userSelectNone, onClick ToggleAdvancedControls ] [ el Styles.smallText (text "More"), Icons.angleDown ]
            ]


viewPage : Model -> Element Msg
viewPage model =
    let
        ( viewScore, paddingTop, paddingLeftRight ) =
            if model.device.phone || model.device.tablet then
                ( row ([ scrollbars, width fill ] ++ Styles.score) [ el (Styles.score ++ [ id Score.elementId, centerX, width (px 800) ]) Element.none ]
                , paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
                , paddingXY 20 0
                )

            else
                ( row (width fill :: Styles.score) [ el (Styles.score ++ [ id Score.elementId, centerX ]) Element.none ]
                , paddingEach { top = 100, bottom = 0, left = 0, right = 0 }
                , paddingXY 250 0
                )
    in
    column [ width fill, spacing 40, paddingXY 10 10, paddingTop ]
        [ el
            [ centerX
            , if model.device.width < 411 then
                Font.size 50

              else
                Font.size 60
            ]
            (text "Scalesmeister")
        , paragraph
            (Styles.subTitle ++ [ paddingEach { top = 0, bottom = 40, left = 0, right = 0 }, centerX ])
            [ paragraph [] [ text "Generate lines for jazz improvisation based on ", el [ Font.bold ] (text "scales"), text " and ", el [ Font.bold ] (text "formulas"), text "." ] ]
        , column
            [ smallSpacing, width fill ]
            [ row
                [ centerX, width fill, paddingLeftRight ]
                [ column [ smallSpacing, width fill ]
                    [ viewPlayControl model
                    , column (Styles.settings ++ [ padding 20, spacing 6, width fill ])
                        [ viewTempoSlider model
                        , viewMainSettingsControls model
                        ]
                    , viewAdvancedControls model
                    ]
                ]
            , viewScore
            ]
        , column
            ([ spacing 5, width fill ] ++ Styles.footer)
            [ row [ centerX ]
                [ text "v0.5.0 | created with "
                , link Styles.link { url = "http://elm-lang.org/", label = text "Elm" }
                ]
            , row [ centerX ]
                [ text "sound samples: "
                , link Styles.link { url = "https://archive.org/details/SalamanderGrandPianoV3", label = text "Salamander Grand Piano" }
                ]
            , row [ centerX ]
                [ text "Inspired by "
                , link Styles.link { url = "https://learningmusic.ableton.com/", label = text "Ableton Learning Music" }
                ]
            , el (centerX :: Styles.gitHubIcon) <| link [] { url = "https://github.com/battermann/Luigi", label = Icons.github }
            ]
        ]


view : Model -> Html.Html Msg
view model =
    Element.layout Styles.page <|
        column
            [ width fill
            , height fill
            , inFront <| viewSelectedDialog model
            ]
            [ viewPage model ]
