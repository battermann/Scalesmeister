module View exposing (view)

import Element exposing (Attribute, Element, alignRight, centerX, centerY, column, el, fill, height, image, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import Html
import Libs.SelectList as SelectList
import List.Extra
import Styles
import Types exposing (Dialog(..), Model, Msg(..), PlayingState(..))
import Types.Formula exposing (Formula)
import Types.Note as Note
import Types.Pitch exposing (displayPitch)
import Types.PitchClass exposing (PitchClass, pitchClassToString)
import Types.Range as Range
import Types.Scale as Scale exposing (Scale(..), ScaleDef)
import Types.Switch as Switch
import Types.TimeSignature exposing (BeatDuration(..), NumberOfBeats(..), TimeSignature(..), beatDuration, timeSignatureToString)
import View.FontAwesome as Icons
import View.RangeInput as RangeInput


smallSpacing : Attribute msg
smallSpacing =
    spacing 2


standardPadding : Attribute msg
standardPadding =
    padding 10


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
        style duration =
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
        [ button
            (Note.Eighth Note.None |> style)
            { label = image [ height (px 20) ] { src = fileName (Note.Eighth Note.None) "eighthnotes", description = "" }
            , onPress = Just ToggleNoteValue
            }
        , if [ Quarter, Half ] |> List.member (model.timeSignature |> beatDuration) then
            button
                (Note.Eighth Note.Triplet |> style)
                { label = image [ height (px 20) ] { src = fileName (Note.Eighth Note.Triplet) "triplet", description = "" }
                , onPress = Just ToggleNoteValue
                }

          else
            el (Note.Eighth Note.Triplet |> style) <|
                image [ Styles.opacity 0.2, height (px 20) ] { src = fileName (Note.Eighth Note.Triplet) "triplet", description = "" }
        ]


viewTempoSlider : Model -> Element Msg
viewTempoSlider model =
    row
        [ spacing 15 ]
        [ el [ spacing 10, padding 2, height (px 40) ] (RangeInput.input model.tempo UpdateTempo)
            |> viewControlWithLabel [ width fill ] ("Tempo: " ++ String.fromFloat model.tempo ++ " bpm")
        , button
            (standardPadding :: Switch.fold Styles.lightButton Styles.page model.clickTrack)
            { label = el [] (model.clickTrack |> Switch.fold Icons.volumeUp Icons.volumeOff), onPress = Just ToggleClick }
            |> viewControlWithLabel [ width (px 40) ] "Click"
        ]


viewTimeSignatureControls : Model -> Element Msg
viewTimeSignatureControls model =
    let
        style ts =
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
                |> List.map (\ts -> button (style ts ++ [ width fill, standardPadding ]) { label = ts |> timeSignatureToString |> text, onPress = Just (SetTimeSignature ts) })
                |> List.Extra.greedyGroupsOf rowLength
                |> List.map (row [ smallSpacing ])
                |> column [ spacing 6 ]
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

        buttonStyle =
            Styles.page ++ [ width fill, standardPadding, Styles.userSelectNone ]
    in
    colOrRow
        [ smallSpacing ]
        [ row
            [ smallSpacing ]
            [ button buttonStyle { label = Icons.doubleAngleLeft, onPress = Just RangeMinSkipDown }
            , button buttonStyle { label = Icons.angleLeft, onPress = Just RangeMinStepDown }
            , button buttonStyle { label = Icons.angleRight, onPress = Just RangeMinStepUp }
            , button buttonStyle { label = Icons.doubleAngleRight, onPress = Just RangeMinSkipUp }
            ]
        , column (Styles.page ++ [ centerX, centerY, standardPadding, smallSpacing, width fill, Styles.userSelectNone ])
            [ row [ spacing 10 ]
                [ text (displayPitch (Range.lowest model.range))
                , text "-"
                , text (displayPitch (Range.highest model.range))
                ]
            ]
        , row
            [ smallSpacing ]
            [ button buttonStyle { label = Icons.doubleAngleLeft, onPress = Just RangeMaxSkipDown }
            , button buttonStyle { label = Icons.angleLeft, onPress = Just RangeMaxStepDown }
            , button buttonStyle { label = Icons.angleRight, onPress = Just RangeMaxStepUp }
            , button buttonStyle { label = Icons.doubleAngleRight, onPress = Just RangeMaxSkipUp }
            ]
        ]
        |> viewControlWithLabel [ width fill, smallSpacing, Styles.userSelectNone ] "Range"


view : Model -> Html.Html Msg
view _ =
    Element.layout [] <|
        row [] [ el [] <| text "Hello World" ]
