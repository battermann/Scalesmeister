module State exposing (..)

import Audio
import Score
import Types exposing (..)
import Types.Pitch as Pitch exposing (..)
import Types.Octave as Octave
import Types.Line as Line exposing (..)
import Types.Scale exposing (..)
import Types.Range exposing (..)
import Types.Note exposing (..)
import Score exposing (..)
import Types.Formula as Formula exposing (..)
import SelectList exposing (SelectList)
import Types.Interval as Interval


scales : SelectList ( String, ScaleDef )
scales =
    SelectList.fromLists []
        ( "Major Pentatonic", majorPentatonic )
        [ ( "Minor Pentatonic", minorPentatonic )
        , ( "Minor ♭6 Pentatonic", minorSixthPentatonic )
        , ( "Minor 7 ♭5 Pentatonic", minorSevenDiminishedFifthPentatonic )
        , ( "Major 6 Pentatonic", majorMinorSixthPentatonic )
        , ( "Major ♭2 Pentatonic", majorMinorSecondPentatonic )
        , ( "Diatonic Major", ionian )
        ]


roots : SelectList Note
roots =
    SelectList.fromLists
        []
        (Note C Natural)
        [ (Note D Flat)
        , (Note D Natural)
        , (Note E Flat)
        , (Note E Natural)
        , (Note F Natural)
        , (Note G Flat)
        , (Note G Natural)
        , (Note A Flat)
        , (Note A Natural)
        , (Note B Flat)
        , (Note B Natural)
        ]


formulas : SelectList Formula
formulas =
    SelectList.fromLists []
        [ 1 ]
        [ [ -1 ]
        , [ 2 ]
        , [ -2 ]
        , Formula.formula1
        , Formula.formula1 |> Formula.invert
        , Formula.formula2
        , Formula.formula2 |> Formula.invert
        , Formula.formula3
        , Formula.formula3 |> Formula.invert
        , Formula.formula4
        , Formula.formula4 |> Formula.invert
        , Formula.formula5
        , Formula.formula5 |> Formula.invert
        ]


mkLine : Range -> ScaleDef -> Formula -> Note -> Line
mkLine range scale formula root =
    Line.fromScaleWithinRange range (Scale root scale)
        |> Line.applyFormula root formula


line : Model -> Line
line model =
    mkLine model.range
        (model.scales |> SelectList.selected |> Tuple.second)
        (model.formulas |> SelectList.selected)
        (model.roots |> SelectList.selected)


init : ( Model, Cmd Msg )
init =
    let
        range =
            { lowest = Pitch (Note C Natural) Octave.three
            , highest = Pitch (Note B Natural) Octave.six
            }

        model =
            { range = range, formulas = formulas, roots = roots, scales = scales, playingState = Stopped, dialog = Nothing }
    in
        ( model, Cmd.batch [ Audio.loadPianoSamples, Score.render (line model) ] )


renderNew : PlayingState -> Model -> ( Model, Cmd msg )
renderNew playingState model =
    case playingState of
        Stopped ->
            ( model, render (line model) )

        Playing ->
            ( model, Cmd.batch [ Audio.stop, render (line model) ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoteOn pitch ->
            ( model, Audio.noteOn pitch )

        NoteOff pitch ->
            ( model, Audio.noteOff pitch )

        TogglePlay ->
            case model.playingState of
                Stopped ->
                    ( { model | playingState = Playing }, Audio.play (line model) )

                Playing ->
                    ( { model | playingState = Stopped }, Audio.stop )

        DownloadPdf ->
            ( model, Score.downloadPdf )

        Open dialog ->
            ( { model | dialog = Just dialog }, Cmd.none )

        CloseDialog ->
            ( { model | dialog = Nothing }, Cmd.none )

        RootSelected note ->
            renderNew model.playingState { model | roots = model.roots |> SelectList.select (((==) note)), playingState = Stopped }

        ScaleSelected scale ->
            renderNew model.playingState { model | scales = model.scales |> SelectList.select (Tuple.second >> ((==) scale)), playingState = Stopped }

        FormulaSelected formula ->
            renderNew model.playingState { model | formulas = model.formulas |> SelectList.select ((==) formula), playingState = Stopped }

        RangeMinStepDown ->
            let
                min =
                    enharmonicEquivalent Pitch.transpose model.range.lowest Pitch.down Interval.minorSecond
                        |> Maybe.withDefault model.range.lowest
            in
                ( { model | range = setLowest model.range min }, Cmd.none )

        RangeMinStepUp ->
            let
                min =
                    enharmonicEquivalent Pitch.transpose model.range.lowest Pitch.up Interval.minorSecond
                        |> Maybe.withDefault model.range.lowest
            in
                ( { model | range = setLowest model.range min }, Cmd.none )

        RangeMinSkipDown ->
            let
                min =
                    enharmonicEquivalent Pitch.transpose model.range.lowest Pitch.down Interval.octave
                        |> Maybe.withDefault model.range.lowest
            in
                ( { model | range = setLowest model.range min }, Cmd.none )

        RangeMinSkipUp ->
            let
                min =
                    enharmonicEquivalent Pitch.transpose model.range.lowest Pitch.up Interval.octave
                        |> Maybe.withDefault model.range.lowest
            in
                ( { model | range = setLowest model.range min }, Cmd.none )

        RangeMaxStepDown ->
            let
                max =
                    enharmonicEquivalent Pitch.transpose model.range.highest Pitch.down Interval.minorSecond
                        |> Maybe.withDefault model.range.highest
            in
                ( { model | range = setHighest model.range max }, Cmd.none )

        RangeMaxStepUp ->
            let
                max =
                    enharmonicEquivalent Pitch.transpose model.range.highest Pitch.up Interval.minorSecond
                        |> Maybe.withDefault model.range.highest
            in
                ( { model | range = setHighest model.range max }, Cmd.none )

        RangeMaxSkipDown ->
            let
                max =
                    enharmonicEquivalent Pitch.transpose model.range.highest Pitch.down Interval.octave
                        |> Maybe.withDefault model.range.highest
            in
                ( { model | range = setHighest model.range max }, Cmd.none )

        RangeMaxSkipUp ->
            let
                max =
                    enharmonicEquivalent Pitch.transpose model.range.highest Pitch.up Interval.octave
                        |> Maybe.withDefault model.range.highest
            in
                ( { model | range = setHighest model.range max }, Cmd.none )
