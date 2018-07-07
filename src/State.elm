module State exposing (..)

import Audio
import Score
import Types exposing (..)
import Types.Pitch as Pitch exposing (..)
import Types.Octave as Octave
import Types.Line as Line exposing (..)
import Types.Scale exposing (..)
import Types.Range as Range exposing (Range)
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


mkLine : Range -> ScaleDef -> Formula -> Note -> Note -> Line
mkLine range scale formula root startingNote =
    Line.fromScaleWithinRange range (Scale root scale)
        |> Line.applyFormula startingNote formula


line : Model -> Line
line model =
    mkLine model.range
        (model.scales |> SelectList.selected |> Tuple.second)
        (model.formulas |> SelectList.selected)
        (model.roots |> SelectList.selected)
        model.startingNote


init : ( Model, Cmd Msg )
init =
    let
        range =
            Range.piano
                |> Range.setLowest (Pitch (Note C Natural) Octave.three)
                |> Range.setHighest (Pitch (Note B Natural) Octave.six)

        model =
            { range = range, formulas = formulas, roots = roots, startingNote = SelectList.selected roots, scales = scales, playingState = Stopped, dialog = Nothing }
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
            renderNew model.playingState { model | roots = model.roots |> SelectList.select (((==) note)), playingState = Stopped, startingNote = note }

        StartingNoteSelected note ->
            renderNew model.playingState { model | startingNote = note, playingState = Stopped }

        ScaleSelected scale ->
            renderNew model.playingState { model | scales = model.scales |> SelectList.select (Tuple.second >> ((==) scale)), playingState = Stopped, startingNote = SelectList.selected model.roots }

        FormulaSelected formula ->
            renderNew model.playingState { model | formulas = model.formulas |> SelectList.select ((==) formula), playingState = Stopped }

        RangeMinStepDown ->
            let
                min =
                    Pitch.transpose [ natural, flat ] (Range.lowest model.range) -1
                        |> Maybe.withDefault (Range.lowest model.range)

                newModel =
                    { model | range = Range.setLowest min model.range }
            in
                ( newModel, render (line newModel) )

        RangeMinStepUp ->
            let
                min =
                    Pitch.transpose [ natural, sharp ] (Range.lowest model.range) 1
                        |> Maybe.withDefault (Range.lowest model.range)

                newModel =
                    { model | range = Range.setLowest min model.range }
            in
                ( newModel, render (line newModel) )

        RangeMinSkipDown ->
            let
                min =
                    Pitch.transpose [ natural, flat ] (Range.lowest model.range) -12
                        |> Maybe.withDefault (Range.lowest model.range)

                newModel =
                    { model | range = Range.setLowest min model.range }
            in
                ( newModel, render (line newModel) )

        RangeMinSkipUp ->
            let
                min =
                    Pitch.transpose [ natural, sharp ] (Range.lowest model.range) 12
                        |> Maybe.withDefault (Range.lowest model.range)

                newModel =
                    { model | range = Range.setLowest min model.range }
            in
                ( newModel, render (line newModel) )

        RangeMaxStepDown ->
            let
                max =
                    Pitch.transpose [ natural, flat ] (Range.highest model.range) -1
                        |> Maybe.withDefault (Range.highest model.range)

                newModel =
                    { model | range = Range.setHighest max model.range }
            in
                ( newModel, render (line newModel) )

        RangeMaxStepUp ->
            let
                max =
                    Pitch.transpose [ natural, sharp ] (Range.highest model.range) 1
                        |> Maybe.withDefault (Range.highest model.range)

                newModel =
                    { model | range = Range.setHighest max model.range }
            in
                ( newModel, render (line newModel) )

        RangeMaxSkipDown ->
            let
                max =
                    Pitch.transpose [ natural, flat ] (Range.highest model.range) -12
                        |> Maybe.withDefault (Range.highest model.range)

                newModel =
                    { model | range = Range.setHighest max model.range }
            in
                ( newModel, render (line newModel) )

        RangeMaxSkipUp ->
            let
                max =
                    Pitch.transpose [ natural, sharp ] (Range.highest model.range) 12
                        |> Maybe.withDefault (Range.highest model.range)

                newModel =
                    { model | range = Range.setHighest max model.range }
            in
                ( newModel, render (line newModel) )
