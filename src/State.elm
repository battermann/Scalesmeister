module State exposing (init, subscriptions, update)

import Audio
import Browser.Dom exposing (Viewport)
import Browser.Events
import Libs.SelectList as SelectList exposing (SelectList)
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass exposing (ScaleClass)
import Score as Score
import Task
import Types exposing (Device, Model, Msg(..), PlayingState(..))
import Types.Formula as Formula exposing (Formula)
import Types.Line as Line exposing (Line)
import Types.Note as Note
import Types.Octave as Octave
import Types.Orchestration as Orchestration
import Types.Pitch as Pitch exposing (Pitch(..), flat, natural, sharp)
import Types.Range as Range exposing (Range)
import Types.Switch as Switch
import Types.TimeSignature as TimeSignature exposing (BeatDuration(..), NumberOfBeats(..), TimeSignature(..))


scales : SelectList ( String, ScaleClass )
scales =
    SelectList.fromLists []
        ( "Major Pentatonic", ScaleClass.majorPentatonic )
        [ ( "Minor Pentatonic", ScaleClass.minorPentatonic )
        , ( "Minor 6 Pentatonic", ScaleClass.minor6Pentatonic )
        , ( "Major ♭6 Pentatonic", ScaleClass.majorFlat6Pentatonic )
        , ( "Minor 7 ♭5 Pentatonic", ScaleClass.minorFlat5Pentatonic )
        , ( "Major ♭2 Pentatonic", ScaleClass.majorFlat2Pentatonic )
        , ( "Diatonic Major", ScaleClass.major )
        ]


roots : SelectList PitchClass
roots =
    SelectList.fromLists
        []
        (PitchClass.pitchClass C PitchClass.natural)
        [ PitchClass.pitchClass D PitchClass.flat
        , PitchClass.pitchClass D PitchClass.natural
        , PitchClass.pitchClass E PitchClass.flat
        , PitchClass.pitchClass E PitchClass.natural
        , PitchClass.pitchClass F PitchClass.natural
        , PitchClass.pitchClass G PitchClass.flat
        , PitchClass.pitchClass G PitchClass.natural
        , PitchClass.pitchClass A PitchClass.flat
        , PitchClass.pitchClass A PitchClass.natural
        , PitchClass.pitchClass B PitchClass.flat
        , PitchClass.pitchClass B PitchClass.natural
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


mkLine : Range -> ScaleClass -> Formula -> PitchClass -> PitchClass -> Line
mkLine range scaleClass formula root startingNote =
    Line.fromScaleWithinRange range (Scale.scale root scaleClass)
        |> List.map (Debug.log "note")
        |> Line.applyFormula startingNote formula


line : Model -> Line
line model =
    mkLine model.range
        (model.scales |> SelectList.selected |> Tuple.second)
        (model.formulas |> SelectList.selected)
        (model.roots |> SelectList.selected)
        model.startingNote


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform (classifyDevice >> WindowResize) Browser.Dom.getViewport


init : ( Model, Cmd Msg )
init =
    let
        range =
            Range.piano
                |> Range.setLowest (Pitch.pitch (PitchClass.pitchClass C PitchClass.natural) Octave.three)
                |> Range.setHighest (Pitch.pitch (PitchClass.pitchClass B PitchClass.natural) Octave.six)

        timeSignature =
            TimeSignature Four TimeSignature.Quarter

        noteDuration =
            Note.Eighth Note.None

        model =
            { range = range
            , formulas = formulas
            , roots = roots
            , startingNote = SelectList.selected roots
            , scales = scales
            , playingState = Stopped
            , dialog = Nothing
            , samplesLoaded = False
            , device = classifyDeviceFrom 0 0
            , timeSignature = timeSignature
            , noteDuration = noteDuration
            , clickTrack = Switch.off
            , tempo = 160
            }
    in
    case line model |> Orchestration.orchestrate timeSignature noteDuration of
        Just orchestration ->
            ( model, Cmd.batch [ initialSizeCmd, Audio.loadPianoSamples, Score.render orchestration ] )

        Nothing ->
            ( model, Cmd.batch [ initialSizeCmd, Audio.loadPianoSamples ] )


classifyDevice : Viewport -> Device
classifyDevice { viewport } =
    classifyDeviceFrom (viewport.width |> round) (viewport.height |> round)


classifyDeviceFrom : Int -> Int -> Device
classifyDeviceFrom width height =
    { width = width
    , height = height
    , phone = width <= 600
    , tablet = width > 600 && width <= 1200
    , desktop = width > 1200 && width <= 1800
    , bigDesktop = width > 1800
    , portrait = width < height
    }


renderNew : PlayingState -> Model -> ( Model, Cmd msg )
renderNew playingState model =
    case ( playingState, line model |> Orchestration.orchestrate model.timeSignature model.noteDuration ) of
        ( _, Nothing ) ->
            ( model, Cmd.none )

        ( Stopped, Just orchestration ) ->
            ( model, Score.render orchestration )

        ( Playing, Just orchestration ) ->
            ( { model | playingState = Stopped }, Cmd.batch [ Audio.stop, Score.render orchestration ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePlay ->
            case model.playingState of
                Stopped ->
                    ( { model | playingState = Playing }, Audio.play model.clickTrack model.timeSignature model.noteDuration (line model) )

                Playing ->
                    ( { model | playingState = Stopped }, Audio.stop )

        DownloadPdf ->
            ( model, Score.downloadAsPdf )

        Open dialog ->
            ( { model | dialog = Just dialog }, Cmd.none )

        CloseDialog ->
            ( { model | dialog = Nothing }, Cmd.none )

        RootSelected note ->
            { model
                | roots = model.roots |> SelectList.select ((==) note)
                , playingState = Stopped
                , startingNote = note
            }
                |> renderNew model.playingState

        StartingNoteSelected note ->
            { model | startingNote = note, playingState = Stopped }
                |> renderNew model.playingState

        ScaleSelected scale ->
            { model
                | scales = model.scales |> SelectList.select (Tuple.second >> (==) scale)
                , playingState = Stopped
                , startingNote = SelectList.selected model.roots
            }
                |> renderNew model.playingState

        FormulaSelected formula ->
            { model
                | formulas = model.formulas |> SelectList.select ((==) formula)
                , playingState = Stopped
            }
                |> renderNew model.playingState

        SetTimeSignature timeSignature ->
            { model
                | timeSignature = timeSignature
                , playingState = Stopped
                , noteDuration =
                    if TimeSignature.durationGte (TimeSignature.beatDuration timeSignature) Quarter then
                        model.noteDuration

                    else
                        Note.Eighth Note.None
            }
                |> renderNew model.playingState

        ToggleNoteValue ->
            (case ( model.noteDuration, model.timeSignature ) of
                ( Note.Eighth Note.None, TimeSignature _ TimeSignature.Quarter ) ->
                    { model | noteDuration = Note.Eighth Note.Triplet }

                ( Note.Eighth Note.Triplet, _ ) ->
                    { model | noteDuration = Note.Eighth Note.None }

                _ ->
                    model
            )
                |> renderNew model.playingState

        RangeMinStepDown ->
            let
                min =
                    Pitch.transposeOld [ natural, flat ] (Range.lowest model.range) -1
                        |> Maybe.withDefault (Range.lowest model.range)
            in
            { model | range = Range.setLowest min model.range }
                |> renderNew model.playingState

        RangeMinStepUp ->
            let
                min =
                    Pitch.transposeOld [ natural, sharp ] (Range.lowest model.range) 1
                        |> Maybe.withDefault (Range.lowest model.range)
            in
            { model | range = Range.setLowest min model.range }
                |> renderNew model.playingState

        RangeMinSkipDown ->
            let
                min =
                    Pitch.transposeOld [ natural, flat ] (Range.lowest model.range) -12
                        |> Maybe.withDefault (Range.lowest model.range)
            in
            { model | range = Range.setLowest min model.range }
                |> renderNew model.playingState

        RangeMinSkipUp ->
            let
                min =
                    Pitch.transposeOld [ natural, sharp ] (Range.lowest model.range) 12
                        |> Maybe.withDefault (Range.lowest model.range)
            in
            { model | range = Range.setLowest min model.range }
                |> renderNew model.playingState

        RangeMaxStepDown ->
            let
                max =
                    Pitch.transposeOld [ natural, flat ] (Range.highest model.range) -1
                        |> Maybe.withDefault (Range.highest model.range)
            in
            { model | range = Range.setHighest max model.range }
                |> renderNew model.playingState

        RangeMaxStepUp ->
            let
                max =
                    Pitch.transposeOld [ natural, sharp ] (Range.highest model.range) 1
                        |> Maybe.withDefault (Range.highest model.range)
            in
            { model | range = Range.setHighest max model.range }
                |> renderNew model.playingState

        RangeMaxSkipDown ->
            let
                max =
                    Pitch.transposeOld [ natural, flat ] (Range.highest model.range) -12
                        |> Maybe.withDefault (Range.highest model.range)
            in
            { model | range = Range.setHighest max model.range }
                |> renderNew model.playingState

        RangeMaxSkipUp ->
            let
                max =
                    Pitch.transposeOld [ natural, sharp ] (Range.highest model.range) 12
                        |> Maybe.withDefault (Range.highest model.range)
            in
            { model | range = Range.setHighest max model.range }
                |> renderNew model.playingState

        SamplesLoaded ->
            ( { model | samplesLoaded = True }, Cmd.none )

        WindowResize device ->
            ( { model | device = device }, Cmd.none )

        ToggleClick ->
            ( { model | clickTrack = model.clickTrack |> Switch.toggle }, model.clickTrack |> Switch.fold Audio.muteClick Audio.unMuteClick )

        UpdateTempo tempo ->
            ( { model | tempo = tempo }, tempo |> round |> Audio.setTempo )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Audio.samplesLoaded SamplesLoaded
        , Browser.Events.onResize (\w h -> classifyDeviceFrom w h |> WindowResize)
        ]
