module State exposing (init, subscriptions, update)

import Audio
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Browser.Navigation as Nav
import Constants
import Libs.SelectList as SelectList
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.Pitch.Enharmonic as Enharmonic
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass exposing (ScaleClass)
import Routing exposing (Route(..))
import Score as Score
import Task
import Types exposing (Device, Dialog(..), Error(..), Model, Msg(..), PlayingState(..))
import Types.Formula as Formula exposing (Formula)
import Types.Line as Line exposing (Line)
import Types.Note as Note
import Types.Orchestration as Orchestration
import Types.Range as Range exposing (Range)
import Types.Switch as Switch
import Types.TimeSignature as TimeSignature exposing (BeatDuration(..), NumberOfBeats(..), TimeSignature(..))
import Url


mkLine : Range -> ScaleClass -> Formula -> PitchClass -> PitchClass -> Line
mkLine range scaleClass formula root startingNote =
    Line.fromScaleWithinRange range (Scale.scale root scaleClass)
        |> Line.applyFormula startingNote formula


line : Model -> Line
line model =
    mkLine model.range
        (model.scales |> SelectList.selected |> Tuple.second)
        model.formula
        (model.roots |> SelectList.selected)
        model.startingNote


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform (classifyDevice >> WindowResize) Browser.Dom.getViewport


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    let
        range =
            Range.piano
                |> Range.setLowest (Pitch.pitch C PitchClass.natural Octave.three)
                |> Range.setHighest (Pitch.pitch B PitchClass.natural Octave.six)

        timeSignature =
            TimeSignature Four TimeSignature.Quarter

        noteDuration =
            Note.Eighth Note.None

        ( settings, cmd ) =
            case Routing.extractRoute url of
                Just (Main root scaleClass formula startingNote) ->
                    ( { root = root
                      , scale = scaleClass
                      , formula = formula
                      , startingNote = startingNote
                      }
                    , Cmd.none
                    )

                Nothing ->
                    let
                        set =
                            { root = SelectList.selected Constants.roots
                            , scale = SelectList.selected Constants.scales
                            , formula = [ 1 ]
                            , startingNote = SelectList.selected Constants.roots
                            }
                    in
                    ( set
                    , Nav.pushUrl key (Routing.mkUrl set.root (Tuple.first set.scale) set.formula set.startingNote)
                    )

        model =
            { range = range
            , formula = settings.formula
            , formulaInput = Formula.toInputString settings.formula
            , roots = Constants.roots |> SelectList.select ((==) settings.root)
            , startingNote = settings.startingNote
            , scales = Constants.scales |> SelectList.select ((==) settings.scale)
            , playingState = Stopped
            , dialog = Nothing
            , samplesLoaded = False
            , device = classifyDeviceFrom 0 0
            , timeSignature = timeSignature
            , noteDuration = noteDuration
            , clickTrack = Switch.off
            , tempo = 160
            , advancedControls = False
            , key = key
            , error = Nothing
            , randomUrl = Routing.mkUrl settings.root (Tuple.first settings.scale) settings.formula settings.startingNote
            }
    in
    case line model |> Orchestration.orchestrate timeSignature noteDuration of
        Just orchestration ->
            ( model, Cmd.batch [ Routing.generateRandomUrl RandomUrlCreated, initialSizeCmd, Audio.loadPianoSamples, Score.render orchestration, cmd ] )

        Nothing ->
            ( { model | error = Just CantCreateLine }, Cmd.batch [ Routing.generateRandomUrl RandomUrlCreated, initialSizeCmd, Audio.loadPianoSamples, cmd ] )


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
renderNew =
    renderNewWithCmd Cmd.none


renderNewWithCmd : Cmd msg -> PlayingState -> Model -> ( Model, Cmd msg )
renderNewWithCmd cmd playingState model =
    case ( playingState, line model |> Orchestration.orchestrate model.timeSignature model.noteDuration ) of
        ( _, Nothing ) ->
            ( { model | error = Just CantCreateLine }, Cmd.batch [ cmd ] )

        ( Stopped, Just orchestration ) ->
            ( { model | error = Nothing }, Cmd.batch [ cmd, Score.render orchestration ] )

        ( Playing, Just orchestration ) ->
            ( { model | playingState = Stopped, error = Nothing }, Cmd.batch [ cmd, Audio.stop, Score.render orchestration ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case Routing.extractRoute url of
                Just (Main root scaleClass formula startingNote) ->
                    { model
                        | roots = model.roots |> SelectList.select ((==) root)
                        , scales = Constants.scales |> SelectList.select ((==) scaleClass)
                        , startingNote =
                            if Types.validStartingNote root (Tuple.second scaleClass) startingNote then
                                startingNote

                            else
                                root
                        , formula = formula
                        , formulaInput = Formula.toInputString formula
                        , dialog = Nothing
                        , playingState = Stopped
                    }
                        |> renderNewWithCmd (Routing.generateRandomUrl RandomUrlCreated) model.playingState

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        TogglePlay ->
            case model.playingState of
                Stopped ->
                    ( { model | playingState = Playing }, Audio.play model.clickTrack model.timeSignature model.noteDuration (line model) )

                Playing ->
                    ( { model | playingState = Stopped }, Audio.stop )

        DownloadPdf ->
            ( model, Score.downloadAsPdf )

        Open dialog ->
            case dialog of
                SelectFormula ->
                    ( { model | dialog = Just dialog, formulaInput = Formula.toInputString model.formula }, Task.attempt (always NoOp) (Browser.Dom.focus "formula-input") )

                _ ->
                    ( { model | dialog = Just dialog }, Cmd.none )

        CloseDialog ->
            ( { model | dialog = Nothing }, Cmd.none )

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
                    Pitch.transposeDown Interval.minorSecond (Range.lowest model.range)
                        |> Result.toMaybe
                        |> Maybe.andThen (Enharmonic.asNaturalOrElseFlat >> Result.toMaybe)
                        |> Maybe.withDefault (Range.lowest model.range)
            in
            { model | range = Range.setLowest min model.range }
                |> renderNew model.playingState

        RangeMinStepUp ->
            let
                min =
                    Pitch.transposeUp Interval.minorSecond (Range.lowest model.range)
                        |> Result.toMaybe
                        |> Maybe.andThen (Enharmonic.asNaturalOrElseSharp >> Result.toMaybe)
                        |> Maybe.withDefault (Range.lowest model.range)
            in
            { model | range = Range.setLowest min model.range }
                |> renderNew model.playingState

        RangeMinSkipDown ->
            let
                min =
                    Pitch.transposeDown Interval.perfectOctave (Range.lowest model.range)
                        |> Result.withDefault (Range.lowest model.range)
            in
            { model | range = Range.setLowest min model.range }
                |> renderNew model.playingState

        RangeMinSkipUp ->
            let
                min =
                    Pitch.transposeUp Interval.perfectOctave (Range.lowest model.range)
                        |> Result.withDefault (Range.lowest model.range)
            in
            { model | range = Range.setLowest min model.range }
                |> renderNew model.playingState

        RangeMaxStepDown ->
            let
                max =
                    Pitch.transposeDown Interval.minorSecond (Range.highest model.range)
                        |> Result.toMaybe
                        |> Maybe.andThen (Enharmonic.asNaturalOrElseFlat >> Result.toMaybe)
                        |> Maybe.withDefault (Range.highest model.range)
            in
            { model | range = Range.setHighest max model.range }
                |> renderNew model.playingState

        RangeMaxStepUp ->
            let
                max =
                    Pitch.transposeUp Interval.minorSecond (Range.highest model.range)
                        |> Result.toMaybe
                        |> Maybe.andThen (Enharmonic.asNaturalOrElseSharp >> Result.toMaybe)
                        |> Maybe.withDefault (Range.highest model.range)
            in
            { model | range = Range.setHighest max model.range }
                |> renderNew model.playingState

        RangeMaxSkipDown ->
            let
                max =
                    Pitch.transposeDown Interval.perfectOctave (Range.highest model.range)
                        |> Result.withDefault (Range.highest model.range)
            in
            { model | range = Range.setHighest max model.range }
                |> renderNew model.playingState

        RangeMaxSkipUp ->
            let
                max =
                    Pitch.transposeUp Interval.perfectOctave (Range.highest model.range)
                        |> Result.withDefault (Range.highest model.range)
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

        ToggleAdvancedControls ->
            ( { model | advancedControls = not model.advancedControls }, Cmd.none )

        FormulaInput input ->
            ( { model | formulaInput = input |> Formula.filter }, Cmd.none )

        FormulaPresetSelected formula ->
            ( { model | formulaInput = formula |> Formula.toInputString }, Task.attempt (always NoOp) (Browser.Dom.focus "formula-input") )

        RandomUrlCreated url ->
            ( { model | randomUrl = url }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Audio.samplesLoaded SamplesLoaded
        , Browser.Events.onResize (\w h -> classifyDeviceFrom w h |> WindowResize)
        ]
