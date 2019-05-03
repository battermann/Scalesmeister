module State exposing (init, subscriptions, update)

import Audio
import Browser.Dom exposing (Viewport)
import Browser.Events
import Libs.SelectList as SelectList exposing (SelectList)
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.Pitch.Enharmonic as Enharmonic
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass exposing (ScaleClass)
import Score as Score
import Task
import Types exposing (Device, Dialog(..), Model, Msg(..), PlayingState(..))
import Types.Formula as Formula exposing (Formula)
import Types.Line as Line exposing (Line)
import Types.Note as Note
import Types.Orchestration as Orchestration
import Types.Range as Range exposing (Range)
import Types.Switch as Switch
import Types.TimeSignature as TimeSignature exposing (BeatDuration(..), NumberOfBeats(..), TimeSignature(..))


scales : SelectList ( String, ScaleClass )
scales =
    SelectList.fromLists []
        ( "Major Pentatonic", ScaleClass.majorPentatonic )
        [ ( "Aeolian", ScaleClass.aeolian )
        , ( "Altered", ScaleClass.altered )
        , ( "Augmented", ScaleClass.augmented )
        , ( "Blues", ScaleClass.blues )
        , ( "Diminished Half Whole", ScaleClass.diminishedHalfToneWholeTone )
        , ( "Diminished Whole Half", ScaleClass.diminishedWholeToneHalfTone )
        , ( "Dorian ♭9", ScaleClass.dorianFlat9 )
        , ( "Dorian ♯11", ScaleClass.dorianSharp11 )
        , ( "Dorian", ScaleClass.dorian )
        , ( "Double Harmonic Minor", ScaleClass.doubleHarmonicMinor )
        , ( "Harmonic Minor", ScaleClass.harmonicMinor )
        , ( "Ionian ♯5", ScaleClass.ionianSharp5 )
        , ( "Ionian", ScaleClass.ionian )
        , ( "Leading Whole Tone", ScaleClass.leadingWholeTone )
        , ( "Locrian ♮13", ScaleClass.locrianNatural13 )
        , ( "Locrian ♮9", ScaleClass.locrianNatural9 )
        , ( "Locrian", ScaleClass.locrian )
        , ( "Lydian Augmented", ScaleClass.lydianAugmented )
        , ( "Lydian Diminished", ScaleClass.lydianDiminished )
        , ( "Lydian Dominant", ScaleClass.lydianDominant )
        , ( "Lydian Minor", ScaleClass.lydianMinor )
        , ( "Lydian ♯9", ScaleClass.lydianSharp9 )
        , ( "Lydian", ScaleClass.lydian )
        , ( "Major", ScaleClass.major )
        , ( "Major ♭2 Pentatonic", ScaleClass.majorFlat2Pentatonic )
        , ( "Major ♭6 Pentatonic", ScaleClass.majorFlat6Pentatonic )
        , ( "Melodic Minor", ScaleClass.melodicMinor )
        , ( "Minor 6 Pentatonic", ScaleClass.minor6Pentatonic )
        , ( "Minor 7 ♭5 Pentatonic", ScaleClass.minorFlat5Pentatonic )
        , ( "Minor Pentatonic", ScaleClass.minorPentatonic )
        , ( "Minor", ScaleClass.minor )
        , ( "Mixolydian ♭13", ScaleClass.mixolydianFlat13 )
        , ( "Mixolydian ♭9 ♭13", ScaleClass.mixolydianFlat9Flat13 )
        , ( "Mixolydian", ScaleClass.mixolydian )
        , ( "Phrygian", ScaleClass.phrygian )
        , ( "Six Tone Symmetrical", ScaleClass.sixToneSymmetrical )
        , ( "Whole Tone", ScaleClass.wholeTone )
        ]


roots : SelectList PitchClass
roots =
    SelectList.fromLists
        []
        (PitchClass.pitchClass C PitchClass.natural)
        [ PitchClass.pitchClass C PitchClass.sharp
        , PitchClass.pitchClass D PitchClass.flat
        , PitchClass.pitchClass D PitchClass.natural
        , PitchClass.pitchClass D PitchClass.sharp
        , PitchClass.pitchClass E PitchClass.flat
        , PitchClass.pitchClass E PitchClass.natural
        , PitchClass.pitchClass E PitchClass.sharp
        , PitchClass.pitchClass F PitchClass.natural
        , PitchClass.pitchClass F PitchClass.sharp
        , PitchClass.pitchClass G PitchClass.flat
        , PitchClass.pitchClass G PitchClass.natural
        , PitchClass.pitchClass G PitchClass.sharp
        , PitchClass.pitchClass A PitchClass.flat
        , PitchClass.pitchClass A PitchClass.natural
        , PitchClass.pitchClass A PitchClass.sharp
        , PitchClass.pitchClass B PitchClass.flat
        , PitchClass.pitchClass B PitchClass.natural
        ]


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


init : ( Model, Cmd Msg )
init =
    let
        range =
            Range.piano
                |> Range.setLowest (Pitch.pitch C PitchClass.natural Octave.three)
                |> Range.setHighest (Pitch.pitch B PitchClass.natural Octave.six)

        timeSignature =
            TimeSignature Four TimeSignature.Quarter

        noteDuration =
            Note.Eighth Note.None

        formula =
            [ 1 ]

        model =
            { range = range
            , formula = formula
            , formulaInput = Formula.serialize formula
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
            , advancedControls = False
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
                    ( { model | dialog = Just dialog, formulaInput = Formula.serialize model.formula }, Task.attempt (always NoOp) (Browser.Dom.focus "formula-input") )

                _ ->
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
            { model
                | startingNote = note
                , playingState = Stopped
            }
                |> renderNew model.playingState

        ScaleSelected scaleName ->
            { model
                | scales = model.scales |> SelectList.select (Tuple.first >> (==) scaleName)
                , playingState = Stopped
                , startingNote = SelectList.selected model.roots
            }
                |> renderNew model.playingState

        FormulaSelected formula ->
            { model
                | formula = formula
                , playingState = Stopped
                , formulaInput = Formula.serialize formula
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
            case input |> Formula.fromString of
                Just formula ->
                    { model | formula = formula, formulaInput = input, playingState = Stopped }
                        |> renderNew model.playingState

                Nothing ->
                    ( { model | formulaInput = input }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Audio.samplesLoaded SamplesLoaded
        , Browser.Events.onResize (\w h -> classifyDeviceFrom w h |> WindowResize)
        ]
