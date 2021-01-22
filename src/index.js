import '@fortawesome/fontawesome-free/css/all.css'
import { Elm } from './Main.elm'
import 'regenerator-runtime/runtime'
import * as Tone from 'tone'
import abcjs from 'abcjs'
import registerServiceWorker from './registerServiceWorker'

const unmuteAudio = require('unmute-ios-audio')

unmuteAudio()

const root = document.getElementById('root')

const app = Elm.Main.init({
  node: root
})

var sampler = null
var part = null
var clickTrack = null

Tone.Transport.bpm.value = 160

app.ports.renderScore.subscribe(function (input) {
  window.requestAnimationFrame(function () {
    const elementId = input[0]
    const score = input[1]
    abcjs.renderAbc(elementId, score)
  })
})

app.ports.loadSamples.subscribe(function (pitchToSampleUrlMapping) {
  const toObj = (array) =>
    array.reduce((obj, item) => {
      obj[item[0]] = window.location.protocol + '//' + window.location.host + '/' + item[1]
      return obj
    }, {})

  sampler = new Tone.Sampler(toObj(pitchToSampleUrlMapping), function () {
    app.ports.samplesLoaded.send(null)
  }).toDestination()
})

app.ports.setTempo.subscribe(function (tempo) {
  Tone.Transport.bpm.value = tempo
})

app.ports.setClickMute.subscribe(function (mute) {
  if (clickTrack != null) {
    clickTrack.mute = mute
  };
})

app.ports.startSequence.subscribe(function (data) {
  Tone.start().then((_) => {
    Tone.Transport.timeSignature = data.timeSignature
    Tone.Transport.loop = true
    Tone.Transport.loopEnd = data.loopEnd

    part = new Tone.Part(function (time, note) {
      sampler.triggerAttackRelease(note, data.noteLength, time)
    }, data.notes)

    setupClick(`${data.timeSignature[0]}/${data.timeSignature[1]}`, data.clickMuted)

    part.start(0)
    Tone.Transport.start('+0.1')
  })
})

app.ports.stopSequence.subscribe(function () {
  Tone.Transport.stop()
  if (part != null) {
    part.clear()
    part = null
  };

  if (clickTrack != null) {
    clickTrack.clear()
    clickTrack = null
  };
})

function setupClick (signature, mute) {
  switch (signature) {
    case '3/4':
      clickTrack = new Tone.Part(function (time, note) {
        metronome.triggerAttackRelease(note, 0.1, time)
      }, [[0, 'C6'], ['0:1', 'C5'], ['0:2', 'C5']])
      break
    case '4/4':
      clickTrack = new Tone.Part(function (time, note) {
        metronome.triggerAttackRelease(note, 0.1, time)
      }, [[0, 'C6'], ['0:1', 'C5'], ['0:2', 'C5'], ['0:3', 'C5']])
      break
    case '5/4':
      clickTrack = new Tone.Part(function (time, note) {
        metronome.triggerAttackRelease(note, 0.1, time)
      }, [[0, 'C6'], ['0:1', 'C5'], ['0:2', 'C5'], ['0:3', 'C5'], ['0:4', 'C5']])
      break
    case '6/4':
      clickTrack = new Tone.Part(function (time, note) {
        metronome.triggerAttackRelease(note, 0.1, time)
      }, [[0, 'C6'], ['0:1', 'C5'], ['0:2', 'C5'], ['0:3', 'C5'], ['0:4', 'C5'], ['0:5', 'C5']])
      break
    case '3/8':
      clickTrack = new Tone.Part(function (time, note) {
        metronome.triggerAttackRelease(note, 0.1, time)
      }, [[0, 'C6']])
      break
    case '5/8':
      clickTrack = new Tone.Part(function (time, note) {
        metronome.triggerAttackRelease(note, 0.1, time)
      }, [[0, 'C6'], ['4n.', 'C5']])
      break
    case '6/8':
      clickTrack = new Tone.Part(function (time, note) {
        metronome.triggerAttackRelease(note, 0.1, time)
      }, [[0, 'C6'], ['4n.', 'C5']])
      break
    case '7/8':
      clickTrack = new Tone.Part(function (time, note) {
        metronome.triggerAttackRelease(note, 0.1, time)
      }, [[0, 'C6'], ['2n', 'C5']])
      break
    case '9/8':
      clickTrack = new Tone.Part(function (time, note) {
        metronome.triggerAttackRelease(note, 0.1, time)
      }, [[0, 'C6'], ['4n.', 'C5'], ['2n.', 'C5']])
      break
    case '12/8':
      clickTrack = new Tone.Part(function (time, note) {
        metronome.triggerAttackRelease(note, 0.1, time)
      }, [[0, 'C6'], ['0:1.5', 'C5'], ['0:3', 'C5'], ['0:4.5', 'C5']])
      break
    default:
      console.log('unknown signature')
      clickTrack = new Tone.Part(function (time, note) { }, [])
      break
  }

  clickTrack.mute = mute
  clickTrack.loop = true
  clickTrack.start(0)
}

const metronome = new Tone.Synth({
  oscillator: {
    type: 'sine',
    modulationFrequency: 0.2
  },
  envelope: {
    attack: 0,
    decay: 0.1,
    sustain: 0,
    release: 0.1
  }
}).toDestination()

metronome.volume.value = -10

registerServiceWorker()
