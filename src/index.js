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
var player = null
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

  player = new Tone.Player(window.location.protocol + '//' + window.location.host + '/samples/click.mp3').toDestination()

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

    clickTrack = new Tone.Part(function (time, note) {
      player.start(time)
    }, data.clicks)

    clickTrack.mute = data.clickMuted
    clickTrack.loop = true
    clickTrack.start(0)

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

registerServiceWorker()
