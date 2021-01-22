import '@fortawesome/fontawesome-free/css/all.css'
import { Elm } from './Main.elm'
import { Sampler, Transport, Part, Player } from 'tone'
import abcjs from 'abcjs'
import registerServiceWorker from './registerServiceWorker'

const root = document.getElementById('root')

const app = Elm.Main.init({
  node: root
})

var sampler = null
var part = null
var player = null
var clickTrack = null

Transport.bpm.value = 160

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

  player = new Player(window.location.protocol + '//' + window.location.host + '/samples/click.mp3').toMaster()

  sampler = new Sampler(toObj(pitchToSampleUrlMapping), function () {
    app.ports.samplesLoaded.send(null)
  }).toMaster()
})

app.ports.setTempo.subscribe(function (tempo) {
  Transport.bpm.value = tempo
})

app.ports.setClickMute.subscribe(function (mute) {
  if (clickTrack != null) {
    clickTrack.mute = mute
  };
})

app.ports.startSequence.subscribe(function (data) {
  Transport.timeSignature = data.timeSignature
  Transport.loop = true
  Transport.loopEnd = data.loopEnd

  part = new Part(function (time, note) {
    sampler.triggerAttackRelease(note, data.noteLength, time)
  }, data.notes)

  clickTrack = new Part(function (time, note) {
    player.start(time)
  }, data.clicks)

  clickTrack.mute = data.clickMuted
  clickTrack.start(0)
  clickTrack.loop = true

  part.start(0)
  Transport.start('+0.1')
})

app.ports.stopSequence.subscribe(function () {
  Transport.stop()
  if (part != null) {
    part.removeAll()
    part = null
  };

  if (clickTrack != null) {
    clickTrack.removeAll()
    clickTrack = null
  };
})

registerServiceWorker()
