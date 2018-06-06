require('./main.css');
import registerServiceWorker from './registerServiceWorker';
import {Sampler} from 'tone';

var AudioContext = window.AudioContext || window.webkitAudioContext;
var audioContext = new AudioContext();

var Elm = require('./Main.elm');

var root = document.getElementById('root');
var app = Elm.Main.embed(root);

var sampler = null;

app.ports.noteOn.subscribe(function(pitch) {
  sampler.triggerAttack(pitch)
});

app.ports.noteOff.subscribe(function(pitch) {
  sampler.triggerRelease(pitch)
});

app.ports.loadSamples.subscribe(function ( pitchToSampleUrlMapping ){
  const toObj = (array) =>
     array.reduce((obj, item) => {
       obj[item[0]] = item[1]
       return obj
     }, {})

  sampler = new Sampler(toObj(pitchToSampleUrlMapping)).toMaster();
});

registerServiceWorker();
