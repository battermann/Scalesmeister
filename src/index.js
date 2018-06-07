require('./main.css');
import registerServiceWorker from './registerServiceWorker';
import {Sampler, Sequence, Transport} from 'tone';

var Elm = require('./Main.elm');

var root = document.getElementById('root');
var app = Elm.Main.embed(root);

var sampler = null;
var sequence = null;

app.ports.noteOn.subscribe(function(pitch) {
  sampler.triggerAttack(pitch)
});

app.ports.noteOff.subscribe(function(pitch) {
  sampler.triggerRelease(pitch)
});

app.ports.loadSamples.subscribe(function(pitchToSampleUrlMapping){
  const toObj = (array) =>
     array.reduce((obj, item) => {
       obj[item[0]] = item[1]
       return obj
     }, {})

  sampler = new Sampler(toObj(pitchToSampleUrlMapping)).toMaster();
});

app.ports.startSequence.subscribe(function(seq){
  var noteLength = "8n"
  sequence = new Sequence(function(time, note){
    sampler.triggerAttackRelease(note, noteLength)
  }, seq, noteLength);
  Transport.start()
  sequence.start();
});

app.ports.stopSequence.subscribe(function(){
  Transport.stop()
  if (sequence != null)  {
    sequence.removeAll();
  }
});

registerServiceWorker();
