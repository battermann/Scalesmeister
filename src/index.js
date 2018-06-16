require('./main.css');
import registerServiceWorker from './registerServiceWorker';
import {Sampler, Sequence, Transport} from 'tone';
import Vex from 'vexflow';
const svg2pdf = require('svg2pdf.js');
const jsPDF = require('jspdf-yworks');

var Elm = require('./Main.elm');

var root = document.getElementById('root');
var app = Elm.Main.embed(root);

var sampler = null;
var sequence = null;

app.ports.downloadPdf.subscribe(function() {
  const svgElement = document.getElementById('score').lastChild;
  const width = 600, height = 400;

  // create a new jsPDF instance
  const pdf = new jsPDF('l', 'pt', [width, height]);

  // render the svg element
  svg2pdf(svgElement, pdf, {
    xOffset: 0,
    yOffset: 0,
    scale: 1
  });

  // or simply safe the created pdf
  pdf.save('luigi-score.pdf');
});

app.ports.renderScore.subscribe(function(input) {
  var elementId = input[0]
  var line = input[1]

  // remove previous score if exists
  var div = document.getElementById(elementId);
  var svg = div.lastChild;
  if (svg != null) {
    div.removeChild(svg);
  }

  var vf = new Vex.Flow.Factory({renderer: {elementId: elementId}});
  var score = vf.EasyScore();
  var system = vf.System();

  system.addStave({
    voices: [score.voice(score.notes(line), { time: '12/4' })]
  }).addClef('treble');

  vf.draw();
});

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
  var subdivision = "8n"
  sequence = new Sequence(function(_, note){
    sampler.triggerAttackRelease(note, noteLength)
  }, seq, subdivision);
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
