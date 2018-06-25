import './main.css';
import registerServiceWorker from './registerServiceWorker';
import {Sampler, Sequence, Transport} from 'tone';
import Vex from 'vexflow';
import svg2pdf from 'svg2pdf.js';
import jsPDF from 'jspdf-yworks';

import Elm from './Main.elm';

const root = document.getElementById('root');
const app = Elm.Main.embed(root);

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
  const elementId = input[0]
  const line = input[1]
  const numerator = input[2]
  const denominator = input[3]

  // remove previous score if exists
  const div = document.getElementById(elementId);
  const svg = div.lastChild;
  if (svg != null) {
    div.removeChild(svg);
  }

  const vf = new Vex.Flow.Factory({renderer: {elementId: elementId}});
  const score = vf.EasyScore();
  const system = vf.System();

  system.addStave({
    voices: [score.voice(score.notes(line), { time: numerator + '/' + denominator })]
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
  const noteLength = "8n"
  const subdivision = "8n"
  sequence = new Sequence(function(_, note){
    sampler.triggerAttackRelease(note, noteLength)
  }, seq, subdivision);

  sequence.start();
  Transport.start("+0.1")
});

app.ports.stopSequence.subscribe(function(){
  Transport.stop()
  if (sequence != null)  {
    sequence.removeAll();
  }
});

registerServiceWorker();
