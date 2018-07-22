import {Sampler, Sequence, Transport, Synth} from 'tone';
import StartAudioContext from 'startaudiocontext';
import svg2pdf from 'svg2pdf.js';
import jsPDF from 'jspdf-yworks';
import abcjs from "abcjs";

import Elm from './Main.elm';

const root = document.getElementById('root');
const app = Elm.Main.embed(root);

var sampler = null;
var sequence = null;

var button = document.getElementById('audio-context');

StartAudioContext(Transport.context, button, function(){
  console.log('audio context started... foo')
			button.remove();
});

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
  const elementId = input[0];
  const score = input[1];
  abcjs.renderAbc(elementId, score);
});


app.ports.loadSamples.subscribe(function(pitchToSampleUrlMapping){
  const toObj = (array) =>
     array.reduce((obj, item) => {
       obj[item[0]] = item[1]
       return obj
     }, {});

  sampler = new Sampler(toObj(pitchToSampleUrlMapping), function() {
    app.ports.samplesLoaded.send("samples loaded");
  }).toMaster();
});

app.ports.startSequence.subscribe(function(seq){
  var debug = document.querySelector("#tone-debug");
  const noteLength = "8n"
  const subdivision = "8n"
  var synth = new Synth().toMaster()
  sequence = new Sequence(function(_, note){
    //sampler.triggerAttackRelease(note, noteLength)
    synth.triggerAttackRelease(note, noteLength)
    debug.textContent = note;
  }, seq, subdivision);

  sequence.start();
  Transport.bpm.value = 160;
  Transport.start("+0.1");
});

app.ports.stopSequence.subscribe(function(){
  Transport.stop()
  if (sequence != null)  {
    sequence.removeAll();
  };
});
