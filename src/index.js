require('./main.css');
import registerServiceWorker from './registerServiceWorker';

var AudioContext = window.AudioContext || window.webkitAudioContext;
var audioContext = new AudioContext();

var Elm = require('./Main.elm');

var root = document.getElementById('root');
var app = Elm.Main.embed(root);

var samples = {};
var request = null;

app.ports.noteOn.subscribe(function(note) {
  var source = audioContext.createBufferSource();
  source.buffer = samples[note];
  source.connect(audioContext.destination);
  source.start();
});

app.ports.noteOff.subscribe(function() {
  console.log("off")
});

app.ports.loadSample.subscribe(function ( array ){
  var key = array[0];
  var url = array[1];

  if (samples[key] == null){
    var request = new XMLHttpRequest();
    request.open('GET', url, true);
    request.responseType = 'arraybuffer';
    request.onload = function () {
        if (request.status === 200) {
            var audioData = request.response;
            audioContext.decodeAudioData(audioData,
                function (audioBuffer){
                    samples[key]=audioBuffer;
                    console.log("Loaded", key, url);
                },
                function (e){
                    notifySampleLoadingFailed(url);
                });
        } else {
            notifySampleLoadingFailed(url);
        }
    }

    request.onerror = function () {
        notifySampleLoadingFailed(url);
    }

    request.send();
  }
});

var notifySampleLoadingFailed = function(url){
  console.log("Unable to load the sample @", url);
};

registerServiceWorker();
