function getBufferSourceNode(url) {
  source = ___ac.createBufferSource();
  var request = new XMLHttpRequest();

  request.open('GET', url, true);

  request.responseType = 'arraybuffer';


  request.onload = function() {
    var audioData = request.response;

    ___ac.decodeAudioData(audioData, function(buffer) {
        source.buffer = buffer;
      },

      function(e){ console.log("Error with decoding audio data " + e); });
  }
  request.send();
  return source;
}