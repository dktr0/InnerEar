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

function loadUserSoundFile(){
  console.log("got here.")
  var filePicker = document.getElementById('soundFileInput')
  var files = filePicker.files

  if(files[0]){

    var reader = new FileReader ();

    // console.log('files: '+files)
    // console.log('file: '+files[0])
    reader.readAsArrayBuffer(files[0])
    // var buff = reader.result
    // console.log("buff length is: " +buff.length());
    var url = URL.createObjectURL(files[0])
    console.log("url: "+url)

    var aud = document.getElementById('userAudio')
    aud.setAttribute('src',url)
  }
  else {
    alert("Please select a sound file")
  }
}