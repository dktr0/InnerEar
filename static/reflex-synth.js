var bufferData

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

  var filePicker = document.getElementById('soundFileInput')
  var files = filePicker.files

  if(files[0]){

    var reader = new FileReader ();

    reader.readAsArrayBuffer(files[0])
    console.log('made it here ')
    reader.addEventListener('loadend',function(e){
      ___ac.decodeAudioData(reader.result, function(buffer){
        bufferData = buffer
        console.log('buffer data loaded and decoded')
      })

    })
    var url = URL.createObjectURL(files[0])
    console.log("url: "+url)

    var aud = document.getElementById('userAudio')
    aud.setAttribute('src',url)
  }
  else {
    alert("Please select a sound file")
  }
}

function drawBufferWaveform (canvasL,canvasR) {
  if (bufferData){
    // var canvasL = document.getElementById('canvasL')
    // var canvasR = document.getElementById('canvasR')
    var ctxL = canvasL.getContext('2d')
    var dataL = bufferData.getChannelData(0)
    var ctxR = canvasR.getContext('2d')
    var dataR = bufferData.getChannelData(1)

    ctxL.moveTo(0,100)

    ctxR.moveTo(0,100)

    var x = Math.round(dataL.length)
    console.log('made it here canvas')



    for (var i=0; i<canvasL.width; i++){
      // var x = Math.round(1000*i/dataL.length)

      var x = i*Math.round(dataL.length/canvasL.width)
      // if(x==Math.round(x)){
      console.log(x)
      ctxL.lineTo(i,dataL[x]*100+100)
      ctxR.lineTo(i,dataR[x]*100+100)

    }
    ctxL.stroke()
    ctxR.stroke()

  } else{
  console.log("WARNING - canvas drawn before buffer loaded")
}
}