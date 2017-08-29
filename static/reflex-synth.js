var bufferData
var buffers = {}

var userAudioNodes = {}


function test(s){
  console.log(s)
  console.log(typeof(s))
  var a = document.getElementById(s);
  while (a==null){
    a = document.getElementById(s);
  }
  console.log(a)

  return ___ac.createMediaElementSource(a)
}


function loadBuffer(inputId){

  var inputElement = document.getElementById(inputId)

  if (inputElement){
    var files = inputElement.files
    
    if (files[0]){
      var file = files[0]
      var bufferReader = new FileReader ();

      // Decode and store the buffer in userAudioNodes
      bufferReader.readAsArrayBuffer(file)
      bufferReader.addEventListener('loadend',function(e){
        console.log('file loaded')
        ___ac.decodeAudioData(bufferReader.result,
          function(buffer){
            userAudioNodes[inputId].buffer = buffer
            console.log("Buffer "+inputId+" successfully decoded.")
        },function(e){
            alert("Error decoding audio data, please try to load another file.")
        })
      })

    } else {
      alert('Please select a sound file to load')
    }
  } else {  // no element 'inputId'
    console.log('Could not find dom element with id: '+inputId)
    alert('An error occurred, you may need to reload the page')
  }
}

function playMediaNode(s){
  var a = document.getElementById(s);
  if (a){
    a.play()
  } else {
    console.log('Error playing media node')
  }
}

function createBufferSourceNodeFromURL(url) {
  var source = ___ac.createBufferSource()
  var request = new XMLHttpRequest();
  request.open('GET', url, true);
  request.responseType = 'arraybuffer';
  request.onload = function() {
    var audioData = request.response;

    ___ac.decodeAudioData(audioData, function(buffer) {
        source.buffer = buffer;
      },
      function(e){ console.log("Error with decoding audio data " + e); });
  } //request onload
  request.send();
  return source;
}

function createBufferSourceNodeFromID(id){
  var source = ___ac.createBufferSource();

  if (userAudioNodes[id].buffer==undefined){
    console.log('WARNING: No buffer loaded')
    console.log('attempting to load buffer...')
    loadBuffer(id)
  } 
  source.buffer = buffers[id]
  return source
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
    console.log("unsucessful load user soundfile")
  }
}

function setAudioSrc(id){
  if (userAudioNodes[id]){
    var x = userAudioNodes[id]
    // loads buffer and sets userAduioNodes[id].buffer to result
    loadBuffer(id)
    

      // Get the file's URL
      urlReader.readAsDataURL(file)
      urlReader.addEventListener('loadend', function(e){
        console.log('url loaded')
        userAudioNodes[inputId].url = urlReader.result;
      })






    x.audioElement.setAttribute('src',)

  } else {
    var audioEl = document.getElementById(id++"Audio")
    var inputEl = document.getElementById(id++"Input")
    userAudioNodes[id] = {}
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
    
    ctxL.clearRect(0, 0, canvasL.width, canvasL.height);
    ctxR.clearRect(0, 0, canvasR.width, canvasR.height);


    ctxL.moveTo(0,100)

    ctxR.moveTo(0,100)

    var x = Math.round(dataL.length)
    console.log('made it here canvas')



    for (var i=0; i<canvasL.width; i++){
      // var x = Math.round(1000*i/dataL.length)
      var x = i*Math.round(dataL.length/canvasL.width)
      // if(x==Math.round(x)){
      ctxL.lineTo(i,dataL[x]*100+100)
      ctxR.lineTo(i,dataR[x]*100+100)

    }
    ctxL.stroke()
    ctxR.stroke()

  } else{
  console.log("WARNING - canvas drawn before buffer loaded")
}
}