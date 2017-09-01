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

function createMediaNode (id){
  var node
  console.log(id)
  if (userAudioNodes[id]){
    var obj = userAudioNodes[id]
    console.log('is in here')
    if (obj.audioElement){
      node = ___ac.createMediaElementSource(obj.audioElement)
    } else{
      var audioElement = document.getElementById(id+"Audio")
      node = ___ac.createMediaElementSource(obj.audioElement)
    }
    userAudioNodes[id].node = node
  } else {
    console.log('else')
    var audioElement = document.getElementById(id+"Audio")
    node = ___ac.createMediaElementSource(audioElement)
    userAudioNodes[id] = {node:node, audioElement:audioElement}
  }
  return node
}

function loadBuffer(inputId){

  var inputElement = document.getElementById(inputId+"Input")

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
            // Set object of id's buffer to the decoded buffer
            if(userAudioNodes[inputId]){
              userAudioNodes[inputId].buffer = buffer
            } else {
              userAudioNodes[inputId] = {buffer:buffer}
            }

            // If the user hasn't changed the loopend, set loopend to the end of the soundfile
            if (userAudioNodes[inputId].loopEnd==undefined){
              userAudioNodes[inputId].loopEnd = buffer.duration
            }
            console.log("Buffer "+inputId+" successfully decoded.")
          },function(e){alert("Error decoding audio data, please try to load another file.")})
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

function createBufferSourceNodeFromID(id,start,end,loop){
  var source = ___ac.createBufferSource();
  source.loop = loop;
  userAudioNodes[id].start = start
  userAudioNodes[id].end = end
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
    

    if (x.inputElement.files){
      var file = files[0];
    

      var urlReader = new FileReader();
      // Get the file's URL
      urlReader.readAsDataURL(file)
      urlReader.addEventListener('loadend', function(e){
        console.log('url loaded')
        x.url = urlReader.result;
        x.audioElement.setAttribute('src',x.url)
      })
    } else {
      console.log ('error in setAudioSrc: no files for userAudioNodes['+id+']')
    }
  } else {
    var audioElement = document.getElementById(id+"Audio")
    var inputElement = document.getElementById(id+"Input")
    if (audioElement && inputElement){
      userAudioNodes[id].audioElement = audioElement
      userAudioNodes[id].inputElement = inputElement
      userAudioNodes[id].loopStart = 0;
      userAudioNodes[id].loopEnd = undefined; 
      userAudioNodes[id].audioElement.ontimeupdate = audioOnTimeUpdate(userAudioNodes[id])

    } else{
      console.log('Error in setAudioSrc: either no input or audio element for: '+id)
    }

  }

}

function audioOnTimeUpdate(obj){
  if (obj.audioElement.loop){
    if(obj.loopEnd==undefined){obj.loopEnd = obj.buffer.duration}
    if(obj.audioElement.currentTime<obj.loopStart){
      obj.audioElement.currentTime = obj.loopStart
    }
    if(obj.audioElement.currentTime>=obj.loopEnd){
      obj.audioElement.currentTime = obj.loopEnd
    }
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

      ctxL.lineTo(i,dataL[x]*100+100)
      ctxR.lineTo(i,dataR[x]*100+100)

    }
    ctxL.stroke()
    ctxR.stroke()

  } else{
  console.log("WARNING - canvas drawn before buffer loaded")
  }
}




