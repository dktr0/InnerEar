var bufferData
var buffers = {}
var overlappedDictionary = {}
var lastPlayingBufferNode;
var userAudioNodes = {}

function startSilentNode () {
  // a permanent buffernode sound  so the sound icon always displays at the
  // top of their window (so the icon doesn't sway student's responses in exercises such as)
  // the threshold of silence exercise
  // This wasn't working with just a simple oscillator with a gain of 0 applied for some reason.
  //  (hence the looped buffernode)
  var emptyArrayBuffer = ___ac.createBuffer(1, ___ac.sampleRate * 10,___ac.sampleRate );
  // Necessary for some reason to insert all 0's (even though it is initialized  as such)
  for (var channel = 0; channel < emptyArrayBuffer.numberOfChannels; channel++) {
    var nowBuffering = emptyArrayBuffer.getChannelData(channel);
    for (var i = 0; i < emptyArrayBuffer.length; i++) {
      nowBuffering[i] = 0;
    }
  }
  var emptyNode = ___ac.createBufferSource()
  emptyNode.buffer = emptyArrayBuffer
  emptyNode.loop = true;
  emptyNode.connect(___ac.destination)
  emptyNode.start();
}


function createScriptProcessorNode (onAudioFunc){
  var sp = ___ac.createScriptProcessor(undefined,2,2)
  sp.onaudioprocess = onAudioFunc;
  console.log('script processor node created')
  return sp;
}

function createClipAtWaveShaper (db){
  var clip = dbToAmp(db)
  var shapeBuffer = new Float32Array(65536);
  var portion = (1-clip)/2
  var left = Math.floor(portion*shapeBuffer.length)
  var right = shapeBuffer.length-left

  for (var i =0; i<shapeBuffer.length; i=i+1){
    if (i < left){
      shapeBuffer[i] = ((-1)*clip)

    } else {
      if (i <right) {
        shapeBuffer[i] = 2*i/shapeBuffer.length-1;
      } else{
        shapeBuffer[i] = clip;
      }
    }
  }
  var distortion = ___ac.createWaveShaper()
  distortion.curve = shapeBuffer;
  return distortion
}

function connectAdditiveNode(listOfNodes, toNode){
  console.log("connecting...")
  for(var i=0; i< listOfNodes.length; i=i+1){
    listOfNodes[i].connect(toNode)
  }
}

//Need to stop and disconnect all sources
function stopOverlappedSound(id){
  if (overlappedDictionary[id] != undefined){
    var nodes = overlappedDictionary[id]

    for (var i=0; i<nodes.length; i=i+1){
      console.log("STOPING AN OVERLAPPED NODE NOW")
      console.log(typeof(nodes[i]))
      nodes[i].stop()
      nodes[i].disconnect();
    }
    overlappedDictionary[id] = undefined
  }
}

function getDistortAtDbFunc(db){
  if (db==undefined){
    console.log ("WARNING - spDistortAtDb wasn't provided an argument containing a decibel value - value of 0dB used")
    db=0
  }
  var clip = Math.pow (10, db/20)
  var func = function (audioProcessingEvent){
    var inputBuffer = audioProcessingEvent.inputBuffer;
        var outputBuffer = audioProcessingEvent.outputBuffer

        for (var channels =0; channels< outputBuffer.numberOfChannels; channels = channels+1){
          var inputData = inputBuffer.getChannelData(channels)
          var outputData = outputBuffer.getChannelData(channels)
          for (var sample = 0; sample<inputBuffer.length; sample = sample+1){
            outputData[sample] = Math.min(Math.max(inputData[sample],clip*(-1)),clip);
          }
        }
  }
  return func;
}

function setGain(db, node){
  var amp  = Math.pow(10,db/20)
  console.log("db:  "+db)
  console.log("amp: "+amp)
  node.gain.value = amp;
}

function createCompressorNode (threshold, knee, ratio, attack, release){
  var comp = ___ac.createDynamicsCompressor()
  comp.threshold.value = threshold;
  comp.knee.value = knee;
  comp.ratio.value = ratio;
  comp.attack.value = attack;
  comp.release.value = release;
  return comp;
}

// 'url' is a string to a local (ie.. on the server) impulse response buffer (audio file)
function createConvolverNode (url){
  var conv = ___ac.createConvolver();

  var request = new XMLHttpRequest();
  request.open('GET', url, true);
  request.responseType = 'arraybuffer';
  request.onload = function() {
    var audioData = request.response;

    ___ac.decodeAudioData(audioData, function(buffer) {
        conv.buffer = buffer;
      },
      function(e){ console.log("Error with decoding audio data " + e); });
  } //request onload
  request.send();
  return conv;
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
  return node;
}

function loadBuffer(inputId){

  if (userAudioNodes[inputId]==undefined){
      userAudioNodes[inputId]={}
  }
  var inputElement = document.getElementById(inputId)

  if (inputElement){

    var files = inputElement.files// function drawSineWave (canvas){
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



function playBufferNode(id, s, e, loop, node){
  // if(userAudioNodes[id].source){
    // userAudioNodes[id].source.stop();
  // }
  if (node.buffer){
  e = Math.max(Math.min(1,e),0)
  s = Math.max(Math.min(e,s),0)
  var start= s*node.buffer.duration
  var end = (e-s)*node.buffer.duration
  console.log('end:' + end)
  console.log('start' + start)
  node.loopStart = start;
  node.loopEnd = e*node.buffer.duration;
  node.loop = loop
  // node.start(___ac.currentTime, start, end)
  node.start(___ac.currentTime, start)

  lastPlayingBufferNode = node;
  }
  else {
    console.log("WARNING - playBufferNode called on a node with no buffer. [playBufferNode(id, s, e, loop, node)] ")
  }

}

function createBufferSourceNodeFromID(id,start,end,loop){
  var source = ___ac.createBufferSource();
  source.loop = loop;
  if (userAudioNodes[id]){
    userAudioNodes[id].start = start
    userAudioNodes[id].end = end
    if (userAudioNodes[id].buffer==undefined){
      console.log('WARNING: No buffer loaded')
      console.log('attempting to load buffer...')
      loadBuffer(id)
    }
    source.buffer = userAudioNodes[id].buffer
    userAudioNodes[id].source = source
    console.log("source:  "+source)
    return source;
  }
   else{
     alert("Please load a sound file to use as a source.")
   }
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



function renderAudioWaveform(id, canvas, attempt){

  if (attempt<5){___ac
    var obj = userAudioNodes[id]


    if (obj){
      if (obj.buffer){
        drawBufferOnCanvas(obj.buffer, canvas)
      }
      else{
        console.log('### WARNING attempted to renderAudioWaveform before buffer of \''+id+'\' was set...\n trying again');
        setTimeout(renderAudioWaveform(id,canvas,alert+1),2000)
      }
    } else {
      console.log('### WARNING attempted to renderAudioWaveform before buffer of \''+id+'\' was set...\n trying again');
      setTimeout(renderAudioWaveform(id,canvas,alert+1),2000)
    }
  }
  else {
    alert('Unable to properly load soundfile, you may need to reload the page.')
  }
}




function loadAndDrawBuffer(inputId, canvas){

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
            drawBufferOnCanvas(buffer,canvas)
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

function startNode(node){
  // if (lastPlayingBufferNode){
  //   lastPlayingBufferNode.stop()
  // }
  console.log(node)
  node.start()
}

// for starting an additive node - calls start on a list of nodes
function startNodes(l){
  for (var i=0; i<l.length; i=i+1){
    l[i].start();
  }
}

function stopNodeByID(id){
  var obj = userAudioNodes[id]
  if(obj){
    if (obj.source){
      obj.source.stop()
    }else {
      console.log("WARNING - tried to stop a node that does not exist")
    }
  }else{
    console.log("WARNING - stopNodeByID called on node with id: "+id+" which does not exist. Ignore this message if not trying to stop a loaded file source");
    console.log(userAudioNodes[id])
  }
}

function drawSineWave (canvas){
  console.log('drawing sine wave')
  var width = canvas.width;
  var height = canvas.height;
  var ctx = canvas.getContext("2d");
  ctx.fillStyle = "rgb(50,50,50)";
  ctx.fillRect(0,0,width,height)
  ctx.lineWidth=1;
  ctx.fillStyle = "rgb(100,170,200)";


  for (var i =0; i < canvas.width; i=i+1){
    var x = i;
    var y = (Math.sin(i*(Math.PI*2)/width)*(-1)*height/2+(height/2));
    ctx.fillRect(x, y, 1, 1);
  }

}

function drawBufferOnCanvas (buff, canvas) {
  if (buff){

    var width = canvas.width
    var height = canvas.height
    var ctx = canvas.getContext('2d')
    ctx.fillStyle = "rgb(50,50,50)"

    ctx.fillRect(0,0,width,height)
    ctx.lineWidth=1;
    ctx.fillStyle = "rgb(100,170,200)";

    var data = buff.getChannelData(0)

    var block = Math.ceil(data.length/width)
    var amp = height/2;


    for(var i=0; i < width; i++){
      var min = 1.0
      var max = -1.0
      for (var j=0; j<block; j++) {
        var x = data[j+i*block];
        if (x < min){
          min = x;
        }
        if (x > max){
          max = x;
        }
      }
      ctx.fillRect(i,(1+min)*amp,1,Math.max(1,(max-min)*amp));
    }
    console.log('done')
  } else{
    console.log("WARNING - canvas drawn before buffer loaded")
  }
}

function dbToAmp (db){
  return Math.pow (10, db/20)
}

function ampToDb (amp){
  return 20*Math.log(amp)
}
