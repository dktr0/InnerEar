var bufferData
var overlappedDictionary = {}
var lastPlayingBufferNode;
var userAudioNodes = {}


function Buffer (url) {
  if (typeof url !== 'string') 
    throw new Error("Smart Buffer needs a url as a string.");
  this.buffer = null;
  this.status = null;
  this.url = url;
  this.loadFile();
}

Buffer.prototype.loadFile = function () {
  var request = new XMLHttpRequest();
  try {
    request.open('GET',this.url, true);
    request.responseType = 'arrayBuffer';
    var closure = this;
    request.onload = function() {
      ___ac.decodeAudioData(request.response, function(x) {
        closure.buffer = x;
        closure.status = 'loaded';
      }, function (e) {
        closure.status = 'error';
        console.log('ERROR - could not decode audio buffer: ' + closure.url);
      });
    };
    request.send();
  } catch(e) {
    console.log(e);
  }
}

function showThings(a,b,c,d){
  console.log(a)
  console.log(b)
  console.log(c)
  console.log(d)
}

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

function disconnectAllAtTime (n,t){
  setTimeout(function(){
    try{
      n.disconnect();
    } catch(e){
      //for buffernodes that complain if you try to reference them after they've played...
    }
  }, t*1000)
}

//Need to stop and disconnect all sources
function stopOverlappedSound(id){
  if (overlappedDictionary[id] != undefined){
    var nodes = overlappedDictionary[id]

    for (var i=0; i<nodes.length; i=i+1){
      console.log("STOPING AN OVERLAPPED NODE NOW")
      try {
        console.log("gets here??????")
        nodes[i].stop()
        console.log("gets here2   ??????");
        nodes[i].disconnect();
        console.log("gets here3  ??????");
      } catch(e){
        console.log("hmm...")
      }
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


// Keep this - gets called when trying to play a LoadedFile where the buffer has not yet been loaded
// Callback for if want to also draw the buffer to a canvas after buffer is loaded
function loadBuffer(inputId, bufferDecodeCallback){
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
            userAudioNodes.file = file

            // If the user hasn't changed the loopend, set loopend to the end of the soundfile
            if (userAudioNodes[inputId].loopEnd==undefined){
              userAudioNodes[inputId].loopEnd = buffer.duration
            }
            console.log("Buffer "+inputId+" successfully decoded.")
            if (bufferDecodeCallback){
              bufferDecodeCallback(inputId)
            }
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

// For files hosted on the server like pinknoise.wav...
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
  if (node.buffer){
    e = Math.max(Math.min(1,e),0)
    s = Math.max(Math.min(e,s),0)
    var loopStart= s*node.buffer.duration
    var loopEnd = e*node.buffer.duration
    node.loopStart = loopStart;
    node.loopEnd = loopEnd;
    node.loop = loop
    node.start(___ac.currentTime, s*node.buffer.duration, (e-s)*node.buffer.duration)
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
     alert("Please load a sound file.")
   }
   return source;
}


// Necessary to have a single function that first loads and then draws the buffer
// because if tried to first load the buffer with reflex/haskell then draw the canvas,
// wouldn't have the callback function
// ie.:     <-  loadbuffer ....
//           <- drawBuffer .....
// won't work bc. can't get decode audio data callback from file loadBuffer...
function loadAndDrawBuffer(inputId, canvas){

  canvas.getContext('2d').clearRect(0,0,canvas.width, canvas.height)
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
      // alert('Please select a sound file to load')
    }
  } else {  // no element 'inputId'
    console.log('Could not find dom element with id: '+inputId)
    alert('An error occurred, you may need to reload the page')
  }
}


function drawFile (filePath, canvas){
  var request = new XMLHttpRequest();
  request.open('GET', filePath, true);
  request.responseType = 'arraybuffer';
  request.onload = function() {
    var audioData = request.response;
    ___ac.decodeAudioData(audioData, function(buffer) {
        drawBufferOnCanvas(buffer, canvas)
      },
      function(e){ console.log("Error with decoding audio data " + e); });
  } //request onload
  request.send();
}

function drawStartEnd(s, e, canvas){

  var ctx = canvas.getContext("2d")
  start = Math.round(Math.max(Math.min(s,1),0)*canvas.width);
  end = Math.round (Math.max(Math.min(e,1),0)*canvas.width);

  ctx.clearRect(0,0, canvas.width, canvas.height)
  ctx.strokeStyle = "#FF0000"
  ctx.beginPath()
  ctx.moveTo(start, 0)
  ctx.lineTo(start, canvas.height)
  ctx.stroke();

  ctx.moveTo(end, 0)
  ctx.lineTo(end, canvas.height)
  ctx.stroke();
}



function startNode(node){
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


// function createMediaNode (id){
//   var node
//   console.log(id)
//   if (userAudioNodes[id]){
//     var obj = userAudioNodes[id]
//     console.log('is in here')
//     if (obj.audioElement){
//       node = ___ac.createMediaElementSource(obj.audioElement)
//     } else{
//       var audioElement = document.getElementById(id+"Audio")
//       node = ___ac.createMediaElementSource(obj.audioElement)
//     }
//     userAudioNodes[id].node = node
//   } else {
//     console.log('else')
//     var audioElement = document.getElementById(id+"Audio")
//     node = ___ac.createMediaElementSource(audioElement)
//     userAudioNodes[id] = {node:node, audioElement:audioElement}
//   }
//   return node;
// }
