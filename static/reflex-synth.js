
var bufferData
var buffers = {}
var lastPlayingBufferNode;
var userAudioNodes = {}


function startSilentNode () {
  // a permanent buffernode sound  so the sound icon always displays at the
  // top of their window (so the icon doesn't sway student's responses in exercises such as)
  // the threshold of silence exercise)
  // This wasn't working with just a simple oscillator with a gain of 0 applied for some reason.
  //  (hence the looped buffernode)




  // 
  // var emptyArrayBuffer = ___ac.createBuffer(1, ___ac.sampleRate * 10,___ac.sampleRate );
  //
  // // Necessary for some reason to insert all 0's (even though it is initialized  as such)
  // for (var channel = 0; channel < emptyArrayBuffer.numberOfChannels; channel++) {
  //   var nowBuffering = emptyArrayBuffer.getChannelData(channel);
  //   for (var i = 0; i < emptyArrayBuffer.length; i++) {
  //     nowBuffering[i] = 0;
  //   }
  // }
  // var emptyNode = ___ac.createBufferSource()
  // emptyNode.buffer = emptyArrayBuffer
  // emptyNode.loop = true;
  // emptyNode.connect(___ac.destination)
  // emptyNode.start();

}

function test(s){
  console.log(s)
  console.log(typeof(s))
  var a = document.getElementById(s);
  while (a==null){
    a = document.getElementById(s);
  }
  console.log(a)

  return ___ac.createMediaElementSource(a);
}


function createScriptProcessorNode (onAudioFunc){
  var sp = ___ac.createScriptProcessor(undefined,2,2)
  sp.onaudioprocess = onAudioFunc;
  console.log('script processor node created')
  return sp;
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
            // outputData[sample] = Math.min(Math.max(inputData[sample],-1/2),1/2);
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
  node.start(___ac.currentTime, start, end)
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
     alert("Please load a sound file to use as a source or select pink noise  or white noise from the dropdown menu on the right.")
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

// function audioOnTimeUpdate(obj){
//   if (obj.audioElement.loop){
//     if(obj.loopEnd==undefined){obj.loopEnd = obj.buffer.duration}
//     if(obj.audioElement.currentTime<obj.loopStart){
//       obj.audioElement.currentTime = obj.loopStart
//     }
//     if(obj.audioElement.currentTime>=obj.loopEnd){
//       obj.audioElement.currentTime = obj.loopEnd
//     }
//   }
// }



// Probably don't need this anymore....
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
  if (lastPlayingBufferNode){
    lastPlayingBufferNode.stop()
  }
  console.log(node)
  node.start()
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
