

function startLoadingAndDecodingMultiple(list, cb){
  var closure = []
  var loaded = []
  for (i in list){

    console.log(i)
    console.log(list[i])
    console.log(loaded)
    list[i].startLoadingAndDecoding((x)=>{
      console.log("x is : " + x)
      console.log("x.status is : " + x.status)

      closure.push(x)
      var haveLoaded = true
      for (j in closure){
        haveLoaded = (closure[j].status == "error" || closure[j].status == "abort" || closure[j].status == "decoded")&&haveLoaded;
        if(haveLoaded){
          loaded.push(closure[j])
        }
      }
      console.log("cb test case:  haveloaded - "+(haveLoaded) +"  loadedlength - "+loaded.length);
      if (haveLoaded && loaded.length >= list.length  ){
        console.log("cb being called...")
        cb(closure)
      }
    });
  }
  console.log("closure:  "+closure)
}



// // Necessary to have a single function that first loads and then draws the buffer
// // because if tried to first load the buffer with reflex/haskell then draw the canvas,
// // wouldn't have the callback function
// // ie.:     <-  loadbuffer ....
// //           <- drawBuffer .....
// // won't work bc. can't get decode audio data callback from file loadBuffer...
// function loadAndDrawBuffer(inputId, canvas){
//
//   canvas.getContext('2d').clearRect(0,0,canvas.width, canvas.height)
//   var inputElement = document.getElementById(inputId)
//
//   if (inputElement){
//
//     var files = inputElement.files
//     if (files[0]){
//       var file = files[0]
//       var bufferReader = new FileReader ();
//       // Decode and store the buffer in userAudioNodes
//       bufferReader.readAsArrayBuffer(file)
//       bufferReader.addEventListener('loadend',function(e){
//         console.log('file loaded')
//         ___ac.decodeAudioData(bufferReader.result,
//           function(buffer){
//             // Set object of id's buffer to the decoded buffer
//             if(userAudioNodes[inputId]){
//               userAudioNodes[inputId].buffer = buffer
//             } else {
//               userAudioNodes[inputId] = {buffer:buffer}
//             }
//
//             // If the user hasn't changed the loopend, set loopend to the end of the soundfile
//             if (userAudioNodes[inputId].loopEnd==undefined){
//               userAudioNodes[inputId].loopEnd = buffer.duration
//             }
//             console.log("Buffer "+inputId+" successfully decoded.")
//             drawBufferOnCanvas(buffer,canvas)
//           },function(e){alert("Error decoding audio data, please try to load another file.")})
//       })
//
//     } else {
//       // alert('Please select a sound file to load')
//     }
//   } else {  // no element 'inputId'
//     console.log('Could not find dom element with id: '+inputId)
//     alert('An error occurred, you may need to reload the page')
//   }
// }

//
// function drawFile (filePath, canvas){
//   var request = new XMLHttpRequest();
//   request.open('GET', filePath, true);
//   request.responseType = 'arraybuffer';
//   request.onload = function() {
//     var audioData = request.response;
//     ___ac.decodeAudioData(audioData, function(buffer) {
//         drawBufferOnCanvas(buffer, canvas)
//       },
//       function(e){ console.log("Error with decoding audio data " + e); });
//   } //request onload
//   request.send();
// }
//
// function drawStartEnd(s, e, canvas){
//
//   var ctx = canvas.getContext("2d")
//   start = Math.round(Math.max(Math.min(s,1),0)*canvas.width);
//   end = Math.round (Math.max(Math.min(e,1),0)*canvas.width);
//
//   ctx.clearRect(0,0, canvas.width, canvas.height)
//   ctx.strokeStyle = "#FF0000"
//   ctx.beginPath()
//   ctx.moveTo(start, 0)
//   ctx.lineTo(start, canvas.height)
//   ctx.stroke();
//
//   ctx.moveTo(end, 0)
//   ctx.lineTo(end, canvas.height)
//   ctx.stroke();
// }



// function startNode(node){
//   console.log(node)
//   node.start()
// }

// // for starting an additive node - calls start on a list of nodes
// function startNodes(l){
//   for (var i=0; i<l.length; i=i+1){
//     l[i].start();
//   }
// }

// function stopNodeByID(id){
//   var obj = userAudioNodes[id]
//   if(obj){
//     if (obj.source){
//       obj.source.stop()
//     }else {
//       console.log("WARNING - tried to stop a node that does not exist")
//     }
//   }else{
//     console.log("WARNING - stopNodeByID called on node with id: "+id+" which does not exist. Ignore this message if not trying to stop a loaded file source");
//     console.log(userAudioNodes[id])
//   }
// }

// function drawSineWave (canvas){
//   console.log('drawing sine wave')
//   var width = canvas.width;
//   var height = canvas.height;
//   var ctx = canvas.getContext("2d");
//   ctx.fillStyle = "rgb(50,50,50)";
//   ctx.fillRect(0,0,width,height)
//   ctx.lineWidth=1;
//   ctx.fillStyle = "rgb(100,170,200)";
//
//
//   for (var i =0; i < canvas.width; i=i+1){
//     var x = i;
//     var y = (Math.sin(i*(Math.PI*2)/width)*(-1)*height/2+(height/2));
//     ctx.fillRect(x, y, 1, 1);
//   }
//
// }


function drawSourceErrorOnCanvas(canvas, errormsg){
  var width = canvas.width;
  var height = canvas.height;
  var ctx = canvas.getContext('2d');
  ctx.fillStyle = "rgb(255,20,20)"
  ctx.fillRect(0,0,width,height);
  console.log("Error with buffer"+errormsg)
}

function drawSourceUnderSpecifiedOnCanvas(canvas) {
  drawSourceErrorOnCanvas(canvas, "Source under specified")
}

function drawSourceLoadingOnCanvas(canvas){
  var width = canvas.width;
  var height = canvas.height;
  var ctx = canvas.getContext('2d');
  ctx.fillStyle = "rgb(50,50,50)"
  ctx.fillRect(0,0,width,height);
  ctx.lineWidth = 1;
  for (var i = 1; i<4; i++){
    ctx.beginPath();
    ctx.arc(width*i/3-width/6,height/2, height/8,0,2*Math.PI);
    ctx.fillStyle = "rgb(100,170,200)";
    ctx.stroke()
  }
}


// TODO finish this for other osc types
function drawOscOnCanvas(canvas, oscType, oscFreq){
  var width = canvas.width;
  var height = canvas.height;
  var ctx = canvas.getContext('2d');
  ctx.fillStyle = "rgb(50,50,50)"
  ctx.fillRect(0,0,width,height);
  ctx.lineWidth = 1;
  ctx.fillStyle = "rgb(100,170,200)"
  var oscFunc;
  if (oscType.toLowerCase() =="sine"){
    oscFunc = Math.sin
  } else  {
    console.log('osc type not yet implemented...')
    oscFunc = Math.sin
  }

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
    console.log("WARNING - tried to draw buffer before buffer loaded")
  }
}

// function dbToAmp (db){
//   return Math.pow (10, db/20)
// }

// function ampToDb (amp){
//   return 20*Math.log(amp)
// }
