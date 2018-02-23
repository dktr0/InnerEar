function Oscillator (type, freq, db){
  var osc = ___ac.createOscillator();
  osc.type = type;
  osc.frequency.value = freq;
  var gain = ___ac.createGain();
  gain.gain.value = dbToAmp(db);

  this.oscillator = osc;
  this.gain = gain;
  this.oscillator.connect(this.gain)
  // return this
  this.frequency = osc.frequency;
  this.type = osc.type
}

Oscillator.prototype.start = function (){
  this.oscillator.start();
}

Oscillator.prototype.stop = function (){
  this.oscillator.stop();
}

// Oscillator.prototype.frequency = function (){
//   return this.oscillator.frequency
// }
//
// Oscillator.prototype.gain = function(){
//   return this.gain.gain;
// }

Oscillator.prototype.disconnect = function (a){
  if (a==undefined){
    this.gain.disconnect();
    this.oscillator.disconnect();
  } else{
    this.gain.disconnect(a)
  }
}

Oscillator.prototype.connect = function(a){
  this.gain.connect(a);
  return a;
}

Oscillator.prototype.setDb = function (db){
  this.gain.gain.value = dbToAmp(db)
}

Oscillator.prototype.setFreq = function (freq){
  this.oscillator.frequency.value = freq
}

Oscillator.prototype.setType = function (type){
  this.oscillator.type = type
}
