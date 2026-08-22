class TvDevice {
  powerOn() { return "TV:on"; }
  mute() { return "TV:muted"; }
}

class RadioDevice {
  powerOn() { return "Radio:on"; }
  mute() { return "Radio:muted"; }
}

class BasicRemote {
  constructor(device) { this.device = device; }
  activate() { return this.device.powerOn(); }
}

class MuteRemote {
  constructor(device) { this.device = device; }
  activate() { return this.device.mute(); }
}

const tv = new TvDevice();
const radio = new RadioDevice();
console.log(`basic-tv=${new BasicRemote(tv).activate()}`);
console.log(`basic-radio=${new BasicRemote(radio).activate()}`);
console.log(`mute-tv=${new MuteRemote(tv).activate()}`);
console.log(`mute-radio=${new MuteRemote(radio).activate()}`);
