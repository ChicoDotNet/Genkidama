interface Device {
  powerOn(): string;
  mute(): string;
}

class TvDevice implements Device {
  powerOn(): string { return "TV:on"; }
  mute(): string { return "TV:muted"; }
}

class RadioDevice implements Device {
  powerOn(): string { return "Radio:on"; }
  mute(): string { return "Radio:muted"; }
}

abstract class RemoteControl {
  constructor(protected readonly device: Device) {}
  abstract activate(): string;
}

class BasicRemote extends RemoteControl {
  activate(): string { return this.device.powerOn(); }
}

class MuteRemote extends RemoteControl {
  activate(): string { return this.device.mute(); }
}

const tv = new TvDevice();
const radio = new RadioDevice();
console.log(`basic-tv=${new BasicRemote(tv).activate()}`);
console.log(`basic-radio=${new BasicRemote(radio).activate()}`);
console.log(`mute-tv=${new MuteRemote(tv).activate()}`);
console.log(`mute-radio=${new MuteRemote(radio).activate()}`);
