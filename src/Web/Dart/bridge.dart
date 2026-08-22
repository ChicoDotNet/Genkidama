abstract interface class Device {
  String powerOn();
  String mute();
}

class TvDevice implements Device {
  @override
  String powerOn() => 'TV:on';
  @override
  String mute() => 'TV:muted';
}

class RadioDevice implements Device {
  @override
  String powerOn() => 'Radio:on';
  @override
  String mute() => 'Radio:muted';
}

abstract class RemoteControl {
  RemoteControl(this.device);
  final Device device;
  String activate();
}

class BasicRemote extends RemoteControl {
  BasicRemote(super.device);
  @override
  String activate() => device.powerOn();
}

class MuteRemote extends RemoteControl {
  MuteRemote(super.device);
  @override
  String activate() => device.mute();
}

void main() {
  final tv = TvDevice();
  final radio = RadioDevice();
  print('basic-tv=${BasicRemote(tv).activate()}');
  print('basic-radio=${BasicRemote(radio).activate()}');
  print('mute-tv=${MuteRemote(tv).activate()}');
  print('mute-radio=${MuteRemote(radio).activate()}');
}
