interface Device {
    String powerOn();
    String mute();
}

final class TvDevice implements Device {
    public String powerOn() { return "TV:on"; }
    public String mute() { return "TV:muted"; }
}

final class RadioDevice implements Device {
    public String powerOn() { return "Radio:on"; }
    public String mute() { return "Radio:muted"; }
}

abstract class RemoteControl {
    protected final Device device;
    protected RemoteControl(Device device) { this.device = device; }
    abstract String activate();
}

final class BasicRemote extends RemoteControl {
    BasicRemote(Device device) { super(device); }
    String activate() { return device.powerOn(); }
}

final class MuteRemote extends RemoteControl {
    MuteRemote(Device device) { super(device); }
    String activate() { return device.mute(); }
}

public final class BridgeExample {
    public static void main(String[] args) {
        Device tv = new TvDevice();
        Device radio = new RadioDevice();
        System.out.println("basic-tv=" + new BasicRemote(tv).activate());
        System.out.println("basic-radio=" + new BasicRemote(radio).activate());
        System.out.println("mute-tv=" + new MuteRemote(tv).activate());
        System.out.println("mute-radio=" + new MuteRemote(radio).activate());
    }
}
