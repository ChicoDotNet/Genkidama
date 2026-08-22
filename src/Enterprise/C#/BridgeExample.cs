using System;

public interface IDevice
{
    string PowerOn();
    string Mute();
}

public sealed class TvDevice : IDevice
{
    public string PowerOn() => "TV:on";
    public string Mute() => "TV:muted";
}

public sealed class RadioDevice : IDevice
{
    public string PowerOn() => "Radio:on";
    public string Mute() => "Radio:muted";
}

public abstract class RemoteControl
{
    protected RemoteControl(IDevice device) => Device = device;
    protected IDevice Device { get; }
    public abstract string Activate();
}

public sealed class BasicRemote(IDevice device) : RemoteControl(device)
{
    public override string Activate() => Device.PowerOn();
}

public sealed class MuteRemote(IDevice device) : RemoteControl(device)
{
    public override string Activate() => Device.Mute();
}

public static class BridgeExample
{
    public static void Main()
    {
        var tv = new TvDevice();
        var radio = new RadioDevice();
        Console.WriteLine($"basic-tv={new BasicRemote(tv).Activate()}");
        Console.WriteLine($"basic-radio={new BasicRemote(radio).Activate()}");
        Console.WriteLine($"mute-tv={new MuteRemote(tv).Activate()}");
        Console.WriteLine($"mute-radio={new MuteRemote(radio).Activate()}");
    }
}
