extends SceneTree

class Device:
    var name: String
    func _init(device_name: String):
        name = device_name
    func power_on() -> String:
        return name + ":on"
    func mute() -> String:
        return name + ":muted"

class BasicRemote:
    var device: Device
    func _init(target: Device):
        device = target
    func activate() -> String:
        return device.power_on()

class MuteRemote:
    var device: Device
    func _init(target: Device):
        device = target
    func activate() -> String:
        return device.mute()

func _initialize() -> void:
    var tv := Device.new("TV")
    var radio := Device.new("Radio")
    print("basic-tv=" + BasicRemote.new(tv).activate())
    print("basic-radio=" + BasicRemote.new(radio).activate())
    print("mute-tv=" + MuteRemote.new(tv).activate())
    print("mute-radio=" + MuteRemote.new(radio).activate())
    quit()
