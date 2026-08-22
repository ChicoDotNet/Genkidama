protocol Device {
    func powerOn() -> String
    func mute() -> String
}

struct TvDevice: Device {
    func powerOn() -> String { "TV:on" }
    func mute() -> String { "TV:muted" }
}

struct RadioDevice: Device {
    func powerOn() -> String { "Radio:on" }
    func mute() -> String { "Radio:muted" }
}

protocol RemoteControl {
    func activate() -> String
}

struct BasicRemote: RemoteControl {
    let device: any Device
    func activate() -> String { device.powerOn() }
}

struct MuteRemote: RemoteControl {
    let device: any Device
    func activate() -> String { device.mute() }
}

let tv = TvDevice()
let radio = RadioDevice()
print("basic-tv=\(BasicRemote(device: tv).activate())")
print("basic-radio=\(BasicRemote(device: radio).activate())")
print("mute-tv=\(MuteRemote(device: tv).activate())")
print("mute-radio=\(MuteRemote(device: radio).activate())")
