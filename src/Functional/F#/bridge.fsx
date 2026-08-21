type Device = {
    PowerOn: unit -> string
    Mute: unit -> string
}

let tv = {
    PowerOn = fun () -> "TV:on"
    Mute = fun () -> "TV:muted"
}

let radio = {
    PowerOn = fun () -> "Radio:on"
    Mute = fun () -> "Radio:muted"
}

let basicRemote (device: Device) = fun () -> device.PowerOn()
let muteRemote (device: Device) = fun () -> device.Mute()

printfn "basic-tv=%s" (basicRemote tv ())
printfn "basic-radio=%s" (basicRemote radio ())
printfn "mute-tv=%s" (muteRemote tv ())
printfn "mute-radio=%s" (muteRemote radio ())
