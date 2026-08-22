type
  Action = proc (): string
  Device = object
    powerOn: Action
    mute: Action

proc makeDevice(name: string): Device =
  result.powerOn = proc (): string = name & ":on"
  result.mute = proc (): string = name & ":muted"

proc activateBasic(device: Device): string = device.powerOn()
proc activateMute(device: Device): string = device.mute()

let tv = makeDevice("TV")
let radio = makeDevice("Radio")

echo "basic-tv=" & activateBasic(tv)
echo "basic-radio=" & activateBasic(radio)
echo "mute-tv=" & activateMute(tv)
echo "mute-radio=" & activateMute(radio)
