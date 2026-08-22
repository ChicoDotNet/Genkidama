def makeDevice(String name) {
    [
        powerOn: { "${name}:on" },
        mute   : { "${name}:muted" }
    ]
}

def activateBasic(device) {
    device.powerOn()
}

def activateMute(device) {
    device.mute()
}

def tv = makeDevice('TV')
def radio = makeDevice('Radio')

println "basic-tv=${activateBasic(tv)}"
println "basic-radio=${activateBasic(radio)}"
println "mute-tv=${activateMute(tv)}"
println "mute-radio=${activateMute(radio)}"
