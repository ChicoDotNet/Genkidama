interface Device {
    fun powerOn(): String
    fun mute(): String
}

class TvDevice : Device {
    override fun powerOn() = "TV:on"
    override fun mute() = "TV:muted"
}

class RadioDevice : Device {
    override fun powerOn() = "Radio:on"
    override fun mute() = "Radio:muted"
}

abstract class RemoteControl(protected val device: Device) {
    abstract fun activate(): String
}

class BasicRemote(device: Device) : RemoteControl(device) {
    override fun activate() = device.powerOn()
}

class MuteRemote(device: Device) : RemoteControl(device) {
    override fun activate() = device.mute()
}

fun main() {
    val tv = TvDevice()
    val radio = RadioDevice()
    println("basic-tv=${BasicRemote(tv).activate()}")
    println("basic-radio=${BasicRemote(radio).activate()}")
    println("mute-tv=${MuteRemote(tv).activate()}")
    println("mute-radio=${MuteRemote(radio).activate()}")
}
