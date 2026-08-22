make_device(name) = (
    power_on = () -> "$(name):on",
    mute = () -> "$(name):muted",
)

activate_basic(device) = device.power_on()
activate_mute(device) = device.mute()

tv = make_device("TV")
radio = make_device("Radio")

println("basic-tv=" * activate_basic(tv))
println("basic-radio=" * activate_basic(radio))
println("mute-tv=" * activate_mute(tv))
println("mute-radio=" * activate_mute(radio))
