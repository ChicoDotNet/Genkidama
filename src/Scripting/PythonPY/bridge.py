class TvDevice:
    def power_on(self):
        return "TV:on"

    def mute(self):
        return "TV:muted"


class RadioDevice:
    def power_on(self):
        return "Radio:on"

    def mute(self):
        return "Radio:muted"


class BasicRemote:
    def __init__(self, device):
        self.device = device

    def activate(self):
        return self.device.power_on()


class MuteRemote:
    def __init__(self, device):
        self.device = device

    def activate(self):
        return self.device.mute()


tv = TvDevice()
radio = RadioDevice()
print(f"basic-tv={BasicRemote(tv).activate()}")
print(f"basic-radio={BasicRemote(radio).activate()}")
print(f"mute-tv={MuteRemote(tv).activate()}")
print(f"mute-radio={MuteRemote(radio).activate()}")
