class Device:
    def __init__(self, name):
        self.name = name

    def power_on(self):
        return self.name + ":on"

    def mute(self):
        return self.name + ":muted"


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


tv = Device("TV")
radio = Device("Radio")
print("basic-tv=" + BasicRemote(tv).activate())
print("basic-radio=" + BasicRemote(radio).activate())
print("mute-tv=" + MuteRemote(tv).activate())
print("mute-radio=" + MuteRemote(radio).activate())
