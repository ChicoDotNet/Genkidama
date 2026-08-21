trait Device {
    fn power_on(&self) -> &'static str;
    fn mute(&self) -> &'static str;
}

struct TvDevice;
impl Device for TvDevice {
    fn power_on(&self) -> &'static str { "TV:on" }
    fn mute(&self) -> &'static str { "TV:muted" }
}

struct RadioDevice;
impl Device for RadioDevice {
    fn power_on(&self) -> &'static str { "Radio:on" }
    fn mute(&self) -> &'static str { "Radio:muted" }
}

struct BasicRemote<'a, D: Device> { device: &'a D }
impl<D: Device> BasicRemote<'_, D> {
    fn activate(&self) -> &'static str { self.device.power_on() }
}

struct MuteRemote<'a, D: Device> { device: &'a D }
impl<D: Device> MuteRemote<'_, D> {
    fn activate(&self) -> &'static str { self.device.mute() }
}

fn main() {
    let tv = TvDevice;
    let radio = RadioDevice;
    println!("basic-tv={}", BasicRemote { device: &tv }.activate());
    println!("basic-radio={}", BasicRemote { device: &radio }.activate());
    println!("mute-tv={}", MuteRemote { device: &tv }.activate());
    println!("mute-radio={}", MuteRemote { device: &radio }.activate());
}
