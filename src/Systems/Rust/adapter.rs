trait TemperatureReader {
    fn read_celsius(&self) -> i32;
}

struct LegacyFahrenheitSensor;

impl LegacyFahrenheitSensor {
    fn read_fahrenheit(&self) -> i32 {
        86
    }
}

struct FahrenheitSensorAdapter<'a> {
    sensor: &'a LegacyFahrenheitSensor,
}

impl TemperatureReader for FahrenheitSensorAdapter<'_> {
    fn read_celsius(&self) -> i32 {
        (self.sensor.read_fahrenheit() - 32) * 5 / 9
    }
}

fn main() {
    let legacy = LegacyFahrenheitSensor;
    let reader = FahrenheitSensorAdapter { sensor: &legacy };
    println!("legacy={}F", legacy.read_fahrenheit());
    println!("adapted={}C", reader.read_celsius());
}
