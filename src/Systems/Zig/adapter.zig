const std = @import("std");

const LegacyFahrenheitSensor = struct {
    fn readFahrenheit(_: *const LegacyFahrenheitSensor) i32 {
        return 86;
    }
};

const FahrenheitSensorAdapter = struct {
    adaptee: *const LegacyFahrenheitSensor,

    fn readCelsius(self: *const FahrenheitSensorAdapter) i32 {
        const fahrenheit = self.adaptee.readFahrenheit();
        return @divTrunc((fahrenheit - 32) * 5, 9);
    }
};

pub fn main() void {
    const legacy = LegacyFahrenheitSensor{};
    const adapter = FahrenheitSensorAdapter{ .adaptee = &legacy };

    std.debug.print("legacy={d}F\n", .{legacy.readFahrenheit()});
    std.debug.print("adapted={d}C\n", .{adapter.readCelsius()});
}
