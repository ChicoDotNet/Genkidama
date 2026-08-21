const std = @import("std");

const Device = struct {
    power_on: *const fn () []const u8,
    mute: *const fn () []const u8,
};

fn tvOn() []const u8 {
    return "TV:on";
}

fn tvMute() []const u8 {
    return "TV:muted";
}

fn radioOn() []const u8 {
    return "Radio:on";
}

fn radioMute() []const u8 {
    return "Radio:muted";
}

fn activateBasic(device: Device) []const u8 {
    return device.power_on();
}

fn activateMute(device: Device) []const u8 {
    return device.mute();
}

pub fn main() void {
    const tv = Device{ .power_on = tvOn, .mute = tvMute };
    const radio = Device{ .power_on = radioOn, .mute = radioMute };
    std.debug.print("basic-tv={s}\n", .{activateBasic(tv)});
    std.debug.print("basic-radio={s}\n", .{activateBasic(radio)});
    std.debug.print("mute-tv={s}\n", .{activateMute(tv)});
    std.debug.print("mute-radio={s}\n", .{activateMute(radio)});
}
