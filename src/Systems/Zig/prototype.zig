const std = @import("std");

const ServiceProfile = struct {
    name: [32]u8,
    name_len: usize,
    features: [64]u8,
    features_len: usize,

    fn init(name: []const u8, features: []const u8) ServiceProfile {
        var profile = ServiceProfile{
            .name = [_]u8{0} ** 32,
            .name_len = name.len,
            .features = [_]u8{0} ** 64,
            .features_len = features.len,
        };
        @memcpy(profile.name[0..name.len], name);
        @memcpy(profile.features[0..features.len], features);
        return profile;
    }

    fn clone(self: ServiceProfile) ServiceProfile {
        return self;
    }

    fn setName(self: *ServiceProfile, value: []const u8) void {
        @memset(self.name[0..], 0);
        self.name_len = value.len;
        @memcpy(self.name[0..value.len], value);
    }

    fn addFeature(self: *ServiceProfile, value: []const u8) void {
        self.features[self.features_len] = ',';
        self.features_len += 1;
        @memcpy(self.features[self.features_len..self.features_len + value.len], value);
        self.features_len += value.len;
    }

    fn print(self: ServiceProfile, label: []const u8) void {
        std.debug.print("{s}={s}: {s}\n", .{ label, self.name[0..self.name_len], self.features[0..self.features_len] });
    }
};

pub fn main() void {
    const original = ServiceProfile.init("orders", "metrics");
    var canary = original.clone();
    canary.setName("orders-canary");
    canary.addFeature("tracing");

    original.print("original");
    canary.print("clone");
}
