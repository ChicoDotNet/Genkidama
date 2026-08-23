const std = @import("std");

const RenderFn = *const fn () []const u8;

fn plain() []const u8 {
    return "alert";
}

fn audit(inner: RenderFn, buffer: []u8) ![]const u8 {
    return try std.fmt.bufPrint(buffer, "audit({s})", .{inner()});
}

fn encrypt(inner: RenderFn, buffer: []u8) ![]const u8 {
    return try std.fmt.bufPrint(buffer, "enc({s})", .{inner()});
}

pub fn main() !void {
    var audit_buf: [64]u8 = undefined;
    var enc_buf: [64]u8 = undefined;
    var inner_buf: [64]u8 = undefined;
    var stacked_buf: [96]u8 = undefined;

    const audited = try audit(plain, audit_buf[0..]);
    const encrypted = try encrypt(plain, enc_buf[0..]);
    const inner = try encrypt(plain, inner_buf[0..]);
    const stacked = try std.fmt.bufPrint(stacked_buf[0..], "audit({s})", .{inner});

    std.debug.print("base={s}\naudit={s}\nencrypted={s}\nstacked={s}\n", .{ plain(), audited, encrypted, stacked });
}
