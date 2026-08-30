const std = @import("std");

fn authenticate(buffer: []u8, user: []const u8) ![]const u8 {
    return try std.fmt.bufPrint(buffer, "auth({s})", .{user});
}

fn reserve(buffer: []u8, sku: []const u8) ![]const u8 {
    return try std.fmt.bufPrint(buffer, "reserve({s})", .{sku});
}

fn charge(buffer: []u8, cents: u32) ![]const u8 {
    return try std.fmt.bufPrint(buffer, "charge({d})", .{cents});
}

fn checkout(buffer: []u8, user: []const u8, sku: []const u8, cents: u32) ![]const u8 {
    var auth_buffer: [64]u8 = undefined;
    var inventory_buffer: [64]u8 = undefined;
    var billing_buffer: [64]u8 = undefined;

    const auth = try authenticate(&auth_buffer, user);
    const inventory = try reserve(&inventory_buffer, sku);
    const billing = try charge(&billing_buffer, cents);

    return try std.fmt.bufPrint(buffer, "checkout={s}>{s}>{s}", .{ auth, inventory, billing });
}

pub fn main(init: std.process.Init) !void {
    var output_buffer: [192]u8 = undefined;
    const output = try checkout(&output_buffer, "alice", "SKU-42", 499);
    const stdout = std.Io.File.stdout();
    try stdout.writeStreamingAll(init.io, output);
    try stdout.writeStreamingAll(init.io, "\n");
}
