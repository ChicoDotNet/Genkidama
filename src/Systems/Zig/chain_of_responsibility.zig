const std = @import("std");

const Handler = struct {
    name: []const u8,
    limit: u32,
};

fn appendBytes(buffer: []u8, length: *usize, bytes: []const u8) !void {
    if (length.* + bytes.len > buffer.len) return error.NoSpaceLeft;
    for (bytes) |byte| {
        buffer[length.*] = byte;
        length.* += 1;
    }
}

fn handle(
    handlers: []const Handler,
    amount: u32,
    visited_buffer: []u8,
    visited_length: *usize,
    result_buffer: []u8,
) ![]const u8 {
    for (handlers, 0..) |handler, index| {
        if (index > 0) try appendBytes(visited_buffer, visited_length, ">");
        try appendBytes(visited_buffer, visited_length, handler.name);
        if (amount <= handler.limit) {
            return std.fmt.bufPrint(result_buffer, "handled={s};result=refund({d})", .{ handler.name, amount });
        }
    }
    return "handled=none;result=rejected";
}

pub fn main() !void {
    const handlers = [_]Handler{
        .{ .name = "faq", .limit = 50 },
        .{ .name = "billing", .limit = 500 },
        .{ .name = "escalation", .limit = std.math.maxInt(u32) },
    };

    var visited_buffer: [64]u8 = undefined;
    var visited_length: usize = 0;
    var result_buffer: [64]u8 = undefined;
    const result = try handle(&handlers, 250, &visited_buffer, &visited_length, &result_buffer);

    var output: [160]u8 = undefined;
    const rendered = try std.fmt.bufPrint(&output, "visited={s};{s}", .{ visited_buffer[0..visited_length], result });
    try std.fs.File.stdout().writeAll(rendered);
}
