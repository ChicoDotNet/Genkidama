const std = @import("std");

const Handler = struct {
    name: []const u8,
    limit: u32,
};

fn handle(handlers: []const Handler, amount: u32, visited: *std.ArrayList(u8), result: []u8) ![]const u8 {
    for (handlers, 0..) |handler, index| {
        if (index > 0) try visited.append('>');
        try visited.appendSlice(handler.name);
        if (amount <= handler.limit) {
            return std.fmt.bufPrint(result, "handled={s};result=refund({d})", .{ handler.name, amount });
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
    var visited_stream = std.io.fixedBufferStream(&visited_buffer);
    var visited = std.ArrayList(u8).initBuffer(visited_stream.buffer);
    var result_buffer: [64]u8 = undefined;
    const result = try handle(&handlers, 250, &visited, &result_buffer);

    var output: [160]u8 = undefined;
    const rendered = try std.fmt.bufPrint(&output, "visited={s};{s}", .{ visited.items, result });
    try std.fs.File.stdout().writeAll(rendered);
}
