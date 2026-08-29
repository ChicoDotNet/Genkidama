const std = @import("std");

const Iterator = struct {
    values: []const i32,
    index: usize = 0,

    fn next(self: *Iterator) ?i32 {
        if (self.index >= self.values.len) return null;
        const value = self.values[self.index];
        self.index += 1;
        return value;
    }
};

pub fn main() !void {
    const values = [_]i32{ 10, 20, 30 };
    var iterator = Iterator{ .values = &values };
    var visited = [_]i32{ 0, 0, 0 };
    var index: usize = 0;

    while (iterator.next()) |value| {
        visited[index] = value;
        index += 1;
    }

    if (index != 3 or visited[0] != 10 or visited[1] != 20 or visited[2] != 30 or iterator.next() != null) {
        return error.IteratorContractFailed;
    }

    std.debug.print("iterator=10,20,30\n", .{});
}
