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

pub fn runIteratorExample() bool {
    const values = [_]i32{ 10, 20, 30 };
    var iterator = Iterator{ .values = &values };
    var visited = [_]i32{ 0, 0, 0 };
    var index: usize = 0;

    while (iterator.next()) |value| {
        visited[index] = value;
        index += 1;
    }

    return index == 3 and visited[0] == 10 and visited[1] == 20 and visited[2] == 30 and iterator.next() == null;
}

pub fn main() !void {
    if (!runIteratorExample()) return error.IteratorContractFailed;
    std.debug.print("iterator=10,20,30\n", .{});
}
