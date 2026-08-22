const std = @import("std");

const Registry = struct {
    count: u32 = 0,
};

var shared_registry = Registry{};

fn instance() *Registry {
    return &shared_registry;
}

pub fn main() void {
    const first = instance();
    const second = instance();
    first.count += 1;
    std.debug.print("same={s}\n", .{if (first == second) "true" else "false"});
    std.debug.print("count={d}\n", .{second.count});
}
