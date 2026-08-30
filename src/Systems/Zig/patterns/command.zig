const std = @import("std");

const Command = union(enum) {
    deposit: i32,
    withdraw: i32,

    fn execute(self: Command, balance: i32) i32 {
        return switch (self) {
            .deposit => |amount| balance + amount,
            .withdraw => |amount| balance - amount,
        };
    }
};

pub fn main() void {
    const queue = [_]Command{ .{ .deposit = 50 }, .{ .withdraw = 20 } };
    var balance: i32 = 100;
    for (queue) |command| {
        balance = command.execute(balance);
    }
    std.debug.assert(balance == 130);
    std.debug.assert(queue.len == 2);
    std.debug.print("balance={d};commands={d}\n", .{ balance, queue.len });
}
