const std = @import("std");

const GateState = enum {
    locked,
    unlocked,
};

const GateAction = enum {
    lock,
    unlock,
};

fn transition(state: GateState, action: GateAction) GateState {
    return switch (state) {
        .locked => if (action == .unlock) .unlocked else .locked,
        .unlocked => if (action == .lock) .locked else .unlocked,
    };
}

pub fn runContract() bool {
    const initial: GateState = .locked;
    if (transition(initial, .lock) != .locked) return false;

    const unlocked = transition(initial, .unlock);
    if (unlocked != .unlocked) return false;
    if (transition(unlocked, .unlock) != .unlocked) return false;

    return transition(unlocked, .lock) == .locked;
}

pub fn main() !void {
    if (!runContract()) return error.StateContractFailed;
    std.debug.print("zig-state: passed\n", .{});
}
