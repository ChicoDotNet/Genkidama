const std = @import("std");

const PricingStrategy = *const fn (i32) i32;

fn price(value: i32, strategy: PricingStrategy) i32 {
    return strategy(value);
}

fn regular(value: i32) i32 {
    return value;
}

fn vip(value: i32) i32 {
    return @divTrunc(value * 80, 100);
}

pub fn main() !void {
    if (price(100, regular) != 100 or price(100, vip) != 80) {
        return error.StrategyContractFailed;
    }
    std.debug.print("regular=100;vip=80\n", .{});
}
