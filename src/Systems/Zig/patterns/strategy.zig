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

pub fn verifyStrategy() bool {
    return price(100, regular) == 100 and price(100, vip) == 80;
}

pub fn main() !void {
    if (!verifyStrategy()) {
        return error.StrategyContractFailed;
    }
    std.debug.print("regular=100;vip=80\n", .{});
}
