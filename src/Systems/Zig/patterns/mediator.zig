const std = @import("std");

const Colleague = enum(u8) {
    payment = 1,
    inventory = 2,
};

const Message = enum {
    paid,
    reserved,
};

const Delivery = struct {
    sender: Colleague,
    recipient: Colleague,
    message: Message,
};

const MediatorError = error{UnknownColleague};

const CheckoutMediator = struct {
    deliveries: [2]Delivery = undefined,
    count: usize = 0,

    fn send(
        self: *CheckoutMediator,
        sender: Colleague,
        recipient_code: u8,
        message: Message,
    ) MediatorError!void {
        const recipient: Colleague = switch (recipient_code) {
            @intFromEnum(Colleague.payment) => .payment,
            @intFromEnum(Colleague.inventory) => .inventory,
            else => return error.UnknownColleague,
        };

        std.debug.assert(self.count < self.deliveries.len);
        self.deliveries[self.count] = .{
            .sender = sender,
            .recipient = recipient,
            .message = message,
        };
        self.count += 1;
    }
};

fn paymentPaid(mediator: *CheckoutMediator) MediatorError!void {
    try mediator.send(
        .payment,
        @intFromEnum(Colleague.inventory),
        .paid,
    );
}

fn inventoryReserved(mediator: *CheckoutMediator) MediatorError!void {
    try mediator.send(
        .inventory,
        @intFromEnum(Colleague.payment),
        .reserved,
    );
}

pub fn verifyMediator() !bool {
    var mediator = CheckoutMediator{};

    try paymentPaid(&mediator);
    try inventoryReserved(&mediator);

    std.debug.assert(mediator.count == 2);
    std.debug.assert(mediator.deliveries[0].sender == .payment);
    std.debug.assert(mediator.deliveries[0].recipient == .inventory);
    std.debug.assert(mediator.deliveries[0].message == .paid);
    std.debug.assert(mediator.deliveries[1].sender == .inventory);
    std.debug.assert(mediator.deliveries[1].recipient == .payment);
    std.debug.assert(mediator.deliveries[1].message == .reserved);

    var rejected_unknown = false;
    mediator.send(.payment, 255, .paid) catch |err| {
        std.debug.assert(err == error.UnknownColleague);
        rejected_unknown = true;
    };
    std.debug.assert(rejected_unknown);

    return true;
}

pub fn main() !void {
    std.debug.assert(try verifyMediator());
    std.debug.print("Zig Mediator: passed\n", .{});
}
