// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library MediatorPattern {
    uint8 private constant PAYMENT = 1;
    uint8 private constant INVENTORY = 2;

    enum Message { Reserve, Reserved }
    enum Delivery { None, InventoryReserved, PaymentConfirmed }

    function send(
        uint8 sender,
        uint8 recipient,
        Message message_
    ) private pure returns (bool ok, Delivery delivery) {
        if (recipient == INVENTORY && sender == PAYMENT && message_ == Message.Reserve) {
            return (true, Delivery.InventoryReserved);
        }
        if (recipient == PAYMENT && sender == INVENTORY && message_ == Message.Reserved) {
            return (true, Delivery.PaymentConfirmed);
        }
        return (false, Delivery.None);
    }

    function run() internal pure returns (bool) {
        (bool outboundOk, Delivery outbound) = send(PAYMENT, INVENTORY, Message.Reserve);
        (bool inboundOk, Delivery inbound_) = send(INVENTORY, PAYMENT, Message.Reserved);
        (bool unknownOk, Delivery unknown) = send(PAYMENT, 3, Message.Reserve);

        return outboundOk
            && outbound == Delivery.InventoryReserved
            && inboundOk
            && inbound_ == Delivery.PaymentConfirmed
            && !unknownOk
            && unknown == Delivery.None;
    }
}
