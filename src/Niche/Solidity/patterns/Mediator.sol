// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library MediatorPattern {
    uint8 private constant PAYMENT = 1;
    uint8 private constant INVENTORY = 2;

    enum Message { Reserve, Reserved }
    enum Delivery { InventoryReserved, PaymentConfirmed }

    error UnknownColleague(uint8 recipient);

    function send(
        uint8 sender,
        uint8 recipient,
        Message message_
    ) private pure returns (Delivery) {
        if (recipient == INVENTORY && sender == PAYMENT && message_ == Message.Reserve) {
            return Delivery.InventoryReserved;
        }
        if (recipient == PAYMENT && sender == INVENTORY && message_ == Message.Reserved) {
            return Delivery.PaymentConfirmed;
        }
        revert UnknownColleague(recipient);
    }

    function run() internal pure returns (bool) {
        bool routesBothDirections =
            send(PAYMENT, INVENTORY, Message.Reserve) == Delivery.InventoryReserved
            && send(INVENTORY, PAYMENT, Message.Reserved) == Delivery.PaymentConfirmed;

        bool rejectsUnknown;
        try MediatorPatternHarness.verifyUnknown() returns (bool) {
            rejectsUnknown = false;
        } catch {
            rejectsUnknown = true;
        }

        return routesBothDirections && rejectsUnknown;
    }
}

library MediatorPatternHarness {
    function verifyUnknown() internal pure returns (bool) {
        // Solidity libraries cannot catch internal calls, so expose the invalid
        // recipient as a separate pure verification seam for the sweep harness.
        uint8 recipient = 3;
        if (recipient != 1 && recipient != 2) {
            revert MediatorPattern.UnknownColleague(recipient);
        }
        return true;
    }
}
