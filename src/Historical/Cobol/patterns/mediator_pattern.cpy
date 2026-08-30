MEDIATOR-PATTERN.
    MOVE SPACES TO TEXT-A TEXT-B TEXT-C TEXT-D

    *> Payment colleague asks the mediator to notify inventory.
    MOVE "payment" TO TEXT-A
    MOVE "inventory" TO TEXT-B
    MOVE "paid" TO TEXT-C
    PERFORM MEDIATOR-SEND
    IF TEXT-D(1:23) NOT = "inventory<-payment:paid"
        PERFORM FAIL
    END-IF

    *> Inventory colleague asks the mediator to notify payment.
    MOVE "inventory" TO TEXT-A
    MOVE "payment" TO TEXT-B
    MOVE "reserved" TO TEXT-C
    PERFORM MEDIATOR-SEND
    IF TEXT-D(1:27) NOT = "payment<-inventory:reserved"
        PERFORM FAIL
    END-IF

    *> The mediator owns colleague discovery and rejects unknown recipients.
    MOVE "payment" TO TEXT-A
    MOVE "unknown" TO TEXT-B
    MOVE "ignored" TO TEXT-C
    PERFORM MEDIATOR-SEND
    IF TEXT-D(1:8) = "REJECTED"
        PERFORM PASS
    ELSE
        PERFORM FAIL
    END-IF.

MEDIATOR-SEND.
    MOVE SPACES TO TEXT-D
    EVALUATE TRUE
        WHEN TEXT-B(1:9) = "inventory"
            PERFORM MEDIATOR-INVENTORY-RECEIVE
        WHEN TEXT-B(1:7) = "payment"
            PERFORM MEDIATOR-PAYMENT-RECEIVE
        WHEN OTHER
            MOVE "REJECTED" TO TEXT-D
    END-EVALUATE.

MEDIATOR-INVENTORY-RECEIVE.
    IF TEXT-A(1:7) = "payment" AND TEXT-C(1:4) = "paid"
        MOVE "inventory<-payment:paid" TO TEXT-D
    ELSE
        PERFORM FAIL
    END-IF.

MEDIATOR-PAYMENT-RECEIVE.
    IF TEXT-A(1:9) = "inventory" AND TEXT-C(1:8) = "reserved"
        MOVE "payment<-inventory:reserved" TO TEXT-D
    ELSE
        PERFORM FAIL
    END-IF.
