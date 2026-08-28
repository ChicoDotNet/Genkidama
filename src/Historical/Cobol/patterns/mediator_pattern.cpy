MEDIATOR-PATTERN.
    MOVE "button" TO TEXT-A
    MOVE "click" TO TEXT-B
    MOVE SPACES TO TEXT-C
    IF TEXT-A(1:6) = "button" AND TEXT-B(1:5) = "click"
        MOVE "panel.refresh" TO TEXT-C
    END-IF
    IF TEXT-C(1:13) NOT = "panel.refresh" PERFORM FAIL END-IF
    MOVE "panel" TO TEXT-A
    MOVE "loaded" TO TEXT-B
    IF TEXT-A(1:5) = "panel" AND TEXT-B(1:6) = "loaded"
        MOVE "button.enable" TO TEXT-C
    END-IF
    IF TEXT-C(1:13) = "button.enable" PERFORM PASS ELSE PERFORM FAIL END-IF.
