       IDENTIFICATION DIVISION.
       PROGRAM-ID. ChainOfResponsibility.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 REFUND-AMOUNT PIC 9(4) VALUE 0250.
       01 VISITED PIC X(32) VALUE SPACES.
       01 RESULT PIC X(64) VALUE SPACES.
       PROCEDURE DIVISION.
           MOVE "faq" TO VISITED
           PERFORM FAQ-HANDLER
           DISPLAY "visited=" FUNCTION TRIM(VISITED)
                   ";" FUNCTION TRIM(RESULT)
           STOP RUN.

       FAQ-HANDLER.
           IF REFUND-AMOUNT <= 50
               MOVE "handled=faq;result=refund(250)" TO RESULT
           ELSE
               MOVE "faq>billing" TO VISITED
               PERFORM BILLING-HANDLER
           END-IF
           .

       BILLING-HANDLER.
           IF REFUND-AMOUNT <= 500
               MOVE "handled=billing;result=refund(250)" TO RESULT
           ELSE
               MOVE "faq>billing>escalation" TO VISITED
               PERFORM ESCALATION-HANDLER
           END-IF
           .

       ESCALATION-HANDLER.
           MOVE "handled=escalation;result=refund(250)" TO RESULT
           .
