       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLYWEIGHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STYLE-COUNT PIC 9 VALUE 0.
       01 RED-ID      PIC 9 VALUE 0.
       01 BLUE-ID     PIC 9 VALUE 0.
       01 RESULT-ID   PIC 9 VALUE 0.
       01 RED-ID-1    PIC 9 VALUE 0.
       01 RED-ID-2    PIC 9 VALUE 0.
       01 BLUE-ID-1   PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM GET-RED
           MOVE RESULT-ID TO RED-ID-1
           PERFORM GET-RED
           MOVE RESULT-ID TO RED-ID-2
           PERFORM GET-BLUE
           MOVE RESULT-ID TO BLUE-ID-1
           IF RED-ID-1 = RED-ID-2 AND RED-ID-1 NOT = BLUE-ID-1
               DISPLAY 'styles=' STYLE-COUNT
                       ';shared=true;text=ABC'
           ELSE
               DISPLAY 'styles=' STYLE-COUNT
                       ';shared=false;text=ABC'
           END-IF
           STOP RUN.
       GET-RED.
           IF RED-ID = 0
               ADD 1 TO STYLE-COUNT
               MOVE STYLE-COUNT TO RED-ID
           END-IF
           MOVE RED-ID TO RESULT-ID.
       GET-BLUE.
           IF BLUE-ID = 0
               ADD 1 TO STYLE-COUNT
               MOVE STYLE-COUNT TO BLUE-ID
           END-IF
           MOVE BLUE-ID TO RESULT-ID.
