       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROXY-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BACKEND-CREATED PIC 9 VALUE 0.
       01 BACKEND-FETCHES PIC 9 VALUE 0.
       01 CACHE-READY PIC 9 VALUE 0.
       01 FIRST-DOC PIC X(7) VALUE SPACES.
       01 SECOND-DOC PIC X(7) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM PROXY-GET
           MOVE FIRST-DOC TO SECOND-DOC
           PERFORM PROXY-GET
           DISPLAY "backend=" BACKEND-CREATED
                   ";fetches=" BACKEND-FETCHES
                   ";first=" FIRST-DOC
                   ";second=" SECOND-DOC
           STOP RUN.

       PROXY-GET.
           IF CACHE-READY = 0
               IF BACKEND-CREATED = 0
                   MOVE 1 TO BACKEND-CREATED
               END-IF
               ADD 1 TO BACKEND-FETCHES
               MOVE "doc(42)" TO FIRST-DOC
               MOVE 1 TO CACHE-READY
           END-IF.
