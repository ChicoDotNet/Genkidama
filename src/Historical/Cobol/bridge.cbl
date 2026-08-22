       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRIDGE-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DEVICE-NAME PIC X(5).
       01 ACTION-NAME PIC X(5).
       01 RESULT-TEXT PIC X(16).

       PROCEDURE DIVISION.
       MAIN.
           MOVE "TV" TO DEVICE-NAME
           MOVE "ON" TO ACTION-NAME
           PERFORM ACTIVATE
           DISPLAY "basic-tv=" FUNCTION TRIM(RESULT-TEXT)

           MOVE "Radio" TO DEVICE-NAME
           MOVE "ON" TO ACTION-NAME
           PERFORM ACTIVATE
           DISPLAY "basic-radio=" FUNCTION TRIM(RESULT-TEXT)

           MOVE "TV" TO DEVICE-NAME
           MOVE "MUTE" TO ACTION-NAME
           PERFORM ACTIVATE
           DISPLAY "mute-tv=" FUNCTION TRIM(RESULT-TEXT)

           MOVE "Radio" TO DEVICE-NAME
           MOVE "MUTE" TO ACTION-NAME
           PERFORM ACTIVATE
           DISPLAY "mute-radio=" FUNCTION TRIM(RESULT-TEXT)
           STOP RUN.

       ACTIVATE.
           IF ACTION-NAME = "ON"
               STRING FUNCTION TRIM(DEVICE-NAME) ":on"
                   INTO RESULT-TEXT
               END-STRING
           ELSE
               STRING FUNCTION TRIM(DEVICE-NAME) ":muted"
                   INTO RESULT-TEXT
               END-STRING
           END-IF.
