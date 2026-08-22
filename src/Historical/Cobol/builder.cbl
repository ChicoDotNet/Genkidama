       IDENTIFICATION DIVISION.
       PROGRAM-ID. Builder.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 REPORT-FORMAT PIC X(4).
       PROCEDURE DIVISION.

           MOVE "TEXT" TO REPORT-FORMAT
           PERFORM BUILD-AVAILABILITY-REPORT
           DISPLAY "---"
           MOVE "HTML" TO REPORT-FORMAT
           PERFORM BUILD-AVAILABILITY-REPORT
           STOP RUN.

       BUILD-AVAILABILITY-REPORT SECTION.
           PERFORM RESET-BUILDER
           PERFORM ADD-TITLE
           PERFORM ADD-SECTION
           .

       RESET-BUILDER SECTION.
           CONTINUE
           .

       ADD-TITLE SECTION.
           IF REPORT-FORMAT = "TEXT"
               DISPLAY "# Service status"
           ELSE
               DISPLAY "<h1>Service status</h1>"
           END-IF
           .

       ADD-SECTION SECTION.
           IF REPORT-FORMAT = "TEXT"
               DISPLAY "## Availability"
               DISPLAY "99.95%"
           ELSE
               DISPLAY "<h2>Availability</h2><p>99.95%</p>"
           END-IF
           .
