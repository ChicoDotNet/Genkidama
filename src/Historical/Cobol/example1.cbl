       IDENTIFICATION DIVISION.
       PROGRAM-ID. Example1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 THEME        PIC X(5).
       PROCEDURE DIVISION.

       PERFORM TEST-THEME WITH THEME "dark"
       PERFORM TEST-THEME WITH THEME "light"
       STOP RUN.

       TEST-THEME SECTION.
           DISPLAY "Testing theme: " THEME
           PERFORM CREATE-BUTTON
           PERFORM CREATE-CHECKBOX
           .

       CREATE-BUTTON SECTION.
           EVALUATE THEME
               WHEN "dark"
                   DISPLAY "Dark Button"
               WHEN "light"
                   DISPLAY "Light Button"
               WHEN OTHER
                   DISPLAY "Unknown Button"
           END-EVALUATE
           .

       CREATE-CHECKBOX SECTION.
           EVALUATE THEME
               WHEN "dark"
                   DISPLAY "Dark Checkbox"
               WHEN "light"
                   DISPLAY "Light Checkbox"
               WHEN OTHER
                   DISPLAY "Unknown Checkbox"
           END-EVALUATE
           .
