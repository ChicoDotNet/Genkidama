       IDENTIFICATION DIVISION.
       PROGRAM-ID. Example1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ACTIVE-FACTORY PIC X(5).
       PROCEDURE DIVISION.

           MOVE "dark" TO ACTIVE-FACTORY
           PERFORM CREATE-UI-FAMILY

           MOVE "light" TO ACTIVE-FACTORY
           PERFORM CREATE-UI-FAMILY

           STOP RUN.

       CREATE-UI-FAMILY SECTION.
           EVALUATE ACTIVE-FACTORY
               WHEN "dark"
                   PERFORM CREATE-DARK-BUTTON
                   PERFORM CREATE-DARK-CHECKBOX
               WHEN "light"
                   PERFORM CREATE-LIGHT-BUTTON
                   PERFORM CREATE-LIGHT-CHECKBOX
               WHEN OTHER
                   DISPLAY "Unknown UI family"
           END-EVALUATE
           .

       CREATE-DARK-BUTTON SECTION.
           DISPLAY "Dark Button"
           .

       CREATE-DARK-CHECKBOX SECTION.
           DISPLAY "Dark Checkbox"
           .

       CREATE-LIGHT-BUTTON SECTION.
           DISPLAY "Light Button"
           .

       CREATE-LIGHT-CHECKBOX SECTION.
           DISPLAY "Light Checkbox"
           .
