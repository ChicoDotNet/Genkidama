       IDENTIFICATION DIVISION.
       PROGRAM-ID. Example3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 REPORT-TYPE        PIC X(10).
       PROCEDURE DIVISION.

       PERFORM TEST-REPORT WITH REPORT-TYPE "pdf"
       PERFORM TEST-REPORT WITH REPORT-TYPE "html"
       STOP RUN.

       TEST-REPORT SECTION.
           DISPLAY "Testing report type: " REPORT-TYPE
           PERFORM GENERATE-REPORT
           .

       GENERATE-REPORT SECTION.
           EVALUATE REPORT-TYPE
               WHEN "pdf"
                   DISPLAY "Generating PDF report"
               WHEN "html"
                   DISPLAY "Generating HTML report"
               WHEN OTHER
                   DISPLAY "Unknown report type"
           END-EVALUATE
           .
