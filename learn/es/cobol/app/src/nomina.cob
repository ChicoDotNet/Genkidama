       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOMINABATCH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "data/employees.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "report.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD               PIC X(160).

       FD  REPORT-FILE.
       01  REPORT-RECORD                 PIC X(160).

       WORKING-STORAGE SECTION.
       01  WS-END-OF-FILE                PIC X VALUE "N".
           88 END-OF-FILE                VALUE "Y".

       01  WS-PARSED-RECORD.
           05 WS-ID-TEXT                 PIC X(12).
           05 WS-NAME                    PIC X(40).
           05 WS-HOURS-TEXT              PIC X(8).
           05 WS-RATE-TEXT               PIC X(16).
           05 WS-DEDUCTION-TEXT          PIC X(16).

       01  WS-NUMERIC-VALUES.
           05 WS-HOURS                   PIC 9(3).
           05 WS-HOURLY-RATE             PIC 9(5)V99.
           05 WS-DEDUCTION-PCT           PIC 9(3)V99.
           05 WS-GROSS                   PIC 9(8)V99.
           05 WS-DEDUCTION               PIC 9(8)V99.
           05 WS-NET                     PIC 9(8)V99.

       01  WS-DISPLAY-VALUES.
           05 WS-GROSS-DISPLAY           PIC ZZZZZZZ9.99.
           05 WS-DEDUCTION-DISPLAY       PIC ZZZZZZZ9.99.
           05 WS-NET-DISPLAY             PIC ZZZZZZZ9.99.

       01  WS-COUNTERS.
           05 WS-FIELD-COUNT             PIC 9 VALUE 0.
           05 WS-PROCESSED               PIC 9(6) VALUE 0.
           05 WS-REJECTED                PIC 9(6) VALUE 0.
           05 WS-PROCESSED-DISPLAY       PIC Z(5)9.
           05 WS-REJECTED-DISPLAY        PIC Z(5)9.

       01  WS-REPORT-LINE                PIC X(160).
       01  WS-REJECTION-REASON           PIC X(50).

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT EMPLOYEE-FILE
                OUTPUT REPORT-FILE
           PERFORM WRITE-HEADER
           PERFORM UNTIL END-OF-FILE
               READ EMPLOYEE-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM
           PERFORM WRITE-SUMMARY
           CLOSE EMPLOYEE-FILE REPORT-FILE
           DISPLAY "NominaBatch completo. Consulta report.txt"
           STOP RUN.

       WRITE-HEADER.
           MOVE "NOMINABATCH - REPORTE" TO REPORT-RECORD
           WRITE REPORT-RECORD
           MOVE "ID|NOMBRE|BRUTO|DEDUCCIONES|NETO" TO REPORT-RECORD
           WRITE REPORT-RECORD.

       PROCESS-RECORD.
           PERFORM RESET-RECORD
           UNSTRING EMPLOYEE-RECORD DELIMITED BY ";"
               INTO WS-ID-TEXT
                    WS-NAME
                    WS-HOURS-TEXT
                    WS-RATE-TEXT
                    WS-DEDUCTION-TEXT
               TALLYING IN WS-FIELD-COUNT
           END-UNSTRING

           IF WS-FIELD-COUNT NOT = 5
               MOVE "FORMATO: se esperaban 5 campos" TO WS-REJECTION-REASON
               PERFORM WRITE-REJECTION
           ELSE
               PERFORM VALIDATE-AND-CALCULATE
           END-IF.

       RESET-RECORD.
           MOVE SPACES TO WS-PARSED-RECORD
           MOVE ZERO TO WS-NUMERIC-VALUES WS-FIELD-COUNT
           MOVE SPACES TO WS-REJECTION-REASON WS-REPORT-LINE.

       VALIDATE-AND-CALCULATE.
           IF FUNCTION TRIM(WS-ID-TEXT) = SPACES
               MOVE "ID vacío" TO WS-REJECTION-REASON
               PERFORM WRITE-REJECTION
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(WS-HOURS-TEXT) NOT = 0
               MOVE "HORAS no es numérico" TO WS-REJECTION-REASON
               PERFORM WRITE-REJECTION
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(WS-RATE-TEXT) NOT = 0
               MOVE "TARIFA no es numérica" TO WS-REJECTION-REASON
               PERFORM WRITE-REJECTION
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(WS-DEDUCTION-TEXT) NOT = 0
               MOVE "DEDUCCIÓN no es numérica" TO WS-REJECTION-REASON
               PERFORM WRITE-REJECTION
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-HOURS = FUNCTION NUMVAL(WS-HOURS-TEXT)
           COMPUTE WS-HOURLY-RATE = FUNCTION NUMVAL(WS-RATE-TEXT)
           COMPUTE WS-DEDUCTION-PCT = FUNCTION NUMVAL(WS-DEDUCTION-TEXT)

           IF WS-HOURS = 0 OR WS-HOURS > 80
               MOVE "HORAS fuera de rango 1..80" TO WS-REJECTION-REASON
               PERFORM WRITE-REJECTION
               EXIT PARAGRAPH
           END-IF

           IF WS-HOURLY-RATE = 0
               MOVE "TARIFA debe ser mayor que cero" TO WS-REJECTION-REASON
               PERFORM WRITE-REJECTION
               EXIT PARAGRAPH
           END-IF

           IF WS-DEDUCTION-PCT > 100
               MOVE "DEDUCCIÓN debe estar entre 0 y 100" TO WS-REJECTION-REASON
               PERFORM WRITE-REJECTION
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-GROSS = WS-HOURS * WS-HOURLY-RATE
           COMPUTE WS-DEDUCTION ROUNDED =
               WS-GROSS * WS-DEDUCTION-PCT / 100
           COMPUTE WS-NET = WS-GROSS - WS-DEDUCTION
           ADD 1 TO WS-PROCESSED
           PERFORM WRITE-PAYROLL-LINE.

       WRITE-PAYROLL-LINE.
           MOVE WS-GROSS TO WS-GROSS-DISPLAY
           MOVE WS-DEDUCTION TO WS-DEDUCTION-DISPLAY
           MOVE WS-NET TO WS-NET-DISPLAY
           STRING
               FUNCTION TRIM(WS-ID-TEXT) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-NAME) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-GROSS-DISPLAY) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-DEDUCTION-DISPLAY) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-NET-DISPLAY) DELIMITED BY SIZE
               INTO WS-REPORT-LINE
           END-STRING
           WRITE REPORT-RECORD FROM WS-REPORT-LINE.

       WRITE-REJECTION.
           ADD 1 TO WS-REJECTED
           STRING
               "RECHAZADO|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-ID-TEXT) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-REJECTION-REASON) DELIMITED BY SIZE
               INTO WS-REPORT-LINE
           END-STRING
           WRITE REPORT-RECORD FROM WS-REPORT-LINE.

       WRITE-SUMMARY.
           MOVE WS-PROCESSED TO WS-PROCESSED-DISPLAY
           MOVE WS-REJECTED TO WS-REJECTED-DISPLAY
           MOVE SPACES TO WS-REPORT-LINE
           STRING
               "RESUMEN|PROCESADOS=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-PROCESSED-DISPLAY) DELIMITED BY SIZE
               "|RECHAZADOS=" DELIMITED BY SIZE
               FUNCTION TRIM(WS-REJECTED-DISPLAY) DELIMITED BY SIZE
               INTO WS-REPORT-LINE
           END-STRING
           WRITE REPORT-RECORD FROM WS-REPORT-LINE.
