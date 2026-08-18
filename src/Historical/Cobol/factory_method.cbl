       IDENTIFICATION DIVISION.
       PROGRAM-ID. FactoryMethod.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FACTORY-KIND PIC 9.
       01 DATABASE-KIND PIC 9.
       PROCEDURE DIVISION.
           MOVE 1 TO FACTORY-KIND
           PERFORM USE-DATABASE
           MOVE 2 TO FACTORY-KIND
           PERFORM USE-DATABASE
           STOP RUN.

       USE-DATABASE SECTION.
           PERFORM CREATE-DATABASE
           EVALUATE DATABASE-KIND
               WHEN 1
                   DISPLAY "PostgreSQL connect"
                   DISPLAY "PostgreSQL query"
               WHEN 2
                   DISPLAY "MySQL connect"
                   DISPLAY "MySQL query"
               WHEN OTHER
                   DISPLAY "Unknown database"
           END-EVALUATE
           .

       CREATE-DATABASE SECTION.
           EVALUATE FACTORY-KIND
               WHEN 1 MOVE 1 TO DATABASE-KIND
               WHEN 2 MOVE 2 TO DATABASE-KIND
               WHEN OTHER MOVE 0 TO DATABASE-KIND
           END-EVALUATE
           .
