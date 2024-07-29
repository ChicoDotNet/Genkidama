       IDENTIFICATION DIVISION.
       PROGRAM-ID. Example2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DB-TYPE        PIC X(10).
       PROCEDURE DIVISION.

       PERFORM TEST-DB WITH DB-TYPE "postgresql"
       PERFORM TEST-DB WITH DB-TYPE "mysql"
       STOP RUN.

       TEST-DB SECTION.
           DISPLAY "Testing database type: " DB-TYPE
           PERFORM CONNECT-DB
           PERFORM QUERY-DB
           .

       CONNECT-DB SECTION.
           EVALUATE DB-TYPE
               WHEN "postgresql"
                   DISPLAY "Connecting to PostgreSQL"
               WHEN "mysql"
                   DISPLAY "Connecting to MySQL"
               WHEN OTHER
                   DISPLAY "Unknown database"
           END-EVALUATE
           .

       QUERY-DB SECTION.
           EVALUATE DB-TYPE
               WHEN "postgresql"
                   DISPLAY "Querying PostgreSQL"
               WHEN "mysql"
                   DISPLAY "Querying MySQL"
               WHEN OTHER
                   DISPLAY "Unknown database"
           END-EVALUATE
           .
