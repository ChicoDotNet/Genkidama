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
