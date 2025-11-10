       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opgave4.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 KUNDEOPL.
           02 Kunde-id         PIC X(10) VALUE spaces.
           02 Fornavn          PIC X(20) VALUE spaces.
           02 Efternavn        PIC X(20) VALUE spaces.
           02 KONTOINFO.
               03 Kontonummer      PIC X(20) VALUE spaces.
               03 Balance          PIC 9(7)V99 VALUE zeros.
               03 Valutakode       PIC X(3) VALUE spaces.

       PROCEDURE DIVISION.
      *Nedenfor kommer en display - Cobols måde at skrive i konsollen
       MOVE "1234567890" TO Kunde-id.
       MOVE "Lars" TO Fornavn.
       MOVE "Hansen" TO Efternavn.
       MOVE "DK123445678912345" TO Kontonummer.
       MOVE "2500.75" TO Balance.
       MOVE "DKK" TO Valutakode.
       DISPLAY KUNDEOPL
       STOP RUN.
