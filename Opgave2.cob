       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opgave2.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Kunde-id         PIC X(10) VALUE spaces.
       01 Fornavn          PIC X(20) VALUE spaces.
       01 Efternavn        PIC X(20) VALUE spaces.
       01 Kontonummer      PIC X(20) VALUE spaces.
       01 Balance          PIC 9(7)V99 VALUE zeros.
       01 Valutakode       PIC X(3) VALUE spaces.

       PROCEDURE DIVISION.
      *Nedenfor kommer en display - Cobols måde at skrive i konsollen
       MOVE "1234567890" TO Kunde-id.
       MOVE "Lars" TO Fornavn.
       MOVE "Hansen" TO Efternavn.
       MOVE "DK123445678912345" TO Kontonummer.
       MOVE "2500.75" TO Balance.
       MOVE "DKK" TO Valutakode.
       DISPLAY Kunde-id
       DISPLAY Fornavn Efternavn
       DISPLAY Kontonummer
       DISPLAY Balance Valutakode
       STOP RUN.
