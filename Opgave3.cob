       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opgave3.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Kunde-id         PIC X(10) VALUE SPACES.
       01 Fornavn          PIC X(20) VALUE SPACES.
       01 Efternavn        PIC X(20) VALUE SPACES.
       01 Kontonummer      PIC X(20) VALUE SPACES.
       01 Balance          PIC 9(7)V99 VALUE ZEROS.
       01 Valutakode       PIC X(3) VALUE SPACES.
       01 Navn             PIC X(40) VALUE SPACES.

       01 IX               PIC 9(2) VALUE 1.
       01 IX2              PIC 9(2) VALUE 1.
       01 Current-char     PIC X(1) VALUE SPACES.
       01 Previous-char    PIC X(1) VALUE SPACES.
       01 Cleaned-navn     PIC X(40) VALUE SPACES.

       PROCEDURE DIVISION.
      *Nedenfor kommer en display - Cobols måde at skrive i konsollen
       MOVE "1234567890" TO Kunde-id.
       MOVE "Lars" TO Fornavn.
       MOVE "Hansen" TO Efternavn.
       MOVE "DK123445678912345" TO Kontonummer.
       MOVE "2500.75" TO Balance.
       MOVE "DKK" TO Valutakode.

       STRING Fornavn DELIMITED BY SIZE " "
           DELIMITED BY SIZE Efternavn
           DELIMITED BY SIZE
           INTO Navn

       PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > LENGTH OF navn
           MOVE Navn(IX:1) TO Current-char
           IF Current-char NOT = SPACE OR Previous-char NOT = SPACE
               MOVE Current-char TO Cleaned-navn(IX2:1)
               ADD 1 TO IX2
           END-IF
           MOVE Current-char TO Previous-char
       END-PERFORM
       MOVE Cleaned-navn TO navn

       DISPLAY "----------------------------------------"
       DISPLAY "Kunde ID : " Kunde-id
       DISPLAY "Navn (renset) : " navn
       DISPLAY "Kontonummer : " Kontonummer
       DISPLAY "Balance : " Balance " " Valutakode
       DISPLAY "----------------------------------------"
       STOP RUN.


