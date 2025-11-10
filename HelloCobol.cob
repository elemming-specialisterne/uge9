       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VAR-text         PIC X(30) VALUE "Hello med Variabel".

       PROCEDURE DIVISION.
      *Nedenfor kommer en display - Cobols måde at skrive i konsollen
       DISPLAY VAR-text
       STOP RUN.
