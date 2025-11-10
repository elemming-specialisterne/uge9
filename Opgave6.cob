       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opgave6.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 KUNDEOPL.
           COPY "KUNDER.cpy".

       WORKING-STORAGE SECTION.
       01 END-OF-FILE  PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE

           PERFORM UNTIL END-OF-FILE = "Y"
               READ INPUT-FILE INTO KUNDEOPL
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       DISPLAY KUNDEOPL
               END-READ
           END-PERFORM
           
           CLOSE INPUT-FILE
           STOP RUN.
