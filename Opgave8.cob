       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opgave8.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INPUT-KONTO-FILE ASSIGN TO "KontoOpl.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "KundeoplysningerOUT.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-KONTO-FILE ASSIGN TO "KontooplysningerOUT.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 KUNDEOPL.
           COPY "KUNDER.cpy".

       FD INPUT-KONTO-FILE.
       01 KONTOOPL.
           COPY "KONTOOPL.cpy".

       FD OUTPUT-FILE.
       01 KUNDE-ADR.
           02 NAVN-ADR     PIC X(100) VALUE SPACES.

       FD OUTPUT-KONTO-FILE.
       01 KUNDEKONTO.
           02 OUTPUT-TEXT  PIC X(100).

       WORKING-STORAGE SECTION.
       01 END-OF-FILE  PIC X VALUE "N".
       01 END-OF-KONTO-FILE  PIC X VALUE "N".

       01 IX               PIC 9(2) VALUE 1.
       01 IX2              PIC 9(2) VALUE 1.
       01 Current-char     PIC X(1) VALUE SPACES.
       01 Previous-char    PIC X(1) VALUE SPACES.
       01 WS-STRING        PIC X(50) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM UNTIL END-OF-FILE = "Y"
               READ INPUT-FILE INTO KUNDEOPL
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       MOVE spaces to NAVN-ADR

                       PERFORM FORMAT-KUNDEID
                       PERFORM FORMAT-NAVN
                       PERFORM FORMAT-ADRESSE
                       PERFORM FORMAT-BY
                       PERFORM FORMAT-KONTAKT

                       PERFORM FORMAT-KONTO

      *                Skriv blank linje
                       PERFORM COPYFILD
               END-READ
           END-PERFORM
           
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.
       
       COPYFILD.
           WRITE KUNDE-ADR
           MOVE spaces to NAVN-ADR
       EXIT.

       FORMAT-KUNDEID.
           STRING  KUNDE-ID OF KUNDEOPL DELIMITED BY SPACE 
                   INTO NAVN-ADR
           perform COPYFILD.
       EXIT.

       FORMAT-NAVN.
           STRING  FORNAVN     DELIMITED BY SPACE 
                   " "         DELIMITED BY SIZE
                   EFTERNAVN   DELIMITED BY SPACE
                   INTO NAVN-ADR
           perform COPYFILD.
       EXIT.

       FORMAT-ADRESSE.
           STRING  VEJNAVN     DELIMITED BY SPACE 
                   " "         DELIMITED BY SIZE
                   HUSNR       DELIMITED BY SPACE
                   ", "        DELIMITED BY SIZE
                   ETAGE       DELIMITED BY SPACE
                   ". "        DELIMITED BY SIZE
                   SIDE        DELIMITED BY SPACE
                   INTO NAVN-ADR
           perform COPYFILD.
       EXIT.

       FORMAT-BY.
           STRING  POSTNR      DELIMITED BY SPACE 
                   " "         DELIMITED BY SIZE
                   CITY        DELIMITED BY SPACE
                   INTO NAVN-ADR
           perform COPYFILD.
       EXIT.

       FORMAT-KONTAKT.
           STRING  "tlf. "     DELIMITED BY SIZE
                   TELEFON     DELIMITED BY SPACE 
                   " Email: "  DELIMITED BY SIZE
                   EMAIL       DELIMITED BY SPACE
                   INTO NAVN-ADR
           perform COPYFILD.
       EXIT.

       FORMAT-KONTO.
           OPEN INPUT INPUT-KONTO-FILE
           PERFORM UNTIL END-OF-KONTO-FILE = "Y"
               READ INPUT-KONTO-FILE INTO KONTOOPL
                   AT END
                       MOVE "Y" TO END-OF-KONTO-FILE
                   NOT AT END
                       IF KUNDE-ID in KONTOOPL = KUNDE-ID in KUNDEOPL
                           MOVE KONTOOPL TO NAVN-ADR
                           perform COPYFILD
                       END-IF
               END-READ
           END-PERFORM
           CLOSE INPUT-KONTO-FILE
           MOVE "N" TO END-OF-KONTO-FILE
       EXIT.

