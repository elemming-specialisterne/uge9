       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opgave9.

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

       01 KONTO-ARRAY occurs 12 Times.
           COPY "KONTOOPL.cpy".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM READ-KONTO

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
           PERFORM varying IX from 1 by 1 until IX > 10
               IF KUNDE-ID in KONTO-ARRAY(IX) = KUNDE-ID in KUNDEOPL
                   MOVE KONTO-ARRAY(IX) TO NAVN-ADR
                   perform COPYFILD
               END-IF
           END-PERFORM
       EXIT.

       READ-KONTO.
           MOVE 1 TO IX
           OPEN INPUT INPUT-KONTO-FILE
           PERFORM UNTIL END-OF-KONTO-FILE = "Y"
               READ INPUT-KONTO-FILE INTO KONTOOPL
                   AT END
                       MOVE "Y" TO END-OF-KONTO-FILE
                   NOT AT END
                       MOVE KONTOOPL TO KONTO-ARRAY(IX)
                       ADD 1 TO IX
               END-READ
           END-PERFORM
           CLOSE INPUT-KONTO-FILE
           MOVE "N" TO END-OF-KONTO-FILE
       EXIT.
           
