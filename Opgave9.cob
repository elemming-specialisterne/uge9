      *****************************************************************
      * PROGRAM-ID: OPGAVE9                                          *
      * FORFATTER:  SPAC-23                                          *
      * DATO:       2025-11-12                                       *
      * FORMÅL:     Læser kunde- og kontooplysninger og producerer   *
      *             formateret rapport med kundedata og tilhørende   *
      *             kontooplysninger                                  *
      * INPUT:      Kundeoplysninger.txt - Kunde stamdata            *
      *             KontoOpl.txt - Konto oplysninger                 *
      * OUTPUT:     KundeoplysningerOUT.txt - Formateret rapport     *
      * NOTER:      Programmet loader alle konti i hukommelse for    *
      *             bedre performance ved søgning                     *
      *****************************************************************
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

      *================================================================
      * DATA DIVISION - Definerer alle filer og data strukturer
      *================================================================
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

      *================================================================
      * WORKING-STORAGE SECTION - Arbejdsvariable og arrays
      *================================================================
       WORKING-STORAGE SECTION.
      * Fil status flags
       01 END-OF-FILE      PIC X VALUE "N".    *> EOF for kunde fil
       01 END-OF-KONTO-FILE PIC X VALUE "N".   *> EOF for konto fil

      * Loop counters og indexer
       01 IX               PIC 9(2) VALUE 1.   *> Array index counter

      * Konto array - gemmer alle konti i hukommelse for hurtig søgning
       01 KONTO-ARRAY OCCURS 12 TIMES.
           COPY "KONTOOPL.cpy".

      *================================================================
      * PROCEDURE DIVISION - Hovedprogramlogik
      *================================================================
       PROCEDURE DIVISION.
       
      *****************************************************************
      * MAIN-PROCEDURE                                                *
      * Formål: Hovedprocedure der koordinerer hele programmet       *
      * Flow:   1. Åbner filer                                        *
      *         2. Indlæser alle konti i hukommelse                   *
      *         3. Processerer hver kunde og deres konti              *
      *         4. Lukker filer og afslutter                          *
      *****************************************************************
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

      *    Læs kontoer ind i array
           PERFORM READ-KONTO

      * Læs alle konto records i hukommelse først
           PERFORM READ-KONTO

      * Processér hver kunde og format deres oplysninger
           PERFORM UNTIL END-OF-FILE = "Y"
               READ INPUT-FILE INTO KUNDEOPL
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       MOVE SPACES TO NAVN-ADR

      * Format alle kunde oplysninger
                       PERFORM FORMAT-KUNDEID      *> Kunde ID
                       PERFORM FORMAT-NAVN         *> For- og efternavn
                       PERFORM FORMAT-ADRESSE      *> Gade og husnummer
                       PERFORM FORMAT-BY           *> Postnr og by
                       PERFORM FORMAT-KONTAKT      *> Telefon og email

      * Find og format kundens konti
                       PERFORM FORMAT-KONTO

      * Skriv tom linje efter hver kunde
                       PERFORM COPYFILD
               END-READ
           END-PERFORM
           
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.
       
      *================================================================
      * UTILITY PROCEDURES - Hjælpeprocedurer
      *================================================================
      
      *****************************************************************
      * COPYFILD                                                      *
      * Formål: Skriver indholdet af NAVN-ADR til output fil og      *
      *         rydder NAVN-ADR for næste linje                       *
      *****************************************************************
       COPYFILD.
           WRITE KUNDE-ADR
           MOVE SPACES TO NAVN-ADR
       EXIT.

      *================================================================
      * FORMATTING PROCEDURES - Formatering af kunde data
      *================================================================
      
      *****************************************************************
      * FORMAT-KUNDEID                                                *
      * Formål: Formatter og skriver kunde ID med label              *
      *****************************************************************
       FORMAT-KUNDEID.
           STRING  "Kunde ID: " DELIMITED BY SIZE
                   KUNDE-ID OF KUNDEOPL DELIMITED BY SPACE 
                   INTO NAVN-ADR
           PERFORM COPYFILD.
       EXIT.

      *****************************************************************
      * FORMAT-NAVN                                                   *
      * Formål: Kombinerer fornavn og efternavn til fuldt navn       *
      *****************************************************************
       FORMAT-NAVN.
           STRING  FORNAVN     DELIMITED BY SPACE 
                   " "         DELIMITED BY SIZE
                   EFTERNAVN   DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD.
       EXIT.

      *****************************************************************
      * FORMAT-ADRESSE                                                *
      * Formål: Formatter adresse med vejnavn, husnr, etage og side  *
      *****************************************************************
       FORMAT-ADRESSE.
           STRING  VEJNAVN     DELIMITED BY SPACE 
                   " "         DELIMITED BY SIZE
                   HUSNR       DELIMITED BY SPACE
                   ", "        DELIMITED BY SIZE
                   ETAGE       DELIMITED BY SPACE
                   ". "        DELIMITED BY SIZE
                   SIDE        DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD.
       EXIT.

      *****************************************************************
      * FORMAT-BY                                                     *
      * Formål: Formatter postnummer og by navn                       *
      *****************************************************************
       FORMAT-BY.
           STRING  POSTNR      DELIMITED BY SPACE 
                   " "         DELIMITED BY SIZE
                   CITY        DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD.
       EXIT.

      *****************************************************************
      * FORMAT-KONTAKT                                                *
      * Formål: Formatter telefon og email oplysninger               *
      *****************************************************************
       FORMAT-KONTAKT.
           STRING  "tlf. "     DELIMITED BY SIZE
                   TELEFON     DELIMITED BY SPACE 
                   " Email: "  DELIMITED BY SIZE
                   EMAIL       DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD.
       EXIT.

      *****************************************************************
      * FORMAT-KONTO                                                  *
      * Formål: Finder og skriver alle konti der tilhører denne      *
      *         kunde ved at søge gennem KONTO-ARRAY                  *
      * Input:  KUNDE-ID fra KUNDEOPL                                 *
      * Output: Formaterede konto linjer                              *
      *****************************************************************
       FORMAT-KONTO.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10
               IF KUNDE-ID IN KONTO-ARRAY(IX) = KUNDE-ID in KUNDEOPL
                   MOVE KONTO-ARRAY(IX) TO NAVN-ADR
                   PERFORM COPYFILD
               END-IF
           END-PERFORM
       EXIT.

      *================================================================
      * FILE HANDLING PROCEDURES - Fil håndtering
      *================================================================
      
      *****************************************************************
      * READ-KONTO                                                    *
      * Formål: Læser alle konto records fra KontoOpl.txt ind i      *
      *         KONTO-ARRAY for hurtig søgning senere                 *
      * Output: KONTO-ARRAY fyldt med alle konto records             *
      * Note:   Kaldes kun én gang ved program start                  *
      *****************************************************************
       READ-KONTO.
           MOVE 1 TO IX                       *> Start ved array index 1
           OPEN INPUT INPUT-KONTO-FILE
           PERFORM UNTIL END-OF-KONTO-FILE = "Y"
               READ INPUT-KONTO-FILE INTO KONTOOPL
                   AT END
                       MOVE "Y" TO END-OF-KONTO-FILE
                   NOT AT END
      * Gem konto record i array
                       MOVE KONTOOPL TO KONTO-ARRAY(IX)
      * Gå til næste array position
                       ADD 1 TO IX
               END-READ
           END-PERFORM
           CLOSE INPUT-KONTO-FILE
      * Reset flag for næste brug
           MOVE "N" TO END-OF-KONTO-FILE
       EXIT.
           
