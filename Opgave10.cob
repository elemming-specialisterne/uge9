      *****************************************************************
      * PROGRAM-ID: OPGAVE9                                           *
      * FORFATTER:  SPAC-23                                           *
      * DATO:       2025-11-12                                        *
      * FORMÅL:     Læser kunde- og kontooplysninger og producerer    *
      *             formateret rapport med kundedata og tilhørende    *
      *             kontooplysninger                                  *
      * INPUT:      Kundeoplysninger.txt - Kunde stamdata             *
      *             KontoOpl.txt - Konto oplysninger                  *
      * OUTPUT:     KundeoplysningerOUT.txt - Formateret rapport      *
      * NOTER:      Programmet loader alle konti i hukommelse for     *
      *             bedre performance ved søgning                     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opgave10.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "Transaktioner.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INPUT-BANK-FILE ASSIGN TO "Banker.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "Kontoudskrifter.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

      *================================================================
      * DATA DIVISION - Definerer alle filer og data strukturer
      *================================================================
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 TRANSAKTIONEROPL.
           COPY "TRANSAKTIONEROPL.cpy".
       FD INPUT-BANK-FILE.
       01 BANKOPL.
           COPY "BANKOPL.cpy".

       FD OUTPUT-FILE.
       01 KONTO-ADR.
           02 NAVN-ADR     PIC X(100) VALUE SPACES.

      *================================================================
      * WORKING-STORAGE SECTION - Arbejdsvariable og arrays
      *================================================================
       WORKING-STORAGE SECTION.
      * Fil status flags
       01 END-OF-FILE      PIC X VALUE "N".    *> EOF for kunde fil
       01 END-OF-BANK-FILE PIC X VALUE "N".    *> EOF for bank fil
       01 PREV-REG-NR      PIC X(6) VALUE SPACES.

      * Loop counters og indexer
       01 IX               PIC 9(3) VALUE 1.   *> Array index counter

      * Konto array - gemmer alle konti i hukommelse for hurtig søgning
       01 BANK-ARRAY OCCURS 100 TIMES.
           COPY "BANKOPL.cpy".

       01 USD              PIC 9V9 value 6.8.
       01 EUR              PIC 9V9 value 7.5.

       01 CONVERTED-VALUTA PIC Z(13)9.99.

      *================================================================
      * PROCEDURE DIVISION - Hovedprogramlogik
      *================================================================
       PROCEDURE DIVISION.
       
      *****************************************************************
      * MAIN-PROCEDURE                                                *
      * Formål: Hovedprocedure der koordinerer hele programmet        *
      * Flow:   1. Åbner filer                                        *
      *         2. Indlæser alle konti i hukommelse                   *
      *         3. Processerer hver kunde og deres konti              *
      *         4. Lukker filer og afslutter                          *
      *****************************************************************
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

      * Læs Banker ind i array
           PERFORM READ-BANKS

      * Processér hver kunde og format deres oplysninger
           PERFORM UNTIL END-OF-FILE = "Y"
               READ INPUT-FILE INTO TRANSAKTIONEROPL
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       if PREV-REG-NR not = REG-NR in TRANSAKTIONEROPL
                          MOVE SPACES TO NAVN-ADR
   
                          PERFORM DASH-LINE
                          PERFORM FORMAT-KUNDENAVN
                          PERFORM FORMAT-ADRESSE
                          PERFORM COPYFILD
                          PERFORM COPYFILD
   
                          PERFORM FORMAT-BANK
                          PERFORM COPYFILD

                          PERFORM FORMAT-KONTOUDSKRIFT-START
                          PERFORM COPYFILD

                          MOVE REG-NR in TRANSAKTIONEROPL to PREV-REG-NR
                       end-if
                       
                       PERFORM FORMAT-KONTOUDSKRIFT


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
      * Formål: Skriver indholdet af NAVN-ADR til output fil og       *
      *         rydder NAVN-ADR for næste linje                       *
      *****************************************************************
       COPYFILD.
           WRITE KONTO-ADR
           MOVE SPACES TO NAVN-ADR
       EXIT.

      *================================================================
      * FORMATTING PROCEDURES - Formatering af kunde data
      *================================================================
      
       DASH-LINE.
           STRING  "-----------------------------" delimited by size
                   "-----------------------------" delimited by size
                   into NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-KUNDENAVN.
           STRING  "Kunde: " delimited by size
                   NAVN delimited by space
                   into NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-ADRESSE.
           STRING  "Adresse: " delimited by size
                   ADRESSE delimited by space
                   into NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BANK.
           perform FIND-BANK
           perform FORMAT-REG-NR
           perform FORMAT-BANK-NAME
           perform FORMAT-BANK-ADRESSE
           perform FORMAT-BANK-TELEFON
           perform FORMAT-BANK-EMAIL
           
           PERFORM COPYFILD
       EXIT.

       FORMAT-KONTOUDSKRIFT.
      * Convert currency to DKK
           EVALUATE VALUTA
               WHEN "USD"
                   MULTIPLY FUNCTION NUMVAL(BELØB) BY USD
                   GIVING CONVERTED-VALUTA
               WHEN "EUR"
                   MULTIPLY FUNCTION NUMVAL(BELØB) BY EUR 
                   GIVING CONVERTED-VALUTA
               WHEN "DKK"
                   MOVE BELØB TO CONVERTED-VALUTA
           end-evaluate
           string  TIDSPUNKT delimited by space
                   " " delimited by size
                   TRANSAKTIONSTYPE delimited by space
                   " " delimited by size
                   function trim(BELØB)(1:1) DELIMITED BY SIZE
                   function Trim(CONVERTED-VALUTA) delimited by space
                   "DKK " delimited by size
                   function Trim(BELØB) delimited by space
                   VALUTA delimited by space
                   " " delimited by size
                   BUTIK delimited by space
                   into NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-REG-NR.
           STRING  "                                 " delimited by size
                   "                                 " delimited by size
                   "Registreringsnummer: "             delimited by size
                   REG-NR in BANK-ARRAY(IX)           delimited by space
                   into NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BANK-NAME.
           STRING  "                                 " delimited by size
                   "                                 " delimited by size
                   "Bank: "                            delimited by size
                   BANKNAVN in BANK-ARRAY(IX)         delimited by space
                   into NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BANK-ADRESSE.
           STRING  "                                 " delimited by size
                   "                                 " delimited by size
                   "Bankadresse: "                     delimited by size
                   BANKADRESSE in BANK-ARRAY(IX)      delimited by space
                   into NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BANK-TELEFON.
           STRING  "                                 " delimited by size
                   "                                 " delimited by size
                   "Telefon: "                         delimited by size
                   TELEFON in BANK-ARRAY(IX)          delimited by space
                   into NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BANK-EMAIL.
           STRING  "                                 " delimited by size
                   "                                 " delimited by size
                   "E-mail: "                          delimited by size
                   EMAIL in BANK-ARRAY(IX)            delimited by space
                   into NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-KONTOUDSKRIFT-START.
           STRING  "Kontoudskrift for kontonr.: "  delimited by size
                   KONTO-ID                        delimited by space
                   into NAVN-ADR
           PERFORM COPYFILD
       EXIT.

      *================================================================
      * FILE HANDLING PROCEDURES - Fil håndtering
      *================================================================
      
      *****************************************************************
      * READ-KONTO                                                    *
      * Formål: Læser alle konto records fra KontoOpl.txt ind i       *
      *         KONTO-ARRAY for hurtig søgning senere                 *
      * Output: KONTO-ARRAY fyldt med alle konto records              *
      * Info:   Kaldes kun én gang ved program start                  *
      *****************************************************************
       READ-BANKS.
           MOVE 1 TO IX                       *> Start ved array index 1
           OPEN INPUT INPUT-BANK-FILE
           PERFORM UNTIL END-OF-BANK-FILE = "Y"
               READ INPUT-BANK-FILE INTO BANKOPL
                   AT END
                       MOVE "Y" TO END-OF-BANK-FILE
                   NOT AT END
      *                display IX
      *                display BANKOPL
      * Gem konto record i array
                       MOVE BANKOPL TO BANK-ARRAY(IX)
      * Gå til næste array position
                       ADD 1 TO IX
               END-READ
           END-PERFORM
           CLOSE INPUT-BANK-FILE
      * Reset flag for næste brug
           MOVE "N" TO END-OF-BANK-FILE
       EXIT.

       FIND-BANK.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 100
               IF REG-NR IN BANK-ARRAY(IX) = REG-NR in TRANSAKTIONEROPL
                   EXIT perform 
               END-IF
           END-PERFORM
       EXIT.
