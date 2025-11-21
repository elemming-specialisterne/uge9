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

       01 USD              PIC 9V9 VALUE 6.8.
       01 EUR              PIC 9V9 VALUE 7.5.

       01 CONVERTED-VALUTA     PIC 9(14)V99.
       01 INDBETALT-DKK        PIC 9(14)V99 VALUE  ZEROES.
       01 UDBETALT-DKK         PIC 9(14)V99 VALUE ZEROES.
       01 SALDO-DKK            PIC 9(14)V99 VALUE  ZEROES.
       01 CONVERTED-DISPLAY    PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01 BELØB-DISPLAY        PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01 SIGN-DISPLAY         PIC x VALUE " ".

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
           MOVE 50000 TO SALDO-DKK

      * Processér hver kunde og format deres oplysninger
           PERFORM UNTIL END-OF-FILE = "Y"
               READ INPUT-FILE INTO TRANSAKTIONEROPL
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       IF PREV-REG-NR NOT = REG-NR IN TRANSAKTIONEROPL
                           AND PREV-REG-NR NOT = SPACES
                           PERFORM SLUT-KONTO
                       END-IF

                       IF PREV-REG-NR NOT = REG-NR IN TRANSAKTIONEROPL
                          PERFORM START-KONTO
                       END-IF
                       
                       PERFORM KONTO-MATH
                       PERFORM FORMAT-KONTOUDSKRIFT


               END-READ
           END-PERFORM
           PERFORM SLUT-KONTO
           
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
           STRING  "-----------------------------" DELIMITED BY SIZE
                   "-----------------------------" DELIMITED BY SIZE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-KUNDENAVN.
           STRING  "Kunde: " DELIMITED BY SIZE
                   NAVN DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-ADRESSE.
           STRING  "Adresse: " DELIMITED BY SIZE
                   ADRESSE DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BANK.
           PERFORM FIND-BANK
           PERFORM FORMAT-REG-NR
           PERFORM FORMAT-BANK-NAME
           PERFORM FORMAT-BANK-ADRESSE
           PERFORM FORMAT-BANK-TELEFON
           PERFORM FORMAT-BANK-EMAIL
           
           PERFORM COPYFILD
       EXIT.

       FORMAT-KONTOUDSKRIFT.
           MOVE CONVERTED-VALUTA TO CONVERTED-DISPLAY
           MOVE FUNCTION NUMVAL(BELØB) TO BELØB-DISPLAY
      * Handle negative sign for BELØB-DISPLAY
           IF FUNCTION trim(BELØB)(1:1) = "-"
               MOVE "-" TO SIGN-DISPLAY
           ELSE
               MOVE " " TO SIGN-DISPLAY
           END-IF
           STRING  TIDSPUNKT DELIMITED BY SPACE
                   " " DELIMITED BY SIZE
                   TRANSAKTIONSTYPE DELIMITED BY SPACE
                   " " DELIMITED BY SIZE
                   SIGN-DISPLAY DELIMITED BY SPACE
                   FUNCTION trim(CONVERTED-DISPLAY) DELIMITED BY SPACE
                   "DKK " DELIMITED BY SIZE
                   SIGN-DISPLAY DELIMITED BY SPACE
                   FUNCTION trim(BELØB-DISPLAY) DELIMITED BY SPACE
                   VALUTA DELIMITED BY SPACE
                   " " DELIMITED BY SIZE
                   BUTIK DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-REG-NR.
           STRING  "                         " DELIMITED BY SIZE
                   "                         " DELIMITED BY SIZE
                   "Registreringsnummer: "     DELIMITED BY SIZE
                   REG-NR IN BANK-ARRAY(IX)    DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BANK-NAME.
           STRING  "                         " DELIMITED BY SIZE
                   "                         " DELIMITED BY SIZE
                   "Bank: "                    DELIMITED BY SIZE
                   BANKNAVN IN BANK-ARRAY(IX)  DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BANK-ADRESSE.
           STRING  "                         "     DELIMITED BY SIZE
                   "                         "     DELIMITED BY SIZE
                   "Bankadresse: "                 DELIMITED BY SIZE
                   BANKADRESSE IN BANK-ARRAY(IX)   DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BANK-TELEFON.
           STRING  "                         " DELIMITED BY SIZE
                   "                         " DELIMITED BY SIZE
                   "Telefon: "                 DELIMITED BY SIZE
                   TELEFON IN BANK-ARRAY(IX)   DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BANK-EMAIL.
           STRING  "                         " DELIMITED BY SIZE
                   "                         " DELIMITED BY SIZE
                   "E-mail: "                  DELIMITED BY SIZE
                   EMAIL IN BANK-ARRAY(IX)     DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-KONTOUDSKRIFT-START.
           STRING  "Kontoudskrift for kontonr.: "  DELIMITED BY SIZE
                   KONTO-ID                        DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.
       
       FORMAT-INDBETALING.
           MOVE INDBETALT-DKK TO CONVERTED-DISPLAY 
           STRING  "Totalt indbetalt (DKK): " DELIMITED BY SIZE
                   FUNCTION trim(CONVERTED-DISPLAY) DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.
       FORMAT-UDBETALING.
           MOVE UDBETALT-DKK TO CONVERTED-DISPLAY 
           STRING  "Totalt udbetalt (DKK): " DELIMITED BY SIZE
                   "-" DELIMITED BY SPACE
                   FUNCTION trim(CONVERTED-DISPLAY) DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.
       FORMAT-SALDO.
           IF FUNCTION trim(SALDO-DKK)(1:1) = "-"
               MOVE "-" TO SIGN-DISPLAY
           ELSE
               MOVE " " TO SIGN-DISPLAY
           END-IF
           MOVE SALDO-DKK TO CONVERTED-DISPLAY 
           STRING  "Totalt udbetalt (DKK): " DELIMITED BY SIZE
                   SIGN-DISPLAY DELIMITED BY SPACE
                   FUNCTION trim(CONVERTED-DISPLAY) DELIMITED BY SPACE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.
       FORMAT-SIGNOUT.
           MOVE "Med venlig hilse" TO NAVN-ADR
           PERFORM COPYFILD
           MOVE FUNCTION  trim(BANKNAVN IN BANK-ARRAY(IX)) TO NAVN-ADR
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
       START-KONTO.
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

           MOVE REG-NR IN TRANSAKTIONEROPL TO PREV-REG-NR
       EXIT.
       
       SLUT-KONTO.
           PERFORM COPYFILD
           PERFORM COPYFILD
           PERFORM FORMAT-INDBETALING
           PERFORM FORMAT-UDBETALING
           PERFORM FORMAT-SALDO
           PERFORM COPYFILD

           PERFORM FORMAT-SIGNOUT
           PERFORM COPYFILD
           PERFORM COPYFILD
           PERFORM COPYFILD

           MOVE ZEROES TO INDBETALT-DKK
           MOVE ZEROES TO UDBETALT-DKK
           MOVE 50000 TO SALDO-DKK
       EXIT.
       KONTO-MATH.
      * Convert currency to DKK
           EVALUATE VALUTA
               WHEN "USD"
                   MULTIPLY FUNCTION NUMVAL(BELØB) BY USD
                   GIVING CONVERTED-VALUTA
               WHEN "EUR"
                   MULTIPLY FUNCTION NUMVAL(BELØB) BY EUR 
                   GIVING CONVERTED-VALUTA
               WHEN "DKK"
                   MOVE FUNCTION NUMVAL(BELØB) TO CONVERTED-VALUTA
           END-EVALUATE
           EVALUATE FUNCTION trim(BELØB)(1:1)
               WHEN "-"
                   ADD CONVERTED-VALUTA TO UDBETALT-DKK
                   SUBTRACT CONVERTED-VALUTA FROM SALDO-DKK
               WHEN OTHER
                   ADD CONVERTED-VALUTA TO INDBETALT-DKK
                   ADD CONVERTED-VALUTA TO SALDO-DKK
           END-EVALUATE
       EXIT.

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
                   EXIT PERFORM 
               END-IF
           END-PERFORM
       EXIT.
