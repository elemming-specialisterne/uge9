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
           SELECT OUTPUT-FILE ASSIGN TO "Statastic.txt"
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

       01 TOP-KUNDER OCCURS 4 TIMES.
           02 KUNDE-ID         PIC X(15) VALUE SPACES.
           02 NAVN             PIC X(30) VALUE SPACES.
           02 SALDO            PIC 9(14)V99 VALUE  ZEROES.

       01 M-IX                 PIC 9(2) VALUE 1.
       01 S-IX                 PIC 9(2) VALUE 1.
       01 MÅNED OCCURS 12 TIMES.
           02 INDBETALT        PIC 9(14)V99 VALUE  ZEROES.
           02 UDBETALT         PIC 9(14)V99 VALUE  ZEROES.

       01 MÅNEDER PIC X(10) OCCURS 12 TIMES.
       01 BUTIK-ARR OCCURS 14 TIMES. *> Last Butik is for sorting
           02 B-TYPE           PIC X(20).
           02 B-ANTAL-TRANS    PIC 9(5) VALUE  ZEROES.
           02 B-SALDO          PIC 9(14)V99 VALUE  ZEROES.

       01 SWAP-1               PIC 9(2).
       01 SWAP-2               PIC 9(2).

       01 USD              PIC 9V9 VALUE 6.8.
       01 EUR              PIC 9V9 VALUE 7.5.

       01 CONVERTED-VALUTA     PIC 9(14)V99.
       01 INDBETALT-DKK        PIC 9(14)V99 VALUE  ZEROES.
       01 UDBETALT-DKK         PIC 9(14)V99 VALUE ZEROES.
       01 SALDO-DKK            PIC 9(14)V99 VALUE  ZEROES.
       01 CONVERTED-DISPLAY    PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01 BELØB-DISPLAY        PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01 FORMATTED-BELØB      PIC X(21).
       01 STRING-PTR           PIC 9(3).
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

           PERFORM INIT-PROCEDURE

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

                          MOVE REG-NR IN TRANSAKTIONEROPL TO PREV-REG-NR
                       END-IF
                       
                       PERFORM KONTO-MATH
               END-READ
           END-PERFORM
           PERFORM SLUT-KONTO

           PERFORM END-PROCEDURE
           
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

       START-KONTO.
           MOVE SPACES TO NAVN-ADR

           MOVE REG-NR IN TRANSAKTIONEROPL TO PREV-REG-NR
       EXIT.
       
       SLUT-KONTO.
           PERFORM SAVE-TOP-3
           MOVE ZEROES TO INDBETALT-DKK
           MOVE ZEROES TO UDBETALT-DKK
           MOVE 50000 TO SALDO-DKK
       EXIT.

       INIT-PROCEDURE.
           PERFORM INIT-MÅNEDER
           PERFORM INIT-BUTIKKER
           PERFORM READ-BANKS
           MOVE 50000 TO SALDO-DKK
       EXIT.
       
       END-PROCEDURE.
           PERFORM FORMAT-TOP-3
           PERFORM COPYFILD
           PERFORM COPYFILD
           PERFORM COPYFILD

           PERFORM FORMAT-MÅNEDER
           PERFORM COPYFILD
           PERFORM COPYFILD
           PERFORM COPYFILD

           PERFORM FORMAT-BUTIKER
           PERFORM COPYFILD
           PERFORM COPYFILD
           PERFORM COPYFILD

           PERFORM FORMAT-TOP-BUTIKER
           PERFORM COPYFILD
           PERFORM COPYFILD
           PERFORM COPYFILD
       EXIT.
      *================================================================
      * FORMATTING PROCEDURES - Formatering af kunde data
      *================================================================
      
       FORMAT-TOP-3.
           MOVE "Top 3 kunder med højeste saldo:" TO NAVN-ADR
           PERFORM COPYFILD

           MOVE TOP-KUNDER(1) TO TOP-KUNDER(4)
           PERFORM FORMAT-TOP-KUNDE
           MOVE TOP-KUNDER(2) TO TOP-KUNDER(4)
           PERFORM FORMAT-TOP-KUNDE
           MOVE TOP-KUNDER(3) TO TOP-KUNDER(4)
           PERFORM FORMAT-TOP-KUNDE
       EXIT.

       FORMAT-TOP-KUNDE.
           MOVE SALDO IN TOP-KUNDER(4) TO CONVERTED-DISPLAY
      * Handle negative sign for BELØB-DISPLAY
           IF FUNCTION trim(SALDO IN TOP-KUNDER(4))(1:1) = "-"
               MOVE "-" TO SIGN-DISPLAY
           ELSE
               MOVE " " TO SIGN-DISPLAY
           END-IF
           STRING  "Kunde-ID: " DELIMITED BY SIZE
                   KUNDE-ID IN TOP-KUNDER(4) DELIMITED BY SPACE
                   ", Navn: " DELIMITED BY SIZE
                   NAVN IN TOP-KUNDER(4) DELIMITED BY SPACE
                   ", Saldo: " DELIMITED BY SIZE
                   FUNCTION trim(CONVERTED-DISPLAY) DELIMITED BY SPACE
                   "DKK " DELIMITED BY SIZE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-MÅNEDER.
           STRING  "Måned     " DELIMITED BY SIZE
                   "  Indbetalinger (DKK)" DELIMITED BY SIZE
                   "   Udbetalinger (DKK)" DELIMITED BY SIZE
                   INTO NAVN-ADR
           PERFORM COPYFILD
           MOVE 1 TO M-IX
           PERFORM UNTIL M-IX > 12
               PERFORM FORMAT-MÅNED
               ADD 1 TO M-IX
           END-PERFORM
       EXIT.

       FORMAT-MÅNED.
           MOVE INDBETALT IN MÅNED(M-IX) TO CONVERTED-DISPLAY
           MOVE UDBETALT IN MÅNED(M-IX) TO BELØB-DISPLAY
      * Create right-aligned 21-char field with minus and amount
           MOVE SPACES TO FORMATTED-BELØB
      * Calculate starting position for right alignment (21 - length + 1)
           COMPUTE STRING-PTR = 21 - LENGTH OF 
               FUNCTION TRIM(BELØB-DISPLAY)
           STRING "-" DELIMITED BY SIZE
                  FUNCTION TRIM(BELØB-DISPLAY) DELIMITED BY SPACE
                  INTO FORMATTED-BELØB
                  WITH POINTER STRING-PTR
           STRING  MÅNEDER(M-IX) DELIMITED BY SIZE
                   CONVERTED-DISPLAY DELIMITED BY SIZE
                   FORMATTED-BELØB DELIMITED BY SIZE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-BUTIKER.
           STRING  "Butik      " DELIMITED BY SIZE
                   "Antal transaktioner" DELIMITED BY SIZE
                   INTO NAVN-ADR
           PERFORM COPYFILD
           MOVE 1 TO S-IX
           PERFORM UNTIL S-IX > 13
               PERFORM FORMAT-BUTIK
               ADD 1 TO S-IX
           END-PERFORM
       EXIT.

       FORMAT-BUTIK.
           STRING  B-TYPE IN BUTIK-ARR(S-IX) DELIMITED BY SIZE
                   "     " DELIMITED BY SIZE
                   B-ANTAL-TRANS IN BUTIK-ARR(S-IX) DELIMITED BY SIZE
                   INTO NAVN-ADR
           PERFORM COPYFILD
       EXIT.

       FORMAT-TOP-BUTIKER.
           PERFORM SORT-BUTIKER-BY-SALDO
           MOVE "Top 5 butikker med højeste omsætning:" TO NAVN-ADR
           PERFORM COPYFILD

           MOVE 1 TO SWAP-1
           PERFORM FORMAT-TOP-BUTIK 5 TIMES
       EXIT.

       FORMAT-TOP-BUTIK.
           MOVE B-SALDO IN BUTIK-ARR(SWAP-1) TO CONVERTED-DISPLAY
           STRING "Butik type: " DELIMITED BY SIZE
                  B-TYPE IN BUTIK-ARR(SWAP-1) DELIMITED BY SPACE
                  " havde " DELIMITED BY SIZE
                  B-ANTAL-TRANS IN BUTIK-ARR(SWAP-1) DELIMITED BY SPACE
                  " Transaktioner " DELIMITED BY SIZE
                  "for en omsætning på: " DELIMITED BY SIZE
                  FUNCTION trim(CONVERTED-DISPLAY) DELIMITED BY SPACE
                  "DKK " DELIMITED BY SIZE
                  INTO NAVN-ADR
           PERFORM COPYFILD
           ADD 1 TO SWAP-1
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
           PERFORM BUTIK-MATH
           PERFORM FIND-MÅNED
           EVALUATE TRANSAKTIONSTYPE
               WHEN "Indbetaling"
                   ADD CONVERTED-VALUTA TO UDBETALT IN MÅNED(M-IX)
               WHEN "Udbetaling"
                   ADD CONVERTED-VALUTA TO INDBETALT IN MÅNED(M-IX)
           END-EVALUATE
       EXIT.

       BUTIK-MATH.
           PERFORM FIND-BUTIK-TYPE
           ADD 1 TO B-ANTAL-TRANS IN BUTIK-ARR(S-IX)
           EVALUATE FUNCTION trim(BELØB)(1:1)
               WHEN "-"
                   SUBTRACT CONVERTED-VALUTA 
                       FROM B-SALDO IN BUTIK-ARR(S-IX)
               WHEN OTHER
                   ADD CONVERTED-VALUTA TO B-SALDO IN BUTIK-ARR(S-IX)
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
      * Gem konto record i array
                       MOVE BANKOPL TO BANK-ARRAY(IX)
      * Gå til næste array position
                       ADD 1 TO IX
               END-READ
           END-PERFORM
           CLOSE INPUT-BANK-FILE
       EXIT.

       SAVE-TOP-3.
      * Save current customer to temp
           MOVE KONTO-ID IN TRANSAKTIONEROPL 
               TO KUNDE-ID IN TOP-KUNDER(4)
           MOVE NAVN IN TRANSAKTIONEROPL TO NAVN IN TOP-KUNDER(4)  
           MOVE SALDO-DKK TO SALDO IN TOP-KUNDER(4)
           
      * Check if better than position 1 (highest)
           IF SALDO-DKK > SALDO IN TOP-KUNDER(1)
              MOVE 1 TO SWAP-2
              PERFORM SHIFT-DOWN-TOP-KUNDER
              MOVE 1 TO SWAP-1
              PERFORM INSERT-AT-POSITION
           ELSE
      * Check if better than position 2
              IF SALDO-DKK > SALDO IN TOP-KUNDER(2)
                 MOVE 2 TO SWAP-2
                 PERFORM SHIFT-DOWN-TOP-KUNDER
                 MOVE 2 TO SWAP-1
                 PERFORM INSERT-AT-POSITION
              ELSE
      * Check if better than position 3
                 IF SALDO-DKK > SALDO IN TOP-KUNDER(3)
                    MOVE 3 TO SWAP-1
                    PERFORM INSERT-AT-POSITION
                 END-IF
              END-IF
           END-IF
       EXIT.
       
      * Shift procedures to make room for new entries
       SHIFT-DOWN-FROM-1.
      * Move position 1 to 2, and position 2 to 3
           MOVE TOP-KUNDER(2) TO TOP-KUNDER(3)
           MOVE TOP-KUNDER(1) TO TOP-KUNDER(2)
       EXIT.
       
       SHIFT-DOWN-FROM-2.
      * Move position 2 to 3
           MOVE TOP-KUNDER(2) TO TOP-KUNDER(3)
       EXIT.
       
       SHIFT-DOWN-TOP-KUNDER.
      * Move position 2 to 3
           MOVE 3 TO SWAP-1
           PERFORM UNTIL SWAP-1 = SWAP-2
               MOVE TOP-KUNDER(SWAP-1 - 1) to TOP-KUNDER(SWAP-1)
               SUBTRACT 1 FROM SWAP-1
           END-PERFORM
           ADD 1 TO SWAP-1
           MOVE TOP-KUNDER(2) TO TOP-KUNDER(3)
       EXIT.
       
      * Insert procedures
       INSERT-AT-POSITION.
           MOVE TOP-KUNDER(4) TO TOP-KUNDER(SWAP-1)
       EXIT.

       FIND-MÅNED.
      * Extract month from TIDSPUNKT (format: YYYY-MM-DD HH:MM:SS.mmm)
      * Month is at position 6-7 in the string
           MOVE FUNCTION NUMVAL(TIDSPUNKT(6:2)) TO M-IX
       EXIT.

       FIND-BUTIK-TYPE.
      * Find matching store type in BUTIK array
           PERFORM VARYING S-IX FROM 1 BY 1 UNTIL S-IX > 13
               IF BUTIK IN TRANSAKTIONEROPL = B-TYPE(S-IX)
                   EXIT PERFORM
               END-IF
           END-PERFORM
       EXIT.

       SORT-BUTIKER-BY-SALDO.
      * Bubble sort BUTIK array by B-SALDO (descending order)
           MOVE 1 TO SWAP-1
           PERFORM UNTIL SWAP-1 > 12
               DISPLAY SWAP-1
               MOVE 1 TO SWAP-2
               PERFORM UNTIL SWAP-2 > (13 - SWAP-1)
                   IF B-SALDO IN BUTIK-ARR(SWAP-2) < 
                      B-SALDO IN BUTIK-ARR(SWAP-2 + 1)
                       PERFORM SWAP-BUTIK-ENTRIES
                   END-IF
                   ADD 1 TO SWAP-2
               END-PERFORM
               ADD 1 TO SWAP-1
           END-PERFORM
       EXIT.

       SWAP-BUTIK-ENTRIES.
      * Swap BUTIK entries at positions SWAP-2 and SWAP-2+1
           MOVE BUTIK-ARR(SWAP-2) TO BUTIK-ARR(14)
           MOVE BUTIK-ARR(SWAP-2 + 1) TO BUTIK-ARR(SWAP-2)
           MOVE BUTIK-ARR(14) TO BUTIK-ARR(SWAP-2 + 1)
       EXIT.

       INIT-BUTIKKER.
           MOVE "Supermarked         " TO B-TYPE(1)
           MOVE "Tojbutik            " TO B-TYPE(2)
           MOVE "Elektronikbutik     " TO B-TYPE(3)
           MOVE "Restaurant          " TO B-TYPE(4)
           MOVE "Boghandel           " TO B-TYPE(5)
           MOVE "Apotek              " TO B-TYPE(6)
           MOVE "Tankstation         " TO B-TYPE(7)
           MOVE "Cafe                " TO B-TYPE(8)
           MOVE "Biograf             " TO B-TYPE(9)
           MOVE "Mobelbutik          " TO B-TYPE(10)
           MOVE "Blomsterhandler     " TO B-TYPE(11)
           MOVE "Bageri              " TO B-TYPE(12)
           MOVE "Fitnesscenter       " TO B-TYPE(13)
       EXIT.

       INIT-MÅNEDER.
           MOVE "Januar    " TO MÅNEDER(1)
           MOVE "Februar   " TO MÅNEDER(2)
           MOVE "Marts     " TO MÅNEDER(3)
           MOVE "April     " TO MÅNEDER(4)
           MOVE "Maj       " TO MÅNEDER(5)
           MOVE "Juni      " TO MÅNEDER(6)
           MOVE "Juli      " TO MÅNEDER(7)
           MOVE "August    " TO MÅNEDER(8)
           MOVE "September " TO MÅNEDER(9)
           MOVE "Oktober   " TO MÅNEDER(10)
           MOVE "November  " TO MÅNEDER(11)
           MOVE "December  " TO MÅNEDER(12)
       EXIT.

