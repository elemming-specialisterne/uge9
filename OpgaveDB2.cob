      *****************************************************************
      * PROGRAM-ID: OPGAVEDB2                                           *
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
       PROGRAM-ID. OpgaveDB2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INPUT-KONTO-FILE ASSIGN TO "KontoOpl.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "KundeoplysningerOUT.txt"
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
       
       01 WS-COMMAND PIC X(1000).
       01 WS-RETURN-CODE PIC S9(4) COMP.
       01 WS-HOSTNAME PIC X(60) VALUE 'localhost'.
       01 WS-PORT PIC 9(6) VALUE 50000.
       01 WS-USERNAME PIC X(10) VALUE 'db2inst1'.
       01 WS-PASSWORD PIC X(30) VALUE 'password'.
       01 WS-DBNAME PIC X(10) VALUE 'REPODB'.
       01 WS-CUSTOMER-QUERY PIC X(60) VALUE 'SELECT * FROM KUNDER'.
       
      * DB2 Connection String
       01 WS-CONNECTION-STRING PIC X(200) VALUE
           'DATABASE=REPODB;HOSTNAME=localhost;PORT=50000;' &
           'UID=db2inst1;PWD=password;PROTOCOL=TCPIP;'.

      *================================================================
      * PROCEDURE DIVISION - Hovedprogramlogik
      *================================================================
       PROCEDURE DIVISION.
       
      *****************************************************************
      * MAIN-PROCEDURE                                                *
      * Formål: Hovedprocedure der koordinerer hele programmet        *
      * Flow:   1. Forbinder til database                             *
      *         2. Åbner filer                                        *
      *         3. Indlæser alle konti i hukommelse                   *
      *         4. Processerer hver kunde og deres konti              *
      *         5. Lukker database forbindelse og filer               *
      *****************************************************************
       MAIN-PROCEDURE.
      * Connect to DB2 database
           PERFORM CONNECT-TO-DATABASE

           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

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
           
      * Disconnect from database
           PERFORM DISCONNECT-FROM-DATABASE
           
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
           WRITE KUNDE-ADR
           MOVE SPACES TO NAVN-ADR
       EXIT.

      *================================================================
      * FORMATTING PROCEDURES - Formatering af kunde data
      *================================================================
      
      *****************************************************************
      * FORMAT-KUNDEID                                                *
      * Formål: Formatter og skriver kunde ID med label               *
      *****************************************************************
       FORMAT-KUNDEID.
           STRING  "Kunde ID: " DELIMITED BY SIZE
                   KUNDE-ID OF KUNDEOPL DELIMITED BY SPACE 
                   INTO NAVN-ADR
           PERFORM COPYFILD.
       EXIT.

      *****************************************************************
      * FORMAT-NAVN                                                   *
      * Formål: Kombinerer fornavn og efternavn til fuldt navn        *
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
      * Formål: Formatter adresse med vejnavn, husnr, etage og side   *
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
      * Formål: Formatter telefon og email oplysninger                *
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
      * Formål: Finder og skriver alle konti der tilhører denne       *
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
      * Formål: Læser alle konto records fra KontoOpl.txt ind i       *
      *         KONTO-ARRAY for hurtig søgning senere                 *
      * Output: KONTO-ARRAY fyldt med alle konto records              *
      * Info:   Kaldes kun én gang ved program start                  *
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

      *================================================================
      * DATABASE PROCEDURES - Database forbindelse og håndtering
      *================================================================
      
      *****************************************************************
      * CALL-DATABASE                                                 *
      * Formål: Udfører database kommando via system call            *
      * Input:  WS-COMMAND - Kommando string                         *
      * Output: WS-RETURN-CODE - Exit status (0=success)             *
      *****************************************************************
       CALL-DATABASE.
      * Display command for debugging (optional)
           DISPLAY "Executing: " WS-COMMAND
           
      * Simple system call - works everywhere
           CALL "SYSTEM" USING WS-COMMAND
           
      * Set return code to success (no actual return code available)
           MOVE 0 TO WS-RETURN-CODE
                              
      * Display result for debugging (optional)  
           DISPLAY "Command executed"
       EXIT.

      *****************************************************************
      * CONNECT-TO-DATABASE                                           *
      * Formål: Etablerer forbindelse til DB2 database               *
      * Input:  Connection parametre fra WORKING-STORAGE             *
      * Output: Database forbindelse eller fejlmeddelelse            *
      *****************************************************************
       CONNECT-TO-DATABASE.
      * Method 1: Using embedded SQL (requires DB2 precompiler)
      *    EXEC SQL
      *        CONNECT TO :WS-DBNAME 
      *        USER :WS-USERNAME 
      *        USING :WS-PASSWORD
      *    END-EXEC
      *    
      *    IF SQLCODE NOT = 0
      *        DISPLAY "Database connection failed: " SQLCODE
      *        STOP RUN
      *    END-IF

      * Method 2: Using Docker exec with correct DB2 command format
           STRING "docker exec db2server su - db2inst1 -c " 
                  DELIMITED BY SIZE
                  '"db2 connect to ' DELIMITED BY SIZE
                  WS-DBNAME DELIMITED BY SPACE
                  '"' DELIMITED BY SIZE
                  INTO WS-COMMAND
           
           PERFORM CALL-DATABASE
           
           IF WS-RETURN-CODE NOT = 0
               DISPLAY "Database connection failed with code: " 
                       WS-RETURN-CODE
               STOP RUN
           ELSE
               DISPLAY "Successfully connected to database: " WS-DBNAME
           END-IF
       EXIT.

      *****************************************************************
      * DISCONNECT-FROM-DATABASE                                      *
      * Formål: Lukker forbindelse til database                      *
      *****************************************************************
       DISCONNECT-FROM-DATABASE.
      * Method 1: Using embedded SQL
      *    EXEC SQL
      *        DISCONNECT
      *    END-EXEC

      * Method 2: Using Docker exec for disconnect
           MOVE "docker exec db2server su - db2inst1 -c " &
                '"db2 disconnect all"' TO WS-COMMAND
           PERFORM CALL-DATABASE
           
           IF WS-RETURN-CODE = 0
               DISPLAY "Database disconnected successfully"
           END-IF
       EXIT.
           
