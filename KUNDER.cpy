
           02 KUNDE-ID             PIC X(10) VALUE spaces.
           02 FORNAVN              PIC X(20) VALUE spaces.
           02 EFTERNAVN            PIC X(20) VALUE spaces.
           02 KONTOINFO.
               03 KONTONUMMER      PIC X(20) VALUE spaces.
               03 BALANCE          PIC 9(7)V99 VALUE zeros.
               03 VALUTAKODE       PIC X(3) VALUE spaces.
           02 ADDRESSE.
               03 VEJNAVN          PIC X(30) VALUE spaces.
               03 HUSNR            PIC X(5) VALUE spaces.
               03 ETAGE            PIC X(5) VALUE spaces.
               03 SIDE             PIC X(5) VALUE spaces.
               03 CITY             PIC X(20) VALUE spaces.
               03 POSTNR           PIC X(4) VALUE spaces.
               03 LANDE-KODE       PIC X(2) VALUE spaces.
           02 .
               03 TELEFON          PIC X(8) VALUE spaces.
               03 EMAIL            PIC X(50) VALUE spaces.
