       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opgave5.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 KUNDEOPL.
           COPY "KUNDER.cpy".

       PROCEDURE DIVISION.
      *Nedenfor kommer en display - Cobols måde at skrive i konsollen
       MOVE "1234567890" TO KUNDE-ID.
       MOVE "Lars" TO FORNAVN.
       MOVE "Hansen" TO EFTERNAVN.
       MOVE "DK123445678912345" TO KONTONUMMER.
       MOVE "2500.75" TO BALANCE.
       MOVE "DKK" TO VALUTAKODE.
       MOVE "Lautruphøjvej" TO VEJNAVN.
       MOVE "1" TO HUSNR.
       MOVE "5" TO ETAGE.
       MOVE "TV" TO SIDE.
       MOVE "Copenhagen" TO CITY.
       MOVE "2605" TO POSTNR.
       MOVE "45" TO LANDE-KODE.
       MOVE "88888888" TO TELEFON.
       MOVE "test.test@test.dk" TO EMAIL.


       DISPLAY KUNDEOPL
       STOP RUN.
