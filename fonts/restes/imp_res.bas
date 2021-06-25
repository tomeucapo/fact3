    GET AREAR, REG, RESGUA
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + RTRIM$(LTRIM$(CAP.LINIES(0))) + RTRIM$(ARCHIMP.NOENSANCHADO)
    FOR I = 1 TO 4: PRINT #AREATXT, CAP.LINIES(I): NEXT

    PRINT #AREATXT, ""
    PRINT #AREATXT, " " + RTRIM$(ARCHIMP.ENSANCHADO) + " Reparaci¢/Factura comptat: " + RTRIM$(LTRIM$(RESGUA.ORDRE)) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, "Codi Client:                                   " + RESGUA.PERSONA.NOM + RTRIM$(ARCHIMP.NEGRITA)
    PRINT #AREATXT, RESGUA.CODCLIENT + SPACE$(29) + RTRIM$(ARCHIMP.NONEGRITA);
    PRINT #AREATXT, SPACE$(8) + RESGUA.PERSONA.COGNOMS
    PRINT #AREATXT, SPACE$(42) + "     D.N.I. " + RESGUA.PERSONA.DNI
    PRINT #AREATXT, "Marca:                Model:"; SPACE$(17) + "  " + RESGUA.PERSONA.DIRECCIO + RTRIM$(ARCHIMP.NEGRITA)
    PRINT #AREATXT, USING "\                   \ \                   \ \ \"; RESGUA.MARCA; RESGUA.MODELO; RTRIM$(ARCHIMP.NONEGRITA);
    PRINT #AREATXT, SPACE$(2) + LTRIM$(RTRIM$(RESGUA.PERSONA.POBLACIO)) + " " + RESGUA.PERSONA.CPOSTAL
    PRINT #AREATXT, SPACE$(45) + "  Tels." + RESGUA.PERSONA.TELEFON1
    PRINT #AREATXT, SPACE$(45) + "       " + RESGUA.PERSONA.TELEFON2
    PRINT #AREATXT, STRING$(78, "Ä")
    PRINT #AREATXT, "Aparell:              N§ SŠrie:            Data Entrada:     Data sortida:" + RTRIM$(ARCHIMP.NEGRITA)
    PRINT #AREATXT, USING "\                  \  \                  \ \           \     \           \&"; RESGUA.APARELL; RESGUA.NSERIE; RESGUA.FETXAIN; RESGUA.FETXAOUT; RTRIM$(ARCHIMP.NONEGRITA)
    PRINT #AREATXT, STRING$(78, "Ä")
    PRINT #AREATXT, "Observacions Client: " + RESGUA.OBSERVACL(1)
    PRINT #AREATXT, "                     " + RESGUA.OBSERVACL(2)
    PRINT #AREATXT, "                     " + RESGUA.OBSERVACL(3)
    PRINT #AREATXT, STRING$(78, "Ä")
    PRINT #AREATXT, "Observacions Servei: " + RESGUA.OBSERVASE(1)
    PRINT #AREATXT, "                     " + RESGUA.OBSERVASE(2)
    PRINT #AREATXT, "                     " + RESGUA.OBSERVASE(3)
    PRINT #AREATXT, STRING$(78, "Ä")
    IF MSG.G$ = "IMPRIMEIX_LINIES" THEN
       PRINT #AREATXT, "QUANT.    CONCEPTE                                 PREU        IMPORT"
       PRINT #AREATXT, STRING$(78, "Ä")
       FOR L = 1 TO 6
           GET AREAF, R, FACTURA
           IF FACTURA.QUANTITAT = 0 THEN
              QUANT$ = "         "
           ELSE
              QUANT$ = FormatD$(FACTURA.QUANTITAT, "##.###,0")
           END IF
           PRINT #AREATXT, USING "\       \ "; QUANT$;
           PRINT #AREATXT, USING "\                                      \ "; FACTURA.CONCEPTE;
           PRINT #AREATXT, USING "\         \ "; RTRIM$(FormatD$(FACTURA.PREU, "#.###.###"));
           PRINT #AREATXT, USING "\          \"; FormatD$(FACTURA.IMPORT, "#.###.###")
           R = R + 1
       NEXT
       PRINT #AREATXT, STRING$(78, "Ä")
       PRINT #AREATXT, "SUBTOTAL:     BASE IMPONIBLE:   ";
       IF FACTURA.DTO > 0 THEN PRINT #AREATXT, "DTO%:   "
       PRINT #AREATXT, "IVA%:    TOTAL IVA:  TOTAL REPARACI¢:"
       PRINT #AREATXT, USING "  ###,###             ###,###      "; FACTURA.TOTALBRUT; FACTURA.BASEIMPONIBLE;
       IF FACTURA.DTO > 0 THEN PRINT #AREATXT, USING "##      "; FACTURA.DTO;
       PRINT #AREATXT, USING "##    \        \           \        \"; FACTURA.TIPOIVA; FormatD$(FACTURA.TOTALIVA, "#.###.###"); FormatD$(FACTURA.TOTALNET, "#.###.###")
       PRINT #AREATXT, STRING$(78, "Ä")
    END IF
    PRINT #AREATXT, USING "\" + SPACE$(48) + "\   FIRMA DEL CLIENT:"; PP.LINIES(0, 0)
    PRINT #AREATXT, USING "\" + SPACE$(48) + "\"; PP.LINIES(0, 1)
    PRINT #AREATXT, USING "\" + SPACE$(48) + "\"; PP.LINIES(0, 2)
    PRINT #AREATXT, SPACE$((40 \ 2) - (LEN(CADFE$) \ 2)) + RTRIM$(ARCHIMP.ENSANCHADO) + CADFE$ + RTRIM$(ARCHIMP.NOENSANCHADO)

    FOR L = 1 TO 17: PRINT #AREATXT, "": NEXT
    RETURN

