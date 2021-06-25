REM $DYNAMIC
SUB LlistarAlbarans (AREA5, AREA2, DEVI$)
    SHARED DIRECCT$, DIRECCF$

    AREATXT = FREEFILE
    OPEN DIRECCT$ + "ALBARANS.TXT" FOR OUTPUT AS AREATXT
    AREA8 = FREEFILE: OPEN DIRECCF$ + "FACTURES.CAB" FOR RANDOM SHARED AS AREA8 LEN = LEN(FACTURA)
    AREA9 = FREEFILE: OPEN DIRECCF$ + "FACTURES.LIN" FOR RANDOM SHARED AS AREA9 LEN = LEN(LINFAC)
    RMF = LOF(AREA9) \ LEN(LINFAC)

    PAG = 1: L = 1
    GET AREA5, 1, CAP   ' AGAFAR CAP€ALERA DEL FITXER DE CAP€ALERES
    GOSUB CAP.LIST      ' IMPRIMIR CAP€ALERA

    ' DEFINIR MASCARA DE LES LINIES
    MASCARA$ = "  \" + SPACE$(9) + "\\" + SPACE$(47) + "\    ##,###,###.## ##.## ##.## ##,###,###.## \      \ \      \   !"
    MAXL = 52: TOTALB& = 0: TOTALN& = 0
    FOR Rl = 1 TO CEMPRE.MAXALBARAN - 1
        CALL FinestraEstat("Processant albarans...", 0)
        GET AREA2, Rl, ALBARAN
        GOSUB PRINTLINIA
        TOTALB& = TOTALB& + ALBARAN.TOTALBRUT
        TOTALN& = TOTALN& + ALBARAN.TOTALNET
        IF L >= MAXL THEN
           L = 1: PAG = PAG + 1
           PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + " " + STRING$(77, "Ä") + RTRIM$(ARCHIMP.COMPRIMIDO)
           PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
           GOSUB CAP.LIST
        END IF
    NEXT
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + " " + STRING$(77, "Ä") + RTRIM$(ARCHIMP.COMPRIMIDO)
    PRINT #AREATXT, USING MASCARA$; SPACE$(9); SPACE$(31) + "Suma dels Totals: "; TOTALB&; 0; 0; TOTALN&

    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT
    CLOSE AREA8, AREA9

    PrintOut "          - Llistant albarans facturats .,."
    CALL ImprimeixFitxerTXT(DIRECCT$ + "ALB_FACT.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    PrintOut "          - Llistant albarans pendents .,."
    CALL ImprimeixFitxerTXT(DIRECCT$ + "ALB_PEND.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP€ALERA DELS LLISTATS
' **************************************************************************

CAP.LIST:
    PRINT #AREATXT, ""
    PRINT #AREATXT, " "; RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, " LLISTAT DELS ALBARANS"
    PRINT #AREATXT, " P…gina:"; PAG
    PRINT #AREATXT, " Data..:"; FormatD$(Now#, "dd-mm-yyyy")
    PRINT #AREATXT, " " + STRING$(77, "Ä");
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""
    PRINT #AREATXT, "  ReferŠncia Nom del client                                          Subtotal   DTO % IVA % Total Net     Data     Data Fact. Fact."
    PRINT #AREATXT, "  ---------- ------------------------------------------------------- ---------- ----- ----- ------------- -------- ---------- -----"
    RETURN

PRINTLINIA:
    NOMPCLIENT$ = RTRIM$(ALBARAN.PERSONA.NOM) + " " + RTRIM$(ALBARAN.PERSONA.COGNOMS)
    DADE$ = "": FACT$ = ""
    IF ALBARAN.DOCUMENT = "F" THEN
       FACT$ = "X"
       FOR RF = 1 TO RMF
           CALL FinestraEstat("Processant ALBA->FACT.", 0)
           GET AREA9, RF, LINFAC
           IF LTRIM$(RTRIM$(LINFAC.LINIA(1).REFALBARAN)) = LTRIM$(RTRIM$(ALBARAN.REFALBARAN)) THEN
              GET AREA8, RF, FACTURA
              DADE$ = FACTURA.DADA
           END IF
       NEXT
    END IF
    PRINT #AREATXT, USING MASCARA$; ALBARAN.REFALBARAN; NOMPCLIENT$; ALBARAN.TOTALBRUT; ALBARAN.DTO; ALBARAN.TIPOIVA; ALBARAN.TOTALNET; ALBARAN.DADA; DADE$; FACT$
    L = L + 1
    RETURN
END SUB

