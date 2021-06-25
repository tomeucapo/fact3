SUB ImprimirFacturav (DEVI$, DP$, MAXALBS, AREA5, AREA3T, AREA2T, AREADOC, MI)
    SHARED DIRECCT$, CADFE$

    '$DYNAMIC
    DIM CLI(0 TO 7) AS STRING
    DIM FAC(0 TO 8) AS STRING
    DIM LINI(0 TO 5) AS STRING

    CLI(0) = FACTURA.CODCLIENT: CLI(1) = FACTURA.PERSONA.NOM
    CLI(2) = LTRIM$(RTRIM$(FACTURA.PERSONA.COGNOMS)): CLI(3) = "D.N.I. " + FACTURA.PERSONA.DNI
    CLI(4) = RTRIM$(ARCHIMP.NOENSANCHADO) + FACTURA.PERSONA.DIRECCIO: CLI(5) = LTRIM$(RTRIM$(FACTURA.PERSONA.CPOSTAL)) + " " + RTRIM$(LTRIM$(FACTURA.PERSONA.POBLACIO))
    CLI(6) = " Tels." + FACTURA.PERSONA.TELEFON1: CLI(7) = "      " + FACTURA.PERSONA.TELEFON2

    TO$ = FormatC$(FACTURA.TOTALBRUT, "##.###.###"): TOTB$ = TO$ + SPACE$(10 - LEN(TO$))
    BA$ = FormatC$(FACTURA.BASEIMPONIBLE, "##.###.###"): BASE$ = BA$ + SPACE$(10 - LEN(BA$))
    DT$ = FormatC$(FACTURA.DTO, "###"): DTO$ = DT$ + SPACE$(3 - LEN(DT$))
    IV$ = FormatC$(FACTURA.TIPOIVA, "###"): IVA$ = IV$ + SPACE$(3 - LEN(IV$))
    TI$ = FormatC$(FACTURA.TOTALIVA, "##.###.###"): TIVA$ = TI$ + SPACE$(10 - LEN(TI$))

    FAC(0) = RTRIM$(ARCHIMP.ENSANCHADO) + "Factura: " + FACTURA.REFFACTURA + " " + RTRIM$(ARCHIMP.NOENSANCHADO)
    FAC(1) = TOTB$: FAC(2) = BASE$
    FAC(3) = DTO$: FAC(4) = IVA$
    FAC(5) = TIVA$: FAC(6) = FormatD$(FACTURA.TOTALNET, "##.###.###")
    FAC(7) = FACTURA.PERSONA.FORMAPAGO: FAC(8) = ""

    GetBackground 1, 1, 25, 80, FACTU$
    COLOR 15, 2: FINESTRA 10, 30, 14, 45, 1, CAIXA1
    COLOR 15, 2: LOCATE 11, 31: PRINT "ACTUALITZANT"
    COLOR 15, 2: LOCATE 13, 31: PRINT "  FITXERS   "

    ' Crear el fitxer temporal d'impressi¢
    AREATXT = FREEFILE
    OPEN DIRECCT$ + "FACTURA.TXT" FOR OUTPUT SHARED AS AREATXT

    PAG = 1: GET AREA5, 1, CAP

    GET AREADOC, MI, TD


    LI = 1: L = 1
    SALT.PAG% = 1: MSK.PEUSP% = 0
    GOSUB PLANTILLA

    DO
        IF LI >= MAXLINS THEN
           FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
           PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
           PRINT #AREATXT, "Suma i continua..."
           PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
           FOR SALT = 1 TO SALT.F%: PRINT #AREATXT, "": NEXT
           PAG = PAG + 1: LI = 1
           SALT.PAG% = 1: GOSUB PLANTILLA
        END IF

        IF LIN$(L, 6) = "*" THEN
           TIPU$ = "FACTURA": GOSUB IMPRI.LINIA
           IF UCASE$(MID$(LIN$(L, 1), 1, 6)) = "C.A.:F" AND MID$(LIN$(L, 8), 8, 2) = "-A" THEN
              FOR RAC = 1 TO MAXALBS
                  GET AREA2T, RAC, ALBARAN
                  IF ALBARAN.REFALBARAN = LIN$(L, 8) THEN EXIT FOR
              NEXT
              GET AREA3T, RAC, LINALBA
              lalb = 1
              DO
                 IF LI >= MAXLINS THEN
                    FOR LC = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
                    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
                    PRINT #AREATXT, "Suma i continua..."
                    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
                    FOR SALT = 1 TO SALT.F%: PRINT #AREATXT, "": NEXT
                    PAG = PAG + 1: LI = 1
                    SALT.PAG% = 1: GOSUB PLANTILLA
                 END IF
                 TIPU$ = "ALBARAN": GOSUB IMPRI.LINIA
                 lalb = lalb + 1: LI = LI + 1
              LOOP UNTIL LINALBA.LINIA(lalb).MARCAR = "-"

              IF LIN$(L + 1, 6) <> "-" THEN
                 PRINT #AREATXT, STRING$(78, "-")
                 LI = LI + 1
              END IF
           END IF
           L = L + 1: LI = LI + 1
        END IF
    LOOP UNTIL LIN$(L, 6) = "-"
    FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT

    MSK.PEUSP% = 1: GOSUB PLANTILLA
    CLOSE #AREATXT

    COLOR 31, 2: LOCATE 11, 31: PRINT "  IMPRIMINT  "
    COLOR 31, 2: LOCATE 13, 31: PRINT "   FITXERS   "

    ImprimeixFitxerTXT DIRECCT$ + "FACTURA.TXT", DEVI$, 100
    PutBackground 1, 1, FACTU$
    ERASE FAC, CLI, LINI
    EXIT SUB
'****************************************************************

PLANTILLA:
    AREAPLA = FREEFILE: NOMP$ = LTRIM$(RTRIM$(DP$ + "PLANTILL\" + TD.FITXER))
    OPEN NOMP$ FOR INPUT SHARED AS AREAPLA

    '****************************************************************
    DO UNTIL EOF(AREAPLA)
       LINE INPUT #AREAPLA, LINIA$
       LL = LEN(LINIA$): GOSUB BUSCA.INSTR
       IF MSK.SURT% = 1 THEN EXIT DO
    LOOP
    '****************************************************************
    MSK.SURT% = 0: SALT.PAG% = 0
    CLOSE #AREAPLA
    RETURN

BUSCA.INSTR:
    CL = 1
    WHILE CL < LL
        IF MID$(LINIA$, CL, 1) = "[" THEN
           TOPE1 = CL + 1: TOPE2 = INSTR(TOPE1, LINIA$, "]") - 1
           LC = TOPE2 - TOPE1
           IN$ = MID$(LINIA$, TOPE1, LC + 1)

           IF MSK.PEUSP% = 1 THEN
              IF MID$(IN$, 1, 10) = "PEU_PAGINA" THEN
                 MSK.PEUSP% = 0
              END IF
           ELSE
              SELECT CASE UCASE$(IN$)
                  CASE "CAP€ALERA"
                       GOSUB CAPSAC
                  CASE "PAGINA"
                       PRINT #AREATXT, USING "\   \"; FormatD$(PAG, "#.###");
                  CASE "DATA"
                       PRINT #AREATXT, USING "\      \"; FACTURA.DADA;
                  CASE ELSE
                       IF MID$(IN$, 1, 3) = "MAX" THEN MAXLINS = VAL(MID$(IN$, 5, 2))
                       IF MID$(IN$, 1, 6) = "SEPARA" THEN SALT.F% = VAL(MID$(IN$, 8, 2))
                       IF MID$(IN$, 1, 6) = "CLIENT" THEN
                          PRINT #AREATXT, CLI(VAL(MID$(IN$, 8, 1)));
                       ELSE
                          IF MID$(IN$, 1, 3) = "FAC" THEN
                             PRINT #AREATXT, FAC(VAL(MID$(IN$, 5, 1)));
                          ELSE
                            IF MID$(IN$, 1, 5) = "LINIA" THEN
                                  MSK.SURT% = 1: RETURN
                            END IF
                          END IF
                       END IF
              END SELECT
              CL = CL + LC + 2
           END IF
        ELSE
           IF MSK.PEUSP% = 0 AND MID$(LINIA$, 1, 12) <> "[PEU_PAGINA]" THEN PRINT #AREATXT, MID$(LINIA$, CL, 1);
        END IF
        CL = CL + 1
    WEND
    IF MSK.PEUSP% = 0 THEN PRINT #AREATXT, ""
    RETURN

IMPRI.LINIA:
    SELECT CASE TIPU$
        CASE IS = "ALBARAN"
             LINI(0) = LINALBA.LINIA(lalb).CODART
             LINI(1) = LINALBA.LINIA(lalb).CONCEPTE
             LINI(2) = STR$(LINALBA.LINIA(lalb).QUANTI)
             LINI(3) = STR$(LINALBA.LINIA(lalb).preu)
             LINI(4) = STR$(LINALBA.LINIA(lalb).DTO)
             LINI(5) = STR$(LINALBA.LINIA(lalb).import)
        CASE IS = "FACTURA"
             LINI(0) = LIN$(L, 1)
             LINI(1) = LIN$(L, 2)
             LINI(2) = LIN$(L, 4)
             LINI(3) = LIN$(L, 3)
             LINI(4) = LIN$(L, 7)
             LINI(5) = LIN$(L, 5)
        CASE ELSE
    END SELECT

    IF UCASE$(MID$(LIN$(L, 1), 1, 6)) = "C.A.:F" AND MID$(LIN$(L, 8), 8, 2) = "-A" AND TIPU$ = "FACTURA" THEN
       CA$ = ""
       PRINT #AREATXT, USING " \                \"; CA$;
       PRINT #AREATXT, LINI(1)
    ELSE
       q$ = LTRIM$(FormatC$(VAL(LINI(2)), "###.###")): QUANT$ = SPACE$(7 - LEN(q$)) + q$
       p$ = FormatC$(VAL(LINI(3)), "#.###.###"): preu$ = SPACE$(9 - LEN(p$)) + p$
       I$ = FormatC$(VAL(LINI(5)), "##.###.###"): import$ = SPACE$(10 - LEN(I$)) + I$

       'PRINT #AREATXT, USING " \                \"; LINI(0);
       PRINT #AREATXT, USING "\     \  "; QUANT$;
       PRINT #AREATXT, " " + LINI(1);
       PRINT #AREATXT, USING " \       \"; preu$;
       PRINT #AREATXT, USING "\\"; FormatC$(VAL(LINI(4)), "##");
       PRINT #AREATXT, USING "    \        \"; import$
    END IF
    RETURN

CAPSAC:
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + RTRIM$(CAP.LINIES(0)) + RTRIM$(ARCHIMP.NOENSANCHADO)
    FOR I = 1 TO 4: PRINT #AREATXT, CAP.LINIES(I): NEXT
    RETURN
END SUB

