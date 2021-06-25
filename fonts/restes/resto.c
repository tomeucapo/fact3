
                       i = 1
                       J = MAXCL - 1

                       trobat = 0
                       WHILE (i < J) AND trobat = 0
                          M = (i + J) \ 2
                          GET AREA4NDX, M, NDXCLIENT
                          IF NDXCLIENT.CODI = ValueCamp$(1) THEN
                             trobat = 44
                          ELSEIF VAL(ValueCamp$(1)) < VAL(NDXCLIENT.CODI) THEN
                             J = M - 1
                          ELSE
                             i = M + 1
                          END IF
                       WEND

                       IF i = J THEN
                          IF i = VAL(ValueCamp$(1)) THEN
                             trobat = 44
                          END IF
                       END IF

'***************************************************************************
                       trobat = 0
                       FOR R = 1 TO MAXCL
                           GET AREA4, R, CLIENT
                           IF ValueCamp$(1) = CLIENT.CODICLIENT THEN
                              GOSUB TRANSPASS
                              DisplayAllCamps
                              trobat = 44
                              EXIT FOR
                           END IF
                       NEXT

'    FAC(1) = FormatD$(FACTURA.TOTALBRUT, "##.###.###"): FAC(2) = FormatD$(FACTURA.BASEIMPONIBLE, "##.###.###")
'    FAC(3) = FormatD$(FACTURA.DTO, "###"): FAC(4) = FormatD$(FACTURA.TIPOIVA, "###")
'    FAC(5) = FormatD$(FACTURA.TOTALIVA, "##.###.###"): FAC(6) = FormatD$(FACTURA.TOTALNET, "##.###.###")

    IF CASO% = 3 THEN
       MAXLINS = 25
    ELSE
       IF CASO% = 5 THEN
          MAXLINS = 17        ' MODE COMPRIMIT
       ELSE
          MAXLINS = 29        ' MODE EXTES
       END IF
    END IF


    DO
        IF LI >= MAXLINS THEN
           IF CASO% = 5 THEN
              FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
              PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
              PRINT #AREATXT, "Suma i continua..."
              PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
              FOR SALT = 1 TO 7: PRINT #AREATXT, "": NEXT
              PAG = PAG + 1: LI = 1
              GOSUB CAPSAC
           ELSE
              FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
              PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
              PRINT #AREATXT, "Suma i continua..."
              PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
              PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
              PAG = PAG + 1: LI = 1
              GOSUB CAPSAC
           END IF
        END IF

        IF lin$(L, 6) = "*" THEN
           TIPU$ = "FACTURA": GOSUB IMPRI.LINIA
           IF UCASE$(MID$(lin$(L, 1), 1, 6)) = "C.A.:F" AND MID$(lin$(L, 8), 8, 2) = "-A" THEN
              FOR RAC = 1 TO MAXALBS
                  GET AREA2T, RAC, ALBARAN
                  IF ALBARAN.REFALBARAN = lin$(L, 8) THEN EXIT FOR
              NEXT
              GET AREA3T, RAC, LINALBA
              lalb = 1
              DO
                 IF LI >= MAXLINS THEN
                    IF CASO% = 5 THEN
                       FOR LC = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
                       PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
                       PRINT #AREATXT, "Suma i continua..."
                       PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
                       FOR SALT = 1 TO 7: PRINT #AREATXT, "": NEXT
                       PAG = PAG + 1: LI = 1
                       GOSUB CAPSAC
                    ELSE
                       FOR LC = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
                       PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
                       PRINT #AREATXT, "Suma i continua..."
                       PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
                       PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
                       PAG = PAG + 1: LI = 1
                       GOSUB CAPSAC
                    END IF
                 END IF
                 TIPU$ = "ALBARAN": GOSUB IMPRI.LINIA
                 lalb = lalb + 1: LI = LI + 1
              LOOP UNTIL LINALBA.LINIA(lalb).MARCAR = "-"

              IF lin$(L + 1, 6) <> "-" THEN
                 PRINT #AREATXT, STRING$(78, "-")
                 LI = LI + 1
              END IF
           END IF
           L = L + 1: LI = LI + 1
        END IF
    LOOP UNTIL lin$(L, 6) = "-"
    FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT

    SELECT CASE CASO%
           CASE 1
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, USING " SUBTOTAL: ##,###,###.##"; FACTURA.TOTALBRUT
                PRINT #AREATXT, USING " DTO%....:            ##"; FACTURA.DTO;
                PRINT #AREATXT, USING SPACE$(24) + "BASE IMPONIBLE: ##,###,###.##"; FACTURA.BASEIMPONIBLE
                PRINT #AREATXT, USING SPACE$(48) + "     IVA ## %.: ##,###,###.##"; FACTURA.TIPOIVA; FACTURA.TOTALIVA
                PRINT #AREATXT, ""
                PRINT #AREATXT, USING SPACE$(48) + "      TOTAL...: ##,###,###.##"; FACTURA.TOTALNET
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO; "  Observacions: " + RTRIM$(ARCHIMP.COMPRIMIDO);
                PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
                PRINT #AREATXT, "                                                     " + RTRIM$(ARCHIMP.COMPRIMIDO); RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, ""
           CASE 2
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, USING SPACE$(48) + "      TOTAL...: ##,###,###.##"; FACTURA.TOTALNET
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO; "  Observacions: " + RTRIM$(ARCHIMP.COMPRIMIDO);
                PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
                PRINT #AREATXT, "                                                     " + RTRIM$(ARCHIMP.COMPRIMIDO); RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, ""
           CASE 3
                PRINT #AREATXT, "" + RTRIM$(ARCHIMP.COMPRIMIDO)
                PRINT #AREATXT, ""
                PRINT #AREATXT, ""
                PRINT #AREATXT, ""
                PRINT #AREATXT, ""
                PRINT #AREATXT, ""
                PRINT #AREATXT, USING "##,###,###.##" + SPACE$(40) + "  ##,###,###.##     ##  ##,###,###.##     ## " + SPACE$(25) + "##,###,###.##"; FACTURA.TOTALBRUT; FACTURA.BASEIMPONIBLE; FACTURA.TIPOIVA; FACTURA.TOTALIVA; FACTURA.DTO;  _
FACTURA.TOTALNET
                PRINT #AREATXT, "" + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
                FOR i = 1 TO 8: PRINT #AREATXT, "": NEXT
                PRINT #AREATXT, "" + RTRIM$(ARCHIMP.COMPRIMIDO)
                PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1))
                PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
                PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO
           CASE 4
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, USING " SUBTOTAL: ##,###,###.##"; FACTURA.TOTALBRUT
                PRINT #AREATXT, USING " DTO%....:            ##"; FACTURA.DTO;
                PRINT #AREATXT, USING SPACE$(21) + "BASE IMPONIBLE: ##,###,###"; FACTURA.BASEIMPONIBLE
                PRINT #AREATXT, USING SPACE$(48) + "   IVA %.:         ##"; FACTURA.TIPOIVA
                PRINT #AREATXT, ""
                PRINT #AREATXT, USING SPACE$(48) + " TOTAL...: ##,###,###"; FACTURA.TOTALNET
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO
                PRINT #AREATXT, STRING$(78, "Ä")
           CASE 5
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, "    SUBTOTAL:   BASE IMPONIBLE:   DTO%:  IVA%:      TOTAL IVA:     TOTAL NET:"
                PRINT #AREATXT, USING "##,###,###.##     ##,###,###.##      ##     ##   ##,###,###.##  ##,###,###.##"; FACTURA.TOTALBRUT; FACTURA.BASEIMPONIBLE; FACTURA.DTO; FACTURA.TIPOIVA; FACTURA.TOTALIVA; FACTURA.TOTALNET
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO; "  Observacions: " + RTRIM$(ARCHIMP.COMPRIMIDO);
                PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
                PRINT #AREATXT, "                                                    " + RTRIM$(ARCHIMP.COMPRIMIDO); RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
           CASE ELSE
    END SELECT
    PRINT #AREATXT, ""
    PRINT #AREATXT, SPACE$((40 \ 2) - (LEN(CADFE$) \ 2)) + RTRIM$(ARCHIMP.ENSANCHADO) + CADFE$ + RTRIM$(ARCHIMP.NOENSANCHADO)
    IF CASO% = 5 THEN FOR L = 1 TO 18: PRINT #AREATXT, "": NEXT

    CLOSE #AREATXT
    COLOR 31, 9: LOCATE 11, 31: PRINT "  IMPRIMINT  "
    COLOR 31, 9: LOCATE 13, 31: PRINT "   FITXERS   "

    ImprimeixFitxerTXT DIRECCT$ + "FACTURA.TXT", DEVI$, 100
    PutBackground 1, 1, FACTU$
    EXIT DEF

CAPSAC:
    PRINT #AREATXT, ""
    PRINT #AREATXT, ""
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + RTRIM$(CAP.LINIES(0)) + RTRIM$(ARCHIMP.NOENSANCHADO)
    FOR i = 1 TO 4: PRINT #AREATXT, CAP.LINIES(i): NEXT
    SELECT CASE MODO
           CASE 2
                PRINT #AREATXT, ""
                PRINT #AREATXT, ""
                PRINT #AREATXT, "                                          " + RTRIM$(ARCHIMP.ENSANCHADO) + "Factura: " + FACTURA.REFFACTURA + RTRIM$(ARCHIMP.NOENSANCHADO)
                PRINT #AREATXT, "              Data:         Codi Client:      " + FACTURA.PERSONA.NOM + RTRIM$(ARCHIMP.NEGRITA)
                PRINT #AREATXT, SPACE$(9) + SPACE$(5) + FACTURA.DADA + SPACE$(6) + FACTURA.CODCLIENT + RTRIM$(ARCHIMP.NONEGRITA);
                PRINT #AREATXT, SPACE$(8) + FACTURA.PERSONA.COGNOMS
                PRINT #AREATXT, SPACE$(42) + "    D.N.I. " + FACTURA.PERSONA.DNI
                PRINT #AREATXT, "P…gina N§:"; SPACE$(34) + "  " + FACTURA.PERSONA.DIRECCIO + RTRIM$(ARCHIMP.NEGRITA)
                PRINT #AREATXT, USING "\  \       \ \"; LTRIM$(STR$(PAG)); RTRIM$(ARCHIMP.NONEGRITA);
                PRINT #AREATXT, SPACE$(34) + LTRIM$(RTRIM$(FACTURA.PERSONA.POBLACIO)) + " " + FACTURA.PERSONA.CPOSTAL
                PRINT #AREATXT, SPACE$(45) + " Tels." + FACTURA.PERSONA.TELEFON1
                PRINT #AREATXT, SPACE$(45) + "      " + FACTURA.PERSONA.TELEFON2

                PRINT #AREATXT, ""
                PRINT #AREATXT, ""
                PRINT #AREATXT, CONFIG$(OPC)
                PRINT #AREATXT, ""
           CASE 4
                PRINT #AREATXT, ""
                PRINT #AREATXT, "                                          " + RTRIM$(ARCHIMP.ENSANCHADO) + "Factura: " + FACTURA.REFFACTURA + RTRIM$(ARCHIMP.NOENSANCHADO)
                PRINT #AREATXT, "                 Data:      Codi Client:" + RTRIM$(ARCHIMP.NEGRITA)
                PRINT #AREATXT, " " + SPACE$(9) + SPACE$(7) + FACTURA.DADA + SPACE$(3) + FACTURA.CODCLIENT + RTRIM$(ARCHIMP.NONEGRITA);
                PRINT #AREATXT, SPACE$(8) + " " + FACTURA.PERSONA.NOM
                PRINT #AREATXT, "           " + SPACE$(34) + "  " + FACTURA.PERSONA.COGNOMS
                PRINT #AREATXT, " P…gina N§:"; SPACE$(34) + "  D.N.I. " + FACTURA.PERSONA.DNI + RTRIM$(ARCHIMP.NEGRITA)
                PRINT #AREATXT, USING " \  \       \ \"; LTRIM$(STR$(PAG)); RTRIM$(ARCHIMP.NONEGRITA);
                PRINT #AREATXT, SPACE$(32) + "  " + FACTURA.PERSONA.DIRECCIO
                PRINT #AREATXT, SPACE$(47) + LTRIM$(RTRIM$(FACTURA.PERSONA.POBLACIO)) + " " + FACTURA.PERSONA.CPOSTAL
                PRINT #AREATXT, SPACE$(47) + "Tels." + FACTURA.PERSONA.TELEFON1
                PRINT #AREATXT, SPACE$(47) + "     " + FACTURA.PERSONA.TELEFON2
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, CONFIG$(0)
                PRINT #AREATXT, STRING$(78, "Ä")
           CASE ELSE
                PRINT #AREATXT, "                                          " + RTRIM$(ARCHIMP.ENSANCHADO) + "Factura: " + FACTURA.REFFACTURA + RTRIM$(ARCHIMP.NOENSANCHADO)
                PRINT #AREATXT, "                 Data:      Codi Client:";
                PRINT #AREATXT, "     Ú" + SPACE$(31) + "¿" + RTRIM$(ARCHIMP.NEGRITA)
                PRINT #AREATXT, "          " + SPACE$(7) + FACTURA.DADA + SPACE$(3) + FACTURA.CODCLIENT + RTRIM$(ARCHIMP.NONEGRITA);
                PRINT #AREATXT, SPACE$(8) + " " + FACTURA.PERSONA.NOM
                PRINT #AREATXT, "           " + SPACE$(34) + "  " + FACTURA.PERSONA.COGNOMS
                PRINT #AREATXT, " P…gina N§:"; SPACE$(34) + "  D.N.I. " + FACTURA.PERSONA.DNI + RTRIM$(ARCHIMP.NEGRITA)
                PRINT #AREATXT, USING " \  \       \ \"; LTRIM$(STR$(PAG)); RTRIM$(ARCHIMP.NONEGRITA);
                PRINT #AREATXT, SPACE$(32) + "  " + FACTURA.PERSONA.DIRECCIO
                PRINT #AREATXT, SPACE$(47) + LTRIM$(RTRIM$(FACTURA.PERSONA.POBLACIO)) + " " + FACTURA.PERSONA.CPOSTAL
                PRINT #AREATXT, SPACE$(47) + "Tels." + FACTURA.PERSONA.TELEFON1
                PRINT #AREATXT, SPACE$(47) + "     " + FACTURA.PERSONA.TELEFON2
                PRINT #AREATXT, SPACE$(45) + "À" + SPACE$(31) + "Ù"

                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, CONFIG$(OPC)
                PRINT #AREATXT, STRING$(78, "Ä")
    END SELECT
    RETURN

IMPRI.LINIA:
    SELECT CASE TIPU$
        CASE IS = "ALBARAN"
             COD$ = LINALBA.LINIA(lalb).CODART
             CON$ = LINALBA.LINIA(lalb).CONCEPTE
             QUA$ = STR$(LINALBA.LINIA(lalb).QUANTI)
             PRE$ = STR$(LINALBA.LINIA(lalb).PREU)
             DTO$ = STR$(LINALBA.LINIA(lalb).DTO)
             IMP$ = STR$(LINALBA.LINIA(lalb).IMPORT)
        CASE IS = "FACTURA"
             COD$ = lin$(L, 1)
             CON$ = lin$(L, 2)
             QUA$ = lin$(L, 4)
             PRE$ = lin$(L, 3)
             DTO$ = lin$(L, 7)
             IMP$ = lin$(L, 5)
        CASE ELSE
    END SELECT
    SELECT CASE MODO
           CASE 0
                IF UCASE$(MID$(lin$(L, 1), 1, 6)) = "C.A.:F" AND MID$(lin$(L, 8), 8, 2) = "-A" AND TIPU$ = "FACTURA" THEN
                   PRINT #AREATXT, USING " \                \"; COD$;
                   PRINT #AREATXT, CON$
                ELSE
                   PRINT #AREATXT, USING " \                \"; COD$;
                   PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); CON$; RTRIM$(ARCHIMP.NOCOMPRIMIDO);
                   PRINT #AREATXT, USING "##,###.#"; VAL(QUA$);
                   PRINT #AREATXT, USING "###,###.##"; VAL(PRE$);
                   PRINT #AREATXT, USING "  ##"; VAL(DTO$);
                   PRINT #AREATXT, USING "##,###,###.##"; VAL(IMP$)
                END IF
           CASE 1
                PRINT #AREATXT, USING "          ##,###.# "; VAL(lin$(L, 4));
                PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); lin$(L, 2); RTRIM$(ARCHIMP.NOCOMPRIMIDO);
                PRINT #AREATXT, USING " ##,###,###.##"; VAL(lin$(L, 5))
           CASE 2
                PRINT #AREATXT, USING "\                \"; lin$(L, 1);
                PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); lin$(L, 2); RTRIM$(ARCHIMP.NOCOMPRIMIDO);
                PRINT #AREATXT, USING "##,###.#"; VAL(lin$(L, 4));
                PRINT #AREATXT, USING " #,##,###"; VAL(lin$(L, 3));
                PRINT #AREATXT, USING "  ##"; VAL(lin$(L, 7));
                PRINT #AREATXT, USING "    ##,###,###.##"; VAL(lin$(L, 5))
           CASE 3
                PRINT #AREATXT, USING " \             \"; lin$(L, 1);
                PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); lin$(L, 2); RTRIM$(ARCHIMP.NOCOMPRIMIDO);
                PRINT #AREATXT, USING "#,###.#"; VAL(lin$(L, 4));
                PRINT #AREATXT, USING " ##,###"; VAL(lin$(L, 3));
                PRINT #AREATXT, USING " ##"; VAL(lin$(L, 7));
                PRINT #AREATXT, USING " #####"; VAL(lin$(L, 5))
           CASE 4
                IF UCASE$(MID$(lin$(L, 1), 1, 6)) = "C.A.:F" AND MID$(lin$(L, 8), 8, 2) = "-A" AND TIPU$ = "FACTURA" THEN
                   CA$ = ""
                   PRINT #AREATXT, USING " \                \"; CA$;
                   PRINT #AREATXT, lin$(L, 2)
                ELSE
                   Q$ = LTRIM$(FormatC$(VAL(QUA$), "###.###")): QUANT$ = SPACE$(7 - LEN(Q$)) + Q$
                   P$ = FormatC$(VAL(PRE$), "#.###.###"): PREU$ = SPACE$(9 - LEN(P$)) + P$
                   i$ = FormatC$(VAL(IMP$), "##.###.###"): IMPORT$ = SPACE$(10 - LEN(i$)) + i$
                   PRINT #AREATXT, USING " \                \"; COD$;
                   PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); CON$; RTRIM$(ARCHIMP.NOCOMPRIMIDO);
                   PRINT #AREATXT, USING " \     \"; QUANT$;
                   PRINT #AREATXT, USING " \       \"; PREU$;
                   PRINT #AREATXT, USING "  ##"; VAL(DTO$);
                   PRINT #AREATXT, USING "  \        \"; IMPORT$
                END IF
           CASE ELSE
    END SELECT
    RETURN

