SUB IMPRIMIRALBARAN (MAX, DEVI$)
    SHARED DIRECCT$

    DIM CONFIG$(3)
    DIM MENU(5) AS STRING
    MASC$(4) = "##,###.#"                     ' QUANTITAT
    MASC$(5) = "##,###,###"                   ' IMPORT
    MASCTOTAL$ = "##,###,###.##"              ' TOTAL

    CONFIG$(0) = " CODI ARTICLE      CONCEPTE                  QUANT.     PREU  DTO%     IMPORT"
    CONFIG$(1) = "         QUANTITAT CONCEPTE                       IMPORT"
    CONFIG$(2) = " CODI ARTICLE     CONCEPTE                  QUANT.    PREU  DTO%          IMPORT"
    CONFIG$(3) = " CODI ARTICLE   CONCEPTE                 QUANT.  PREU  % IMPORT"

    MENU(1) = "  DOCUMENT TIPUS PER DEFECTE     "
    MENU(2) = "  DOCUMENT TIPUS 1               "
    MENU(3) = "  DOCUMENT AMB PAPER PRE-IMPRES  "
    MENU(4) = "  DOCUMENT COMPACTAT (RESGUARDS) "
    MENU(5) = "  DOCUMENT TIPUS COMPACTE        "


    GetBackground 1, 1, 24, 79, FACTU$
    CALL Menu2(MENU(), CASO%, 17, 23, LEN(MENU(1)) + 2, 5, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
    IF CASO% = 999 THEN
       PutBackground 1, 1, FACTU$
       ERASE MENU, CONFIG$
       EXIT SUB
    END IF
    MODO = CASO% - 1: OPC = CASO% - 1

    COLOR 15, 9: FINESTRA 10, 30, 14, 45, 1, CAIXA1
    COLOR 31, 9: LOCATE 11, 31: PRINT "ACTUALITZANT"
    COLOR 31, 9: LOCATE 13, 31: PRINT "  FITXERS   "

    AREATXT = FREEFILE
    OPEN DIRECCT$ + "FACTURA.TXT" FOR OUTPUT SHARED AS AREATXT


    PAG = 1
    GET AREA5, 1, CAP
    GOSUB CAPSA
    IF CASO% = 3 THEN
       MAXLINS = 25
    ELSE
    IF CASO% = 5 THEN
          MAXLINS = 17        ' MODE COMPRIMIT
       ELSE
          MAXLINS = 29        ' MODE EXTES
       END IF
    END IF
    LI = 1: L = 1

    DO
        IF LI = MAXLINS THEN
           IF CASO% = 5 THEN
              FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
              PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
              PRINT #AREATXT, "Suma i continua..."
              PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
              FOR SALT = 1 TO 9
                  PRINT #AREATXT, ""
              NEXT
              PAG = PAG + 1
              GOSUB CAPSA
           ELSE
              FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
              PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
              PRINT #AREATXT, "Suma i continua..."
              PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
              PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
              PAG = PAG + 1
              GOSUB CAPSA

           END IF
        END IF

        IF LIN$(L, 6) = "*" THEN
           SELECT CASE MODO
                  CASE 0
                       PRINT #AREATXT, USING " \                \"; LIN$(L, 1);
                       PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); LIN$(L, 2); RTRIM$(ARCHIMP.NOCOMPRIMIDO);
                       PRINT #AREATXT, USING "##,###.#"; VAL(LIN$(L, 4));
                       PRINT #AREATXT, USING "###,###.##"; VAL(LIN$(L, 3));
                       PRINT #AREATXT, USING "  ##"; VAL(LIN$(L, 7));
                       PRINT #AREATXT, USING "##,###,###.##"; VAL(LIN$(L, 5))
                  CASE 1
                       PRINT #AREATXT, USING "          ##,###.# "; VAL(LIN$(L, 4));
                       PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); LIN$(L, 2); RTRIM$(ARCHIMP.NOCOMPRIMIDO);
                       PRINT #AREATXT, USING " ##,###,###.##"; VAL(LIN$(L, 5))
                  CASE 2
                       PRINT #AREATXT, USING "\                \"; LIN$(L, 1);
                       PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); LIN$(L, 2); RTRIM$(ARCHIMP.NOCOMPRIMIDO);
                       PRINT #AREATXT, USING "##,###.#"; VAL(LIN$(L, 4));
                       PRINT #AREATXT, USING " #,##,###"; VAL(LIN$(L, 3));
                       PRINT #AREATXT, USING "  ##"; VAL(LIN$(L, 7));
                       PRINT #AREATXT, USING "    ##,###,###.##"; VAL(LIN$(L, 5))
                  CASE 3
                       PRINT #AREATXT, USING " \             \"; LIN$(L, 1);
                       PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); LIN$(L, 2); RTRIM$(ARCHIMP.NOCOMPRIMIDO);
                       PRINT #AREATXT, USING "#,###.#"; VAL(LIN$(L, 4));
                       PRINT #AREATXT, USING " ##,###"; VAL(LIN$(L, 3));
                       PRINT #AREATXT, USING " ##"; VAL(LIN$(L, 7));
                       PRINT #AREATXT, USING " #####"; VAL(LIN$(L, 5))
                  CASE 4
                       Q$ = LTRIM$(FormatC$(VAL(LIN$(L, 4)), "###.###")): QUANT$ = SPACE$(7 - LEN(Q$)) + Q$
                       P$ = FormatC$(VAL(LIN$(L, 3)), "#.###.###"): PREU$ = SPACE$(9 - LEN(P$)) + P$
                       I$ = FormatC$(VAL(LIN$(L, 5)), "##.###.###"): IMPORT$ = SPACE$(10 - LEN(I$)) + I$
                       PRINT #AREATXT, USING " \                \"; LIN$(L, 1);
                       PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); LIN$(L, 2); RTRIM$(ARCHIMP.NOCOMPRIMIDO);
                       PRINT #AREATXT, USING " \     \"; QUANT$;
                       PRINT #AREATXT, USING " \       \"; PREU$;
                       PRINT #AREATXT, USING "  ##"; VAL(LIN$(L, 7));
                       PRINT #AREATXT, USING "  \        \"; IMPORT$
                  CASE ELSE
           END SELECT

        END IF
        L = L + 1: LI = LI + 1
    LOOP UNTIL LIN$(L, 6) = "-"

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
                FOR I = 1 TO 8: PRINT #AREATXT, "": NEXT
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
                PRINT #AREATXT, "SUBTOTAL:     BASE IMPONIBLE:   DTO%:  IVA%:      TOTAL IVA:  TOTAL FACTURA:"
                PRINT #AREATXT, USING "###,###.##     ###,###.##        ##      ##   ##,###,###.##    #,###,###.##"; FACTURA.TOTALBRUT; FACTURA.BASEIMPONIBLE; FACTURA.DTO; FACTURA.TIPOIVA; FACTURA.TOTALIVA; FACTURA.TOTALNET
                PRINT #AREATXT, STRING$(78, "Ä")
                PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO; "  Observacions: " + RTRIM$(ARCHIMP.COMPRIMIDO);
                PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
                PRINT #AREATXT, "                                                     " + RTRIM$(ARCHIMP.COMPRIMIDO); RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
                FOR L = 1 TO 16: PRINT #AREATXT, "": NEXT

           CASE ELSE
    END SELECT

    CLOSE #AREATXT

    COLOR 31, 9: LOCATE 11, 31: PRINT "  IMPRIMINT  "
    COLOR 31, 9: LOCATE 13, 31: PRINT "   FITXERS   "

    ImprimeixFitxerTXT DIRECCT$ + "FACTURA.TXT", DEVI$, 100

    PutBackground 1, 1, FACTU$
    EXIT SUB

'
'
'
CAPSA:
    PRINT #AREATXT, ""
    PRINT #AREATXT, ""
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + RTRIM$(CAP.LINIES(0)) + RTRIM$(ARCHIMP.NOENSANCHADO)
    FOR I = 1 TO 4: PRINT #AREATXT, CAP.LINIES(I): NEXT
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
END SUB

