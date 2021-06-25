' ********************************************************************
'
' Fitxer....................: RES_002.BAS
' Titol.....................: Modul per el enlla‡ar una factura amb un
'                             resguard.
'
' ********************************************************************
'
' Data inici................: 27/06/1996 21:27:00
' Data de la darrera revisi¢: 23/02/1998 14:08:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/98 (C)
'               
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB IMPRIMIRRESGUARD (DEVI$, AREA4!, FACTURAT!)
DECLARE SUB CARREGARALBARAN (R!, MAXLIN!)
DECLARE SUB Resguards.Factura (DIRECCP$, DIRECC$, DIRECI$, DP$, DEVI$, IMPRESORA!)
DECLARE SUB MASCREGS ()
DECLARE SUB LLISTAR (AREA4!, AREA3!, MAXLINS!)
DECLARE FUNCTION FindClient% (CAMP$, INDEX() AS ANY, MAXCL!)
DECLARE SUB MASCCLIS ()
DECLARE SUB SortClients (INDEX() AS ANY, MAXCL!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'c:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'c:\FACT3\FONTS\RESGUARD.BI'
'$INCLUDE: 'c:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'c:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'c:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'c:\FACT3\FONTS\CONSTS.BI'

COMMON SHARED /INDEX/ INDEX() AS INDEXCLI
COMMON SHARED DIRECC$, DIRCP$, DEVI$, DIRECCT$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT, rndx, MIDPOINT

'$DYNAMIC
DIM SHARED LIN$(100, 7)           ' LINIES DE LA FACTURA TEMPORAL
DIM SHARED OBS$(1)                ' LINIES D'OBSERVACIO
DIM SHARED CEMPRE AS CONTROLEMP       '     "      DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAP€ALERAS DE DOCUMENTS
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
'DIM SHARED ALBARAN AS ALBAR
DIM SHARED FACTURA AS FACTU
DIM SHARED LINFACTU AS LINIAFACT
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED CLIENTS AS CLIENT
DIM SHARED RESGUA AS RESG
DIM SHARED CFG AS CONFIG
DIM SHARED EMPRES AS EMPRESA
DIM SHARED CTRL AS CONTROL
DIM SHARED COLORS AS COLO
DIM SHARED COL(2, 1)
DIM SHARED USR AS USUARIS
DIM PASO AS TRANS
DIM ANYS AS ANNO

'********************************************************************
'  Comprovaci¢ de la cridada del programa principal
'********************************************************************
	  TMP$ = ENVIRON$("TEMPORAL")
	  DEF SEG = VARSEG(CFG)
	  BLOAD TMP$ + "PASS.TMP", VARPTR(CFG)
	  DEF SEG

	  DEF SEG = VARSEG(USR)
	  BLOAD TMP$ + "PASU.TMP", VARPTR(USR)
	  DEF SEG

	  DEF SEG = VARSEG(EMPRES)
	  BLOAD TMP$ + "PASE.TMP", VARPTR(EMPRES)
	  DEF SEG

	  DEF SEG = VARSEG(PASO)
	  BLOAD TMP$ + "PROT.TMP", VARPTR(PASO)
	  DEF SEG
	  CA$ = PASO.CLAU
	  CAD$ = PASO.APLICACIO
	  CA2$ = ""
	  FOR L% = 1 TO LEN(CAD$)
	      CA2$ = CA2$ + ENCRIPT$(MID$(CA$, L%, 1), L%)
	  NEXT

	  IF LTRIM$(RTRIM$(CA2$)) <> "RES_002.EXE," THEN
	     tecla = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "Pitji una tecla...", 0)
	     SYSTEM
	  END IF

	  KILL TMP$ + "PASS.TMP"
	  KILL TMP$ + "PASU.TMP"
	  KILL TMP$ + "PASE.TMP"
	  KILL TMP$ + "PROT.TMP"



      UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
      DBF$ = LTRIM$(RTRIM$(CFG.DDADE))             ' Assignar direcctoris
      SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
      MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
      HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
      SYS$ = LTRIM$(RTRIM$(CFG.DSIST))

      DIRECCF$ = DBF$     ' Subdirecctori de les base de dades
      DIRECCP$ = SCR$ + "\"      ' Subdirecctori de les pantalles
      DIRECCR$ = MSK$ + "\"     ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = HLP$ + "\"    ' Subdirecctori de ajuda
      DIRECCI$ = UNIDAD$ + "\IMPRESOR\"  ' Subdirecctori de les impresores
      DIRECCT$ = DBF$ + "TEXTOS\"
      DIRECCH$ = DBF$ + "HISTORIC\"
      DP$ = DBF$

      MAXLINS = USR.MAXLINS
      MAXFAC = USR.LINALBA
      IVA = EMPRES.IVA
      DTO = EMPRES.DTO
      R = EMPRES.ANY
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA

      SetDirRecursos (DIRECCR$)
      IF TESTSCREEN = &HB000 THEN
	 COL(0, 0) = 7: COL(0, 1) = 0
	 COL(1, 0) = 0: COL(1, 1) = 7
	 COL(2, 0) = 14: COL(2, 1) = 9
      ELSE
	 AREAC = FREEFILE: OPEN DIRECCF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
	 GET AREAC, 1, COLORS
	 COL(0, 0) = COLORS.COL(0, 0): COL(0, 1) = COLORS.COL(0, 1)
	 COL(1, 0) = COLORS.COL(1, 0): COL(1, 1) = COLORS.COL(1, 1)
	 COL(2, 0) = COLORS.COL(2, 0): COL(2, 1) = COLORS.COL(2, 1)
	 CLOSE AREAC
      END IF

      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, R, ANYS

      DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"
      CLOSE AREAA
      
      CALL Resguards.Factura(DIRECCP$, DIRECCF$, DIRECCI$, DP$, DEV$, IMPRESORA)
      SYSTEM

ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

REM $STATIC
SUB CARREGARALBARAN (R, MAXLIN)
    SHARED LIN$()

    GET AREA3, R, LINFACTU
    FOR J = 1 TO MAXLIN
	LIN$(J, 1) = LINFACTU.LINIA(J).CODART
	LIN$(J, 2) = LINFACTU.LINIA(J).CONCEPTE
	LIN$(J, 3) = STR$(LINFACTU.LINIA(J).PREU)
	LIN$(J, 4) = STR$(LINFACTU.LINIA(J).QUANTI)
	LIN$(J, 7) = STR$(LINFACTU.LINIA(J).DTO)
	LIN$(J, 5) = STR$(LINFACTU.LINIA(J).IMPORT)
	LIN$(J, 6) = LINFACTU.LINIA(J).MARCAR
    NEXT
    GET AREA2, R, FACTURA
END SUB

FUNCTION FindClient% (CAMP$, INDEX() AS INDEXCLI, MAXCL) STATIC
	 SHARED rndx
	 TOPRECORD = MAXCL - 1
	 BottomRecord = 1

	 DO UNTIL (TOPRECORD < BottomRecord)
	    
	    MIDPOINT = (TOPRECORD + BottomRecord) \ 2

	    TEST$ = RTRIM$(ForaEspai$(INDEX(MIDPOINT).CODI))

	    IF TEST$ = CAMP$ THEN
	       EXIT DO
	    ELSEIF CAMP$ > TEST$ THEN
	       BottomRecord = MIDPOINT + 1
	    ELSE
	       TOPRECORD = MIDPOINT - 1
	    END IF
	 LOOP

	 IF TEST$ = CAMP$ THEN
	    GET AREA3, INDEX(MIDPOINT).REGISTRE, CLIENTS
	    FindClient% = TRUE
	    rndx = MIDPOINT
	 ELSE
	    FindClient% = FALSE
	 END IF
END FUNCTION

SUB IMPRIMIRRESGUARD (DEVI$, AREA4, FACTURAT)
   SHARED DIRECCT$

   AREATXT = FREEFILE
   OPEN DIRECCT$ + "RES_ALBA.TXT" FOR OUTPUT SHARED AS AREATXT

   PAG = 1
   GET AREA4, 1, CAP
   GOSUB CAPSA
   MAXLINS = 13
   LI = 1: L = 1

    DO
	IF LI = 13 THEN
	    FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
	    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
	    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
	    PAG = PAG + 1: LI = 1
	    GOSUB CAPSA
	END IF

	IF LIN$(L, 6) = "*" THEN
	   PRINT #AREATXT, USING " \             \"; LIN$(L, 1);
	   PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); LIN$(L, 2); RTRIM$(ARCHIMP.NOCOMPRIMIDO);
	   PRINT #AREATXT, USING "#,###.#"; VAL(LIN$(L, 4));
	   PRINT #AREATXT, USING " ##,###"; VAL(LIN$(L, 3));
	   PRINT #AREATXT, USING " ##"; VAL(LIN$(L, 7));
	   PRINT #AREATXT, USING " #####"; VAL(LIN$(L, 5))
	END IF
	L = L + 1: LI = LI + 1
    LOOP UNTIL LIN$(L, 6) = "-"

    FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT

    PRINT #AREATXT, STRING$(78, "Ä")
    PRINT #AREATXT, USING " SUBTOTAL: ##,###,###.##"; FACTURA.TOTALBRUT
    PRINT #AREATXT, USING " DTO%....:            ##"; FACTURA.DTO;
    PRINT #AREATXT, USING SPACE$(26) + "BASE IMPONIBLE: ##,###,###.##"; FACTURA.BASEIMPONIBLE
    PRINT #AREATXT, USING SPACE$(50) + "     IVA ## %.: ##,###,###.##"; FACTURA.TIPOIVA; FACTURA.TOTALIVA
    PRINT #AREATXT, ""
    PRINT #AREATXT, USING SPACE$(50) + "      TOTAL...: ##,###,###.##"; FACTURA.TOTALNET
    PRINT #AREATXT, STRING$(78, "Ä")
    PRINT #AREATXT, " Observacions Client: " + RESGUA.OBSERVACL(1)
    PRINT #AREATXT, "                      " + RESGUA.OBSERVACL(2)
    PRINT #AREATXT, "                      " + RESGUA.OBSERVACL(3)
    PRINT #AREATXT, STRING$(78, "Ä")
    PRINT #AREATXT, " Observacions Servei: " + RESGUA.OBSERVASE(1)
    PRINT #AREATXT, "                      " + RESGUA.OBSERVASE(2)
    PRINT #AREATXT, "                      " + RESGUA.OBSERVASE(3)
    PRINT #AREATXT, STRING$(78, "Ä")

    
    PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO      '; " Observacions:" + RTRIM$(ARCHIMP.COMPRIMIDO);
    PRINT #AREATXT, STRING$(78, "Ä")
    PRINT #AREATXT, ""

    CLOSE #AREATXT


    '                                                    OJO !!!!
    '                                                    |
    '                                                    V
    ImprimeixFitxerTXT DIRECCT$ + "RES_ALBA.TXT", DEVI$, 80
    EXIT SUB
 

CAPSA:
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    FOR I = 1 TO 4: PRINT #AREATXT, CAP.LINIES(I): NEXT
    PRINT #AREATXT, ""

    PRINT #AREATXT, " Factura N§:     Data:      Codi Client:";
    PRINT #AREATXT, "     Ú" + SPACE$(31) + "¿" + RTRIM$(ARCHIMP.NEGRITA)
    PRINT #AREATXT, " " + FACTURA.REFFACTURA + SPACE$(7) + FACTURA.DADA + SPACE$(3) + FACTURA.CODCLIENT + RTRIM$(ARCHIMP.NONEGRITA);
    PRINT #AREATXT, SPACE$(8) + " " + FACTURA.PERSONA.NOM
    PRINT #AREATXT, "           " + SPACE$(34) + "  " + FACTURA.PERSONA.COGNOMS
    PRINT #AREATXT, " P…gina N§:"; SPACE$(34) + "  D.N.I. " + FACTURA.PERSONA.DNI + RTRIM$(ARCHIMP.NEGRITA)
    PRINT #AREATXT, USING " \  \       \ \"; LTRIM$(STR$(PAG)); RTRIM$(ARCHIMP.NONEGRITA);
    PRINT #AREATXT, SPACE$(32) + "  " + FACTURA.PERSONA.DIRECCIO
    PRINT #AREATXT, SPACE$(47) + LTRIM$(RTRIM$(FACTURA.PERSONA.POBLACIO)) + " " + FACTURA.PERSONA.CPOSTAL
    PRINT #AREATXT, SPACE$(47) + "Tels." + FACTURA.PERSONA.TELEFON1
    PRINT #AREATXT, SPACE$(47) + "     " + FACTURA.PERSONA.TELEFON2
    PRINT #AREATXT, SPACE$(45) + "À" + SPACE$(31) + "Ù"
    PRINT #AREATXT, " " + STRING$(78, "Ä")
    PRINT #AREATXT, " N§ Ordre:     Marca:                Model:" + RTRIM$(ARCHIMP.NEGRITA)
    PRINT #AREATXT, USING " \        \    \                   \ \                   \ \ \"; RESGUA.ORDRE; RESGUA.MARCA; RESGUA.MODELO; RTRIM$(ARCHIMP.NONEGRITA)
    PRINT #AREATXT, ""
    PRINT #AREATXT, " Aparell:              N§ SŠrie:            Data Entrada:     Data sortida:" + RTRIM$(ARCHIMP.NEGRITA)
    PRINT #AREATXT, USING " \                  \  \                  \ \      \          \      \\\"; RESGUA.APARELL; RESGUA.NSERIE; RESGUA.FETXAIN; RESGUA.FETXAOUT; RTRIM$(ARCHIMP.NONEGRITA)
    PRINT #AREATXT, " " + STRING$(78, "Ä")
    PRINT #AREATXT, " CODI ARTICLE   CONCEPTE                 QUANT.  PREU  % IMPORT"
    PRINT #AREATXT, " " + STRING$(78, "Ä")
    RETURN
    
CAPSA.RESGUARD:
    PRINT #AREATXT, "Observacions Client: " + RESGUA.OBSERVACL(1)
    PRINT #AREATXT, "                     " + RESGUA.OBSERVACL(2)
    PRINT #AREATXT, "                     " + RESGUA.OBSERVACL(3)
    PRINT #AREATXT, STRING$(78, "Ä")
    PRINT #AREATXT, "Observacions Servei: " + RESGUA.OBSERVASE(1)
    PRINT #AREATXT, "                     " + RESGUA.OBSERVASE(2)
    PRINT #AREATXT, "                     " + RESGUA.OBSERVASE(3)
    PRINT #AREATXT, STRING$(78, "Ä")
    RETURN

END SUB

SUB MASCREGS
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 4, 1, 20, 80, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
    LOCATE 5, 2: PRINT "ORDRE      NOM CLIENT" + SPACE$(10) + " COGNOMS" + SPACE$(21) + " DATA IN  DATA OUT": COLOR COL(0, 0), COL(0, 1)

    LOCATE 6, 2: PRINT "ÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ"
    FOR L = 7 TO 19: LOCATE L, 2: PRINT "          ³                    ³                            ³        ³": NEXT
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR              "
    LOCATE 24, 2: PRINT "<F7>=FACTURAR RESGUARD";
END SUB

SUB Resguards.Factura (DIRECCP$, DIRECC$, DIRECI$, DP$, DEVI$, IMPRESORA) STATIC
    SHARED rndx, DIRECCT$

    DIRECCT$ = DP$ + "TEXTOS\"
    ON ERROR GOTO ERRORS
    COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "  OBRINT..."
    GOSUB OBRIRFITXERS

    DIM MENU(1 TO 3) AS STRING

    IF MAXRE = 1 THEN
       X = 7: R = 1: L = 1
       BEEPtecla = Avis("AVIS:", "El fitxer de resguards est… buit.", "Pitji una tecla...", 0)
       GOSUB tanca
    END IF

    GOSUB TORNAR.LINKAR
    MASCREGS                          ' PINTA LA MASCARA DE LA LLISTA
    MAXRE = RC
    X = 7: R = 1: L = 1
    '
    ' ********* MOSTRA ELS REGISTRES QUE CABIGEN EN UNA PANTALLA
    '
    GOSUB LISTA
    MAXRE = RC - 1
    X = 7: R = 1                      ' INICIA CURSOR I POSICIONS A PANTALLA
    
    GET AREAR, R, RESGUA
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
    DO
      OP$ = INKEY$
      SELECT CASE OP$
	     CASE CHR$(0) + "P"              ' BAIXA UN REGISTRE
		  GOSUB BAIXA
	     CASE CHR$(0) + "H"              ' PUJA UN REGISTRE
		  GOSUB PUJA
	     CASE CHR$(27)
		  COLOR 15, 9: FINESTRA 10, 30, 14, 42, 1, CAIXA1
		  COLOR 31, 9: LOCATE 12, 31: PRINT " TANCANT..."
		  GOSUB tanca
	     CASE CHR$(0) + CHR$(65)
		  GOSUB ENLLASAR
		  GOSUB TORNAR.LINKAR: X = 7: R = 1: MASCREGS
		  GOSUB LISTA: X = 7: R = 1: GET AREAR, R, RESGUA
		  COLCURS = CCF: GOSUB SHOWCURSOR
	     CASE CHR$(0) + "Q"
		  COLCURS = 9: GOSUB SHOWCURSOR
		  R = R + 13
		  X = 19
		  GOSUB LISTA: X = 19
		  COLCURS = CCF: GOSUB SHOWCURSOR
	     CASE ELSE
		  LOCATE , , 0
      END SELECT
   LOOP
   RETURN

'************************************************************************
' Control del cursor per el llistat amb despla‡ament de barres
' funci¢ parescuda al DBEDIT de Clipper.
'************************************************************************

PUJA:
       IF X = 7 THEN
	  GET AREAR, R, RESGUA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = 1 THEN
	     X = 7: R = 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 7
	  SCROLLDOWN 18, 78, 6, 1, 1
	  GET AREAR, R, RESGUA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREAR, R, RESGUA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  GET AREAR, R, RESGUA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXA:
       IF X = 19 THEN
	  GET AREAR, R, RESGUA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	  IF R = MAXRE THEN
	     R = MAXRE: X = 19
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = 19
	  SCROLLUP 18, 78, 6, 1, 1
	  GET AREAR, R, RESGUA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREAR, R, RESGUA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXRE THEN
	     R = MAXRE
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  GET AREAR, R, RESGUA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

'************************************************************************
' Mostra el cursor a la posici¢ que est…
'************************************************************************

SHOWCURSOR:
	  LOCATE X, 2: PRINT RESGUA.ORDRE; "³"; RESGUA.PERSONA.NOM; "³"; MID$(RESGUA.PERSONA.COGNOMS, 1, 28); "³"; RESGUA.FETXAIN; "³"; RESGUA.FETXAOUT
	  RETURN

'************************************************************************
' LLISTA UN BLOC
'************************************************************************

LISTA:
    COLOR 15, 9: X = 7
    FOR X = 7 TO 19
       IF R = MAXRE THEN R = R - 1: EXIT FOR
       GET AREAR, R, RESGUA: COLOR COL(0, 0), COL(0, 1)
       LOCATE X, 2: PRINT RESGUA.ORDRE; "³"; RESGUA.PERSONA.NOM; "³"; MID$(RESGUA.PERSONA.COGNOMS, 1, 28); "³"; RESGUA.FETXAIN; "³"; RESGUA.FETXAOUT
       R = R + 1
    NEXT
    RETURN


ENLLASAR:
	  GetBackground 1, 1, 24, 79, cli$
	  RV = RESGUA.REGALB
	  GOSUB MASCARA
	  SetMaxCamps 1
	  SetInitCamp 0, 9, 61, ASCI, 0, "99/99/99", "": SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
	  SetInitCamp 1, 10, 32, ASCI, 0, "XXXXXXXXX", "": SetColorCamp 1, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
	  InsertValueCamp 0, FETXA$: InsertValueCamp 1, MID$(FETXA$, 7, 2) + "0000-RF"
	  DisplayAllCamps
	  FOR C = 0 TO 1
	      VALUE = ReadCamp(C)
	      SELECT CASE VALUE
		     CASE F1 TO F10
			  C = C - 1
		     CASE SALIR
			  PutBackground 1, 1, cli$
			  RETURN
		     CASE ELSE
	      END SELECT
	  NEXT
	  COLOR 27, COL(0, 1): CENTRAR 14, "¨ Estas segur d'enlla‡ar-los (S/N) ?"
	  DO
	    T$ = INKEY$
	    IF UCASE$(T$) = "S" THEN EXIT DO
	    IF UCASE$(T$) = "N" THEN
	       PutBackground 1, 1, cli$
	       RETURN
	    END IF
	  LOOP
	  COLOR 15: CENTRAR 14, "                                    "
	  TROBAT = 22
	  '
	  ' BUSCAR FACTURA
	  '
	  FOR RA = 1 TO MAXAL - 1
	      GET AREA2, RA, FACTURA
	      IF FACTURA.REFFACTURA = ValueCamp$(1) THEN
		 IF FACTURA.CODCLIENT <> RESGUA.CODCLIENT THEN
		    BEEP
		    COLOR 27, COL(0, 1): CENTRAR 14, "ERROR: AQUESTA FACTURA NO PERTANY A AQUEST CLIENT"
		    C$ = INPUT$(1)
		    PutBackground 1, 1, cli$
		    RETURN
		 END IF
		 GET AREAC, RV, RESGUA
		 RESGUA.FETXAOUT = ValueCamp$(0)
		 RESGUA.REGALB = RA
		 RESGUA.ALBARAN = FACTURA.REFFACTURA
		 PUT AREAC, RV, RESGUA
		 TROBAT = 99
		 EXIT FOR
	      END IF
	  NEXT

	  IF TROBAT <> 99 THEN
	     CENTRAR 14, "ERROR: FACTURA NO TROBADA, PITJA <ENTER>"
	     C$ = INPUT$(1)
	     PutBackground 1, 1, cli$
	     RETURN
	  END IF

	 
	 DO
	  MENU(1) = "IMPRIMIR FACTURA+RESGUARD"
	  MENU(2) = "SORTIR                   "
	  CALL MENU2(MENU(), CASO%, 13, 14, LEN(MENU(1)) + 2, 2, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
	  SELECT CASE CASO%
		 CASE 1
		      CALL CARREGARALBARAN(RA, 100)
		      CALL IMPRIMIRRESGUARD(DEVI$, AREA4, 0)
		 CASE 2
		      PutBackground 1, 1, cli$
		      RETURN
		 CASE 999
		      PutBackground 1, 1, cli$
		      RETURN
	  END SELECT
	 LOOP

FACTURA.ALBARAN:
	  BEEP
	  RETURN

'************************************************************************
' FITXA D'UN RESGUARD
'************************************************************************

MASCARA:
	  COLOR COL(0, 0), COL(0, 1)
	  FINESTRA 2, 10, 20, 76, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
	   LOCATE 3, 11: PRINT "RESGUARD No.: "; : COLOR COL(0, 0), COL(0, 1): PRINT RV: COLOR COL(2, 0), COL(2, 1)
	   LOCATE 4, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0), COL(2, 1)
	   LOCATE 5, 11: PRINT "N§ D'ORDRE.......:"; : COLOR COL(0, 0), COL(0, 1): PRINT RESGUA.ORDRE: COLOR COL(2, 0), COL(2, 1)
	   NOM$ = RTRIM$(LTRIM$(RESGUA.PERSONA.NOM)) + " " + LTRIM$(RTRIM$(RESGUA.PERSONA.COGNOMS))
	   LOCATE 6, 11: PRINT "NOM DEL CLIENT...:"; : COLOR COL(0, 0), COL(0, 1): PRINT NOM$: COLOR COL(2, 0), COL(2, 1)
	  LOCATE 7, 11: PRINT STRING$(65, "Ä");
	  LOCATE 9, 11: PRINT "DATA ENTRADA.......: "; : COLOR COL(0, 0), COL(0, 1): PRINT RESGUA.FETXAIN; : COLOR COL(2, 0), COL(2, 1): PRINT "       DATA SORTIDA:"
	  LOCATE 10, 11: PRINT "REFERŠNCIA FACTURA: "
	  LOCATE 12, 11: PRINT STRING$(65, "Ä");
	  RETURN


'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE CLIENTS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA4 = FREEFILE: OPEN DIRECC$ + "\PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CAP)
      AREA2 = FREEFILE: OPEN DIRECC$ + "FACTURES.CAB" FOR RANDOM SHARED AS AREA2 LEN = LEN(FACTURA)
      AREA3 = FREEFILE: OPEN DIRECC$ + "FACTURES.LIN" FOR RANDOM SHARED AS AREA3 LEN = LEN(LINFACTU)
      AREAC = FREEFILE: OPEN DIRECC$ + "RESGUARD.DAT" FOR RANDOM SHARED AS AREAC LEN = LEN(RESGUA)
      AREAR = FREEFILE: OPEN DIRECC$ + "RESGUARD.BCK" FOR RANDOM SHARED AS AREAR LEN = LEN(RESGUA)

'************************************************************************
' DEMANAR IMPRESORA ACTIVA
'************************************************************************

      AREA5 = FREEFILE: OPEN "..\SPOOL.CFG" FOR RANDOM SHARED AS AREA5 LEN = LEN(SPOOLIMP)
      GET AREA5, IMPRESORA, SPOOLIMP
      FITXER$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
      CLOSE AREA5

'************************************************************************
' OBRIR FITXER DE LA IMPRESORA
'************************************************************************

      AREA6 = FREEFILE: OPEN DIRECI$ + FITXER$ FOR RANDOM SHARED AS AREA6 LEN = LEN(ARCHIMP)
      GET AREA6, 1, ARCHIMP
      CLOSE AREA6

'************************************************************************
' ACTUALITZAR REGISTRES
'************************************************************************
      GET AREA, 1, CEMPRE
      GET AREAC, 1, RESGUA
      MAXFA = CEMPRE.MAXFACTURA
      MAXAL = CEMPRE.MAXFACTURA
      MAXCL = CEMPRE.MAXCLIENTS
      MAXR = RESGUA.MAXREG
      RETURN


TORNAR.LINKAR:
    RC = 1: FOR R = 1 TO MAXR
	GET AREAC, R, RESGUA
	IF RESGUA.REGALB = 0 AND LTRIM$(RESGUA.ALBARAN) = "" THEN
	   RESGUA.REGALB = R
	   PUT AREAR, RC, RESGUA
	   RC = RC + 1
	END IF
    NEXT
    IF RC = 1 THEN GOSUB tanca
    GET AREAR, 1, RESGUA
    RESGUA.CPY = "Fitxer dels RESGUARDS. (IMATGE) COPIA" + CHR$(26)
    RESGUA.MAXREG = RC
    PUT AREAR, 1, RESGUA
    MAXRE = RC
    RETURN

tanca:
      KILL DIRECC$ + "RESGUARD.BCK"
      FOR C = 0 TO 13: DeleteCamp C: NEXT
      ERASE CAMPS
      RESET
      EXIT SUB
END SUB

SUB SortClients (INDEX() AS INDEXCLI, MAXCL) STATIC
    
    OFFSET = MAXCL - 1 \ 2

    DO WHILE OFFSET > 0
       LIMIT = MAXCL - 1 - OFFSET
       DO
	 SWITCH = FALSE
	 FOR I = 1 TO LIMIT
	     IF INDEX(I).CODI > INDEX(I + OFFSET).CODI THEN
		SWAP INDEX(I), INDEX(I + OFFSET)
		SWITCH = I
	     END IF
	 NEXT

	 LIMIT = SWITCH
       LOOP WHILE SWITCH
       OFFSET = OFFSET \ 2
    LOOP

END SUB

