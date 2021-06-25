' ********************************************************************
'
' Fitxer....................: STOCK.BAS
' Titol.....................: Modul per el mateniment del stock
'
' ********************************************************************
'
' Data inici................: 29/05/1996 18:37:37 (Data del FACT2)
' Data de la darrera revisi¢: 10/06/1998 13:48:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/98 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE FUNCTION FindRecordFA% (CAMP$, INDEXFA() AS ANY, MAXFAM!)
DECLARE FUNCTION FindRecord% (CAMP$, INDEX() AS ANY, MAXST!)
DECLARE SUB SumarStock (AREA3!, MAXST!)
DECLARE SUB MASCLIST ()
DECLARE SUB SortIndex (INDEX() AS ANY, MAXST!)
DECLARE SUB LLISTAR (AREA4!, AREA3!, MAXLINS!)
DECLARE SUB MantenimentStock (DIRECCP$, DIRECC$, DIRECI$, DP$, DEV$, IMPRESORA!, MAXLINS!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STOCK.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'c:\FACT3\FONTS\PROVEID.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'


COMMON SHARED DIRECCM$, DIRCP$, DIRECCT$, DEVI$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5, AREAFN, AREAFA
COMMON SHARED GUARDAT, TROBAT

DIM SHARED OBS$(1)                    ' LINIES D'OBSERVACIO
DIM SHARED STOCK AS STK               ' ESTRUCTURA DEL FITXER DEL STOCK
DIM SHARED STOCK1 AS STK1
DIM SHARED FAMILIES AS FAMI
DIM SHARED CEMPRE AS CONTROLEMP       '     "      DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CFG AS CONFIG              '     "      DEL FITXER DE CONFIGURACI¢
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAP€ALERAS DE DOCUMENTS
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED PROVEIDORS AS PROVEID
DIM SHARED EMPRES AS EMPRESA
DIM SHARED CTRL AS CONTROL
DIM COLORS AS COLO
DIM SHARED COL(2, 1)
DIM SHARED USR AS USUARIS
DIM SHARED PASO AS TRANS
DIM SHARED ANYS AS ANNO

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

	  IF LTRIM$(RTRIM$(CA2$)) <> "STOCK.EXE" THEN
	     tecla = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "Pitji una tecla...", 0)
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
      MAXFAC = USR.LINFACT
      IVA = EMPRES.IVA
      DTO = EMPRES.DTO
      R = EMPRES.ANY
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA
			      
      SetFormatCC (34)
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

      CALL MantenimentStock(DIRECCP$, DIRECCF$, DIRECCI$, DBF$, DEV$, IMPRESORA, MAXLINS)
      SYSTEM


ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

FUNCTION FindRecord% (CAMP$, INDEX() AS INDEXTYPE, MAXST) STATIC
	 TOPRECORD = MAXST - 1
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
	    GET AREA3, INDEX(MIDPOINT).REGISTRE, STOCK
	    FindRecord% = TRUE
	 ELSE
	    FindRecord% = FALSE
	 END IF
END FUNCTION

FUNCTION FindRecordFA% (CAMP$, INDEXFA() AS INDEXFAMI, MAXFAM) STATIC
	 SHARED AREAFA
	 TOPRECORD = MAXFAM
	 BottomRecord = 1

	 DO UNTIL (TOPRECORD < BottomRecord)
	    MIDPOINT = (TOPRECORD + BottomRecord) \ 2
	    TEST$ = RTRIM$(ForaEspai$(INDEXFA(MIDPOINT).CODI))

	    IF TEST$ = CAMP$ THEN
	       EXIT DO
	    ELSEIF CAMP$ > TEST$ THEN
	       BottomRecord = MIDPOINT + 1
	    ELSE
	       TOPRECORD = MIDPOINT - 1
	    END IF
	 LOOP

	 IF TEST$ = CAMP$ THEN
	    GET AREAFA, INDEXFA(MIDPOINT).REGISTRE, FAMILIES
	    FindRecordFA% = TRUE
	 ELSE
	    FindRecordFA% = FALSE
	 END IF

END FUNCTION

SUB LLISTAR (AREA4, AREA3, MAXLINS)
    SHARED DIRECCT$, DIRECCM$, DEVI$
    DIM TEXTOSMENUP(6) AS STRING
    DIM CAB$(3)
	
    TEXTOSMENUP(1) = "LLISTAR TOT    "
    TEXTOSMENUP(2) = "PER FAMILIA    "
    TEXTOSMENUP(3) = "PER DATA       "
    TEXTOSMENUP(4) = "PER QUANTITAT  "

    LOCATE 22, 2: PRINT STRING$(70, " ");
    CALL MENU2(TEXTOSMENUP(), CASO%, 10, 10, LEN(TEXTOSMENUP(1)) + 2, 4, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
    SELECT CASE CASO%
	   CASE 1

	   CASE 2
		LOCATE , , 1, 13, 14: FINESTRA 10, 28, 13, 60, 1, CAIXA1
		SetMaxCamps 0: DeleteCamp 0
		SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
		SetInitCamp 0, 11, 39, ASCI, 0, STRING$(20, "X"), "Familia:"
		DisplayAllCamps
		IF ReadCamp(0) = SALIR THEN EXIT SUB
	   CASE 3
		LOCATE , , 1, 13, 14: FINESTRA 10, 28, 13, 50, 1, CAIXA1
		SetMaxCamps 0
		DeleteCamp 0
		SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
		SetInitCamp 0, 11, 39, DAT, 0, "99/99/99", "Data:"
		DisplayAllCamps
		IF ReadCamp(0) = SALIR THEN EXIT SUB
	   CASE 4
		LOCATE , , 1, 13, 14: FINESTRA 10, 28, 13, 50, 1, CAIXA1
		SetMaxCamps 0
		DeleteCamp 0
		SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
		SetInitCamp 0, 11, 39, NUM, 0, "999999999", "Quantitat:"
		DisplayAllCamps
		IF ReadCamp(0) = SALIR THEN EXIT SUB
	   CASE 999
		ERASE TEXTOSMENUP, CAB$
		EXIT SUB
    END SELECT
    ' ********* GENERAR FITXER DE TEXTE DEL LLISTAT **********
    AREAMSK = FREEFILE: OPEN DIRECCM$ + "PLANTILL\STOCK.MSK" FOR INPUT SHARED AS AREAMSK
    AREATXT = FREEFILE: OPEN DIRECCT$ + "STOCK.TXT" FOR OUTPUT SHARED AS AREATXT
    
    PAG = 1: L = 1
    ' DEFINIR MASCARA DE LES LINIES
    GET AREA4, 1, CAP             ' AGAFAR CAP€ALERA DEL FITXER DE CAP€ALERES
    LINE INPUT #AREAMSK, CAB$(1)  ' AGAFAR LA CAP€ALERA DELS CAMPS
    LINE INPUT #AREAMSK, CAB$(2)
    LINE INPUT #AREAMSK, CAB$(3)
    CLOSE #AREAMSK

    GOSUB CAPSA         ' IMPRIMIR CAP€ALERA

    ' LLISTAR EL FITXER DEL STOCK

    CONDI$ = LTRIM$(RTRIM$(ValueCamp$(0)))
    FOR R = 1 TO CEMPRE.MAXSTOCK - 1
	GET AREA3, R, STOCK
	SELECT CASE CASO% - 1      ' SELECCIONA EL TIPUS DE LLISTAT
	       ' ************ LLISTAR TOT **************
	       CASE 0
		    IF STOCK.MARCAT = " " THEN GOSUB PRINTLINIA
	       ' ************ LLISTAR PER FAMILIES ************
	       CASE 1
		    CONDF$ = LTRIM$(RTRIM$(STOCK.FAMILIA))
		    IF CONDI$ = CONDF$ THEN
		       IF STOCK.MARCAT = " " THEN GOSUB PRINTLINIA
		    END IF

	       ' ************ LLISTAR PER DADES ************
	       CASE 2
		    IF STOCK.FECHACOMP = ValueCamp$(0) THEN
		       IF STOCK.MARCAT = " " THEN GOSUB PRINTLINIA
		    END IF

	       ' ************ LLISTAR PER EXISTENCIES ************
	       CASE 3
		    IF STOCK.EXISTENCIA = VAL(ValueCamp$(0)) THEN
		       IF STOCK.MARCAT = " " THEN GOSUB PRINTLINIA
		    END IF
	       CASE ELSE
	END SELECT
    NEXT

    FOR LA = L TO MAXLINS: PRINT #AREATXT, "": NEXT
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(80, "Ä") + RTRIM$(ARCHIMP.COMPRIMIDO)
    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT

    CALL ImprimeixFitxerTXT(DIRECCT$ + "STOCK.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP€ALERA DELS LLISTATS
' **************************************************************************

CAPSA:
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    SELECT CASE CASO% - 1
	   CASE 0
		PRINT #AREATXT, "LLISTAT TOTAL DEL STOCK"
	   CASE 1
		PRINT #AREATXT, "LLISTAT DE LA FAMILIA " + ValueCamp$(0)
	   CASE 2
		PRINT #AREATXT, "LLISTAT DE ARTICLES COMPRATS A LA DATA " + ValueCamp$(0)
	   CASE 3
		PRINT #AREATXT, "LLISTAT DE ARTICLES AMB EXISTENCIES " + ValueCamp$(0)
	   CASE ELSE
    END SELECT
    PRINT #AREATXT, "P…gina:"; PAG
    PRINT #AREATXT, "Data..: "; FormatD$(Now#, "dd/mm/yyyy")
    PRINT #AREATXT, STRING$(78, "Ä");
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""
    PRINT #AREATXT, CAB$(1)
    PRINT #AREATXT, CAB$(2)
    RETURN

PRINTLINIA:
     IF L >= MAXLINS THEN
	L = 1: PAG = PAG + 1
	PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(80, "Ä")
	PRINT #AREATXT, ""
	PRINT #AREATXT, ""
	GOSUB CAPSA
     ELSE
	FOR C = 1 TO LEN(STOCK.CODI)
	   IF MID$(STOCK.CODI, C, 1) = CHR$(0) THEN MID$(STOCK.CODI, C, 1) = CHR$(32)
	NEXT
	PRINT #AREATXT, USING CAB$(3); STOCK.CODI; STOCK.DESCRIPCIO; STOCK.EXISTENCIA; STOCK.PREU; STOCK.DESCOMPTE; STOCK.MARGECOMER; STOCK.PVPACONSE; STOCK.FECHACOMP; STOCK.NOMPROVEEDOR
	L = L + 1
     END IF
    RETURN
END SUB

SUB MantenimentStock (DIRECCP$, DIRECC$, DIRECI$, DP$, DEV$, IMPRESORA, MAXLINS) STATIC
    SHARED DIRECCT$, DIRECCM$

    ON ERROR GOTO ERRORS
    '****** L'STOCK EST… A L'AREA NUMERO 3
    
    COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "  OBRINT..."
    GOSUB OBRIRFITXERS

    DIRECCT$ = DP$ + "TEXTOS\"
    DEVI$ = DEV$: DIRECCM$ = DP$

    MAXFAM = LOF(AREAFA) \ LEN(FAMILIES)

    IF MAXFAM = 0 THEN
       AVIS.SONOR (1)
       tecla = Avis("ERROR:", "No hi ha cap familia definida.", "Pitji una tecla...", 0)
       RESET: EXIT SUB
    END IF

    DIM INDEX(1 TO MAXST) AS INDEXTYPE
    DIM INDEXFA(1 TO MAXFAM) AS INDEXFAMI
    DIM NDXFILE AS INDEXTYPE
    DIM NDXFAMI AS INDEXFAMI
    DIM NDXPROV AS INDEXPROVEIDOR

    IF MAXST = 1 THEN
       X = 7: R = 1: L = 1
       BEEP: tecla = Avis("AVIS:", "El fitxer del megatzem est… buit.", "Pitji una tecla...", 0)
       GOSUB INSERTARARTICLE
       IF ANUL = 1 THEN GOSUB TANCA
       GOSUB GUARDARINDEX
    END IF
    
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "INDEXANT..."
    GOSUB INDEXAR

    CALL SortIndex(INDEX(), MAXST)        ' ORDENAR L'INDEX
    X = 7: R = 1: L = 1

    '
    ' ********* MOSTRA ELS REGISTRES QUE CABIGEN EN UNA PANTALLA
    '
    MASCLIST                          ' PINTA LA MASCARA DE LA LLISTA
    RESTA = 0: GOSUB LISTA

    X = 7: R = 1                      ' INICIA CURSOR I POSICIONS A PANTALLA
    GET AREA3, R, STOCK
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
    DO
      COLOR COL(0, 0), COL(0, 1): LOCATE 25, 70: PRINT CHR$(16); : COLOR COL(0, 0), COL(0, 0) XOR COL(0, 1): PRINT TIME$; : COLOR COL(0, 0), COL(0, 1): PRINT CHR$(17);
      OP$ = INKEY$
      COLOR COL(0, 0), COL(0, 1): EstatTeclesControl 25, 3
      SELECT CASE OP$
	     CASE CHR$(0) + "P"      ' BAIXA UN REGISTRE
		  GOSUB BAIXA
	     CASE CHR$(0) + "H"      ' PUJA UN REGISTRE
		  GOSUB PUJA
	     CASE CHR$(27)
		  COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
		  COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT " TANCANT..."
		  GOSUB GUARDARINDEX
		  GOSUB TANCA
	     CASE CHR$(13)
		  GOSUB EDITARARTICLE        ' EDITAR ARTICLE
	     CASE CHR$(0) + CHR$(60)
		  GOSUB INSERTARARTICLE      ' INSERTAR ARTICLE
	     CASE CHR$(0) + CHR$(61)
		  CALL SumarStock(AREA3, MAXST)
	     CASE CHR$(0) + CHR$(62)
		  GOSUB CONSULTARARTICLE     ' CONSULTAR ARTICLES
	     CASE CHR$(0) + CHR$(63)
		  GOSUB DELETERECORD         ' MARCAR REGISTRE
	     CASE CHR$(0) + CHR$(64)
		  GetBackground 1, 1, 24, 79, cli$
		  CALL LLISTAR(AREA4, AREA3, MAXLINS)   ' LLISTAR STOCK
		  PutBackground 1, 1, cli$
	     CASE CHR$(0) + "Q"
		  GOSUB BOTA.AVALL
	     CASE CHR$(0) + "I"
		  GOSUB BOTA.AMUNT
	     CASE ELSE
		  LOCATE , , 0
		  COLOR COL(0, 0), COL(0, 1): LOCATE 24, 65: PRINT "Reg:"; RTRIM$(STR$(R)); "/"; LTRIM$(STR$(MAXST)); "  ";
      END SELECT
   LOOP
   RETURN

BOTA.AVALL:
   IF R >= (MAXST - 1) THEN RETURN
   GOSUB ANAR.TOPE
   S = 1
   DO
	S = S + 1
	IF RB + S > MAXST - 1 THEN
	   WHILE (RB + S > MAXST - 1)
		 S = S - 1
	   WEND
	   EXIT DO
	END IF
   LOOP UNTIL S = 13
   RB = RB + S: R = RB: RESTA = 0: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREA3, R, STOCK: X = XB
   COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
   R = RB
   RETURN

BOTA.AMUNT:
   IF R = 1 THEN RETURN
   GOSUB ANAR.TOPE
   S = 1
   DO
	S = S + 1
	IF RB - S < 1 THEN
	   WHILE (RB - S < 1)
		 S = S - 1
	   WEND
	   EXIT DO
	END IF
   LOOP UNTIL S = 13

   RB = RB - S: R = RB: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREA3, RB, STOCK: X = XB
   COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
   R = RB

   RETURN

ANAR.TOPE:
   XB = X: RB = R         ' Guarda l'estat actual
   DO UNTIL XB = 7
       XB = XB - 1        ' Resta l'estat actual fins arribar al tope del recuadre
       RB = RB - 1
   LOOP
   RETURN

'************************************************************************
' Control del cursor per el llistat amb despla‡ament de barres
' funci¢ parescuda al DBEDIT de Clipper.
'************************************************************************

PUJA:
       IF X = 7 THEN
	  IF R < 1 THEN R = 1
	  GET AREA3, R, STOCK
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R <= 1 THEN
	     X = 7: R = 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 7
	  SCROLLDOWN 18, 78, 6, 1, 1
	  GET AREA3, R, STOCK
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA3, R, STOCK
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  GET AREA3, R, STOCK
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXA:
       IF X = 19 THEN
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R >= (MAXST - 1) THEN
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  GET AREA3, R, STOCK
	  R = R + 1: X = 19
	  SCROLLUP 18, 78, 6, 1, 1
	  GET AREA3, R, STOCK
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  IF R < 1 THEN R = 1
	  GET AREA3, R, STOCK
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R >= MAXST - 1 THEN
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  GET AREA3, R, STOCK
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

'************************************************************************
' Mostra el cursor a la posici¢ que est…
'************************************************************************

SHOWCURSOR:
	  LOCATE X, 2: PRINT STOCK.CODI; "³"; STOCK.DESCRIPCIO;
	  PRINT USING "³##,###,###.##³ \ \"; STOCK.PVPACONSE; STOCK.MARCAT
	  RETURN

'************************************************************************
' CONSUTAR REGISTRES UN PER UN MOSTRANT LA FITXA
' DE L'ARTICLE.
'************************************************************************

CONSULTARARTICLE:
	  GetBackground 1, 1, 24, 79, STK$
	  GOSUB INITCAMPS
	  SetMaxCamps 11

	  FOR C = 0 TO 12: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  FOR C = 0 TO 12: DeleteCamp C: NEXT

	  RV = R
	  GET AREA3, R, STOCK
	  GOSUB MASCARA
	  GOSUB MOSTRA
	  DO
	    T$ = INKEY$
	    SELECT CASE T$
		   CASE CHR$(0) + "P"      ' BAIXA UN REGISTRE
			GOSUB BXA
		   CASE CHR$(0) + "H"      ' PUJA UN REGISTRE
			GOSUB PJA
		   CASE CHR$(27)
			PutBackground 1, 1, STK$
			FOR C = 0 TO 12: DeleteCamp C: NEXT
			RETURN
		   CASE ELSE
	    END SELECT
	  LOOP
BXA:
	  RV = RV + 1
	  GOSUB MOSTRA
	  COLOR COL(2, 0), COL(2, 1): LOCATE 5, 11: PRINT "ARTICLE N§ : "; : COLOR COL(0, 0), COL(0, 1): PRINT RV
	  RETURN
PJA:
	  RV = RV - 1
	  GOSUB MOSTRA
	  COLOR COL(2, 0), COL(2, 1): LOCATE 5, 11: PRINT "ARTICLE N§ : "; : COLOR COL(0, 0), COL(0, 1): PRINT RV
	  RETURN

MOSTRA:
	  IF RV <= 1 THEN RV = 1
	  IF RV >= MAXST - 1 THEN RV = MAXST - 1
	  GET AREA3, RV, STOCK: GOSUB MOURECAMPS
	  COLOR COL(0, 0), COL(0, 1)
	  DisplayAllCamps

	  IF STOCK.MARCAT = "*" THEN
	     LOCATE 5, 50: COLOR 28, 9: PRINT "<< ESTA DE BAIXA >>"
	  ELSE
	     LOCATE 5, 50: COLOR 28, 9: PRINT "                   "
	  END IF
	  
	  RETURN

'************************************************************************
' INSERTAR ARTICLE
'************************************************************************
INSERTARARTICLE:
	  RVELL = R: TRO$ = "   ": EDIT$ = "NON"
	  TANT% = 0
	  FOR RR = 1 TO MAXST - 1
	      GET AREA3, RR, STOCK
	      TANT% = 100 / MAXST * RR
	      COLOR 27, COL(0, 1): LOCATE 5, 40: PRINT "Processant..."; : COLOR COL(0, 0): PRINT TANT%; "% "
	      IF STOCK.MARCAT = "*" THEN
		 RT = RR: TRO$ = "ZZZ"
		 EXIT FOR
	      END IF
	  NEXT
	  LOCATE 5, 40: PRINT "                   "

	  IF TRO$ = "ZZZ" THEN
	     RNOU = RT            ' SI N'HA TROBAT QUE L'EDITI D'AMUNT
	  ELSE
	     IF TRO$ = "   " THEN RNOU = MAXST         ' SI NO N'HA TROBAT QUE N'AFEGESQUI UN
	  END IF

	  GetBackground 1, 1, 24, 79, STK$
	  RV = RNOU                            ' DESIGNAR EL REGISTRE NOU
	  GOSUB MASCARA: SetMaxCamps 11
	  FOR C = 0 TO MAXCAMPS: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  GOSUB INITCAMPS: COLOR COL(0, 0), COL(0, 1)
	  GOSUB BUIDARCAMPS     ' BUIDAR CAMPS
	  GOSUB READCAMPS       ' INTRODUIR DADES ALS CAMPS
	  GOSUB MOUREFITXER     ' MOURE LES DADES DELS CAMPS AL FITXER
	  DisplayAllCamps

	  IF ANUL = 1 THEN      ' SI ES PITJA <ESC> A UN DELS CAMPS
	     PutBackground 1, 1, STK$
	     FOR C = 0 TO MAXCAMPS: DeleteCamp C: NEXT: RETURN
	  END IF

	  LOCK AREA3, RNOU
	  PUT AREA3, RNOU, STOCK  ' GRAVA DINS EL FITXER
	  UNLOCK AREA3, RNOU

	  IF TRO$ = "   " THEN
	     CEMPRE.MAXSTOCK = MAXST + 1
	     MAXST = MAXST + 1
	     PUT AREA, 1, CEMPRE
	  END IF

	  REDIM PRESERVE INDEX(1 TO MAXST) AS INDEXTYPE
	  INDEX(RV).REGISTRE = RV
	  INDEX(RV).CODI = STOCK.CODI

	  GOSUB GUARDARINDEX
	  PutBackground 1, 1, STK$
	  COLCURS = 12: GOSUB SHOWCURSOR
	  FOR C = 0 TO MAXCAMPS: DeleteCamp C: NEXT
	  RETURN
'************************************************************************
' EDITAR ARTICLE
'************************************************************************

EDITARARTICLE:
	  GetBackground 1, 1, 24, 79, STK$
	  RV = R: EDIT$ = "XXX": GOSUB MASCARA

	  LOCK AREA3, R
	  GET AREA3, R, STOCK
	  SetMaxCamps 11
	  FOR C = 0 TO 12: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  GOSUB INITCAMPS

	  GOSUB MOURECAMPS
	  COLOR COL(0, 0), COL(0, 1): CALL DisplayAllCamps
	  GOSUB READCAMPS

	  IF ANUL = 1 THEN           ' SI ES PITJA <ESC> A UN DELS CAMPS
	     PutBackground 1, 1, STK$
	     FOR C = 0 TO 12: DeleteCamp C: NEXT
	     UNLOCK AREA3, R : RETURN
	  END IF

	  GOSUB MOUREFITXER       ' MOURE LES DADES DELS CAMPS AL FITXER
	  BEEP
	  PUT AREA3, RV, STOCK     ' GRAVA DINS EL FITXER

	  UNLOCK AREA3, R
	  GOSUB INDEXAR
TORNAR:
	  PutBackground 1, 1, STK$
	  FOR C = 0 TO 12: DeleteCamp C: NEXT
	  RETURN

'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

READCAMPS:
       CORRECTE = 0
       GOSUB INDEXAR
       IF EDIT$ <> "XXX" THEN InsertValueCamp 1, FormatD$(Now#, "dd/mm/yy")
       DO

	FOR C = 0 TO MAXCAMPS
	    VALUE = ReadCamp(C)
	    SELECT CASE VALUE
		   CASE 0
			CAMPTEMP$ = ForaEspai$(ValueCamp$(0))
			IF FindRecord%(ForaEspai$(CAMPTEMP$), INDEX(), MAXST) THEN
			   IF STOCK.MARCAT = "*" THEN
			      ZZ$ = ""
			   ELSE
			      LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "<< AQUEST CODI EXISTEIX >>"
			      BEEP: C = C - 1
			      LOCATE 5, 50: COLOR COL(0, 0), COL(0, 1): PRINT "                          "
			   END IF
			END IF
		   CASE 3
			CODI$ = LTRIM$(RTRIM$(ValueCamp$(3)))
			FOR RP = 1 TO MAXPR
			    GET AREAPA, RP, PROVEIDORS
			    IF CODI$ = LTRIM$(RTRIM$(PROVEIDORS.CODI)) THEN
			       EXIT FOR
			    END IF
			NEXT
			InsertValueCamp 4, PROVEIDORS.NOM
			DisplayAllCamps
		   CASE 5
			CAMPTEMP$ = ForaEspai$(ValueCamp$(5))
			IF FindRecordFA%(ForaEspai$(CAMPTEMP$), INDEXFA(), MAXFAM) THEN
			   IF FAMILIES.MARCAT = "*" THEN
			      ZZ$ = ""
			   ELSE
			      LOCATE 10, 35: PRINT FAMILIES.DESCRIPCIO
			      DESC$ = FAMILIES.DESCRIPCIO
			   END IF
			ELSE
			   LOCATE 5, 47: COLOR 28, COL(0, 1): PRINT "<< AQUEST CODI NO EXISTEIX >>"
			   BEEP: C = C - 1
			   LOCATE 5, 47: COLOR COL(0, 0), COL(0, 1): PRINT "                             "
			END IF

		   CASE 999
			ANUL = 1: RETURN
		   CASE F1 TO F10
			SOUND 50, .5: C = C - 1
		   CASE ELSE
			PREUARTICLE = VAL(ValueCamp$(6))
			EXISTENTS = VAL(ValueCamp$(8))
			TIPUSIVA = VAL(ValueCamp$(12))
			TIPUSMC = VAL(ValueCamp$(7))
			DESCOMPT = VAL(ValueCamp$(11))

			TOTALBRUT = PREUARTICLE * EXISTENTS

			BASEIMPONIBLE = TOTALBRUT - (TOTALBRUT * DESCOMPT) / 100
			IVA = BASEIMPONIBLE * (TIPUSIVA) / 100
			IMPORTE = BASEIMPONIBLE + IVA
			PP = (PREUARTICLE - DTO) * (TIPUSMC) / 100
			PVPA = PREUARTICLE + PP
			LOCATE 16, 56: COLOR COL(0, 0), COL(0, 1): PRINT USING "##,###,###.##"; PVPA; : COLOR 14
			LOCATE 18, 27: COLOR COL(0, 0), COL(0, 1): PRINT USING "##,###,###.##"; IMPORTE; : COLOR 14
	    END SELECT
	NEXT
	LOCATE 19, 29: COLOR 27, COL(0, 1): PRINT " Son correctes aquestes dades (S/N) ?"
	COLOR COL(0, 0), COL(0, 1)

	  DO
	   DO: T$ = INKEY$: LOOP UNTIL T$ <> ""
	   IF UCASE$(T$) = "S" THEN CORRECTE = 888: EXIT DO
	   IF UCASE$(T$) = "N" THEN CORRECTE = 0: EXIT DO
	  LOOP
       LOOP UNTIL CORRECTE = 888
       RETURN

'************************************************************************
' INICIALITZA CAMPS
'************************************************************************
INITCAMPS:
	  FOR C = 0 TO 12: DeleteCamp C: NEXT
	  SetInitCamp 0, 7, 27, ASCI, 0, STRING$(18, "X"), ""
	  SetInitCamp 1, 7, 61, DAT, 0, "99/99/99", ""
	  SetInitCamp 2, 8, 27, ASCI, 0, STRING$(49, "X"), ""
	  SetInitCamp 3, 9, 27, ASCI, 0, "XXXXXXX", ""
	  SetInitCamp 4, 9, 46, ASCI, 0, STRING$(20, "X"), ""
	  SetInitCamp 5, 10, 27, ASCI, 0, STRING$(4, "X"), ""
	  SetInitCamp 6, 12, 27, ASCI, 0, "9999999999", ""
	  SetInitCamp 7, 12, 53, ASCI, 0, "99999", ""
	  SetInitCamp 8, 13, 53, ASCI, 0, "9999999999", ""
	  SetInitCamp 9, 14, 53, ASCI, 0, "9999999999", ""
	  SetInitCamp 10, 15, 27, ASCI, 0, "99999", ""
	  SetInitCamp 11, 16, 27, ASCI, 0, "99999", ""
	  DisplayAllCamps
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPS:
	  InsertValueCamp 0, STOCK.CODI
	  InsertValueCamp 1, STOCK.FECHACOMP
	  InsertValueCamp 2, STOCK.DESCRIPCIO
	  InsertValueCamp 3, STOCK.CODIPROVEEDOR
	  InsertValueCamp 4, STOCK.NOMPROVEEDOR
	  InsertValueCamp 5, "    "
	  InsertValueCamp 6, LTRIM$(STR$(STOCK.PREU))
	  InsertValueCamp 7, LTRIM$(STR$(STOCK.MARGECOMER))
	  InsertValueCamp 8, LTRIM$(STR$(STOCK.STOCKMAX))
	  InsertValueCamp 9, LTRIM$(STR$(STOCK.STOCKMIN))
	  InsertValueCamp 10, LTRIM$(STR$(STOCK.DESCOMPTE))
	  InsertValueCamp 11, LTRIM$(STR$(STOCK.TIPOIVA))
	  COLOR COL(0, 0), COL(0, 1): LOCATE 10, 35: PRINT STOCK.FAMILIA
	  LOCATE 18, 27: COLOR 15: PRINT USING "##,###,###.##"; STOCK.IMPORT; : COLOR 14
	  LOCATE 16, 56: COLOR 15: PRINT USING "##,###,###.##"; STOCK.PVPACONSE; : COLOR 14
	  RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXER:
	  STOCK.CODI = ForaEspai$(ValueCamp(0))
	  STOCK.FECHACOMP = ValueCamp$(1)
	  STOCK.DESCRIPCIO = ValueCamp$(2)
	  STOCK.CODIPROVEEDOR = ValueCamp$(3)
	  STOCK.NOMPROVEEDOR = ValueCamp$(4)
	  STOCK.FAMILIA = DESC$
	  STOCK.PREU = VAL(ValueCamp$(6))
	  STOCK.MARGECOMER = VAL(ValueCamp$(7))
	  STOCK.PVPACONSE = PVPA
	  STOCK.EXISTENCIA = VAL(ValueCamp$(8))
	  STOCK.STOCKMAX = VAL(ValueCamp$(8))
	  STOCK.STOCKMIN = VAL(ValueCamp$(9))
	  STOCK.DESCOMPTE = VAL(ValueCamp$(10))
	  STOCK.TIPOIVA = VAL(ValueCamp$(11))
	  STOCK.TOTALIVA = IVA
	  STOCK.IMPORT = IMPORTE
	  STOCK.MARCAT = ""
	  RETURN

'************************************************************************
' BUIDAR ELS CAMPS DEL FITXER
'************************************************************************
BUIDARCAMPS:
	  STOCK.CODI = ""
	  STOCK.FECHACOMP = FETXA$
	  STOCK.DESCRIPCIO = ""
	  STOCK.CODIPROVEEDOR = ""
	  STOCK.NOMPROVEEDOR = ""
	  STOCK.FAMILIA = ""
	  STOCK.PREU = 0
	  STOCK.PVPACONSE = 0
	  STOCK.MARGECOMER = 0
	  STOCK.EXISTENCIA = 0
	  STOCK.STOCKMAX = 0
	  STOCK.STOCKMIN = 0
	  STOCK.DESCOMPTE = 0
	  STOCK.TIPOIVA = 0
	  STOCK.TOTALIVA = 0
	  STOCK.IMPORT = 0
	  FOR C = 0 TO 12: DeleteCamp C: NEXT
	  RETURN

'************************************************************************
' FITXA DE UN ARTICLE
'************************************************************************

MASCARA:
	  COLOR COL(0, 0), COL(0, 1)
	  FINESTRA 4, 10, 20, 76, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
	  LOCATE 5, 11: PRINT "ARTICLE N§ : "; : COLOR COL(0, 0), COL(0, 1): PRINT RV
	  LOCATE 6, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0), COL(2, 1)
	  LOCATE 7, 11: PRINT "CODI ARTICLE...:                      DATA COMPRA:";
	  LOCATE 8, 11: PRINT "DESCRIPCIO.....: ";
	  LOCATE 9, 11: PRINT "CODI PROVE‹DOR : " + SPACE$(7) + " PROVEIDOR:"
	  LOCATE 10, 11: PRINT "FAMILIA .......:    <-->"
	  LOCATE 11, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0), COL(2, 1)
	  LOCATE 12, 11: PRINT "PREU COMPRA ...: " + SPACE$(9) + "MARGE COMERCIAL:       %"
	  LOCATE 13, 11: PRINT "                 " + SPACE$(10) + "         STOCK:"
	  LOCATE 14, 11: PRINT SPACE$(27) + "   STOCK MINIM:"
	  LOCATE 15, 11: PRINT "DESCOMPTE .....:      %"
	  LOCATE 16, 11: PRINT "I.V.A. ........:      %       PREU DE VENDA:"
	  LOCATE 17, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0), COL(2, 1)
	  LOCATE 18, 11: PRINT "IMPORT TOTAL ..:                                  "
	  RETURN

'************************************************************************
' LLISTA UN BLOC DEL STOCK DEFINIT
'************************************************************************

LISTA:
    COLOR COL(0, 0), COL(0, 1)
    IF RESTA = 0 THEN a = 7: b = 19: C = 1 ELSE a = 19: b = 7: C = -1
    FOR X = 7 TO 19
	LOCATE X, 2: PRINT SPACE$(18); "³"; SPACE$(40); "³             ³    "
    NEXT
    FOR X = a TO b STEP C
       IF R >= MAXST OR R < 1 THEN EXIT FOR
       GET AREA3, R, STOCK
       GOSUB SHOWCURSOR
       IF RESTA = 0 THEN R = R + 1 ELSE R = R - 1
    NEXT
    RETURN

'************************************************************************
' MARCAR UN REGISTRE COM A BORRAT
'************************************************************************
					   
DELETERECORD:
    LOCK AREA3, R
    GET AREA3, R, STOCK
    IF STOCK.MARCAT = "*" THEN
       STOCK.MARCAT = " "
       PUT AREA3, R, STOCK
       UNLOCK AREA3, R
    ELSE
      IF STOCK.MARCAT = " " THEN
	 STOCK.MARCAT = "*"
	 PUT AREA3, R, STOCK
	 UNLOCK AREA3, R
      END IF
    END IF
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
    RETURN

'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE STOCK
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "STOCK.NDX" FOR RANDOM SHARED AS AREA2 LEN = LEN(NDXFILE)
      AREA3 = FREEFILE: OPEN DIRECC$ + "STOCK.DAT" FOR RANDOM SHARED AS AREA3 LEN = LEN(STOCK)
      AREAFN = FREEFILE: OPEN DIRECC$ + "FAMILIES.NDX" FOR RANDOM SHARED AS AREAFN LEN = LEN(NDXFAMI)
      AREAFA = FREEFILE: OPEN DIRECC$ + "FAMILIES.DAT" FOR RANDOM SHARED AS AREAFA LEN = LEN(FAMILIES)
      AREAPN = FREEFILE: OPEN DIRECC$ + "PROVEID.NDX" FOR RANDOM SHARED AS AREAPN LEN = LEN(NDXPROV)
      AREAPA = FREEFILE: OPEN DIRECC$ + "PROVEID.DAT" FOR RANDOM SHARED AS AREAPA LEN = LEN(PROVEIDORS)
      AREA4 = FREEFILE: OPEN DP$ + "PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CAP)
      
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
      MAXST = CEMPRE.MAXSTOCK
      MAXPR = CEMPRE.MAXPROVEID
      RETURN

INDEXAR:
      FOR RI = 1 TO MAXST - 1               ' COL.LOCAR A MEM•RIA L'INDEX
	  GET AREA2, RI, NDXFILE
	  INDEX(RI).REGISTRE = NDXFILE.REGISTRE
	  INDEX(RI).CODI = NDXFILE.CODI
      NEXT

      FOR RI = 1 TO MAXFAM               ' COL.LOCAR A MEM•RIA L'INDEX
	  GET AREAFN, RI, NDXFAMI
	  INDEXFA(RI).REGISTRE = NDXFAMI.REGISTRE
	  INDEXFA(RI).CODI = NDXFAMI.CODI
      NEXT
      RETURN

GUARDARINDEX:
      CALL SortIndex(INDEX(), MAXST - 1)      ' ORDENAR L'INDEX
      LOCK AREA2
      FOR RI = 1 TO MAXST - 1
	  NDXFILE.REGISTRE = INDEX(RI).REGISTRE
	  NDXFILE.CODI = INDEX(RI).CODI
	  PUT AREA2, RI, NDXFILE
      NEXT
      UNLOCK AREA2
      RETURN
TANCA:
      FOR C = 0 TO 12: DeleteCamp C: NEXT
      ERASE INDEX, CAMPS: RESET
      CLOSE AREA: CLOSE AREA3: CLOSE AREA4: CLOSE AREA2
      EXIT SUB


END SUB

SUB MASCLIST
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 4, 1, 20, 80, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
    LOCATE 5, 2: PRINT "Codi Article       Descripci¢" + SPACE$(31) + "P.V.P.       Baixa": COLOR COL(0, 0), COL(0, 1)
    LOCATE 6, 2: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄ"
    FOR L = 7 TO 19
	LOCATE L, 2: PRINT "                  ³                                        ³             ³"
    NEXT
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR ARTICLE <F2>=INSERTAR ARTICLE  <" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR              <F3>=SUMAR STOCK       <F4>=CONSULTAR"
    LOCATE 24, 2: PRINT "<F5>=MARCAR ARTICLE       <F6>=LLISTAR";
END SUB

SUB SortIndex (INDEX() AS INDEXTYPE, MAXST) STATIC
    'SHARED INDEX() AS INDEXTYPE
    
    OFFSET = MAXST - 1 \ 2

    DO WHILE OFFSET > 0
       LIMIT = MAXST - 1 - OFFSET
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

SUB SumarStock (AREA3, MAXST)
    'IF TESTSCREEN = &HB000 THEN SAVESCREEN DIRECCP$ + "STOCK.SCR", 0, 4100 ELSE CALL SAVESCRN
    GetBackground 1, 1, 24, 79, STK$
    COLOR COL(0, 0), COL(0, 1): FINESTRA 9, 10, 16, 60, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
    LOCATE 10, 12: COLOR COL(2, 1): PRINT "SUMA DEL STOCK"
    suma = 0: sumau = 0
    FOR R = 1 TO MAXST
	GET AREA3, R, STOCK
	suma = suma + STOCK.IMPORT
	sumau = sumau + STOCK.PREU
	COLOR COL(2, 0), COL(2, 1): LOCATE 11, 12: PRINT "Total Imports.......: "; : COLOR COL(0, 0), COL(0, 1): PRINT USING "##,###,###.##"; suma; : COLOR COL(2, 0), COL(2, 1): PRINT " Pts."
	COLOR COL(2, 0), COL(2, 1): LOCATE 12, 12: PRINT "Total Preus Unitaris: "; : COLOR COL(0, 0), COL(0, 1): PRINT USING "##,###,###.##"; sumau; : COLOR COL(2, 0), COL(2, 1): PRINT " Pts."
	COLOR COL(2, 0), COL(2, 1): LOCATE 14, 12: PRINT "N£mero d'articles...: "; : COLOR COL(0, 0), COL(0, 1): PRINT USING " ##,####,####"; R;
    NEXT
    SOUND 300, 2
    SOUND 100, 2
    SOUND 300, 2
    C$ = INPUT$(1)
    'IF TESTSCREEN = &HB000 THEN RESTORESCREEN DIRECCP$ + "STOCK.SCR", 0 ELSE CALL RESTSCRN
    PutBackground 1, 1, STK$
END SUB

SUB TRANFEREIXFITXERS (AREA2, AREA3)
    R = 1
    COLOR 15, 9
    DO WHILE NOT EOF(AREA2)
       GET AREA2, R, STOCK1
       STOCK.CODI = STOCK1.CODIGO
       STOCK.DESCRIPCIO = STOCK1.ARTICULO
       STOCK.CODIPROVEEDOR = STOCK1.CODIPROVE
       STOCK.NOMPROVEEDOR = STOCK1.NOMPROVEEDOR
       STOCK.EXISTENCIA = VAL(STOCK1.EXISTENCIA)
       STOCK.PREU = VAL(STOCK1.PRECIO)
       STOCK.DESCOMPTE = VAL(STOCK1.DESCUENTO)
       STOCK.PREUNET = VAL(STOCK1.PREUNET)
       STOCK.MARGECOMER = VAL(STOCK1.MARGECOMER)
       STOCK.PVPACONSE = VAL(STOCK1.PVPACONSE)
       STOCK.IMPORT = VAL(STOCK1.IMPORTE)
       STOCK.TIPOIVA = VAL(STOCK1.TIPOIVA)
       STOCK.TOTALIVA = VAL(STOCK1.TOTALIVA)

       STOCK.STOCKMIN = VAL(STOCK1.STOCKMIN)
       STOCK.STOCKMAX = VAL(STOCK1.STOCKMAX)
     
       STOCK.FECHACOMP = STOCK1.FECHACOMP
       STOCK.FAMILIA = STOCK1.FAMILIA
       STOCK.MARCAT = STOCK1.MARCAT
       LOCATE 1, 1: PRINT R
       PUT AREA3, R, STOCK
       R = R + 1
    LOOP
    PRINT "TRANSFERIT... Ok"
END SUB

