' ********************************************************************
'
' Fitxer....................: CLI_004.BAS
' Titol.....................: Modul per el manteniment dels CP's
'
' ********************************************************************
'
' Data inici................: 25/03/1997 17:10:00
' Data de la darrera revisi¢: 10/06/1998 14:08:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/98 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB MantenimentCPS (DIRECCP$, DIRECC$, DIRECI$, DP$, DEVI$, IMPRESORA, MAXLINS)
DECLARE FUNCTION FindCP% (INDEX() AS ANY, CAMP$, MAXCP!)
DECLARE SUB LISTCP (AREA4!, AREA3!, DIRECC$, DEVI$, MAXLINS!)
DECLARE SUB SortCPs (INDEX() AS ANY, MAXCP!)
DECLARE SUB MASCCP ()

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CP.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'

COMMON SHARED DIRECC$, DIRCP$, DEVI$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT, RNDX, MAXCP

'$DYNAMIC
DIM SHARED CFG AS CONFIG
DIM SHARED EMPRES AS EMPRESA
DIM SHARED CTRL AS CONTROL
DIM SHARED CEMPRE AS CONTROLEMP       '     "      DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAP€ALERAS DE DOCUMENTS
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED CODISP AS CP
DIM SHARED COLORS AS COLO
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

	  IF LTRIM$(RTRIM$(CA2$)) <> "CLI_004.EXE" THEN
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

      SetDirRecursos (DIRECCR$)
      SetFormatCC (34)

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

      CALL MantenimentCPS(DIRECCP$, DIRECCF$, DIRECCI$, DP$, DEV$, IMPRESORA, MAXLINS)
      SYSTEM

ERRORS:

      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

REM $STATIC
FUNCTION FindCP% (INDEX() AS INDEXCP, CAMP$, MAXCP)
	 SHARED RNDX
	 TOPRECORD = MAXCP
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
	    GET AREA3, INDEX(MIDPOINT).REGISTRE, CODISP
	    FindCP% = TRUE
	    RNDX = MIDPOINT
	 ELSE
	    FindCP% = FALSE
	 END IF
END FUNCTION

SUB LISTCP (AREA4, AREA3, DIRECC$, DEVI$, MAXLINS)
    SHARED DIRECCT$, DIRECCM$, MAXCP
    DIM CAB$(3)
    DIRECCT$ = DIRECC$ + "TEXTOS\"
    AREAMSK = FREEFILE: OPEN DIRECC$ + "PLANTILL\CODISP.MSK" FOR INPUT SHARED AS AREAMSK
    AREATXT = FREEFILE: OPEN DIRECCT$ + "CODISP.TXT" FOR OUTPUT SHARED AS AREATXT
    
    ' DEFINIR MASCARA DE LES LINIES
    GET AREA4, 1, CAP             ' AGAFAR CAP€ALERA DEL FITXER DE CAP€ALERES
    LINE INPUT #AREAMSK, CAB$(1)  ' AGAFAR LA CAP€ALERA DELS CAMPS
    LINE INPUT #AREAMSK, CAB$(2)
    LINE INPUT #AREAMSK, CAB$(3)
    CLOSE #AREAMSK

    AVIS.SONOR (1)
    tecla = Avis("AVIS:", "INSERTI PAPER A LA IMPRESORA", "Pitji una tecla...", 0)
    IF INKEY$ = CHR$(27) THEN EXIT SUB

    PAG = 1: L = 1
    GOSUB CAPSA         ' IMPRIMIR CAP€ALERA

    FOR R = 1 TO MAXCP
	GET AREA3, R, CODISP
	IF CODISP.MARCAT <> "*" THEN
	   IF L > MAXLINS THEN
	      PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(79, "Ä") + RTRIM$(ARCHIMP.SALTOPAGINA)
	      L = 1: PAG = PAG + 1
	      GOSUB CAPSA
	   ELSE
	      PRINT #AREATXT, USING CAB$(3); CODISP.CODI; CODISP.POBLE; CODISP.PAIS; CODISP.PROVINCIA
	      L = L + 1
	   END IF
	END IF
    NEXT

    FOR LA = L TO MAXLINS: PRINT #AREATXT, "": NEXT
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(79, "Ä") + RTRIM$(ARCHIMP.COMPRIMIDO)
    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT
    CALL ImprimeixFitxerTXT(DIRECCT$ + "CODISP.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP€ALERA DELS LLISTATS
' **************************************************************************

CAPSA:
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, "LLISTAT TOTAL DELS CODIS POSTALS"
    PRINT #AREATXT, "P…gina:"; PAG
    PRINT #AREATXT, "Data..: "; FormatD$(Now#, "dd/mm/yyyy")
    PRINT #AREATXT, STRING$(78, "Ä");
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""
    PRINT #AREATXT, CAB$(1)
    PRINT #AREATXT, CAB$(2)
    RETURN
END SUB

SUB MantenimentCPS (DIRECCP$, DIRECC$, DIRECI$, DP$, DEVI$, IMPRESORA, MAXLINS) STATIC
    SHARED RNDX

    ON ERROR GOTO ERRORS
    COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "  OBRINT..."

    GOSUB OBRIRFITXERS
    MAXCP = LOF(AREA3) \ LEN(CODISCP) + 1

    DIM INDEX(1 TO MAXCP) AS INDEXCP
    DIM NDXCP AS INDEXCP

    IF MAXCP = 1 THEN
       X = 7: R = 1: L = 1
       BEEP: tecla = Avis("AVIS:", "El fitxer de codis postals est… buit.", "Pitji una tecla...", 0)
       GOSUB INSERTARCLIENT
       IF ANUL = 1 THEN RESET: EXIT SUB
       GOSUB GUARDARINDEX
    END IF
    
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "INDEXANT..."
    GOSUB INDEXAR
    CALL SortCPs(INDEX(), MAXCP)          ' ORDENAR L'INDEX

    MASCCP                              ' PINTA LA MASCARA DE LA LLISTA
    X = 7: R = 1: L = 1
    '
    ' ********* MOSTRA ELS REGISTRES QUE CABIGEN EN UNA PANTALLA
    '
    GOSUB LISTA

    X = 7: R = 1                      ' INICIA CURSOR I POSICIONS A PANTALLA
    GET AREA3, R, CODISP
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
    DO
      MAXCP = LOF(AREA3) \ LEN(CODISP) + 1

      OP$ = INKEY$
      SELECT CASE OP$
	     CASE CHR$(0) + "P"              ' BAIXA UN REGISTRE
		  GOSUB BAIXA
	     CASE CHR$(0) + "H"              ' PUJA UN REGISTRE
		  GOSUB PUJA
	     CASE CHR$(27)
		  COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
		  COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT " TANCANT..."
		  GOSUB TANCA
	     CASE CHR$(13)
		  GOSUB EDITARCLIENT         ' EDITAR CLIENT
	     CASE CHR$(0) + CHR$(60)
		  GOSUB INSERTARCLIENT       ' INSERTAR CLIENT
	     CASE CHR$(0) + CHR$(62)
		  GOSUB CONSULTARCLIENT      ' CONSULTAR CLIENT
	     CASE CHR$(0) + CHR$(63)
		  GOSUB DELETERECORD         ' MARCAR REGISTRE
	     CASE CHR$(0) + CHR$(64)
		  CALL LISTCP(AREA4, AREA3, DP$, DEVI$, MAXLINS)' LLISTAR CLIENTS
	     CASE CHR$(0) + "Q"
		  GOSUB BOTA.AVALL
	     CASE CHR$(0) + "I"
		  GOSUB BOTA.AMUNT
	     CASE ELSE
		  LOCATE , , 0
		  COLOR COL(0, 0), COL(0, 1): LOCATE 24, 60: PRINT "Registre: "; LTRIM$(STR$(R)); "/"; LTRIM$(STR$(MAXCP - 1)); "  ";
      END SELECT
   LOOP
   RETURN

BOTA.AVALL:
   IF R = MAXCP - 1 THEN RETURN
   GOSUB ANAR.TOPE
   S = 1
   DO
	S = S + 1
	IF RB + S > MAXCP - 1 THEN
	   WHILE (RB + S > MAXCP - 1)
		 S = S - 1
	   WEND
	   EXIT DO
	END IF
   LOOP UNTIL S = 13
   RB = RB + S: R = RB: RESTA = 0: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREA3, RB, CODISP: X = XB
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
   GET AREA3, RB, CODISP: X = XB
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
	  GET AREA3, R, CODISP
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = 1 THEN
	     X = 7: R = 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 7
	  SCROLLDOWN 18, 78, 6, 1, 1
	  GET AREA3, R, CODISP
	  
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA3, R, CODISP
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  GET AREA3, R, CODISP
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXA:
       IF X = 19 THEN
	  GET AREA3, R, CODISP
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXCP - 1 THEN
	      R = MAXCP - 1: X = 19
	      COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR: GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = 19
	  SCROLLUP 18, 78, 6, 1, 1
	  GET AREA3, R, CODISP
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA3, R, CODISP
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXCP - 1 THEN
	     R = MAXCP - 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  GET AREA3, R, CODISP
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

'************************************************************************
' Mostra el cursor a la posici¢ que est…
'************************************************************************

SHOWCURSOR:
	  LOCATE X, 2: PRINT CODISP.CODI; "³"; CODISP.POBLE; "³"; CODISP.PROVINCIA; "³"; CODISP.MARCAT; "       "
	  RETURN

'************************************************************************
' CONSUTAR REGISTRES UN PER UN, MOSTRANT LA FITXA
'************************************************************************

CONSULTARCLIENT:
	  GetBackground 1, 1, 24, 79, cli$
	  GOSUB INITCAMPS
	  SetMaxCamps 3

	  FOR C = 0 TO 3: SetColorCamp C, COL(0, 1), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  FOR C = 0 TO 3: DeleteCamp C: NEXT

	  RV = R
	  GET AREA3, R, CODISP
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
			PutBackground 1, 1, cli$
			FOR C = 0 TO 2: DeleteCamp C: NEXT
			RETURN
		   CASE ELSE
	    END SELECT
	  LOOP
BXA:
	  RV = RV + 1
	  GOSUB MOSTRA
	  COLOR COL(2, 0), COL(2, 1): LOCATE 5, 11: PRINT "Codi No.: "; :  COLOR COL(0, 0), COL(0, 1): PRINT RV
	  RETURN
PJA:
	  RV = RV - 1
	  GOSUB MOSTRA
	  COLOR COL(2, 0), COL(2, 1): LOCATE 5, 11: PRINT "Codi No.: "; :  COLOR COL(0, 0), COL(0, 1): PRINT RV
	  RETURN

MOSTRA:
	  IF RV <= 1 THEN RV = 1
	  IF RV >= MAXCP - 1 THEN RV = MAXCP - 1
	  GET AREA3, RV, CODISP: GOSUB MOURECAMPS
	  COLOR COL(0, 0), COL(0, 1)
	  DisplayAllCamps

	  IF CODISP.MARCAT = "*" THEN
	     LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "<< ESTA DE BAIXA >>"
	  ELSE
	     LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "                   "
	  END IF
	  RETURN

'************************************************************************
' INSERTAR CLIENT
'************************************************************************

INSERTARCLIENT:
	  ' MIRA A VEURE SI HI HA ALGUN REGISTRE MARCAT PER
	  ' BORRAR-LO
	  
	  TANT% = 0: TROBAT = 0: RVELL = R: T$ = "   "

	  FOR RR = 1 TO MAXCP
	      GET AREA3, RR, CODISP
	      TANT% = 100 / MAXCP * RR
	      COLOR 27, COL(0, 1): LOCATE 5, 40: PRINT "Processant..."; : COLOR COL(0, 0): PRINT TANT%; "% "
	      IF CODISP.MARCAT = "*" THEN
		 RT = RR: T$ = "ZZZ"
		 EXIT FOR
	      END IF
	  NEXT
	  LOCATE 5, 40: PRINT "                   "

	  IF T$ = "ZZZ" THEN
	     RNOU = RR                      ' SI N'HA TROBAT QUE L'EDITI D'AMUNT
	  ELSE
	     RNOU = MAXCP                  ' SI NO N'HA TROBAT QUE N'AFEGESQUI UN
	  END IF

	  GetBackground 1, 1, 24, 79, clis$

	  RV = RNOU                         ' DESIGNAR EL REGISTRE NOU
	  GOSUB MASCARA
	  SetMaxCamps 3
	  FOR C = 0 TO 3: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  
	  GOSUB INITCAMPS: COLOR 15, 9
	  GOSUB BUIDARCAMPS                 ' BUIDAR CAMPS
	  GOSUB READCAMPS                   ' INTRODU‹R DADES ALS CAMPS
	  GOSUB MOUREFITXER                 ' MOURE LES DADES DELS CAMPS AL FITXER
	  DisplayAllCamps

	  IF ANUL = 1 THEN                  ' SI ES PITJA <ESC> A UN DELS CAMPS
	     PutBackground 1, 1, clis$
	     FOR C = 0 TO 3: DeleteCamp C: NEXT
	     RETURN
	  END IF
	  
	  PUT AREA3, RNOU, CODISP          ' GRAVAR DINS EL FITXER

	  ' ACTUALITZA L'INDEX

	  RNDX = RNOU
	  IF T$ <> "ZZZ" THEN MAXCP = MAXCP + 1
	  GOSUB ACTUALIZ.INDEX
	  GOSUB GUARDARINDEX
	  

	  PutBackground 1, 1, clis$
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR: FOR C = 0 TO 2: DeleteCamp C: NEXT
	  RETURN

'************************************************************************
' EDITAR CLIENT
'************************************************************************

EDITARCLIENT:
	  GetBackground 1, 1, 24, 79, cli$
	  RV = R
	  GOSUB MASCARA
	  LOCK AREA3, RV
	  GET AREA3, R, CODISP
	  SetMaxCamps 3
	  FOR C = 0 TO 3: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  GOSUB INITCAMPS
	  GOSUB MOURECAMPS
	  COLOR 15, 9: CALL DisplayAllCamps: EDIT = 99
	  GOSUB READCAMPS
	  EDIT = 0
	  IF ANUL = 1 THEN           ' SI ES PITJA <ESC> A UN DELS CAMPS
	     PutBackground 1, 1, cli$
	     UNLOCK AREA3, RV
	     FOR C = 0 TO 3: DeleteCamp C: NEXT
	     RETURN
	  END IF
	  GOSUB MOUREFITXER          ' MOURE LES DADES DELS CAMPS AL FITXER
	  PUT AREA3, RV, CODISP    ' GRAVA DINS EL FITXER
	  
TORNAR:
	  FOR C = 0 TO 3: DeleteCamp C: NEXT
	  PutBackground 1, 1, cli$
	  UNLOCK AREA3, RV
	  RETURN


'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

READCAMPS:
       GOSUB INDEXAR
       CORRECTE = 0: ANUL = 0
       SetMaxCamps 3
       DO
	  FOR C = 0 TO 3
	     VALUE = ReadCamp(C)
	     SELECT CASE VALUE
		    CASE 0
			  CAMPTEMP$ = ForaEspai$(ValueCamp$(0))
			  IF FindCP%(INDEX(), CAMPTEMP$, MAXCP) THEN
			     IF CODISP.MARCAT <> "*" THEN
				LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "<< AQUEST CODI EXISTEIX >>"
				BEEP
				LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "                          "
				C = C - 1
			     END IF
			  END IF
		    CASE 999
			 ANUL = 1
			 RETURN
		    CASE F1 TO F10
			 C = C - 1: SOUND 50, .5
		    CASE ELSE
	     END SELECT
	 NEXT

	  LOCATE 19, 29: COLOR 27: PRINT " Son correctes aquestes dades (S/N) ?"
	  COLOR 15, 9

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
	  FOR C = 0 TO 3: DeleteCamp C: NEXT
	  SetInitCamp 0, 7, 29, ASCI, 0, "XXXXXXX", ""
	  SetInitCamp 1, 8, 29, ASCI, 0, STRING$(27, "X"), ""
	  SetInitCamp 2, 10, 29, ASCI, 0, STRING$(27, "X"), ""
	  SetInitCamp 3, 11, 29, ASCI, 0, STRING$(28, "X"), ""
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPS:
	  InsertValueCamp 0, CODISP.CODI
	  InsertValueCamp 1, CODISP.POBLE
	  InsertValueCamp 2, CODISP.PAIS
	  InsertValueCamp 3, CODISP.PROVINCIA
	  RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXER:
	  CODISP.CODI = RTRIM$(ValueCamp(0))
	  CODISP.POBLE = ValueCamp$(1)
	  CODISP.PAIS = ValueCamp$(2)
	  CODISP.PROVINCIA = ValueCamp$(3)
	  CODISP.MARCAT = "-"
	  RETURN

'************************************************************************
' BUIDAR ELS CAMPS DEL FITXER
'************************************************************************
BUIDARCAMPS:
	   CODISP.CODI = STRING$(LEN(CLIENTS.CODI), CHR$(32))
	   CODISP.POBLE = ""
	   CODISP.PAIS = ""
	   CODISP.PROVINCIA = ""
	   CODISP.MARCAT = "-"
	   FOR C = 0 TO 3: DeleteCamp C: NEXT
	   RETURN

'************************************************************************
' FITXA DE UN CLIENT
'************************************************************************

MASCARA:
	  COLOR COL(0, 0), COL(0, 1)
	  FINESTRA 4, 10, 13, 76, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
	  LOCATE 5, 11: PRINT "Codi No.: "; :  COLOR COL(0, 0), COL(0, 1): PRINT RV
	  LOCATE 6, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0), COL(2, 1)
	  LOCATE 7, 11: PRINT "             Codi:"
	  LOCATE 8, 11: PRINT "            Poble:"
	  COLOR COL(0, 0), COL(0, 1): LOCATE 9, 11: PRINT STRING$(65, "Ä"); :  COLOR COL(2, 0), COL(2, 1)
	  LOCATE 10, 11: PRINT "            Pais:"
	  LOCATE 11, 11: PRINT "       Provincia:"

	  RETURN

'************************************************************************
' LLISTA UN BLOC
'************************************************************************

LISTA:
    COLOR 15, 9: X = 7
    FOR X = 7 TO 19
	COLOR COL(0, 0), COL(0, 1)
	LOCATE X, 2: PRINT SPACE$(12); "³"; SPACE$(27); "³"; SPACE$(28); "³        "
	'LOCATE X, 2: PRINT "      ³          " + SPACE$(11) + "  ³      " + SPACE$(30) + "    ³     ": COLOR COL(0, 0), COL(0, 1)
    NEXT

    FOR X = 7 TO 19
       IF R = MAXCP THEN EXIT FOR
       GET AREA3, R, CODISP
       COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
       R = R + 1
    NEXT
    RETURN

'************************************************************************
' MARCAR UN REGISTRE COM A BORRAT
'************************************************************************

DELETERECORD:
    GET AREA3, R, CODISP             ' CARREGAR EL REGISTRE DINS MEMORIA
    IF CODISP.MARCAT = "*" THEN       ' COMPROVAR SI ESTA MARCAT
       CODISP.MARCAT = "-"
       PUT AREA3, R, CODISP
    ELSE                               ' SI NO ESTA MARCAT QUE EL MARQUI
      IF CODISP.MARCAT <> "*" THEN
	 CODISP.MARCAT = "*"
	 PUT AREA3, R, CODISP
      END IF
    END IF
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR     ' TORNA A VISUALITZAR LA LINIA
    RETURN

'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE CLIENTS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "CODISP.NDX" FOR RANDOM SHARED AS AREA2 LEN = LEN(NDXCP)
      AREA3 = FREEFILE: OPEN DIRECC$ + "CODISP.DAT" FOR RANDOM SHARED AS AREA3 LEN = LEN(CODISP)
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
      RETURN

INDEXAR:
      FOR RI = 1 TO MAXCP                   ' COL.LOCAR A MEM•RIA L'INDEX I LA CLAU
	  GET AREA2, RI, NDXCP
	  INDEX(RI).REGISTRE = NDXCP.REGISTRE
	  INDEX(RI).CODI = NDXCP.CODI

      NEXT
      'GOSUB TANCA
      RETURN

GUARDARINDEX:
      CALL SortCPs(INDEX(), MAXCP - 1)   ' ORDENAR L'INDEX
      
      FOR RI = 1 TO MAXCP                  ' COL.LOCAR A MEM•RIA L'INDEX I LA CLAU
	  NDXCP.REGISTRE = INDEX(RI).REGISTRE
	  NDXCP.CODI = INDEX(RI).CODI
	  PUT AREA2, RI, NDXCP
      NEXT
      RETURN

TANCA:
      FOR C = 0 TO 2: DeleteCamp C: NEXT
      ERASE INDEX, CAMPS
      RESET
      EXIT SUB

ACTUALIZ.INDEX:
      REDIM PRESERVE INDEX(1 TO MAXCP) AS INDEXCP
      LOCATE 24, 10: PRINT MAXCP
      INDEX(RNDX).REGISTRE = RNDX
      INDEX(RNDX).CODI = CODISP.CODI
      RETURN
END SUB

SUB MASCCP
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 4, 1, 20, 80, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
    LOCATE 5, 2: PRINT "Codi        ³Poblaci¢                   ³Provincia                   ³Baixa": COLOR COL(0, 0), COL(0, 1)
    LOCATE 6, 2: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄ"

    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR CODI    <F2>=INSERTAR CODI     <" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR              <F3>=SUMAR             <F4>=CONSULTAR"
    LOCATE 24, 2: PRINT "<F5>=MARCAR CODI          <F6>=LLISTAR";
END SUB

SUB SortCPs (INDEX() AS INDEXCP, MAXCP) STATIC
    
    OFFSET = MAXCP \ 2

    DO WHILE OFFSET > 0
       LIMIT = MAXCP - OFFSET
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

