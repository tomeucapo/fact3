' ********************************************************************
'
' Fitxer....................: PROVEID.BAS
' Titol.....................: Modul per el manteniment de proveidors
'
' ********************************************************************
'
' Data inici................: 16/09/1997 13:31:00
' Data de la darrera revisi?: 24/02/1998 21:38:00
' Autor.....................: Tomeu Cap? Cap?
' CopyRight.................: Smart Software 1993/98 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB SortCPS (INDEX() AS ANY, MAXCP!)
DECLARE SUB MantenimentProveidors (DIRECCP$, DIRECC$, DIRECI$, DP$, DEVI$, IMPRESORA!, MAXLINS!)
DECLARE SUB LISTCLI (AREA4!, AREA3!, DIRECC$, DEVI$, MAXLINS!)
DECLARE SUB MASCLIST ()
DECLARE FUNCTION FindProveidor% (CAMP$, INDEX() AS ANY, MAXPR!)
DECLARE SUB MASCCLIS ()
DECLARE SUB SortProveidors (INDEX() AS ANY, MAXPR!)
DECLARE FUNCTION FindCP% (INDEX() AS ANY, CAMP$, MAXCP!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'c:\FACT3\FONTS\CP.BI'
'$INCLUDE: 'c:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'c:\FACT3\FONTS\PROVEID.BI'
'$INCLUDE: 'c:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'c:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'c:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'c:\FACT3\FONTS\CONSTS.BI'

COMMON SHARED DIRECC$, DIRCP$, DEVI$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT, RNDX

DIM SHARED CFG AS CONFIG
DIM SHARED EMPRES AS EMPRESA
DIM SHARED CTRL AS CONTROL
DIM SHARED CEMPRE AS CONTROLEMP       '     "      DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAP?ALERAS DE DOCUMENTS
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED PROVEIDORS AS PROVEID
DIM SHARED COLORS AS COLO
DIM SHARED COL(2, 1)
DIM SHARED CODISP AS CP
DIM SHARED USR AS USUARIS
DIM SHARED DOCNUM AS DN
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

	  IF LTRIM$(RTRIM$(CA2$)) <> "PROVEID.EXE" THEN
	     tecla = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "Piji una tecla...", 0)
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
      MAXFAC = USR.LINFACT
      IVA = EMPRES.IVA
      DTO = EMPRES.DTO
      R = EMPRES.ANY
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA

      SetDirRecursos (DIRECCR$)
      SetFormatCC (34)

      AREAC = FREEFILE: OPEN DIRECCF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
      GET AREAC, 1, COLORS
      COL(0, 0) = COLORS.COL(0, 0): COL(0, 1) = COLORS.COL(0, 1)
      COL(1, 0) = COLORS.COL(1, 0): COL(1, 1) = COLORS.COL(1, 1)
      COL(2, 0) = COLORS.COL(2, 0): COL(2, 1) = COLORS.COL(2, 1)
      CLOSE AREAC

      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, R, ANYS

      DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"
      CLOSE AREAA

      CALL MantenimentProveidors(DIRECCP$, DIRECCF$, DIRECCI$, DBF$, DEV$, IMPRESORA, MAXLINS)
      SYSTEM

ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

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
	    GET AREA13, INDEX(MIDPOINT).REGISTRE, CODISP
	    FindCP% = TRUE
	    RNDX = MIDPOINT
	 ELSE
	    FindCP% = FALSE
	 END IF
END FUNCTION

FUNCTION FindProveidor% (CAMP$, INDEX() AS INDEXPROVEIDOR, MAXPR) STATIC
	 SHARED RNDX
	 TOPRECORD = MAXPR - 1
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
	    GET AREA3, INDEX(MIDPOINT).REGISTRE, PROVEIDORS
	    FindProveidor% = TRUE
	    RNDX = MIDPOINT
	 ELSE
	    FindProveidor% = FALSE
	 END IF
END FUNCTION

SUB LISTCLI (AREA4, AREA3, DIRECC$, DEVI$, MAXLINS)
    SHARED DIRECCT$, DIRECCM$
    DIM CAB$(3)
    DIRECCT$ = DIRECC$ + "TEXTOS\"

    AREAMSK = FREEFILE: OPEN DIRECC$ + "PLANTILL\PROVEIDO.MSK" FOR INPUT SHARED AS AREAMSK
    AREATXT = FREEFILE: OPEN DIRECCT$ + "PROVEIDO.TXT" FOR OUTPUT SHARED AS AREATXT
    
    ' DEFINIR MASCARA DE LES LINIES
    GET AREA4, 1, CAP             ' AGAFAR CAP?ALERA DEL FITXER DE CAP?ALERES
    LINE INPUT #AREAMSK, CAB$(1)  ' AGAFAR LA CAP?ALERA DELS CAMPS
    LINE INPUT #AREAMSK, CAB$(2)
    LINE INPUT #AREAMSK, CAB$(3)
    CLOSE #AREAMSK
    'Avis.Sonor (1)
    'tecla = Avis("AVIS:", "INSERTI PAPER A LA IMPRESORA", "Piji una tecla...", 0)
    'IF INKEY$ = CHR$(27) THEN EXIT SUB

    PAG = 1: L = 1
    GOSUB CAPSA         ' IMPRIMIR CAP?ALERA

    FOR R = 1 TO CEMPRE.MAXPROVEID - 1
	GET AREA3, R, PROVEIDORS
	IF PROVEIDORS.MARCAT <> "*" THEN
	   IF L > MAXLINS THEN
	      PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(79, "?") + RTRIM$(ARCHIMP.SALTOPAGINA)
	      'PRINT #AREATXT, ""
	      'PRINT #AREATXT, ""
	      'PRINT #AREATXT, ""
	      'PRINT #AREATXT, ""
	      'PRINT #AREATXT, ""
	      'AVIS.SONOR (1)
	      'AVIS "AVIS:", "INSERTI PAPER A LA IMPRESORA", 0
	      L = 1: PAG = PAG + 1
	      GOSUB CAPSA
	   ELSE
	      N$ = RTRIM$(LTRIM$(PROVEIDORS.NOM)) + " " + RTRIM$(LTRIM$(PROVEIDORS.NOM1))
	      NOM$ = MID$(N$, 1, 40)
	      PRINT #AREATXT, USING CAB$(3); PROVEIDORS.CODI; PROVEIDORS.COMPTA; NOM$; PROVEIDORS.DIRECCIO; PROVEIDORS.POBLACIO; PROVEIDORS.TEL1
	      L = L + 1
	   END IF
	END IF
    NEXT

    FOR LA = L TO MAXLINS: PRINT #AREATXT, "": NEXT
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(79, "?") + RTRIM$(ARCHIMP.COMPRIMIDO)
    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT
    CALL ImprimeixFitxerTXT(DIRECCT$ + "PROVEIDO.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP?ALERA DELS LLISTATS
' **************************************************************************

CAPSA:
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, "LLISTAT TOTAL DELS PROVEIDORS"
    PRINT #AREATXT, "P?gina:"; PAG
    PRINT #AREATXT, "Data..: "; FormatD$(Now#, "dd/mm/yyyy")
    PRINT #AREATXT, STRING$(78, "?");
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""
    PRINT #AREATXT, CAB$(1)
    PRINT #AREATXT, CAB$(2)
    RETURN
END SUB

SUB MantenimentProveidors (DIRECCP$, DIRECC$, DIRECI$, DP$, DEVI$, IMPRESORA, MAXLINS) STATIC
    SHARED RNDX

    ON ERROR GOTO ERRORS
    COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "  OBRINT..."
    GOSUB OBRIRFITXERS

    
    DIM NDXCP(1 TO MAXCP) AS INDEXCP
    DIM INDEX(1 TO MAXPR) AS INDEXPROVEIDOR
    DIM NDXProveidor AS INDEXPROVEIDOR
    DIM NDXCODISP AS INDEXCP

    IF MAXPR = 1 THEN
       X = 7: R = 1: L = 1
       BEEP: tecla = Avis("AVIS:", "El fitxer de Prove?dors est? buit.", "Pitjau una tecla...", 0)
       GOSUB INSERTARProveidor
       IF ANUL = 1 THEN GOSUB TANCA
       GOSUB GUARDARINDEX
    END IF
    
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "INDEXANT..."
    GOSUB INDEXAR
    GOSUB INDEXARCP
    CALL SortProveidors(INDEX(), MAXPR)          ' ORDENAR L'INDEX
    CALL SortCPS(NDXCP(), MAXCP)

    MASCCLIS                          ' PINTA LA MASCARA DE LA LLISTA
    X = 7: R = 1: L = 1
    '
    ' ********* MOSTRA ELS REGISTRES QUE CABIGEN EN UNA PANTALLA
    '
    GOSUB LISTA

    X = 7: R = 1                      ' INICIA CURSOR I POSICIONS A PANTALLA
    GET AREA3, R, PROVEIDORS
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
    DO
      OP$ = INKEY$
      SELECT CASE OP$
	     CASE CHR$(0) + "P"              ' BAIXA UN REGISTRE
		  GOSUB BAIXA
	     CASE CHR$(0) + "H"              ' PUJA UN REGISTRE
		  GOSUB PUJA
	     CASE CHR$(27)
		  COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
		  COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT " TANCANT..."
		  GOSUB GUARDARINDEX
		  GOSUB TANCA
	     CASE CHR$(13)
		  GOSUB EDITARProveidor         ' EDITAR Proveidor
	     CASE CHR$(0) + CHR$(60)
		  GOSUB INSERTARProveidor       ' INSERTAR Proveidor
	     CASE CHR$(0) + CHR$(62)
		  GOSUB CONSULTARProveidor      ' CONSULTAR Proveidor
	     CASE CHR$(0) + CHR$(63)
		  GOSUB DELETERECORD         ' MARCAR REGISTRE
	     CASE CHR$(0) + CHR$(64)
		  CALL LISTCLI(AREA4, AREA3, DP$, DEVI$, MAXLINS)' LLISTAR ProveidorS
	     CASE CHR$(0) + "Q"
		  GOSUB BOTA.AVALL
	     CASE CHR$(0) + "I"
		  GOSUB BOTA.AMUNT
	     CASE ELSE
		  LOCATE , , 0
		  COLOR COL(0, 0), COL(0, 1): LOCATE 24, 60: PRINT "Registre: "; R; "  ";
      END SELECT
   LOOP
   RETURN

BOTA.AVALL:
   IF R = MAXPR - 1 THEN RETURN
   GOSUB ANAR.TOPE
   S = 1
   DO
	S = S + 1
	IF RB + S > MAXPR - 1 THEN
	   WHILE (RB + S > MAXPR - 1)
		 S = S - 1
	   WEND
	   EXIT DO
	END IF
   LOOP UNTIL S = 13
   RB = RB + S: R = RB: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREA3, RB, PROVEIDORS: X = XB
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
   GET AREA3, RB, PROVEIDORS: X = XB
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
' Control del cursor per el llistat amb despla?ament de barres
' funci? parescuda al DBEDIT de Clipper.
'************************************************************************

PUJA:
       IF X = 7 THEN
	  GET AREA3, R, PROVEIDORS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = 1 THEN
	     X = 7: R = 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 7
	  ScrollDown 18, 78, 6, 1, 1
	  GET AREA3, R, PROVEIDORS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA3, R, PROVEIDORS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  GET AREA3, R, PROVEIDORS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXA:
       IF X = 19 THEN
	  GET AREA3, R, PROVEIDORS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXPR - 1 THEN
	      R = MAXPR - 1: X = 19
	      COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR: GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = 19
	  ScrollUp 18, 78, 6, 1, 1
	  GET AREA3, R, PROVEIDORS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA3, R, PROVEIDORS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXPR - 1 THEN
	     R = MAXPR - 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  GET AREA3, R, PROVEIDORS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

'************************************************************************
' Mostra el cursor a la posici? on est?
'************************************************************************

SHOWCURSOR:
	  LOCATE X, 2: PRINT PROVEIDORS.CODI; " "; PROVEIDORS.NOM; "                    "; PROVEIDORS.COMPTA; PROVEIDORS.MARCAT; "         "
	  RETURN

'************************************************************************
' CONSUTAR REGISTRES UN PER UN MOSTRANT LA FITXA
' DEL Proveidor
'************************************************************************

CONSULTARProveidor:
	  GetBackground 1, 1, 24, 79, cli$
	  GOSUB INITCAMPS
	  SetMaxCamps 14

	  FOR C = 0 TO 14: SetColorCamp C, COL(0, 1), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  FOR C = 0 TO 14: DeleteCamp C: NEXT

	  RV = R
	  GET AREA3, R, PROVEIDORS
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
			FOR C = 0 TO 14: DeleteCamp C: NEXT
			RETURN
		   CASE ELSE
	    END SELECT
	  LOOP
BXA:
	  RV = RV + 1
	  GOSUB MOSTRA
	  COLOR COL(2, 0), COL(2, 1): LOCATE 5, 11: PRINT "Proveidor No.: "; :  COLOR COL(0, 0), COL(0, 1): PRINT RV
	  RETURN
PJA:
	  RV = RV - 1
	  GOSUB MOSTRA
	  COLOR COL(2, 0), COL(2, 1): LOCATE 5, 11: PRINT "Proveidor No.: "; :  COLOR COL(0, 0), COL(0, 1): PRINT RV
	  RETURN

MOSTRA:
	  IF RV <= 1 THEN RV = 1
	  IF RV >= MAXPR - 1 THEN RV = MAXPR - 1
	  GET AREA3, RV, PROVEIDORS: GOSUB MOURECAMPS
	  COLOR COL(0, 0), COL(0, 1)
	  DisplayAllCamps

	  IF PROVEIDORS.MARCAT = "*" THEN
	     LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "<< ESTA DE BAIXA >>"
	  ELSE
	     LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "                   "
	  END IF
	  RETURN

'************************************************************************
' INSERTAR Proveidor
'************************************************************************

INSERTARProveidor:
	  ' MIRA A VEURE SI HI HA ALGUN REGISTRE MARCAT PER
	  ' BORRAR-LO
	  TANT% = 0: TROBAT = 0: RVELL = R: T$ = "   "
	  FOR RR = 1 TO MAXPR - 1
	      GET AREA3, RR, PROVEIDORS
	      TANT% = 100 / MAXPR * RR
	      COLOR 27, COL(0, 1): LOCATE 5, 40: PRINT "Processant..."; : COLOR COL(0, 0): PRINT TANT%; "% "
	      IF PROVEIDORS.MARCAT = "*" THEN
		 RT = RR: T$ = "ZZZ"
		 EXIT FOR
	      END IF
	  NEXT
	  LOCATE 5, 40: PRINT "                   "
	  IF T$ = "ZZZ" THEN
	     RNOU = RR                      ' SI N'HA TROBAT QUE L'EDITI D'AMUNT
	  ELSE
	     REF$ = DOC.REF$
	     ALB$ = LTRIM$(STR$(DOC.MAX%))
	     L2 = 10
	     FOR L = LEN(ALB$) TO 1 STEP -1
		 MID$(REF$, L2, 1) = MID$(ALB$, L, 1): L2 = L2 - 1
	     NEXT
	     RNOU = MAXPR                   ' SI NO N'HA TROBAT QUE N'AFEGESQUI UN
	  END IF
					
	  GetBackground 1, 1, 24, 79, clis$

	  RV = RNOU                         ' DESIGNAR EL REGISTRE NOU
	  GOSUB MASCARA
	  SetMaxCamps 14
	  FOR C = 0 TO 14: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  
	  GOSUB INITCAMPS: COLOR 15, 9
	  GOSUB BUIDARCAMPS                 ' BUIDAR CAMPS
	  InsertValueCamp 0, REF$
	  DisplayAllCamps
	  GOSUB READCAMPS                   ' INTRODU?R DADES ALS CAMPS
	  GOSUB MOUREFITXER                 ' MOURE LES DADES DELS CAMPS AL FITXER
	  DisplayAllCamps

	  IF ANUL = 1 THEN                  ' SI ES PITJA <ESC> A UN DELS CAMPS
	     PutBackground 1, 1, clis$
	     FOR C = 0 TO 14: DeleteCamp C: NEXT
	     RETURN
	  END IF
	  
	  PUT AREA3, RNOU, PROVEIDORS          ' GRAVAR DINS EL FITXER

	  IF RNOU = MAXPR THEN              ' SI EL REGISTRE NOU ES IGUAL AL NOMBRE M?XIM DE ProveidorS
	     CEMPRE.MAXPROVEID = MAXPR + 1
	     DOCNUM.MAXPROV = DOC.MAX% + 1
	     MAXPR = MAXPR + 1
	     DOCMAX% = DOCMAX% + 1
	     PUT AREA, 1, CEMPRE
	     PUT AREAD, 1, DOCNUM
	  END IF

	  ' ACTUALITZA L'INDEX
	  RNDX = RNOU: GOSUB ACTUALIZ.INDEX
	  GOSUB GUARDARINDEX
	  PutBackground 1, 1, clis$
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR: FOR C = 0 TO 14: DeleteCamp C: NEXT
	  RETURN

'************************************************************************
' EDITAR Proveidor
'************************************************************************

EDITARProveidor:
	  GetBackground 1, 1, 24, 79, cli$
	  RV = R
	  GOSUB MASCARA
	  LOCK AREA3, RV
	  GET AREA3, R, PROVEIDORS
	  SetMaxCamps 14
	  FOR C = 0 TO 14: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  GOSUB INITCAMPS
	  GOSUB MOURECAMPS
	  COLOR 15, 9: CALL DisplayAllCamps: EDIT = 99
	  GOSUB READCAMPS
	  EDIT = 0
	  IF ANUL = 1 THEN           ' SI ES PITJA <ESC> A UN DELS CAMPS
	     PutBackground 1, 1, cli$
	     UNLOCK AREA3, RV
	     FOR C = 0 TO 14: DeleteCamp C: NEXT
	     RETURN
	  END IF
	  GOSUB MOUREFITXER          ' MOURE LES DADES DELS CAMPS AL FITXER
	  PUT AREA3, RV, PROVEIDORS     ' GRAVA DINS EL FITXER
	  
TORNAR:
	  FOR C = 0 TO 14: DeleteCamp C: NEXT
	  PutBackground 1, 1, cli$
	  UNLOCK AREA3, RV
	  RETURN


'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

READCAMPS:
       CORRECTE = 0: ANUL = 0
       GOSUB INDEXAR: SetMaxCamps 14
       DisplayAllCamps
       DO
	  FOR C = 0 TO 14
	     VALUE = ReadCamp(C)
	     SELECT CASE VALUE
		    CASE 0
			  CAMPTEMP$ = ForaEspai$(ValueCamp$(0))
			  IF FindProveidor%(ForaEspai$(CAMPTEMP$), INDEX(), MAXPR) THEN
			     IF PROVEIDORS.MARCAT <> "*" THEN
				LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "<< AQUEST CODI EXISTEIX >>"
				BEEP
				LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "                          "
				C = C - 1
			     END IF
			  END IF
		    CASE 5
			  CAMPTEMP$ = ForaEspai$(ValueCamp$(5))
			  IF FindCP%(NDXCP(), ForaEspai$(CAMPTEMP$), MAXCP) THEN
			     IF CODISP.MARCAT <> "*" THEN
				InsertValueCamp 5, CODISP.POBLE
				InsertValueCamp 13, CODISP.PROVINCIA
				InsertValueCamp 10, CODISP.PAIS
			     END IF
			  END IF
			  DisplayAllCamps
		    CASE 6
			  CAMPTEMP$ = UCASE$(ForaEspai$(ValueCamp$(6)))
			  FOR RK = 1 TO MAXCP
			      GET AREA13, RK, CODISP
			      IF CAMPTEMP$ = UCASE$(ForaEspai$(CODISP.POBLE)) THEN
				 InsertValueCamp 5, CODISP.CODI
				 InsertValueCamp 13, CODISP.PROVINCIA
				 InsertValueCamp 10, CODISP.PAIS
			      END IF
			  NEXT
			  DisplayAllCamps
		    CASE 999
			 ANUL = 1
			 RETURN
		    CASE F1 TO F10
			 C = C - 1
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
	  FOR C = 0 TO 14: DeleteCamp C: NEXT
	  SetInitCamp 0, 7, 28, ASCI, 0, "XXXXXXX", "Codi prove?dor:"
	  SetInitCamp 1, 8, 28, ASCI, 0, "XXXXXXXXXXXXXXXXXXXX", "Nom:"
	  SetInitCamp 2, 8, 58, ASCI, 0, "XXXXXXXXXXX", "N.I.F.:"
	  SetInitCamp 3, 9, 28, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", "Nom 2:"
	  SetInitCamp 4, 11, 28, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", "Direcci?:"
	  SetInitCamp 5, 12, 28, ASCI, 0, "XXXXXXX", "Codi Postal:"
	  SetInitCamp 6, 12, 48, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXX", "Poblaci?:"
	  SetInitCamp 7, 13, 28, ASCI, 0, "XXXXXXXXXXXXXXX", "Tel?fon 1:"
	  SetInitCamp 8, 13, 57, ASCI, 0, "XXXXXXXXXXXXXXX", "Tel?fon 2:"
	  SetInitCamp 9, 14, 28, ASCI, 0, "XXXXXXXXXXXXXXX", "Tel?fon 3:"
	  SetInitCamp 10, 14, 57, ASCI, 0, "XXXXXXXXXX", "Pa?s:"
	  SetInitCamp 11, 15, 28, ASCI, 0, "XXXXXXXXXXXXXXX", "Fax 1:"
	  SetInitCamp 12, 15, 57, ASCI, 0, "XXXXXXXXXXXXXXX", "Fax 2:"
	  SetInitCamp 13, 16, 28, ASCI, 0, STRING$(20, "X"), "Prov?ncia:"
	  SetInitCamp 14, 18, 28, ASCI, 0, "XXXXXXXXXX", "COMPTE:"
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPS:
	   InsertValueCamp 0, PROVEIDORS.CODI
	   InsertValueCamp 1, PROVEIDORS.NOM
	   InsertValueCamp 2, PROVEIDORS.NIF
	   InsertValueCamp 3, PROVEIDORS.NOM1
	   InsertValueCamp 4, PROVEIDORS.DIRECCIO
	   InsertValueCamp 5, PROVEIDORS.CPOSTAL
	   InsertValueCamp 6, PROVEIDORS.POBLACIO
	   InsertValueCamp 7, PROVEIDORS.TEL1
	   InsertValueCamp 8, PROVEIDORS.TEL2
	   InsertValueCamp 9, PROVEIDORS.TEL3
	   InsertValueCamp 10, PROVEIDORS.PAIS
	   InsertValueCamp 11, PROVEIDORS.FAX1
	   InsertValueCamp 12, PROVEIDORS.FAX2
	   InsertValueCamp 13, PROVEIDORS.PROVINCIA
	   InsertValueCamp 14, PROVEIDORS.COMPTA
	   RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXER:
	   PROVEIDORS.CODI = RTRIM$(ValueCamp$(0))
	   PROVEIDORS.NOM = ValueCamp$(1)
	   PROVEIDORS.NIF = ValueCamp$(2)
	   PROVEIDORS.NOM1 = ValueCamp$(3)
	   PROVEIDORS.DIRECCIO = ValueCamp$(4)
	   PROVEIDORS.CPOSTAL = ValueCamp$(5)
	   PROVEIDORS.POBLACIO = ValueCamp$(6)
	   PROVEIDORS.TEL1 = ValueCamp$(7)
	   PROVEIDORS.TEL2 = ValueCamp$(8)
	   PROVEIDORS.TEL3 = ValueCamp$(9)
	   PROVEIDORS.PAIS = ValueCamp$(10)
	   PROVEIDORS.FAX1 = ValueCamp$(11)
	   PROVEIDORS.FAX2 = ValueCamp$(12)
	   PROVEIDORS.PROVINCIA = ValueCamp$(13)
	   PROVEIDORS.COMPTA = ValueCamp$(14)
	   PROVEIDORS.MARCAT = "-"
	   RETURN

'************************************************************************
' BUIDAR ELS CAMPS DEL FITXER
'************************************************************************
BUIDARCAMPS:
	   PROVEIDORS.CODI = STRING$(LEN(PROVEIDORS.CODI), CHR$(32))
	   PROVEIDORS.NOM = ""
	   PROVEIDORS.NIF = ""
	   PROVEIDORS.NOM1 = ""
	   PROVEIDORS.DIRECCIO = ""
	   PROVEIDORS.CPOSTAL = ""
	   PROVEIDORS.POBLACIO = ""
	   PROVEIDORS.TEL1 = ""
	   PROVEIDORS.TEL2 = ""
	   PROVEIDORS.TEL3 = ""
	   PROVEIDORS.PAIS = ""
	   PROVEIDORS.FAX1 = ""
	   PROVEIDORS.FAX2 = ""
	   PROVEIDORS.COMPTA = ""
	   PROVEIDORS.MARCAT = "-"

	   FOR C = 0 TO 14: DeleteCamp C: NEXT
	   RETURN

'************************************************************************
' FITXA DE UN Proveidor
'************************************************************************

MASCARA:
	  COLOR COL(0, 0), COL(0, 1)
	  FINESTRA 4, 10, 20, 76, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
	  LOCATE 5, 11: PRINT "Prove?dor No.: "; :  COLOR COL(0, 0), COL(0, 1): PRINT RV
	  LOCATE 6, 11: PRINT STRING$(65, "?"); : COLOR COL(2, 0), COL(2, 1)
	  COLOR COL(0, 0), COL(0, 1): LOCATE 10, 11: PRINT STRING$(65, "?"); :  COLOR COL(2, 0), COL(2, 1)
	  RETURN

'************************************************************************
' LLISTA UN BLOC
'************************************************************************

LISTA:
    FOR X = 7 TO 19
	COLOR COL(0, 0), COL(0, 1)
	LOCATE X, 2: PRINT STRING$(78, " ");
    NEXT
    FOR X = 7 TO 19
       IF R = MAXPR THEN EXIT FOR
       GET AREA3, R, PROVEIDORS
       COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
       R = R + 1
    NEXT
    RETURN
'************************************************************************
' MARCAR UN REGISTRE COM A BORRAT
'************************************************************************

DELETERECORD:
    GET AREA3, R, PROVEIDORS              ' CARREGAR EL REGISTRE DINS MEMORIA
    IF PROVEIDORS.MARCAT = "*" THEN       ' COMPROVAR SI ESTA MARCAT
       PROVEIDORS.MARCAT = "-"
       PUT AREA3, R, PROVEIDORS
    ELSE                               ' SI NO ESTA MARCAT QUE EL MARQUI
      IF PROVEIDORS.MARCAT <> "*" THEN
	 PROVEIDORS.MARCAT = "*"
	 PUT AREA3, R, PROVEIDORS
      END IF
    END IF
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR     ' TORNA A VISUALITZAR LA LINIA
    RETURN

'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE ProveidorS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "PROVEID.NDX" FOR RANDOM SHARED AS AREA2 LEN = LEN(NDXProveidor)
      AREA3 = FREEFILE: OPEN DIRECC$ + "PROVEID.DAT" FOR RANDOM SHARED AS AREA3 LEN = LEN(PROVEIDORS)
      AREA12 = FREEFILE: OPEN DIRECC$ + "CODISP.NDX" FOR RANDOM SHARED AS AREA12 LEN = LEN(NDXCODISP)
      AREA13 = FREEFILE: OPEN DIRECC$ + "CODISP.DAT" FOR RANDOM SHARED AS AREA13 LEN = LEN(CODISP)
      AREA4 = FREEFILE: OPEN DP$ + "PLANTILL\CAP?ALER.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CAP)
      AREAD = FREEFILE: OPEN DIRECC$ + "DOCUMENT.NUM" FOR RANDOM AS AREAD LEN = LEN(DOCNUM)

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
      GET AREAD, 1, DOCNUM
      MAXPR = CEMPRE.MAXPROVEID
      MAXCP = LOF(AREA13) \ LEN(CODISP)
      DOC.REF$ = DOCNUM.NUMPROV
      DOC.MAX% = DOCNUM.MAXPROV
      RETURN

INDEXAR:
      FOR RI = 1 TO MAXPR                   ' COL.LOCAR A MEM?RIA L'INDEX I LA CLAU
	  GET AREA2, RI, NDXProveidor
	  INDEX(RI).REGISTRE = NDXProveidor.REGISTRE
	  INDEX(RI).CODI = NDXProveidor.CODI
      NEXT
      'GOSUB TANCA
      RETURN

INDEXARCP:
      FOR RI = 1 TO MAXCP                   ' COL.LOCAR A MEM?RIA L'INDEX I LA CLAU
	  GET AREA2, RI, NDXCODISP
	  NDXCP(RI).REGISTRE = NDXCODISP.REGISTRE
	  NDXCP(RI).CODI = NDXCODISP.CODI
      NEXT
      'GOSUB TANCA
      RETURN

GUARDARINDEX:
      CALL SortProveidors(INDEX(), MAXPR)         ' ORDENAR L'INDEX
      FOR RI = 1 TO MAXPR                      ' COL.LOCAR A MEM?RIA L'INDEX I LA CLAU
	  NDXProveidor.REGISTRE = INDEX(RI).REGISTRE
	  NDXProveidor.CODI = INDEX(RI).CODI
	  PUT AREA2, RI, NDXProveidor
      NEXT
      RETURN

TANCA:
      GOSUB GUARDARINDEX
      FOR C = 0 TO 14: DeleteCamp C: NEXT
      ERASE INDEX, CAMPS
      RESET
      EXIT SUB

ACTUALIZ.INDEX:
      REDIM PRESERVE INDEX(1 TO MAXPR) AS INDEXPROVEIDOR
      INDEX(RNDX).REGISTRE = RNDX
      INDEX(RNDX).CODI = PROVEIDORS.CODI
      RETURN
END SUB

SUB MASCCLIS
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 4, 1, 20, 80, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
    LOCATE 5, 2: PRINT "Codi    Nom prove?dor" + SPACE$(35) + "Compte      Baixa": COLOR COL(0, 0), COL(0, 1)
    LOCATE 6, 2: PRINT STRING$(78, "?");
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR PROVEIDORS <F2>=INSERTAR PROVEIDORS  <" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR                 <F3>=SUMAR                <F4>=CONSULTAR"
    LOCATE 24, 2: PRINT "<F5>=MARCAR PROVEIDORS       <F6>=LLISTAR";
END SUB

SUB SortCPS (INDEX() AS INDEXCP, MAXCP) STATIC

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

SUB SortProveidors (INDEX() AS INDEXCLI, MAXPR) STATIC
    
    OFFSET = MAXPR - 1 \ 2

    DO WHILE OFFSET > 0
       LIMIT = MAXPR - 1 - OFFSET
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

