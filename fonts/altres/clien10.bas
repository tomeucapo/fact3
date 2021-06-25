' ********************************************************************
'
' Fitxer....................: PRO_001.BAS
' Titol.....................: Modul per el manteniment de proveidors
'
' ********************************************************************
'
' Data inici................: 16/09/1997 13:31:00
' Data de la darrera revisi¢:
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB SortCPS (INDEX() AS ANY, MAXCP!)
DECLARE SUB MantenimentClients (DIRECCP$, DIRECC$, DIRECI$, DP$, DEVI$, IMPRESORA!, MAXLINS!)
DECLARE SUB LISTCLI (AREA4!, AREA3!, DIRECC$, DEVI$, MAXLINS!)
DECLARE SUB MASCLIST ()
DECLARE FUNCTION FindClient% (CAMP$, INDEX() AS ANY, MAXCL!)
DECLARE SUB MASCCLIS ()
DECLARE SUB SortClients (INDEX() AS ANY, MAXCL!)
DECLARE FUNCTION FindCP% (INDEX() AS ANY, CAMP$, MAXCP!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'c:\FACT3\FONTS\CP.BI'
'$INCLUDE: 'c:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'c:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'c:\FACT3\FONTS\VIDEOF.BI'
'$INCLUDE: 'c:\FACT3\FONTS\CAMPS.BI'

COMMON SHARED DIRECC$, DIRCP$, DEVI$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT, RNDX

DIM SHARED CFG AS CONFIG
DIM SHARED EMPRES AS EMPRESA
DIM SHARED CTRL AS CONTROL
DIM SHARED CEMPRE AS CONTROLEMP       '     "      DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAP€ALERAS DE DOCUMENTS
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED CLIENTS AS CLIENT
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

	  IF LTRIM$(RTRIM$(CA2$)) <> "CLIENTS.EXE" THEN
	     AVIS "ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", 0
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


      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, R, ANYS

      DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"
      CLOSE AREAA

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

      CALL MantenimentClients(DIRECCP$, DIRECCF$, DIRECCI$, DBF$, DEV$, IMPRESORA, MAXLINS)
      SYSTEM

ERRORS:
       LOCATE 24, 2: PRINT STRING$(78, " ");
       SELECT CASE ERR
	      CASE 70
		   MissatgeError ERR, "REGISTRE BLOQUETJAT PER UN ALTRE USUARI"
	      CASE ELSE
       END SELECT
       COLOR 15, 9: LOCATE 24, 2: PRINT STRING$(78, " ");
       LOCATE 24, 2: PRINT "<F5>=MARCAR CLIENTS       <F6>=LLISTAR";
       RESUME NEXT

FUNCTION FindClient% (CAMP$, INDEX() AS INDEXCLI, MAXCL) STATIC
	 SHARED RNDX
	 TOPRECORD = MAXCL - 1
	 BottomRecord = 1

	 DO UNTIL (TOPRECORD < BottomRecord)
	    
	    MIDPOINT = (TOPRECORD + BottomRecord) \ 2

	    TEST$ = RTRIM$(FORAESPAI$(INDEX(MIDPOINT).CODI))

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
	    RNDX = MIDPOINT
	 ELSE
	    FindClient% = FALSE
	 END IF
END FUNCTION

FUNCTION FindCP% (INDEX() AS INDEXCP, CAMP$, MAXCP)
	 SHARED RNDX
	 TOPRECORD = MAXCP
	 BottomRecord = 1

	 DO UNTIL (TOPRECORD < BottomRecord)
	    MIDPOINT = (TOPRECORD + BottomRecord) \ 2
	    TEST$ = RTRIM$(FORAESPAI$(INDEX(MIDPOINT).CODI))

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

SUB LISTCLI (AREA4, AREA3, DIRECC$, DEVI$, MAXLINS)
    SHARED DIRECCT$, DIRECCM$
    DIM CAB$(3)
    DIRECCT$ = DIRECC$ + "TEXTOS\"

    AREAMSK = FREEFILE: OPEN DIRECC$ + "PLANTILL\CLIENTS.MSK" FOR INPUT SHARED AS AREAMSK
    AREATXT = FREEFILE: OPEN DIRECCT$ + "CLIENTS.TXT" FOR OUTPUT SHARED AS AREATXT
    
    ' DEFINIR MASCARA DE LES LINIES
    GET AREA4, 1, CAP             ' AGAFAR CAP€ALERA DEL FITXER DE CAP€ALERES
    LINE INPUT #AREAMSK, CAB$(1)  ' AGAFAR LA CAP€ALERA DELS CAMPS
    LINE INPUT #AREAMSK, CAB$(2)
    LINE INPUT #AREAMSK, CAB$(3)
    CLOSE #AREAMSK
    AVIS.SONOR (1)
    AVIS "AVIS:", "INSERTI PAPER A LA IMPRESORA", 0
    IF INKEY$ = CHR$(27) THEN EXIT SUB

    PAG = 1: L = 1
    GOSUB CAPSA         ' IMPRIMIR CAP€ALERA

    FOR R = 1 TO CEMPRE.MAXCLIENTS - 1
	GET AREA3, R, CLIENTS
	IF CLIENTS.MARCAT <> "*" THEN
	   IF L > MAXLINS THEN
	      PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(79, "Ä") + RTRIM$(ARCHIMP.SALTOPAGINA)
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
	      N$ = RTRIM$(LTRIM$(CLIENTS.NOM)) + " " + RTRIM$(LTRIM$(CLIENTS.COGNOMS))
	      NOM$ = MID$(N$, 1, 40)
	      PRINT #AREATXT, USING CAB$(3); CLIENTS.CODICLIENT; NOM$; CLIENTS.DIRECCIO; CLIENTS.POBLACIO; CLIENTS.TELEFON1
	      L = L + 1
	   END IF
	END IF
    NEXT

    FOR LA = L TO MAXLINS: PRINT #AREATXT, "": NEXT
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(79, "Ä") + RTRIM$(ARCHIMP.COMPRIMIDO)
    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT
    CALL ImprimeixFitxerTXT(DIRECCT$ + "CLIENTS.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP€ALERA DELS LLISTATS
' **************************************************************************

CAPSA:
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, "LLISTAT TOTAL DELS CLIENTS"
    PRINT #AREATXT, "P…gina:"; PAG
    PRINT #AREATXT, "Data..: "; FETXA$
    PRINT #AREATXT, STRING$(78, "Ä");
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""
    PRINT #AREATXT, CAB$(1)
    PRINT #AREATXT, CAB$(2)
    RETURN
END SUB

SUB MantenimentClients (DIRECCP$, DIRECC$, DIRECI$, DP$, DEVI$, IMPRESORA, MAXLINS) STATIC
    SHARED RNDX

    ON ERROR GOTO ERRORS
    COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "  OBRINT..."
    GOSUB OBRIRFITXERS

    
    DIM NDXCP(1 TO MAXCP) AS INDEXCP
    DIM INDEX(1 TO MAXCL) AS INDEXCLI
    DIM NDXCLIENT AS INDEXCLI
    DIM NDXCODISP AS INDEXCP

    IF MAXCL = 1 THEN
       X = 7: R = 1: L = 1
       BEEP: AVIS "AVIS:", "El fitxer de clients est… buit.", 0
       GOSUB INSERTARCLIENT
       IF ANUL = 1 THEN GOSUB TANCA
       GOSUB GUARDARINDEX
    END IF
    
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "INDEXANT..."
    GOSUB INDEXAR
    GOSUB INDEXARCP
    CALL SortClients(INDEX(), MAXCL)          ' ORDENAR L'INDEX
    CALL SortCPS(NDXCP(), MAXCP)

    MASCCLIS                          ' PINTA LA MASCARA DE LA LLISTA
    X = 7: R = 1: L = 1
    '
    ' ********* MOSTRA ELS REGISTRES QUE CABIGEN EN UNA PANTALLA
    '
    GOSUB LISTA

    X = 7: R = 1                      ' INICIA CURSOR I POSICIONS A PANTALLA
    GET AREA3, R, CLIENTS
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
		  GOSUB EDITARCLIENT         ' EDITAR CLIENT
	     CASE CHR$(0) + CHR$(60)
		  GOSUB INSERTARCLIENT       ' INSERTAR CLIENT
	     CASE CHR$(0) + CHR$(62)
		  GOSUB CONSULTARCLIENT      ' CONSULTAR CLIENT
	     CASE CHR$(0) + CHR$(63)
		  GOSUB DELETERECORD         ' MARCAR REGISTRE
	     CASE CHR$(0) + CHR$(64)
		  CALL LISTCLI(AREA4, AREA3, DP$, DEVI$, MAXLINS)' LLISTAR CLIENTS
	     CASE CHR$(0) + "Q"
		  IF R > MAXCL - 1 THEN
		     R = MAXCL - 1
		     X = 7: GET AREA3, R, CLIENTS
		     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
		  ELSE
		     R = R + 13
		     GOSUB LISTA
		     X = 7: GET AREA3, R, CLIENTS
		     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
		  END IF
	     CASE CHR$(0) + "I"
		  IF R = 1 THEN
		     R = 1
		     X = 7: GET AREA3, R, CLIENTS
		     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
		  ELSE
		     R = R - 13
		     GOSUB LISTA: X = 7: GET AREA3, R, CLIENTS
		     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
		  END IF
	     CASE ELSE
		  LOCATE , , 0
		  COLOR COL(0, 0), COL(0, 1): LOCATE 24, 60: PRINT "Registre: "; R; "  ";
      END SELECT
   LOOP
   RETURN

'************************************************************************
' Control del cursor per el llistat amb despla‡ament de barres
' funci¢ parescuda al DBEDIT de Clipper.
'************************************************************************

PUJA:
       IF X = 7 THEN
	  GET AREA3, R, CLIENTS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = 1 THEN
	     X = 7: R = 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 7
	  SCROLLDOWN 18, 78, 6, 1, 1
	  GET AREA3, R, CLIENTS
	  
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA3, R, CLIENTS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  GET AREA3, R, CLIENTS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXA:
       IF X = 19 THEN
	  GET AREA3, R, CLIENTS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXCL - 1 THEN
	      R = MAXCL - 1: X = 19
	      COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR: GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = 19
	  SCROLLUP 18, 78, 6, 1, 1
	  GET AREA3, R, CLIENTS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA3, R, CLIENTS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXCL - 1 THEN
	     R = MAXCL - 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  GET AREA3, R, CLIENTS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

'************************************************************************
' Mostra el cursor a la posici¢ que est…
'************************************************************************

SHOWCURSOR:
	  
	  LOCATE X, 2: PRINT CLIENTS.CODICLIENT; " "; CLIENTS.NOM; " "; CLIENTS.COGNOMS; "       "; CLIENTS.MARCAT; "        "
	  RETURN

'************************************************************************
' CONSUTAR REGISTRES UN PER UN MOSTRANT LA FITXA
' DEL CLIENT
'************************************************************************

CONSULTARCLIENT:
	  GetBackground 1, 1, 24, 79, cli$
	  GOSUB INITCAMPS
	  SETMAXCAMPS 13

	  FOR C = 0 TO 13: SETCOLORCAMPS C, COL(0, 1), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  FOR C = 0 TO 13: ERASEFIELD C: NEXT

	  RV = R
	  GET AREA3, R, CLIENTS
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
			'IF TESTSCREEN = &HB000 THEN RESTORESCREEN DIRECCP$ + "CLIENT.SCR", 0 ELSE CALL RESTSCRN
			FOR C = 0 TO 13: ERASEFIELD C: NEXT
			RETURN
		   CASE ELSE
	    END SELECT
	  LOOP
BXA:
	  RV = RV + 1
	  GOSUB MOSTRA
	  COLOR COL(2, 0), COL(2, 1): LOCATE 5, 11: PRINT "Client No.: "; :  COLOR COL(0, 0), COL(0, 1): PRINT RV
	  RETURN
PJA:
	  RV = RV - 1
	  GOSUB MOSTRA
	  COLOR COL(2, 0), COL(2, 1): LOCATE 5, 11: PRINT "Client No.: "; :  COLOR COL(0, 0), COL(0, 1): PRINT RV
	  RETURN

MOSTRA:
	  IF RV <= 1 THEN RV = 1
	  IF RV >= MAXCL - 1 THEN RV = MAXCL - 1
	  GET AREA3, RV, CLIENTS: GOSUB MOURECAMPS
	  COLOR COL(0, 0), COL(0, 1)
	  DISPLAYALL

	  IF CLIENTS.MARCAT = "*" THEN
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
	  FOR RR = 1 TO MAXCL - 1
	      GET AREA3, RR, CLIENTS
	      TANT% = 100 / MAXCL * RR
	      COLOR 27, COL(0, 1): LOCATE 5, 40: PRINT "Processant..."; : COLOR COL(0, 0): PRINT TANT%; "% "
	      IF CLIENTS.MARCAT = "*" THEN
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
	     RNOU = MAXCL                   ' SI NO N'HA TROBAT QUE N'AFEGESQUI UN
	  END IF
					
	  GetBackground 1, 1, 24, 79, clis$

	  RV = RNOU                         ' DESIGNAR EL REGISTRE NOU
	  GOSUB MASCARA
	  SETMAXCAMPS 13
	  FOR C = 0 TO 13: SETCOLORCAMPS C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  
	  GOSUB INITCAMPS: COLOR 15, 9
	  GOSUB BUIDARCAMPS                 ' BUIDAR CAMPS
	  INSERTCAMP 0, REF$
	  GOSUB READCAMPS                   ' INTRODU‹R DADES ALS CAMPS
	  GOSUB MOUREFITXER                 ' MOURE LES DADES DELS CAMPS AL FITXER
	  DISPLAYALL

	  IF ANUL = 1 THEN                  ' SI ES PITJA <ESC> A UN DELS CAMPS
	     PutBackground 1, 1, clis$
	     FOR C = 0 TO 13: ERASEFIELD C: NEXT
	     RETURN
	  END IF
	  
	  PUT AREA3, RNOU, CLIENTS          ' GRAVAR DINS EL FITXER

	  IF RNOU = MAXCL THEN              ' SI EL REGISTRE NOU ES IGUAL AL NOMBRE M…XIM DE CLIENTS
	     CEMPRE.MAXCLIENTS = MAXCL + 1
	     DOCNUM.MAXCLIS = DOC.MAX% + 1
	     MAXCL = MAXCL + 1
	     DOCMAX% = DOCMAX% + 1
	     PUT AREA, 1, CEMPRE
	     PUT AREAD, 1, DOCNUM
	  END IF

	  ' ACTUALITZA L'INDEX
	  RNDX = RNOU: GOSUB ACTUALIZ.INDEX
	  GOSUB GUARDARINDEX
	  PutBackground 1, 1, clis$
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR: FOR C = 0 TO 13: ERASEFIELD C: NEXT
	  RETURN

'************************************************************************
' EDITAR CLIENT
'************************************************************************

EDITARCLIENT:
	  GetBackground 1, 1, 24, 79, cli$
	  RV = R
	  GOSUB MASCARA
	  LOCK AREA3, RV
	  GET AREA3, R, CLIENTS
	  SETMAXCAMPS 13
	  FOR C = 0 TO 13: SETCOLORCAMPS C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  GOSUB INITCAMPS
	  GOSUB MOURECAMPS
	  COLOR 15, 9: CALL DISPLAYALL: EDIT = 99
	  GOSUB READCAMPS
	  EDIT = 0
	  IF ANUL = 1 THEN           ' SI ES PITJA <ESC> A UN DELS CAMPS
	     PutBackground 1, 1, cli$
	     UNLOCK AREA3, RV
	     FOR C = 0 TO 13: ERASEFIELD C: NEXT
	     RETURN
	  END IF
	  GOSUB MOUREFITXER          ' MOURE LES DADES DELS CAMPS AL FITXER
	  PUT AREA3, RV, CLIENTS     ' GRAVA DINS EL FITXER
	  
TORNAR:
	  FOR C = 0 TO 13: ERASEFIELD C: NEXT
	  PutBackground 1, 1, cli$
	  UNLOCK AREA3, RV
	  RETURN


'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

READCAMPS:
       CORRECTE = 0: ANUL = 0
       GOSUB INDEXAR: SETMAXCAMPS 13
       DO
	  FOR C = 0 TO 13
	     VALUE = LLEGIRCAMP(C)
	     SELECT CASE VALUE
		    CASE 0
			  CAMPTEMP$ = FORAESPAI$(SHOWFIELD$(0))
			  IF FindClient%(FORAESPAI$(CAMPTEMP$), INDEX(), MAXCL) THEN
			     IF CLIENTS.MARCAT <> "*" THEN
				LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "<< AQUEST CODI EXISTEIX >>"
				BEEP
				LOCATE 5, 50: COLOR 28, COL(0, 1): PRINT "                          "
				C = C - 1
			     END IF
			  END IF
		    CASE 5
			  CAMPTEMP$ = FORAESPAI$(SHOWFIELD$(5))
			  IF FindCP%(NDXCP(), FORAESPAI$(CAMPTEMP$), MAXCP) THEN
			     IF CODISP.MARCAT <> "*" THEN
				INSERTCAMP 6, CODISP.POBLE
				INSERTCAMP 9, CODISP.PROVINCIA
			     END IF
			  END IF
			  DISPLAYALL
		    CASE 6
			  CAMPTEMP$ = UCASE$(FORAESPAI$(SHOWFIELD$(6)))
			  FOR RK = 1 TO MAXCP
			      GET AREA13, RK, CODISP
			      IF CAMPTEMP$ = UCASE$(FORAESPAI$(CODISP.POBLE)) THEN
				 INSERTCAMP 5, CODISP.CODI
				 INSERTCAMP 9, CODISP.PROVINCIA
			      END IF
			  NEXT
			  DISPLAYALL
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
	  FOR C = 0 TO 13: ERASEFIELD C: NEXT
	  INITCAMP 0, 7, 29, ASCI, 0, "XXXXXXXXXX", ""
	  INITCAMP 1, 8, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXX", ""
	  INITCAMP 2, 8, 58, ASCI, 0, "XXXXXXXXXXX", ""
	  INITCAMP 3, 9, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  INITCAMP 4, 11, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  INITCAMP 5, 12, 29, ASCI, 0, "XXXXXXX", ""
	  INITCAMP 6, 12, 48, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  INITCAMP 7, 13, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXX", ""
	  INITCAMP 8, 14, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXX", ""
	  INITCAMP 9, 15, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  INITCAMP 10, 17, 29, NUM, 0, "999", ""
	  INITCAMP 11, 17, 58, ASCI, 0, "XXXXXXXXXXXXXXXXX", ""
	  INITCAMP 12, 18, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  INITCAMP 13, 19, 29, ASCI, 0, "XXXXXXXXXX", ""
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPS:
	   INSERTCAMP 0, CLIENTS.CODICLIENT
	   INSERTCAMP 1, CLIENTS.NOM
	   INSERTCAMP 2, CLIENTS.DNI
	   INSERTCAMP 3, CLIENTS.COGNOMS
	   INSERTCAMP 4, CLIENTS.DIRECCIO
	   INSERTCAMP 5, CLIENTS.CPOSTAL
	   INSERTCAMP 6, CLIENTS.POBLACIO
	   INSERTCAMP 7, CLIENTS.TELEFON1
	   INSERTCAMP 8, CLIENTS.TELEFON2
	   INSERTCAMP 9, CLIENTS.PROVINCIA
	   INSERTCAMP 10, LTRIM$(STR$(CLIENTS.DTO))
	   INSERTCAMP 11, CLIENTS.FORMAPAGO
	   INSERTCAMP 12, CLIENTS.BANC
	   INSERTCAMP 13, CLIENTS.COMPTE
	  RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXER:
	   CLIENTS.CODICLIENT = RTRIM$(SHOWFIELD(0))
	   CLIENTS.NOM = SHOWFIELD$(1)
	   CLIENTS.DNI = SHOWFIELD$(2)
	   CLIENTS.COGNOMS = SHOWFIELD$(3)
	   CLIENTS.DIRECCIO = SHOWFIELD$(4)
	   CLIENTS.CPOSTAL = SHOWFIELD$(5)
	   CLIENTS.POBLACIO = SHOWFIELD$(6)
	   CLIENTS.TELEFON1 = SHOWFIELD$(7)
	   CLIENTS.TELEFON2 = SHOWFIELD$(8)
	   CLIENTS.PROVINCIA = SHOWFIELD$(9)
	   CLIENTS.DTO = VAL(SHOWFIELD$(10))
	   CLIENTS.FORMAPAGO = SHOWFIELD$(11)
	   CLIENTS.BANC = SHOWFIELD$(12)
	   CLIENTS.COMPTE = SHOWFIELD$(13)
	   CLIENTS.MARCAT = "-"
	   RETURN

'************************************************************************
' BUIDAR ELS CAMPS DEL FITXER
'************************************************************************
BUIDARCAMPS:
	   CLIENTS.CODICLIENT = STRING$(LEN(CLIENTS.CODICLIENT), CHR$(32))
	   CLIENTS.NOM = ""
	   CLIENTS.DNI = ""
	   CLIENTS.COGNOMS = ""
	   CLIENTS.DIRECCIO = ""
	   CLIENTS.CPOSTAL = ""
	   CLIENTS.POBLACIO = ""
	   CLIENTS.TELEFON1 = ""
	   CLIENTS.TELEFON2 = ""
	   CLIENTS.PROVINCIA = ""
	   CLIENTS.DTO = 0
	   CLIENTS.FORMAPAGO = ""
	   CLIENTS.BANC = ""
	   CLIENTS.COMPTE = ""
	   CLIENTS.MARCAT = "-"
	   FOR C = 0 TO 13: ERASEFIELD C: NEXT
	   RETURN

'************************************************************************
' FITXA DE UN CLIENT
'************************************************************************

MASCARA:
	  COLOR COL(0, 0), COL(0, 1)
	  FINESTRA 4, 10, 20, 76, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
	  LOCATE 5, 11: PRINT "Client No.: "; :  COLOR COL(0, 0), COL(0, 1): PRINT RV
	  LOCATE 6, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0), COL(2, 1)
	  LOCATE 7, 11: PRINT "Codi client......:"
	  LOCATE 8, 11: PRINT "Nom del client...:" + SPACE$(20) + " D.N.I..:"
	  LOCATE 9, 11: PRINT "Cognoms o mal nom:"
	  COLOR COL(0, 0), COL(0, 1): LOCATE 10, 11: PRINT STRING$(65, "Ä"); :  COLOR COL(2, 0), COL(2, 1)
	  LOCATE 11, 11: PRINT "Direcci¢.........:"
	  LOCATE 12, 11: PRINT "Codi Postal......:         Poblaci¢.:"
	  LOCATE 13, 11: PRINT "TelŠfon 1........:"
	  LOCATE 14, 11: PRINT "TelŠfon 2 o FAX..:"
	  LOCATE 15, 11: PRINT "Provincia........:"
	  COLOR COL(0, 0), COL(0, 1): LOCATE 16, 11: PRINT STRING$(65, "Ä"); :  COLOR COL(2, 0), COL(2, 1)
	  LOCATE 17, 11: PRINT "Descompte %......:" + SPACE$(11) + "Forma de pagament:"
	  LOCATE 18, 11: PRINT "Banc/Sucursal....:"
	  LOCATE 19, 11: PRINT "Compte...........:"
	  RETURN

'************************************************************************
' LLISTA UN BLOC
'************************************************************************

LISTA:
    COLOR 15, 9: X = 7
    FOR X = 7 TO 19
	COLOR COL(0, 0), COL(0, 1)
	LOCATE X, 2: PRINT STRING$(78, " ");
    NEXT

    FOR X = 7 TO 19
       IF R = MAXCL THEN R = R - 1: EXIT FOR
       GET AREA3, R, CLIENTS:
       COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
       R = R + 1
    NEXT
    RETURN

'************************************************************************
' MARCAR UN REGISTRE COM A BORRAT
'************************************************************************

DELETERECORD:
    GET AREA3, R, CLIENTS              ' CARREGAR EL REGISTRE DINS MEMORIA
    IF CLIENTS.MARCAT = "*" THEN       ' COMPROVAR SI ESTA MARCAT
       CLIENTS.MARCAT = "-"
       PUT AREA3, R, CLIENTS
    ELSE                               ' SI NO ESTA MARCAT QUE EL MARQUI
      IF CLIENTS.MARCAT <> "*" THEN
	 CLIENTS.MARCAT = "*"
	 PUT AREA3, R, CLIENTS
      END IF
    END IF
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR     ' TORNA A VISUALITZAR LA LINIA
    RETURN

'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE CLIENTS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "CLIENTS.NDX" FOR RANDOM SHARED AS AREA2 LEN = LEN(NDXCLIENT)
      AREA3 = FREEFILE: OPEN DIRECC$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA3 LEN = LEN(CLIENTS)
      AREA12 = FREEFILE: OPEN DIRECC$ + "CODISP.NDX" FOR RANDOM SHARED AS AREA12 LEN = LEN(NDXCODISP)
      AREA13 = FREEFILE: OPEN DIRECC$ + "CODISP.DAT" FOR RANDOM SHARED AS AREA13 LEN = LEN(CODISP)
      AREA4 = FREEFILE: OPEN DP$ + "PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CAP)
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
      MAXCL = CEMPRE.MAXCLIENTS
      MAXCP = LOF(AREA13) \ LEN(CODISP)
      DOC.REF$ = DOCNUM.NUMCLIS
      DOC.MAX% = DOCNUM.MAXCLIS
      RETURN

INDEXAR:
      FOR RI = 1 TO MAXCL                   ' COL.LOCAR A MEM•RIA L'INDEX I LA CLAU
	  GET AREA2, RI, NDXCLIENT
	  INDEX(RI).REGISTRE = NDXCLIENT.REGISTRE
	  INDEX(RI).CODI = NDXCLIENT.CODI
      NEXT
      'GOSUB TANCA
      RETURN

INDEXARCP:
      FOR RI = 1 TO MAXCP                   ' COL.LOCAR A MEM•RIA L'INDEX I LA CLAU
	  GET AREA2, RI, NDXCODISP
	  NDXCP(RI).REGISTRE = NDXCODISP.REGISTRE
	  NDXCP(RI).CODI = NDXCODISP.CODI
      NEXT
      'GOSUB TANCA
      RETURN

GUARDARINDEX:
      CALL SortClients(INDEX(), MAXCL)         ' ORDENAR L'INDEX
      FOR RI = 1 TO MAXCL                      ' COL.LOCAR A MEM•RIA L'INDEX I LA CLAU
	  NDXCLIENT.REGISTRE = INDEX(RI).REGISTRE
	  NDXCLIENT.CODI = INDEX(RI).CODI
	  PUT AREA2, RI, NDXCLIENT
      NEXT
      RETURN

TANCA:
      GOSUB GUARDARINDEX
      FOR C = 0 TO 13: ERASEFIELD C: NEXT
      ERASE INDEX, CAMPS
      RESET
      EXIT SUB

ACTUALIZ.INDEX:
      REDIM PRESERVE INDEX(1 TO MAXCL) AS INDEXCLI
      INDEX(RNDX).REGISTRE = RNDX
      INDEX(RNDX).CODI = CLIENTS.CODICLIENT
      RETURN
END SUB

SUB MASCCLIS
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 4, 1, 20, 80, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
    LOCATE 5, 2: PRINT "Codi       Nom Client" + SPACE$(11) + "Cognoms" + SPACE$(30) + "Baixa": COLOR COL(0, 0), COL(0, 1)
    LOCATE 6, 2: PRINT STRING$(78, "Ä");
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR CLIENTS <F2>=INSERTAR CLIENTS  <" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR              <F3>=SUMAR             <F4>=CONSULTAR"
    LOCATE 24, 2: PRINT "<F5>=MARCAR CLIENTS       <F6>=LLISTAR";
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

