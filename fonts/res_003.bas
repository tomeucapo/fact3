' ********************************************************************
'
' Fitxer....................: RES_003.BAS
' Titol.....................: Modul per el fer un hist•ric d'una reparaci¢
'
' ********************************************************************
'
' Data inici................: 12/10/1996 19:41:06
' Data de la darrera revisi¢:
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/96 (C)
'                             Summer'96 (C)
'
' ********************************************************************
' Notes:
'        
' ********************************************************************

DECLARE SUB IMPRIMIRRESGUARD (DEVI$, AREA4!, FACTURAT!)
DECLARE SUB CARREGARALBARAN (R!, MAXLIN!)
DECLARE SUB HistoricReparacio (DIRECCP$, DIRECC$, DIRECI$, DEVI$, IMPRESORA!)
DECLARE SUB MASCREGS ()
DECLARE SUB LLISTAR (AREA4!, AREA3!, MAXLINS!)
DECLARE FUNCTION FindClient% (CAMP$, INDEX() AS ANY, MAXCL!)
DECLARE SUB MASCCLIS ()
DECLARE SUB SortClients (INDEX() AS ANY, MAXCL!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'c:\FACT2\FONTS\FACTURA.BI'
'$INCLUDE: 'c:\FACT2\FONTS\RESGUARD.BI'
'$INCLUDE: 'c:\FACT2\FONTS\STRUCTS.BI'
'$INCLUDE: 'c:\FACT2\FONTS\VIDEOF.BI'
'$INCLUDE: 'CAMPS.BI'

COMMON SHARED /INDEX/ INDEX() AS INDEXCLI
COMMON SHARED DIRECC$, DIRCP$, DEVI$, DIRECCT$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT, rndx, MIDPOINT

DIM SHARED LIN$(100, 7)           ' LINIES DE LA FACTURA TEMPORAL
DIM SHARED OBS$(1)                ' LINIES D'OBSERVACIO
DIM SHARED CEMPRE AS CONTROLEMP       '     "      DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAP€ALERAS DE DOCUMENTS
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED FACTURA AS FACTU
DIM SHARED LINFACT AS LINIES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED CLIENTS AS CLIENT
DIM SHARED RESGUA AS RESG
DIM SHARED CFG AS CONFIG
DIM SHARED EMPRES AS EMPRESA
DIM SHARED CTRL AS CONTROL
DIM SHARED COLORS AS COLO
DIM SHARED COL(2, 1)
DIM SHARED USR AS USUARIS

      GOSUB TESTGEST
      OPEN "..\CONFIG.FAC" FOR RANDOM SHARED AS 1 LEN = LEN(CFG)
      OPEN "..\EMPRESA.FAC" FOR RANDOM SHARED AS 2 LEN = LEN(EMPRES)
      OPEN "..\CONTROL.CFG" FOR RANDOM SHARED AS 3 LEN = LEN(CTRL)
      OPEN "..\USUARIS.FAC" FOR RANDOM SHARED AS 5 LEN = LEN(USR)

      GET 3, 1, CTRL
      GET 1, CTRL.EMPRESA, CFG       ' RECULL LA CONFIGURACIO DE LA EMPRESA
      GET 2, CTRL.EMPRESA, EMPRES    ' RECULL LA CONFIGURACI¢ SECUNDARIA
      GET 5, CTRL.USUARI, USR

      UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
      DBF$ = LTRIM$(RTRIM$(CFG.DDADE))             ' Assignar direcctoris
      SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
      MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
      HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
      SYS$ = LTRIM$(RTRIM$(CFG.DSIST))
      IM$ = LTRIM$(RTRIM$(CFG.DIMPO))

      DIRECCF$ = UNIDAD$ + DBF$ + "\"      ' Subdirecctori de les base de dades
      DIRECCP$ = UNIDAD$ + SCR$ + "\"      ' Subdirecctori de les pantalles
      DIRECCR$ = UNIDAD$ + MSK$ + "\"      ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = UNIDAD$ + HLP$ + "\"     ' Subdirecctori de ajuda
      DIRECCI$ = UNIDAD$ + "\IMPRESOR\"    ' Subdirecctori de les impresores
      DIRECCT$ = UNIDAD$ + DBF$ + "\TEXTOS\"
      DIRECCS$ = UNIDAD$ + SYS$ + "\"
      DIRECCIM$ = UNIDAD$ + IM$ + "\"

      MAXLINS = USR.MAXLINS
      MAXFAC = USR.LINFACT
      IVA = EMPR.IVA
      DTO = EMPR.DTO
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA

      CLOSE 1, 2, 3, 5

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
      CALL HistoricReparacio(DIRECCP$, DIRECCF$, DIRECCI$, DEV$, IMPRESORA)
      SYSTEM


TESTGEST:
      NOM$ = "..\" + STRING$(8, 255) + "." + "ÿÿÿ"
      OPEN NOM$ FOR RANDOM SHARED AS 2 LEN = 10
      FIELD 2, 10 AS LIN$
      GET 2, 1
      IF LIN$ <> ">“““““““< " THEN
	 COLOR 15, 7
	 FINESTRA 10, 20, 14, 51, 1, CAIXA1: COLOR 31, 7
	 LOCATE 12, 21: PRINT "ENTRADA INCORRECTA AL PROGRAMA"
	 LOCATE 16, 1: COLOR 7, 0: SYSTEM
      END IF
      CLOSE 2
      RETURN


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

SUB CARREGARALBARAN (R, MAXLIN)
    SHARED LIN$()

    GET AREA3, R, LINFACT
    FOR J = 1 TO MAXLIN
	LIN$(J, 1) = LINFACT.LINIA(J).CODART
	LIN$(J, 2) = LINFACT.LINIA(J).CONCEPTE
	LIN$(J, 3) = STR$(LINFACT.LINIA(J).PREU)
	LIN$(J, 4) = STR$(LINFACT.LINIA(J).QUANTI)
	LIN$(J, 7) = STR$(LINFACT.LINIA(J).DTO)
	LIN$(J, 5) = STR$(LINFACT.LINIA(J).IMPORT)
	LIN$(J, 6) = LINFACT.LINIA(J).MARCAR
    NEXT
    GET AREA2, R, FACTURA
END SUB

FUNCTION FindClient% (CAMP$, INDEX() AS INDEXCLI, MAXCL) STATIC
	 SHARED rndx
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
	    rndx = MIDPOINT
	 ELSE
	    FindClient% = FALSE
	 END IF
END FUNCTION

SUB HistoricReparacio (DIRECCP$, DIRECC$, DIRECI$, DEVI$, IMPRESORA) STATIC
    SHARED rndx, DIRECCT$

    DIRECCT$ = DIRECC$ + "\TEXTOS\"
    ON ERROR GOTO ERRORS
    COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "  OBRINT..."
    GOSUB OBRIRFITXERS

    DIM MENU(1 TO 3) AS STRING

    IF MAXRE = 1 THEN
       X = 7: R = 1: L = 1
       BEEP: AVIS "AVIS:", "El fitxer de resguards est… buit.", 0
       GOSUB TANCA
    END IF

    GOSUB AGAFAR
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
    CENTRAR 4, "Reparacions corresponents al N.S.: " + SHOWFIELD$(4)
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
		  GOSUB TANCA
	     CASE CHR$(13)
		  GOSUB AGAFAR
	     CASE CHR$(0) + CHR$(65)
		  'GOSUB ENLLASAR
		  GOSUB TORNAR.LINKAR: X = 7: R = 1: MASCREGS
		  GOSUB LISTA: X = 7: R = 1: GET AREAR, R, RESGUA
		  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     CASE CHR$(0) + "Q"
		  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
		  R = R + 13: X = 19
		  GOSUB LISTA: X = 19
		  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
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
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
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
       GET AREAR, R, RESGUA: COLOR CCT, 9
       LOCATE X, 2: PRINT RESGUA.ORDRE; "³"; RESGUA.PERSONA.NOM; "³"; MID$(RESGUA.PERSONA.COGNOMS, 1, 28); "³"; RESGUA.FETXAIN; "³"; RESGUA.FETXAOUT
       R = R + 1
    NEXT
    RETURN

AGAFAR:
       GOSUB MASCARA
       SETMAXCAMPS 13
       GOSUB INITCAMPS
       GOSUB BUIDARCAMPS
       FOR C = 0 TO 13: SETCOLORCAMPS C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
       GOSUB READCAMPS
       IF ANUL = 1 THEN GOSUB TANCA
       RETURN
'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

READCAMPS:
       IF EDIT = 0 THEN
	  REF$ = "00000000-R": ALB$ = LTRIM$(STR$(RV)): L2 = 8
	  FOR L = LEN(ALB$) TO 1 STEP -1
	      MID$(REF$, L2, 1) = MID$(ALB$, L, 1): L2 = L2 - 1
	  NEXT
	  INSERTCAMP 1, REF$
	  INSERTCAMP 6, FETXA$
	  LOCATE , , 0
	  DISPLAYALL
       END IF
       SETMAXCAMPS 13: OLDVALUE$ = SPACE$(10)
       GetBackground 2, 10, 21, 77, TELBUF$
       DO
	 BUSCA$ = ""
	 FOR C = 0 TO 13
	     VALUE = LLEGIRCAMP(C)
	     SELECT CASE VALUE
		     CASE 4
			  GOSUB TORNAR.LINKAR
			  RETURN
		     CASE F1 TO F10
			  C = C - 1
		     CASE 999
			 ANUL = 1: RETURN
		     CASE ELSE
	     END SELECT
	 NEXT
	  COLOR 27: CENTRAR 19, "Son correctes aquestes dades (S/N) ?"
	  COLOR COL(0, 0), COL(0, 1): CORRECTE = 0
	  DO
	    DO: T$ = INKEY$: LOOP UNTIL T$ <> ""
	    IF UCASE$(T$) = "S" THEN CORRECTE = 888: EXIT DO
	    IF UCASE$(T$) = "N" THEN CORRECTE = 0: EXIT DO
	  LOOP
	  CENTRAR 19, "                                    "
       LOOP UNTIL CORRECTE = 888
       RETURN

'************************************************************************
' INICIALITZA CAMPS
'************************************************************************

INITCAMPS:

	  FOR C = 0 TO 13: ERASEFIELD C: NEXT
	  INITCAMP 0, 5, 29, ASCI, 0, STRING$(10, "X"), ""   ' CODI CLIENT
	  INITCAMP 1, 5, 63, ASCI, 0, STRING$(10, "X"), ""   ' N§ D'ORDRE
	  INITCAMP 2, 8, 17, ASCI, 0, STRING$(20, "X"), ""   ' MARCA
	  INITCAMP 3, 8, 54, ASCI, 0, STRING$(20, "X"), ""   ' MODEL
	  INITCAMP 4, 9, 23, ASCI, 0, STRING$(20, "X"), ""   ' SERIE
	  INITCAMP 5, 9, 56, ASCI, 0, STRING$(20, "X"), ""   ' APARELL
	  INITCAMP 6, 10, 24, ASCI, 0, "99/99/99", ""    ' DATA IN
	  INITCAMP 7, 10, 61, ASCI, 0, "99/99/99", ""     ' DATA OUT
	  INITCAMP 8, 12, 11, ASCI, 0, STRING$(50, "X"), ""  ' OBSERVACIONS CLIENT
	  INITCAMP 9, 13, 11, ASCI, 0, STRING$(50, "X"), ""
	  INITCAMP 10, 14, 11, ASCI, 0, STRING$(50, "X"), ""
	  INITCAMP 11, 16, 11, ASCI, 0, STRING$(50, "X"), ""' OBSERVACIONS SERVEI
	  INITCAMP 12, 17, 11, ASCI, 0, STRING$(50, "X"), ""
	  INITCAMP 13, 18, 11, ASCI, 0, STRING$(50, "X"), ""
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPS:
	  LOCATE 6, 29: PRINT STRING$(47, " ")
	  COLOR COL(0, 0): LOCATE 6, 29: PRINT LTRIM$(RTRIM$(RESGUA.PERSONA.NOM)) + " " + RTRIM$(LTRIM$(RESGUA.PERSONA.COGNOMS))
	  INSERTCAMP 0, RESGUA.CODCLIENT       ' CODI CLIENT
	  INSERTCAMP 1, RESGUA.ORDRE           ' N§ D'ORDRE
	  INSERTCAMP 2, RESGUA.MARCA           ' MARCA
	  INSERTCAMP 3, RESGUA.MODELO          ' MODEL
	  INSERTCAMP 4, RESGUA.NSERIE          ' SERIE
	  INSERTCAMP 5, RESGUA.APARELL         ' APARELL
	  INSERTCAMP 6, RESGUA.FETXAIN         ' DATA IN
	  INSERTCAMP 7, RESGUA.FETXAOUT        ' DATA OUT
	  INSERTCAMP 8, RESGUA.OBSERVACL(1)    ' OBSERVACIONS CLIENT
	  INSERTCAMP 9, RESGUA.OBSERVACL(2)
	  INSERTCAMP 10, RESGUA.OBSERVACL(3)
	  INSERTCAMP 11, RESGUA.OBSERVASE(1)   ' OBSERVACIONS SERVEI
	  INSERTCAMP 12, RESGUA.OBSERVASE(2)
	  INSERTCAMP 13, RESGUA.OBSERVASE(3)
	  RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXER:
	   RESGUA.CPY = "" + CHR$(26)
	   RESGUA.PERSONA.CODICLIENT = CLIENTS.CODICLIENT
	   RESGUA.PERSONA.NOM = CLIENTS.NOM
	   RESGUA.PERSONA.COGNOMS = CLIENTS.COGNOMS
	   RESGUA.PERSONA.DIRECCIO = CLIENTS.DIRECCIO
	   RESGUA.PERSONA.DNI = CLIENTS.DNI
	   RESGUA.PERSONA.PROVINCIA = CLIENTS.PROVINCIA
	   RESGUA.PERSONA.POBLACIO = CLIENTS.POBLACIO
	   RESGUA.PERSONA.TELEFON1 = CLIENTS.TELEFON1
	   RESGUA.PERSONA.TELEFON2 = CLIENTS.TELEFON2
	   RESGUA.PERSONA.DTO = CLIENTS.DTO
	   RESGUA.PERSONA.FORMAPAGO = CLIENTS.FORMAPAGO
	   RESGUA.PERSONA.CPOSTAL = CLIENTS.CPOSTAL
	   RESGUA.PERSONA.BANC = CLIENTS.BANC
	   RESGUA.PERSONA.COMPTE = CLIENTS.COMPTE
	   RESGUA.PERSONA.MARCAT = CLIENTS.MARCAT
	   RESGUA.CODCLIENT = SHOWFIELD$(0)
	   RESGUA.REGALB = 0
	   RESGUA.ALBARAN = ""
	   RESGUA.ORDRE = SHOWFIELD$(1)
	   RESGUA.MARCA = SHOWFIELD$(2)
	   RESGUA.MODELO = SHOWFIELD$(3)
	   RESGUA.NSERIE = SHOWFIELD$(4)
	   RESGUA.APARELL = SHOWFIELD$(5)
	   RESGUA.FETXAIN = SHOWFIELD$(6)
	   RESGUA.FETXAOUT = SHOWFIELD$(7)
	   FOR LO = 1 TO 3: RESGUA.OBSERVACL(LO) = SHOWFIELD$(LO + 7): NEXT
	   FOR LO = 1 TO 3: RESGUA.OBSERVASE(LO) = SHOWFIELD$(LO + 10): NEXT
	   RESGUA.MARCAT = "-"
	   FOR C = 0 TO 13: ERASEFIELD C: NEXT
	   RETURN

'************************************************************************
' BUIDAR ELS CAMPS DEL FITXER
'************************************************************************
BUIDARCAMPS:
	   RESGUA.CODCLIENT = STRING$(LEN(CLIENTS.CODICLIENT), CHR$(32))
	   RESGUA.CPY = ""
	   RESGUA.REGALB = 0
	   RESGUA.ALBARAN = ""
	   RESGUA.ORDRE = ""
	   RESGUA.MARCA = ""
	   RESGUA.MODELO = ""
	   RESGUA.NSERIE = ""
	   RESGUA.APARELL = ""
	   RESGUA.FETXAIN = FETXA$
	   RESGUA.FETXAOUT = "  /  /  "
	   FOR LO = 1 TO 3: RESGUA.OBSERVACL(LO) = "": NEXT
	   FOR LO = 1 TO 3: RESGUA.OBSERVASE(LO) = "": NEXT
	   RESGUA.MARCAT = "-"
	   FOR C = 0 TO 13: ERASEFIELD C: NEXT
	   RETURN





'************************************************************************
' FITXA D'UN RESGUARD
'************************************************************************

MASCARA:
	  COLOR COL(0, 0), COL(0, 1)
	  FINESTRA 2, 10, 20, 76, 1, CAIXA1: COLOR COL(2, 0)
	  LOCATE 3, 11: PRINT "RESGUARD No.: "; : COLOR COL(0, 0): PRINT RV: COLOR COL(2, 0)
	  LOCATE 4, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0)
	  LOCATE 5, 11: PRINT "CODI CLIENT......:                       N§ D'ORDRE:"
	  LOCATE 6, 11: PRINT "NOM DEL CLIENT...:"                     '           1         2
	  LOCATE 7, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0)   '  12345678901234567890
	  LOCATE 8, 11: PRINT "MARCA:                               MODEL:                    "
	  LOCATE 9, 11: PRINT "N§ DE SERIE:                         APARELL:                    "
	  LOCATE 10, 11: PRINT "DATA ENTRADA:                        DATA SORTIDA:"
	  LOCATE 11, 11: PRINT STRING$(65, "Ä");
	  CENTRAR 11, " OBSERVACIONS DEL CLIENT "
	  LOCATE 15, 11: PRINT STRING$(65, "Ä");
	  CENTRAR 15, " OBSERVACIONS DEL SERVEI "
	  RETURN
'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE CLIENTS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA4 = FREEFILE: OPEN DIRECC$ + "\PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CAP)
      AREA2 = FREEFILE: OPEN DIRECC$ + "FACTURES.CAB" FOR RANDOM SHARED AS AREA2 LEN = LEN(FACTURA)
      AREA3 = FREEFILE: OPEN DIRECC$ + "FACTURES.LIN" FOR RANDOM SHARED AS AREA3 LEN = LEN(LINFACT)
      AREAC = FREEFILE: OPEN DIRECC$ + "RESGUARD.DAT" FOR RANDOM SHARED AS AREAC LEN = LEN(RESGUA)
      AREAR = FREEFILE: OPEN DIRECC$ + "HISTORIC.BCK" FOR RANDOM SHARED AS AREAR LEN = LEN(RESGUA)

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
	IF RESGUA.NSERIE = LTRIM$(RTRIM$(SHOWFIELD$(4))) THEN
	   RESGUA.REGALB = R
	   PUT AREAR, RC, RESGUA
	   RC = RC + 1
	END IF
    NEXT
    IF RC = 1 THEN
       AVIS.SONOR (1)
       AVIS "ERROR:", "No hi ha cap reparaci¢ amb aquest nombre de sŠrie", 0
       GOSUB TANCA
    END IF
    GET AREAR, 1, RESGUA
    RESGUA.CPY = "Fitxer dels RESGUARDS. (IMATGE) COPIA" + CHR$(26)
    RESGUA.MAXREG = RC
    PUT AREAR, 1, RESGUA
    MAXRE = RC
    RETURN

TANCA:
      KILL DIRECC$ + "HISTORIC.BCK"
      FOR C = 0 TO 13: ERASEFIELD C: NEXT
      ERASE CAMPS
      RESET
      EXIT SUB
END SUB

SUB IMPRIMIRRESGUARD (DEVI$, AREA4, FACTURAT)
   SHARED DIRECCT$

   DIM CONFIG$(0)
   DIM MENU(3) AS STRING

   CONFIG$(0) = " CODI ARTICLE   CONCEPTE                 QUANT.  PREU  % IMPORT"

    AREATXT = FREEFILE
    OPEN DIRECCT$ + "RES_ALBA.TXT" FOR OUTPUT SHARED AS AREATXT

    PAG = 1
    GET AREA4, 1, CAP
    GOSUB CAPSA
    MAXLINS = 13
    LI = 1: L = 1

    DO
	IF LI = MAXLINS THEN
	   IF CASO% <> 3 THEN
	      FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
	      PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + " " + STRING$(78, "Ä")
	      PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
	      PAG = PAG + 1
	      GOSUB CAPSA
	   END IF
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

    PRINT #AREATXT, " " + STRING$(78, "Ä")
    PRINT #AREATXT, USING " SUBTOTAL: ##,###,###.##"; FACTURA.TOTALBRUT
    PRINT #AREATXT, USING " DTO%....:            ##"; FACTURA.DTO;
    PRINT #AREATXT, USING SPACE$(25) + "BASE IMPONIBLE: ##,###,###"; FACTURA.BASEIMPONIBLE
    PRINT #AREATXT, USING SPACE$(52) + "     IVA %.:         ##"; FACTURA.TIPOIVA
    PRINT #AREATXT, ""
    PRINT #AREATXT, USING SPACE$(52) + "   TOTAL...: ##,###,###"; FACTURA.TOTALNET
    PRINT #AREATXT, " " + STRING$(78, "Ä")
    PRINT #AREATXT, " Observacions Client: " + RESGUA.OBSERVACL(1)
    PRINT #AREATXT, "                      " + RESGUA.OBSERVACL(2)
    PRINT #AREATXT, "                      " + RESGUA.OBSERVACL(3)
    PRINT #AREATXT, " " + STRING$(78, "Ä")
    PRINT #AREATXT, " Observacions Servei: " + RESGUA.OBSERVASE(1)
    PRINT #AREATXT, "                      " + RESGUA.OBSERVASE(2)
    PRINT #AREATXT, "                      " + RESGUA.OBSERVASE(3)
    PRINT #AREATXT, " " + STRING$(78, "Ä")

    
    PRINT #AREATXT, " " + "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO      '; " Observacions:" + RTRIM$(ARCHIMP.COMPRIMIDO);
    'PRINT #AREATXT, LTRIM$(RTRIM$(FACTURA.OBSERVA(1))) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    'PRINT #AREATXT, "                                                     " + RTRIM$(ARCHIMP.COMPRIMIDO); LTRIM$(RTRIM$(FACTURA.OBSERVA(2))) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    PRINT #AREATXT, " " + STRING$(78, "Ä")
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

    IF FACTURAT = 0 THEN
       PRINT #AREATXT, " FACTURA N§:     Data:      Codi Client:";
    ELSE
       IF FACTURAT = 99 THEN PRINT #AREATXT, " Factura N§:     Data:      Codi Client:";
    END IF

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
    PRINT #AREATXT, CONFIG$(0)
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
    LOCATE 5, 2: PRINT "ORDRE     ³NOM CLIENT" + SPACE$(10) + "³COGNOMS" + SPACE$(21) + "³DATA IN ³DATA OUT": COLOR COL(0, 0), COL(0, 1)
    LOCATE 6, 2: PRINT "ÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ"
    FOR L = 7 TO 19: LOCATE L, 2: PRINT "          ³                    ³                            ³        ³": NEXT
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR              "
    LOCATE 24, 2: PRINT "<F4>=CONSULTAR   <F5>=FILTRAR   <F6>=LLISTAR";
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

