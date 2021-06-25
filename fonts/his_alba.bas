' ********************************************************************
'
' Fitxer....................: HIS_ALBA.BAS
' Titol.....................: Modul per crear hist•rics damunt una
'                             sŠrie d'albarans.
' ********************************************************************
'
' Data inici................: 08/11/1996 16:40:00
' Data de la darrera revisi¢: 07/25/1997 18:44:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************
'

DECLARE SUB HistoricsAlbarans (DIRECC$, DIRECCI$, DIRECCT$, DEVICE$, IMPRESORA!)
DECLARE SUB SortAlbarans (INDEX() AS ANY, MAXAL!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\consts.bi'


COMMON SHARED DIRECC$, DIRCP$, DIRECCT$, DOCU$
COMMON SHARED AREA, AREA2, AREA3, AREA4, AREA5, AREAFACC, AREAFACL
COMMON SHARED GUARDAT, MIDPOINT, MAXCL, MAXFA, R, EDIT
COMMON SHARED TOTAL, MAX, NOU, TROBAT, DEVICE$, DEVI$
COMMON SHARED COL()

DIM SHARED COLORS AS COLO
DIM SHARED COL(2, 1)
DIM SHARED FLD(6) AS CN           ' CONFIGURACI¢ DE CAMPS
DIM SHARED MASC$(6)               ' CONFIGURACI¢ DE MASCARES
DIM SHARED LIN$(100, 7)           ' LINIES DE LA FACTURA TEMPORAL
DIM SHARED OBS$(1)                ' LINIES D'OBSERVACIO
DIM SHARED NTXALBA AS CABNTXAL
DIM SHARED CLIENTS AS CLIENT
DIM SHARED CEMPRE AS CONTROLEMP
'DIM SHARED FACTURA AS FACTURA
DIM SHARED ALBARAN AS ALBAR
DIM SHARED LINALBA AS LINIES
DIM SHARED TEXTOSMENUP(6) AS STRING
DIM SHARED CAP AS CAPSALDOCS
DIM SHARED SPOOLIMP AS SPOOL
DIM SHARED ARCHIMP AS IMPRESORA
DIM SHARED CFG AS CONFIG
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
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

      DIRECCF$ = UNIDAD$ + DBF$ + "\"      ' Subdirecctori de les base de dades
      DIRECCP$ = UNIDAD$ + SCR$ + "\"      ' Subdirecctori de les pantalles
      DIRECCR$ = UNIDAD$ + MSK$ + "\"      ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = UNIDAD$ + HLP$ + "\"     ' Subdirecctori de ajuda
      DIRECCI$ = UNIDAD$ + "\IMPRESOR\"    ' Subdirecctori de les impresores
      DIRECCT$ = UNIDAD$ + DBF$ + "\TEXTOS\"
      DIRECCH$ = UNIDAD$ + DBF$ + "\HISTORIC\"

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


      CALL HistoricsAlbarans(DIRECCF$, DIRECCI$, DIRECCT$, DEV$, IMPRESORA)
      RESET
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

SUB HistoricsAlbarans (DIRECC$, DIRECCI$, DIRECCT$, DEVICE$, IMPRESORA)

    GOSUB OBRIFITXERS
    T$ = "Ref.Albaran  Client                                             Data         "

    IF MAXAL = 1 THEN
       Avis "ERROR:", "EL FITXER D'ALBARANS EST… BUIT!", 0
       EXIT SUB
    END IF

    DIM INDEX(1 TO MAXAL) AS CABNTXAL
    DIM LLISTA$(1 TO MAXAL)
    'CALL SAVESCRN
    GetBackground 1, 1, 25, 79, FACTU$
    COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 10, 20, 65, 1, CAIXA1
    COLOR COL(2, 0), COL(2, 1): LOCATE 11, 11: PRINT "Historics d'albarans": COLOR COL(0, 0), COL(0, 1)
    LOCATE 12, 11: PRINT STRING$(54, "Ä");
    COLOR COL(2, 0), COL(2, 1)
    LOCATE 14, 13: PRINT "  Nom i Cognoms: "
    SetMaxCamps 2
    FOR C = 0 TO 3: SETCOLORCAMPS C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
    INITCAMP 0, 13, 30, ASCI, 0, "XXXXXXXXXX", "Codi de client:"
    INITCAMP 1, 16, 30, ASCI, 0, "99/99/99", "Data Principi:"
    INITCAMP 2, 17, 30, ASCI, 0, "99/99/99", "Data Final:"
    DISPLAYALL

    '
    ' Demanar el codi del client
    '
    FOR S = 0 TO 2
	VALUE = LLEGIRCAMP(S)
	SELECT CASE VALUE
	       CASE 0
		       NO = 0
		       COLOR 28: LOCATE 11, 40: PRINT "                       ": COLOR 15, 9
		       CODCLI$ = SHOWFIELD$(0)
		       FOR RC = 1 TO MAXCL
			   GET AREA4, RC, CLIENTS
			   IF CLIENTS.CODICLIENT = CODCLI$ THEN
			      NO = 99: EXIT FOR
			   ELSE
			      NO = 44
			   END IF
		       NEXT
		       IF NO = 44 THEN
			  COLOR 28: LOCATE 11, 40: PRINT "<< CLIENT INEXISTENT >>"
			  BEEP: S = S - 1
			  COLOR COL(0, 0): LOCATE 11, 40: PRINT "                       "
		       ELSE
			  LOCATE 14, 30: PRINT RTRIM$(LTRIM$(CLIENTS.NOM))
			  LOCATE 15, 30: PRINT CLIENTS.COGNOMS
			  LOCATE 17, 50: PRINT C - 1
			  '
			  ' Indexar la cap‡alera dels albarans amb la clau de la DATA
			  '
			  C = 1
			  FOR R = 1 TO MAXAL
			      GET AREA2, R, ALBARAN
			      IF ALBARAN.CODCLIENT = CODCLI$ THEN
				 INDEX(C).REGISTRE = R
				 INDEX(C).DADA = ALBARAN.DADA
				 INDEX(C).REFALBARAN = ALBARAN.REFALBARAN
				 C = C + 1
			      END IF
			  NEXT
			  CALL SortAlbarans(INDEX(), C)     ' Ordenar index
			  INSERTCAMP 1, INDEX(1).DADA
			  INSERTCAMP 2, INDEX(C - 1).DADA
			  DISPLAYALL
		       END IF
	       CASE F1
		    SHELL "BKHLP HISTORICS": S = S - 1
	       CASE F2 TO F10
		    S = S - 1: SOUND 50, .5
	       CASE 999
		    GOSUB SAL
	       CASE ELSE
	END SELECT
    NEXT
    CALL SortAlbarans(INDEX(), C - 1)
    F = 1: TOTAL = 0
    FOR P = 1 TO C - 1
	GET AREA2, INDEX(P).REGISTRE, ALBARAN
	IF ALBARAN.CODCLIENT = CODCLI$ THEN
	   TOTAL = TOTAL + ALBARAN.TOTALNET
	   NOM$ = ALBARAN.PERSONA.NOM + " " + ALBARAN.PERSONA.COGNOMS
	   LLISTA$(F) = ALBARAN.REFALBARAN + "    " + NOM$ + INDEX(P).DADA
	   F = F + 1
	END IF
	IF SHOWFIELD$(1) <> SHOWFIELD$(2) THEN
	   IF ALBARAN.DADA = SHOWFIELD$(2) THEN EXIT FOR
	END IF
    NEXT
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<F1>=AJUDA          <F7>=IMPRIMIR";
    LOCATE 23, 2: PRINT "<F2>=GUARDAR        <ESC>=SORTIR"; : LOCATE , , 0
    LOCATE 24, 50: PRINT "Total Net..:"; : COLOR 3: PRINT USING "##,###,###.##"; TOTAL;

    DO
     L = Achoice(5, 1, 20, 79, C - 1, LLISTA$(), COL(), T$, 13)
     SELECT CASE L
	    CASE IS = -1
		 SHELL "BKHLP HISTORICS"
	    CASE IS = -7
		 GOSUB IMPRIMIR
	    CASE IS = 0
		 'CALL RESTSCRN
		 PutBackground 1, 1, FACTU$
		 EXIT SUB
	    CASE ELSE
		SOUND 50, .5
     END SELECT
    LOOP

IMPRIMIR:
    AREATXT = FREEFILE: OPEN DIRECCT$ + "HIS_ALBA.TXT" FOR OUTPUT SHARED AS AREATXT
    GET AREA2, INDEX(P).REGISTRE, ALBARAN
    GET AREA5, 1, CAP: PAG = 1
    GOSUB CAPSA
    LI = 1
    FOR E = 1 TO C - 1
	IF LI >= 33 THEN
	   PRINT #AREATXT, " " + STRING$(79, "Ä")
	   PRINT #AREATXT, ARCHIMP.SALTOPAGINA
	   LI = 1: PAG = PAG + 1
	   GOSUB CAPSA
	END IF
	GET AREA2, INDEX(E).REGISTRE, ALBARAN
	GET AREA3, INDEX(E).REGISTRE, LINALBA
	d$ = ALBARAN.DADA
	L = 1
	DO
	    LI$ = LINALBA.LINIA(L).CONCEPTE
	    IF LINALBA.LINIA(L).QUANTI = 0 THEN C$ = " " ELSE C$ = STR$(LINALBA.LINIA(L).QUANTI)
	    PRINT #AREATXT, USING "  \      \   \     \  \" + SPACE$(LEN(LI$) - 2) + "\"; d$; C$; LI$
	    d$ = ""
	    LI = LI + 1
	    L = L + 1
	LOOP UNTIL LINALBA.LINIA(L).MARCAR = "-"

	FOR O = 1 TO 2
	    PRINT #AREATXT, USING "                      \" + SPACE$(LEN(ALBARAN.OBSERVA(O)) - 2) + "\"; ALBARAN.OBSERVA(O)
	    LI = LI + 1
	NEXT
	PRINT #AREATXT, USING "             " + SPACE$(LEN(LI$) - 3) + "Total Albaran: ##,###,###.##"; ALBARAN.TOTALNET
	LI = LI + 1
    NEXT
    PRINT #AREATXT, " " + STRING$(79, "Ä")
    PRINT #AREATXT, USING "             " + SPACE$(LEN(LI$) - 3) + "        TOTAL: ##,###,###.##"; TOTAL
    PRINT #AREATXT, " " + STRING$(79, "Ä")
    CLOSE AREATXT

    CALL ImprimeixFitxerTXT(DIRECCT$ + "HIS_ALBA.TXT", DEVICE$, 180)
    RETURN
CAPSA:
    PRINT #AREATXT, " " + RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    FOR I = 1 TO 4: PRINT #AREATXT, " " + CAP.LINIES(I): NEXT

    PRINT #AREATXT, SPACE$(41) + "Ú" + SPACE$(31) + "¿"
    PRINT #AREATXT, SPACE$(43) + ALBARAN.PERSONA.NOM
    PRINT #AREATXT, " P…gina N§:"; SPACE$(31) + " " + ALBARAN.PERSONA.COGNOMS + LTRIM$(ARCHIMP.NEGRITA)
    PRINT #AREATXT, USING " \  \       \ \"; LTRIM$(STR$(PAG)); RTRIM$(ARCHIMP.NONEGRITA);
    PRINT #AREATXT, SPACE$(29) + " D.N.I. " + ALBARAN.PERSONA.DNI
    PRINT #AREATXT, SPACE$(41) + "  " + ALBARAN.PERSONA.DIRECCIO
    PRINT #AREATXT, SPACE$(43) + LTRIM$(RTRIM$(ALBARAN.PERSONA.POBLACIO)) + " " + ALBARAN.PERSONA.CPOSTAL
    PRINT #AREATXT, SPACE$(43) + "Tels." + ALBARAN.PERSONA.TELEFON1
    PRINT #AREATXT, SPACE$(43) + "     " + ALBARAN.PERSONA.TELEFON2
    PRINT #AREATXT, SPACE$(41) + "À" + SPACE$(31) + "Ù"

    PRINT #AREATXT, " " + STRING$(79, "Ä")
    PRINT #AREATXT, "  Data      Quant.    Concepte"
    PRINT #AREATXT, " " + STRING$(79, "Ä")
    RETURN
SAL:
    'CALL RESTSCRN
    PutBackground 1, 1, FACTU$
    EXIT SUB

OBRIFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "ALBARAN.CAB" FOR RANDOM SHARED AS AREA2 LEN = LEN(ALBARAN)
      AREA3 = FREEFILE: OPEN DIRECC$ + "ALBARAN.LIN" FOR RANDOM SHARED AS AREA3 LEN = LEN(LINALBA)
      AREA4 = FREEFILE: OPEN DIRECC$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CLIENTS)
      AREA5 = FREEFILE: OPEN DIRECC$ + "PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA5 LEN = LEN(CAP)
      AREANTX = FREEFILE: OPEN DIRECC$ + "ALBARAN.NDX" FOR RANDOM SHARED AS AREANTX LEN = LEN(NTXALBA)
'*********************************************************************
' DEMANAR IMPRESORA ACTIVA
'*********************************************************************
      AREA6 = FREEFILE: OPEN "..\SPOOL.CFG" FOR RANDOM SHARED AS AREA6 LEN = LEN(SPOOLIMP)
      GET AREA6, IMPRESORA, SPOOLIMP
      FITXER$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
      CLOSE AREA6

'*********************************************************************
' OBRIR FITXER DE LA IMPRESORA
'*********************************************************************

      AREA7 = FREEFILE: OPEN DIRECCI$ + FITXER$ FOR RANDOM SHARED AS AREA7 LEN = LEN(ARCHIMP)
      GET AREA7, 1, ARCHIMP
      CLOSE AREA7

'*********************************************************************
' INICIAR EL CONTROL DELS REGISTRES
'*********************************************************************
      GET AREA, 1, CEMPRE
      MAXAL = CEMPRE.MAXALBARAN
      MAXCL = CEMPRE.MAXCLIENTS
      MAXST = CEMPRE.MAXSTOCK
      RETURN


END SUB

SUB SortAlbarans (INDEX() AS CABNTXAL, MAXAL) STATIC

    OFFSET = MAXAL - 1 \ 2

    DO WHILE OFFSET > 0
       LIMIT = MAXAL - 1 - OFFSET
       DO
	 SWITCH = FALSE
	 FOR I = 1 TO LIMIT
	     IF INDEX(I).DADA > INDEX(I + OFFSET).DADA THEN
		SWAP INDEX(I), INDEX(I + OFFSET)
		SWITCH = I
	     END IF
	 NEXT

	 LIMIT = SWITCH
       LOOP WHILE SWITCH
       OFFSET = OFFSET \ 2
    LOOP

END SUB

