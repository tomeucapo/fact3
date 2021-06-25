' ********************************************************************
'
' Fitxer....................: PRO_001.BAS
' Titol.....................: Modul per el mateniment dels prove‹dors.
'
' ********************************************************************
'
' Data inici................: 16/09/1996
' Data de la darrera revisi¢:
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************
'

DECLARE FUNCTION FindProveidor% (CAMP$, INDEX() AS ANY, MAXPR!)
DECLARE SUB LISTPRO (AREA4!, AREA3!, DIRECC$, DEVI$, MAXLINS!)
DECLARE SUB SortProveidors (INDEX() AS ANY, MAXPR!)
DECLARE SUB MantenimentProveidors (DIRECCP$, DIRECC$, DIRECI$, DEVI$, IMPRESORA!, MAXLINS!)
DECLARE SUB LISTCLI (AREA4!, AREA3!, DIRECC$, DEVI$, MAXLINS!)
DECLARE SUB SKIP (A, R)
DECLARE SUB MASCLIST ()
DECLARE SUB MASCCLIS ()

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'c:\FACT2\FONTS\PROVEID.BI'
'$INCLUDE: 'c:\FACT2\FONTS\FACTURA.BI'
'$INCLUDE: 'c:\FACT2\FONTS\STRUCTS.BI'
'$INCLUDE: 'c:\FACT2\FONTS\VIDEOF.BI'
'$INCLUDE: 'CAMPS.BI'

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
DIM SHARED PROVEIDORS AS PROVEID
DIM COLORS AS COLO
DIM SHARED COL(2, 1)
DIM SHARED USR AS USUARIS

'     GOSUB TESTGEST
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


      CALL MantenimentProveidors(DIRECCP$, DIRECCF$, DIRECCI$, DEV$, IMPRESORA, MAXLINS)
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
       LOCATE 24, 2: PRINT "<F5>=MARCAR PROVEIDORS       <F6>=LLISTAR";
       RESUME NEXT

FUNCTION FindProveidor% (CAMP$, INDEX() AS INDEXPROVEIDOR, MAXPR) STATIC
	 SHARED RNDX
	 TOPRECORD = MAXPR
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
	    GET AREA3, INDEX(MIDPOINT).REGISTRE, PROVEIDORS
	    FindProveidor% = TRUE
	    RNDX = MIDPOINT
	 ELSE
	    FindProveidor% = FALSE
	 END IF
END FUNCTION

SUB LISTPRO (AREA4, AREA3, DIRECC$, DEVI$, MAXLINS)
    SHARED DIRECCT$, DIRECCM$
    DIM CAB$(3)
    DIRECCT$ = DIRECC$ + "TEXTOS\"
    AREAMSK = FREEFILE: OPEN DIRECC$ + "PLANTILL\PROVEIDO.MSK" FOR INPUT SHARED AS AREAMSK
    AREATXT = FREEFILE: OPEN DIRECCT$ + "PROVEIDO.TXT" FOR OUTPUT SHARED AS AREATXT

    AVIS "AVIS:", "INSERTI PAPER A LA IMPRESSORA,     PLEASE !!! :-)", 0
    PAG = 1: L = 1
    ' DEFINIR MASCARA DE LES LINIES
    GET AREA4, 1, CAP             ' AGAFAR CAP€ALERA DEL FITXER DE CAP€ALERES
    LINE INPUT #AREAMSK, CAB$(1)  ' AGAFAR LA CAP€ALERA DELS CAMPS
    LINE INPUT #AREAMSK, CAB$(2)
    LINE INPUT #AREAMSK, CAB$(3)
    CLOSE #AREAMSK

    GOSUB CAPSA         ' IMPRIMIR CAP€ALERA

    ' LLISTAR EL FITXER DELS PROVEIDORS

    FOR R = 1 TO CEMPRE.MAXPROVEID - 1
	GET AREA3, R, PROVEIDORS
	IF PROVEIDORS.MARCAT <> "*" THEN GOSUB PRINTLINIA
    NEXT

    FOR LA = L TO MAXLINS: PRINT #AREATXT, "": NEXT
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(79, "Ä") + RTRIM$(ARCHIMP.COMPRIMIDO)
    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT
    CALL ImprimeixFitxerTXT(DIRECCT$ + "PROVEIDO.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP€ALERA DELS LLISTATS
' **************************************************************************

CAPSA:
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, "LLISTAT TOTAL DELS PROVEIDORS"
    PRINT #AREATXT, "P…gina:"; PAG
    PRINT #AREATXT, "Data..: "; FETXA$
    PRINT #AREATXT, STRING$(78, "Ä");
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""
    PRINT #AREATXT, CAB$(1)
    PRINT #AREATXT, CAB$(2)
    RETURN

PRINTLINIA:
     IF L >= MAXLINS THEN
	SOUND 50, .5
	AVIS "AVIS:", "INSERTI PAPER A LA IMPRESSORA,     PLEASE !!! :-)", 0
	L = 1: PAG = PAG + 1
	PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(79, "Ä")
	PRINT #AREATXT, ""
	PRINT #AREATXT, ""
	GOSUB CAPSA
     ELSE
	N$ = RTRIM$(LTRIM$(PROVEIDORS.NOM)) + " " + RTRIM$(LTRIM$(PROVEIDORS.NOM1))
	NOM$ = MID$(N$, 1, 40)
	PRINT #AREATXT, USING CAB$(3); PROVEIDORS.CODI; PROVEIDORS.COMPTA; NOM$; PROVEIDORS.DIRECCIO; PROVEIDORS.POBLACIO; PROVEIDORS.TEL1
	L = L + 1
     END IF
    RETURN
END SUB

SUB MantenimentProveidors (DIRECCP$, DIRECC$, DIRECI$, DEVI$, IMPRESORA, MAXLINS) STATIC
    SHARED RNDX

    ON ERROR GOTO ERRORS
    COLOR 15, 9: FINESTRA 10, 30, 14, 42, 1, CAIXA1
    COLOR 31, 9: LOCATE 12, 31: PRINT "  OBRINT..."
    GOSUB OBRIRFITXERS

    IF MAXPR = 1 THEN
       X = 7: R = 1: L = 1
       BEEP: AVIS "AVIS:", "El fitxer dels proveidors est… buit.", 0
       GOSUB INSERTARCLIENT
       IF ANUL = 1 THEN GOSUB TANCA
    END IF
    
    MASCCLIS                          ' PINTA LA MASCARA DE LA LLISTA
    X = 7: R = 1: L = 1
    '
    ' ********* MOSTRA ELS REGISTRES QUE CABIGEN EN UNA PANTALLA
    '
    GOSUB LISTA

    X = 7: R = 1                      ' INICIA CURSOR I POSICIONS A PANTALLA
    SKIP AREA3, R: RETRIEVE AREA3, PROVEIDORS
    COLCURS = 12: GOSUB SHOWCURSOR
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
		  GOSUB EDITARCLIENT         ' EDITAR CLIENT
	     CASE CHR$(0) + CHR$(60)
		  GOSUB INSERTARCLIENT       ' INSERTAR CLIENT
	     CASE CHR$(0) + CHR$(62)
		  GOSUB CONSULTARCLIENT      ' CONSULTAR CLIENT
	     CASE CHR$(0) + CHR$(63)
		  GOSUB DELETERECORD         ' MARCAR REGISTRE
	     CASE CHR$(0) + CHR$(64)
		  CALL LISTPRO(AREA4, AREA3, DIRECC$, DEVI$, MAXLINS)' LLISTAR PROVEIDORS
	     CASE CHR$(0) + "Q"
		  COLCURS = 9: GOSUB SHOWCURSOR
		  R = R + 13
		  X = 19
		  GOSUB LISTA: X = 19
		  COLCURS = 12: GOSUB SHOWCURSOR
	     CASE ELSE
		  LOCATE , , 0
		  COLOR 15, 9: LOCATE 24, 60: PRINT "Registre: "; R; "  ";
      END SELECT
   LOOP
   RETURN

'************************************************************************
' Control del cursor per el llistat amb despla‡ament de barres
' funci¢ parescuda al DBEDIT de Clipper.
'************************************************************************

PUJA:
       IF X = 7 THEN
	  SKIP AREA3, R
	  COLCURS = 9: GOSUB SHOWCURSOR
	  IF R = 1 THEN
	     X = 7: R = 1
	     COLCURS = 12: GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 7
	  SCROLLDOWN 18, 78, 6, 1, 1
	  SKIP AREA3, R
	  COLCURS = 12: GOSUB SHOWCURSOR
       ELSE
	  SKIP AREA3, R
	  COLCURS = 9: GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  SKIP AREA3, R
	  COLCURS = 12: GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXA:
       IF X = 19 THEN
	  SKIP AREA3, R
	  COLCURS = 9: GOSUB SHOWCURSOR
	  IF EOF(AREA3) THEN
	     R = MAXPR - 1: X = 19
	     COLCURS = 12: GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = 19
	  SCROLLUP 18, 78, 6, 1, 1
	  SKIP AREA3, R
	  COLCURS = 12: GOSUB SHOWCURSOR
       ELSE
	  SKIP AREA3, R
	  COLCURS = 9: GOSUB SHOWCURSOR
	  IF EOF(AREA3) THEN
	     COLCURS = 12: GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  SKIP AREA3, R
	  COLCURS = 12: GOSUB SHOWCURSOR
       END IF
       RETURN

'************************************************************************
' Mostra el cursor a la posici¢ que est…
'************************************************************************

SHOWCURSOR:
	  COLOR 15, COLCURS
	  LOCATE X, 2: PRINT PROVEIDORS.CODI; " "; PROVEIDORS.NOM; " "; PROVEIDORS.NOM1; PROVEIDORS.MARCAT; "        "
	  RETURN

'************************************************************************
' CONSUTAR REGISTRES UN PER UN MOSTRANT LA FITXA
' DEL CLIENT
'************************************************************************

CONSULTARCLIENT:
	  'IF TESTSCREEN = &HB000 THEN SaveScreen DIRECCP$ + "PROVEI.SCR", 0, 4100 ELSE CALL SAVESCRN

	  GOSUB INITCAMPS
	  SETMAXCAMPS 14

	  FOR C = 0 TO 14: SETCOLORCAMPS C, 15, 12, 15, 9, 14, 9: NEXT
	  FOR C = 0 TO 14: ERASEFIELD C: NEXT

	  RV = R
	  SKIP AREA3, R: RETRIEVE AREA3, PROVEIDORS
	  GOSUB MASCARA
	  GOSUB MOSTRA
	  DO
	    RETRIEVE AREA3, PROVEIDORS
	    GOSUB MOURECAMPS
	    GOSUB MOSTRA
	    COLOR 14: LOCATE 5, 11: PRINT "Prove‹dor No.: "; : COLOR 15: PRINT RV
	    T$ = INKEY$
	    SELECT CASE T$
		   CASE CHR$(0) + "P"      ' BAIXA UN REGISTRE
			RV = RV + 1: MOVENEXT AREA3
			IF EOF(AREA3) THEN
			   BEEP: MOVELAST AREA3: RV = RV - 1
			END IF
		   CASE CHR$(0) + "H"      ' PUJA UN REGISTRE
			RV = RV - 1: MOVEPREVIOUS AREA3
			IF BOF(AREA3) THEN
			   BEEP: MOVEFIRST AREA3: RV = RV + 1
			END IF
		   CASE CHR$(27)

			'IF TESTSCREEN = &HB000 THEN RestoreScreen DIRECCP$ + "PROVEI.SCR", 0 ELSE CALL RESTSCRN

			FOR C = 0 TO 13: ERASEFIELD C: NEXT
			RETURN
		   CASE ELSE
	    END SELECT
	  LOOP

MOSTRA:
	  COLOR 15, 9
	  DISPLAYALL
	  RETURN

'************************************************************************
' INSERTAR CLIENT
'************************************************************************

INSERTARCLIENT:
	  'IF TESTSCREEN = &HB000 THEN SaveScreen DIRECCP$ + "PROVEI.SCR", 0, 4100 ELSE CALL SAVESCRN
	  GetBackground 1, 1, 25, 80, PRO$

	  RV = MAXPR                         ' DESIGNAR EL REGISTRE NOU
	  GOSUB MASCARA
	  SETMAXCAMPS 14
	  FOR C = 0 TO 14: SETCOLORCAMPS C, 15, 12, 15, 9, 14, 9: NEXT
	  
	  GOSUB INITCAMPS: COLOR 15, 9
	  GOSUB BUIDARCAMPS                 ' BUIDAR CAMPS
	  INSERTCAMP 0, RTRIM$(LTRIM$(STR$(RV)))
	  DISPLAYALL
	  GOSUB READCAMPS                   ' INTRODU‹R DADES ALS CAMPS
	  GOSUB MOUREFITXER                 ' MOURE LES DADES DELS CAMPS AL FITXER
	  DISPLAYALL

	  IF ANUL = 1 THEN                  ' SI ES PITJA <ESC> A UN DELS CAMPS
	     'IF TESTSCREEN = &HB000 THEN RestoreScreen DIRECCP$ + "PROVEI.SCR", 0 ELSE CALL RESTSCRN
	     PutBackground 1, 1, PRO$
	     FOR C = 0 TO 14: ERASEFIELD C: NEXT
	     RETURN
	  END IF
	  RETRIEVE AREA3, PROVEIDORS
	  INSERT AREA3, PROVEIDORS

	  CEMPRE.MAXPROVEID = MAXPR + 1
	  MAXPR = MAXPR + 1
	  PUT AREA, 1, CEMPRE

	  'IF TESTSCREEN = &HB000 THEN RestoreScreen DIRECCP$ + "PROVEI.SCR", 0 ELSE CALL RESTSCRN
	  PutBackground 1, 1, PRO$
	  COLCURS = 12: GOSUB SHOWCURSOR: FOR C = 0 TO 14: ERASEFIELD C: NEXT
	  RETURN

'************************************************************************
' EDITAR CLIENT
'************************************************************************

EDITARCLIENT:
	  'F TESTSCREEN = &HB000 THEN SaveScreen DIRECCP$ + "PROVEI.SCR", 0, 4100 ELSE CALL SAVESCRN
	  GetBackground 1, 1, 25, 80, PRO$
	  RV = R
	  GOSUB MASCARA
	  SKIP AREA3, RV
	  RETRIEVE AREA3, PROVEIDORS
	  SETMAXCAMPS 14
	  FOR C = 0 TO 14: SETCOLORCAMPS C, 15, 12, 15, 9, 14, 9: NEXT
	  GOSUB INITCAMPS
	  GOSUB MOURECAMPS
	  COLOR 15, 9: CALL DISPLAYALL: EDIT = 99
	  GOSUB READCAMPS
	  EDIT = 0
	  IF ANUL = 1 THEN           ' SI ES PITJA <ESC> A UN DELS CAMPS
	     'IF TESTSCREEN = &HB000 THEN RestoreScreen DIRECCP$ + "PROVEI.SCR", 0 ELSE CALL RESTSCRN
	     PutBackground 1, 1, PRO$
	     FOR C = 0 TO 14: ERASEFIELD C: NEXT
	     RETURN
	  END IF
	  SKIP AREA3, RV
	  GOSUB MOUREFITXER             ' MOURE LES DADES DELS CAMPS AL FITXER
	  UPDATE AREA3, PROVEIDORS
	  CHECKPOINT

TORNAR:
	  FOR C = 0 TO 14: ERASEFIELD C: NEXT
	  PutBackground 1, 1, PRO$
	  RETURN


'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

READCAMPS:
       CORRECTE = 0: ANUL = 0
       SETMAXCAMPS 14
       DO
	  FOR C = 0 TO 14
	     VALUE = LLEGIRCAMP(C)
	     SELECT CASE VALUE
		    CASE 0
			  'CAMPTEMP$ = SHOWFIELD$(0)
			  'IF FindProveidor%(FORAESPAI$(CAMPTEMP$), INDEX(), MAXPR) THEN
			  '   IF PROVEIDORS.MARCAT <> "*" THEN
			  '      BEEP: LOCATE 5, 50: COLOR 28, 9: PRINT "<< AQUEST CODI EXISTEIX >>"
			  '      C = C - 1
			  '   END IF
			  'END IF
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
	  FOR C = 0 TO 14: ERASEFIELD C: NEXT
	  INITCAMP 0, 7, 28, ASCI, 0, "XXXXXXX", "Codi proveidor:"
	  INITCAMP 1, 8, 28, ASCI, 0, "XXXXXXXXXXXXXXXXXXXX", "Nom:"
	  INITCAMP 2, 8, 58, ASCI, 0, "XXXXXXXXXXX", "N.I.F.:"
	  INITCAMP 3, 9, 28, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", "Nom 2:"
	  INITCAMP 4, 11, 28, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", "Direcci¢:"
	  INITCAMP 5, 12, 28, ASCI, 0, "XXXXXXX", "Codi Postal:"
	  INITCAMP 6, 12, 48, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXX", "Poblaci¢:"
	  INITCAMP 7, 13, 28, ASCI, 0, "XXXXXXXXXXXXXXX", "TelŠfon 1:"
	  INITCAMP 8, 13, 57, ASCI, 0, "XXXXXXXXXXXXXXX", "TelŠfon 2:"
	  INITCAMP 9, 14, 28, ASCI, 0, "XXXXXXXXXXXXXXX", "TelŠfon 3:"
	  INITCAMP 10, 14, 57, ASCI, 0, "XXXXXXXXXX", "Pais:"
	  INITCAMP 11, 15, 28, ASCI, 0, "XXXXXXXXXXXXXXX", "Fax 1:"
	  INITCAMP 12, 15, 57, ASCI, 0, "XXXXXXXXXXXXXXX", "Fax 2:"
	  INITCAMP 13, 16, 28, ASCI, 0, STRING$(20, "X"), "Provincia:"
	  INITCAMP 14, 18, 28, ASCI, 0, "XXXXXXXXXX", "Compte:"
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPS:
	  INSERTCAMP 0, PROVEIDORS.CODI
	  INSERTCAMP 1, PROVEIDORS.NOM
	  INSERTCAMP 2, PROVEIDORS.NIF
	  INSERTCAMP 3, PROVEIDORS.NOM1
	  INSERTCAMP 4, PROVEIDORS.DIRECCIO
	  INSERTCAMP 5, PROVEIDORS.CPOSTAL
	  INSERTCAMP 6, PROVEIDORS.POBLACIO
	  INSERTCAMP 7, PROVEIDORS.TEL1
	  INSERTCAMP 8, PROVEIDORS.TEL2
	  INSERTCAMP 9, PROVEIDORS.TEL3
	  INSERTCAMP 10, PROVEIDORS.PAIS
	  INSERTCAMP 11, PROVEIDORS.FAX1
	  INSERTCAMP 12, PROVEIDORS.FAX2
	  INSERTCAMP 13, PROVEIDORS.PROVINCIA
	  INSERTCAMP 14, PROVEIDORS.COMPTA
	  RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXER:
	   PROVEIDORS.REGISTRE = RV
	   PROVEIDORS.CODI = RTRIM$(SHOWFIELD(0))
	   PROVEIDORS.NOM = SHOWFIELD$(1)
	   PROVEIDORS.NIF = SHOWFIELD$(2)
	   PROVEIDORS.NOM1 = SHOWFIELD$(3)
	   PROVEIDORS.DIRECCIO = SHOWFIELD$(4)
	   PROVEIDORS.CPOSTAL = SHOWFIELD$(5)
	   PROVEIDORS.POBLACIO = SHOWFIELD$(6)
	   PROVEIDORS.TEL1 = SHOWFIELD$(7)
	   PROVEIDORS.TEL2 = SHOWFIELD$(8)
	   PROVEIDORS.TEL3 = SHOWFIELD$(9)
	   PROVEIDORS.PAIS = SHOWFIELD$(10)
	   PROVEIDORS.FAX1 = SHOWFIELD$(11)
	   PROVEIDORS.FAX2 = SHOWFIELD$(12)
	   PROVEIDORS.PROVINCIA = SHOWFIELD$(13)
	   PROVEIDORS.COMPTA = SHOWFIELD$(14)
	   PROVEIDORS.MARCAT = "-"
	   RETURN

'************************************************************************
' BUIDAR ELS CAMPS DEL FITXER
'************************************************************************
BUIDARCAMPS:
	   PROVEIDORS.CODI = STRING$(LEN(PROVEIDORS.CODI), CHR$(32))
	   PROVEIDORS.NOM = ""
	   PROVEIDORS.NOM1 = ""
	   PROVEIDORS.COMPTA = ""
	   PROVEIDORS.NIF = ""
	   PROVEIDORS.DIRECCIO = ""
	   PROVEIDORS.CPOSTAL = ""
	   PROVEIDORS.POBLACIO = ""
	   PROVEIDORS.TEL1 = ""
	   PROVEIDORS.TEL2 = ""
	   PROVEIDORS.TEL3 = ""
	   PROVEIDORS.FAX1 = ""
	   PROVEIDORS.FAX2 = ""
	   PROVEIDORS.PAIS = ""
	   PROVEIDORS.PROVINCIA = ""
	   PROVEIDORS.MARCAT = "-"
	   FOR C = 0 TO 14: ERASEFIELD C: NEXT
	   RETURN

'************************************************************************
' FITXA DE UN CLIENT
'************************************************************************

MASCARA:
	  COLOR 15, 9
	  FINESTRA 4, 10, 20, 76, 1, CAIXA1: COLOR 14
	  LOCATE 5, 11: PRINT "Prove‹dor No.: "; : COLOR 15: PRINT RV
	  LOCATE 6, 11: PRINT STRING$(65, "Ä"); : COLOR 14
	  COLOR 15: LOCATE 10, 11: PRINT STRING$(65, "Ä"); : COLOR 14
	  COLOR 15: LOCATE 17, 11: PRINT STRING$(65, "Ä"); : COLOR 14
	  RETURN

'************************************************************************
' LLISTA UN BLOC
'************************************************************************

LISTA:
    COLOR 15, 9: X = 7
    MOVEFIRST AREA3
    FOR X = 7 TO 19
       'IF EOF(AREA3) THEN R = R - 1: EXIT FOR

       'SKIP AREA3, R: COLOR 15, 9
       MOVENEXT AREA3
       COLCURS = 9: GOSUB SHOWCURSOR
       'R = R + 1
    NEXT
    RETURN

'************************************************************************
' MARCAR UN REGISTRE COM A BORRAT
'************************************************************************

DELETERECORD:

    DELETE AREA3

    RETURN

'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE PROVEIDORS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA3 = FREEFILE: OPEN DIRECC$ + "PROVEIDO.BDD" FOR ISAM PROVEID "ProveidorsBook" AS AREA3
      AREA4 = FREEFILE: OPEN DIRECC$ + "PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CAP)
      CREATEINDEX AREA3, "C", 0, "CODI"
      SETINDEX AREA3, "C"

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
      MAXPR = CEMPRE.MAXPROVEID
      RETURN

TANCA:
      FOR C = 0 TO 14: ERASEFIELD C: NEXT
      DELETEINDEX AREA3, "C"
      ERASE CAMPS
      RESET
      EXIT SUB

END SUB

SUB MASCCLIS
    COLOR 15, 9
    FINESTRA 4, 1, 20, 80, 1, CAIXA1: COLOR 14
    LOCATE 5, 2: PRINT "Codi    Nom prove‹dor" + SPACE$(41) + "       Baixa": COLOR 15
    LOCATE 6, 2: PRINT STRING$(78, "Ä");
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR PROVEIDORS <F2>=INSERTAR PROVEIDORS  <" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR                 <F3>=SUMAR                <F4>=CONSULTAR"
    LOCATE 24, 2: PRINT "<F5>=MARCAR PROVEIDORS       <F6>=LLISTAR";
END SUB

SUB SKIP (A, R)
    MOVEFIRST A
    FOR BOTS = 1 TO R - 1: MOVENEXT A: NEXT
END SUB

SUB SortProveidors (INDEX() AS INDEXPROVEIDOR, MAXPR) STATIC
    
    OFFSET = MAXPR \ 2

    DO WHILE OFFSET > 0
       LIMIT = MAXPR - OFFSET
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

