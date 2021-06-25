' ********************************************************************
'
' Fitxer....................: ALT_FACT.BAS
' Titol.....................: Modul per el mateniment de factures
'
' ********************************************************************
'
' Data inici................: 22/09/1996 23:30:00
' Data de la darrera revisi¢: 09/02/1997 18:57:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: DRAC Software 1993/97 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB GuardaAlbaran (RG!, MAXLIN!, AREANUM!, TIPUS%, GUARDAT!, EDIT!)
DECLARE SUB CrearAlbarans (MAX!, DTO!, IVA!, AREANUM!, AREA3!, AREA2!, AREA4!, AREAST!, MAXST!, MAXAL!, EDIT!, DH$, DEVI$)
DECLARE FUNCTION INICIAREF! (AREANUM!, R!, MARCAT!, REFOLD$)
DECLARE SUB ReadCalculFactura ()
DECLARE SUB LLISTARALBARAN (AREA5!, AREA2!, DEVI$)
DECLARE SUB SUMARALBARANS (MAXAL!, AREA2!)
DECLARE SUB IMPRIMIRALBARAN (MAX!, DEVI$)
DECLARE SUB MASCALBA (MAX!)
DECLARE SUB MASCALBS ()
DECLARE SUB MASCLIST ()
DECLARE SUB CARREGARALBARAN (R!, MAXLIN!)
DECLARE SUB OrdenarIndex (INDEX() AS ANY, MAXST!)
DECLARE SUB INITALBARAN (MAXLIN!)
DECLARE SUB READCAPSALERA (MAXAL!, MAXCL!)
DECLARE SUB REPINTACAPSALERA (MAX!)
DECLARE SUB ReadObserva ()
DECLARE SUB AlbaransEdit (MAX!, DTO!, IVA!, DIRECC$, DIRCP$, DIRECI$, DEVI$, DH$, IMPRESORA!)
DECLARE FUNCTION POSSAESPAI$ (CAD$, C!)
DECLARE FUNCTION CercarRecord% (CAMP$, INDEX() AS ANY, MAXST!, AREAST!)

COMMON SHARED DIRECC$, DIRECCP$, DIRECCT$, DOCU$, DIRECCH$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5, AREANDX
COMMON SHARED GUARDAT, MIDPOINT, MAXCL, MAXAL, R, EDIT
COMMON SHARED TOTAL, MAX, NOU, TROBAT, DEVICE$, DEVI$, UNIDAD$
COMMON SHARED COL()

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT2\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT2\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT2\FONTS\STOCK.BI'
'$INCLUDE: 'C:\FACT2\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT2\FONTS\CAMPS.BI'
'$INCLUDE: 'C:\FACT2\FONTS\WIN.BI'


'$DYNAMIC
DIM SHARED FLD(6) AS CN           ' CONFIGURACI¢ DE CAMPS
DIM SHARED MASC$(6)               ' CONFIGURACI¢ DE MASCARES
DIM SHARED LIN$(100, 7)           ' LINIES DE LA FACTURA TEMPORAL
DIM SHARED OBS$(1)                ' LINIES D'OBSERVACIO
DIM SHARED COL(2, 1)
DIM SHARED CLIENT AS CLIENT
DIM SHARED CEMPRE AS CONTROLEMP
DIM SHARED ALBARAN AS ALBAR
DIM SHARED FACTURA AS FACTU
DIM SHARED LINFACT AS LINIAFACT
DIM SHARED TEXTOSMENUP(6) AS STRING
DIM SHARED STOCK AS STK
DIM SHARED DOCNUM AS DN
DIM SHARED CAP AS CAPSALDOCS
DIM SHARED SPOOLIMP AS SPOOL
DIM SHARED ARCHIMP AS IMPRESORA
DIM SHARED CFG AS CONFIG
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
DIM SHARED USR AS USUARIS
DIM COLORS AS COLO


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
	 COL(2, 0) = 15: COL(2, 1) = 0
      ELSE
	 AREAC = FREEFILE: OPEN DIRECCF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
	 GET AREAC, 1, COLORS
	 COL(0, 0) = COLORS.COL(0, 0): COL(0, 1) = COLORS.COL(0, 1)
	 COL(1, 0) = COLORS.COL(1, 0): COL(1, 1) = COLORS.COL(1, 1)
	 COL(2, 0) = COLORS.COL(2, 0): COL(2, 1) = COLORS.COL(2, 1)
	 CLOSE AREAC
      END IF
      CALL AlbaransEdit(MAXFAC, INT(EMPRES.DTO), INT(EMPRES.IVA), DIRECCF$, DIRECCP$, DIRECCI$, DIRECCH$, DEV$, IMPRESORA)
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

REM $STATIC
SUB AlbaransEdit (MAX, DTO, IVA, DIRECC$, DIRCP$, DIRECI$, DH$, DEVI$, IMPRESORA)
    GOSUB OBRIFITXERS

    DIM NDXFILE AS INDEXTYPE
    
    X = 5: R = 1: L = 1: R = 1

    DIRECCT$ = DIRECC$ + "TEXTOS\"

    SELECT CASE COMMAND$
	   CASE "AUTOMATIC"
		GOSUB DEFWINDOWS
		CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXST, MAXAL, 0, DH$, DEVI$)
		PackWindows
		RESET
		EXIT SUB
	   CASE "MANTENIMENT"
		GOSUB DBEDIT
	   CASE ELSE
		PRINT "Falten par…metres"
		EXIT SUB
    END SELECT
		
DBEDIT:
    GOSUB DEFWINDOWS

    MASCALBS                          ' PINTA LA MASCARA DE LA LLISTA
    SetScoreBoard SON                 ' ACTIVA EL VISOR D'ESTAT DE LES TECLES
				      ' DE CONTROL
    GOSUB LISTA
    X = 5: R = 1                      ' INICIA CURSOR I POSICIONS A PANTALLA
    GET AREA2, R, FACTURA
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
    DO
      OP$ = INKEY$
      COLOR COL(0, 0), COL(0, 1): CALL EstatTeclesControl(25, 3)
      SELECT CASE OP$
	     CASE CHR$(0) + "P"      ' BAIXA UN REGISTRE
		  GOSUB BAIXACURSOR
	     CASE CHR$(0) + "H"      ' PUJA UN REGISTRE
		  GOSUB PUJACURSOR
	     CASE CHR$(27)
		  DeleteWindow 0
		  PackWindows
		  SetScoreBoard SOFF: RESET
		  EXIT SUB
	     CASE CHR$(13)
		  XOLD = X: ROLD = R
		  CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXST, MAXAL, 1, DH$, DEVI$)
		  MASCALBS
		  X = 5: R = 1
		  GOSUB LISTA
		  X = XOLD: R = ROLD: GET AREA2, R, FACTURA: COLOR COL(1, 0), COL(1, 1): : GOSUB SHOWCURSOR
	     CASE CHR$(0) + CHR$(60)
		  XOLD = X: ROLD = R
		  CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXST, MAXAL, 0, DH$, DEVI$)
		  MASCALBS
		  X = 5: R = 1
		  GOSUB LISTA
		  X = XOLD: R = ROLD: GET AREA2, R, FACTURA: COLOR COL(1, 0), COL(1, 1): : GOSUB SHOWCURSOR
	     CASE CHR$(0) + CHR$(61)
		  SUMARALBARANS MAXAL, AREA2
	     CASE CHR$(0) + CHR$(63)
		  GOSUB DELETERECORD         ' MARCAR REGISTRE
	     CASE CHR$(0) + CHR$(64)
		  GetBackground 1, 1, 24, 79, FACTU$
		  CALL LLISTARALBARAN(AREA5, AREA2, DEVI$)
		  PutBackground 1, 1, FACTU$
	     CASE CHR$(0) + CHR$(65)
		  CALL CARREGARALBARAN(R, MAX)
		  CALL IMPRIMIRALBARAN(MAX, DEVI$)
	     'CASE CHR$(0) + "Q"
	     '     COLCURS = 9: GOSUB SHOWCURSOR
	     '     R = R + 13: X = 19
	     '     GOSUB LISTA: X = 19
	     '     COLCURS = COL(1,1): GOSUB SHOWCURSOR
	     CASE ELSE
      END SELECT
   LOOP
   RETURN

'************************************************************************
' Control del cursor per el llistat amb despla‡ament de barres
' funci¢ parescuda al DBEDIT de Clipper.
'************************************************************************

PUJACURSOR:
       IF X = 5 THEN
	  GET AREA2, R, FACTURA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = 1 THEN
	     X = 5: R = 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 5
	  SCROLLDOWN 18, 78, 4, 1, 1
	  GET AREA2, R, FACTURA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA2, R, FACTURA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  GET AREA2, R, FACTURA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXACURSOR:
       IF X = 19 THEN
	  GET AREA2, R, FACTURA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXAL - 1 THEN
	     R = MAXAL - 1
	     X = 19
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = 19
	  SCROLLUP 18, 78, 4, 1, 1
	  GET AREA2, R, FACTURA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA2, R, FACTURA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXAL - 1 THEN
	     R = MAXAL - 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  GET AREA2, R, FACTURA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

'************************************************************************
' Mostra el cursor a la posici¢ que est…
'************************************************************************

SHOWCURSOR:
	  IF FACTURA.MARCAT = "*" THEN COLOR COL(0, 0) XOR COL(0, 1)
	  NOMS$ = SPACE$(27)
	  NOMA$ = RTRIM$(LTRIM$(FACTURA.PERSONA.COGNOMS)) + "; " + LTRIM$(RTRIM$(FACTURA.PERSONA.NOM))
	  LN = LEN(NOMA$): IF LN > 27 THEN LN = 27
	  MID$(NOMS$, 1, LN) = NOMA$
	  LOCATE X, 2: PRINT FACTURA.REFFACTURA; " º"; NOMS$; "º";
	  PRINT USING "##,###,###.##º##.##º##.##º##,###,###.##"; FACTURA.TOTALBRUT; FACTURA.DTO; FACTURA.TIPOIVA; FACTURA.TOTALNET
	  RETURN

LISTA:
    IF MAXAL = 1 THEN
       NOU = 2
       AVIS.SONOR (1)
       AVIS "AVIS:", "No hi ha cap factura creada, ara es procedir… a crear-ne una", 0
       CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXST, MAXAL, 0, DH$, DEVI$)
       IF NOU = 9 THEN RESET: EXIT SUB
       MASCALBS
       X = 5: R = 1: GOSUB LISTA: COLCURS = COL(0, 1): GOSUB SHOWCURSOR
    END IF

    COLOR COL(0, 0), COL(0, 1)
    FOR X = 5 TO 19: LOCATE X, 2: PRINT "          º                           º             º     º     º             ": NEXT
    FOR X = 5 TO 19
       IF R = MAXAL THEN EXIT FOR
       GET AREA2, R, FACTURA
       COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
       R = R + 1
    NEXT
    RETURN

DELETERECORD:
    GET AREA2, R, FACTURA
    IF FACTURA.MARCAT = " " THEN
       FACTURA.MARCAT = "*"
       PUT AREA2, R, FACTURA
    ELSE
       FACTURA.MARCAT = " "
       PUT AREA2, R, FACTURA
    END IF

    GET AREA2, R, FACTURA
    COLCURS = COL(1, 1): GOSUB SHOWCURSOR
    RETURN

OBRIFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "FACTURES.CAB" FOR RANDOM SHARED AS AREA2 LEN = LEN(FACTURA)
      AREA3 = FREEFILE: OPEN DIRECC$ + "FACTURES.LIN" FOR RANDOM SHARED AS AREA3 LEN = LEN(LINFACT)
      AREA4 = FREEFILE: OPEN DIRECC$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CLIENT)
      AREA5 = FREEFILE: OPEN DIRECC$ + "PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA5 LEN = LEN(CAP)
      AREAST = FREEFILE: OPEN DIRECC$ + "STOCK.DAT" FOR RANDOM SHARED AS AREAST LEN = LEN(STOCK)
      AREANDX = FREEFILE: OPEN DIRECC$ + "STOCK.NDX" FOR RANDOM SHARED AS AREANDX LEN = LEN(NDXFILE)
      AREANUM = FREEFILE: OPEN DIRECC$ + "DOCUMENT.NUM" FOR RANDOM SHARED AS AREANUM LEN = LEN(DOCNUM)

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

      AREA7 = FREEFILE: OPEN DIRECI$ + FITXER$ FOR RANDOM SHARED AS AREA7 LEN = LEN(ARCHIMP)
      GET AREA7, 1, ARCHIMP
      CLOSE AREA7

'*********************************************************************
' INICIAR EL CONTROL DELS REGISTRES
'*********************************************************************

      GET AREA, 1, CEMPRE
      GET AREANUM, 1, DOCNUM

      MAXAL = CEMPRE.MAXFACTURA
      MAXCL = CEMPRE.MAXCLIENTS
      MAXST = CEMPRE.MAXSTOCK

'      DOCNUM.MAXCLIS = MAXCL
'      DOCNUM.MAXPROV = 0
'      DOCNUM.MAXRESG = 0
'      DOCNUM.MAXNUM = 2
'
'      DOCNUM.NUMCLIS = "4300000000"
'      DOCNUM.NUMREGS = "00000000-R"
'      DOCNUM.NUMPROV = "4200000000"
'
'      DOCNUM.FACTNUM(1).CONALBA = "Facturaci¢ del taller"
'      DOCNUM.FACTNUM(1).NUMALBA = "0000000-A"
'      DOCNUM.FACTNUM(1).MAXALBA = CEMPRE.MAXALBARAN
'
'      DOCNUM.FACTNUM(1).CONFACT = "Facturaci¢ del taller"
'      DOCNUM.FACTNUM(1).NUMFACT = "000000-RF"
'      DOCNUM.FACTNUM(1).MAXFACT = 1
'
'      DOCNUM.FACTNUM(2).CONALBA = "Facturaci¢ de la botiga"
'      DOCNUM.FACTNUM(2).NUMALBA = "0000000-A"
'      DOCNUM.FACTNUM(2).MAXALBA = CEMPRE.MAXALBARAN
'
'      DOCNUM.FACTNUM(2).CONFACT = "Facturaci¢ de la botiga"
'      DOCNUM.FACTNUM(2).NUMFACT = "000000-BF"
'      DOCNUM.FACTNUM(2).MAXFACT = 4
'      PUT AREANUM, 1, DOCNUM
'

      RETURN
DEFWINDOWS:
    IF SetInitWindows(3) = TRUE THEN
       AVIS "ERROR:", "Mem•ria insuficient!!!", 0
       RETURN
    END IF

    WIN = InitNewWindow(0, 1, 1, 25, 80, 0, "Factura")
    WIN2 = InitNewWindow(1, 2, 1, 20, 80, 0, "Manteniment de Factures")
    WIN3 = InitNewWindow(2, 9, 10, 17, 50, 1, "Suma dels totals")

    SetAllColors COL(0, 0), COL(0, 1), COL(1, 1), COL(1, 0), COL(2, 0), COL(2, 1)
    SetStyleWindow 0, 0, ""
    SetStyleWindow 1, 0, ""
    SetStyleWindow 2, 1, ""
    RETURN

END SUB

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

FUNCTION CercarRecord% (CAMP$, INDEX() AS INDEXTYPE, MAXST, AREAST) STATIC
	 SHARED MIDPOINT

	 TOPRECORD = MAXST - 1
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
	    GET AREAST, INDEX(MIDPOINT).REGISTRE, STOCK
	    CercarRecord% = TRUE
	 ELSE
	    CercarRecord% = FALSE
	 END IF
END FUNCTION

SUB CrearAlbarans (MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXST, MAXAL, EDIT, DH$, DEVI$)
       SHARED LIN$(), TEXTOSMENUP() AS STRING
       SHARED UNIDAD$, DIRECCH$
      
       DIM MA$(2)
'*********************************************************************
' INDEXAR FITXER DEL STOCK
'*********************************************************************
       GetBackground 10, 30, 15, 48, Bt$
       COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 47, 1, CAIXA1
       COLOR 31, COL(0, 1): LOCATE 11, 31: PRINT "ESPERA UN MOMENT"
       COLOR 31, COL(0, 1): LOCATE 13, 31: PRINT " PROCESSANT ... "
       DIM INDEX(1 TO MAXST) AS INDEXTYPE
       DIM M$(2)
       DIM NDXFILE AS INDEXTYPE
       GOSUB CARREGARINDEX
       PutBackground 10, 30, Bt$

'*********************************************************************

       FLD(1).XCAMP = 2: FLD(1).LON = 18: FLD(1).TIPUS = ASCI: FLD(1).MASK = STRING$(18, "X")
       FLD(2).XCAMP = 21: FLD(2).LON = 40: FLD(2).TIPUS = ASCI: FLD(2).MASK = STRING$(40, "X")
       FLD(3).XCAMP = 51: FLD(3).LON = 9: FLD(3).TIPUS = NUM: FLD(3).MASK = "999999999"
       FLD(4).XCAMP = 61: FLD(4).LON = 8: FLD(4).TIPUS = NUM: FLD(4).MASK = "99999.99"

'*********************************************************************
' INICIAR MASCARES DELS CAMPS
'*********************************************************************
       MASC$(1) = "\              \"             ' CODI ARTICLE
       MASC$(2) = "\" + SPACE$(27) + "\"         ' CONCEPTE
       MASC$(3) = "#,##,###"                     ' PREU
       MASC$(4) = "#,###.##"                     ' QUANTITAT
       MASC$(5) = "##,###,###"                   ' IMPORT
       MASCTOTAL$ = "##,###,###.##"              ' TOTAL

'*********************************************************************
' MIRA A VEURA SI HA DE EDITAR ALGUN REGISTRE
'*********************************************************************
       TROBAT$ = "*"
       IF EDIT = 1 THEN
	  CALL CARREGARALBARAN(R, MAX): Q = R
	  IF FACTURA.DOCUMENT = "A" THEN DOCU$ = "ALBAR…"
	  IF FACTURA.DOCUMENT = "F" THEN DOCU$ = "FACTURA"
       ELSE

	  FOR Q = 1 TO MAXAL - 1
	      GET AREA2, Q, FACTURA
	      IF FACTURA.MARCAT = "*" THEN
		 TROBAT$ = "Z"
		 EXIT FOR
	      END IF
	  NEXT

	  IF TROBAT$ = "Z" THEN   ' SI HA TROBAT ALGUN REGISTRE MARCAT
	     R = Q
	     REFOLD$ = FACTURA.REFFACTURA
	  ELSE
	     R = MAXAL
	  END IF
	  DOCU$ = "FACTURA"

	  INITALBARAN (MAX)    ' POSSA A PUNT LES VARIABLES DEL FACTURA
	  LIN$(1, 6) = "-"     ' DEIXA UNA LINIA DISPONIBLE PER EDITAR-LA
       END IF


'*********************************************************************
' INICIALITZAR VALORS DE LA FACTURA
'*********************************************************************
       '
       ' INICIA N£MERO DE REFERŠNCIA
       '
       IF EDIT = 0 THEN
	  IF TROBAT$ = "Z" THEN MARCAT = TRUE ELSE MARCAT = FALSE
	  TIPUS% = INICIAREF(AREANUM, R, MARCAT, REFOLD$)
	  IF NOU = 9 THEN EXIT SUB
       END IF
       
       MASCALBA (MAX)                               ' PINTAR MASCARA
       IF EDIT = 1 THEN GOSUB LLISTA.TROS.ALBARAN   ' SI L'ALBARA S'HA DE MODIFICAR QUE LLISTI UN TROS DEL ALBARA
       CALL READCAPSALERA(MAXAL, MAXCL)             ' DEMANAR DADES DE LA CAP€ALERA

       LOCATE 23, 2: PRINT "ENTER=EDITAR LINIA    ESC=ANULAR    SUPR=SUPRIMIR LINIA";
       LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR    CTRL+F=ACABAR DE FACTURAR";

       L = 1: C = 1: YC = 9: MODIF = 0: GUARDAT = 0
       GOSUB MARCACAMP      ' MARCA CAMP DE CODI ARTICLE

       DO
	 T$ = INKEY$
	 SELECT CASE T$
		CASE CHR$(0) + "S"
		     GOSUB BORRA
		CASE CHR$(0) + "H"
		     GOSUB PUJA
		CASE CHR$(0) + "P"
		     GOSUB BAIXA
		CASE CHR$(13)
		     GOSUB LLEGEIXCAMP
		     GOSUB MARCACAMP
		CASE CHR$(6)
		     GetBackground 1, 1, 24, 79, RESBUF$
		     IF MODIF = 0 THEN
			ReadObserva                  ' EDITAR OBSERVACIONS
			FACTURA.TOTALBRUT = TOTAL
			FACTURA.DTO = DTO
			FACTURA.TIPOIVA = IVA
			ReadCalculFactura            ' CALCULAR L'IVA I EL DTO DE LA FACTURA
		     ELSE
			IF MODIF = 1 THEN ReadCalculFactura  ' CALCULAR L'IVA I EL DTO DE LA FACTURA
		     END IF

		     DO
		       TEXTOSMENUP(1) = " MODIFICAR CAP€ALERA    "
		       TEXTOSMENUP(2) = " MODIFICAR LINIES       "
		       TEXTOSMENUP(3) = " MODIFICAR OBSERVACIONS "
		       TEXTOSMENUP(4) = " IMPRIMIR FACTURA       "
		       TEXTOSMENUP(5) = " GUARDAR FACTURA        "
		       COLOR COL(1, 1), CCT
		       CALL MENU2(TEXTOSMENUP(), CASO%, 17, 23, LEN(TEXTOSMENUP(1)) + 2, 5, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
		       SELECT CASE CASO%
			      CASE 1
				   CALL READCAPSALERA(MAXAL, MAXCL)
			      CASE 2
				   MODIF = 1
				   PutBackground 1, 1, RESBUF$
				   REPINTACAPSALERA (MAXAL)
				   EXIT DO
			      CASE 3
				   PutBackground 1, 1, RESBUF$
				   REPINTACAPSALERA (MAXAL)
				   ReadObserva
			      CASE 4
				   CALL IMPRIMIRALBARAN(MAX, DEVI$)
			      CASE 5
				   IF EDIT = 1 THEN
				      RG = Q
				   ELSE
				      RG = MAXAL
				      IF TROBAT$ = "Z" THEN RG = Q
				   END IF
				   CALL GuardaAlbaran(RG, MAX, AREANUM, TIPUS%, GUARDAT, EDIT)
			      CASE 999
				   IF NOU = 2 THEN
				      ERASE INDEX
				      NOU = 9
				      EXIT SUB
				   ELSE
				      IF NOU = 3 THEN
					 ERASE INDEX
					 GOSUB GUARDADAR.NUMERO.ALBARA
					 PutBackground 1, 1, RESBUF$
					 EXIT SUB
				      END IF
				   END IF
				   ERASE INDEX
				   GOSUB GUARDADAR.NUMERO.ALBARA
			      CASE ELSE
		       END SELECT
		     LOOP
		CASE CHR$(27)
		     IF NOU = 2 THEN        ' SI ES EL PRIMER FACTURA QUE ES FA
			ERASE INDEX
			DeleteWindow 0
			NOU = 9
			EXIT SUB
		     ELSE
			ERASE INDEX
			DeleteWindow 0
			IF NOU = 3 THEN EXIT SUB ELSE EXIT SUB
		     END IF
		CASE ELSE
		     COLOR COL(2, 0), COL(0, 1): LOCATE 20, 2: PRINT "Linia: "; : COLOR COL(2, 0) XOR COL(2, 1): PRINT L;
		     LOCATE 20, 53: PRINT VAL(LIN$(L, 7));
		     LOCATE 20, 20: PRINT LIN$(L, 6)
	 END SELECT
       LOOP

'*********************************************************************
' CONTROL DEL CURSOR PER L'FACTURA
'*********************************************************************

BORRA:
	 IMPORT = VAL(LIN$(L, 5))
	 TOTAL = TOTAL - IMPORT
	 IF TOTAL < 0 THEN TOTAL = 0

	 FOR CB = 1 TO 4
	     X = FLD(CB).XCAMP: LE = FLD(CB).LON
	     LIN$(L, CB) = SPACE$(LE)
	     COLOR COL(1, 1), CCT: LOCATE YC, X: PRINT STRING$(LE, " ");
	 NEXT
	 
	 IF LIN$(L, 6) <> "-" THEN
	    LIN$(L, 6) = "*"
	 END IF
	 LIN$(L, 5) = ""
	 IF LIN$(1, 6) = "-" THEN LIN$(1, 6) = "*"

	 LOCATE YC, 61: PRINT "        "
	 LOCATE YC, 70: PRINT "          "
	 COLOR COL(0, 0) XOR COL(0, 1), COL(0, 1)
	 LOCATE 20, 67: PRINT USING MASCTOTAL$; TOTAL
	 GOSUB MOSTRA
	 GOSUB MARCACAMP
	 RETURN

PUJA:
	 IF L = 1 THEN SOUND 50, .5: AVIS "AVIS:", "Principi de la factura", 0: RETURN
	 X = FLD(C).XCAMP: LE = FLD(C).LON: T = FLD(C).TIPUS
	 COLOR COL(0, 0), COL(0, 1): LOCATE YC, X: PRINT STRING$(LE, " ");
	 LOCATE YC, X, 0: PRINT LIN$(L, C)
	 C = 1: YC = YC - 1: L = L - 1
	 LOCATE 20, 20: PRINT "       ";

	 IF YC < 9 THEN
	    SCROLLDOWN 17, 78, 8, 1, 1
	    YC = 9
	    GOSUB MOSTRA
	    LOCATE 20, 20: PRINT "  AMUNT";
	 END IF
	 
	 GOSUB MOSTRA
	 GOSUB MARCACAMP
	 RETURN

BAIXA:
	 IF L = MAX THEN SOUND 50, .5: AVIS "AVIS:", "Final de la factura", 0: RETURN
	 X = FLD(C).XCAMP: LE = FLD(C).LON: T = FLD(C).TIPUS
	 COLOR COL(0, 0), COL(0, 1): LOCATE YC, X: PRINT STRING$(LE, " ");
	 LOCATE YC, X: PRINT LIN$(L, C)
	 LOCATE YC, X, 0
	 C = 1: YC = YC + 1: L = L + 1
	 LOCATE 20, 20: PRINT "       ";
	 IF YC > 18 THEN
	    SCROLLUP 17, 78, 8, 1, 1
	    YC = 18
	    GOSUB MOSTRA
	    LOCATE 20, 20: PRINT "  AVALL";
	 END IF
	 
	 GOSUB MOSTRA
	 GOSUB MARCACAMP
	 RETURN

MOSTRA:
	 COLOR COL(0, 0), COL(0, 1)
	 LOCATE YC, 2: PRINT SPACE$(18); "º"; SPACE$(29); "º"; SPACE$(9); "º"; SPACE$(8); "º"; SPACE$(10)
	 IF LIN$(L, 6) = "*" OR LIN$(L, 6) = "-" THEN
	    LOCATE YC, 2: PRINT USING MASC$(1); LIN$(L, 1)
	    LOCATE YC, 2 + 18 + 1: PRINT USING MASC$(2); LIN$(L, 2)
	    LOCATE YC, 52: PRINT USING MASC$(3); VAL(LIN$(L, 3))
	    LOCATE YC, FLD(4).XCAMP: PRINT USING MASC$(4); VAL(LIN$(L, 4))
	    LOCATE YC, 70: PRINT USING MASC$(5); VAL(LIN$(L, 5))
	    LOCATE 20, 53: PRINT VAL(LIN$(L, 7))
	 END IF
	 RETURN

MARCACAMP:
	 X = FLD(C).XCAMP: LE = FLD(C).LON: T = FLD(C).TIPUS
	 COLOR COL(1, 0), COL(1, 1): LOCATE YC, X: PRINT STRING$(LE, " ");
	 LOCATE YC, X: PRINT LIN$(L, C)
	 LOCATE YC, X, 0
	 RETURN

'*********************************************************************
' RUTINA PER LLEGIR ELS CAMPS D'UNA LINIA
'*********************************************************************

LLEGEIXCAMP:
     COLOR COL(0, 0), COL(0, 1)
     LOCATE 23, 2: PRINT SPACE$(70);
     LOCATE 24, 2: PRINT SPACE$(70);
     LOCATE 22, 2: PRINT SPACE$(70);
     IF LIN$(L, 6) <> "*" AND LIN$(L, 6) <> "-" THEN
	LOCATE 23, 2: PRINT "VES PER ORDRE !!!": BEEP
	LOCATE 23, 2: PRINT "ENTER=EDITAR LINIA    ESC=ANULAR    SUPR=SUPRIMIR LINIA";
	LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR    CTRL+F=ACABAR DE FACTURAR";
	RETURN
     END IF
     LOCATE 23, 2: PRINT "ENTER=PASSAR A L'ALTRE CAMP      ESC=SORTIR"
     SALE = 0
     TROBAT = 0
     SETMAXCAMPS 4
     FOR C = 0 TO 3
	 SETCOLORCAMPS C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
	 X = FLD(C + 1).XCAMP: T = FLD(C + 1).TIPUS: M$ = LTRIM$(RTRIM$(FLD(C + 1).MASK))
	 INITCAMP C, YC, X, T, 0, M$, ""
     NEXT

     SETCOLORCAMPS 4, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
     INITCAMP 4, 20, 53, NUM, 0, "999", ""

     FOR C = 0 TO 4
	 INSERTCAMP C, LIN$(L, C + 1): VELL$ = LIN$(L, C + 1)

	 VALUE = LLEGIRCAMP(C)
	 SELECT CASE VALUE
		CASE F1
		     IF FRE(-1) < 111000 THEN
			AVIS "ERROR FATAL:", "Mem•ria insuficient !!!", 0
			CW$ = INKEY$
		     ELSE
			SHELL "BKHLP ALBARANS": C = C - 1
		     END IF
		CASE F2
		     IF C + 1 = 1 THEN GOSUB LLISTA.STOCK
		CASE F3 TO F10
		     SOUND 50, .5
		CASE SALIR
		     LOCATE 23, 2: PRINT "ENTER=EDITAR LINIA    ESC=ANULAR    SUPR=SUPRIMIR LINIA";
		     LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR    CTRL+F=ACABAR L'ALBAR…";
		     X = FLD(1).XCAMP
		     RETURN

		CASE 0
		     CAMPTEMP$ = FORAESPAI$(SHOWFIELD$(0))
		     IF CercarRecord%(CAMPTEMP$, INDEX(), MAXST, AREAST) THEN
			IF STOCK.MARCAT <> "*" THEN
			   LIN$(L, 2) = STOCK.DESCRIPCIO
			   IF STOCK.EXISTENCIA < STOCK.STOCKMIN THEN
			      COLOR 27: BEEP: LOCATE 22, 2: PRINT "Aquest article est… baix minims            "
			   ELSE
			      IF STOCK.EXISTENCIA = 0 THEN
				 COLOR 27: BEEP: LOCATE 22, 2: PRINT "Aquest article est… amb existŠncies 0"
			      END IF
			   END IF
			   TROBAT = 999
			END IF
		     END IF
		CASE 1

		CASE 3
		     IF TROBAT = 999 THEN                        ' RESTAR AL STOCK
			LOCK AREAST, INDEX(MIDPOINT).REGISTRE
			GET AREAST, INDEX(MIDPOINT).REGISTRE, STOCK
			SAC = STOCK.EXISTENCIA - VAL(LIN$(L, 4))
			GOSUB ACTUALIZHISTORIC
			STOCK.EXISTENCIA = SAC
			STOCK.STOCKMAX = SAC
			PUT AREAST, INDEX(MIDPOINT).REGISTRE, STOCK
			UNLOCK AREAST, INDEX(MIDPOINT).REGISTRE
		     END IF
		CASE 2
		     M$(1) = "Preu Venda ": M$(2) = "Preu Compra"
		     GetBackground 1, 1, 24, 79, sfactu$
		     CALL MENU2(M$(), CASO%, INT(YC), INT(FLD(C + 1).XCAMP), 13, 2, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
		     PutBackground 1, 1, sfactu$
		     SELECT CASE CASO%
			    CASE 1
				 LIN$(L, 3) = LTRIM$(STR$(STOCK.PVPACONSE))
				 LIN$(L, 7) = ""
			    CASE 2
				 LIN$(L, 3) = LTRIM$(STR$(STOCK.PREU))
				 LIN$(L, 7) = ""
			    CASE 999
				 LIN$(L, 3) = LTRIM$(STR$(STOCK.PREU))
				 LIN$(L, 7) = ""
			    CASE ELSE
		     END SELECT
		CASE 4
		     'INSERTCAMP 4, LIN$(L, 7)
 
		     PREU = VAL(LIN$(L, 3))
		     QUANT = VAL(LIN$(L, 4))
		     DESCOMPTE = VAL(LIN$(L, 7))

		     IMPORT = QUANT * PREU - ((QUANT * PREU) * DESCOMPTE) / 100

		     LIN$(L, C + 1) = STR$(IMPORT)

		     SUBTOTAL = 0        ' TORNA A RECALCULAR ELS TOTALS
		     FOR J = 1 TO MAX
			 SUBTOTAL = SUBTOTAL + VAL(LIN$(J, 5))
		     NEXT

		     TOTAL = SUBTOTAL

		     LOCATE YC, 70: PRINT USING MASC$(5); IMPORT
		     COLOR COL(0, 0) XOR COL(0, 1), COL(0, 1)
		     LOCATE 20, 67: PRINT USING MASCTOTAL$; TOTAL

		     IF L = MAX THEN
			IF LIN$(L, 6) = "-" THEN LIN$(L, 6) = "*"
			ELSE
			   IF LIN$(L, 6) = "-" THEN
			      LIN$(L, 6) = "*"
			   END IF
			   IF LIN$(L + 1, 6) <> "-" AND LIN$(L + 1, 6) <> "*" THEN
			      LIN$(L + 1, 6) = "-"
			   END IF
			END IF

			IF YC = 18 THEN
			   SCROLLUP 17, 78, 8, 1, 1
			   YC = 18: L = L + 1: EXIT FOR
			   GOSUB MOSTRA
			ELSE
			   L = L + 1: YC = YC + 1: EXIT FOR
			END IF
		CASE ELSE
	 END SELECT
	 LIN$(L, C + 1) = SHOWFIELD$(C)


      '   IF T = ASCI THEN
      '      LOCATE YC, X: PRINT USING MASC$(C + 1); SHOWFIELD$(C)
      '   ELSE
      '      LOCATE YC, X: PRINT USING MASC$(C + 1); VAL(SHOWFIELD$(C))
      '   END IF
	  
       '  GOSUB MOSTRA

	 'COLOR COL(0, 0), COL(0, 1)
	 'LOCATE YC, 2: PRINT SPACE$(18); "º"; SPACE$(29); "º"; SPACE$(9); "º"; SPACE$(8); "º"; SPACE$(10)
	 'IF LIN$(L, 6) = "*" OR LIN$(L, 6) = "-" THEN
	 '   LOCATE YC, 2: PRINT USING MASC$(1); LIN$(L, 1)
	 '   LOCATE YC, 2 + 18 + 1: PRINT USING MASC$(2); LIN$(L, 2)
	 '   LOCATE YC, 52: PRINT USING MASC$(3); VAL(LIN$(L, 3))
	 '   LOCATE YC, FLD(4).XCAMP: PRINT USING MASC$(4); VAL(LIN$(L, 4))
	 '   LOCATE YC, 70: PRINT USING MASC$(5); VAL(LIN$(L, 5))
	 '   LOCATE 20, 53: PRINT VAL(LIN$(L, 7))
	 'END IF
	 'RETURN 

	 DISPLAYALL
      NEXT
       C = 1


      COLOR COL(0, 0), COL(0, 1): LOCATE 22, 2: PRINT SPACE$(70);
      LOCATE 23, 2: PRINT "ENTER=EDITAR LINIA    ESC=ANULAR    SUPR=SUPRIMIR LINIA"
      LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR    CTRL+F=ACABAR LA FACTURA";
      RETURN

LLISTA.TROS.ALBARAN:
      YC = 9
      FOR L = 1 TO 10
	  GOSUB MOSTRA
	  YC = YC + 1
      NEXT
      TOTAL = FACTURA.TOTALBRUT
      COLOR COL(0, 0) XOR COL(0, 1), COL(0, 1)
      LOCATE 20, 67: PRINT USING MASCTOTAL$; TOTAL
      RETURN

'<-------------------------- AQUI

LLISTA.STOCK:
       IF MAXST = 1 THEN
	  AVIS "AVIS:", "La base de dades dels articles est… buida", 0
	  RETURN
       END IF
       DIM LL$(MAXST - 1)
       GetBackground 3, 9, 21, 71, LLISTA$
       SetScoreBoard SOFF: LOCATE , , 0
       FOR Q = 1 TO MAXST - 1
	   GET AREAST, Q, STOCK
	   IF STOCK.MARCAT = " " THEN
	      CODI$ = STOCK.CODI
	      N$ = MID$(STOCK.DESCRIPCIO, 1, 38)
	      LL$(Q) = CODI$ + " " + N$
	   END IF
       NEXT
       
       ASEL = ACHOICE(3, 9, 20, 70, Q, LL$(), COL(), "Codi               Descripcio                               ", 12)
       IF ASEL = 0 OR ASEL = -14 OR ASEL = -13 THEN
	  PutBackground 3, 9, LLISTA$
	  ERASE LL$
	  RETURN
       END IF
       IF ASEL <= 0 THEN ASEL = 1
       GET AREAST, ASEL, STOCK
       LIN$(L, 1) = STOCK.CODI
       LIN$(L, 2) = STOCK.DESCRIPCIO
       LIN$(L, 3) = STR$(STOCK.PVPACONSE)

       INSERTCAMP 0, STOCK.CODI
       INSERTCAMP 1, STOCK.DESCRIPCIO
       INSERTCAMP 2, STR$(STOCK.PVPACONSE)

       PutBackground 3, 9, LLISTA$
       ERASE LL$
       RETURN

GUARDADAR.NUMERO.ALBARA:
      GetBackground 1, 1, 24, 79, FACTU$
      IF GUARDAT = 999 THEN
	 IF EDIT = 0 THEN
	    IF TROBAT$ <> "Z" THEN
	       CEMPRE.MAXFACTURA = MAXAL + 1
	       PUT AREA, 1, CEMPRE
	       MAXAL = CEMPRE.MAXFACTURA
	       EXIT SUB
	       BEEP
	    END IF
	 END IF
      ELSE
	 COLOR COL(0, 0), COL(0, 1)
	 FINESTRA 10, 20, 14, 65, 1, CAIXA1
	 COLOR COL(0, 0) XOR COL(0, 1), COL(0, 1)
	 LOCATE 11, 21: PRINT "         La factura no est… guardada": COLOR COL(2, 0), COL(2, 1)
	 LOCATE 13, 21: PRINT "    ¨ Estas segur que vols sortir (S/N) ?"
	   DO
	     T$ = INKEY$
	   LOOP UNTIL T$ <> ""
	   SELECT CASE UCASE$(T$)
		  CASE "S"
		       DeleteWindow 0
		       EXIT SUB
		  CASE "N"
		       PutBackground 1, 1, FACTU$
		       RETURN
		  CASE ELSE
		       PutBackground 1, 1, FACTU$
		       RETURN
	   END SELECT
      END IF
      RETURN

CARREGARINDEX:
      FOR RI = 1 TO MAXST                     ' COL.LOCAR A MEM•RIA L'INDEX
	  GET AREANDX, RI, NDXFILE
	  INDEX(RI).REGISTRE = NDXFILE.REGISTRE
	  INDEX(RI).CODI = NDXFILE.CODI
      NEXT
      RETURN

ACTUALIZHISTORIC:
       CHDIR MID$(DH$, 1, LEN(DH$) - 1)
       AREAHIS = FREEFILE: OPEN "TEMPORAL.DAT" FOR OUTPUT AS AREAHIS
       PRINT #AREAHIS, TIME$ + " " + DATA$(1) + " -  Sortida del stock (VENDES)-FACTURA"
       PRINT #AREAHIS, STRING$(65, "Ä")
       PRINT #AREAHIS, "Article venut a: "; LTRIM$(RTRIM$(FACTURA.PERSONA.NOM)) + " " + LTRIM$(RTRIM$(FACTURA.PERSONA.COGNOMS))
       PRINT #AREAHIS, STRING$(65, "Ä")
       PRINT #AREAHIS, USING "Codi Article...: \" + SPACE$(16) + "\"; STOCK.CODI
       PRINT #AREAHIS, USING "Descripci¢.....: \" + SPACE$(38) + "\"; STOCK.DESCRIPCIO
       PRINT #AREAHIS, ""
       PRINT #AREAHIS, USING "Quant. Anterior: ##,###.##       "; STOCK.EXISTENCIA
       PRINT #AREAHIS, USING "Quant. Venuda..: ##,###.##       "; VAL(LIN$(L, 4))
       PRINT #AREAHIS, USING "Quant. Actual..: ##,###.##       "; SAC
       PRINT #AREAHIS, STRING$(65, "Ä")
       CLOSE #AREAHIS
       IF DIR$("STOCK.DAT") = "" THEN
	  SHELL "COPY TEMPORAL.DAT STOCK.DAT /A /Y >nul"
       ELSE
	  SHELL "COPY STOCK.DAT+TEMPORAL.DAT /A /Y >nul"
       END IF
       KILL "TEMPORAL.DAT"
       CHDIR UNIDAD$ + "\MAIN"
       RETURN
END SUB

SUB GuardaAlbaran (RG, MAXLIN, AREANUM, TIPUS%, GUARDAT, EDIT)

    IF FACTURA.MARCAT = "*" THEN FACTURA.MARCAT = " "
    FACTURA.DOCUMENT = "F": NOU = 3

    IF GUARDAT <> 999 AND EDIT = 0 THEN
       GET AREANUM, 1, DOCNUM
       DOCNUM.FACTNUM(TIPUS%).MAXFACT = DOCNUM.FACTNUM(TIPUS%).MAXFACT + 1
       PUT AREANUM, 1, DOCNUM
    END IF

    PUT AREA2, RG, FACTURA
    FOR J = 1 TO MAXLIN
	LINFACT.LINIA(J).CODART = LIN$(J, 1)
	LINFACT.LINIA(J).CONCEPTE = LIN$(J, 2)
	LINFACT.LINIA(J).PREU = VAL(LIN$(J, 3))
	LINFACT.LINIA(J).QUANTI = VAL(LIN$(J, 4))
	LINFACT.LINIA(J).DTO = VAL(LIN$(J, 7))
	LINFACT.LINIA(J).IMPORT = VAL(LIN$(J, 5))
	LINFACT.LINIA(J).MARCAR = LIN$(J, 6)
    NEXT
    PUT AREA3, RG, LINFACT
    GUARDAT = 999

END SUB

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

    MENU(1) = "  DOCUMENT TIPUS PER DEFECTE      "
    MENU(2) = "  DOCUMENT TIPUS 1                "
    MENU(3) = "  DOCUMENT AMB PAPER PRE-IMPRES   "
    MENU(4) = "  DOCUMENT COMPACTAT (RESGUARDS)  "
    MENU(5) = "  DOCUMENT PER DEFECTE P…GINA 1/5 "

    GetBackground 1, 1, 24, 79, FACTU$
    CALL MENU2(MENU(), CASO%, 17, 23, LEN(MENU(1)) + 2, 5, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
    IF CASO% = 999 THEN
       'IF TESTSCREEN = &HB000 THEN RestoreScreen DIRECCP$ + "FACTUR.SCR", 0 ELSE CALL RESTSCRN
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
       IF CASO% = 5 THEN MAXLINS = 10 ELSE MAXLINS = 29
    END IF
    LI = 1: L = 1

    DO
	IF LI = MAXLINS THEN
	   IF CASO% <> 3 THEN
	      FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
	      PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + " " + STRING$(80, "Ä")
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
		       PRINT #AREATXT, USING " \                \"; LIN$(L, 1);
		       PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); LIN$(L, 2); RTRIM$(ARCHIMP.NOCOMPRIMIDO);
		       PRINT #AREATXT, USING "##,###.#"; VAL(LIN$(L, 4));
		       PRINT #AREATXT, USING "###,###.##"; VAL(LIN$(L, 3));
		       PRINT #AREATXT, USING "  ##"; VAL(LIN$(L, 7));
		       PRINT #AREATXT, USING "##,###,###.##"; VAL(LIN$(L, 5))
		  CASE ELSE
	   END SELECT

	END IF
	L = L + 1: LI = LI + 1
    LOOP UNTIL LIN$(L, 6) = "-"

    FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT

    SELECT CASE CASO%
	   CASE 1
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, USING " SUBTOTAL: ##,###,###.##"; FACTURA.TOTALBRUT
		PRINT #AREATXT, USING " DTO%....:            ##"; FACTURA.DTO;
		PRINT #AREATXT, USING SPACE$(26) + "BASE IMPONIBLE: ##,###,###.##"; FACTURA.BASEIMPONIBLE
		PRINT #AREATXT, USING SPACE$(50) + "     IVA ## %.: ##,###,###.##"; FACTURA.TIPOIVA; FACTURA.TOTALIVA
		PRINT #AREATXT, ""
		PRINT #AREATXT, USING SPACE$(50) + "      TOTAL...: ##,###,###.##"; FACTURA.TOTALNET
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, " " + "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO; "  Observacions: " + RTRIM$(ARCHIMP.COMPRIMIDO);
		PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		PRINT #AREATXT, "                                                     " + RTRIM$(ARCHIMP.COMPRIMIDO); RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, ""
	   CASE 2
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, USING SPACE$(52) + "      TOTAL...: ##,###,###.##"; FACTURA.TOTALNET
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, " " + "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO; "  Observacions: " + RTRIM$(ARCHIMP.COMPRIMIDO);
		PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		PRINT #AREATXT, "                                                     " + RTRIM$(ARCHIMP.COMPRIMIDO); RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, ""
	   CASE 3
		PRINT #AREATXT, "" + RTRIM$(ARCHIMP.COMPRIMIDO)
		PRINT #AREATXT, ""
		PRINT #AREATXT, ""
		PRINT #AREATXT, ""
		PRINT #AREATXT, ""
		PRINT #AREATXT, ""
		PRINT #AREATXT, USING "##,###,###.##" + SPACE$(40) + "  ##,###,###.##     ##  ##,###,###.##     ## " + SPACE$(25) + "##,###,###.##"; FACTURA.TOTALBRUT; FACTURA.BASEIMPONIBLE; FACTURA.TIPOIVA; FACTURA.TOTALIVA; FACTURA.DTO; FACTURA. _
TOTALNET
		PRINT #AREATXT, "" + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		FOR I = 1 TO 8: PRINT #AREATXT, "": NEXT
		PRINT #AREATXT, "" + RTRIM$(ARCHIMP.COMPRIMIDO)
		PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1))
		PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO
	   CASE 4
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, USING " SUBTOTAL: ##,###,###.##"; FACTURA.TOTALBRUT
		PRINT #AREATXT, USING " DTO%....:            ##"; FACTURA.DTO;
		PRINT #AREATXT, USING SPACE$(23) + "BASE IMPONIBLE: ##,###,###"; FACTURA.BASEIMPONIBLE
		PRINT #AREATXT, USING SPACE$(52) + "   IVA %.:         ##"; FACTURA.TIPOIVA
		PRINT #AREATXT, ""
		PRINT #AREATXT, USING SPACE$(52) + " TOTAL...: ##,###,###"; FACTURA.TOTALNET
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO
		PRINT #AREATXT, " " + STRING$(78, "Ä")
	   CASE 5
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, "    SUBTOTAL:     BASE IMPONIBLE:   DTO%:  IVA%:      TOTAL IVA:  TOTAL FACTURA:"
		PRINT #AREATXT, USING "##,###,###.##     ##,###,###.##     ##     ##      ##,###,###.##  ##,###,###.##"; FACTURA.TOTALBRUT; FACTURA.BASEIMPONIBLE; FACTURA.DTO; FACTURA.TIPOIVA; FACTURA.TOTALIVA; FACTURA.TOTALNET
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO

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
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + RTRIM$(CAP.LINIES(0)) + RTRIM$(ARCHIMP.NOENSANCHADO)
    FOR I = 1 TO 4: PRINT #AREATXT, CAP.LINIES(I): NEXT
    SELECT CASE MODO
	   CASE 2
		PRINT #AREATXT, ""
		PRINT #AREATXT, ""
		PRINT #AREATXT, ""
		PRINT #AREATXT, "FACTURA N§:   Data:         Codi Client:      " + FACTURA.PERSONA.NOM + RTRIM$(ARCHIMP.NEGRITA)
		PRINT #AREATXT, FACTURA.REFFACTURA + SPACE$(5) + FACTURA.DADA + SPACE$(6) + FACTURA.CODCLIENT + RTRIM$(ARCHIMP.NONEGRITA);
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
		PRINT #AREATXT, " Factura N§:     Data:      Codi Client:" + RTRIM$(ARCHIMP.NEGRITA)
		PRINT #AREATXT, " " + FACTURA.REFFACTURA + SPACE$(7) + FACTURA.DADA + SPACE$(3) + FACTURA.CODCLIENT + RTRIM$(ARCHIMP.NONEGRITA);
		PRINT #AREATXT, SPACE$(8) + " " + FACTURA.PERSONA.NOM
		PRINT #AREATXT, "           " + SPACE$(34) + "  " + FACTURA.PERSONA.COGNOMS
		PRINT #AREATXT, " P…gina N§:"; SPACE$(34) + "  D.N.I. " + FACTURA.PERSONA.DNI + RTRIM$(ARCHIMP.NEGRITA)
		PRINT #AREATXT, USING " \  \       \ \"; LTRIM$(STR$(PAG)); RTRIM$(ARCHIMP.NONEGRITA);
		PRINT #AREATXT, SPACE$(32) + "  " + FACTURA.PERSONA.DIRECCIO
		PRINT #AREATXT, SPACE$(47) + LTRIM$(RTRIM$(FACTURA.PERSONA.POBLACIO)) + " " + FACTURA.PERSONA.CPOSTAL
		PRINT #AREATXT, SPACE$(47) + "Tels." + FACTURA.PERSONA.TELEFON1
		PRINT #AREATXT, SPACE$(47) + "     " + FACTURA.PERSONA.TELEFON2
		PRINT #AREATXT, " " + STRING$(78, "Ä")
		PRINT #AREATXT, CONFIG$(0)
		PRINT #AREATXT, " " + STRING$(78, "Ä")

	   CASE ELSE
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
		PRINT #AREATXT, CONFIG$(0)
		PRINT #AREATXT, " " + STRING$(78, "Ä")
    END SELECT
    RETURN
END SUB

FUNCTION INICIAREF (AREANUM, R, MARCAT, REFOLD$)
	  DIM OP$(1 TO 10)
	 
	  GET AREANUM, 1, DOCNUM
	  MAXDOC% = DOCNUM.MAXNUM
	  FOR RE = 1 TO MAXDOC%: OP$(RE) = DOCNUM.FACTNUM(RE).CONFACT: NEXT
	  CALL MENU2(OP$(), CASO%, 10, 10, LEN(OP$(1)) + 2, MAXDOC%, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))

	  IF CASO% = 999 OR CASO% = 888 THEN
	     NOU = 9
	     EXIT FUNCTION
	  ELSE
	     REF$ = DOCNUM.FACTNUM(CASO%).NUMFACT
	     ALB$ = LTRIM$(STR$(DOCNUM.FACTNUM(CASO%).MAXFACT))
	  END IF

	  L2 = 6
	  FOR L = LEN(ALB$) TO 1 STEP -1
	      MID$(REF$, L2, 1) = MID$(ALB$, L, 1): L2 = L2 - 1
	  NEXT

'          IF MARCAT = TRUE THEN
'             IF MID$(REFOLD$, 8, 2) = MID$(REF$, 8, 2) THEN
'                MID$(REF$, 1, 2) = MID$(FETXA$, 7, 2)
'                MID$(REF$, 3, 4) = MID$(REFOLD$, 3, 4)
'             ELSE
'                MID$(REF$, 1, 2) = MID$(FETXA$, 7, 2)
'             END IF
 '         ELSE
	     MID$(REF$, 1, 2) = MID$(DATA$(1), 7, 2)
'          END IF

	  INICIAREF = CASO%
	  FACTURA.REFFACTURA = REF$
END FUNCTION

SUB INITALBARAN (MAXLIN)

    FACTURA.REFFACTURA = CHR$(0)               ' INCIAR FACTURA
    FACTURA.CODCLIENT = CHR$(0)
    FACTURA.DADA = DATA$(1)
    FACTURA.PERSONA.CODICLIENT = CHR$(0)
    FACTURA.PERSONA.NOM = CHR$(0)
    FACTURA.PERSONA.COGNOMS = CHR$(0)
    FACTURA.PERSONA.DNI = CHR$(0)
    FACTURA.PERSONA.POBLACIO = CHR$(0)
    FACTURA.PERSONA.TELEFON1 = CHR$(0)
    FACTURA.PERSONA.TELEFON2 = CHR$(0)
    FACTURA.PERSONA.DTO = 0
    FACTURA.PERSONA.FORMAPAGO = CHR$(0)
    FACTURA.PERSONA.CPOSTAL = CHR$(0)
    FACTURA.PERSONA.BANC = ""
    FACTURA.PERSONA.COMPTE = ""

    FACTURA.TOTALBRUT = 0
    FACTURA.BASEIMPONIBLE = 0
    FACTURA.TIPOIVA = 0
    FACTURA.TOTALIVA = 0
    FACTURA.DTO = 0
    FACTURA.TOTALNET = 0
    TOTAL = 0
    FOR J = 1 TO MAXLIN
	LINFACT.LINIA(J).CODART = CHR$(0)
	LINFACT.LINIA(J).CONCEPTE = CHR$(0)
	LINFACT.LINIA(J).PREU = 0
	LINFACT.LINIA(J).QUANTI = 0
	LINFACT.LINIA(J).DTO = 0
	LINFACT.LINIA(J).IMPORT = 0
	LINFACT.LINIA(J).MARCAR = CHR$(0)
	LIN$(J, 1) = "": LIN$(J, 2) = "": LIN$(J, 3) = ""
	LIN$(J, 4) = "": LIN$(J, 5) = "": LIN$(J, 6) = "": LIN$(J, 7) = ""
    NEXT

END SUB

SUB LLISTARALBARAN (AREA5, AREA2, DEVI$)
    SHARED DIRECCT$

    AREATXT = FREEFILE
    OPEN DIRECCT$ + "FACTURAS.TXT" FOR OUTPUT AS AREATXT

    PAG = 1: L = 1
    GET AREA5, 1, CAP   ' AGAFAR CAP€ALERA DEL FITXER DE CAP€ALERES

    GOSUB CAP.LIST     ' IMPRIMIR CAP€ALERA

    ' DEFINIR MASCARA DE LES LINIES                        
    SUMAIVB& = 0: SUMAIVT& = 0
    TOTALN& = 0: TOTABN& = 0
    SUMATALLER& = 0: SUMABOTIGA& = 0
    MASCARA$ = " ³\" + SPACE$(8) + "\³\" + SPACE$(47) + "\  ³ ##,###,###.##³##.##³###,###.##³##,###,###.##³\      \³"
    MASCAR2$ = "  \" + SPACE$(8) + "\ \" + SPACE$(47) + "\  ³ ##,###,###.##³##.##³###,###.##³##,###,###.##³\      \"
    MAXL = 54: TOTALB& = 0: TOTALN& = 0
    MAXF# = CEMPRE.MAXFACTURA - 1
    FOR RL! = 1 TO CEMPRE.MAXFACTURA - 1
	BARRA 10, 10, MAXF#, RL!
	GET AREA2, RL!, FACTURA
	IF FACTURA.MARCAT <> "*" THEN GOSUB PRINTLINIA
	       
	       IF MID$(FACTURA.REFFACTURA, 8, 2) = "RF" THEN
		   SUMAIVT& = SUMAIVT& + FACTURA.TOTALIVA
		   TOTALB& = TOTALB& + FACTURA.TOTALBRUT
		   SUMATALLER& = SUMATALLER& + FACTURA.TOTALNET
	       ELSE
		  IF MID$(FACTURA.REFFACTURA, 8, 2) = "BF" THEN
		     SUMAIVB& = SUMAIVB& + FACTURA.TOTALIVA
		     TOTALN& = TOTALN& + FACTURA.TOTALBRUT
		     SUMABOTIGA& = SUMABOTIGA& + FACTURA.TOTALNET
		  END IF
	       END IF

	IF L >= MAXL THEN
	   L = 1
	   PAG = PAG + 1
	   PRINT #AREATXT, " ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ"
	   PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
	   PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
	   GOSUB CAP.LIST
	END IF
    NEXT
    PRINT #AREATXT, " ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÙ"
    
    PRINT #AREATXT, USING MASCAR2$; SPACE$(9); SPACE$(20) + "Suma dels Totals de Taller: "; TOTALB&; 0; SUMAIVT&; SUMATALLER&
    PRINT #AREATXT, USING MASCAR2$; SPACE$(9); SPACE$(20) + "Suma dels Totals de Botiga: "; TOTALN&; 0; SUMAIVB&; SUMABOTIGA&
    PRINT #AREATXT, "                                                                ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT

    CALL ImprimeixFitxerTXT(DIRECCT$ + "FACTURAS.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP€ALERA DELS LLISTATS
' **************************************************************************

CAP.LIST:
    PRINT #AREATXT, " "; RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, " LLISTAT TOTAL DE LES FACTURES"
    PRINT #AREATXT, " P…gina:"; PAG
    PRINT #AREATXT, " Data..: "; FETXA$
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""

     '                                    1         2         3         4         5
     '                           12345678901234567890123456789012345678901234567890
    PRINT #AREATXT, " ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¿"
    PRINT #AREATXT, " ³ReferŠncia³Nom del client                                     ³Subtotal      ³DTO %³IVA %     ³Total Net    ³Data    ³"
    PRINT #AREATXT, " ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ´"
															       
    RETURN

PRINTLINIA:
    NOMPCLIENT$ = LTRIM$(RTRIM$(FACTURA.PERSONA.NOM)) + " " + LTRIM$(RTRIM$(FACTURA.PERSONA.COGNOMS))
    IF NOMPCLIENT$ = " " THEN
       NOMPCLIENT$ = SPACE$(52)
    END IF
    PRINT #AREATXT, USING MASCARA$; FACTURA.REFFACTURA; NOMPCLIENT$; FACTURA.TOTALBRUT; FACTURA.DTO; FACTURA.TIPOIVA; FACTURA.TOTALNET; FACTURA.DADA
    L = L + 1
    RETURN
END SUB

SUB MASCALBA (MAX)

ShowWindow 0

'COLOR COL(0, 0), COL(0, 1)
'FINESTRA 1, 1, 25, 80, 0, CAIXA1
COLOR COL(2, 0)
LOCATE , , 1, 13, 14
LOCATE 2, 2: PRINT "Factura N£mero:"; SPACE$(10); "Document...: "; : COLOR 15: PRINT DOCU$; : COLOR COL(2, 0): PRINT "    M…xim de l¡nies:"; : COLOR COL(0, 0): PRINT MAX: COLOR COL(2, 0)
LOCATE 4, 2: PRINT "Ref. Factura:"
LOCATE 3, 27: PRINT "Codi Client:            Data:"
LOCATE 4, 27: PRINT "Nom........:"
LOCATE 5, 27: PRINT "Cognoms....:"
COLOR COL(0, 0)

LOCATE 20, 40: PRINT "DTO.%  LINIA:"

LOCATE 6, 1: PRINT "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍÍ¹"
LOCATE 7, 1: PRINT "º Codi Article     ºConcepte                     ºPreu     ºQuant.  ºImport    º"
LOCATE 8, 1: PRINT "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÍÍ¹"
LOCATE 19, 1: PRINT "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÍÍ¹"
FOR L = 9 TO 18: LOCATE L, 2: PRINT SPACE$(18); "º"; SPACE$(29); "º"; SPACE$(9); "º"; SPACE$(8); "º": NEXT
LOCATE 20, 61: PRINT "Total:"
LOCATE 21, 2: PRINT STRING$(78, "Í");
COLOR 3, COL(0, 1)
LOCATE 20, 67: PRINT USING "##,###,###.##"; TOTAL
END SUB

SUB MASCALBS
    ShowWindow 1

    LOCATE 3, 2: PRINT "ReferŠnciaºClient" + SPACE$(21) + "ºTotal brut   ºDTO% ºIVA% ºTotal Net"
    LOCATE 4, 1: PRINT "Ì"; STRING$(78, "Í"); "¹"
    LOCATE 4, 12: PRINT "Î": LOCATE 4, 40: PRINT "Î": LOCATE 4, 54: PRINT "Î": LOCATE 4, 60: PRINT "Î": LOCATE 4, 66: PRINT "Î"
    LOCATE 20, 12: PRINT "Ê": LOCATE 20, 40: PRINT "Ê": LOCATE 20, 54: PRINT "Ê": LOCATE 20, 60: PRINT "Ê": LOCATE 20, 66: PRINT "Ê"

    FINESTRA 21, 1, 25, 80, 0, CAIXA1

    LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR FACTURA <F2>=INSERTAR FACTURA  <" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR              <F3>=SUMAR FACTURES    <F4>=CONSULTAR";
    LOCATE 24, 2: PRINT "<F5>=MARCAR FACTURA       <F6>=LLISTAR           <F7>=IMPRIMIR";
END SUB

SUB OrdenarIndex (INDEX() AS INDEXTYPE, MAXST) STATIC

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

FUNCTION POSSAESPAI$ (CAD$, C)

	 FOR L = 1 TO LEN(CAD$)
	     IF MID$(CAD$, L, 1) = CHR$(C) THEN MID$(CAD$, L, 1) = CHR$(32)
	 NEXT
	 POSSAESPAI$ = CAD$

END FUNCTION

SUB ReadCalculFactura
 
  'SaveScreen DIRECCP$ + "FACTURA.TMP", 0, 4000
  GetBackground 1, 1, 24, 79, RESBUF$
  COLOR 1, 3
  FINESTRA 9, 10, 14, 60, 1, CAIXA1

  COLOR COL(2, 0), 3
  LOCATE 10, 12: PRINT "Subtotal:"; : COLOR 15: PRINT FACTURA.TOTALBRUT; : COLOR 14
  LOCATE 13, 33: PRINT "      Total:"; : COLOR 15: PRINT FACTURA.TOTALNET; : COLOR 14
  LOCATE 10, 33: PRINT "Descompte %:"; : COLOR 15: PRINT FACTURA.DTO; : COLOR 14
  LOCATE 11, 33: PRINT "      IVA %:"; : COLOR 15: PRINT FACTURA.TIPOIVA;
  SETMAXCAMPS 1

  SETCOLORCAMPS 0, 15, 1, 15, 3, 14, 3
  SETCOLORCAMPS 1, 15, 1, 15, 3, 14, 3

  INSERTCAMP 0, LTRIM$(STR$(FACTURA.DTO))
  INSERTCAMP 1, LTRIM$(STR$(FACTURA.TIPOIVA))
  INITCAMP 0, 10, 45, NUM, 0, "999", ""
  INITCAMP 1, 11, 45, NUM, 0, "999", ""
  IF LLEGIRCAMP(0) = SALIR THEN EXIT SUB
  IF LLEGIRCAMP(1) = SALIR THEN EXIT SUB
  
  FACTURA.DTO = VAL(SHOWFIELD$(0))
  FACTURA.TIPOIVA = VAL(SHOWFIELD$(1))

  'FACTURA.BASEIMPONIBLE=TOTAL        ' NO HI HA DESCOMPTE

   FACTURA.BASEIMPONIBLE = FACTURA.TOTALBRUT - (FACTURA.TOTALBRUT * FACTURA.DTO) / 100
   BASEIMP = FACTURA.BASEIMPONIBLE
   IVA = BASEIMP * (FACTURA.TIPOIVA) / 100
   FACTURA.TOTALIVA = IVA
   FACTURA.TOTALNET = BASEIMP + IVA

   COLOR 14: LOCATE 13, 33: PRINT "      Total:";
   COLOR 15: PRINT USING "##,###,###.##"; FACTURA.TOTALNET; : COLOR 14

  'FACTURA.BASEIMPONIBLE = FACTURA.TOTALBRUT - FACTURA.DTO

END SUB

SUB READCAPSALERA (MAXAL, MAXCL)
       DIM LLISTA$(MAXCL - 1)
       FOR C = 0 TO 10: ERASEFIELD C: NEXT C

       REPINTACAPSALERA (MAXAL)
       FOR C = 0 TO 4
	   VALUE = LLEGIRCAMP(C)
	   SELECT CASE VALUE
		  CASE 1
		       TROBAT = 0
		       FOR R = 1 TO MAXCL
			   GET AREA4, R, CLIENT
			   IF SHOWFIELD$(1) = CLIENT.CODICLIENT THEN
			      INSERTCAMP 3, CLIENT.NOM
			      INSERTCAMP 4, CLIENT.COGNOMS
			      FACTURA.PERSONA.NOM = CLIENT.NOM
			      FACTURA.PERSONA.COGNOMS = CLIENT.COGNOMS
			      FACTURA.PERSONA.DNI = CLIENT.DNI
			      FACTURA.PERSONA.DIRECCIO = CLIENT.DIRECCIO
			      FACTURA.PERSONA.POBLACIO = CLIENT.POBLACIO
			      FACTURA.PERSONA.TELEFON1 = CLIENT.TELEFON1
			      FACTURA.PERSONA.TELEFON2 = CLIENT.TELEFON2
			      FACTURA.PERSONA.CPOSTAL = CLIENT.CPOSTAL
			      FACTURA.PERSONA.DTO = CLIENT.DTO
			      FACTURA.PERSONA.FORMAPAGO = CLIENT.FORMAPAGO
			      TROBAT = 44
			      EXIT FOR
			   END IF
		       NEXT
			   IF TROBAT = 44 THEN
			      DISPLAYALL
			      FACTURA.REFFACTURA = SHOWFIELD$(0)
			      FACTURA.CODCLIENT = SHOWFIELD$(1)
			   END IF
		  CASE F1
		     IF FRE(-1) < 111000 THEN
			AVIS "ERROR FATAL:", "Mem•ria insuficient !!!", 0
			CW$ = INKEY$
		     ELSE
			SHELL "BKHLP CAP€ALERA_ALBARANS": C = C - 1
		     END IF
		  CASE F2
		       IF C = 1 THEN GOSUB LLISTA.CLIENTS ELSE C = C - 1
		  CASE F3 TO F10
		       C = C - 1
		  CASE 999
		       EXIT SUB
		  CASE ELSE
	   END SELECT
       NEXT
       FACTURA.DADA = SHOWFIELD$(2)
       ERASE LLISTA$
       EXIT SUB

LLISTA.CLIENTS:
       IF MAXCL = 1 THEN
	  AVIS "AVIS:", "La base de dades dels clients est… buida", 0
	  RETURN
       END IF

       'SaveScreen DIRECCP$ + "LLISTA.CLI", 0, 4000
       GetBackground 1, 1, 24, 79, LLISTA$
       SetScoreBoard SOFF: LOCATE , , 0: R = 1
       FOR Q = 1 TO MAXCL - 1
	   GET AREA4, R, CLIENT
	   IF CLIENT.MARCAT = "-" THEN
	      CODI$ = CLIENT.CODICLIENT
	      N$ = LTRIM$(RTRIM$(CLIENT.NOM))
	      C$ = LTRIM$(RTRIM$(CLIENT.COGNOMS))
	      S$ = N$ + " " + C$: NOM$ = MID$(S$, 1, 40)
	      LLISTA$(R) = CODI$ + " " + NOM$
	      R = R + 1
	   END IF
       NEXT

       ASEL = ACHOICE(3, 9, 20, 61, R - 1, LLISTA$(), COL(), "Codi       Nom                                     ", 12)
       IF ASEL = 0 OR ASEL = -14 OR ASEL = -13 THEN
	  'RestoreScreen DIRECCP$ + "LLISTA.CLI", 0: KILL DIRECCP$ + "LLISTA.CLI"
	  PutBackground 1, 1, LLISTA$
	  C = C - 1: RETURN
       END IF
       IF ASEL <= 0 THEN ASEL = 1
       GET AREA4, ASEL, CLIENT
       INSERTCAMP 1, CLIENT.CODICLIENT
       CALL DISPLAYALL: C = C - 1
       'RestoreScreen DIRECCP$ + "LLISTA.CLI", 0: KILL DIRECCP$ + "LLISTA.CLI"
       PutBackground 1, 1, LLISTA$
       RETURN

END SUB

SUB ReadObserva
    SHARED OBS$()
    LOCATE 22, 2: PRINT SPACE$(70);
    LOCATE 23, 2: PRINT SPACE$(70);
    LOCATE 24, 2: PRINT SPACE$(70);

    SETMAXCAMPS 1
    SETCOLORCAMPS 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
    SETCOLORCAMPS 1, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
    
    INITCAMP 0, 22, 20, ASCI, 0, STRING$(50, "X"), ""
    INITCAMP 1, 23, 20, ASCI, 0, STRING$(50, "X"), ""

    COLOR COL(0, 0) XOR COL(0, 1), COL(0, 1)
    LOCATE 24, 2: PRINT "ENTER=SORTIR";
    LOCATE 22, 2: PRINT "Observacions: ";
    INSERTCAMP 0, FACTURA.OBSERVA(1): INSERTCAMP 1, FACTURA.OBSERVA(2)

    IF LLEGIRCAMP(0) = SALIR THEN
       OBS$(0) = SHOWFIELD$(0)
       EXIT SUB
    END IF
    IF LLEGIRCAMP(1) = SALIR THEN
       OBS$(1) = SHOWFIELD$(1)
       EXIT SUB
    END IF
    OBS$(0) = SHOWFIELD$(0)
    OBS$(1) = SHOWFIELD$(1)
    FACTURA.OBSERVA(1) = OBS$(0)
    FACTURA.OBSERVA(2) = OBS$(1)
    LOCATE 22, 2: PRINT SPACE$(70);
    LOCATE 23, 2: PRINT "ENTER=EDITAR LINIA    ESC=ANULAR    SUPR=SUPRIMIR LINIA";
    LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR    CTRL+F=ACABAR DE FACTURAR";
END SUB

SUB REPINTACAPSALERA (MAX)
       FOR C = 0 TO 10: ERASEFIELD C: NEXT C

       SETMAXCAMPS 4
       FOR C = 0 TO 4: SETCOLORCAMPS C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT

       INITCAMP 0, 4, 15, ASCI, 0, "XXXXXXXXX", ""
       INITCAMP 1, 3, 39, ASCI, 0, "XXXXXXXXXX", ""
       INITCAMP 2, 3, 56, NUM, 0, "99/99/99", ""
       INITCAMP 3, 4, 39, ASCI, 0, STRING$(20, "X"), ""
       INITCAMP 4, 5, 39, ASCI, 0, STRING$(30, "X"), ""

       COLOR 15: LOCATE 2, 18: PRINT R

       INSERTCAMP 0, FACTURA.REFFACTURA
       INSERTCAMP 1, FACTURA.CODCLIENT
       INSERTCAMP 2, FACTURA.DADA
       INSERTCAMP 3, FACTURA.PERSONA.NOM
       INSERTCAMP 4, FACTURA.PERSONA.COGNOMS
       DISPLAYALL

END SUB

SUB SUMARALBARANS (MAXAL, AREA2)
'           GetBackground 1, 1, 24, 79, FACTU$

	   ShowWindow 2
	   AVIS.SONOR (1)
	   
	   SUMANET = 0
	   SUMAIVA = 0: SUMATALLER = 0: SUMABOTIGA = 0

	   FOR S = 1 TO MAXAL
	       GET AREA2, S, FACTURA
	       SUMANET = SUMANET + FACTURA.TOTALNET
	       SUMAIVA = SUMAIVA + FACTURA.TOTALIVA
	       IF MID$(FACTURA.REFFACTURA, 8, 2) = "RF" THEN
		   SUMATALLER = SUMATALLER + FACTURA.TOTALNET
	       ELSE
		  IF MID$(FACTURA.REFFACTURA, 8, 2) = "BF" THEN
		     SUMABOTIGA = SUMABOTIGA + FACTURA.TOTALNET
		  END IF
	       END IF
	      
	   NEXT
	   COLOR COL(2, 0), COL(2, 1)
	   LOCATE 12, 11: PRINT "TOTAL TALLER....:"; : COLOR COL(0, 0), COL(0, 1)
	   PRINT USING "##,###,###.##"; SUMATALLER:  COLOR COL(2, 0), COL(2, 1)
	   LOCATE 13, 11: PRINT "TOTAL BOTIGA....:"; : COLOR COL(0, 0), COL(0, 1)
	   PRINT USING "##,###,###.##"; SUMABOTIGA:  COLOR COL(2, 0), COL(2, 1)
	   LOCATE 14, 11: PRINT "IVA's...........:"; : COLOR COL(0, 0), COL(0, 1)
	   PRINT USING "##,###,###.##"; SUMAIVA:  COLOR COL(2, 0), COL(2, 1)
	   LOCATE 15, 11: PRINT "TOTAL...........:"; : COLOR COL(0, 0), COL(0, 1)
	   PRINT USING "##,###,###.##"; SUMANET: COLOR COL(2, 0), COL(2, 1)

	   C$ = INPUT$(1)
	   'PutBackground 1, 1, FACTU$
	   DeleteWindow 2
END SUB

SUB TROS
'      DO
 '
 '        SETMAXCAMPS 0: SETCOLORCAMPS 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
 '        X = FLD(C).XCAMP: T = FLD(C).TIPUS: M$ = LTRIM$(RTRIM$(FLD(C).MASK))
 '        INITCAMP 0, YC, X, T, 0, M$, ""
 '        INSERTCAMP 0, LIN$(L, C): VELL$ = LIN$(L, C)
 '
 '        VALUE = LLEGIRCAMP(0)
 '        SELECT CASE VALUE
 '               CASE F1
 '                    IF FRE(-1) < 111000 THEN
 '                       AVIS "ERROR FATAL:", "Mem•ria insuficient !!!", 0
 '                       CW$ = INKEY$
 '                    ELSE
 '                       SHELL "BKHLP ALBARANS": C = C - 1
 '                    END IF
 '               CASE F2
 '                    IF C = 1 THEN GOSUB LLISTA.STOCK
 '               CASE F3 TO F10
 '                    SOUND 50, .5
 '               CASE SALIR
 '                    LOCATE 23, 2: PRINT "ENTER=EDITAR LINIA    ESC=ANULAR    SUPR=SUPRIMIR LINIA";
 '                    LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR    CTRL+F=ACABAR L'ALBAR…";
 '                    X = FLD(1).XCAMP
 '                    RETURN
 '               CASE ELSE
 '        END SELECT
 '
 '        LIN$(L, C) = SHOWFIELD$(0)
 '        CAMPTEMP$ = FORAESPAI$(SHOWFIELD$(0))
 '
 '           IF C = 1 THEN
 '              IF CercarRecord%(CAMPTEMP$, INDEX(), MAXST, AREAST) THEN
 '                 IF STOCK.MARCAT <> "*" THEN
 '                    LIN$(L, 2) = STOCK.DESCRIPCIO
 '                    IF STOCK.EXISTENCIA < STOCK.STOCKMIN THEN
 '                       COLOR 27: BEEP: LOCATE 22, 2: PRINT "Aquest article est… baix minims            "
 '                    ELSE
 '                       IF STOCK.EXISTENCIA = 0 THEN
 '                          COLOR 27: BEEP: LOCATE 22, 2: PRINT "Aquest article est… amb existŠncies 0"
 '                       END IF
 '                    END IF
 '                    TROBAT = 999
 '                 END IF
 '              END IF
 '           END IF
 '
 '           IF C = 4 THEN
 '              IF TROBAT = 999 THEN                        ' RESTAR AL STOCK
 '                 LOCK AREAST, INDEX(MIDPOINT).REGISTRE
 '                 GET AREAST, INDEX(MIDPOINT).REGISTRE, STOCK
 '                 SAC = STOCK.EXISTENCIA - VAL(LIN$(L, 4))
 '                 GOSUB ACTUALIZHISTORIC
 '                 STOCK.EXISTENCIA = SAC
 '                 STOCK.STOCKMAX = SAC
 '                 PUT AREAST, INDEX(MIDPOINT).REGISTRE, STOCK
  '                UNLOCK AREAST, INDEX(MIDPOINT).REGISTRE
  '             END IF
  '          END IF
  '
  '       IF T = ASCI THEN
  '          LOCATE YC, X: PRINT USING MASC$(C); SHOWFIELD$(0)
   ''      ELSE
  '          LOCATE YC, X: PRINT USING MASC$(C); VAL(SHOWFIELD$(0))
  '       END IF
  '
  '       ERASEFIELD 0
  '       C = C + 1
  '       GOSUB MOSTRA
  '       IF C = 3 THEN
  '                M$(1) = "Preu Venda ": M$(2) = "Preu Compra"
  '                GetBackground 1, 1, 24, 79, sfactu$
  '                CALL MENU2(M$(), CASO%, INT(YC), INT(FLD(C).XCAMP), 13, 2, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
  '                PutBackground 1, 1, sfactu$
  '                SELECT CASE CASO%
  '                       CASE 1
  '                            LIN$(L, 3) = LTRIM$(STR$(STOCK.PVPACONSE))
  '                            LIN$(L, 7) = ""
  '                       CASE 2
  '                            LIN$(L, 3) = LTRIM$(STR$(STOCK.PREU))
  '                            LIN$(L, 7) = ""
  '                       CASE 999
  ''                            LIN$(L, 3) = LTRIM$(STR$(STOCK.PREU))
  '                            LIN$(L, 7) = ""
  '                       CASE ELSE
  '                END SELECT
  '
  '       END IF
  '
  '       IF C = 5 THEN
  '          SETMAXCAMPS 0: SETCOLORCAMPS 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
  '          INITCAMP 0, 20, 53, NUM, 0, "999", ""
  '
  '          ERASEFIELD 0
   '         INSERTCAMP 0, LIN$(L, 7)
  '          IF LLEGIRCAMP(0) = SALIR THEN BEEP
   '         LIN$(L, 7) = SHOWFIELD$(0)
   '
   '         PREU = VAL(LIN$(L, 3))
   '         QUANT = VAL(LIN$(L, 4))
   '         DESCOMPTE = VAL(LIN$(L, 7))
    '
   '         IMPORT = QUANT * PREU - ((QUANT * PREU) * DESCOMPTE) / 100
    '
    '        LIN$(L, C) = STR$(IMPORT)
     '
     '       SUBTOTAL = 0        ' TORNA A RECALCULAR ELS TOTALS
     '       FOR J = 1 TO MAX
     '           SUBTOTAL = SUBTOTAL + VAL(LIN$(J, 5))
     '       NEXT
     '
     '       TOTAL = SUBTOTAL
     '
     '       LOCATE YC, 70: PRINT USING MASC$(5); IMPORT
     '       COLOR COL(0, 0) XOR COL(0, 1), COL(0, 1)
     '       LOCATE 20, 67: PRINT USING MASCTOTAL$; TOTAL
     '
      '      IF L = MAX THEN
     '          IF LIN$(L, 6) = "-" THEN LIN$(L, 6) = "*"
      '      ELSE
      '         IF LIN$(L, 6) = "-" THEN
      '            LIN$(L, 6) = "*"
      '         END IF
      '         IF LIN$(L + 1, 6) <> "-" AND LIN$(L + 1, 6) <> "*" THEN
      '            LIN$(L + 1, 6) = "-"
      '         END IF
      '      END IF
      '
      '      IF YC = 18 THEN
      '         SCROLLUP 17, 78, 8, 1, 1
      '         YC = 18: L = L + 1: C = 1: SALE = 1
      '         GOSUB MOSTRA
      '      ELSE
      '         C = 1: L = L + 1: YC = YC + 1: SALE = 1
      '      END IF
      '   END IF
      'LOOP UNTIL SALE = 1

END SUB

