' ********************************************************************
'
' Fitxer....................: BKHLP.BAS
' Titol.....................: Modul per el mateniment de les ajudes
'
' ********************************************************************
'
' Data inici................: 20/04/1996
' Data de la darrera revisi¢: 07/11/1997 21:57:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
'
' ********************************************************************
' Notes: S'ha de posar la barra de scroll (UP & DOWN)
'
' ********************************************************************

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'


COMMON SHARED DIRECC$, DIRCP$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT

DIM SHARED EMPRES AS EMPRESA
DIM SHARED CEMPRE AS CONTROLEMP       '     "      DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CFG AS CONFIG              '     "      DEL FITXER DE CONFIGURACI¢
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED CTRL AS CONTROL
DIM SHARED USR AS USUARIS
DIM SHARED COLORS AS COLO
DIM SHARED col(2, 1)
DIM PASO AS TRANS
DIM ANYS AS ANNO

      GOSUB INIC.FITXERS
      IF COMMAND$ = "" THEN GOSUB CREDITS
      DIM L$(5000)
      OPEN DIRECCHE$ + "AJUDA.HLP" FOR INPUT SHARED AS #1
      LIN$ = "": L = 0
      ACTIVAT = 0
      DO UNTIL EOF(1)
	  LINE INPUT #1, LIN$
	  IF ACTIVAT = 0 THEN
	     IF INSTR(1, LIN$, COMMAND$) THEN
		LINE INPUT #1, LIN$
		ACTIVAT = 44
	     END IF
	  END IF

	  IF ACTIVAT = 44 THEN
	     IF INSTR(1, LIN$, "&FT") AND ACTIVAT = 44 THEN LMAX = L: EXIT DO
	     L$(L) = LIN$
	     L = L + 1
	  END IF
      LOOP
      CLOSE #1

      GetBackground 1, 1, 20, 80, BUFF$
      GOSUB MASK
      GOSUB MOSTRA.TXT
      X = 3: LI = 0: XC = 3
      GOSUB estat
      COLOR col(1, 0), col(1, 1): LOCATE X, 2: PRINT STRING$(78, " ");
      COLOR col(1, 0), col(1, 1): LOCATE X, 2: PRINT L$(LI)
      DO
	 T$ = INKEY$
	 SELECT CASE T$
		CASE CHR$(0) + "P"
		     GOSUB PRINT.LINIA.UP: GOSUB estat
		CASE CHR$(0) + "H"
		     GOSUB PRINT.LINIA.DOWN: GOSUB estat
		CASE CHR$(27)
		     PutBackground 1, 1, BUFF$
		     SYSTEM
		CASE ELSE
		     COLOR col(0, 0), col(0, 1)
	 END SELECT
	 
      LOOP
      END

estat:
     COLOR col(0, 0), col(0, 1)
     LOCATE 4 + (nPos / 2), 80: PRINT "²"
     nPos = INT((LI * 100) / LMAX)
     IF nPos > 25 THEN
	DO
	  nPos = nPos - 1
	LOOP UNTIL nPos <= 19
     END IF
     LOCATE 4 + (nPos / 2), 80: PRINT "Û"
     RETURN


PRINT.LINIA.UP:
      IF LI = LMAX THEN
      ELSE
	 IF X >= 19 THEN
	    COLOR col(0, 0), col(0, 1): LOCATE X, 2: PRINT STRING$(78, " ");
	    LOCATE X, 2: PRINT L$(LI)
	    SCROLLUP 18, 78, 2, 1, 1
	    LI = LI + 1
	    COLOR col(1, 0), col(1, 1): LOCATE X, 2: PRINT STRING$(78, " ");
	    COLOR col(1, 0), col(1, 1): LOCATE X, 2: PRINT L$(LI)
	 ELSE
	    COLOR col(0, 0), col(0, 1): LOCATE X, 2: PRINT STRING$(78, " ");
	    LOCATE X, 2: PRINT L$(LI)
	    X = X + 1: LI = LI + 1
	    XC = XC + 1
	    COLOR col(1, 0), col(1, 1): LOCATE X, 2: PRINT STRING$(78, " ");
	    COLOR col(1, 0), col(1, 1): LOCATE X, 2: PRINT L$(LI)
	 END IF
      END IF
      RETURN

PRINT.LINIA.DOWN:
      IF LI = 0 THEN
	
      ELSE
	 IF X = 3 THEN
	    COLOR col(0, 0), col(0, 1): LOCATE X, 2: PRINT STRING$(78, " ");
	    LOCATE X, 2: PRINT L$(LI)
	    SCROLLDOWN 18, 78, 2, 1, 1
	    LI = LI - 1
	    COLOR col(1, 0), col(1, 1): LOCATE X, 2: PRINT STRING$(78, " ");
	    COLOR col(1, 0), col(1, 1): LOCATE X, 2: PRINT L$(LI)
	 ELSE
	    COLOR col(0, 0), col(0, 1): LOCATE X, 2: PRINT STRING$(78, " ");
	    LOCATE X, 2: PRINT L$(LI)
	    X = X - 1: LI = LI - 1
	    XC = XC - 1
	    COLOR col(1, 0), col(1, 1): LOCATE X, 2: PRINT STRING$(78, " ");
	    COLOR col(1, 0), col(1, 1): LOCATE X, 2: PRINT L$(LI)
	 END IF
      END IF
      RETURN

MASK:
      COLOR col(0, 0), col(0, 1): FINESTRA 2, 1, 20, 80, 0, CAIXA1
      FOR S = 3 TO 19: LOCATE S, 80: PRINT "²": NEXT
      LOCATE 3, 80: PRINT CHR$(24): LOCATE 19, 80: PRINT CHR$(25)
      COLOR col(2, 0), col(2, 1)
      CENTRAR 2, " Ajuda (TEMA: " + COMMAND$ + ") "
      COLOR col(0, 0), col(0, 1)
      RETURN

MOSTRA.TXT:
      FOR L = 0 TO 13
	  COLOR col(0, 0), col(0, 1): LOCATE L + 3, 2: PRINT L$(L)
      NEXT
      RETURN

CREDITS:
      COLOR 14, 0
      PRINT "Book Help 1.1": COLOR 2
      PRINT "Llibre per gestionar l'ajuda del programa Facturaci¢ " + versio: COLOR 15
      PRINT "Smart Software 1993/96 (C). Tomeu Cap¢ Cap¢ 1996 (C)": COLOR 7
      PRINT
      PRINT "Sintaxi: BKHLP <TEMA>"
      PRINT
      SYSTEM
'*************************************************************
' INICIA FITXERS
'*************************************************************

INIC.FITXERS:
      OPEN "..\CONFIG.FAC" FOR RANDOM SHARED AS 2 LEN = LEN(CFG)
      
      GET 2, 1, CFG    ' RECULL LA CONFIGURACI¢ SECUNDARIA

      UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
      HLP$ = LTRIM$(RTRIM$(CFG.DHELP))

      DIRECCHE$ = HLP$ + "\"    ' Subdirecctori de ajuda
      col(0, 0) = 15: col(0, 1) = 2
      col(1, 0) = 15: col(1, 1) = 9
      col(2, 0) = 9: col(2, 1) = 2
      CLOSE 2
      RETURN

