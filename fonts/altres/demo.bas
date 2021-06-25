DECLARE SUB ScrFonsDemo ()
DECLARE SUB ScrMouseDemo ()
DECLARE SUB ScrWindowsContDemo ()
DECLARE SUB AjudaWin (TEMA!)
DECLARE SUB OBRIRCAIXA (WX!, LY!)
DECLARE SUB ScrWindowsDemo ()
DECLARE SUB ScrColorsDemo ()
DECLARE SUB InitVars ()
DECLARE SUB ScrIntro ()
DECLARE FUNCTION CENTER.H! (LO!)
DECLARE FUNCTION CENTER.V! (LO!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DMOUSE.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'


DIM SHARED TITOLT(0 TO 10) AS STRING * 50
DIM SHARED LON(0 TO 10)
DIM SHARED LINE$(0 TO 1000)


GOSUB INICI
CALL ScrFonsDemo
CALL ScrColorsDemo
CALL ScrWindowsDemo
CALL ScrWindowsContDemo
CALL ScrMouseDemo
END


INICI:       CALL InitVars
	     CALL ScrIntro
	     RETURN

SUB AjudaWin (TEMA)
    AVIS.SONOR (1)

    CALL GetBackground(1, 1, 24, 79, HLP$)
    COLOR 14, 4
    CALL OBRIRCAIXA(10, 40): COLOR 15
    LOCATE 7, CENTER.H(14): PRINT " Ajuda RÖpida "
    SELECT CASE TEMA
	   CASE 0
		LOCATE 9, 29: PRINT "<" + CHR$(24) + "> Pujar linia"
		LOCATE 11, 29: PRINT "<" + CHR$(25) + "> Baixar linia"
		LOCATE 13, 29: PRINT "<F1> Aquesta pantalla"
		LOCATE 15, 29: PRINT "<ESC> Continuar la demo"
	   CASE 1
		LOCATE 9, 26: PRINT "Introdueix un nom d'un fitxer"
		LOCATE 10, 26: PRINT "de text per poder fer la segÅent"
		LOCATE 11, 26: PRINT "demostraci¢."
	   CASE ELSE
		BEEP
    END SELECT
    C$ = INPUT$(1)
    PutBackground 1, 1, HLP$
END SUB

FUNCTION CENTER.H (LO)
	 CENTER.H = 80 \ 2 - LO \ 2
END FUNCTION

FUNCTION CENTER.V (LO)
	 CENTER.V = 25 \ 2 - LO \ 2
END FUNCTION

SUB InitVars
    TITOLT(0) = "Smart-Libs 2.0 & 3.0"
    TITOLT(1) = "Presentaci¢ de les noves bibliotäques"
    TITOLT(2) = "i utilitats de Smart per QuickBasic 7.01 i d'altres"
    TITOLT(3) = "Per: Smart Software 1993/96"
    TITOLT(4) = "Creadors:"
    TITOLT(5) = "Joan Miquel Payeras i Cresp° & Tomeu Cap¢ i Cap¢"
    TITOLT(6) = "Summer 1995/96 (C)"
    TITOLT(7) = "Polsa una tecla per a continuar"

    FOR t = 0 TO 7
	LON(t) = LEN(LTRIM$(RTRIM$(TITOLT(t))))
    NEXT

END SUB

SUB OBRIRCAIXA (WX, LY)
    L = 1: Y = CENTER.H(L)

    FOR W = 1 TO WX
	X = CENTER.V(W)
	FINESTRA (X), (Y), (X + W), (Y + L), 1, CAIXA1
    NEXT

    FOR L = 1 TO LY
	Y = CENTER.H(L)
	FINESTRA (X), (Y), (X + W), (Y + L), 1, CAIXA1
    NEXT

END SUB

SUB ScrColorsDemo
    MISS$ = "Aixï Çs una prova de la funci¢ FastCol": L = LEN(MISS$)
    COLOR 3, 9
    FINESTRA (CENTER.V(1) - 1), (CENTER.H(L) - 2), (CENTER.V(1) + 1), (CENTER.H(L) + L + 1), 1, CAIXA2
    COLOR 15
    LOCATE CENTER.V(1), CENTER.H(L): PRINT MISS$
    C$ = INPUT$(1)

    FOR X = 0 TO 25
	FOR Y = 0 TO 79
	    FASTCOL Y, X, (X * X + Y * Y)

	NEXT
    NEXT
    'SO.LLARG
    C$ = INPUT$(1)
END SUB

SUB ScrFonsDemo
    COLOR 7, 9
    MISS$ = "Aixï Çs una prova de la funci¢ Fons": L = LEN(MISS$)

    FINESTRA (CENTER.V(1) - 1), (CENTER.H(L) - 2), (CENTER.V(1) + 2), (CENTER.H(L) + L + 1), 1, CAIXA2
    COLOR 15
    LOCATE CENTER.V(1), CENTER.H(L): PRINT MISS$
    COLOR 27: LOCATE CENTER.V(-2), CENTER.H(15): PRINT "Pitja una tecla"
    C$ = INPUT$(1)
    COLOR 7, 0
    FOR C = 32 TO 255: CALL FONS(C): NEXT

END SUB

SUB ScrIntro
    x2 = 25: COLOR 7, 9
    FOR X = 1 TO 25 \ 2 + 1
	LOCATE X, 1: PRINT STRING$(80, "±");
	LOCATE x2, 1: PRINT STRING$(80, "±");
	x2 = x2 - 1
	TEMPS (.1)
    NEXT
    
    'SO.LLARG
    COLOR 15, 9

    CALL OBRIRCAIXA(15, 60)
    LOCATE 8, CENTER.H(LON(0)): PRINT RTRIM$(LTRIM$(TITOLT(0))): COLOR 2
    LOCATE 9, CENTER.H(LON(1)): PRINT RTRIM$(LTRIM$(TITOLT(1)))
    LOCATE 10, CENTER.H(LON(2)): PRINT RTRIM$(LTRIM$(TITOLT(2))): COLOR 15
    LOCATE 12, CENTER.H(LON(3)): PRINT RTRIM$(LTRIM$(TITOLT(3))): COLOR 9
    LOCATE 14, CENTER.H(LON(4)): PRINT RTRIM$(LTRIM$(TITOLT(4))): COLOR 14
    LOCATE 15, CENTER.H(LON(5)): PRINT RTRIM$(LTRIM$(TITOLT(5)))
    LOCATE 16, CENTER.H(LON(6)): PRINT RTRIM$(LTRIM$(TITOLT(6))): COLOR 27
    LOCATE 19, CENTER.H(LON(7)): PRINT RTRIM$(LTRIM$(TITOLT(7)))

    'SO.LLARG
    MouseOn
    DO
      
    LOOP UNTIL INKEY$ <> "" OR MouseDown(1)
    CLS
    MouseOff
END SUB

SUB ScrMouseDemo
    COLOR 7, 9: CLS
    FINESTRA 1, 1, 3, 16, 1, CAIXA2

    LOCATE 2, 20: PRINT "  -Prova ja!, les noves utilitats de ratol° que inclueix"
    LOCATE 3, 20: PRINT "les noves bibliotäques versi¢ 3.0.": COLOR 0, 7
    LOCATE 25, 1: PRINT STRING$(80, " ");
    LOCATE 25, 1: PRINT " <ENTER> Seguir  <ESC> Sortir"; : COLOR 15, 9
    CALL MouseOn: P = 1
    DO
      C$ = INKEY$
      IF C$ = CHR$(13) THEN
	 IF P >= 2 THEN P = 1
	 GOSUB PROVES
	 P = P + 1
      END IF
      LOCATE 2, 2: PRINT "X="; MOUSEXT;
      LOCATE 2, 10: PRINT "Y="; MOUSEYT;
    LOOP UNTIL C$ = CHR$(27) OR MouseDown(3)
    MouseOff
    EXIT SUB

PROVES:
    SELECT CASE P
	   CASE 1
		MouseOff
		FINESTRA 5, 1, 23, 79, 1, CAIXA1: MouseOn
		CALL MouseLimit(1, 1, 5, 79, 23)
	   CASE ELSE
    END SELECT
    RETURN
END SUB

SUB ScrWindowsContDemo

    PALETTE 2, 9
    COLOR 15, 7
    CLS

    COLOR 15, 5: FINESTRA 4, 4, 10, 20, 1, CAIXA1: LOCATE 7, 8: PRINT "Finestra 1"
    COLOR 15, 3: FINESTRA 4, 22, 10, 38, 1, CAIXA2: LOCATE 7, 25: PRINT "Finestra 2"
    COLOR 15, 2: FINESTRA 4, 40, 10, 57, 1, CAIXA3: LOCATE 7, 44: PRINT "Finestra 3"

    COLOR 0, 7
    LOCATE 15, 10, 0: PRINT "-Fitxis be amb les distintes finestres, que tenen marcs distints"
    LOCATE 16, 10: PRINT "voste tambÇ pot canviar-los, tambÇ es possible controlar el"
    LOCATE 17, 10: PRINT "sombretjat. Amb el procediment Finestra:": COLOR 15
    LOCATE 19, 10: PRINT "                               Finestra(X,Y,X2,Y2,SHADOW,STILE$)": COLOR 0
    LOCATE 21, 10: PRINT "-Els estils dels marcs es passen per el parÖmetre STILE$": COLOR 15
    LOCATE 22, 10: PRINT "Exemple:"; : COLOR 0: PRINT " STILE$=" + CHR$(34) + "…Õª∫»Õº" + CHR$(34): COLOR 15
    COLOR 12, 2: LOCATE 25, 1: PRINT STRING$(80, " ");
    LOCATE 25, 1: PRINT " <ENTER> Seguir  <ESC> Sortir";

    DO
       C$ = INKEY$
       IF C$ = CHR$(27) THEN COLOR 7, 0: CLS : PALETTE 2, 2: SYSTEM
    LOOP UNTIL C$ = CHR$(13)

END SUB

SUB ScrWindowsDemo
    SHARED LINE$()
    COLOR 9, 9: FONS 219
    COLOR 15

    Y% = 1
    FOR X% = 1 TO 19 STEP 4
	COLOR RND * 12, RND * 15
	FINESTRA X%, Y%, X% + 10, Y% + 20, 1, CAIXA1: Y% = Y% + 9
	TEMPS (1)
    NEXT
    Y% = 60
    FOR X% = 1 TO 19 STEP 4
	COLOR RND * 15, RND * 14
	FINESTRA X%, Y%, X% + 10, Y% + 19, 1, CAIXA1: Y% = Y% - 9
	TEMPS (1)
    NEXT

    COLOR 7, 9
    MISS$ = "Aixï Çs una prova de la funci¢ Finestra": L = LEN(MISS$)

    FINESTRA (CENTER.V(1) - 1), (CENTER.H(L) - 2), (CENTER.V(1) + 2), (CENTER.H(L) + L + 1), 1, CAIXA2
    COLOR 15
    LOCATE CENTER.V(1), CENTER.H(L): PRINT MISS$
    COLOR 27: LOCATE CENTER.V(-2), CENTER.H(15): PRINT "Pitja una tecla"
    C$ = INPUT$(1)

    COLOR 15, 9
    CALL OBRIRCAIXA(3, 40)
    LOCATE 13, 23: PRINT "n"
    SetMaxCamps 0: SETCOLORCAMP 0, 15, 2, 15, 1, 15, 9
    SETINITCAMP 0, 13, 37, ASCI, 0, STRING$(22, "X"), "Fitxer a veure:"
    InsertValueCamp 0, "DEMO.TXT"
    DisplayAllCamps
    FOR C = 0 TO MAXCAMPS
       VALUE = ReadCamp(C)
       IF VALUE = 999 THEN EXIT SUB
       IF VALUE = F1 THEN
	  AjudaWin (1)
	  C = C - 1
       END IF
       IF VALUE = 0 THEN
	  IF DIR$(RTRIM$(LTRIM$(ValueCamp$(0)))) = "" OR RTRIM$(LTRIM$(ValueCamp$(0))) = "" THEN
	     't = AVIS("ERROR:", "El fitxer " + RTRIM$(LTRIM$(ValueCamp$(0))) + " no existeix", "Pitja una tecla...", 0)
	     BEEP
	     C = C - 1
	  END IF
       END IF
    NEXT

    AREATXT = FREEFILE
    OPEN RTRIM$(LTRIM$(ValueCamp$(0))) FOR INPUT AS AREATXT
    L = 0

    DO WHILE NOT EOF(AREATXT)
       LINE INPUT #AREATXT, D$
       LINE$(L) = MID$(D$, 1, 59)
       L = L + 1
    LOOP
    CLOSE AREATXT

    COLOR 1, 2
    CALL OBRIRCAIXA(15, 60): COLOR 15
    MISS$ = "Funcions Finestra, ScrollUp & ScrollDown"
    LINE$ = STRING$(59, "ƒ")
    LOCATE 6, CENTER.H(LEN(MISS$)): PRINT MISS$: COLOR 1
    LOCATE 7, CENTER.H(LEN(LINE$)): PRINT LINE$
    COLOR 2, 15: LOCATE 7, CENTER.H(LEN(NOMF$)): PRINT NOMF$
    GOSUB LLISTA

    XC = 8: L = 0
    DO
       t$ = INKEY$

       LOCATE XC, 11, 1, 13, 14
       SELECT CASE t$
	      CASE CHR$(0) + "P"
		   IF XC >= 20 THEN
		      SCROLLUP 19, 68, 7, 10, 1
		      XC = 20: L = L + 1
		      COLOR 0, 2
		      LOCATE 20, 11, 1, 13, 14: PRINT STRING$(59, " ");
		      LOCATE 20, 11, 1, 13, 14: PRINT LINE$(L)
		   ELSE
		     XC = XC + 1: L = L + 1
		     LOCATE XC, 11, 1, 13, 14
		   END IF
	      CASE CHR$(0) + "H"
		   IF XC <= 8 THEN
		      SCROLLDOWN 19, 68, 7, 10, 1
		      XC = 8: L = L - 1: IF L <= 0 THEN L = 0
		      COLOR 0, 2
		      LOCATE 8, 11, 1, 13, 14: PRINT STRING$(59, " ");
		      LOCATE 8, 11, 1, 13, 14: PRINT LINE$(L)
		   ELSE
		      XC = XC - 1: L = L - 1
		      LOCATE XC, 11, 1, 13, 14
		   END IF
	      CASE CHR$(27)
		   'O.LLARG
		   EXIT SUB
	      CASE CHR$(0) + ";"
		   AjudaWin (0)
	      CASE ELSE
       END SELECT
    LOOP

LLISTA:
    X = 8
    FOR L = 0 TO 12
	COLOR 0, 2
	LOCATE X, 11: PRINT LINE$(L)
	X = X + 1
    NEXT
    RETURN
END SUB

