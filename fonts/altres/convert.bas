DECLARE FUNCTION AgafarFitxersMes! (MES$, ANNO$, FILE$())
DECLARE SUB DefinirMesos (LLENG!, MESOS$())

COMMON SHARED MAXLINS
COMMON SHARED DIRECCF$, DIRECCP$, DIRECCU$

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'CAMPS.BI'
'$INCLUDE: 'C:\FACT2\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT2\FONTS\VIDEOF.BI'
'$INCLUDE: 'C:\FACT2\FONTS\DMOUSE.BI'


'***************************************************************************
'************** DEFINICION DE VARIABLES CONSTANTES *************************
'***************************************************************************
CONST NOMUS$ = "ESTRELLA DE MAR"


'$DYNAMIC
DIM SHARED SEL$(4, 4), COMMENT$(4, 4)
DIM SHARED COL(2, 1)
DIM SHARED MESOS$(12)
DIM SHARED FILE$(1500)
DIM CFG AS CONFIG
DIM EMPR AS EMPRESA
DIM CTRL AS CONTROL
DIM MENUS AS CMENU
DIM SPL AS SPOOL
DIM COLORS AS COLO

IF TESTSCREEN = &HB000 THEN
   COL(0, 0) = 15: COL(0, 1) = 3
   COL(1, 0) = 14: COL(1, 1) = 1
   COL(2, 0) = 14: COL(2, 1) = 3
   SEL$(2, 3) = "PANTALLA COLOR   ": COMMENT$(2, 3) = "Canviar pantalla de B/W a Color"
ELSE
   COL(0, 0) = 15: COL(0, 1) = 3
   COL(1, 0) = 14: COL(1, 1) = 1
   COL(2, 0) = 14: COL(2, 1) = 3
   SEL$(2, 3) = "PANTALLA B/W     ": COMMENT$(2, 3) = "Canviar pantalla de Color a B/W"
END IF

'********************************************************************
'                          MENU PRINCIPAL
'********************************************************************
      'GOSUB INICIAR
      GOSUB MASCARA
      GOSUB AJUDA
      GOSUB DEFINEMENU
      CALL DefinirMesos(LLENGUA, MESOS$())
DO
      GOSUB MASCARA
      CALL POPMENU(1, 1, 2, OPCIO%, SEL$(), COMMENT$(), COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1), COL())
      SELECT CASE OPCIO%
	     CASE 11
		  GOSUB MANUAL
	     CASE 12
		  GOSUB TRANSFEREIX
	     CASE 31, 999
		  COLOR 7, 0: CLS : COLOR 15, 9: PRINT SPACE$(80);
		  CENTRAR 1, "CONVERSOR 1.0 - Conversor de dades": COLOR 7, 0: PRINT : PRINT : COLOR 11
		  PRINT "DRAC 1993/96 (C). Summer'96 (C)": COLOR 14
		  PRINT "Tomeu Cap¢ Cap¢ 1996 (C)"
		  PRINT : COLOR 12
		  PRINT "Telf & Fax-Modem:"; : COLOR 14: PRINT " 971-540778": COLOR 12
		  PRINT "e-mail: "; : COLOR 27: PRINT "<"; : COLOR 14: PRINT "tcc@ctv.es"; : COLOR 27: PRINT ">"
		  COLOR 7, 0: PRINT
		  ERASE FILE$, MESOS$
		  SYSTEM
	     CASE 32
		  IF LLENGUA = 0 THEN
		     LLENGUA = 1
		     CALL DefinirMesos(LLENGUA, MESOS$())
		  ELSE
		     LLENGUA = 0
		     CALL DefinirMesos(LLENGUA, MESOS$())
		  END IF
	     CASE 33
		  IF COL(0, 0) = 7 THEN
		     COL(0, 0) = 15: COL(0, 1) = 3
		     COL(1, 0) = 14: COL(1, 1) = 1
		     COL(2, 0) = 14: COL(2, 1) = 3
		     SEL$(2, 3) = "PANTALLA B/W     ": COMMENT$(2, 3) = "Canviar pantalla de Color a B/W"
		  ELSE
		     COL(0, 0) = 7: COL(0, 1) = 0
		     COL(1, 0) = 0: COL(1, 1) = 7
		     COL(2, 0) = 15: COL(2, 1) = 0
		     SEL$(2, 3) = "PANTALLA COLOR   ": COMMENT$(2, 3) = "Canviar pantalla de B/W a Color"
		  END IF
		  GOSUB MASCARA
	     CASE 34
		  GOSUB AJUDA
	     CASE ELSE
		  SOUND 50, .5
      END SELECT
LOOP

MANUAL:
      CALL MENU2(MESOS$(), CASO%, 7, 10, 15, 12, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
      IF CASO% = 999 THEN RETURN
      MANUAL = 99
      MES$ = LTRIM$(RTRIM$(STR$(CASO%)))
      IF LEN(MES$) = 1 THEN MES$ = "0" + MES$
      ANNO$ = MID$(DATE$, 9, 10)
      SETMAXCAMPS 0: SETCOLORCAMPS 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
      INITCAMP 0, 8, 35, ASCI, 0, "XXXX", "Any:"
      FINESTRA 7, 26, 9, 50, 1, CAIXA1
      INSERTCAMP 0, MID$(DATE$, 7, 10)
      DISPLAYALL
      FOR C = 0 TO 0
	 VALUE = LLEGIRCAMP(C)
	 SELECT CASE VALUE
		CASE IS = 0
		     IF LTRIM$(RTRIM$(SHOWFIELD$(0))) > MID$(DATE$, 7, 10) THEN
			CENTRAR 24, "ERROR DATA INCORRECTA"
			BEEP: CENTRAR 24, "                     "
			C = C - 1
		     END IF
		CASE IS = 999
		     MANUAL = 0
		     RETURN
		CASE F1 TO F10
		     SOUND 50, .5
		     C = C - 1
		CASE ELSE
	 END SELECT
      NEXT
      ANNO$ = MID$(SHOWFIELD$(0), 3, 2)
      F$ = "01/" + MES$ + "/" + ANNO$
      METOD$ = "manual"
      GOSUB TRANSFEREIX
      MANUAL = 0
      RETURN

TRANSFEREIX:
      IF MANUAL = 0 THEN
	 MES$ = MID$(DATE$, 1, 2)
	 ANNO$ = MID$(DATE$, 9, 10)
	 F$ = FETXA$
	 METOD$ = "per detecci¢ automÖtica de la data"
      END IF
      P = AgafarFitxersMes(MES$, ANNO$, FILE$())
      NOM$ = MESOS$(VAL(MES$)) + ".TXT"
      FINESTRA 10, 14, 20, 60, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
      LOCATE 11, 15: PRINT "Conversi¢ " + METOD$
      LOCATE 12, 15: PRINT STRING$(45, "ƒ");
      LOCATE 14, 19: PRINT "Mes actual: "; : COLOR COL(0, 0), COL(0, 1): PRINT MESOS$(VAL(MES$))
      COLOR COL(2, 0), COL(2, 1): LOCATE 15, 19: PRINT "Data......: "; : COLOR COL(0, 0), COL(0, 1): PRINT F$
      COLOR COL(2, 0), COL(2, 1): LOCATE 16, 19: PRINT "Fitxer....: "; : COLOR COL(0, 0), COL(0, 1): PRINT NOM$
      COLOR 26: LOCATE 18, 19: PRINT "Pitja una tecla per continuar"
      C$ = INPUT$(1): IF C$ = CHR$(27) THEN RETURN
      IF FILE$(1) = "" THEN
	 BEEP
	 COLOR 20: LOCATE 18, 19: PRINT "No hi ha cap lectura feta.   "
	 C$ = INPUT$(1): IF C$ = CHR$(27) THEN RETURN
	 RETURN
      END IF
      COLOR 15: LOCATE 18, 19: PRINT "Creant fitxer                "
      OPEN NOM$ FOR OUTPUT AS #1
      PAG = 1: GOSUB CAPSA
      MASC$ = " \\             \        \    \        \    \        \    \      \    \      \"
      FOR F = 1 TO P
	  AREATXT = FREEFILE: OPEN FILE$(F) FOR INPUT AS #AREATXT
	  COLOR COL(0, 0): LOCATE 18, 19: PRINT "Cercant fitxer "; FILE$(F); "  "
	  DO
	    COLOR COL(0, 0): LOCATE 19, 19: PRINT "Juntant fitxer "; FILE$(F); "  "
	    LINE INPUT #AREATXT, LIN$
	    FET$ = MID$(LIN$, 2, 10): HORA$ = MID$(LIN$, 15, 23)
	    LINE INPUT #AREATXT, LIN$
	    PERIF$ = MID$(LIN$, 30, 31): KWH$ = MID$(LIN$, 41, 50)
	    KVARXL$ = MID$(LIN$, 54, 63): KVARXC$ = MID$(LIN$, 67, 76)
	    PRINT #1, USING MASC$; PERIF$; KWH$; KVARXL$; KVARXC$; FET$; HORA$
	  LOOP UNTIL EOF(AREATXT)
	  CLOSE #AREATXT
      NEXT
      PRINT #1, " " + STRING$(78, "ƒ")
      PRINT #1, ""
      CLOSE #1
      PLAY "L20GBGBG"
      COLOR 26: LOCATE 18, 19: PRINT "Pitja una tecla per continuar"
      C$ = INPUT$(1): IF C$ = CHR$(27) THEN RETURN
      RETURN
      
CAPSA:
      PRINT #1, ""
      PRINT #1, " APARTAMENTS ESTRELLA DE MAR"
      PRINT #1, " Llistat resum del consum eläctric"
      PRINT #1, ""
      PRINT #1, "          MES: "; MESOS$(VAL(MES$))
      PRINT #1, "       PAGINA: "; PAG
      PRINT #1, " DATA LLISTAT: "; FETXA$
      PRINT #1, ""
      PRINT #1, " " + STRING$(78, "ƒ")
      PRINT #1, " Perifäric      kW/h          kvar/h  Xl    kvar/h  Xc    Data        Hora"
      PRINT #1, " -----------    ----------    ----------    ----------    --------    --------"
      RETURN

DEFINEMENU:

       SEL$(0, 0) = " CONVERSI¢ ": COMMENT$(0, 0) = "Converter 1.0"
       SEL$(1, 0) = " ESTADISTICA ": COMMENT$(1, 0) = "Utilitat per fer una estadistica dels valors"
       SEL$(2, 0) = " SISTEMA ": COMMENT$(2, 0) = "Utilitats de sistema"

       SEL$(0, 1) = "CONVERSI¢ MANUAL     ": COMMENT$(0, 1) = "L'usuari ha de seleccionar el mes"
       SEL$(1, 1) = "CONVERSI¢ AUTOMATICA ": COMMENT$(1, 1) = "El programa per defäcte el mes actual"
       
       SEL$(0, 3) = "SORTIR           ": COMMENT$(0, 3) = "Sortir del programa"
       SEL$(1, 3) = "SELECCIONA IDIOMA": COMMENT$(1, 3) = "*** NO FET ***"
       SEL$(3, 3) = "SOBRE EL PROGRAMA": COMMENT$(3, 3) = "About this program :)"
       RETURN

AJUDA:
       COLOR 15, COL(0, 1)
       FINESTRA 5, 10, 19, 70, 1, CAIXA1
       COLOR COL(2, 0): LOCATE 6, 11: PRINT "CONVESOR 1.0": COLOR COL(0, 0)
       LOCATE 8, 11: PRINT "Programa per l'conversi¢ de fitxers WAT->TXT per mesos"
       LOCATE 9, 11: PRINT "Desenvolupat per DRAC 1993/96 (C)"
       LOCATE 10, 11: PRINT "Tomeu Cap¢ Cap¢ 1996 (C)"
       LOCATE 13, 11: COLOR COL(2, 0): PRINT "        N£mero de särie: "; : COLOR COL(0, 0): PRINT "5866620115-DS-002-10G21"
       LOCATE 14, 11: COLOR COL(2, 0): PRINT "    Empresa autoritzada: "; : COLOR COL(0, 0): PRINT RTRIM$(NOMUS$)
       LOCATE 15, 11: COLOR COL(2, 0): PRINT "     Memïria disponible: "; : COLOR COL(0, 0): PRINT FRE(-1); " Octets"
       COLOR 15: LOCATE 18, 11: PRINT "Pitja una tecla per a continuar ..."
       T$ = INPUT$(1)
       RETURN

MASCARA:
      COLOR COL(0, 0), COL(0, 1)
      FONS (177): FINESTRA 21, 1, 25, 80, 1, CAIXA1: LOCATE 1, 1: PRINT SPACE$(80)
      COLOR COL(2, 0), COL(2, 1): LOCATE 1, 52: PRINT "CONVERSOR 1.0. DRAC'96(C)"
      RETURN

REM $STATIC
FUNCTION AgafarFitxersMes (MES$, ANNO$, FILE$())
    X$ = DIR$("???" + MES$ + ANNO$ + ".WAT"): P = 1
    DO WHILE X$ <> ""
       FILE$(P) = X$
       X$ = DIR$
       P = P + 1
    LOOP
    P = P - 1
    AgafarFitxersMes = P
END FUNCTION

SUB DefinirMesos (LLENG, MESOS$())
    SELECT CASE LLENG
	   CASE 0
		MESOS$(1) = "GENER"
		MESOS$(2) = "FEBRER"
		MESOS$(3) = "MARÄ"
		MESOS$(4) = "ABRIL"
		MESOS$(5) = "MAIG"
		MESOS$(6) = "JUNY"
		MESOS$(7) = "JULIOL"
		MESOS$(8) = "AGOST"
		MESOS$(9) = "SEPTEMBRE"
		MESOS$(10) = "OCTUBRE"
		MESOS$(11) = "NOVEMBRE"
		MESOS$(12) = "DECEMBRE"
	   CASE 1
		MESOS$(1) = "ENERO"
		MESOS$(2) = "FEBRERO"
		MESOS$(3) = "MARZO"
		MESOS$(4) = "ABRIL"
		MESOS$(5) = "MAYO"
		MESOS$(6) = "JUNIO"
		MESOS$(7) = "JULIO"
		MESOS$(8) = "AGOSTO"
		MESOS$(9) = "SEPTIMBRE"
		MESOS$(10) = "OCTUBRE"
		MESOS$(11) = "NOVIEMBRE"
		MESOS$(12) = "DICIEMBRE"
	   CASE 2
	   CASE ELSE
    END SELECT
END SUB

