' ********************************************************************
'
' Fitxer....................: GESTIO.BAS
' Titol.....................: Programa de facturaci¢, Control d'stock
'                             i reparacions.
' ********************************************************************
'
' Data inici................: 13/03/1997 20:38:00
' Data de la darrera revisi¢: 25/03/1997 01:45:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
'
' ********************************************************************
' Notes: AQUESTA ES L'AMPLIACI¢ DEL PROGRAMA FACTURACI¢ 2.0
'
' ********************************************************************

DECLARE FUNCTION CercaEXE! (NOM$)
DECLARE SUB LOADERRORS (NOM$)

COMMON SHARED MAXLINS

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CAMPS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\WIN.BI'

'DECLARE SUB LOADERRORS (NOM$)

'***************************************************************************
'************** DEFINICION DE VARIABLES CONSTANTES *************************
'***************************************************************************


'$DYNAMIC
DIM MENUO(0 TO 4) AS STRING, COMENTA$(0 TO 3, 0)
DIM SHARED SEL$(9, 9), COMMENT$(9, 9)
DIM SHARED col(2, 1)
DIM SHARED LINERR$(76)
DIM CFG AS CONFIG
DIM EMPR AS EMPRESA
DIM USR AS USUARIS
DIM CTRL AS CONTROL
DIM MENUS AS CMENU
DIM SPL AS SPOOL
DIM COLORS AS COLO
DIM PASO AS TRANS
DIM ANYS AS ANNO

'********************************************************************
'                          INICIALITZAR
'********************************************************************

ON ERROR GOTO ERRMISS
GOSUB TESTCOLORS
CARREGAT = 0
GOSUB COMPROVA      ' COMPROVA QUE EL PROGRAMA FUNCIONA AMB L'ORDINADOR QUE LI PERTOCA
GOSUB MASCARA       ' TREU LA PRIMERA PANTALLA
GOSUB TRIA.USUARI   ' SELECCIONAR L'USUARI
GOSUB OPCIONS       ' GENERAR LES OPCIONS DEL MEN£
GOSUB CREDITS

'********************************************************************
'                          MENU PRINCIPAL
'********************************************************************

OLDIR1$ = CURDIR$
'OLDMENU = 0: XOLD = 1: YOLD = 1: ACT% = 0
DO
MENU:
      CHDIR UNIDAD$: CHDRIVE (MID$(UNIDAD$, 1, 2))
      OLDIR$ = CURDIR$
      GOSUB MASCARA
      LOCATE 2, 1: PRINT STRING$(80, " ");
      LOCATE 3, 1: PRINT STRING$(80, "ƒ");
      LOCATE 23, 70: COLOR col(0, 0), col(0, 1): PRINT "<F1> AJUDA"
      CALL POPMENU(2, 2, 4, OPCIO%, SEL$(), COMMENT$(), col(0, 0), col(0, 1), col(1, 0), col(1, 1), col())

      IF OPCIO% = 999 THEN
	 CHDRIVE (MID$(OLDIR1$, 1, 2))
	 GOSUB TANCA.TOT
      END IF

      IF OPCIO% = 888 THEN
	 CHDIR MID$(MAIN$, 1, LEN(MAIN$) - 1): SHELL "BKHLP WELCOME": CHDIR OLDIR$
      END IF

      SUBMENU = VAL(MID$(LTRIM$(RTRIM$(STR$(OPCIO%))), 1, 1))
      OPC = VAL(MID$(LTRIM$(RTRIM$(STR$(OPCIO%))), 2, 1)) - 1

      IF CercaEXE(MENUS.PROC(OPC, SUBMENU)) = FALSE THEN
	 CAD$ = MENUS.PROC(OPC, SUBMENU): GOSUB PASSDADES
	 CHDIR MID$(MAIN$, 1, LEN(MAIN$) - 1): SHELL MENUS.PROC(OPC, SUBMENU): CHDIR OLDIR$
	 IF LTRIM$(RTRIM$(CAD$)) = "CONFIG.EXE" THEN GOSUB ACT.COLORS
      ELSE
	 SELECT CASE LTRIM$(RTRIM$(MENUS.PROC(OPC, SUBMENU)))
		CASE IS = "UTILS"
		     AVIS "AVIS:", "UTILITATS NO DISPONIBLES", 0
		CASE IS = "CANVI_EMPRES"
		     GOSUB TRIA.USUARI
		CASE IS = "ACABAR"
		     GOSUB SURT
		CASE IS = "MANT_ALBA"
		     GetBackground 1, 1, 25, 80, MENUa$
		     DO
			MENUO(1) = "Manteniment Albarans": COMENTA$(0, 0) = "El manteniment d'albarans"
			MENUO(2) = "Facturar Albarans   ": COMENTA$(1, 0) = "Facturar els albarans pendents"
			MENUO(3) = "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ": COMENTA$(2, 0) = "SEPARADOR"
			MENUO(4) = "Borrar Albarans     ": COMENTA$(3, 0) = "Borra albarans"
			CALL MENUBAR(MENUO(), COMENTA$(), 0, CASO%, 4, 50, 20, 4, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
			SELECT CASE CASO%
			       CASE 1
				    CAD$ = "ALT_ALBA.EXE": GOSUB PASSDADES
				    CHDIR MID$(MAIN$, 1, LEN(MAIN$) - 1)
				    SHELL CAD$
				    CHDIR OLDIR$
				    PutBackground 1, 1, MENUa$
			       CASE 2
			       CASE ELSE
			END SELECT
			
		     LOOP UNTIL CASO% = -19 OR CASO% = -4 OR CASO% = 999
		CASE IS = "MANT_TALLER"
		     GetBackground 1, 1, 25, 80, MENUa$
		     DO
			MENUO(1) = "Manteniment Resguards": COMENTA$(0, 0) = "El manteniment d'entrades al taller"
			MENUO(2) = "Facturar Resguards   ": COMENTA$(1, 0) = "Facturar sortides del taller"
			MENUO(3) = "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ": COMENTA$(2, 0) = "SEPARADOR"
			MENUO(4) = "Histïrics Resguards  ": COMENTA$(3, 0) = "Fer un seguiment d'una o mÇs reparacions"
			CALL MENUBAR(MENUO(), COMENTA$(), 0, CASO%, 4, 50, 21, 4, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
			SELECT CASE CASO%
			       CASE 1
				    CAD$ = "RES_001.EXE": GOSUB PASSDADES
				    CHDIR MID$(MAIN$, 1, LEN(MAIN$) - 1)
				    SHELL CAD$
				    CHDIR OLDIR$
				    PutBackground 1, 1, MENUa$
			       CASE 2
				    CAD$ = "RES_002.EXE": GOSUB PASSDADES
				    CHDIR MID$(MAIN$, 1, LEN(MAIN$) - 1)
				    SHELL CAD$
				    CHDIR OLDIR$
				    PutBackground 1, 1, MENUa$

			       CASE ELSE
			END SELECT

		     LOOP UNTIL CASO% = -19 OR CASO% = -4 OR CASO% = 999

		CASE IS = "MANT_FACTUR"
		     GetBackground 1, 1, 25, 80, MENUa$
		     DO
			MENUO(1) = "Facturaci¢ Directe     ": COMENTA$(0, 0) = "Crea factures diräctament"
			MENUO(2) = "Manteniment de Factures": COMENTA$(1, 0) = "Manteniment de factures. Altes, Baixes, Llistats, etc..."
			MENUO(3) = "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ": COMENTA$(2, 0) = "SEPARADOR"
			MENUO(4) = "Reimpresi¢ de factures ": COMENTA$(3, 0) = "Reimpresi¢ de les factures"
			CALL MENUBAR(MENUO(), COMENTA$(), 0, CASO%, 5, 50, 23, 4, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
			SELECT CASE CASO%
			       CASE 1
				    CAD$ = "ALT_FACT.EXE": GOSUB PASSDADES
				    CHDIR MID$(MAIN$, 1, LEN(MAIN$) - 1)
				    SHELL "ALT_FACT AUTOMATIC"
				    CHDIR OLDIR$
				    PutBackground 1, 1, MENUa$
			       CASE 2
				    CAD$ = "ALT_FACT.EXE": GOSUB PASSDADES
				    CHDIR MID$(MAIN$, 1, LEN(MAIN$) - 1)
				    SHELL "ALT_FACT MANTENIMENT"
				    CHDIR OLDIR$
				    PutBackground 1, 1, MENUa$
			       CASE ELSE
			END SELECT
		     LOOP UNTIL CASO% = -19 OR CASO% = -4 OR CASO% = 999

		CASE IS = ""
		CASE ELSE
		     BEEP
	 END SELECT
      END IF
      
LOOP

'***************************************************************************
'                         DEFINICI¢ DEL MENU PRINCIPAL
'***************************************************************************

OPCIONS:
       GetBackground 10, 10, 15, 71, ME$
       COLOR col(0, 0), col(0, 1)
       FINESTRA 10, 10, 14, 70, 1, CAIXA1
       LOCATE 12, 24: PRINT "CARREGANT ELS MENUS ->"
       AREAMENU = FREEFILE
       OPEN DIRECCR$ + "MENU_OP.DAT" FOR RANDOM SHARED AS AREAMENU LEN = LEN(MENUS)
       OP = 0: GET AREAMENU, 1, MENUS
       FOR MENU = 0 TO 6
	   TANT% = MENU * 100 / 6
	   LOCATE 12, 46: PRINT TANT%; " %"
	   DO
	     IF MENU = 0 THEN
		SEL$(OP, MENU) = RTRIM$(MENUS.OPCIO(OP, MENU)) + " "
		COMMENT$(OP, MENU) = RTRIM$(MENUS.Missatge(OP, MENU))
		OP = OP + 1
	     ELSE
		SEL$(OP, MENU) = RTRIM$(MENUS.OPCIO(OP, MENU))
		COMMENT$(OP, MENU) = RTRIM$(MENUS.Missatge(OP, MENU))
		OP = OP + 1
	     END IF
	   LOOP UNTIL OP = MENUS.CONTROL(MENU)
	   OP = 0
       NEXT
       CLOSE AREAMENU
       PutBackground 10, 10, ME$
RETURN

'***************************************************************************
'                         SELECCIONAR USUARI
'***************************************************************************
TRIA.USUARI:
      GetBackground 1, 1, 25, 80, LOGIN$

      IF CARREGAT = 0 THEN
	 COLOR col(0, 0), col(0, 1)
	 FINESTRA 10, 10, 14, 70, 1, CAIXA1
	 LOCATE 12, 24: PRINT "CARREGANT FITXERS DE CONFIGURACI¢"
	 OPEN "CONTROL.CFG" FOR RANDOM SHARED AS 3 LEN = LEN(CTRL)
	 OPEN "SPOOL.CFG" FOR RANDOM SHARED AS 4 LEN = LEN(SPL)
	 OPEN "EMPRESA.FAC" FOR RANDOM SHARED AS 2 LEN = LEN(EMPR)
	 OPEN "USUARIS.FAC" FOR RANDOM SHARED AS 5 LEN = LEN(USR)
	 GOSUB MASCARA
      END IF
      
      GET 3, 1, CTRL
      GET 5, CTRL.USUARI, USR

      IF SetInitWindows(1) = TRUE THEN
	 RETURN
      END IF
      IF CARREGAT = 0 THEN TITOL$ = "LOGIN" ELSE TITOL$ = "Canvi d'empresa"

      WIN1 = InitNewWindow(0, 7, 19, 17, 60, 1, TITOL$)
      SetAllColors col(0, 0), col(0, 1), col(1, 1), col(1, 0), col(2, 0), col(2, 1)
      SetStyleWindow 0, 0, ""
      ShowWindow 0
      
      LOCATE 12, 20: PRINT STRING$(60 - 20, "ƒ");
      
      SETMAXCAMPS 3
      FOR C = 0 TO 3
	  ERASEFIELD C
	  SETCOLORCAMPS C, col(1, 0), col(1, 1), col(0, 0), col(0, 1), col(2, 0), col(2, 1)
      NEXT
      INITCAMP 0, 9, 41, num, 0, "999", "Empresa Nß:"
      INITCAMP 1, 10, 41, ENCRIP, 15, "XXXXXXXXXX", "Clau:"
      INITCAMP 2, 14, 41, ASCI, 0, "XXXXXXXXXXX", "Usuari:"
      INITCAMP 3, 15, 41, ENCRIP, 15, "XXXXXXXXXX", "Clau:"

      INSERTCAMP 0, LTRIM$(STR$(CTRL.EMPRESA))
      INSERTCAMP 2, RTRIM$(LTRIM$(USR.NOM))
      
      CTRL.EMPRESA = VAL(SHOWFIELD$(0))
      GET 1, VAL(SHOWFIELD$(0)), CFG     ' RECULL LA CONFIGURACIO DE LA EMPRESA
      GET 2, VAL(SHOWFIELD$(0)), EMPR    ' RECULL LA CONFIGURACI¢ SECUNDARIA
      GET 4, EMPR.IMPRESORA, SPL
      
      USUARI = VAL(SHOWFIELD$(0))
      NOMUS$ = CFG.NOM
      LOCATE 1, 11: PRINT SPACE$(30): LOCATE 1, 11
      COLOR 7: PRINT RTRIM$(STR$(USUARI)) + " "; : COLOR 15: PRINT RTRIM$(NOMUS$)
      DISPLAYALL
      '
      ' ESPECIFICAR USUARI I CLAU
      '
      IF CARREGAT = 0 THEN CI = 2: CFI = 3 ELSE CI = 0: CFI = 3
      FOR C = CI TO CFI
	  VALUE = LLEGIRCAMP(C)
	  SELECT CASE VALUE
		 CASE 0
		      WRONG = FALSE
		      IF VAL(SHOWFIELD$(0)) > CFG.MAXREG - 1 OR VAL(SHOWFIELD$(0)) = 0 THEN
			 ER1$ = "NUMERO D'EMPRESA INCORRECTE"
			 GOSUB MIS
		      END IF
		      CTRL.EMPRESA = VAL(SHOWFIELD$(0))
		      GET 1, VAL(SHOWFIELD$(0)), CFG     ' RECULL LA CONFIGURACIO DE LA EMPRESA
		      GET 2, VAL(SHOWFIELD$(0)), EMPR    ' RECULL LA CONFIGURACI¢ SECUNDARIA
		      GET 4, EMPR.IMPRESORA, SPL
		      PUT 3, 1, CTRL
		      USUARI = VAL(SHOWFIELD$(0))
		      NOMUS$ = CFG.NOM
		      LOCATE 1, 11: PRINT SPACE$(30): LOCATE 1, 11
		      COLOR 7: PRINT RTRIM$(STR$(USUARI)) + " "; : COLOR 15: PRINT RTRIM$(NOMUS$)
		 CASE 1
		      PASS$ = ENCRIPT$(EMPR.PASSWORD, 15)
		      CLAU$ = RTRIM$(SHOWFIELD$(1))
		      PASS$ = RTRIM$(PASS$)

		      FOR L = 1 TO LEN(PASS$)
			  IF MID$(PASS$, L, 1) = "/" THEN MID$(PASS$, L, 1) = CHR$(32)
			  IF MID$(CLAU$, L, 1) = "/" THEN MID$(CLAU$, L, 1) = CHR$(32)
		      NEXT
 
		      CL$ = UCASE$(LTRIM$(RTRIM$(CLAU$)))
		      PA$ = UCASE$(LTRIM$(RTRIM$(PASS$)))

		      EM = VAL(SHOWFIELD$(0))
		      IF CL$ <> PA$ THEN
			 ER2$ = "CLAU D'ACCESS INCORRECTE"
			 GOSUB MIS
		      END IF

		 CASE 2
		      MAXUS = LOF(5) \ LEN(USR): RT = 0
		      FOR R = 1 TO MAXUS
			  GET 5, R, USR
			  IF UCASE$(LTRIM$(RTRIM$(USR.NOM))) = UCASE$(LTRIM$(RTRIM$(SHOWFIELD$(2)))) THEN
			     RT = R
			     EXIT FOR
			  END IF
		      NEXT
		      IF RT = 0 THEN
			 ER1$ = "NOM D'USUARI INCORRECTE"
			 GOSUB MIS
		      END IF
		      CLAUUS$ = USR.PASSWORD
		      GET 3, 1, CTRL
		      CTRL.USUARI = RT
		      PUT 3, 1, CTRL
		 CASE 3
		      PASS$ = UCASE$(ENCRIPT$(CLAUUS$, 15))
		      CLAU$ = UCASE$(RTRIM$(SHOWFIELD$(3)))

		      FOR L = 1 TO LEN(PASS$)
			  IF MID$(PASS$, L, 1) = "/" THEN MID$(PASS$, L, 1) = CHR$(32)
			  IF MID$(CLAU$, L, 1) = "/" THEN MID$(CLAU$, L, 1) = CHR$(32)
		      NEXT

		      CL$ = LTRIM$(RTRIM$(CLAU$))
		      PA$ = LTRIM$(RTRIM$(PASS$))
		      
		      IF CL$ <> PA$ THEN
			 INSERTCAMP 3, "€€€€€€€€€€": DISPLAYALL
			 ER2$ = "CLAU D'ACCESS INCORRECTE"
			 GOSUB MIS
		      END IF
		      INSERTCAMP 3, "€€€€€€€€€€": DISPLAYALL
		      EM = VAL(SHOWFIELD$(0))
		 CASE F1
		      CHDIR "MAIN": SHELL "BKHLP ENTRADA": CHDIR ".."
		      C = C - 1
		 CASE F2 TO F10
		      C = C - 1: SOUND 50, .5
		 CASE 999
		      IF CARREGAT = 0 THEN
			 COLOR 7, 0: CLS : RESET
			 SYSTEM
		      ELSE
			 DeleteWindow 0
			 GOSUB MASCARA
			 RETURN
		      END IF
		 CASE ELSE
	  END SELECT
      NEXT

      UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
      DBF$ = LTRIM$(RTRIM$(CFG.DDADE))             ' Assignar direcctoris
      SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
      MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
      HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
      SYS$ = LTRIM$(RTRIM$(CFG.DSIST))
      MAIN$ = LTRIM$(RTRIM$(CFG.DMAIN))
      
      DIRECCF$ = DBF$           ' Subdirecctori de les base de dades
      DIRECCP$ = SCR$           ' Subdirecctori de les pantalles
      DIRECCR$ = MSK$           ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = HLP$          ' Subdirecctori de ajuda
      DIRECCI$ = "IMPRESOR\"    ' Subdirecctori de les impresores
      DIRECCS$ = SYS$
      
      USUARI = VAL(SHOWFIELD$(0))
      US = CTRL.USUARI
      NOMEM$ = CFG.NOM
      NOMUS$ = USR.NOM
      POBLA$ = CFG.POBLA
      MAXLINS = USR.MAXLINS
      MAXFACLINS = USR.LINFACT
      IVA = EMPR.IVA
      DTO = EMPR.DTO
      DEV$ = LTRIM$(RTRIM$(EMPR.DEVICE))
      IMPRESORA = EMPR.IMPRESORA
      ANYE = EMPR.ANY

      COLOR col(0, 0), col(0, 1)
      FINESTRA 10, 10, 14, 70, 1, CAIXA1
      LOCATE 12, 24: PRINT "CARREGANT ELS MISSATGES D'ERRORS ..."
      LOADERRORS DIRECCR$ + "ERRORS.ERR"
      LOCATE 12, 24: PRINT "CARREGANT ELS COLORS ...            "

      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, ANYE, ANYS
      ANY$ = ANYS.ANY
      CLOSE AREAA

      GOSUB ACT.COLORS
      CARREGAT = 999
      PutBackground 1, 1, LOGIN$
      GOSUB MASCARA
      LPRINT CHR$(27) + "C" + CHR$(32)
      RETURN

ACT.COLORS:
      AREAC = FREEFILE: OPEN DIRECCF$ + ANY$ + "\COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
      GET AREAC, 1, COLORS
      col(0, 0) = COLORS.col(0, 0): col(0, 1) = COLORS.col(0, 1)
      col(1, 0) = COLORS.col(1, 0): col(1, 1) = COLORS.col(1, 1)
      col(2, 0) = COLORS.col(2, 0): col(2, 1) = COLORS.col(2, 1)
      CLOSE AREAC
      RETURN
'***************************************************************************
'                         MASCARA DEL MENU PRINCIPAL
'***************************************************************************

MASCARA:
      CLS : COLOR col(0, 0), col(0, 1): LOCATE , , 0
      FONS (177): FINESTRA 21, 1, 25, 80, 1, CAIXA1: LOCATE 1, 1: PRINT SPACE$(80)
      NOMEM$ = CFG.NOM
      NOMUS$ = USR.NOM

      COLOR col(2, 0), col(2, 1): LOCATE 1, 64: PRINT "FACTURACI¢ " + versio
      COLOR col(2, 0), col(2, 1): LOCATE 1, 2: PRINT "EMPRESA: ";
      COLOR col(0, 0), col(0, 1): PRINT RTRIM$(STR$(EM)) + " ";
      COLOR col(2, 0), col(2, 1): PRINT RTRIM$(NOMEM$)
      
      COLOR col(2, 0), col(2, 1): LOCATE 22, 26: PRINT "      USUARI: ";
      COLOR col(0, 0), col(0, 1): PRINT RTRIM$(STR$(US)) + " ";
      COLOR col(2, 0), col(2, 1): PRINT RTRIM$(NOMUS$)
      
      LOCATE 23, 70: COLOR col(0, 0): PRINT "<F1> AJUDA"
      
      RETURN

'***************************************************************************
'                COMPROVA SI EL PROGRAMA ESTA BEN INSTAL.LAT
'***************************************************************************

COMPROVA:
      
      LOCATE 25, 1: PRINT "*** COMPROVANT SI EL PROGRAMA ESTA BEN INSTAL.LAT";
      OPEN "CONFIG.FAC" FOR RANDOM SHARED AS 1 LEN = LEN(CFG)
      
      GET 1, 1, CFG
      'CFG.DRIVE = "C:\FACT3"
      'CFG.DDADE = "C:\FACT3\FACTU001\"             ' Assignar direcctoris
      'CFG.DPANT = "C:\FACT3\PANTS\"
      'CFG.DRECU = "C:\FACT3\RECURSOS\"
      'CFG.DHELP = "C:\FACT3\HELP\"
      'CFG.DSIST = "C:\FACT3\SYS\"
      'CFG.DMAIN = "C:\FACT3\MAIN\"
      'CFG.DEXPO = "C:\FACT3\EXPORT\"
      'CFG.DIMPO = "C:\FACT3\IMPORT\"
      'PUT 1, 1, CFG
      IF CFG.INSTALAT = "INSTAL.LAT" THEN
	 IF CFG.VAR <> TESTSCREEN THEN
	    LOCATE 25, 1: PRINT "ERROR: EL PROGRAMA ES DEFECTUOS O NO ESTÖ BEN INSTAL.LAT   ";
	    CFG.INSTALAT = "TEXT!!!!!!"
	    PUT 1, 1, CFG
	    SYSTEM
	 END IF

	 CAD$ = ENCRIPT$(CFG.INST, 45)          ' DESENCRIPTAR EL COPYRIGHT

	 IF CAD$ <> MID$(VERSVGA$, 1, 29) THEN  ' COMPROVAR SI ES CORRECTE
	    LOCATE 25, 1: PRINT "ERROR: LA CONFIGURACI¢ DEL ORDINADOR ES ERRONEA            ";
	    CFG.INSTALAT = "WRONG!!!!!"
	    PUT 1, 1, CFG
	    SYSTEM
	 END IF
      ELSE
	 IF RTRIM$(CFG.INSTALAT) = "WRONG!!!!!" THEN
	    BEEP
	    AVIS "ERROR:", "PROGRAMA PROTEGIT. PROC MAIN LOCKED!!!!", 1
	    SYSTEM
	 END IF
	 IF RTRIM$(CFG.INSTALAT) <> "PRGTOM0710" THEN
	    CFG.INSTALAT = "          "
	    PUT 1, 1, CFG
	    SYSTEM
	 END IF
      END IF
      LOCATE 25, 1: PRINT "*** COMPROVANT EL NUMERO DE SäRIE                ";
      IF DIR$("SERIAL.NUM") = "" THEN
	 BEEP
	 AVIS "ERROR:", "PROGRAMA PROTEGIT. PROC MAIN LOCKED!!!!", 1
	 SYSTEM
      END IF
      OPEN "SERIAL.NUM" FOR RANDOM AS 10 LEN = 23
      FIELD 10, 23 AS NUMSER$
      GET 10, 1
      NS$ = SPACE$(23)
      FOR L = 1 TO 23: MID$(NS$, L, 1) = ENCRIPT$(MID$(NUMSER$, L, 1), ASC(MID$(CAD$, L, 1))): NEXT
      IF MID$(NUMSER$, 11, 3) = "DSS" THEN
	 LSET NUMSER$ = NS$
	 PUT 10, 1
	 NS$ = SPACE$(23)
	 FOR L = 1 TO 23: MID$(NS$, L, 1) = ENCRIPT$(MID$(NUMSER$, L, 1), ASC(MID$(CAD$, L, 1))): NEXT
      END IF
      CLOSE 10
      RETURN


'***************************************************************************
' TANCAMENT DE TOTS ELS FITXERS
'***************************************************************************

CIERRA:
      RESET
      RETURN


SURT:
       DIM OP$(2)
       OP$(1) = "          Aceptar": OP$(2) = "         Cancel.lar"
       AVIS.SONOR (3)
       OPC = ACHOICE(10, 20, 15, 52, 2, OP$(), col(), " Desitja sortir del programa?  ", 1)
       IF OPC = 1 THEN GOSUB TANCA.TOT
       ERASE OP$
       COLOR col(0, 0), col(0, 1)
       RETURN

CREDITS:
       IF SetInitWindows(1) = TRUE THEN
	  RETURN
       END IF

       WIN2 = InitNewWindow(0, 5, 10, 20, 70, 1, "Sobre ell...")
       SetAllColors col(0, 0), col(0, 1), col(1, 1), col(1, 0), col(2, 0), col(2, 1)
       SetStyleWindow 0, 0, ""
       ShowWindow 0

       COLOR col(2, 0), col(2, 1)
       CENTRAR 6, PROGRAMA + " " + versio
       COLOR col(0, 0), col(0, 1)
       CENTRAR 7, "Programa de facturaci¢+control d'stock+reparacions"
       CENTRAR 9, COPYRIGHT
       CENTRAR 10, AUTOR: COLOR col(2, 0), col(2, 1)
       LOCATE 12, 11: PRINT "         N£mero de särie: "; : COLOR col(0, 0), col(0, 1): PRINT NS$
       LOCATE 13, 11:  COLOR col(2, 0), col(2, 1): PRINT "     Empresa autoritzada: "; : COLOR col(0, 0), col(0, 1): PRINT RTRIM$(NOMEM$) + " Any:" + ANY$
       LOCATE 14, 11:  COLOR col(2, 0), col(2, 1): PRINT " Direcctori de l'empresa: "; : COLOR col(0, 0), col(0, 1): PRINT DIRECCF$ + ANY$ + "\"
       LOCATE 15, 11:  COLOR col(2, 0), col(2, 1): PRINT "               Impresora: "; : COLOR col(0, 0), col(0, 1): PRINT SPL.DESCRIPCION
     LOCATE 16, 11, 0: COLOR col(2, 0), col(2, 1): PRINT "    Memïria convencional:"; : COLOR col(0, 0), col(0, 1)
       PRINT USING "##,###,### Octets disponibles"; FRE(-1)

       COLOR 15: LOCATE 19, 11: PRINT "              Pitja una tecla per a continuar ..."
       C$ = INPUT$(1)
       DeleteWindow 0
       PackWindows
       RETURN


MIS:   COLOR col(0, 0), col(0, 1): AVIS.SONOR (1)
       AVIS "ERROR:", ER1$ + ER2$, 0
       IF CARREGAT = 0 THEN
	  GOSUB CIERRA
	  RESET: CLR: SYSTEM
       ELSE
	  RETURN MENU
       END IF

       RETURN

TESTCOLORS:
	 CALL CLR: LOCATE 24, 1
	 IF TESTSCREEN = &HB000 THEN
	    col(0, 0) = 7: col(0, 1) = 0
	    col(1, 0) = 0: col(1, 1) = 7
	    col(2, 0) = 15: col(2, 1) = 0
	    PRINT "*** MONO DETECTED";
	 ELSE
	    col(0, 0) = 7: col(0, 1) = 1
	    col(1, 0) = 14: col(1, 1) = 3
	    col(2, 0) = 15: col(2, 1) = 1
	    PRINT "*** COLOR DETECTED";
	 END IF
	 RETURN


ERRMISS:   AVIS.SONOR (1)
	   AVIS "ERROR:", LINERR$(ERR), 0
	   RESUME NEXT


TANCA.TOT:
	  COLOR col(0, 0), col(0, 1): CLS
	  FINESTRA 1, 1, 4, 79, 1, CAIXA1
	  COLOR col(2, 0), col(2, 1)
	  CENTRAR 2, "*** FI DE LA SESI¢ AMB " + PROGRAMA + " " + versio + " ***"
	  COLOR col(1, 0), col(2, 1)
	  LOCATE 3, 2: PRINT STRING$(77, " ");
	  CENTRAR 3, COPYRIGHT
	  PRINT : PRINT : PRINT
	  GOSUB CIERRA
	  SYSTEM

PASSDADES:
	  TMP$ = ENVIRON$("TEMPORAL")
	  DEF SEG = VARSEG(CFG)
	  BSAVE TMP$ + "PASS.TMP", VARPTR(CFG), LEN(CFG)
	  DEF SEG

	  DEF SEG = VARSEG(EMPR)
	  BSAVE TMP$ + "PASE.TMP", VARPTR(EMPR), LEN(EMPR) + 16
	  DEF SEG

	  DEF SEG = VARSEG(USR)
	  BSAVE TMP$ + "PASU.TMP", VARPTR(USR), LEN(USR)
	  DEF SEG
	  
	  CA$ = ""
	  FOR L% = 1 TO LEN(CAD$)
	      CA$ = CA$ + ENCRIPT$(MID$(CAD$, L%, 1), L%)
	  NEXT

	  PASO.APLICACIO = CAD$
	  PASO.CLAU = CA$

	  DEF SEG = VARSEG(PASS)
	  BSAVE TMP$ + "PROT.TMP", VARPTR(PASO), LEN(PASO)
	  DEF SEG
	  RETURN

FUNCTION CercaEXE (NOM$)
    NOM$ = LTRIM$(RTRIM$(NOM$))
    CercaEXE = TRUE
    IF INSTR(1, NOM$, ".EXE") THEN CercaEXE = FALSE
END FUNCTION

SUB LOADERRORS (NOM$)

OPEN NOM$ FOR INPUT AS #23

LIN$ = ""
DO UNTIL EOF(23)
   INPUT #23, LIN$

      IF MID$(LIN$, 1, 1) = ";" OR MID$(LIN$, 1, 1) = " " OR MID$(LIN$, 1, 1) = "" THEN

      ELSE
	 NUMER% = VAL(MID$(LIN$, 1, 2))
	 COMM$ = MID$(LIN$, 4, 30)
	 LINERR$(NUMER%) = COMM$
      END IF
LOOP

CLOSE #23
END SUB

SUB TROS

END SUB

