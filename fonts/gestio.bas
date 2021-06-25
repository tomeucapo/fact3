' ********************************************************************
'
' Fitxer....................: GESTIO.BAS
' Titol.....................: Programa de facturaci¢, Control d'stock
'                             i reparacions.
' ********************************************************************
'
' Data inici................: 13/03/1997 20:38:00    (Data del FACT3)
' Data de la darrera revisi¢: 01/04/1998 16:17:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/98 (C)
'
' ********************************************************************
' Notes: AQUESTA ES L'AMPLIACI¢ DEL FACTURACI¢ 2.0
'
' ********************************************************************

DECLARE FUNCTION DOSMultiplexorTest! (DRV!)
DECLARE FUNCTION CercaEXE! (NOM$)
DECLARE FUNCTION ActivarFelicitacion$ ()

COMMON SHARED MAXLINS
'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'D:\FACT3\FONTS\DMOUSE.BI'
'$INCLUDE: 'D:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'D:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'D:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'D:\FACT3\FONTS\WIN.BI'
'$INCLUDE: 'D:\FACT3\FONTS\CONSTS.BI'


'***************************************************************************
'************** DEFINICION DE VARIABLES CONSTANTES *************************
'***************************************************************************
TYPE DF
     EST(0 TO 3) AS INTEGER
END TYPE

'$DYNAMIC
DIM MENUO(0 TO 4) AS STRING, COMENTA$(0 TO 3, 0)
DIM SHARED SEL$(9, 9), COMMENT$(9, 9)
DIM SHARED col(2, 1)
DIM SHARED estat(3)
DIM SHARED ESTFILE AS DF
DIM CFG AS CONFIG
DIM EMPR AS EMPRESA
DIM USR AS USUARIS
DIM CTRL AS CONTROL
DIM MENUS AS CMENU
DIM SPL AS SPOOL
DIM COLORS AS COLO
DIM PASO AS TRANS
DIM ANYS AS ANNO
NO.CREDITS = FALSE

'********************************************************************
'                          INICIALITZAR
'********************************************************************

ON ERROR GOTO ERRMISS
CARREGAT = 0
GOSUB COMPROVA      ' COMPROVA QUE EL PROGRAMA FUNCIONA AMB L'ORDINADOR QUE LI PERTOCA
GOSUB MASCARA       ' TREU LA PRIMERA PANTALLA
GOSUB TEST.OK.BAT   ' COMPROVA EL TIPUS D'ENTRADA
GOSUB OPCIONS       ' GENERAR LES OPCIONS DEL MEN£
IF NO.CREDITS = FALSE THEN GOSUB CREDITS

'********************************************************************
'                          MENU PRINCIPAL
'********************************************************************

OLDIR1$ = CURDIR$
DO
MENU:
      CHDIR UNIDAD$: CHDRIVE (MID$(UNIDAD$, 1, 2))
      OLDIR$ = CURDIR$

      GOSUB MASCARA
      COLOR col(2, 0), col(2, 1)
      LOCATE 2, 1: PRINT STRING$(80, " "); : LOCATE 3, 1: PRINT STRING$(80, "ƒ");
      'LOCATE 23, 70: COLOR col(0, 0), col(0, 1): PRINT "<F1> AJUDA"

      CALL PopMenu(2, 2, 4, OPCIO%, SEL$(), COMMENT$(), col(0, 0), col(0, 1), col(1, 0), col(1, 1), col(), estat())

      IF OPCIO% = 999 THEN
	 CHDRIVE (MID$(OLDIR1$, 1, 2))
	 GOSUB TANCA.TOT
      END IF

      IF OPCIO% = 888 THEN
	 CHDIR MID$(MAIN$, 1, LEN(MAIN$) - 1)
	 SHELL "BKHLP WELCOME"
	 CHDIR ".."
      END IF

      SUBMENU = VAL(MID$(LTRIM$(RTRIM$(STR$(OPCIO%))), 1, 1))
      OPC = VAL(MID$(LTRIM$(RTRIM$(STR$(OPCIO%))), 2, 1)) - 1

      IF CercaEXE(MENUS.PROC(OPC, SUBMENU)) = FALSE THEN
	 CAD$ = MENUS.PROC(OPC, SUBMENU): GOSUB PASSDADES
	 COM$ = CAD$: GOSUB genera.BAT: SYSTEM
	 IF LTRIM$(RTRIM$(CAD$)) = "CONFIG.EXE" THEN GOSUB ACT.COLORS
      ELSE
	 SELECT CASE LTRIM$(RTRIM$(MENUS.PROC(OPC, SUBMENU)))
		CASE IS = "INFORMACIO"
		     GOSUB CREDITS
		     GOSUB EXTRACREDITS
		CASE IS = "UTILS"
		     DO
			MENUO(1) = "Seguiment de moviments ": COMENTA$(0, 0) = "Fer un seguiment dels moviments realitzats per un client"
			MENUO(2) = "Seguiments anuals     >": COMENTA$(1, 0) = "Utilitat per controlar els anys de la facturaci¢"
			MENUO(3) = "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ": COMENTA$(2, 0) = "SEPARADOR"
			MENUO(4) = "Reset de fitxers       ": COMENTA$(3, 0) = "Borra albarans"
			CALL MenuBar(MENUO(), COMENTA$(), 0, CASO%, 7, 31, LEN(MENUO(1)) + 1, 4, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
			SELECT CASE CASO%
			       CASE 1
				    CAD$ = "HIS_CLI.EXE": GOSUB PASSDADES
				    COM$ = CAD$: GOSUB genera.BAT: SYSTEM
			       CASE 2
				    CAD$ = "CLO_ANY.EXE": GOSUB PASSDADES
				    COM$ = CAD$: GOSUB genera.BAT: SYSTEM
			       CASE 4
				    CAD$ = "RESET.EXE": GOSUB PASSDADES
				    COM$ = CAD$: GOSUB genera.BAT: SYSTEM
			       CASE ELSE
			END SELECT
		     LOOP UNTIL CASO% = -19 OR CASO% = -4 OR CASO% = 999
		CASE IS = "CANVI_EMPRES"
		     GOSUB TRIA.USUARI
		CASE IS = "ACABAR"
		     GOSUB SURT
		CASE IS = "MANT_ALBA"
		     DO
			MENUO(1) = "~Manteniment Albarans": COMENTA$(0, 0) = "El manteniment d'albarans"
			MENUO(2) = "~Facturar Albarans  >": COMENTA$(1, 0) = "Facturar els albarans pendents"
			MENUO(3) = "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ": COMENTA$(2, 0) = "SEPARADOR"
			MENUO(4) = "Borrar Albarans     ": COMENTA$(3, 0) = "Borra albarans"
			CALL MenuBar(MENUO(), COMENTA$(), 0, CASO%, 4, 50, 20, 4, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
			SELECT CASE CASO%
			       CASE 1
				    CAD$ = "ALT_ALBA.EXE": GOSUB PASSDADES
				    COM$ = CAD$: GOSUB genera.BAT: SYSTEM
			       CASE 2
				    CAD$ = "FAC_ALBA.EXE": GOSUB PASSDADES
				    COM$ = CAD$: GOSUB genera.BAT: SYSTEM
			       CASE ELSE
			END SELECT
		     LOOP UNTIL CASO% = -19 OR CASO% = -4 OR CASO% = 999
		CASE IS = "MANT_TALLER"
		     DO
			MENUO(1) = "~Manteniment Resguards": COMENTA$(0, 0) = "El manteniment d'entrades al taller"
			MENUO(2) = "~Facturar Resguards   ": COMENTA$(1, 0) = "Facturar sortides del taller"
			MENUO(3) = "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ": COMENTA$(2, 0) = "SEPARADOR"
			MENUO(4) = "~Histïrics Resguards  ": COMENTA$(3, 0) = "Fer un seguiment d'una o mÇs reparacions"
			CALL MenuBar(MENUO(), COMENTA$(), 0, CASO%, 4, 50, 21, 4, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
			SELECT CASE CASO%
			       CASE 1
				    CAD$ = "RES_001.EXE": GOSUB PASSDADES
				    COM$ = CAD$: GOSUB genera.BAT: SYSTEM
			       CASE 2
				    CAD$ = "RES_002.EXE": GOSUB PASSDADES
				    COM$ = CAD$: GOSUB genera.BAT: SYSTEM
			       CASE 4
				    CAD$ = "RES_003.EXE": GOSUB PASSDADES
				    COM$ = CAD$: GOSUB genera.BAT: SYSTEM
			       CASE ELSE
			END SELECT
		     LOOP UNTIL CASO% = -19 OR CASO% = -4 OR CASO% = 999
		CASE IS = "MANT_FACTUR"
		     DO
			MENUO(1) = "~Facturaci¢ Directe     ": COMENTA$(0, 0) = "Crea factures diräctament"
			MENUO(2) = "~Manteniment de Factures": COMENTA$(1, 0) = "Manteniment de factures. Altes, Baixes, Llistats, etc..."
			MENUO(3) = "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ": COMENTA$(2, 0) = "SEPARADOR"
			MENUO(4) = "~Reimpresi¢ de factures ": COMENTA$(3, 0) = "Reimpresi¢ de les factures"
			CALL MenuBar(MENUO(), COMENTA$(), 0, CASO%, 5, 50, 23, 4, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
			SELECT CASE CASO%
			       CASE 1
				    CAD$ = "ALT_FACT.EXE": GOSUB PASSDADES
				    COM$ = CAD$ + " AUTOMATIC": GOSUB genera.BAT: SYSTEM
			       CASE 2
				    CAD$ = "ALT_FACT.EXE": GOSUB PASSDADES
				    COM$ = CAD$ + " MANTENIMENT": GOSUB genera.BAT: SYSTEM
			       CASE ELSE
			END SELECT
		     LOOP UNTIL CASO% = -19 OR CASO% = -4 OR CASO% = 999
		CASE IS = "MANT_COBROS"
		     DO
			MENUO(1) = "~Generar un cobro       ": COMENTA$(0, 0) = "Realitza un cobrament d'un client"
			MENUO(2) = "~Manteniment de Cobros  ": COMENTA$(1, 0) = "Controla el cobro de les factures de clients"
			CALL MenuBar(MENUO(), COMENTA$(), 0, CASO%, 13, 50, 23, 2, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
			SELECT CASE CASO%
			       CASE 1
				    CAD$ = "MAN_COBR.EXE CREAR": GOSUB PASSDADES
				    COM$ = CAD$: GOSUB genera.BAT: SYSTEM
			       CASE 2
				    CAD$ = "MAN_COBR.EXE MANTENIMENT": GOSUB PASSDADES
				    COM$ = CAD$: GOSUB genera.BAT: SYSTEM
			       CASE ELSE
			END SELECT
		     LOOP UNTIL CASO% = -19 OR CASO% = -4 OR CASO% = 999

		CASE IS = ""
		CASE ELSE
		     IF OPCIO% = -1 THEN
			CHDIR MID$(MAIN$, 1, LEN(MAIN$) - 1)
			SHELL "BKHLP WELCOME"
			CHDIR ".."
		     END IF
		END SELECT
      END IF
      
LOOP

'***************************************************************************
'                         DEFINICI¢ DEL MENU PRINCIPAL
'***************************************************************************

OPCIONS:
       GetBackground 1, 1, 24, 80, ME$
       AREAMENU = FREEFILE
       OPEN DIRECCR$ + "MENU_OP.DAT" FOR RANDOM SHARED AS AREAMENU LEN = LEN(MENUS)
       OP = 0: GET AREAMENU, 1, MENUS
       FOR MENU = 0 TO 6
	   TANT% = MENU * 100 / 6
	   FinestraEstat "  Carregant els menus ...   " + LTRIM$(STR$(TANT%)) + " %  ", 0
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
       PutBackground 1, 1, ME$
RETURN

'***************************************************************************
'                         SELECCIONAR USUARI
'***************************************************************************
TRIA.USUARI:
      GetBackground 1, 1, 25, 80, LOGIN$
      IF CARREGAT = 0 THEN TITOL$ = "LOGIN" ELSE TITOL$ = "Canvi d'empresa"
      WIN1 = InitNewWindow(1, 7, 19, 17, 60, 1, TITOL$)
      SetColorWindow 1, col(0, 0), col(0, 1), col(1, 1), col(1, 0), col(2, 0), col(2, 1)
      SetStyleWindow 1, 0, ""
      ShowWindow 1
      
      LOCATE 12, 20: PRINT STRING$(60 - 20, "ƒ");
      
      SetMaxCamps 3
      FOR C = 0 TO 3
	  DeleteCamp C
	  SetColorCamp C, col(1, 0), col(1, 1), col(0, 0), col(0, 1), col(2, 0), col(2, 1)
      NEXT
      SetInitCamp 0, 9, 41, num, 0, "999", "Empresa Nß:"
      SetInitCamp 1, 10, 41, ENCRIP, 15, "XXXXXXXXXX", "Clau:"
      SetInitCamp 2, 14, 41, ASCI, 0, "XXXXXXXXXXX", "Usuari:"
      SetInitCamp 3, 15, 41, ENCRIP, 15, "XXXXXXXXXX", "Clau:"

      InsertValueCamp 0, LTRIM$(STR$(CTRL.EMPRESA))
      InsertValueCamp 2, RTRIM$(LTRIM$(USR.NOM))
      
      CTRL.EMPRESA = VAL(ValueCamp$(0))
      GET 1, VAL(ValueCamp$(0)), CFG     ' RECULL LA CONFIGURACIO DE LA EMPRESA
      GET 2, VAL(ValueCamp$(0)), EMPR    ' RECULL LA CONFIGURACI¢ SECUNDARIA
      GET 4, EMPR.IMPRESORA, SPL

      MAIN$ = LTRIM$(RTRIM$(CFG.DMAIN))
      DBF$ = LTRIM$(RTRIM$(CFG.DDADE))

      SetDirRecursos (LTRIM$(RTRIM$(CFG.DRECU)))
      USUARI = VAL(ValueCamp$(0))
      NOMUS$ = CFG.NOM

      LOCATE 1, 11: PRINT SPACE$(30): LOCATE 1, 11
      COLOR col(1, 0), col(1, 1): PRINT RTRIM$(STR$(USUARI)) + " "; : COLOR col(2, 0), col(2, 1): PRINT RTRIM$(NOMUS$)
      DisplayAllCamps
      '
      ' ESPECIFICAR USUARI I CLAU
      '
      IF CARREGAT = 0 THEN CI = 2: CFI = 3 ELSE CI = 0: CFI = 3
      FOR C = CI TO CFI
	  VALUE = ReadCamp(C)
	  SELECT CASE VALUE
		 CASE 0
		      WRONG = FALSE
		      IF VAL(ValueCamp$(0)) > CFG.MAXREG - 1 OR VAL(ValueCamp$(0)) = 0 THEN
			 ER1$ = "NUMERO D'EMPRESA INCORRECTE"
			 GOSUB MIS
		      END IF
		      CTRL.EMPRESA = VAL(ValueCamp$(0))
		      GET 1, VAL(ValueCamp$(0)), CFG     ' RECULL LA CONFIGURACIO DE LA EMPRESA
		      GET 2, VAL(ValueCamp$(0)), EMPR    ' RECULL LA CONFIGURACI¢ SECUNDARIA
		      GET 4, EMPR.IMPRESORA, SPL
		      PUT 3, 1, CTRL
		      USUARI = VAL(ValueCamp$(0))
		      NOMUS$ = CFG.NOM
		      LOCATE 1, 11: PRINT SPACE$(30): LOCATE 1, 11
		      COLOR col(1, 0), col(1, 1): PRINT RTRIM$(STR$(USUARI)) + " "; : COLOR col(2, 0), col(2, 1): PRINT RTRIM$(NOMUS$)
		 CASE 1
		      PASS$ = ENCRIPT$(EMPR.PASSWORD, 15)
		      CLAU$ = RTRIM$(ValueCamp$(1))
		      PASS$ = RTRIM$(PASS$)

		      FOR L = 1 TO LEN(PASS$)
			  IF MID$(PASS$, L, 1) = "/" THEN MID$(PASS$, L, 1) = CHR$(32)
			  IF MID$(CLAU$, L, 1) = "/" THEN MID$(CLAU$, L, 1) = CHR$(32)
		      NEXT
 
		      CL$ = UCASE$(LTRIM$(RTRIM$(CLAU$)))
		      PA$ = UCASE$(LTRIM$(RTRIM$(PASS$)))

		      EM = VAL(ValueCamp$(0))
		      IF CL$ <> PA$ THEN
			 ER2$ = "CLAU D'ACCESS INCORRECTE"
			 GOSUB MIS
		      END IF

		 CASE 2
		      MAXUS = LOF(5) \ LEN(USR): RT = 0
		      FOR R = 1 TO MAXUS
			  GET 5, R, USR
			  IF UCASE$(LTRIM$(RTRIM$(USR.NOM))) = UCASE$(LTRIM$(RTRIM$(ValueCamp$(2)))) THEN
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
		      CLAU$ = UCASE$(RTRIM$(ValueCamp$(3)))

		      FOR L = 1 TO LEN(PASS$)
			  IF MID$(PASS$, L, 1) = "/" THEN MID$(PASS$, L, 1) = CHR$(32)
			  IF MID$(CLAU$, L, 1) = "/" THEN MID$(CLAU$, L, 1) = CHR$(32)
		      NEXT

		      CL$ = LTRIM$(RTRIM$(CLAU$))
		      PA$ = LTRIM$(RTRIM$(PASS$))
		      
		      IF CL$ <> PA$ THEN
			 InsertValueCamp 3, "€€€€€€€€€€": DisplayAllCamps
			 ER2$ = "CLAU D'ACCESS INCORRECTE"
			 GOSUB MIS
		      END IF
		      InsertValueCamp 3, "€€€€€€€€€€": DisplayAllCamps
		      EM = VAL(ValueCamp$(0))
		 CASE F1
		      CHDIR MID$(MAIN$, 1, LEN(MAIN$) - 1)
		      SHELL "BKHLP ENTRADA"
		      CHDIR ".."
		      C = C - 1
		 CASE F2 TO F10
		      C = C - 1: SOUND 50, .5
		 CASE 999
		      IF CARREGAT = 0 THEN
			 COLOR 7, 0: CLS : RESET
			 SYSTEM 2                ' Surt definitivament
		      ELSE
			 DeleteWindow 0
			 GOSUB MASCARA
			 RETURN
		      END IF
		 CASE ELSE
	  END SELECT
      NEXT

DIRECCTE:
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
      
      USUARI = VAL(ValueCamp$(0))
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
      LOCATE 12, 24: PRINT "Carregant els missatges d'errors ..."
      SetDirRecursos (DIRECCR$)

      AF$ = ActivarFelicitacion$
      SELECT CASE MID$(AF$, 1, 1)
	     CASE IS = "1"
		  AREAFE = FREEFILE: OPEN DIRECCF$ + "PLANTILL\FESTES.DAT" FOR OUTPUT AS AREAFE
		  PRINT #AREAFE, MID$(AF$, 2, LEN(AF$))
		  felic$ = MID$(AF$, 2, LEN(AF$))
		  CLOSE #AREAFE
	     CASE IS = "2"
		  AREAFE = FREEFILE: OPEN DIRECCF$ + "PLANTILL\FESTES.DAT" FOR OUTPUT AS AREAFE
		  PRINT #AREAFE, MID$(AF$, 2, LEN(AF$))
		  CLOSE #AREAFE
		  felic$ = MID$(AF$, 2, LEN(AF$))
	     CASE ELSE
		  IF DIR$(DIRECCF$ + "PLANTILL\FESTES.DAT") <> "" THEN KILL DIRECCF$ + "PLANTILL\FESTES.DAT"
		  felic$ = ""
      END SELECT

      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, ANYE, ANYS
      ANY$ = ANYS.ANY
      CLOSE AREAA
      LOCATE 12, 24: PRINT "Carregant la configuraci¢ de colors "
      
      GOSUB ACT.COLORS
      CARREGAT = 999
      GOSUB MASCARA
      RETURN

ACT.COLORS:
      AREAC = FREEFILE: OPEN DBF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
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
      FONS (177)
      FINESTRA 21, 1, 25, 80, 1, CAIXA1: LOCATE 1, 1: PRINT SPACE$(80)
      NOMEM$ = CFG.NOM
      NOMUS$ = USR.NOM
      IF felic$ <> "" THEN
	 COLOR col(1, 0), col(1, 1)
	 FINESTRA 10, 20, 13, 60, 1, CAIXA2: COLOR col(1, 0), col(1, 1)
	 CENTRAR 11, "  Smart Software vos desitga:  "
	 COLOR col(2, 0), col(1, 1): CENTRAR 12, felic$
      END IF
      COLOR col(2, 0), col(2, 1): LOCATE 1, 64: PRINT PROGRAMA + " " + versio
      COLOR col(2, 0), col(2, 1): LOCATE 1, 2: PRINT "EMPRESA: ";
      COLOR col(0, 0), col(0, 1): PRINT RTRIM$(STR$(EM)) + " ";
      COLOR col(2, 0), col(2, 1): PRINT RTRIM$(NOMEM$);
      COLOR col(2, 0), col(2, 1): PRINT " - Any:";
      COLOR col(0, 0), col(0, 1): PRINT RTRIM$(ANY$)
      
      COLOR col(2, 0), col(2, 1): LOCATE 22, 26: PRINT "      USUARI: ";
      COLOR col(0, 0), col(0, 1): PRINT RTRIM$(STR$(US)) + " ";
      COLOR col(2, 0), col(2, 1): PRINT RTRIM$(NOMUS$)
      'LOCATE 23, 70: COLOR col(0, 0): PRINT "<F1> AJUDA"
      GOSUB ShowBar
      RETURN

OBRIRF:
	 COLOR 15, 9
	 FINESTRA 10, 10, 14, 70, 1, CAIXA1
	 LOCATE 12, 24: PRINT "Carregant els fitxers de configuraci¢ ..."
	 OPEN "CONTROL.CFG" FOR RANDOM SHARED AS 3 LEN = LEN(CTRL)
	 OPEN "SPOOL.CFG" FOR RANDOM SHARED AS 4 LEN = LEN(SPL)
	 OPEN "EMPRESA.FAC" FOR RANDOM SHARED AS 2 LEN = LEN(EMPR)
	 OPEN "USUARIS.FAC" FOR RANDOM SHARED AS 5 LEN = LEN(USR)
	 GET 3, 1, CTRL
	 GET 5, CTRL.USUARI, USR

	 SetDirRecursos (LTRIM$(RTRIM$(CFG.DRECU)))
	 DBF$ = LTRIM$(RTRIM$(CFG.DDADE))
	 GET 1, CTRL.EMPRESA, CFG     ' RECULL LA CONFIGURACIO DE LA EMPRESA
	 GOSUB TESTCOLORS
	 GOSUB MASCARA
	 RETURN
'***************************************************************************
'                COMPROVA SI EL PROGRAMA ESTA BEN INSTAL.LAT
'***************************************************************************

COMPROVA:
      
      LOCATE 25, 1: PRINT "*** COMPROVANT LA INSTAL.LACI¢ DEL PROGRAMA";
      GOSUB TESTDRVS

      IF DIR$("CONFIG.FAC") = "" THEN
	 BEEP
	 DO
	   VALUE = Avis("ERROR:", "Falta el fitxer de configuraci¢", "Pitji <ENTER>", 0)
	 LOOP UNTIL VALUE = 1
	 SYSTEM 2
      END IF
      OPEN "CONFIG.FAC" FOR RANDOM SHARED AS 1 LEN = LEN(CFG)
      GET 1, 1, CFG
      IF CFG.INSTALAT = "INSTAL.LAT" THEN
	 IF CFG.VAR <> TESTSCREEN THEN
	    LOCATE 25, 1: PRINT "ERROR GREU: La configuraci¢ del pass A no Çs correcta";
	    CFG.INSTALAT = "TEXT!!!!!!"
	    PUT 1, 1, CFG
	    SYSTEM 2
	 END IF

	 CAD$ = ENCRIPT$(CFG.INST, 45)          ' DESENCRIPTAR EL COPYRIGHT

	 IF CAD$ <> MID$(VERSVGA$, 1, 29) THEN  ' COMPROVAR SI ES CORRECTE
	    LOCATE 25, 1: PRINT "ERROR GREU: La configuraci¢ del pass B no Çs correcta"
	    CFG.INSTALAT = "WRONG!!!!!"
	    PUT 1, 1, CFG
	    SYSTEM 2
	 END IF
      ELSE
	 IF RTRIM$(CFG.INSTALAT) = "WRONG!!!!!" THEN
	    BEEP
	    DO
	       VALUE = Avis("Avis:", "El programa ja no funciona correctament. Consulti BKHLP", "Pitji <ENTER>", 0)
	    LOOP UNTIL VALUE = 1
	    SYSTEM 2
	 END IF
	 IF RTRIM$(CFG.INSTALAT) <> "PRGTOM0710" THEN
	    CFG.INSTALAT = "          "
	    PUT 1, 1, CFG
	    SYSTEM 2
	 END IF
      END IF

      LOCATE 25, 1: PRINT "*** COMPROVANT EL NUMERO DE SäRIE                ";
      IF DIR$("SERIAL.NUM") = "" THEN
	 BEEP
	 DO
	   VALUE = Avis("ERROR:", "Falta un dels fitxers de seguretat", "Pitji <ENTER>", 1)
	 LOOP UNTIL VALUE = 1
	 SYSTEM 2
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


SURT:  DIM OP$(2)
       OP$(1) = "          Aceptar": OP$(2) = "         Cancel.lar"
       Avis.Sonor (3)
       OPC = Achoice(10, 20, 15, 52, 2, OP$(), col(), " Desitja sortir del programa?  ", 1, "")
       IF OPC = 1 THEN GOSUB TANCA.TOT
       ERASE OP$
       COLOR col(0, 0), col(0, 1)
       RETURN

CREDITS:
       ShowWindow 0
       COLOR col(2, 0), col(2, 1)
       CENTRAR 6, PROGRAMA + " " + versio
       COLOR col(0, 0), col(0, 1)
       CENTRAR 7, "Programa per gestionar facturaci¢ i control de taller"
       CENTRAR 9, COPYRIGHT
       CENTRAR 10, AUTOR: COLOR col(2, 0), col(2, 1)
       CENTRAR 11, "Tots els drets reservats": COLOR col(2, 0), col(2, 1)
       LOCATE 13, 11: PRINT "         N£mero de särie: "; : COLOR col(0, 0), col(0, 1): PRINT NS$
       LOCATE 14, 11:  COLOR col(2, 0), col(2, 1): PRINT "     Empresa autoritzada: "; : COLOR col(0, 0), col(0, 1): PRINT RTRIM$(NOMEM$)
       COLOR col(2, 0), col(2, 1)

       IF felic$ <> "" THEN COLOR 26: CENTRAR 16, felic$
       COLOR col(2, 0), col(2, 1)
       LOCATE 19, 11: PRINT "              Pitja una tecla per a continuar ..."
       C$ = INPUT$(1)
       DeleteWindow 0
       RETURN

EXTRACREDITS:
       ShowWindow 0
       LOCATE 8, 11:  COLOR col(2, 0), col(2, 1): PRINT " Direcctori de l'empresa: "; : COLOR col(0, 0), col(0, 1): PRINT DIRECCF$ + ANY$ + "\"
       LOCATE 9, 11:  COLOR col(2, 0), col(2, 1): PRINT "               Impresora: "; : COLOR col(0, 0), col(0, 1): PRINT SPL.DESCRIPCION
       LOCATE 10, 11, 0: COLOR col(2, 0), col(2, 1): PRINT "    Memïria convencional:"; : COLOR col(0, 0), col(0, 1)
       PRINT USING "##,###,### Octets disponibles"; FRE(-1)
       LOCATE 11, 11, 0: COLOR col(2, 0), col(2, 1): PRINT "       Any de facturaci¢: " + ANY$
       COLOR col(2, 0), col(2, 1): LOCATE 19, 11: PRINT "              Pitja una tecla per a continuar ..."
       C$ = INPUT$(1)
       DeleteWindow 0
       RETURN


MIS:   COLOR col(0, 0), col(0, 1): Avis.Sonor (1)

       VALUE = Avis("ERROR:", ER1$ + ER2$, "Pitja una tecla...", 0)
       IF CARREGAT = 0 THEN
	  GOSUB CIERRA
	  RESET: Clr: SYSTEM 2
       ELSE
	  RETURN MENU
       END IF

       RETURN

TESTCOLORS:
	 LOCATE 24, 1
	 IF TESTSCREEN = &HB000 THEN
	    col(0, 0) = 7: col(0, 1) = 0
	    col(1, 0) = 0: col(1, 1) = 7
	    col(2, 0) = 15: col(2, 1) = 0
	 ELSE
	    GOSUB ACT.COLORS
	 END IF
	 RETURN


ERRMISS:   Avis.Sonor (1)            ' Mostra per pantalla l'error generat
	   IF ShowError = 1 THEN
	      RESUME
	   ELSE
	      RESUME NEXT
	   END IF
	   
TANCA.TOT:
	  COLOR 7, 0: CLS
	  COLOR col(0, 0), col(0, 1)
	  FINESTRA 1, 1, 4, 79, 1, CAIXA1
	  COLOR col(2, 0), col(2, 1)
	  CENTRAR 2, "*** FI DE LA SESI¢ AMB " + PROGRAMA + " " + versio + " ***"
	  COLOR col(1, 0), col(2, 1)
	  LOCATE 3, 2: PRINT STRING$(77, " ");
	  CENTRAR 3, COPYRIGHT
	  PRINT : PRINT : PRINT
	  GOSUB CIERRA
	  SYSTEM 2

DEFWINDOWS:
       IF SetInitWindows(2) = TRUE THEN
	  tecla = Avis("ERROR 000W:", "Error al inicialitzar les finestres", "Pitja una tecla...", 0)
	  RETURN
       END IF

       IF SetMaxBotons(1) = TRUE THEN
	  tecla = Avis("ERROR 000B:", "Error al inicialitzar els botons", "Pitja una tecla...", 0)
	  RETURN
       END IF
       
       WIN2 = InitNewWindow(0, 5, 10, 20, 70, 1, "Credits")
       SetAllColors col(0, 0), col(0, 1), col(1, 1), col(1, 0), col(2, 0), col(2, 1)
       SetStyleWindow 0, 0, ""

       BOT1 = SetInitBoto(17, 30, 19, 40, "Aceptar", 0)
       SetBotoColor col(0, 0), col(0, 1), col(1, 1), col(1, 0), 0
       RETURN

PASSDADES:
	  TMP$ = ENVIRON$("TEMPORAL")
	  IF TMP$ = "" THEN
	     tecla = Avis("Error DOS:", "Falta la variable d'entorn TEMPORAL=C:\TEMP\", "Pitji una tecla...", 0)
	     RETURN MENU
	  END IF

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

genera.BAT:
	 AREABAT = FREEFILE
	 OPEN "CRIDADA.BAT" FOR OUTPUT AS AREABAT
	 PRINT #AREABAT, "ECHO OFF"
	 PRINT #AREABAT, "CD " + MID$(MAIN$, 1, LEN(MAIN$) - 1)
	 PRINT #AREABAT, "" + COM$
	 PRINT #AREABAT, "CD " + OLDIR$
	 PRINT #AREABAT, "ECHO NO_USUARI >PASS.DAT"
	 CLOSE #AREABAT
	 TMP$ = ENVIRON$("TEMPORAL")
	 AREAT = FREEFILE: OPEN TMP$ + "MENU.TMP" FOR RANDOM AS AREAT LEN = LEN(ESTFILE)
	 FOR L = 0 TO 3: ESTFILE.EST(L) = estat(L): NEXT
	 PUT AREAT, 1, ESTFILE
	 CLOSE AREAT
	 RETURN

TEST.OK.BAT:
	 IF DIR$("PASS.DAT") = "" THEN
	    NO.CREDITS = FALSE
	    GOSUB OBRIRF
	    GOSUB DEFWINDOWS
	    GOSUB TRIA.USUARI   ' SELECCIONAR L'USUARI
	 ELSE
	    NO.CREDITS = TRUE
	    KILL "PASS.DAT"
	    OPEN "CONTROL.CFG" FOR RANDOM SHARED AS 3 LEN = LEN(CTRL)
	    OPEN "SPOOL.CFG" FOR RANDOM SHARED AS 4 LEN = LEN(SPL)
	    OPEN "EMPRESA.FAC" FOR RANDOM SHARED AS 2 LEN = LEN(EMPR)
	    OPEN "USUARIS.FAC" FOR RANDOM SHARED AS 5 LEN = LEN(USR)

	    GET 3, 1, CTRL
	    GET 5, CTRL.USUARI, USR

	    GET 1, CTRL.EMPRESA, CFG     ' RECULL LA CONFIGURACIO DE LA EMPRESA
	    GET 2, CTRL.EMPRESA, EMPR    ' RECULL LA CONFIGURACI¢ SECUNDARIA
	    GET 4, EMPR.IMPRESORA, SPL

	    EM = CTRL.EMPRESA

	    GOSUB DIRECCTE
	    GOSUB ACT.COLORS
	    GOSUB DEFWINDOWS

	    TMP$ = ENVIRON$("TEMPORAL")
	    AREAT = FREEFILE: OPEN TMP$ + "MENU.TMP" FOR RANDOM AS AREAT LEN = LEN(ESTFILE)
	    GET AREAT, 1, ESTFILE
	    FOR L = 0 TO 3: estat(L) = ESTFILE.EST(L): NEXT
	    CLOSE AREAT
	 END IF
	 RETURN
    
TESTDRVS:
	 LOCATE 22, 1: COLOR 7, 0: PRINT "*** COMPROVANT COMPARTICI¢ DE FITXERS ";
	 IF MID$(HEX$(DOSMultiplexorTest(&H1000)), 3, 2) = "00" THEN
	    PRINT "ERROR. AX= 0x" + HEX$(REGS.ax);
	    'tecla = Avis("ERROR:", "El SHARE no estÖ carregat, es recomana que s'activi", "Pitji una tecla per continuar", 0)
	 ELSE
	    PRINT "Ok. AX= 0x" + HEX$(REGS.ax);
	 END IF
	
	 LOCATE 23, 1: COLOR 7, 0: PRINT "*** COMPROVANT DRIVER HIMEM ";
	 IF MID$(HEX$(DOSMultiplexorTest(&H4300)), 3, 2) = "00" THEN
	    PRINT "ERROR. AX= 0x" + HEX$(REGS.ax);
	    tecla = Avis("AVIS:", "El HIMEM no estÖ carregat, es recomana que s'activi", "Pitji una tecla per continuar", 0)
	 ELSE
	    PRINT "Ok. AX= 0x" + HEX$(REGS.ax);
	 END IF

	 LOCATE 24, 1: COLOR 7, 0: PRINT "*** COMPROVANT EL CLIENT ACTUAL DE NOVELL ";
	 IF MID$(HEX$(DOSMultiplexorTest(&H7A00)), 3, 2) = "00" THEN
	    PRINT "ERROR. AX= 0x" + HEX$(REGS.ax);
	    'tecla = Avis("AVIS:", "El Novell Client per a DOS no estÖ instal.lat", "Pitji una tecla per continuar", 0)
	 ELSE
	    PRINT "Ok. AX= 0x" + HEX$(REGS.ax);
	 END IF
	 RETURN

ShowBar:
      LOCATE 25, 1: COLOR 0, 7: PRINT STRING$(80, " ");
      LOCATE 25, 1: PRINT " F1 Ajuda   F2 Magatzem   F3 Clients   F4 Vendes   F5 Rebuts   F6 Compres";
      COLOR col(0, 0), col(0, 1)
      RETURN

REM $STATIC
FUNCTION ActivarFelicitacion$
	 fetxa$ = FormatD$(Now#, "dd/mm/yyyy")
	 dia$ = MID$(fetxa$, 1, 2)
	 meso$ = MID$(fetxa$, 4, 2)
	 ANYS$ = MID$(fetxa$, 7, 4)
	 AN$ = LTRIM$(ANYS$)

	 IF VAL(dia$) >= 20 AND VAL(meso$) = 12 THEN
	    ActivarFelicitacion$ = "1*** Bon Nadal ***"
	 ELSE
	    IF ((VAL(dia$) >= 1 AND NOT VAL(dia$) > 7) AND VAL(meso$) = 1) THEN
	       ActivarFelicitacion$ = "2*** Bon Nadal i feliá any " + AN$ + " ***"
	    ELSE
	       ActivarFelicitacion$ = "NULL"
	    END IF
	 END IF
END FUNCTION

REM $DYNAMIC
FUNCTION CercaEXE (NOM$)
    NOM$ = LTRIM$(RTRIM$(NOM$))
    CercaEXE = TRUE
    IF INSTR(1, NOM$, ".EXE") THEN CercaEXE = FALSE
END FUNCTION

REM $STATIC
FUNCTION DOSMultiplexorTest (DRV)
	 REGS.ax = DRV
	 InterruptX &H2F, REGS, REGS
	 DOSMultiplexorTest = REGS.ax
END FUNCTION

