' ********************************************************************
'
' Fitxer....................: CONFIG.BAS
' Titol.....................: Modul per la configuraci¢ dels valors
'                             de l'empresa.
' ********************************************************************
'
' Data inici................: 17/12/1996 18:30:00
' Data de la darrera revisi¢: 22/03/1999 21:10:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/99 (C)
'
' ********************************************************************
' Notes: Totes les plantilles dels documents estiran en el direcctori
'        C:\FACT3\FACTUXXX\PLANTILL.
' ********************************************************************

DECLARE FUNCTION ComprovaFitxersPass! (modul$)
DECLARE SUB inicialitzadirs ()
DECLARE SUB inicialitzavars ()
DECLARE SUB MissatgeError (E!, M$)
DECLARE SUB MASCCFG (C%)
DECLARE FUNCTION FORASPC$ (CAD$)
DECLARE SUB ProcConfig (DIRECCP$, DIRECC$, DIRECI$, DEV$, IMPRESORA!)
DECLARE SUB inicialitzacolors ()
DECLARE SUB AssignaAnyActual ()

COMMON SHARED DIRECCR$
COMMON SHARED DIRECC$, DIRCP$, DIRECCP$, DIRECCF$, DIRECCI$, DEV$, IMPRESORA
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT
COMMON SHARED MAXLINS, MAXFAC, IVA, DTO, R

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'D:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'D:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'D:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'D:\FACT3\FONTS\RESGUARD.BI'
'$INCLUDE: 'D:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'D:\FACT3\FONTS\WIN.BI'
'$INCLUDE: 'D:\FACT3\FONTS\CONSTS.BI'

'$DYNAMIC
DIM SHARED EMPRES AS EMPRESA
DIM SHARED CEMPRE AS CONTROLEMP
DIM SHARED CFG AS CONFIG              '     "      DEL FITXER DE CONFIGURACI¢
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAPÄALERAS DE DOCUMENTS
DIM SHARED PP AS PIEPAGINA
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED DOCNUM AS DN
DIM SHARED CTRL AS CONTROL
DIM SHARED COLORS AS COLO
DIM SHARED COL(2, 1)
DIM SHARED RESGUA AS RESG
DIM SHARED USR AS USUARIS
DIM SHARED MENUU(9) AS STRING, COMMENTU$(8, 1)
DIM SHARED PASO AS TRANS
DIM SHARED ANYS AS ANNO

      ON ERROR GOTO ERRORS

      IF NOT ComprovaFitxersPass("CONFIG.EXE") THEN
	 BEEP: PRINT "ERROR: Al llegir els fitxers de transpass"
	 SYSTEM
      END IF

      inicialitzadirs
      inicialitzavars
      inicialitzacolors
      AssignaAnyActual
      SetDirRecursos (DIRECCR$)
      
      CALL ProcConfig(DIRECCP$, DIRECCF$, DIRECCI$, DEV$, IMPRESORA)

ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

REM $STATIC
SUB AssignaAnyActual
    SHARED DIRECCF$, R

    AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
    GET AREAA, R, ANYS

    DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"

    CLOSE AREAA
END SUB

FUNCTION ComprovaFitxersPass (modul$)

	  ComprovaFitxersPass = TRUE

	  TMP$ = ENVIRON$("TEMPORAL")
	  IF DIR$(TMP$ + "PASS.TMP") = "" THEN
	     ComprovaFitxersPass = FALSE
	  ELSE
	     DEF SEG = VARSEG(CFG)
	     BLOAD TMP$ + "PASS.TMP", VARPTR(CFG)
	     DEF SEG
	  END IF

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

	  ' Comprova que el modul que s'ha sol.licitat Çs el correcte

	  IF LTRIM$(RTRIM$(CA2$)) <> modul$ THEN
	     comprovafitxerpass = FALSE
	     TELCA = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "PITJA UNA TECLA...", 0)
	     END
	  END IF

	  KILL TMP$ + "PASS.TMP"    ' Borra els fitxers de transpass
	  KILL TMP$ + "PASU.TMP"
	  KILL TMP$ + "PASE.TMP"
	  KILL TMP$ + "PROT.TMP"
END FUNCTION

FUNCTION FORASPC$ (CAD$)
	 FOR L = 1 TO LEN(CAD$)
	     IF MID$(CAD$, L, 1) = CHR$(32) THEN MID$(CAD$, L, 1) = CHR$(0)
	 NEXT
	 FORASPC$ = CAD$
END FUNCTION

SUB inicialitzacolors
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
END SUB

SUB inicialitzadirs
    UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
    DBF$ = LTRIM$(RTRIM$(CFG.DDADE))     ' Assignar direcctoris
    SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
    MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
    HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
    SYS$ = LTRIM$(RTRIM$(CFG.DSIST))

    DIRECCF$ = DBF$            ' Subdirecctori de les base de dades
    DIRECCP$ = SCR$ + "\"      ' Subdirecctori de les pantalles
    DIRECCR$ = MSK$ + "\"      ' Subdirecctori de mascares, errors, etc...
    DIRECCHE$ = HLP$ + "\"     ' Subdirecctori de ajuda
    DIRECCI$ = UNIDAD$ + "\IMPRESOR\"  ' Subdirecctori de les impresores
    DIRECCT$ = DBF$ + "TEXTOS\"
    DIRECCH$ = DBF$ + "HISTORIC\"
END SUB

SUB inicialitzavars
    MAXLINS = USR.MAXLINS
    MAXFAC = USR.LINALBA
    IVA = EMPRES.IVA
    DTO = EMPRES.DTO
    R = EMPRES.ANY
    DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
    IMPRESORA = USR.IMPRESORA
END SUB

SUB MASCCFG (C%)
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    SELECT CASE C%
	   CASE 1
		LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR EMPRESA    <F2>=CREAR EMPRESA NOVA"
		LOCATE 23, 2: PRINT "<ESC>=SORTIR                 <F3>=SUMAR EMPRESES    "
	   CASE 2
		LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR IMPRESORA"
		LOCATE 23, 2: PRINT "<ESC>=SORTIR"
	   CASE 3
		LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR CAPÄALERA "
		LOCATE 23, 2: PRINT "<ESC>=SORTIR"
	   CASE ELSE
    END SELECT
END SUB

SUB ProcConfig (DIRECCP$, DIRECC$, DIRECI$, DEV$, IMPRESORA) STATIC

    DIM MENU(9) AS STRING, COMMENT$(8, 1)
    GetBackground 1, 1, 25, 80, factsu$
    GOSUB OBRIRFITXERS
    GOSUB DEFWINDOWS
   DO
      MENU(1) = " Control d'~empreses      > ": COMMENT$(0, 0) = " Configurar valors inicials de l'empresa "
      MENU(2) = " Configuraci¢ d'~usuaris  > ": COMMENT$(1, 0) = " Configurar valors del usuari "
      MENU(3) = STRING$(27, "ƒ"): COMMENT$(2, 0) = ""
      MENU(4) = " ~Impresores                ": COMMENT$(3, 0) = " Configurar tipus d'impresora "
      MENU(5) = STRING$(27, "ƒ"): COMMENT$(4, 0) = ""
      MENU(6) = " Text ~capáalera            ": COMMENT$(5, 0) = " Configurar la capáalera dels documents "
      MENU(7) = " Text ~peu de pÖgina        ": COMMENT$(6, 0) = " Configurar els peus de pÖgina dels documents "
      MENU(8) = STRING$(27, "ƒ"): COMMENT$(7, 0) = ""
      MENU(9) = " Configuraci¢ dels c~olors  ": COMMENT$(8, 0) = " Configura colors "
      DO
	  CALL MenuBar(MENU(), COMMENT$(), 0, CASO%, 9, 31, LEN(MENU$(1)) + 1, 9, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
      LOOP UNTIL CASO% <> 888

		  GetBackground 1, 1, 25, 80, MENUS$
		  SELECT CASE CASO%
			 CASE 1
			    
			    DO
			      MENU(1) = " CREAR EMPRESA NOVA      ": COMMENT$(0, 0) = ""
			      MENU(2) = " EDITAR EMPRESA ACTUAL   ": COMMENT$(1, 0) = ""
			      MENU(3) = STRING$(24, "ƒ"): COMMENT$(2, 0) = ""
			      MENU(4) = " CONFIGURACI¢ DIRECTORIS ": COMMENT$(3, 0) = ""
			      CALL MenuBar(MENU(), COMMENT$(), 0, CO%, 10, 50, LEN(MENU$(1)), 4, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
			      SELECT CASE CO%
				     CASE 1
					  GOSUB INSERTAR
				     CASE 2
					  GOSUB EDITAR
				     CASE 4
					  GOSUB CONF.DIRECC
				     CASE ELSE
			      END SELECT
			    LOOP UNTIL CO% = 999 OR CO% = -4 OR CO% = -19
			    PutBackground 1, 1, MENUS$
			 CASE 2
			      R = EMP: GOSUB MENU.USUAR
			 CASE 4
			      R = EMP: GOSUB IMPRESORA
			 CASE 6
			      R = EMP: GOSUB CAPSALERA
			 CASE 7
			      R = EMP: GOSUB PIEPAG
			 CASE 9
			      R = EMP: GOSUB CONF.COLORS
			 CASE 999, -4, -19
			      PackWindows
			      RESET
			      SYSTEM
		  END SELECT
    LOOP

    

'************************************************************************
' CONFIGURACI¢ PARTICULAR DE CADA USUARI
'************************************************************************

MENU.USUAR:

   DO
      MENUU(1) = "˛DOCUMENTS         ": COMMENTU$(0, 0) = " Configurar parÖmetres dels documents "
      MENUU(2) = "˛PASSWORDS         ": COMMENTU$(1, 0) = " Configurar el password del usuari "
      MENUU(3) = STRING$(19, "ƒ"): COMMENTU$(2, 0) = ""
      MENUU(4) = "˛TIPUS D'IMPRESORA ": COMMENTU$(3, 0) = " Configurar tipus d'impresora "
      MENUU(5) = STRING$(19, "ƒ"): COMMENTU$(4, 0) = ""
      MENUU(6) = "˛CREAR USUARIS     ": COMMENTU$(5, 0) = " Afegeix usuaris al programa"

      CALL MenuBar(MENUU(), COMMENTU$(), 0, CASO%, 9, 59, LEN(MENUU$(1)), 6, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))

      SELECT CASE CASO%
	     CASE 1
		  GOSUB DOCUM
	     CASE 2
		  GOSUB PASS
	     CASE 4
		  GOSUB IMPRE
	     CASE ELSE
		  PutBackground 1, 1, MENUS$
		  RETURN
      END SELECT
   LOOP

'***********************************************************************
' Configuraci¢ de la clau d'access
'***********************************************************************

PASS:
      ShowWindow 1
      SetMaxCamps 2
      SetInitCamp 0, 12, 29, 4, 15, "XXXXXXXXXX", "Clau actual:"
      SetInitCamp 1, 14, 29, 4, 15, "XXXXXXXXXX", "Clau nova:"
      SetInitCamp 2, 15, 29, 4, 15, "XXXXXXXXXX", "Clau modificada:"

      FOR C = 0 TO 2
	  DeleteCamp C
	  SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
      NEXT
      FOR C = 0 TO 2: InsertValueCamp 0, "//////////": NEXT
      DisplayAllCamps

      FOR C = 0 TO 2
	  IF C = 0 THEN SetMaxCamps 0
	  VALUE = ReadCamp(C)
	  
	  SELECT CASE VALUE
		 CASE 0
		      GET AREA4, 1, CTRL
		      GET AREA5, CTRL.USUARI, USR
		      
		      PASSW$ = ENCRIPT$(USR.PASSWORD, 15)

		      FOR L = 1 TO LEN(PASSW$)
			  IF MID$(PASSW$, L, 1) = "/" THEN MID$(PASSW$, L, 1) = CHR$(32)
		      NEXT

		      IF PASSW$ <> ValueCamp$(0) THEN
			 InsertValueCamp 0, "€€€€€€€€€€"
			 DisplayAllCamps
			 DeleteCamp 0
			 Avis.Sonor (1)
			 tecla = Avis("ERROR:", "CLAU D'ACCESS INCORRECTE", "Pitja una tecla...", 0)
			 C = C - 1
		      END IF
		      SetMaxCamps 2
		      InsertValueCamp 0, "€€€€€€€€€€"
		      DisplayAllCamps
		 CASE 1
		      PASSWN1$ = ENCRIPT$(ValueCamp$(1), 15)
		      FOR L = 1 TO LEN(PASSWN1$)
			  IF MID$(PASSWN1$, L, 1) = "/" THEN MID$(PASSWN1$, L, 1) = CHR$(32)
		      NEXT
		      SetMaxCamps 2
		      InsertValueCamp 1, "€€€€€€€€€€"
		      DisplayAllCamps
		 CASE 2
		      PASSWN2$ = ENCRIPT$(ValueCamp$(2), 15)
		      FOR L = 1 TO LEN(PASSWN2$)
			  IF MID$(PASSWN2$, L, 1) = "/" THEN MID$(PASSWN2$, L, 1) = CHR$(32)
		      NEXT
		      InsertValueCamp 2, "€€€€€€€€€€"
		      DisplayAllCamps
		      IF PASSWN1$ <> PASSWN2$ THEN
			 Avis.Sonor (1)
			 tecla = Avis("ERROR:", "LES DUES CLAUS NO ES CORRESPONEN", "Pitja una tecla...", 0)
			 DeleteWindow 1
			 RETURN
		      END IF
		      GET AREA5, CTRL.USUARI, USR
		      USR.PASSWORD = PASSWN2$
		      PUT AREA5, CTRL.USUARI, USR
		 CASE SALIR
			 DeleteWindow 1
			 RETURN
		 CASE ELSE
	  END SELECT
      NEXT
      DeleteWindow 1
      RETURN

'***********************************************************************
' Configuraci¢ dels documents
'***********************************************************************

DOCUM:

   DO
      MENUU(1) = "˛TIPUS DE FACTURACI¢": COMMENTU$(0, 0) = " Configurar els distints tipus de facturaci¢ "
      MENUU(2) = "˛DOCUMENTS          ": COMMENTU$(1, 0) = " Configurar parÖmetres dels documents "
      MENUU(3) = "˛VENDES             ": COMMENTU$(2, 0) = " Configurar parÖmetres de les vendes "

      CALL MenuBar(MENUU(), COMMENTU$(), 0, CASO%, 11, 57, LEN(MENUU$(1)), 3, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
      SELECT CASE CASO%
	     CASE 1
		  GOSUB DOCUMD
	     CASE 2
		  GOSUB PDOC
	     CASE 3
		  GOSUB PVEN
	     CASE ELSE
		  RETURN
      END SELECT
   LOOP

'***********************************************************************
' Configuraci¢ dels documents en general
'***********************************************************************

PDOC:
      ShowWindow 2
      
      SetMaxCamps 8
      SetInitCamp 0, 11, 38, ASCI, 0, "999", "Nß Linies dels albarans:"
      SetInitCamp 1, 12, 38, ASCI, 0, "999", "Nß Linies de les factures:"
      SetInitCamp 2, 14, 38, ASCI, 0, "999", "Nß Linies dels llistats:"
      SetInitCamp 3, 16, 38, ASCI, 0, "99999", "Nß Prïxim de resguard:"
      SetInitCamp 4, 17, 38, ASCI, 0, "99999", "Nß Prïxim de client:"
      SetInitCamp 5, 18, 38, ASCI, 0, "99999", "Nß Prïxim de proveidor:"
      SetInitCamp 6, 11, 63, ASCI, 0, "XXXXXXXXXX", "Codi de client:"
      SetInitCamp 7, 12, 63, ASCI, 0, "XXXXXXXXXX", "Codi de resguard:"
      SetInitCamp 8, 13, 63, ASCI, 0, "XXXXXXXXXX", "Codi de proveidor:"

      FOR C = 0 TO 8
	  SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
      NEXT
      
      GET AREA4, 1, CTRL
      GET AREA5, CTRL.USUARI, USR
      GET AREANUM, 1, DOCNUM

      InsertValueCamp 0, LTRIM$(STR$(USR.LINALBA))
      InsertValueCamp 1, LTRIM$(STR$(USR.LINFACT))
      InsertValueCamp 2, LTRIM$(STR$(USR.MAXLINS))
      InsertValueCamp 3, LTRIM$(STR$(DOCNUM.MAXRESG))
      InsertValueCamp 4, LTRIM$(STR$(DOCNUM.MAXCLIS))
      InsertValueCamp 5, LTRIM$(STR$(DOCNUM.MAXPROV))
      InsertValueCamp 6, LTRIM$(DOCNUM.NUMCLIS)
      InsertValueCamp 7, LTRIM$(DOCNUM.NUMREGS)
      InsertValueCamp 8, LTRIM$(DOCNUM.NUMPROV)

      DisplayAllCamps

    DO
      FOR C = 0 TO 8
	  VALUE = ReadCamp(C)
	  SELECT CASE VALUE
		 CASE 0
		      IF VAL(LTRIM$(RTRIM$(ValueCamp$(0)))) > 100 THEN
			 BEEP
			 C = C - 1
		      END IF
		 CASE 1
		      IF VAL(LTRIM$(RTRIM$(ValueCamp$(1)))) > 100 THEN
			 BEEP
			 C = C - 1
		      END IF
		 CASE 999
		      DeleteWindow 2
		      RETURN
		 CASE ELSE
	  END SELECT
      NEXT
      MAXALB = VAL(LTRIM$(RTRIM$(ValueCamp$(0))))
      MAXFAC = VAL(LTRIM$(RTRIM$(ValueCamp$(1))))
      MAXLIN = VAL(LTRIM$(RTRIM$(ValueCamp$(2))))

      MAXRES = VAL(LTRIM$(RTRIM$(ValueCamp$(3))))
      MAXCLI = VAL(LTRIM$(RTRIM$(ValueCamp$(4))))
      MAXPRO = VAL(LTRIM$(RTRIM$(ValueCamp$(5))))

      NUMCLI$ = LTRIM$(RTRIM$(ValueCamp$(6)))
      NUMRES$ = LTRIM$(RTRIM$(ValueCamp$(7)))
      NUMPRO$ = LTRIM$(RTRIM$(ValueCamp$(8)))

    LOOP UNTIL MAXALB < 100 OR MAXFAC < 100

    USR.LINALBA = MAXALB
    USR.LINFACT = MAXFAC
    USR.MAXLINS = MAXLIN

    DOCNUM.MAXRESG = MAXRES
    DOCNUM.MAXCLIS = MAXCLI
    DOCNUM.MAXPROV = MAXPRO
    DOCNUM.NUMCLIS = NUMCLI$
    DOCNUM.NUMREGS = NUMRES$
    DOCNUM.NUMPROV = NUMPRO$

    PUT AREA5, CTRL.USUARI, USR
    PUT AREANUM, 1, DOCNUM
    DeleteWindow 2
    RETURN

'***********************************************************************
' Configuraci¢ dels tipus de facturaci¢
'***********************************************************************

DOCUMD:
      DIM DOC$(10)
      LOCATE , , 0
      GetBackground 1, 1, 25, 79, FACTU$
DO
      TITOL$ = " Descripci¢               Codi      "

      GET AREANUM, 1, DOCNUM
      MAXDOC = DOCNUM.MAXNUM

      FOR XX = 1 TO MAXDOC            ' AVOCAR A DINS MEMORIA EL CATALEG DELS DOCUMENTS
	  DOC$(XX) = ""
	  FITX$ = LTRIM$(RTRIM$(DOCNUM.FACTNUM(XX).NUMFACT))
	  DESCRIP$ = LTRIM$(RTRIM$(DOCNUM.FACTNUM(XX).CONFACT))
	  DOC$(XX) = MARCA$ + " " + DESCRIP$ + SPACE$(LEN(TITOL$) - LEN(FITX$) - LEN(DESCRIP$) - 2) + FITX$
      NEXT XX
      DOCS = Achoice(4, 1, 17, 38, MAXDOC, DOC$(), COL(), TITOL$, 3, "")

      SELECT CASE DOCS
	     CASE 0, -13, -14
		  PutBackground 1, 1, FACTU$
		  EXIT DO
	     CASE -1
		  tecla = Avis("ERROR:", "No hi ha ajuda disponible", "Pitja una tecla...", 1)
	     CASE ELSE
		  ShowWindow 3
		  SetMaxCamps 2
		  SetInitCamp 0, 12, 38, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", "Descripci¢:"
		  SetInitCamp 1, 13, 38, ASCI, 0, "999999-XX", "Referäncia:"
		  SetInitCamp 2, 15, 38, ASCI, 0, "9999", "Prïxim document a generar:"
		  FOR C = 0 TO 2
		      SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
		  NEXT

		  InsertValueCamp 0, DOCNUM.FACTNUM(DOCS).CONFACT
		  InsertValueCamp 1, DOCNUM.FACTNUM(DOCS).NUMFACT
		  InsertValueCamp 2, LTRIM$(STR$(DOCNUM.FACTNUM(DOCS).MAXFACT))
		  DisplayAllCamps
		  FOR C = 0 TO 2
		      VALUE = ReadCamp(C)
		      SELECT CASE VALUE
			     CASE 999
				  ERASE DOC$
				  DeleteWindow 3
				  EXIT FOR
			     CASE F1 TO F10
				  C = C - 1
			     CASE ELSE
		      END SELECT
		  NEXT

		  DOCNUM.FACTNUM(DOCS).CONFACT = ValueCamp$(0)
		  DOCNUM.FACTNUM(DOCS).NUMFACT = ValueCamp$(1)
		  DOCNUM.FACTNUM(DOCS).MAXFACT = VAL(ValueCamp$(2))
		  PUT AREANUM, 1, DOCNUM
		  ERASE DOC$
		  DeleteWindow 3

      END SELECT
LOOP
      ERASE DOC$

    RETURN

'***********************************************************************
' Configuraci¢ dels parÖmetres de les vendes
'***********************************************************************

PVEN:
      ShowWindow 4

      SetMaxCamps 1
      SetInitCamp 0, 11, 38, ASCI, 0, "999", "Descompte %:"
      SetInitCamp 1, 12, 38, ASCI, 0, "999", "I.V.A.    %:"
      
      FOR C = 0 TO 1
	  SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
      NEXT

      GET AREA4, 1, CTRL
      GET AREA2, CTRL.EMPRESA, EMPRES

      InsertValueCamp 0, LTRIM$(STR$(EMPRES.DTO))
      InsertValueCamp 1, LTRIM$(STR$(EMPRES.IVA))

      DisplayAllCamps

    
      FOR C = 0 TO 1
	  VALUE = ReadCamp(C)
	  SELECT CASE VALUE
		 CASE 0
		 CASE 1
		 CASE 999
		      DeleteWindow 4
		      RETURN
		 CASE ELSE
	  END SELECT
      NEXT
      DTO = VAL(LTRIM$(RTRIM$(ValueCamp$(0))))
      IVA = VAL(LTRIM$(RTRIM$(ValueCamp$(1))))
			  
    EMPRES.DTO = DTO
    EMPRES.IVA = IVA
    PUT AREA2, CTRL.EMPRESA, EMPRES

    DeleteWindow 4
    RETURN

'***********************************************************************
' Configuraci¢ de les impresores
'***********************************************************************


IMPRE:
      DIM LPT$(6)
      DIM IMPRESORA$(100)
      LPT$(1) = "LPT1:      "
      LPT$(2) = "LPT2:      "
      LPT$(3) = "LPT3:      "
      LPT$(4) = "COM1:      "
      LPT$(5) = "COM2:      "
      LPT$(6) = "FITXER     "

      LOCATE , , 0
      GetBackground 1, 1, 25, 79, FACTU$
      TITOL$ = "  Impresora                     Fitxer"

      GET AREA4, 1, CTRL
      GET AREA5, CTRL.USUARI, USR

      AREA6 = FREEFILE: OPEN "..\SPOOL.CFG" FOR RANDOM SHARED AS AREA6 LEN = LEN(SPOOLIMP)
      GET AREA6, USR.IMPRESORA, SPOOLIMP
      FITXER$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
      RMAXIMP = LOF(AREA6) \ LEN(SPOOLIMP)

      FOR IM = 1 TO RMAXIMP            ' AVOCAR A DINS MEMORIA EL CATALEG DE LES IMPRESORES
	  IMPRESORA$(IM) = ""
	  GET AREA6, IM, SPOOLIMP
	  FITX$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
	  DESCRIP$ = LTRIM$(RTRIM$(SPOOLIMP.DESCRIPCION))
	  IF FITX$ = FITXER$ THEN MARCA$ = ">" ELSE MARCA$ = " "
	  IMPRESORA$(IM) = MARCA$ + " " + DESCRIP$ + SPACE$(LEN(TITOL$) - LEN(FITX$) - LEN(DESCRIP$) - 2) + FITX$
      NEXT IM

      IMPRE = Achoice(4, 1, 17, 40, RMAXIMP, IMPRESORA$(), COL(), TITOL$, 3, "")

      IF IMPRE = 0 OR IMPE = -14 OR IMPE = -13 THEN
	 CLOSE AREA6: ERASE LPT$, IMPRESORA$
	 PutBackground 1, 1, FACTU$
	 RETURN
      ELSE
	 CALL Menu2(LPT$(), CASO%, 10, 40, LEN(LPT$(1)) + 2, 6, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
	 IF CASO% = 999 THEN
	    CLOSE AREA6: ERASE LPT$, IMPRESORA$
	    PutBackground 1, 1, FACTU$
	    RETURN
	 ELSE
	    IF CASO% = 6 THEN
	       FINESTRA 9, 10, 14, 78, 1, CAIXA1
	       SetMaxCamps 0
	       SetColorCamp 0, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1), COL(2, 0), COL(2, 1)
	       SetInitCamp 0, 11, 25, ASCI, 0, STRING$(50, "X"), "Nom fitxer:"
	       DisplayAllCamps
	       IF ReadCamp(0) = SALIR THEN PutBackground 1, 1, FACTU$: CLOSE AREA6: RETURN
	       DEVICE$ = LTRIM$(RTRIM$(ValueCamp$(0)))
	    ELSE
	       DEVICE$ = LTRIM$(RTRIM$(LPT$(CASO%)))
	    END IF
	 END IF

	 GET AREA4, 1, CTRL
	 R = CTRL.USUARI
	 GET AREA5, R, USR
	 USR.DEVICE = DEVICE$
	 USR.IMPRESORA = IMPRE
	 PUT AREA5, R, USR
	 
	 GET AREA6, IMPRE, SPOOLIMP
	 FITXER$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
	 CLOSE AREA6: ERASE LPT$, IMPRESORA$
	 PutBackground 1, 1, FACTU$
	 RETURN
      END IF

'************************************************************************
' CREAR UNA EMPRESA NOVA
'************************************************************************
INSERTAR:

	  RNOU = MAXEM
	  RV = RNOU                            ' DESIGNAR EL REGISTRE NOU
	  GOSUB MASCARA
	  SetMaxCamps 3
	  FOR C = 0 TO 3: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  GOSUB SetInitCampS: COLOR COL(0, 0), COL(0, 1)
	  GOSUB BUIDARCAMPS     ' BUIDAR CAMPS
	  FOR C = 0 TO 3: DeleteCamp C: NEXT

	  DisplayAllCamps
	  GOSUB READCAMPS       ' INTRODUIR DADES ALS CAMPS
	  GOSUB MOUREFITXER     ' MOURE LES DADES DELS CAMPS AL FITXER
	  
	  IF ANUL = 1 THEN      ' SI ES PITJA <ESC> A UN DELS CAMPS
	     DeleteWindow 0
	     RETURN
	  END IF

	  CFG.INSTALAT = "INSTAL.LAT"

	    'CFG.DRIVE =
	    CFG.DDADE = LTRIM$(RTRIM$(ValueCamp$(11)))
	    CFG.DPANT = LTRIM$(RTRIM$(ValueCamp$(12)))
	    CFG.DRECU = LTRIM$(RTRIM$(ValueCamp$(13)))

	    EMPRES.IVA = VAL(ValueCamp$(7))
	    EMPRES.DTO = VAL(ValueCamp$(6))
	    USR.LINFACT = VAL(ValueCamp$(5))
	    D$ = ValueCamp$(9)

	    FOR L = 1 TO LEN(D$)
	      IF MID$(D$, L, 1) = "/" THEN MID$(D$, L, 1) = CHR$(32)
	    NEXT

	    C$ = LTRIM$(D$)
	    EMPRES.PASSWORD = ENCRIPT$(C$, 15)


	  PUT AREA1, RNOU, CFG     ' GRAVA DINS ELS FITXERS

	  GET AREA1, 1, CFG
	  CFG.MAXREG = MAXEM + 1
	  MAXEM = MAXEM + 1
	  PUT AREA1, 1, CFG

	  DBFC$ = LTRIM$(RTRIM$(CFG.DDADE))
 
	  LOCATE 24, 2: PRINT SPACE$(79);
	  LOCATE 24, 2: PRINT "Creant el direcctori " + DBFC$;
	  MKDIR DBFC$

	  LOCATE 24, 2: PRINT SPACE$(79);
	  LOCATE 24, 2: PRINT "Creant el direcctoris secundaris";
	  MKDIR DBFC$ + "\TEXTOS"
	  MKDIR DBFC$ + "\PLANTILL"
	  MKDIR DBFC$ + "\HISTORIC"

	  LOCATE 24, 2: PRINT SPACE$(79);
	  LOCATE 24, 2: PRINT "Preparant fitxers ...";

	  AREAC4 = FREEFILE: OPEN DBFC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREAC4 LEN = LEN(CEMPRE)
	  AREAC5 = FREEFILE: OPEN DBFC$ + "RESGUARD.DAT" FOR RANDOM SHARED AS AREAC5 LEN = LEN(RESGUA)
	  AREAC6 = FREEFILE: OPEN DBFC$ + "ENTRADES.DAT" FOR RANDOM SHARED AS AREAC6 LEN = 10
	  CEMPRE.MAXALBARAN = 1
	  CEMPRE.MAXFACTURA = 1
	  CEMPRE.MAXSTOCK = 1
	  CEMPRE.MAXCLIENTS = 1
	  CEMPRE.MAXPROVEID = 1
	  CEMPRE.MAXCOMANDE = 1
	  PUT AREAC4, 1, CEMPRE
	  RESGUA.CPY = "Fitxer dels RESGUARDS. Facturaci¢ 3.0" + CHR$(26)
	  RESGUA.MAXREG = 1
	  PUT AREAC5, 1, RESGUA: NE = 1
	  PUT AREAC6, 1, NE

	  AREAC = FREEFILE: OPEN DBFC$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)

	  IF TESTSCREEN = &HB000 THEN
	     COLORS.COL(0, 0) = 7: COLORS.COL(0, 1) = 0
	     COLORS.COL(1, 0) = 0: COLORS.COL(1, 1) = 7
	     COLORS.COL(2, 0) = 15: COLORS.COL(2, 1) = 0
	  ELSE
	     COLORS.COL(0, 0) = 15: COLORS.COL(0, 1) = 9
	     COLORS.COL(1, 0) = 15: COLORS.COL(1, 1) = 12
	     COLORS.COL(2, 0) = 14: COLORS.COL(2, 1) = 9
	  END IF
	  PUT AREAC, 1, COLORS
	  CLOSE AREAC4, AREAC5, AREAC6, AREAC
	  DeleteWindow 0
	  RETURN

'************************************************************************
' EDITAR EMPRESA
'************************************************************************
EDITAR:
	  
	  GOSUB MASCARA

	  GET AREA4, 1, CTRL
	  R = CTRL.EMPRESA

	  LOCK AREA1, R : GET AREA1, R, CFG
	  LOCK AREA2, R : GET AREA2, R, EMPRES

	  SetMaxCamps 3
	  FOR C = 0 TO 3: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  GOSUB SetInitCampS
	  GOSUB MOURECAMPS
	  COLOR COL(0, 0), COL(0, 1): CALL DisplayAllCamps
	  GOSUB READCAMPS

	  IF ANUL = 1 THEN           ' SI ES PITJA <ESC> A UN DELS CAMPS
	     UNLOCK AREA1, R : UNLOCK AREA2, R
	     DeleteWindow 0
	     RETURN
	  END IF

	  GOSUB MOUREFITXER       ' MOURE LES DADES DELS CAMPS AL FITXER
	  PUT AREA1, R, CFG
	  PUT AREA2, R, EMPRES
	  UNLOCK AREA1, R : UNLOCK AREA2, R
TORNAR:
	  DeleteWindow 0
	  RETURN



'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

READCAMPS:
       CORRECTE = 0
       DO

	  SetMaxCamps 3
	  FOR C = 0 TO 3
	      VALUE = ReadCamp(C)
	      SELECT CASE VALUE
		     CASE F1 TO F10
			  C = C - 1: SOUND 50, .5
		     CASE SALIR
			  ANUL = 1: RETURN
		     CASE ELSE
	      END SELECT
	  NEXT
	  LOCATE 11, 29: COLOR 27: PRINT " Son correctes aquestes dades (S/N) ?"
	  COLOR COL(0, 0), COL(0, 1)

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
SetInitCampS:
	  SetInitCamp 0, 7, 27, ASCI, 0, STRING$(30, "X"), ""
	  SetInitCamp 1, 8, 27, ASCI, 0, STRING$(30, "X"), ""
	  SetInitCamp 2, 9, 27, ASCI, 0, STRING$(18, "X"), ""
	  SetInitCamp 3, 9, 57, ASCI, 0, STRING$(19, "X"), ""
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPS:
	  InsertValueCamp 0, CFG.NOM
	  InsertValueCamp 1, CFG.DIREC
	  InsertValueCamp 2, CFG.TELF
	  InsertValueCamp 3, CFG.POBLA
	  RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXER:
	    IF R = 1 THEN
	       CFG.COPY = "Configuraci¢ Facturaci¢ 3.0" + CHR$(26)
	    ELSE
	       CFG.COPY = "CFG Nß:" + STR$(R) + CHR$(26)
	    END IF

	    CFG.NOM = LTRIM$(RTRIM$(ValueCamp$(0)))
	    CFG.DIREC = LTRIM$(RTRIM$(ValueCamp$(1)))
	    CFG.TELF = LTRIM$(RTRIM$(ValueCamp$(2)))
	    CFG.POBLA = LTRIM$(RTRIM$(ValueCamp$(3)))

	    RETURN

'************************************************************************
' BUIDAR ELS CAMPS DEL FITXER
'************************************************************************
BUIDARCAMPS:
	    CFG.NOM = ""
	    CFG.DIREC = ""
	    CFG.POBLA = ""
	    CFG.TELF = ""
	    RETURN

'************************************************************************
' FITXA
'************************************************************************

MASCARA:
      COLOR COL(0, 0), COL(0, 1)
	  ShowWindow 0
	  LOCATE 5, 11: PRINT "Empresa No.: "; : COLOR COL(0, 0): PRINT RV
	  LOCATE 6, 11: PRINT STRING$(65, "ƒ"); : COLOR COL(2, 0)
	  LOCATE 7, 11: PRINT "Nom............: ";
	  LOCATE 8, 11: PRINT "Direcci¢.......: ";
	  LOCATE 9, 11: PRINT "Teläfon........: " + SPACE$(19) + "Poblaci¢:": COLOR COL(0, 0)

	  RETURN

'************************************************************************
'CONFIGURAR DIRECTORIS
'************************************************************************
CONF.DIRECC:
	  GET AREA4, 1, CTRL
	  R = CTRL.EMPRESA

	  LOCK AREA1, R : GET AREA1, R, CFG
	  
	  ShowWindow 6
	  SetMaxCamps 9
	  FOR C = 0 TO 9: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  SetInitCamp 0, 6, 38, ASCI, 0, STRING$(30, "X"), "Direcctori programa:"
	  SetInitCamp 1, 7, 38, ASCI, 0, STRING$(30, "X"), "Direcctori dades:"
	  SetInitCamp 2, 8, 38, ASCI, 0, STRING$(30, "X"), "Direcctori pantalles:"
	  SetInitCamp 3, 9, 38, ASCI, 0, STRING$(30, "X"), "Direcctori recursos:"
	  SetInitCamp 4, 10, 38, ASCI, 0, STRING$(30, "X"), "Direcctori principal:"
	  SetInitCamp 5, 11, 38, ASCI, 0, STRING$(30, "X"), "Direcctori d'exportaci¢:"
	  SetInitCamp 6, 12, 38, ASCI, 0, STRING$(30, "X"), "Direcctori d'importaci¢:"
	  SetInitCamp 7, 13, 38, ASCI, 0, STRING$(30, "X"), "Direcctori de sistema:"
	  SetInitCamp 8, 14, 38, ASCI, 0, STRING$(30, "X"), "Direcctori d'ajuda:"
	  SetInitCamp 9, 15, 38, ASCI, 0, STRING$(30, "X"), "Direcctori RAMDISK:"
	  
	  InsertValueCamp 0, CFG.DRIVE
	  InsertValueCamp 1, CFG.DDADE
	  InsertValueCamp 2, CFG.DPANT
	  InsertValueCamp 3, CFG.DRECU
	  InsertValueCamp 4, CFG.DMAIN
	  InsertValueCamp 5, CFG.DEXPO
	  InsertValueCamp 6, CFG.DIMPO
	  InsertValueCamp 7, CFG.DSIST
	  InsertValueCamp 8, CFG.DHELP
	  InsertValueCamp 9, CFG.DTEMP
	  
	  COLOR COL(0, 0), COL(0, 1): CALL DisplayAllCamps
	  FOR C = 0 TO 9
	      VALUE = ReadCamp(C)
	      SELECT CASE VALUE
		     CASE 999
			  UNLOCK AREA1, R
			  DeleteWindow 6
			  RETURN
		     CASE F1 TO F10
			  C = C - 1
		     CASE ELSE
	      END SELECT
	  NEXT

	  CFG.DRIVE = LTRIM$(RTRIM$(ValueCamp$(0)))
	  CFG.DDADE = LTRIM$(RTRIM$(ValueCamp$(1)))
	  CFG.DPANT = LTRIM$(RTRIM$(ValueCamp$(2)))
	  CFG.DRECU = LTRIM$(RTRIM$(ValueCamp$(3)))
	  CFG.DMAIN = LTRIM$(RTRIM$(ValueCamp$(4)))
	  CFG.DEXPO = LTRIM$(RTRIM$(ValueCamp$(5)))
	  CFG.DIMPO = LTRIM$(RTRIM$(ValueCamp$(6)))
	  CFG.DSIST = LTRIM$(RTRIM$(ValueCamp$(7)))
	  CFG.DHELP = LTRIM$(RTRIM$(ValueCamp$(8)))
	  CFG.DTEMP = LTRIM$(RTRIM$(ValueCamp$(9)))
	  
	  PUT AREA1, R, CFG
	  UNLOCK AREA1, R
TORNARMENU:
	  DeleteWindow 6
	  RETURN


      RETURN

'************************************************************************
'MODIFICAR IMPRESORA
'************************************************************************
IMPRESORA:

      DIM ELPT$(6)
      DIM EIMPRESORA$(100)
      ELPT$(1) = "LPT1:      "
      ELPT$(2) = "LPT2:      "
      ELPT$(3) = "LPT3:      "
      ELPT$(4) = "COM1:      "
      ELPT$(5) = "COM2:      "
      ELPT$(6) = "FITXER     "

      LOCATE , , 0
      GetBackground 1, 1, 25, 79, FACTU$
      TITOL$ = "  Impresora                     Fitxer"
      GET AREA4, 1, CTRL
      R = CTRL.EMPRESA
      GET AREA2, R, EMPRES

      AREA5 = FREEFILE: OPEN "..\SPOOL.CFG" FOR RANDOM SHARED AS AREA5 LEN = LEN(SPOOLIMP)
      GET AREA5, EMPRES.IMPRESORA, SPOOLIMP
      FITXER$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
      RMAXIMP = LOF(AREA5) \ LEN(SPOOLIMP)

      FOR IA = 1 TO RMAXIMP            ' AVOCAR A DINS MEMORIA EL CATALEG DE LES IMPRESORES
	  GET AREA5, IA, SPOOLIMP
	  FITX$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
	  DESCRIP$ = LTRIM$(RTRIM$(SPOOLIMP.DESCRIPCION))
	  IF FITX$ = FITXER$ THEN MARCA$ = "*" ELSE MARCA$ = " "
	  EIMPRESORA$(IA) = MARCA$ + " " + DESCRIP$ + SPACE$(LEN(TITOL$) - LEN(FITX$) - LEN(DESCRIP$) - 2) + FITX$
      NEXT

      IMPRE = Achoice(4, 1, 17, 40, RMAXIMP, EIMPRESORA$(), COL(), TITOL$, 3, "")

      IF IMPRE = 0 OR IMPE = -14 OR IMPE = -13 THEN
	 CLOSE AREA5: ERASE ELPT$, EIMPRESORA$
	 PutBackground 1, 1, FACTU$
	 RETURN
      ELSE
	 

	 CALL Menu2(ELPT$(), CASO%, 10, 40, LEN(ELPT$(1)) + 2, 6, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))

	 IF CASO% = 999 THEN
	    CLOSE AREA5: PutBackground 1, 1, FACTU$: ERASE ELPT$, IMPRESORA$
	    RETURN
	 ELSE
	    IF CASO% = 6 THEN
	       FINESTRA 9, 10, 14, 78, 1, CAIXA1
	       SetMaxCamps 0
	       SetColorCamp 0, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1), COL(2, 0), COL(2, 1)
	       SetInitCamp 0, 11, 25, ASCI, 0, STRING$(50, "X"), "Nom fitxer:"
	       DisplayAllCamps
	       IF ReadCamp(0) = SALIR THEN PutBackground 1, 1, FACTU$: CLOSE AREA5: ERASE LPT$, IMPRESORA$: RETURN
	       DEVICE$ = LTRIM$(RTRIM$(ValueCamp$(0)))
	    ELSE
	       DEVICE$ = LTRIM$(RTRIM$(ELPT$(CASO%)))
	    END IF
	 END IF

	 LOCK AREA2, R
	 GET AREA2, R, EMPRES
	 EMPRES.DEVICE = DEVICE$
	 EMPRES.IMPRESORA = IMPRE
	 PUT AREA2, R, EMPRES
	 UNLOCK AREA2, R

	 GET AREA5, IMPRE, SPOOLIMP
	 FITXER$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
	 CLOSE AREA5
	 ERASE ELPT$, EIMPRESORA$
	 PutBackground 1, 1, FACTU$
	 RETURN
      END IF

'************************************************************************
'MODIFICAR CAPÄALERA
'************************************************************************
CAPSALERA:
      GetBackground 1, 1, 25, 79, FACTU$
      COLOR COL(0, 0), COL(0, 1)

      GET AREA4, 1, CTRL
      R = CTRL.EMPRESA
      GET AREA1, R, CFG
      diremp$ = RTRIM$(LTRIM$(CFG.DDADE))
      AREACAP = FREEFILE: OPEN diremp$ + "PLANTILL\CAPÄALER.DAT" FOR RANDOM SHARED AS AREACAP LEN = LEN(CAP)

      FINESTRA 9, 10, 17, 61, 1, CAIXA1
      SetMaxCamps 6
      FOR C = 0 TO 6: SetColorCamp C, COL(1, 0), COL(1, 1), COL(1, 0), COL(1, 1), COL(2, 0), COL(2, 1): NEXT
      S = 10
      FOR L = 0 TO 6
	  SetInitCamp L, S, 11, ASCI, 0, STRING$(50, "X"), ""
	  S = S + 1
      NEXT

      GET AREACAP, 1, CAP: LOCK AREACAP, 1
      FOR L = 0 TO 6: InsertValueCamp L, CAP.LINIES(L): NEXT

      DisplayAllCamps
      FOR Q = 0 TO 6
	  IF ReadCamp(Q) = SALIR THEN
	     CLOSE AREACAP
	     PutBackground 1, 1, FACTU$
	     RETURN
	  END IF
      NEXT
      FOR Q = 0 TO 6
	  CAP.LINIES(Q) = ValueCamp$(Q)
      NEXT
      PUT AREACAP, 1, CAP
      CLOSE AREACAP
      PutBackground 1, 1, FACTU$
      RETURN

'************************************************************************
'MODIFICAR PEUS DE PÖGINA
'************************************************************************

PIEPAG:
      GetBackground 1, 1, 25, 79, FACTU$
      COLOR COL(0, 0), COL(0, 1)

      GET AREA4, 1, CTRL
      R = CTRL.EMPRESA
      GET AREA1, R, CFG
      diremp$ = RTRIM$(LTRIM$(CFG.DDADE))
      
      AREAP = FREEFILE: OPEN diremp$ + "PLANTILL\PEUSPAGI.DAT" FOR RANDOM SHARED AS AREAP LEN = LEN(PP)

      FINESTRA 9, 10, 13, 61, 1, CAIXA1
      SetMaxCamps 2
      FOR C = 0 TO 2: SetColorCamp C, COL(1, 0), COL(1, 1), COL(1, 0), COL(1, 1), COL(2, 0), COL(2, 1): NEXT
      S = 10
      FOR L = 0 TO 2
	  SetInitCamp L, S, 11, ASCI, 0, STRING$(50, "X"), ""
	  S = S + 1
      NEXT

      GET AREAP, 1, PP: LOCK AREAP, 1
      FOR L = 0 TO 2
	  InsertValueCamp L, PP.LINIES(0, L)
      NEXT

      DisplayAllCamps
      FOR Q = 0 TO 2
	  IF ReadCamp(Q) = SALIR THEN
	     UNLOCK AREAP, 1
	     CLOSE AREAP
	     PutBackground 1, 1, FACTU$
	     RETURN
	  END IF
      NEXT
      FOR Q = 0 TO 2
	  PP.LINIES(0, Q) = ValueCamp$(Q)
      NEXT
      PUT AREAP, 1, PP: UNLOCK AREAP, 1
      CLOSE AREAP
      PutBackground 1, 1, FACTU$
      RETURN

'************************************************************************
'MODIFICAR ELS COLORS
'************************************************************************
CONF.COLORS:
      GET AREA4, 1, CTRL
      R = CTRL.EMPRESA
      GET AREA1, R, CFG
      GET AREA2, R, EMPRES

      diremp$ = RTRIM$(LTRIM$(CFG.DDADE))
      
      AREAC = FREEFILE: OPEN diremp$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
      GET AREAC, 1, COLORS: COLOR COLORS.COL(0, 0), COLORS.COL(0, 1)
      SetMaxCamps 5
      FOR C = 0 TO 5: SetColorCamp C, (COLORS.COL(1, 0)), (COLORS.COL(1, 1)), (COLORS.COL(0, 0)), (COLORS.COL(0, 1)), (COLORS.COL(2, 0)), (COLORS.COL(2, 1)): NEXT
      SetInitCamp 0, 11, 30, ASCI, 0, "999", "Tinta:": SetInitCamp 1, 11, 40, ASCI, 0, "999", "Fons:"
      SetInitCamp 2, 13, 30, ASCI, 0, "999", "Tinta:": SetInitCamp 3, 13, 40, ASCI, 0, "999", "Fons:"
      SetInitCamp 4, 15, 30, ASCI, 0, "999", "Tinta:": SetInitCamp 5, 15, 40, ASCI, 0, "999", "Fons:"
      InsertValueCamp 0, LTRIM$(STR$(COLORS.COL(0, 0))): InsertValueCamp 1, LTRIM$(STR$(COLORS.COL(0, 1)))
      InsertValueCamp 2, LTRIM$(STR$(COLORS.COL(1, 0))): InsertValueCamp 3, LTRIM$(STR$(COLORS.COL(1, 1)))
      InsertValueCamp 4, LTRIM$(STR$(COLORS.COL(2, 0))): InsertValueCamp 5, LTRIM$(STR$(COLORS.COL(2, 1)))
      ShowWindow 5
      GOSUB PINTA
      
      FOR C = 0 TO 5
	  VALUE = ReadCamp(C)
	  GOSUB ACTUALIZ.COLOR

	  IF VALUE = SALIR THEN
	     DeleteWindow 5
	     RETURN
	  END IF
      NEXT

      COLORS.COL(0, 0) = VAL(ValueCamp$(0)): COLORS.COL(0, 1) = VAL(ValueCamp$(1))
      COLORS.COL(1, 0) = VAL(ValueCamp$(2)): COLORS.COL(1, 1) = VAL(ValueCamp$(3))
      COLORS.COL(2, 0) = VAL(ValueCamp$(4)): COLORS.COL(2, 1) = VAL(ValueCamp$(5))

      PUT AREAC, 1, COLORS
      DeleteWindow 5
      RETURN

ACTUALIZ.COLOR:
      COL(0, 0) = VAL(ValueCamp$(0)): COL(0, 1) = VAL(ValueCamp$(1))
      COL(1, 0) = VAL(ValueCamp$(2)): COL(1, 1) = VAL(ValueCamp$(3))
      COL(2, 0) = VAL(ValueCamp$(4)): COL(2, 1) = VAL(ValueCamp$(5))
      GOSUB PINTA
      RETURN
PINTA:
      COLOR COLORS.COL(0, 0), COLORS.COL(0, 1): LOCATE 11, 11: PRINT "Text normal "
      COLOR COLORS.COL(1, 0), COLORS.COL(1, 1): LOCATE 13, 11: PRINT "Text subrat "
      COLOR COLORS.COL(2, 0), COLORS.COL(2, 1): LOCATE 15, 11: PRINT "Text titol  "
      FOR S = 0 TO 5: SetColorCamp S, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
      DisplayAllCamps
      RETURN

'************************************************************************
' OBRIR ELS FITXERS
'************************************************************************

OBRIRFITXERS:
  
      AREA1 = FREEFILE: OPEN "..\CONFIG.FAC" FOR RANDOM SHARED AS AREA1 LEN = LEN(CFG)
      AREA2 = FREEFILE: OPEN "..\EMPRESA.FAC" FOR RANDOM SHARED AS AREA2 LEN = LEN(EMPRES)
      AREA3 = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA3 LEN = LEN(CEMPRE)
      AREA4 = FREEFILE: OPEN "..\CONTROL.CFG" FOR RANDOM SHARED AS AREA4 LEN = LEN(CTRL)
      AREA5 = FREEFILE: OPEN "..\USUARIS.FAC" FOR RANDOM SHARED AS AREA5 LEN = LEN(USR)
      AREANUM = FREEFILE: OPEN DIRECC$ + "DOCUMENT.NUM" FOR RANDOM SHARED AS AREANUM LEN = LEN(DOCNUM)

'************************************************************************
' ACTUALITZAR REGISTRES
'************************************************************************

      GET AREA3, 1, CEMPRE
      MAXEM = CFG.MAXREG
      GET AREANUM, 1, DOCNUM
      RETURN

DEFWINDOWS:
      IF SetInitWindows(7) = TRUE THEN
	 tecla = Avis("ERROR 001W:", "Memïria insuficient per crear les finästres!!!", "Pitja una tecla...", 0)
	 EXIT SUB
      END IF

      WIN1 = InitNewWindow(0, 4, 10, 12, 76, 1, "Fitxa de l'empresa")
      WIN2 = InitNewWindow(1, 9, 10, 17, 44, 1, "Canvi de Password")
      WIN3 = InitNewWindow(2, 9, 10, 21, 77, 1, "ParÖmetres dels documents")
      WIN4 = InitNewWindow(3, 9, 10, 17, 70, 1, "ParÖmetres dels documents")
      WIN5 = InitNewWindow(4, 9, 10, 14, 60, 1, "ParÖmetres de les vendes")
      WIN6 = InitNewWindow(5, 9, 10, 17, 70, 1, "Configuraci¢ Colors")
      WIN7 = InitNewWindow(6, 4, 10, 17, 70, 1, "Configuraci¢ de direcctoris")
      
      FOR W = 0 TO 6
	  SetStyleWindow W, 0, ""
	  SetColorWindow W, COL(0, 0), COL(0, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
      NEXT
      RETURN
END SUB

