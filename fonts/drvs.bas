' ********************************************************************
'
' Fitxer....................: DRVS.BAS
' Titol.....................: Modul per el mateniment d'impresores
'
' ********************************************************************
'
' Data inici................: 20/09/1997  2:42:00
' Data de la darrera revisi¢: 20/09/1997 21:11:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
' Codi......................: MS-BASIC 7.01 (PDS)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'

DECLARE SUB InitDesktop ()
DECLARE FUNCTION ConverterCCACN.CNACC$ (CAD$, SISTEMA!)

DIM SHARED SPOOLIMP AS SPOOL
DIM SHARED ARCHIMP AS IMPRESORA
DIM SHARED CFG AS CONFIG
DIM SHARED COL(2, 1)

     ON ERROR GOTO ERRORS
     CLS
     GOSUB COLS
     InitDesktop

     IF DIR$("CONFIG.FAC") = "" THEN
	tecla = Avis("ERROR:", "En aquest direcctori no hi ha el fitxer de configuraci¢", "Piji una tecla...", 0)
	SYSTEM
     END IF
     AREACFG = FREEFILE: OPEN "CONFIG.FAC" FOR RANDOM SHARED AS AREACFG LEN = LEN(CFG)
     
     GET AREACFG, 1, CFG
     DIRP$ = LTRIM$(RTRIM$(CFG.DRIVE))
     
     AREASPL = FREEFILE: OPEN "SPOOL.CFG" FOR RANDOM AS AREASPL LEN = LEN(SPOOLIMP)
     
MOSTRAR:
     DO
      LOCATE , , 0
      TITOL$ = "Nom de la impresora             Fitxer"
      RMAXIMP = LOF(AREASPL) \ LEN(SPOOLIMP)

      DIM IMPRESORA$(RMAXIMP)
      FOR IM = 1 TO RMAXIMP            ' AVOCAR A DINS MEMORIA EL CATALEG DE LES IMPRESORES
	  IMPRESORA$(IM) = ""
	  GET AREASPL, IM, SPOOLIMP
	  FITX$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
	  DESCRIP$ = SPOOLIMP.DESCRIPCION
	  IMPRESORA$(IM) = DESCRIP$ + " " + SPACE$(LEN(TITOL$) - LEN(FITX$) - LEN(DESCRIP$) - 1) + FITX$
      NEXT IM

      IF ORDENA = 1 THEN CALL DIMSort(IMPRESORA$(), RMAXIMP + 1)
      IMPRE = Achoice(5, 10, 17, 49, RMAXIMP, IMPRESORA$(), COL(), TITOL$, 1, "")

      IF IMPRE = 0 OR IMPE = -14 OR IMPE = -13 THEN
	 ERASE IMPRESORA$
	 SYSTEM
      ELSE
	 IF IMPRE < 0 THEN
	    SELECT CASE IMPRE
		   CASE -3
			IF ORDENA = 1 THEN ORDENA = 0 ELSE ORDENA = 1
		   CASE -2
			GOSUB INSERTAR
		   CASE -4
		   CASE -5
			GOSUB test
		   CASE ELSE
	    END SELECT
	 ELSE
	    NOMF$ = LTRIM$(MID$(IMPRESORA$(IMPRE), 27, 29))
	    GOSUB MODIFICAR
	 END IF
      END IF
      ERASE IMPRESORA$
     LOOP

INSERTAR:
     GetBackground 1, 1, 25, 80, IMPRESOR$
     RMAXIMP = LOF(AREASPL) \ LEN(SPOOLIMP) + 1
     IMPRESORADEF = RMAXIMP
     BOXMSG$ = "Insertar una impresora nova": GOSUB MASCARA
     SetMaxCamps 20
     GOSUB INITCAMPS
     FOR C = 0 TO MAXCAMPS: DeleteCamp C: NEXT
     DisplayAllCamps
     ANULA = 0
     FOR C = 0 TO MAXCAMPS
	 VALUE = ReadCamp(C)
	 SELECT CASE VALUE
		CASE 0 TO MAXCAMPS
		CASE 999
		     ANULA = 1
		     EXIT FOR
		CASE ELSE
		     C = C - 1
	 END SELECT
     NEXT
     IF ANULA = 0 THEN
	NOMF$ = LTRIM$(RTRIM$(ValueCamp$(1)))
	AREAPRN = FREEFILE: OPEN DIRP$ + "\IMPRESOR\" + NOMF$ FOR RANDOM AS AREAPRN LEN = LEN(ARCHIMP)
	RMAXIMP = LOF(AREASPL) \ LEN(SPOOLIMP)
	GOSUB CAMPSFITXER
	PUT AREASPL, RMAXIMP + 1, SPOOLIMP
	PUT AREAPRN, 1, ARCHIMP
	CLOSE AREAPRN
	tecla = Avis("Si vol imprimir la p…gina de prova", "", "pitji <ENTER>,<ESC> Anula", 0)
	IF tecla = 1 THEN
	   GOSUB test
	END IF
     END IF
     PutBackground 1, 1, IMPRESOR$
     RETURN

MODIFICAR:
     FOR R = 1 TO RMAXIMP
	 GET AREASPL, R, SPOOLIMP
	 IF LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO)) = LTRIM$(RTRIM$(NOMF$)) THEN
	    NOMF$ = SPOOLIMP.ARCHIVO
	    DESC$ = LTRIM$(RTRIM$(SPOOLIMP.DESCRIPCION))
	    EXIT FOR
	 END IF
     NEXT
     AREAPRN = FREEFILE: OPEN DIRP$ + "\IMPRESOR\" + RTRIM$(NOMF$) FOR RANDOM AS AREAPRN LEN = LEN(ARCHIMP)
     IMPRESORADEF = R
     GetBackground 1, 1, 25, 80, IMPRESOR$
     BOXMSG$ = "Modificant impresora ": GOSUB MASCARA
     SetMaxCamps 20
     GET AREAPRN, 1, ARCHIMP
     GOSUB INITCAMPS
     GOSUB INSERTACAMPS
     DisplayAllCamps
     FOR C = 0 TO MAXCAMPS
	 VALUE = ReadCamp(C)
	 SELECT CASE VALUE
		CASE 0 TO MAXCAMPS
		CASE 999
		     EXIT FOR
		CASE ELSE
		     C = C - 1
	 END SELECT
     NEXT
     GOSUB CAMPSFITXER
     PUT AREAPRN, 1, ARCHIMP
     CLOSE AREAPRN
     tecla = Avis("Si vol imprimir la p…gina de prova", "", "pitji <ENTER>,<ESC> Anula", 0)
     IF tecla = 1 THEN
	GOSUB test
     END IF

     PutBackground 1, 1, IMPRESOR$
     RETURN

INITCAMPS:
     SetInitCamp 0, 7, 14, ASCI, 0, STRING$(20, "X"), ""    ' Descripci¢
     SetInitCamp 1, 7, 48, ASCI, 0, "XXXXXXXXXXXX", ""      ' Nom del fitxer

     SetInitCamp 2, 9, 19, ASCI, 0, STRING$(22, "X"), ""    ' ENSANCHADO
     SetInitCamp 3, 9, 57, ASCI, 0, STRING$(22, "X"), ""

     SetInitCamp 4, 10, 19, ASCI, 0, STRING$(22, "X"), ""   ' COMPRIMIDO
     SetInitCamp 5, 10, 57, ASCI, 0, STRING$(22, "X"), ""

     SetInitCamp 6, 11, 19, ASCI, 0, STRING$(22, "X"), ""   ' ITALICA
     SetInitCamp 7, 11, 57, ASCI, 0, STRING$(22, "X"), ""

     SetInitCamp 8, 12, 19, ASCI, 0, STRING$(22, "X"), ""   ' SUBRAYADO
     SetInitCamp 9, 12, 57, ASCI, 0, STRING$(22, "X"), ""

     SetInitCamp 10, 13, 19, ASCI, 0, STRING$(22, "X"), ""  ' NEGRITA
     SetInitCamp 11, 13, 57, ASCI, 0, STRING$(22, "X"), ""

     SetInitCamp 12, 15, 19, ASCI, 0, STRING$(22, "X"), ""  ' SALTO DE PAGINA
     SetInitCamp 13, 15, 57, ASCI, 0, STRING$(22, "X"), ""  ' RESET DE BUFFER
     SetInitCamp 14, 16, 19, ASCI, 0, STRING$(22, "X"), ""  ' RESET GENERAL
     SetInitCamp 15, 16, 57, ASCI, 0, STRING$(22, "X"), ""  ' INHIBIR IMPRESORA
     SetInitCamp 16, 17, 19, ASCI, 0, STRING$(22, "X"), ""  ' BELL
     SetInitCamp 17, 19, 19, ASCI, 0, STRING$(22, "X"), ""  ' BANDEJA 1
     SetInitCamp 18, 19, 57, ASCI, 0, STRING$(22, "X"), ""  ' BANDEJA 2
     SetInitCamp 19, 20, 19, ASCI, 0, STRING$(22, "X"), ""  ' DIRECCION 1
     SetInitCamp 20, 20, 57, ASCI, 0, STRING$(22, "X"), ""  ' DIRECCION 2

     FOR C = 0 TO MAXCAMPS
	 SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
     NEXT
     RETURN

INSERTACAMPS:
     InsertValueCamp 0, DESC$
     InsertValueCamp 1, NOMF$
								    
     InsertValueCamp 2, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.ENSANCHADO), 100)
     InsertValueCamp 3, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.NOENSANCHADO), 100)

     InsertValueCamp 4, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.COMPRIMIDO), 100)
     InsertValueCamp 5, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.NOCOMPRIMIDO), 100)

     InsertValueCamp 6, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.ITALICA), 100)
     InsertValueCamp 7, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.NOITALICA), 100)

     InsertValueCamp 8, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.SUBRAYADO), 100)
     InsertValueCamp 9, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.NOSUBRAYADO), 100)

     InsertValueCamp 10, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.NEGRITA), 100)
     InsertValueCamp 11, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.NONEGRITA), 100)

     InsertValueCamp 12, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.SALTOPAGINA), 100)
     InsertValueCamp 13, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.RESETBUFFER), 100)
     InsertValueCamp 14, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.RESETGENERAL), 100)
     InsertValueCamp 15, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.INHIBEIMPRESORA), 100)
     InsertValueCamp 16, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.BELL), 100)
     InsertValueCamp 17, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.BANDEJA1), 100)
     InsertValueCamp 18, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.BANDEJA2), 100)
     InsertValueCamp 19, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.DIRECCION1), 100)
     InsertValueCamp 20, ConverterCCACN.CNACC$(RTRIM$(ARCHIMP.DIRECCION2), 100)
     RETURN

CAMPSFITXER:
      SPOOLIMP.DESCRIPCION = ValueCamp$(0)
      SPOOLIMP.ARCHIVO = ValueCamp$(1)

      ARCHIMP.ENSANCHADO = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(2)), 200)
      ARCHIMP.NOENSANCHADO = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(3)), 200)
      ARCHIMP.COMPRIMIDO = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(4)), 200)
      ARCHIMP.NOCOMPRIMIDO = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(5)), 200)
      ARCHIMP.ITALICA = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(6)), 200)
      ARCHIMP.NOITALICA = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(7)), 200)
      ARCHIMP.SUBRAYADO = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(8)), 200)
      ARCHIMP.NOSUBRAYADO = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(9)), 200)
      ARCHIMP.NEGRITA = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(10)), 200)
      ARCHIMP.NONEGRITA = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(11)), 200)
      ARCHIMP.SALTOPAGINA = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(12)), 200)
      ARCHIMP.RESETBUFFER = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(13)), 200)
      ARCHIMP.RESETGENERAL = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(14)), 200)
      ARCHIMP.INHIBEIMPRESORA = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(15)), 200)
      ARCHIMP.BELL = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(16)), 200)
      ARCHIMP.BANDEJA1 = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(17)), 200)
      ARCHIMP.BANDEJA2 = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(18)), 200)
      ARCHIMP.DIRECCION1 = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(19)), 200)
      ARCHIMP.DIRECCION2 = ConverterCCACN.CNACC$(RTRIM$(ValueCamp$(20)), 200)
      RETURN
     

MASCARA:
     COLOR COL(0, 0), COL(0, 1): FINESTRA 4, 1, 22, 80, 0, CAIXA2
     LOCATE 8, 2: PRINT STRING$(78, "Ä"); : LOCATE 14, 2: PRINT STRING$(78, "Ä");
     LOCATE 18, 2: PRINT STRING$(78, "Ä");
     LOCATE 6, 2: PRINT STRING$(78, "Ä"); : COLOR COL(2, 0), COL(2, 1)
     LOCATE 5, 2: PRINT BOXMSG$ + " -- N£mero: "; IMPRESORADEF
     LOCATE 7, 2: PRINT "Descripci¢: " + STRING$(20, "X") + "  Nom fitxer: XXXXXXXX.XXX"
     LOCATE 9, 2: PRINT "Activa ample....:XXXXXXXXXXXXXXXXXXXXXX  Anula ample..:XXXXXXXXXXXXXXXXXXXXXX"
     LOCATE 10, 2: PRINT "Activa comprimit:XXXXXXXXXXXXXXXXXXXXXX  Anula compri.:XXXXXXXXXXXXXXXXXXXXXX"
     LOCATE 11, 2: PRINT "Activa italica..:XXXXXXXXXXXXXXXXXXXXXX  Anula italica:XXXXXXXXXXXXXXXXXXXXXX"
     LOCATE 12, 2: PRINT "Activa subratlla:XXXXXXXXXXXXXXXXXXXXXX  Anula subrat.:XXXXXXXXXXXXXXXXXXXXXX"
     LOCATE 13, 2: PRINT "Activa negreta..:XXXXXXXXXXXXXXXXXXXXXX  Anula negreta:XXXXXXXXXXXXXXXXXXXXXX"
     LOCATE 15, 2: PRINT "Salt de p…gina..:XXXXXXXXXXXXXXXXXXXXXX  Reset buffer.:XXXXXXXXXXXXXXXXXXXXXX"
     LOCATE 16, 2: PRINT "Reset general...:XXXXXXXXXXXXXXXXXXXXXX  Inhibir LPT..:XXXXXXXXXXXXXXXXXXXXXX"
     LOCATE 17, 2: PRINT "Avis sonor......:XXXXXXXXXXXXXXXXXXXXXX"
     LOCATE 19, 2: PRINT "Palengana 1.....:XXXXXXXXXXXXXXXXXXXXXX  Palengana 2..:XXXXXXXXXXXXXXXXXXXXXX"
     LOCATE 20, 2: PRINT "Unidireccional..:XXXXXXXXXXXXXXXXXXXXXX  Bidireccional:XXXXXXXXXXXXXXXXXXXXXX"
     RETURN

test:
	    LCENTER RTRIM$(ARCHIMP.ENSANCHADO) + "Smart Software 1993/97 (C)" + RTRIM$(ARCHIMP.NOENSANCHADO)
	    LPRINT
	    LPRINT " Enhorabona!"
	    LPRINT "            -Si pot llegir aquest text es que ha instal.lat b‚ la seva"
	    LPRINT "            impresora."
	    LPRINT
	    LPRINT "Nom de fitxer.....: " + DIRP$ + "\IMPRESORA\" + NOMF$
	    LPRINT "Nom del dispositiu: " + SPOOLIMP.DESCRIPCION
	    LPRINT "Impresi¢ acual....: LPT1:"
	    RETURN
COLS:
	    COL(0, 0) = 15: COL(0, 1) = 9
	    COL(1, 0) = 0: COL(1, 1) = 7
	    COL(2, 0) = 7: COL(2, 1) = 9
	    RETURN
ERRORS:
	    IF ShowError THEN
	       RESUME NEXT
	    ELSE
	       RESET
	       SYSTEM
	    END IF

FUNCTION ConverterCCACN.CNACC$ (CAD$, SISTEMA)
	 CAD$ = LTRIM$(RTRIM$(CAD$))
	 L = LEN(CAD$): I = 1
	 CFIN$ = ""
	 IF L = 0 THEN GOSUB FIN
	 SELECT CASE SISTEMA
		CASE 100
		     FOR I = 1 TO L
			 ASCII = ASC(MID$(CAD$, I, 1))
			 IF I = L THEN S$ = "" ELSE S$ = "+"
			 NUME$ = RTRIM$(LTRIM$(STR$(ASCII))) + S$
			 CFIN$ = CFIN$ + NUME$
		     NEXT
		CASE 200
		     DO
			 MN$ = "": SURT = 0
			 DO
			       ASCII = ASC(MID$(CAD$, I, 1))
			       IF ASCII >= 48 AND NOT ASCII >= 58 THEN
				  MN$ = MN$ + CHR$(ASCII)
			       END IF
			       I = I + 1
			 LOOP UNTIL ASCII = 43 OR I > L
			 NUMF$ = MID$(MN$, 1, LEN(MN$))
			 IF VAL(NUMF$) > 255 THEN NUMF$ = "255"
			 CFIN$ = CFIN$ + CHR$(VAL(NUMF$))
		     LOOP UNTIL I > L
		CASE ELSE
		     tecla = Avis("ERROR ConverterCCACN.CNACC:", "El sistema de conversi¢ no ha estat assignat", "Pitji una tecla...", 0)
		     ConverterCCACN.CNACC$ = "ERROR: En el par…metre SISTEMA"
		     EXIT FUNCTION
	 END SELECT
FIN:
	 ConverterCCACN.CNACC$ = CFIN$
END FUNCTION

SUB InitDesktop
    COLOR COL(0, 0), COL(0, 1)
    FONS 176
    FINESTRA 1, 1, 3, 80, 0, CAIXA2
    FINESTRA 23, 1, 25, 80, 0, CAIXA2
    COLOR COL(2, 0), COL(2, 1)
    LOCATE 2, 2: PRINT "Controlador d'impresores 1.0"
    LOCATE 2, 68: PRINT "TCC 1997 (C)";
    LOCATE 24, 2: PRINT "<ESC> Sortir  <F2> Insertar  <F3> Ordenar  <F4> Consultar  <ENTER> Modificar";
    COLOR COL(0, 0), COL(0, 1)
END SUB

