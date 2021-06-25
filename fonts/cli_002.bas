' ********************************************************************
'
' Fitxer....................: CLI_002.BAS
' Titol.....................: Modul per buscar clients
'
' ********************************************************************
'
' Data inici................: 27/06/1996
' Data de la darrera revisi¢: 20/12/1997 14:21:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB MASCCLIS ()
DECLARE SUB CercarClient (DIRECCP$, DIRECC$, DIRECI$, IMPRESORA!, DP$, DEV$)
DECLARE FUNCTION FindClient% (CAMP$, INDEX() AS ANY, MAXCL!)
DECLARE SUB SortClients (INDEX() AS ANY, MAXCL!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'

COMMON SHARED /INDEX/ INDEX() AS INDEXCLI
COMMON SHARED DIRECC$, DIRCP$, DEVI$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT, RNDX

DIM SHARED CEMPRE AS CONTROLEMP       '     "      DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAP€ALERAS DE DOCUMENTS
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED CLIENTS AS CLIENT
DIM SHARED CFG AS CONFIG
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
DIM SHARED COLORS AS COLO
DIM SHARED COL(2, 1)
DIM SHARED USR AS USUARIS
DIM SHARED PASO AS TRANS
DIM SHARED ANYS AS ANNO



'********************************************************************
'  Comprovaci¢ de la cridada del programa principal
'********************************************************************
	  TMP$ = ENVIRON$("TEMPORAL")
	  DEF SEG = VARSEG(CFG)
	  BLOAD TMP$ + "PASS.TMP", VARPTR(CFG)
	  DEF SEG

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

	  IF LTRIM$(RTRIM$(CA2$)) <> "CLI_002.EXE" THEN
	     tecla = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "Pitji una tecla...", 0)
	     SYSTEM
	  END IF

	  KILL TMP$ + "PASS.TMP"
	  KILL TMP$ + "PASU.TMP"
	  KILL TMP$ + "PASE.TMP"
	  KILL TMP$ + "PROT.TMP"



      UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
      DBF$ = LTRIM$(RTRIM$(CFG.DDADE))             ' Assignar direcctoris
      SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
      MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
      HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
      SYS$ = LTRIM$(RTRIM$(CFG.DSIST))

      DIRECCF$ = DBF$     ' Subdirecctori de les base de dades
      DIRECCP$ = SCR$ + "\"      ' Subdirecctori de les pantalles
      DIRECCR$ = MSK$ + "\"     ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = HLP$ + "\"    ' Subdirecctori de ajuda
      DIRECCI$ = UNIDAD$ + "\IMPRESOR\"  ' Subdirecctori de les impresores
      DIRECCT$ = DBF$ + "TEXTOS\"
      DIRECCH$ = DBF$ + "HISTORIC\"
      DP$ = DBF$

      MAXLINS = USR.MAXLINS
      MAXFAC = USR.LINALBA
      IVA = EMPRES.IVA
      DTO = EMPRES.DTO
      R = EMPRES.ANY
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA

      SetDirRecursos (DIRECCR$)

      ON ERROR GOTO ERRORS
      AREAC = FREEFILE: OPEN DIRECCF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
      GET AREAC, 1, COLORS
      COL(0, 0) = COLORS.COL(0, 0): COL(0, 1) = COLORS.COL(0, 1)
      COL(1, 0) = COLORS.COL(1, 0): COL(1, 1) = COLORS.COL(1, 1)
      COL(2, 0) = COLORS.COL(2, 0): COL(2, 1) = COLORS.COL(2, 1)
      CLOSE AREAC

      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, R, ANYS
      DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"
      CLOSE AREAA

      CALL CercarClient(DIRECCP$, DIRECCF$, DIRECCI$, IMPRESORA, DP$, DEV$)
      SYSTEM

ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

SUB CercarClient (DIRECCP$, DIRECC$, DIRECI$, IMPRESORA, DP$, DEV$) STATIC
    SHARED RNDX

    ON ERROR GOTO ERRORS
    GOSUB OBRIRFITXERS

    DIM INDEX(1 TO MAXCL) AS INDEXCLI
    DIM NDXCLIENT AS INDEXCLI
    DIM MENU(0 TO 6) AS STRING, MISS$(0 TO 5, 1)

    IF MAXCL = 1 THEN
       X = 7: R = 1: L = 1
       BEEP: tecla = Avis("AVIS:", "El fitxer de clients est… buit.", "Pitja una tecla...", 0)
       GOSUB TANCA
    END IF

    GOSUB INDEXAR
MENU:
  DO
    MENU(1) = " Pel telŠfon     ": MISS$(0, 1) = "Cercar un client pel seu telŠfon"
    MENU(2) = " Per direcci¢    ": MISS$(1, 1) = "Cercar un client per la seva direcci¢"
    MENU(3) = " Pels cognoms    ": MISS$(2, 1) = "Cercar un client pels seus cognoms"
    MENU(4) = " Pel DNI         ": MISS$(3, 1) = "Cercar un client pel seu D.N.I."
    MENU(5) = " Per la poblaci¢ ": MISS$(4, 1) = "Cercar un client per la seva poblaci¢"
    MENU(6) = " Per el nom      ": MISS$(5, 1) = "Cercar un client per el seu nom"

    CALL MENUBAR(MENU(), MISS$(), 1, CASO%, 5, 48, LEN(MENU(1)) + 1, 6, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
    SELECT CASE CASO%
	   CASE 1
		CAMP = 7: GOSUB PRESENTAR.MASCARA
	   CASE 2
		CAMP = 4: GOSUB PRESENTAR.MASCARA
	   CASE 3
		CAMP = 3: GOSUB PRESENTAR.MASCARA
	   CASE 4
		CAMP = 2: GOSUB PRESENTAR.MASCARA
	   CASE 5
		CAMP = 6: GOSUB PRESENTAR.MASCARA
	   CASE 6
		CAMP = 1: GOSUB PRESENTAR.MASCARA

	   CASE IS = 999, -4, -19
		GOSUB TANCA
	   CASE ELSE
		
    END SELECT
  LOOP
    
' MASCARA INICIAL D'UN CLIENT
PRESENTAR.MASCARA:
       GOSUB MASCARA
       FOR C = 0 TO 13: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
       GOSUB INITCAMPS: COLOR 15, 9
       GOSUB BUIDARCAMPS                 ' BUIDAR CAMPS
       GOSUB READCAMPS                   ' INTRODU‹R DADES AL
       SetMaxCamps 13
       GOSUB MOURECAMPS
       DisplayAllCamps
       C$ = INPUT$(1)
       RETURN
'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

READCAMPS:
       CORRECTE = 0: ANUL = 0: ACTIVAT = 0
       GOSUB INDEXAR: SetMaxCamps 13
		   DO
		     TROBAT = 0
		     SetMaxCamps CAMP: DeleteCamp CAMP
		     IF ReadCamp(CAMP) = SALIR THEN ANUL = 1: RETURN
		     CAMPTEMP$ = UCASE$(ValueCamp$(CAMP))

		     FOR R = 1 TO MAXCL
			 GET AREA3, R, CLIENTS
			 SELECT CASE CAMP    ' SELECCIONAR EL TIPUS DE BUSQUEDA
				CASE 1
				     IF INSTR(LTRIM$(RTRIM$(UCASE$(CLIENTS.NOM))), LTRIM$(RTRIM$(CAMPTEMP$))) THEN
					TROBAT = 999: RT = R: : ACTIVAT = 31
				     END IF
				CASE 2
				     IF UCASE$(CLIENTS.DNI) = CAMPTEMP$ THEN
					TROBAT = 999: RT = R: : ACTIVAT = 0
				     END IF
				CASE 3
				     IF INSTR(LTRIM$(RTRIM$(UCASE$(CLIENTS.COGNOMS))), LTRIM$(RTRIM$(CAMPTEMP$))) THEN
					TROBAT = 999: RT = R: ACTIVAT = 31
				     END IF
				CASE 4
				     IF INSTR(LTRIM$(RTRIM$(UCASE$(CLIENTS.DIRECCIO))), LTRIM$(RTRIM$(CAMPTEMP$))) THEN
					TROBAT = 999: RT = R: : ACTIVAT = 31
				     END IF
				CASE 6
				     IF UCASE$(CLIENTS.POBLACIO) = CAMPTEMP$ OR CLIENTS.TELEFON2 = CAMPTEMP$ THEN
					TROBAT = 999: RT = R: ACTIVAT = 31
				     END IF
				CASE 7
				     IF UCASE$(CLIENTS.TELEFON1) = CAMPTEMP$ OR CLIENTS.TELEFON2 = CAMPTEMP$ THEN
					TROBAT = 999: RT = R: ACTIVAT = 0
				     END IF
				CASE ELSE
			 END SELECT
		     NEXT
		   LOOP UNTIL TROBAT = 999
		   GET AREA3, RT, CLIENTS: RC = 1
		   COLOR COL(2, 0), COL(2, 1): LOCATE 5, 11: PRINT "Client No.: "; : COLOR 15: PRINT RT
		   IF ACTIVAT <> 0 THEN
		      FOR R = 1 TO MAXCL      ' SI HI HA MES D'UN CLIENT GENERA UNA LLISTA
			  SELECT CASE CAMP
				CASE 1
				     GET AREA3, R, CLIENTS
				     IF INSTR(LTRIM$(RTRIM$(UCASE$(CLIENTS.NOM))), LTRIM$(RTRIM$(CAMPTEMP$))) THEN
					PUT AREACOPIA, RC, CLIENTS
					RC = RC + 1
				     END IF
				CASE 3
				     GET AREA3, R, CLIENTS
				     IF INSTR(LTRIM$(RTRIM$(UCASE$(CLIENTS.COGNOMS))), LTRIM$(RTRIM$(CAMPTEMP$))) THEN
					PUT AREACOPIA, RC, CLIENTS
					RC = RC + 1
				     END IF
				CASE 4
				     GET AREA3, R, CLIENTS
				     IF INSTR(LTRIM$(RTRIM$(UCASE$(CLIENTS.DIRECCIO))), LTRIM$(RTRIM$(CAMPTEMP$))) THEN
					PUT AREACOPIA, RC, CLIENTS
					RC = RC + 1
				     END IF
				CASE 6
				     GET AREA3, R, CLIENTS
				     IF CAMPTEMP$ = UCASE$(CLIENTS.POBLACIO) THEN
					PUT AREACOPIA, RC, CLIENTS
					RC = RC + 1
				     END IF
				CASE ELSE
			  END SELECT
		      NEXT
		      MAX = RC - 1
		      IF MAX = 1 THEN
			 GET AREACOPIA, MAX, CLIENTS
			 RETURN
		      ELSE
			 MAX = RC - 1
			 GOSUB LLISTA
		      END IF
		   END IF
       RETURN

LLISTA:
    MASCCLIS                          ' PINTA LA MASCARA DE LA LLISTA
    X = 7: R = 1: L = 1
    '
    ' ********* MOSTRA ELS REGISTRES QUE CABIGEN EN UNA PANTALLA
    '
    GOSUB LISTA

    X = 7: R = 1                      ' INICIA CURSOR I POSICIONS A PANTALLA
    GET AREACOPIA, R, CLIENTS
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
    DO
      OP$ = INKEY$
      SELECT CASE OP$
	     CASE CHR$(0) + "P"              ' BAIXA UN REGISTRE
		  GOSUB BAIXA
	     CASE CHR$(0) + "H"              ' PUJA UN REGISTRE
		  GOSUB PUJA
	     CASE CHR$(27)
		  GOTO MENU
	     CASE CHR$(0) + CHR$(62)
		  GetBackground 1, 1, 25, 79, FACTU$
		  GOSUB MASCARA
		  SetMaxCamps 13
		  GOSUB MOURECAMPS
		  DisplayAllCamps
		  C$ = INPUT$(1)
		  PutBackground 1, 1, FACTU$
	     CASE CHR$(0) + "Q"
		  GOSUB BOTA.AVALL
	     CASE CHR$(0) + "I"
		  GOSUB BOTA.AMUNT
	     CASE ELSE
		  LOCATE , , 0
		  COLOR COL(0, 0), COL(0, 1): LOCATE 24, 60: PRINT "Registre: "; R; "  ";
      END SELECT
   LOOP
   RETURN

'************************************************************************
' Control del cursor per el llistat amb despla‡ament de barres
' funci¢ parescuda al DBEDIT de Clipper.
'************************************************************************

PUJA:
       IF X = 7 THEN
	  GET AREACOPIA, R, CLIENTS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = 1 THEN
	     X = 7: R = 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 7
	  SCROLLDOWN 18, 78, 6, 1, 1
	  GET AREACOPIA, R, CLIENTS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREACOPIA, R, CLIENTS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  GET AREACOPIA, R, CLIENTS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXA:
       IF X = 19 THEN
	  GET AREACOPIA, R, CLIENTS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAX THEN
	     R = MAX: X = 19
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = 19
	  SCROLLUP 18, 78, 6, 1, 1
	  GET AREACOPIA, R, CLIENTS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREACOPIA, R, CLIENTS
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAX THEN
	     R = MAX
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  GET AREACOPIA, R, CLIENTS
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BOTA.AVALL:
   IF R = MAX THEN RETURN
   GOSUB ANAR.TOPE
   S = 1
   DO
	S = S + 1
	IF RB + S > MAX THEN
	   WHILE (RB + S > MAX)
		 S = S - 1
	   WEND
	   EXIT DO
	END IF
   LOOP UNTIL S = 13
   RB = RB + S: R = RB: RESTA = 0: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREACOPIA, R, CLIENTS: X = XB
   COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
   R = RB
   RETURN

BOTA.AMUNT:
   IF R = 1 THEN RETURN
   GOSUB ANAR.TOPE
   S = 1
   DO
	S = S + 1
	IF RB - S < 1 THEN
	   WHILE (RB - S < 1)
		 S = S - 1
	   WEND
	   EXIT DO
	END IF
   LOOP UNTIL S = 13

   RB = RB - S: R = RB: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREACOPIA, R, CLIENTS: X = XB
   COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
   R = RB

   RETURN

ANAR.TOPE:
   XB = X: RB = R         ' Guarda l'estat actual
   DO UNTIL XB = 7
       XB = XB - 1        ' Resta l'estat actual fins arribar al tope del recuadre
       RB = RB - 1
   LOOP
   RETURN

'************************************************************************
' Mostra el cursor a la posici¢ que est…
'************************************************************************

SHOWCURSOR:
	  N$ = LTRIM$(RTRIM$(CLIENTS.NOM))
	  COG$ = CLIENTS.COGNOMS
	  NC$ = N$ + " " + COG$
	  LOCATE X, 2: PRINT USING "\         \ \" + SPACE$(30) + "\ \                 \"; CLIENTS.CODICLIENT; NC$; CLIENTS.POBLACIO; "                               "
	  RETURN
'************************************************************************
' INICIALITZA CAMPS
'************************************************************************
INITCAMPS:
	  FOR C = 0 TO 13: DeleteCamp C: NEXT
	  SetInitCamp 0, 7, 29, ASCI, 0, "XXXXXXXXXX", ""
	  SetInitCamp 1, 8, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 2, 8, 58, ASCI, 0, "XXXXXXXXXXX", ""
	  SetInitCamp 3, 9, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 4, 11, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 5, 12, 29, ASCI, 0, "XXXXXXX", ""
	  SetInitCamp 6, 12, 48, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 7, 13, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 8, 14, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 9, 15, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 10, 17, 29, NUM, 0, "999", ""
	  SetInitCamp 11, 17, 58, ASCI, 0, "XXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 12, 18, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 13, 19, 29, ASCI, 0, "XXXXXXXXXX", ""
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPS:
	   InsertValueCamp 0, CLIENTS.CODICLIENT
	   InsertValueCamp 1, CLIENTS.NOM
	   InsertValueCamp 2, CLIENTS.DNI
	   InsertValueCamp 3, CLIENTS.COGNOMS
	   InsertValueCamp 4, CLIENTS.DIRECCIO
	   InsertValueCamp 5, CLIENTS.CPOSTAL
	   InsertValueCamp 6, CLIENTS.POBLACIO
	   InsertValueCamp 7, CLIENTS.TELEFON1
	   InsertValueCamp 8, CLIENTS.TELEFON2
	   InsertValueCamp 9, CLIENTS.PROVINCIA
	   InsertValueCamp 10, LTRIM$(STR$(CLIENTS.DTO))
	   InsertValueCamp 11, CLIENTS.FORMAPAGO
	   InsertValueCamp 12, CLIENTS.BANC
	   InsertValueCamp 13, CLIENTS.COMPTE
	  RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXER:
	   CLIENTS.CODICLIENT = RTRIM$(ValueCamp(0))
	   CLIENTS.NOM = ValueCamp$(1)
	   CLIENTS.DNI = ValueCamp$(2)
	   CLIENTS.COGNOMS = ValueCamp$(3)
	   CLIENTS.DIRECCIO = ValueCamp$(4)
	   CLIENTS.CPOSTAL = ValueCamp$(5)
	   CLIENTS.POBLACIO = ValueCamp$(6)
	   CLIENTS.TELEFON1 = ValueCamp$(7)
	   CLIENTS.TELEFON2 = ValueCamp$(8)
	   CLIENTS.PROVINCIA = ValueCamp$(9)
	   CLIENTS.DTO = VAL(ValueCamp$(10))
	   CLIENTS.FORMAPAGO = ValueCamp$(11)
	   CLIENTS.BANC = ValueCamp$(12)
	   CLIENTS.COMPTE = ValueCamp$(13)
	   CLIENTS.MARCAT = " "
	   RETURN

'************************************************************************
' BUIDAR ELS CAMPS DEL FITXER
'************************************************************************
BUIDARCAMPS:
	   CLIENTS.CODICLIENT = STRING$(LEN(CLIENTS.CODICLIENT), CHR$(32))
	   CLIENTS.NOM = ""
	   CLIENTS.DNI = ""
	   CLIENTS.COGNOMS = ""
	   CLIENTS.DIRECCIO = ""
	   CLIENTS.CPOSTAL = ""
	   CLIENTS.POBLACIO = ""
	   CLIENTS.TELEFON1 = ""
	   CLIENTS.TELEFON2 = ""
	   CLIENTS.PROVINCIA = ""
	   CLIENTS.DTO = 0
	   CLIENTS.FORMAPAGO = ""
	   CLIENTS.BANC = ""
	   CLIENTS.COMPTE = ""
	   CLIENTS.MARCAT = " "
	   FOR C = 0 TO 13: DeleteCamp C: NEXT
	   RETURN

'************************************************************************
' FITXA DE UN CLIENT
'************************************************************************

MASCARA:
	  COLOR COL(0, 0), COL(0, 1)
	  FINESTRA 4, 10, 20, 76, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
	  LOCATE 5, 11: PRINT "Client No.: "; : COLOR COL(0, 0), COL(0, 1): PRINT RV
	  LOCATE 6, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0), COL(2, 1)
	  LOCATE 7, 11: PRINT "Codi client......:"
	  LOCATE 8, 11: PRINT "Nom del client...:" + SPACE$(20) + " D.N.I..:"
	  LOCATE 9, 11: PRINT "Cognoms o mal nom:"
	  COLOR COL(0, 0), COL(0, 1): LOCATE 10, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0), COL(2, 1)
	  LOCATE 11, 11: PRINT "Direcci¢.........:"
	  LOCATE 12, 11: PRINT "Codi Postal......:         Poblaci¢.:"
	  LOCATE 13, 11: PRINT "TelŠfon 1........:"
	  LOCATE 14, 11: PRINT "TelŠfon 2 o FAX..:"
	  LOCATE 15, 11: PRINT "Provincia........:"
	  COLOR COL(0, 0), COL(0, 1): LOCATE 16, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0), COL(2, 1)
	  LOCATE 17, 11: PRINT "Descompte %......:" + SPACE$(11) + "Forma de pagament:"
	  LOCATE 18, 11: PRINT "Banc/Sucursal....:"
	  LOCATE 19, 11: PRINT "Compte...........:"
	  RETURN

'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE CLIENTS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "CLIENTS.NDX" FOR RANDOM SHARED AS AREA2 LEN = LEN(NDXCLIENT)
      AREA3 = FREEFILE: OPEN DIRECC$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA3 LEN = LEN(CLIENTS)
      AREACOPIA = FREEFILE: OPEN DIRECC$ + "CLIENTS.BCK" FOR RANDOM SHARED AS AREACOPIA LEN = LEN(CLIENTS)
      AREA4 = FREEFILE: OPEN DP$ + "PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CAP)

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
      MAXCL = CEMPRE.MAXCLIENTS
      RETURN

LISTA:
    COLOR COL(0, 0), COL(0, 1): X = 7
    FOR X = 7 TO 19
       IF R >= MAX + 1 THEN EXIT FOR
       GET AREACOPIA, R, CLIENTS: COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
       R = R + 1
    NEXT
    RETURN


INDEXAR:
      FOR RI = 1 TO MAXCL                   ' COL.LOCAR A MEM•RIA L'INDEX I LA CLAU
	  GET AREA2, RI, NDXCLIENT
	  INDEX(RI).REGISTRE = NDXCLIENT.REGISTRE
	  INDEX(RI).CODI = NDXCLIENT.CODI
      NEXT
      RETURN

TANCA:
      FOR C = 0 TO 13: DeleteCamp C: NEXT
      RESET
      KILL DIRECC$ + "CLIENTS.BCK"
      EXIT SUB

END SUB

FUNCTION FindClient% (CAMP$, INDEX() AS INDEXCLI, MAXCL) STATIC
	 SHARED RNDX
	 TOPRECORD = MAXCL - 1
	 BottomRecord = 1

	 DO UNTIL (TOPRECORD < BottomRecord)

	    MIDPOINT = (TOPRECORD + BottomRecord) \ 2

	    TEST$ = RTRIM$(ForaEspai$(INDEX(MIDPOINT).CODI))

	    IF TEST$ = CAMP$ THEN
	       EXIT DO
	    ELSEIF CAMP$ > TEST$ THEN
	       BottomRecord = MIDPOINT + 1
	    ELSE
	       TOPRECORD = MIDPOINT - 1
	    END IF
	 LOOP

	 IF TEST$ = CAMP$ THEN
	    GET AREA3, INDEX(MIDPOINT).REGISTRE, CLIENTS
	    FindClient% = TRUE
	    RNDX = MIDPOINT
	 ELSE
	    FindClient% = FALSE
	 END IF
END FUNCTION

SUB MASCCLIS
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 4, 1, 20, 80, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
    LOCATE 5, 2: PRINT "Codi      Nom Client" + SPACE$(23) + "Poblaci¢": COLOR COL(0, 0), COL(0, 1)
    LOCATE 6, 2: PRINT STRING$(78, "Ä");
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<" + CHR$(25) + ">=BAIXAR     <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR   <F4>=CONSULTAR"
    LOCATE 24, 2: PRINT "<F6>=LLISTAR";
END SUB

