' ********************************************************************
'
' Fitxer....................: ALT_FACT.BAS
' Titol.....................: Modul per el mateniment de factures
'
' ********************************************************************
'
' Data inici................: 22/09/1996 23:30:00
' Data de la darrera revisi¢: 22/02/1998 21:58:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/98 (C)
' Codi......................: MS-BASIC 7.01 (PDS)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB CrearAlbarans (MAX!, DTO!, IVA!, AREANUM!, AREA3!, AREA2!, AREA4!, AREAST!, MAXST!, MAXAL!, EDIT!, DH$, DEVI$, AREA2T!, AREA3T!)
DECLARE SUB ImprimirFactura (DEVI$, MAXALBS!, AREA5!, AREA3T!, AREA2T!)
DECLARE SUB AlbaransEdit (MAX!, DTO!, IVA!, DIRECC$, DIRCP$, DIRECI$, DH$, DIRPLA$, DEVI$, IMPRESORA!)
DECLARE SUB GuardaAlbaran (RG!, MAXLIN!, AREANUM!, TIPUS%, GUARDAT!, EDIT!)
DECLARE FUNCTION INICIAREF! (AREANUM!, R!, MARCAT!, REFOLD$)
DECLARE SUB ReadCalculFactura ()
DECLARE SUB LLISTARALBARAN (AREA5!, AREA2!, DEVI$)
DECLARE SUB SUMARALBARANS (MAXAL!, AREA2!)
DECLARE SUB MASCALBA (MAX!)
DECLARE SUB MASCALBS ()
DECLARE SUB MASCLIST ()
DECLARE SUB CARREGARALBARAN (R!, MAXLIN!)
DECLARE SUB OrdenarIndex (INDEX() AS ANY, MAXST!)
DECLARE SUB INITALBARAN (MAXLIN!)
DECLARE SUB READCAPSALERA (MAXAL!, MAXCL!)
DECLARE SUB REPINTACAPSALERA (MAX!)
DECLARE SUB ReadObserva ()
DECLARE FUNCTION CercarRecord% (CAMP$, INDEX() AS ANY, MAXST!, AREAST!)

COMMON SHARED DIRECC$, DIRECCP$, DIRECCT$, DOCU$, DIRECCH$, CADFE$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5, AREANDX
COMMON SHARED GUARDAT, MIDPOINT, MAXCL, MAXAL, R, EDIT
COMMON SHARED TOTAL, MAX, nou, TROBAT, DEVICE$, DEVI$, UNIDAD$, ANY$
COMMON SHARED MAXALBS, col()

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STOCK.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'

'$DYNAMIC
DIM SHARED FLD(6) AS CN           ' CONFIGURACI¢ DE CAMPS
DIM SHARED MASC$(6)               ' CONFIGURACI¢ DE MASCARES
DIM SHARED LIN$(100, 8)           ' LINIES DE LA FACTURA TEMPORAL
DIM SHARED OBS$(1)                ' LINIES D'OBSERVACIO
DIM SHARED col(2, 1)
DIM SHARED CLIENT AS CLIENT
DIM SHARED CEMPRE AS CONTROLEMP
DIM SHARED ALBARAN AS ALBAR
DIM SHARED FACTURA AS FACTU
DIM SHARED LINFACT AS LINIAFACT
DIM SHARED LINALBA AS LINIES
DIM SHARED TEXTOSMENUP(6) AS STRING
DIM SHARED COMMENT$(7, 9)
DIM SHARED STOCK AS STK
DIM SHARED DOCNUM AS DN
DIM SHARED TD AS TIPUSDOC
DIM SHARED CAP AS CAPSALDOCS
DIM SHARED SPOOLIMP AS SPOOL
DIM SHARED ARCHIMP AS IMPRESORA
DIM SHARED CFG AS CONFIG
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
DIM SHARED USR AS USUARIS
DIM COLORS AS COLO
DIM SHARED PASO AS TRANS
DIM SHARED ANYS AS ANNO

	  TMP$ = ENVIRON$("TEMPORAL")
	  IF DIR$(TMP$ + "PASS.TMP") = "" THEN
	     BEEP: PRINT "ERROR: Al inicialitzar el m•dul de transpass 1"
	     SYSTEM
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

	  IF LTRIM$(RTRIM$(CA2$)) <> "ALT_FACT.EXE" THEN
	     TELCA = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "PITJA UNA TECLA...", 0)
	     END
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

      DIRECCF$ = DBF$       ' Subdirecctori de les base de dades
      DIRECCP$ = SCR$       ' Subdirecctori de les pantalles
      DIRECCR$ = MSK$     ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = HLP$     ' Subdirecctori de ajuda
      DIRECCI$ = UNIDAD$ + "\IMPRESOR\"    ' Subdirecctori de les impresores
      DIRECCT$ = DBF$ + "TEXTOS\"
      DIRECCH$ = DBF$ + "HISTORIC\"
      DIRECCPL$ = DBF$ + "PLANTILL\"

      MAXLINS = USR.MAXLINS
      MAXFAC = USR.LINFACT
      IVA = EMPRES.IVA
      DTO = EMPRES.DTO
      R = EMPRES.ANY
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA
      SetDirRecursos (DIRECCR$)
      SetFormatCC (34)

      AREAC = FREEFILE: OPEN DIRECCF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
      GET AREAC, 1, COLORS
      col(0, 0) = COLORS.col(0, 0): col(0, 1) = COLORS.col(0, 1)
      col(1, 0) = COLORS.col(1, 0): col(1, 1) = COLORS.col(1, 1)
      col(2, 0) = COLORS.col(2, 0): col(2, 1) = COLORS.col(2, 1)
      CLOSE AREAC

      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, R, ANYS
      DIRECCF$ = DIRECCF$ + ANYS.ANY + "\": ANY$ = MID$(ANYS.ANY, 3, 2)
      CLOSE AREAA

      CALL AlbaransEdit(MAXFAC, INT(EMPRES.DTO), INT(EMPRES.IVA), DIRECCF$, DBF$, DIRECCI$, DIRECCH$, DIRECCPL$, DEV$, IMPRESORA)
      SYSTEM


ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

REM $STATIC
SUB AlbaransEdit (MAX, DTO, IVA, DIRECC$, DIRCP$, DIRECI$, DH$, DIRPLA$, DEVI$, IMPRESORA)
    SHARED CADFE$
    ON ERROR GOTO ERRORS
    GOSUB OBRIFITXERS
    DIM NDXFILE AS INDEXTYPE

    DIRECCT$ = DIRCP$ + "TEXTOS\"
    SELECT CASE COMMAND$
	   CASE "AUTOMATIC"
		CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXST, MAXAL, 0, DH$, DEVI$, AREA2T, AREA3T)
		RESET: EXIT SUB
	   CASE "MANTENIMENT"
		GOSUB DBEDIT
	   CASE ELSE
		PRINT "Smart-Factur 2.0": PRINT
		PRINT "Falten par…metres --> ALT_FACT (AUTOMATIC|MANTENIMENT)": PRINT
		PRINT "AUTOMATIC   = Facturaci¢ dirŠcte"
		PRINT "MANTENIMENT = Manteniment de factures": PRINT
		EXIT SUB
    END SELECT
		
DBEDIT:
    MASCALBS                          ' PINTA LA MASCARA DE LA LLISTA
    SetScoreBoard SON                 ' ACTIVA EL VISOR D'ESTAT DE LES TECLES

    RESTA = 0
    IF MAXAL > 11 THEN
       R = MAXAL - 5: L = 1: X = 5
       GOSUB LISTA
       X = 5 + 4: R = MAXAL - 1                  ' INICIA CURSOR I POSICIONS A PANTALLA
    ELSE
       X = 5: R = 1: GOSUB LISTA: X = 5: R = 1
    END IF

    GET AREA2, R, FACTURA
    COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
    DO
      OP$ = INKEY$
      COLOR col(0, 0), col(0, 1): CALL EstatTeclesControl(25, 3)
      SELECT CASE OP$
	     CASE CHR$(0) + "P"      ' BAIXA UN REGISTRE
		  GOSUB BAIXACURSOR
	     CASE CHR$(0) + "H"      ' PUJA UN REGISTRE
		  GOSUB PUJACURSOR
	     CASE CHR$(27)
		  SetScoreBoard SOFF: RESET
		  EXIT SUB
	     CASE CHR$(13)
		  XOLD = X: ROLD = R
		  GetBackground 1, 1, 24, 80, L$
		  CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXST, MAXAL, 1, DH$, DEVI$, AREA2T, AREA3T)
		  MASCALBS
		  PutBackground 1, 1, L$
		  X = XOLD: R = ROLD: GET AREA2, R, FACTURA: COLOR col(1, 0), col(1, 1): : GOSUB SHOWCURSOR
	     CASE CHR$(0) + CHR$(60)
		  nou = 0
		  GetBackground 1, 1, 24, 80, L$
		  XOLD = X: ROLD = R
		  CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXST, MAXAL, 0, DH$, DEVI$, AREA2T, AREA3T)
		  MASCALBS
		  X = XOLD: R = ROLD: GET AREA2, R, FACTURA: COLOR col(1, 0), col(1, 1): : GOSUB SHOWCURSOR
		  PutBackground 1, 1, L$
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
		  CALL ImprimirFactura(DEVI$, MAXALBS, AREA5, AREA3T, AREA2T)
	     CASE CHR$(0) + "Q"
		  GOSUB BOTA.AVALL
	     CASE CHR$(0) + "I"
		  GOSUB BOTA.AMUNT
		  GET AREA2, R, FACTURA: COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
	     CASE ELSE
      END SELECT
   LOOP
   RETURN

BOTA.AVALL:
   IF R = MAXAL - 1 THEN RETURN
   GOSUB ANAR.TOPE
   S = 1
   DO
	S = S + 1
	IF RB + S > MAXAL - 1 THEN
	   WHILE (RB + S > MAXAL - 1)
		 S = S - 1
	   WEND
	   EXIT DO
	END IF
   LOOP UNTIL S = 13
   RB = RB + S: R = RB: RESTA = 0: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREA2, RB, FACTURA: X = XB
   COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
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
   GET AREA2, RB, FACTURA: X = XB: COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
   R = RB
   RETURN

ANAR.TOPE:
   XB = X: RB = R         ' Guarda l'estat actual
   DO UNTIL XB = 5
       XB = XB - 1        ' Resta l'estat actual fins arribar al tope del recuadre
       RB = RB - 1
   LOOP
   RETURN

'************************************************************************
' Control del cursor per el llistat amb despla‡ament de barres
' funci¢ parescuda al DBEDIT de Clipper.
'************************************************************************

PUJACURSOR:
       IF X = 5 THEN
	  GET AREA2, R, FACTURA
	  COLOR col(0, 0), col(0, 1): GOSUB SHOWCURSOR
	  IF R = 1 THEN
	     X = 5: R = 1
	     COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 5
	  ScrollDown 18, 78, 4, 1, 1
	  GET AREA2, R, FACTURA
	  COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA2, R, FACTURA
	  COLOR col(0, 0), col(0, 1): GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  GET AREA2, R, FACTURA
	  COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXACURSOR:
       IF X = 19 THEN
	  GET AREA2, R, FACTURA
	  COLOR col(0, 0), col(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXAL - 1 THEN
	     R = MAXAL - 1
	     X = 19
	     COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = 19
	  ScrollUp 18, 78, 4, 1, 1
	  GET AREA2, R, FACTURA
	  COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA2, R, FACTURA
	  COLOR col(0, 0), col(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXAL - 1 THEN
	     R = MAXAL - 1
	     COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  GET AREA2, R, FACTURA
	  COLOR col(1, 0), col(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

'************************************************************************
' Mostra el cursor a la posici¢ que est…
'************************************************************************

SHOWCURSOR:
	  IF FACTURA.MARCAT = "*" THEN COLOR col(0, 0) XOR col(0, 1)
	  NOMS$ = SPACE$(27): NOMA$ = RTRIM$(LTRIM$(FACTURA.PERSONA.COGNOMS)) + "; " + LTRIM$(RTRIM$(FACTURA.PERSONA.NOM))
	  LN = LEN(NOMA$): IF LN > 27 THEN LN = 27
	  MID$(NOMS$, 1, LN) = NOMA$
	  LOCATE X, 2: PRINT FACTURA.REFFACTURA; " º"; NOMS$; "º";
	  TOB$ = LTRIM$(FormatD$(FACTURA.TOTALBRUT, "##.###.###")): TOTB$ = SPACE$(10 - LEN(TOB$)) + TOB$
	  TON$ = LTRIM$(FormatD$(FACTURA.TOTALNET, "##.###.###")): TOTN$ = SPACE$(10 - LEN(TON$)) + TON$
	  PRINT USING "   \        \º ####º ####º   \        \"; TOTB$; FACTURA.DTO; FACTURA.TIPOIVA; TOTN$
	  RETURN

LISTA:
    IF MAXAL = 1 THEN
       nou = 2
       Avis.Sonor (1)
       TECLA = Avis("AVIS:", "No hi ha cap factura creada, ara es procedir… a crear-ne una", "PITJA UNA TECLA...", 0)
       CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXST, MAXAL, 0, DH$, DEVI$, AREA2T, AREA3T)
       IF nou = 9 THEN RESET: EXIT SUB
       MASCALBS
       X = 5: R = 1: GOSUB LISTA: COLCURS = col(0, 1): GOSUB SHOWCURSOR
    END IF
    COLOR col(0, 0), col(0, 1)
    FOR X = 5 TO 19: LOCATE X, 2: PRINT "          º                           º             º     º     º             ": NEXT
    COLOR col(0, 0), col(0, 1)
    IF RESTA = 0 THEN a = 5: b = 19: C = 1 ELSE a = 19: b = 5: C = -1
    FOR X = a TO b STEP C
       IF R >= MAXAL OR R < 1 THEN EXIT FOR
       GET AREA2, R, FACTURA
       GOSUB SHOWCURSOR
       IF RESTA = 0 THEN R = R + 1 ELSE R = R - 1
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
    COLCURS = col(1, 1): GOSUB SHOWCURSOR
    RETURN

OBRIFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "FACTURES.CAB" FOR RANDOM SHARED AS AREA2 LEN = LEN(FACTURA)
      AREA3 = FREEFILE: OPEN DIRECC$ + "FACTURES.LIN" FOR RANDOM SHARED AS AREA3 LEN = LEN(LINFACT)
      AREA2T = FREEFILE: OPEN DIRECC$ + "ALBARAN.CAB" FOR RANDOM SHARED AS AREA2T LEN = LEN(ALBARAN)
      AREA3T = FREEFILE: OPEN DIRECC$ + "ALBARAN.LIN" FOR RANDOM SHARED AS AREA3T LEN = LEN(LINALBA)
      AREA4 = FREEFILE: OPEN DIRECC$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CLIENT)
      AREA5 = FREEFILE: OPEN DIRPLA$ + "CAP€ALER.DAT" FOR RANDOM SHARED AS AREA5 LEN = LEN(CAP)
      AREAST = FREEFILE: OPEN DIRECC$ + "STOCK.DAT" FOR RANDOM SHARED AS AREAST LEN = LEN(STOCK)
      AREANDX = FREEFILE: OPEN DIRECC$ + "STOCK.NDX" FOR RANDOM SHARED AS AREANDX LEN = LEN(NDXFILE)
      AREANUM = FREEFILE: OPEN DIRECC$ + "DOCUMENT.NUM" FOR RANDOM SHARED AS AREANUM LEN = LEN(DOCNUM)

      IF DIR$(DIRPLA$ + "FESTES.DAT") <> "" THEN
	 AREAFE = FREEFILE: OPEN DIRPLA$ + "FESTES.DAT" FOR INPUT AS AREAFE
	 INPUT #AREAFE, CADFE$
	 CLOSE AREAFE
      END IF
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

      MAXALBS = CEMPRE.MAXALBARAN
      MAXAL = CEMPRE.MAXFACTURA
      MAXCL = CEMPRE.MAXCLIENTS
      MAXST = CEMPRE.MAXSTOCK

      AREADOC = FREEFILE: OPEN DIRPLA$ + "FAC_MSK.LST" FOR RANDOM SHARED AS AREADOC LEN = LEN(TD)

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
	LIN$(J, 8) = LINFACT.LINIA(J).REFALBARAN
    NEXT
    GET AREA2, R, FACTURA
END SUB

FUNCTION CercarRecord% (CAMP$, INDEX() AS INDEXTYPE, MAXST, AREAST) STATIC
	 SHARED MIDPOINT

	 TOPRECORD = MAXST - 1
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
	    GET AREAST, INDEX(MIDPOINT).REGISTRE, STOCK
	    CercarRecord% = TRUE
	 ELSE
	    CercarRecord% = FALSE
	 END IF
END FUNCTION

SUB CrearAlbarans (MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXST, MAXAL, EDIT, DH$, DEVI$, AREA2T, AREA3T)
       SHARED LIN$(), TEXTOSMENUP() AS STRING
       SHARED UNIDAD$, DIRECCH$
      
       ON ERROR GOTO ERRORS
       DIM MA$(2)
'*********************************************************************
' INDEXAR FITXER DEL STOCK
'*********************************************************************
       GetBackground 10, 30, 15, 48, Bt$
       COLOR col(0, 0), col(0, 1): FINESTRA 10, 30, 14, 47, 1, CAIXA1
       COLOR 31, col(0, 1): LOCATE 11, 31: PRINT "ESPERA UN MOMENT"
       COLOR 31, col(0, 1): LOCATE 13, 31: PRINT " PROCESSANT ... "
       DIM INDEX(1 TO MAXST) AS INDEXTYPE
       DIM M$(2)
       DIM NDXFILE AS INDEXTYPE
       GOSUB CARREGARINDEX
       PutBackground 10, 30, Bt$

'*********************************************************************

       FLD(1).XCAMP = 2: FLD(1).LON = 18: FLD(1).TIPUS = ASCI: FLD(1).MASK = STRING$(18, "X")
       FLD(2).XCAMP = 21: FLD(2).LON = 40: FLD(2).TIPUS = ASCI: FLD(2).MASK = STRING$(40, "X")
       FLD(3).XCAMP = 51: FLD(3).LON = 9: FLD(3).TIPUS = num: FLD(3).MASK = "999999999"
       FLD(4).XCAMP = 61: FLD(4).LON = 8: FLD(4).TIPUS = num: FLD(4).MASK = "99999.99"

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
	  IF nou = 9 THEN EXIT SUB
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
		       TEXTOSMENUP(1) = " MODIFICAR ~CAP€ALERA    "
		       TEXTOSMENUP(2) = " MODIFICAR ~LINIES       "
		       TEXTOSMENUP(3) = " MODIFICAR ~OBSERVACIONS "
		       TEXTOSMENUP(4) = " ~IMPRIMIR FACTURA       "
		       TEXTOSMENUP(5) = " ~GUARDAR FACTURA        "
		       COLOR col(1, 1), CCT
		       CALL MenuBar(TEXTOSMENUP(), COMMENT$(), 0, CASO%, 17, 23, LEN(TEXTOSMENUP(1)) + 2, 5, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
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
				   CALL ImprimirFactura(DEVI$, MAXALBS, AREA5, AREA3T, AREA2T)
			      CASE 5
				   IF EDIT = 1 THEN
				      RG = Q
				   ELSE
				      RG = MAXAL: IF TROBAT$ = "Z" THEN RG = Q
				   END IF
				   CALL GuardaAlbaran(RG, MAX, AREANUM, TIPUS%, GUARDAT, EDIT)
			      CASE 999
				   IF nou = 2 THEN
				      ERASE INDEX: nou = 9
				      EXIT SUB
				   ELSE
				      IF nou = 3 THEN
					 ERASE INDEX
					 GOSUB GUARDADAR.NUMERO.ALBARA
					 PutBackground 1, 1, fact$
					 EXIT SUB
				      END IF
				   END IF
				   ERASE INDEX
				   GOSUB GUARDADAR.NUMERO.ALBARA
			      CASE ELSE
		       END SELECT
		     LOOP
		CASE CHR$(27)
		     IF nou = 2 THEN        ' SI ES EL PRIMER FACTURA QUE ES FA
			ERASE INDEX: nou = 9
			EXIT SUB
		     ELSE
			ERASE INDEX
			IF nou = 3 THEN EXIT SUB ELSE EXIT SUB
		     END IF
		CASE ELSE
		     COLOR col(2, 0), col(0, 1): LOCATE 20, 2: PRINT "Linia: "; : COLOR col(2, 0) XOR col(2, 1): PRINT L;
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
	     COLOR col(1, 1), CCT: LOCATE YC, X: PRINT STRING$(LE, " ");
	 NEXT
	 
	 IF LIN$(L, 6) <> "-" THEN
	    LIN$(L, 6) = "*"
	 END IF
	 LIN$(L, 5) = ""
	 IF LIN$(1, 6) = "-" THEN LIN$(1, 6) = "*"

	 LOCATE YC, 61: PRINT "        "
	 LOCATE YC, 70: PRINT "          "
	 COLOR col(0, 0) XOR col(0, 1), col(0, 1)
	 LOCATE 20, 67: PRINT USING MASCTOTAL$; TOTAL
	 GOSUB MOSTRA
	 GOSUB MARCACAMP
	 RETURN

PUJA:
	 IF L = 1 THEN SOUND 50, .5: TECLA = Avis("AVIS:", "Principi de la factura", "PITJA UNA TECLA...", 0): RETURN
	 X = FLD(C).XCAMP: LE = FLD(C).LON: T = FLD(C).TIPUS
	 COLOR col(0, 0), col(0, 1): LOCATE YC, X: PRINT STRING$(LE, " ");
	 LOCATE YC, X, 0: PRINT LIN$(L, C)
	 C = 1: YC = YC - 1: L = L - 1
	 LOCATE 20, 20: PRINT "       ";

	 IF YC < 9 THEN
	    ScrollDown 17, 78, 8, 1, 1
	    YC = 9
	    GOSUB MOSTRA
	    LOCATE 20, 20: PRINT "  AMUNT";
	 END IF
	 
	 GOSUB MOSTRA
	 GOSUB MARCACAMP
	 RETURN

BAIXA:
	 IF L = MAX THEN SOUND 50, .5: TECLA = Avis("AVIS:", "Final de la factura", "PITJA UNA TECLA...", 0): RETURN
	 X = FLD(C).XCAMP: LE = FLD(C).LON: T = FLD(C).TIPUS
	 COLOR col(0, 0), col(0, 1): LOCATE YC, X: PRINT STRING$(LE, " ");
	 LOCATE YC, X: PRINT LIN$(L, C)
	 LOCATE YC, X, 0
	 C = 1: YC = YC + 1: L = L + 1
	 LOCATE 20, 20: PRINT "       ";
	 IF YC > 18 THEN
	    ScrollUp 17, 78, 8, 1, 1
	    YC = 18
	    GOSUB MOSTRA
	    LOCATE 20, 20: PRINT "  AVALL";
	 END IF
	 GOSUB MOSTRA
	 GOSUB MARCACAMP
	 RETURN

MOSTRA:
	 COLOR col(0, 0), col(0, 1)
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
	 COLOR col(1, 0), col(1, 1): LOCATE YC, X: PRINT STRING$(LE, " ");
	 LOCATE YC, X: PRINT LIN$(L, C)
	 LOCATE YC, X, 0
	 RETURN

'*********************************************************************
' RUTINA PER LLEGIR ELS CAMPS D'UNA LINIA
'*********************************************************************

LLEGEIXCAMP:
     COLOR col(0, 0), col(0, 1)
     LOCATE 23, 2: PRINT SPACE$(70);
     LOCATE 24, 2: PRINT SPACE$(70);
     LOCATE 22, 2: PRINT SPACE$(70);
     IF LIN$(L, 6) <> "*" AND LIN$(L, 6) <> "-" THEN
	LOCATE 23, 2: PRINT "VES PER ORDRE !!!": BEEP
	LOCATE 23, 2: PRINT "ENTER=EDITAR LINIA    ESC=ANULAR    SUPR=SUPRIMIR LINIA";
	LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR    CTRL+F=ACABAR DE FACTURAR";
	RETURN
     END IF
     LOCATE 23, 2: PRINT "<F2>=LLISTA DE L'STOCK      <ESC>=SORTIR"
     SALE = 0
     TROBAT = 0
     GetBackground 10, 10, 25, 71, LIS$
     FINESTRA 10, 10, 22, 66, 1, CAIXA1

     SetMaxCamps 4
     SetInitCamp 0, 12, 26, ASCI, 0, LTRIM$(RTRIM$(FLD(1).MASK)), "Codi Article:"
     SetInitCamp 1, 14, 26, ASCI, 0, LTRIM$(RTRIM$(FLD(2).MASK)), "Descripci¢:"
     SetInitCamp 2, 16, 26, ASCI, 0, LTRIM$(RTRIM$(FLD(3).MASK)), "Preu Unitari:"
     SetInitCamp 3, 17, 26, ASCI, 0, LTRIM$(RTRIM$(FLD(4).MASK)), "Quantitat:"
     SetInitCamp 4, 18, 26, ASCI, 0, "999", "Dto %:"

     FOR C = 0 TO 4: SetColorCamp C, col(1, 0), col(1, 1), col(0, 0), col(0, 1), col(2, 0), col(2, 1): NEXT
     FOR C = 0 TO 3: InsertValueCamp C, LIN$(L, C + 1): VELL$ = LIN$(L, C + 1): NEXT
     InsertValueCamp 4, LIN$(L, 7)
     DisplayAllCamps
     FOR C = 0 TO 4
	 InsertValueCamp C, LIN$(L, C + 1): VELL$ = LIN$(L, C + 1)
	 VALUE = ReadCamp(C)
	 SELECT CASE VALUE
		CASE F2
		     IF C = 0 OR C = 1 THEN GOSUB LLISTA.STOCK
		     C = C - 1
		CASE F3 TO F10
		     SOUND 50, .5
		CASE SALIR
		     C = 1
		     LOCATE 23, 2: PRINT "ENTER=EDITAR LINIA    ESC=ANULAR    SUPR=SUPRIMIR LINIA";
		     LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR    CTRL+F=ACABAR L'ALBAR…";
		     PutBackground 10, 10, LIS$
		     GOSUB MOSTRA: X = FLD(1).XCAMP
		     RETURN
		CASE 0
		     CAMPTEMP$ = ForaEspai$(ValueCamp$(0))
		     IF CercarRecord%(CAMPTEMP$, INDEX(), MAXST, AREAST) THEN
			IF STOCK.MARCAT <> "*" THEN
			   LIN$(L, 2) = STOCK.DESCRIPCIO
			   IF STOCK.EXISTENCIA < STOCK.STOCKMIN THEN
			      COLOR 27: BEEP: LOCATE 21, 12: PRINT "Aquest article est… baix minims            "
			   ELSE
			      IF STOCK.EXISTENCIA = 0 THEN
				 COLOR 27: BEEP: LOCATE 21, 12: PRINT "Aquest article est… amb existŠncies 0"
			      END IF
			   END IF
			   TROBAT = 999
			   LIN$(L, 3) = LTRIM$(STR$(STOCK.PVPACONSE))
			   LIN$(L, 7) = ""
			END IF
		     END IF
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
		CASE 4
		     'InsertValueCamp 4, LIN$(L, 7)
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
			   PutBackground 10, 10, LIS$
			   ScrollUp 17, 78, 8, 1, 1
			   YC = 18: L = L + 1: EXIT FOR
			   GOSUB MOSTRA
			ELSE
			   L = L + 1: YC = YC + 1: EXIT FOR
			END IF

		CASE ELSE
	 END SELECT
	 LIN$(L, C + 1) = ValueCamp$(C)
	 DisplayAllCamps
      NEXT
      C = 1
      L = L - 1: YC = YC - 1
      PutBackground 10, 10, LIS$
      GOSUB MOSTRA
      L = L + 1: YC = YC + 1
      LOCATE YC, 70: PRINT USING MASC$(5); IMPORT
      COLOR col(0, 0) XOR col(0, 1), col(0, 1)
      LOCATE 20, 67: PRINT USING MASCTOTAL$; TOTAL
      COLOR col(0, 0), col(0, 1): LOCATE 22, 2: PRINT SPACE$(70);
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
      COLOR col(0, 0) XOR col(0, 1), col(0, 1)
      LOCATE 20, 67: PRINT USING MASCTOTAL$; TOTAL
      RETURN

LLISTA.STOCK:
       IF MAXST = 1 THEN
	  TECLA = Avis("AVIS:", "La base de dades dels articles est… buida", "PITJA UNA TECLA...", 0): RETURN
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
       NEXT: CAM$ = LTRIM$(RTRIM$(ValueCamp$(C))): IF C = 0 THEN b = 1 ELSE b = 12
       ASEL = Achoice(3, 9, 20, 70, Q - 1, LL$(), col(), "Codi               Descripci¢                               ", b, CAM$)
       IF ASEL = 0 OR ASEL = -14 OR ASEL = -13 THEN
	  PutBackground 3, 9, LLISTA$
	  ERASE LL$: RETURN
       END IF
       IF ASEL <= 0 THEN ASEL = 1
       GET AREAST, ASEL, STOCK
       LIN$(L, 1) = STOCK.CODI
       LIN$(L, 2) = STOCK.DESCRIPCIO
       LIN$(L, 3) = STR$(STOCK.PVPACONSE)
       InsertValueCamp 0, STOCK.CODI
       InsertValueCamp 1, STOCK.DESCRIPCIO
       InsertValueCamp 2, STR$(STOCK.PVPACONSE)
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
	 COLOR col(0, 0), col(0, 1)
	 FINESTRA 10, 20, 14, 65, 1, CAIXA1
	 COLOR col(0, 0) XOR col(0, 1), col(0, 1)
	 LOCATE 11, 21: PRINT "         La factura no est… guardada": COLOR col(2, 0), col(2, 1)
	 LOCATE 13, 21: PRINT "    ¨ Estas segur que vols sortir (S/N) ?"
	   DO
	     T$ = INKEY$
	   LOOP UNTIL T$ <> ""
	   SELECT CASE UCASE$(T$)
		  CASE "S"
		       PutBackground 1, 1, fact$
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
      RETURN
END SUB

SUB GuardaAlbaran (RG, MAXLIN, AREANUM, TIPUS%, GUARDAT, EDIT)

    IF FACTURA.MARCAT = "*" THEN FACTURA.MARCAT = " "
    FACTURA.DOCUMENT = "F": nou = 3

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

SUB ImprimirFactura (DEVI$, MAXALBS, AREA5, AREA3T, AREA2T)
    SHARED DIRECCT$, CADFE$
    DIM CONFIG$(3)
    DIM MENU(5) AS STRING

    CONFIG$(0) = " CODI ARTICLE      CONCEPTE                  QUANT.     PREU  DTO%     IMPORT"
    CONFIG$(1) = "         QUANTITAT CONCEPTE                       IMPORT"
    CONFIG$(2) = " CODI ARTICLE     CONCEPTE                  QUANT.    PREU  DTO%          IMPORT"
    CONFIG$(3) = " CODI ARTICLE   CONCEPTE                 QUANT.  PREU  % IMPORT"
    MENU(1) = "  DOCUMENT TIPUS PER DEFECTE     "
    MENU(2) = "  DOCUMENT TIPUS 1               "
    MENU(3) = "  DOCUMENT AMB PAPER PRE-IMPRES  "
    MENU(4) = "  DOCUMENT COMPACTAT (RESGUARDS) "
    MENU(5) = "  DOCUMENT TIPUS COMPACTE        "

    GetBackground 1, 1, 24, 79, FACTU$
    CALL Menu2(MENU(), CASO%, 17, 23, LEN(MENU(1)) + 2, 5, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
    IF CASO% = 999 THEN
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

    PAG = 1: GET AREA5, 1, CAP
    GOSUB CAPSAC

    IF CASO% = 3 THEN
       MAXLINS = 25
    ELSE
       IF CASO% = 5 THEN
	  MAXLINS = 17        ' MODE COMPRIMIT
       ELSE
	  MAXLINS = 29        ' MODE EXTES
       END IF
    END IF
    LI = 1: L = 1

    DO
	IF LI >= MAXLINS THEN
	   IF CASO% = 5 THEN
	      FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
	      PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
	      PRINT #AREATXT, "Suma i continua..."
	      PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
	      FOR SALT = 1 TO 7: PRINT #AREATXT, "": NEXT
	      PAG = PAG + 1: LI = 1
	      GOSUB CAPSAC
	   ELSE
	      FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
	      PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
	      PRINT #AREATXT, "Suma i continua..."
	      PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
	      PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
	      PAG = PAG + 1: LI = 1
	      GOSUB CAPSAC
	   END IF
	END IF

	IF LIN$(L, 6) = "*" THEN
	   TIPU$ = "FACTURA": GOSUB IMPRI.LINIA
	   IF UCASE$(MID$(LIN$(L, 1), 1, 6)) = "C.A.:F" AND MID$(LIN$(L, 8), 8, 2) = "-A" THEN
	      FOR RAC = 1 TO MAXALBS
		  GET AREA2T, RAC, ALBARAN
		  IF ALBARAN.REFALBARAN = LIN$(L, 8) THEN EXIT FOR
	      NEXT
	      GET AREA3T, RAC, LINALBA
	      lalb = 1
	      DO
		 IF LI >= MAXLINS THEN
		    IF CASO% = 5 THEN
		       FOR LC = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
		       PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
		       PRINT #AREATXT, "Suma i continua..."
		       PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
		       FOR SALT = 1 TO 7: PRINT #AREATXT, "": NEXT
		       PAG = PAG + 1: LI = 1
		       GOSUB CAPSAC
		    ELSE
		       FOR LC = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
		       PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
		       PRINT #AREATXT, "Suma i continua..."
		       PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + STRING$(78, "Ä")
		       PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
		       PAG = PAG + 1: LI = 1
		       GOSUB CAPSAC
		    END IF
		 END IF
		 TIPU$ = "ALBARAN": GOSUB IMPRI.LINIA
		 lalb = lalb + 1: LI = LI + 1
	      LOOP UNTIL LINALBA.LINIA(lalb).MARCAR = "-"

	      IF LIN$(L + 1, 6) <> "-" THEN
		 PRINT #AREATXT, STRING$(78, "-")
		 LI = LI + 1
	      END IF
	   END IF
	   L = L + 1: LI = LI + 1
	END IF
    LOOP UNTIL LIN$(L, 6) = "-"
    FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT

    SELECT CASE CASO%
	   CASE 1
		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, USING " SUBTOTAL: ##,###,###.##"; FACTURA.TOTALBRUT
		PRINT #AREATXT, USING " DTO%....:            ##"; FACTURA.DTO;
		PRINT #AREATXT, USING SPACE$(24) + "BASE IMPONIBLE: ##,###,###.##"; FACTURA.BASEIMPONIBLE
		PRINT #AREATXT, USING SPACE$(48) + "     IVA ## %.: ##,###,###.##"; FACTURA.TIPOIVA; FACTURA.TOTALIVA
		PRINT #AREATXT, ""
		PRINT #AREATXT, USING SPACE$(48) + "      TOTAL...: ##,###,###.##"; FACTURA.TOTALNET
		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO; "  Observacions: " + RTRIM$(ARCHIMP.COMPRIMIDO);
		PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		PRINT #AREATXT, "                                                     " + RTRIM$(ARCHIMP.COMPRIMIDO); RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, ""
	   CASE 2
		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, USING SPACE$(48) + "      TOTAL...: ##,###,###.##"; FACTURA.TOTALNET
		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO; "  Observacions: " + RTRIM$(ARCHIMP.COMPRIMIDO);
		PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		PRINT #AREATXT, "                                                     " + RTRIM$(ARCHIMP.COMPRIMIDO); RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		PRINT #AREATXT, STRING$(78, "Ä")
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
		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, USING " SUBTOTAL: ##,###,###.##"; FACTURA.TOTALBRUT
		PRINT #AREATXT, USING " DTO%....:            ##"; FACTURA.DTO;
		PRINT #AREATXT, USING SPACE$(21) + "BASE IMPONIBLE: ##,###,###"; FACTURA.BASEIMPONIBLE
		PRINT #AREATXT, USING SPACE$(48) + "   IVA %.:         ##"; FACTURA.TIPOIVA
		PRINT #AREATXT, ""
		PRINT #AREATXT, USING SPACE$(48) + " TOTAL...: ##,###,###"; FACTURA.TOTALNET
		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO
		PRINT #AREATXT, STRING$(78, "Ä")
	   CASE 5
		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, "    SUBTOTAL:   BASE IMPONIBLE:   DTO%:  IVA%:      TOTAL IVA:     TOTAL NET:"
		PRINT #AREATXT, USING "##,###,###.##     ##,###,###.##      ##     ##   ##,###,###.##  ##,###,###.##"; FACTURA.TOTALBRUT; FACTURA.BASEIMPONIBLE; FACTURA.DTO; FACTURA.TIPOIVA; FACTURA.TOTALIVA; FACTURA.TOTALNET
		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, "Forma de pagament: "; FACTURA.PERSONA.FORMAPAGO; "  Observacions: " + RTRIM$(ARCHIMP.COMPRIMIDO);
		PRINT #AREATXT, RTRIM$(FACTURA.OBSERVA(1)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		PRINT #AREATXT, "                                                    " + RTRIM$(ARCHIMP.COMPRIMIDO); RTRIM$(FACTURA.OBSERVA(2)) + RTRIM$(ARCHIMP.NOCOMPRIMIDO)
	   CASE ELSE
    END SELECT
    PRINT #AREATXT, ""
    PRINT #AREATXT, SPACE$((40 \ 2) - (LEN(CADFE$) \ 2)) + RTRIM$(ARCHIMP.ENSANCHADO) + CADFE$ + RTRIM$(ARCHIMP.NOENSANCHADO)
    IF CASO% = 5 THEN FOR L = 1 TO 18: PRINT #AREATXT, "": NEXT

    CLOSE #AREATXT
    COLOR 31, 9: LOCATE 11, 31: PRINT "  IMPRIMINT  "
    COLOR 31, 9: LOCATE 13, 31: PRINT "   FITXERS   "

    ImprimeixFitxerTXT DIRECCT$ + "FACTURA.TXT", DEVI$, 100
    PutBackground 1, 1, FACTU$
    EXIT SUB

CAPSAC:
    PRINT #AREATXT, ""
    PRINT #AREATXT, ""
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + RTRIM$(CAP.LINIES(0)) + RTRIM$(ARCHIMP.NOENSANCHADO)
    FOR I = 1 TO 4: PRINT #AREATXT, CAP.LINIES(I): NEXT
    SELECT CASE MODO
	   CASE 2
		PRINT #AREATXT, ""
		PRINT #AREATXT, ""
		PRINT #AREATXT, "                                          " + RTRIM$(ARCHIMP.ENSANCHADO) + "Factura: " + FACTURA.REFFACTURA + RTRIM$(ARCHIMP.NOENSANCHADO)
		PRINT #AREATXT, "              Data:         Codi Client:      " + FACTURA.PERSONA.NOM + RTRIM$(ARCHIMP.NEGRITA)
		PRINT #AREATXT, SPACE$(9) + SPACE$(5) + FACTURA.DADA + SPACE$(6) + FACTURA.CODCLIENT + RTRIM$(ARCHIMP.NONEGRITA);
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
		PRINT #AREATXT, "                                          " + RTRIM$(ARCHIMP.ENSANCHADO) + "Factura: " + FACTURA.REFFACTURA + RTRIM$(ARCHIMP.NOENSANCHADO)
		PRINT #AREATXT, "                 Data:      Codi Client:" + RTRIM$(ARCHIMP.NEGRITA)
		PRINT #AREATXT, " " + SPACE$(9) + SPACE$(7) + FACTURA.DADA + SPACE$(3) + FACTURA.CODCLIENT + RTRIM$(ARCHIMP.NONEGRITA);
		PRINT #AREATXT, SPACE$(8) + " " + FACTURA.PERSONA.NOM
		PRINT #AREATXT, "           " + SPACE$(34) + "  " + FACTURA.PERSONA.COGNOMS
		PRINT #AREATXT, " P…gina N§:"; SPACE$(34) + "  D.N.I. " + FACTURA.PERSONA.DNI + RTRIM$(ARCHIMP.NEGRITA)
		PRINT #AREATXT, USING " \  \       \ \"; LTRIM$(STR$(PAG)); RTRIM$(ARCHIMP.NONEGRITA);
		PRINT #AREATXT, SPACE$(32) + "  " + FACTURA.PERSONA.DIRECCIO
		PRINT #AREATXT, SPACE$(47) + LTRIM$(RTRIM$(FACTURA.PERSONA.POBLACIO)) + " " + FACTURA.PERSONA.CPOSTAL
		PRINT #AREATXT, SPACE$(47) + "Tels." + FACTURA.PERSONA.TELEFON1
		PRINT #AREATXT, SPACE$(47) + "     " + FACTURA.PERSONA.TELEFON2
		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, CONFIG$(0)
		PRINT #AREATXT, STRING$(78, "Ä")
	   CASE ELSE
		PRINT #AREATXT, "                                          " + RTRIM$(ARCHIMP.ENSANCHADO) + "Factura: " + FACTURA.REFFACTURA + RTRIM$(ARCHIMP.NOENSANCHADO)
		PRINT #AREATXT, "                 Data:      Codi Client:";
		PRINT #AREATXT, "     Ú" + SPACE$(31) + "¿" + RTRIM$(ARCHIMP.NEGRITA)
		PRINT #AREATXT, "          " + SPACE$(7) + FACTURA.DADA + SPACE$(3) + FACTURA.CODCLIENT + RTRIM$(ARCHIMP.NONEGRITA);
		PRINT #AREATXT, SPACE$(8) + " " + FACTURA.PERSONA.NOM
		PRINT #AREATXT, "           " + SPACE$(34) + "  " + FACTURA.PERSONA.COGNOMS
		PRINT #AREATXT, " P…gina N§:"; SPACE$(34) + "  D.N.I. " + FACTURA.PERSONA.DNI + RTRIM$(ARCHIMP.NEGRITA)
		PRINT #AREATXT, USING " \  \       \ \"; LTRIM$(STR$(PAG)); RTRIM$(ARCHIMP.NONEGRITA);
		PRINT #AREATXT, SPACE$(32) + "  " + FACTURA.PERSONA.DIRECCIO
		PRINT #AREATXT, SPACE$(47) + LTRIM$(RTRIM$(FACTURA.PERSONA.POBLACIO)) + " " + FACTURA.PERSONA.CPOSTAL
		PRINT #AREATXT, SPACE$(47) + "Tels." + FACTURA.PERSONA.TELEFON1
		PRINT #AREATXT, SPACE$(47) + "     " + FACTURA.PERSONA.TELEFON2
		PRINT #AREATXT, SPACE$(45) + "À" + SPACE$(31) + "Ù"

		PRINT #AREATXT, STRING$(78, "Ä")
		PRINT #AREATXT, CONFIG$(OPC)
		PRINT #AREATXT, STRING$(78, "Ä")
    END SELECT
    RETURN

IMPRI.LINIA:
    SELECT CASE TIPU$
	CASE IS = "ALBARAN"
	     COD$ = LINALBA.LINIA(lalb).CODART
	     CON$ = LINALBA.LINIA(lalb).CONCEPTE
	     QUA$ = STR$(LINALBA.LINIA(lalb).QUANTI)
	     PRE$ = STR$(LINALBA.LINIA(lalb).PREU)
	     DTO$ = STR$(LINALBA.LINIA(lalb).DTO)
	     IMP$ = STR$(LINALBA.LINIA(lalb).IMPORT)
	CASE IS = "FACTURA"
	     COD$ = LIN$(L, 1)
	     CON$ = LIN$(L, 2)
	     QUA$ = LIN$(L, 4)
	     PRE$ = LIN$(L, 3)
	     DTO$ = LIN$(L, 7)
	     IMP$ = LIN$(L, 5)
	CASE ELSE
    END SELECT
    SELECT CASE MODO
	   CASE 0
		IF UCASE$(MID$(LIN$(L, 1), 1, 6)) = "C.A.:F" AND MID$(LIN$(L, 8), 8, 2) = "-A" AND TIPU$ = "FACTURA" THEN
		   PRINT #AREATXT, USING " \                \"; COD$;
		   PRINT #AREATXT, CON$
		ELSE
		   PRINT #AREATXT, USING " \                \"; COD$;
		   PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); CON$; RTRIM$(ARCHIMP.NOCOMPRIMIDO);
		   PRINT #AREATXT, USING "##,###.#"; VAL(QUA$);
		   PRINT #AREATXT, USING "###,###.##"; VAL(PRE$);
		   PRINT #AREATXT, USING "  ##"; VAL(DTO$);
		   PRINT #AREATXT, USING "##,###,###.##"; VAL(IMP$)
		END IF
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
		IF UCASE$(MID$(LIN$(L, 1), 1, 6)) = "C.A.:F" AND MID$(LIN$(L, 8), 8, 2) = "-A" AND TIPU$ = "FACTURA" THEN
		   CA$ = ""
		   PRINT #AREATXT, USING " \                \"; CA$;
		   PRINT #AREATXT, LIN$(L, 2)
		ELSE
		   Q$ = LTRIM$(FormatC$(VAL(QUA$), "###.###")): QUANT$ = SPACE$(7 - LEN(Q$)) + Q$
		   P$ = FormatC$(VAL(PRE$), "#.###.###"): PREU$ = SPACE$(9 - LEN(P$)) + P$
		   I$ = FormatC$(VAL(IMP$), "##.###.###"): IMPORT$ = SPACE$(10 - LEN(I$)) + I$
		   PRINT #AREATXT, USING " \                \"; COD$;
		   PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO); CON$; RTRIM$(ARCHIMP.NOCOMPRIMIDO);
		   PRINT #AREATXT, USING " \     \"; QUANT$;
		   PRINT #AREATXT, USING " \       \"; PREU$;
		   PRINT #AREATXT, USING "  ##"; VAL(DTO$);
		   PRINT #AREATXT, USING "  \        \"; IMPORT$
		END IF
	   CASE ELSE
    END SELECT
    RETURN
END SUB

FUNCTION INICIAREF (AREANUM, R, MARCAT, REFOLD$)
	  SHARED ANY$
	  DIM OP$(1 TO 10)
	 
	  GetBackground 1, 1, 24, 80, me$
	  GET AREANUM, 1, DOCNUM
	  MAXDOC% = DOCNUM.MAXNUM
	  FOR RE = 1 TO MAXDOC%: OP$(RE) = DOCNUM.FACTNUM(RE).CONFACT: NEXT
	  CALL Menu2(OP$(), CASO%, 10, 10, LEN(OP$(1)) + 2, MAXDOC%, col(0, 0), col(0, 1), col(1, 0), col(1, 1))

	  IF CASO% = 999 OR CASO% = 888 THEN
	     nou = 9
	     PutBackground 1, 1, me$
	     EXIT FUNCTION
	  ELSE
	     REF$ = DOCNUM.FACTNUM(CASO%).NUMFACT
	     ALB$ = LTRIM$(STR$(DOCNUM.FACTNUM(CASO%).MAXFACT))
	  END IF

	  L2 = 6
	  FOR L = LEN(ALB$) TO 1 STEP -1
	      MID$(REF$, L2, 1) = MID$(ALB$, L, 1): L2 = L2 - 1
	  NEXT

	  DA$ = MID$(FormatD$(Now#, "dd/mm/yy"), 7, 2)
	  IF VAL(ANY$) < VAL(DA$) THEN
	     MID$(REF$, 1, 2) = ANY$
	  ELSE
	     MID$(REF$, 1, 2) = DA$
	  END IF

	  INICIAREF = CASO%
	  FACTURA.REFFACTURA = REF$
	  PutBackground 1, 1, me$
END FUNCTION

SUB INITALBARAN (MAXLIN)
    FACTURA.REFFACTURA = CHR$(0)               ' INCIAR FACTURA
    FACTURA.CODCLIENT = CHR$(0)
    FACTURA.DADA = FormatD$(Now#, "dd/mm/yy")
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
    MASCARA$ = " ³\" + SPACE$(8) + "\³\" + SPACE$(47) + "\  ³ #,###,###,###³##.##³##,###,###³#,###,###,###³\      \³"
    MASCAR2$ = "  \" + SPACE$(8) + "\ \" + SPACE$(47) + "\  ³ #,###,###,###³##.##³##,###,###³#,###,###,###³\      \"
    MAXL = 54: TOTALB& = 0: TOTALN& = 0
    MAXF# = CEMPRE.MAXFACTURA - 1
    FOR RL! = 1 TO CEMPRE.MAXFACTURA - 1
	CALL FinestraEstat("Generant llistats...", 0)
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
    CALL FinestraEstat("Imprimint llistats...", 0)
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
    PRINT #AREATXT, " Data..: "; FormatD$(Now#, "dd-mm-yyyy")
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""
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
    GetBackground 1, 1, 25, 80, fact$
    COLOR col(0, 0), col(0, 1): FINESTRA 1, 1, 25, 80, 0, CAIXA1
    COLOR col(2, 0): LOCATE , , 1, 13, 14
    LOCATE 2, 2: PRINT "Factura n£mero:"; SPACE$(10); "   Document: "; : COLOR 15: PRINT DOCU$; : COLOR col(2, 0): PRINT "    M…xim de l¡nies:"; : COLOR col(0, 0): PRINT MAX: COLOR col(2, 0)
    LOCATE 3, 2: PRINT "Ref. Factura:"
    LOCATE 3, 27: PRINT "                        Data:"
    COLOR col(0, 0): LOCATE 20, 40: PRINT " DTO% Linia:"
    LOCATE 6, 1: PRINT "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍÍ¹"
    LOCATE 7, 1: PRINT "ºCodi d'article    ºConcepte                     ºPreu     ºQuantit.ºImport    º"
    LOCATE 8, 1: PRINT "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÍÍ¹"
    LOCATE 19, 1: PRINT "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÍÍ¹"
    FOR L = 9 TO 18: LOCATE L, 2: PRINT SPACE$(18); "º"; SPACE$(29); "º"; SPACE$(9); "º"; SPACE$(8); "º": NEXT
    LOCATE 20, 61: PRINT "Total:"
    LOCATE 21, 2: PRINT STRING$(78, "Í");
    COLOR col(0, 0), col(0, 1): LOCATE 20, 67: PRINT USING "##,###,###.##"; TOTAL
END SUB

SUB MASCALBS
    COLOR col(0, 0), col(0, 1): FINESTRA 2, 1, 20, 80, 0, CAIXA1
    LOCATE 3, 2: PRINT "ReferŠnciaºClient" + SPACE$(21) + "ºTotal brut   ºDTO% ºIVA% ºTotal Net"
    LOCATE 4, 1: PRINT "Ì"; STRING$(78, "Í"); "¹"
    LOCATE 4, 12: PRINT "Î": LOCATE 4, 40: PRINT "Î": LOCATE 4, 54: PRINT "Î": LOCATE 4, 60: PRINT "Î": LOCATE 4, 66: PRINT "Î"
    LOCATE 20, 12: PRINT "Ê": LOCATE 20, 40: PRINT "Ê": LOCATE 20, 54: PRINT "Ê": LOCATE 20, 60: PRINT "Ê": LOCATE 20, 66: PRINT "Ê"
    LOCATE 2, 12: PRINT "Ë": LOCATE 2, 40: PRINT "Ë": LOCATE 2, 54: PRINT "Ë": LOCATE 2, 60: PRINT "Ë": LOCATE 2, 66: PRINT "Ë"
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

SUB ReadCalculFactura
 
  GetBackground 1, 1, 24, 79, RESBUF$
  COLOR col(0, 0), col(0, 1): FINESTRA 9, 10, 14, 60, 1, CAIXA1
  COLOR col(2, 0), col(0, 1)
  LOCATE 10, 12: PRINT "Subtotal:"; : COLOR 15: PRINT FACTURA.TOTALBRUT; : COLOR 14
  LOCATE 13, 33: PRINT "      Total:"; : COLOR 15: PRINT FACTURA.TOTALNET; : COLOR 14
  LOCATE 10, 33: PRINT "Descompte %:"; : COLOR 15: PRINT FACTURA.DTO; : COLOR 14
  LOCATE 11, 33: PRINT "      IVA %:"; : COLOR 15: PRINT FACTURA.TIPOIVA;

  SetMaxCamps 1
  SetColorCamp 0, col(0, 0), col(0, 1), col(1, 0), col(1, 1), col(2, 0), col(2, 1)
  SetColorCamp 1, col(0, 0), col(0, 1), col(1, 0), col(1, 1), col(2, 0), col(2, 1)
  InsertValueCamp 0, LTRIM$(STR$(FACTURA.DTO))
  InsertValueCamp 1, LTRIM$(STR$(FACTURA.TIPOIVA))
  SetInitCamp 0, 10, 45, num, 0, "999", ""
  SetInitCamp 1, 11, 45, num, 0, "999", ""
  IF ReadCamp(0) = SALIR THEN EXIT SUB
  IF ReadCamp(1) = SALIR THEN EXIT SUB
  
  FACTURA.DTO = VAL(ValueCamp$(0))
  FACTURA.TIPOIVA = VAL(ValueCamp$(1))
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

       FOR C = 0 TO 10: DeleteCamp C: NEXT C
       REPINTACAPSALERA (MAXAL)
       GOSUB AJUDA

       FOR C = 0 TO 5
	   VALUE = ReadCamp(C)
	   SELECT CASE VALUE
		  CASE 1
		       TROBAT = 0
		       FOR R = 1 TO MAXCL
			   GET AREA4, R, CLIENT
			   IF ValueCamp$(1) = CLIENT.CODICLIENT THEN
			      GOSUB TRANSPASS
			      DisplayAllCamps
			      TROBAT = 44
			      EXIT FOR
			   END IF
		       NEXT
		       IF TROBAT = 44 THEN
			  FACTURA.REFFACTURA = ValueCamp$(0)
			  FACTURA.CODCLIENT = ValueCamp$(1)
		       END IF
		  CASE F2
		       IF C = 1 OR C = 3 THEN GOSUB LLISTA.CLIENTS
		       C = C - 1
		  CASE F3 TO F10
		       C = C - 1
		  CASE 999
		       EXIT SUB
		  CASE ELSE
	   END SELECT
       NEXT
       FACTURA.DADA = ValueCamp$(2)
       FACTURA.PERSONA.FORMAPAGO = ValueCamp$(5)
       DisplayAllCamps
       ERASE LLISTA$
       EXIT SUB

LLISTA.CLIENTS:
       IF MAXCL = 1 THEN
	  TECLA = Avis("AVIS:", "La base de dades dels clients est… buida", "PITJA UNA TECLA...", 0)
	  RETURN
       END IF

       GetBackground 1, 1, 24, 79, LLISTA$
       SetScoreBoard SOFF: LOCATE , , 0: R = 1
       FOR Q = 1 TO MAXCL - 1
	   GET AREA4, Q, CLIENT
	   IF CLIENT.MARCAT = "-" THEN
	      CODI$ = CLIENT.CODICLIENT
	      N$ = LTRIM$(RTRIM$(CLIENT.NOM))
	      C$ = LTRIM$(RTRIM$(CLIENT.COGNOMS))
	      S$ = N$ + " " + C$: NOM$ = MID$(S$, 1, 40)
	      LLISTA$(R) = CODI$ + " " + NOM$
	      R = R + 1
	   END IF
       NEXT

       ASEL = Achoice(3, 9, 20, 61, R - 1, LLISTA$(), col(), "Codi       Nom                                     ", 12, ValueCamp$(3))
       IF ASEL = 0 OR ASEL = -14 OR ASEL = -13 THEN
	  PutBackground 1, 1, LLISTA$
	  RETURN
       END IF
       IF ASEL <= 0 THEN ASEL = 1
       GET AREA4, ASEL, CLIENT
       InsertValueCamp 1, MID$(LLISTA$(ASEL), 1, 10)
       GOSUB TRANSPASS
       PutBackground 1, 1, LLISTA$
       DisplayAllCamps
       RETURN

AJUDA:
      COLOR col(0, 0), col(0, 1): LOCATE 22, 2: PRINT SPACE$(78);
      COLOR col(0, 0), col(0, 1): LOCATE 23, 2: PRINT SPACE$(78);
      COLOR col(0, 0), col(0, 1): LOCATE 24, 2: PRINT SPACE$(78);
      LOCATE 23, 2: PRINT "ESC=EDITAR LINIES   F2=LLISTAT DE CLIENTS"
      LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR";
      RETURN

TRANSPASS:
      InsertValueCamp 3, CLIENT.NOM
      InsertValueCamp 4, CLIENT.COGNOMS
      InsertValueCamp 5, CLIENT.FORMAPAGO
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
      RETURN
END SUB

SUB ReadObserva
    SHARED OBS$()
    LOCATE 22, 2: PRINT SPACE$(70);
    LOCATE 23, 2: PRINT SPACE$(70);
    LOCATE 24, 2: PRINT SPACE$(70);

    SetMaxCamps 1
    SetColorCamp 0, col(1, 0), col(1, 1), col(0, 0), col(0, 1), col(2, 0), col(2, 1)
    SetColorCamp 1, col(1, 0), col(1, 1), col(0, 0), col(0, 1), col(2, 0), col(2, 1)
    
    SetInitCamp 0, 22, 20, ASCI, 0, STRING$(50, "X"), ""
    SetInitCamp 1, 23, 20, ASCI, 0, STRING$(50, "X"), ""

    COLOR col(0, 0) XOR col(0, 1), col(0, 1)
    LOCATE 24, 2: PRINT "ENTER=SORTIR";
    LOCATE 22, 2: PRINT "Observacions: ";
    InsertValueCamp 0, FACTURA.OBSERVA(1): InsertValueCamp 1, FACTURA.OBSERVA(2)

    IF ReadCamp(0) = SALIR THEN
       OBS$(0) = ValueCamp$(0)
       EXIT SUB
    END IF
    IF ReadCamp(1) = SALIR THEN
       OBS$(1) = ValueCamp$(1)
       EXIT SUB
    END IF
    OBS$(0) = ValueCamp$(0)
    OBS$(1) = ValueCamp$(1)
    FACTURA.OBSERVA(1) = OBS$(0)
    FACTURA.OBSERVA(2) = OBS$(1)
    LOCATE 22, 2: PRINT SPACE$(70);
    LOCATE 23, 2: PRINT "ENTER=EDITAR LINIA    ESC=ANULAR    SUPR=SUPRIMIR LINIA";
    LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR    CTRL+F=ACABAR DE FACTURAR";
END SUB

SUB REPINTACAPSALERA (MAX)
       FOR C = 0 TO 10: DeleteCamp C: NEXT C

       SetMaxCamps 5
       FOR C = 0 TO 5: SetColorCamp C, col(1, 0), col(1, 1), col(0, 0), col(0, 1), col(2, 0), col(2, 1): NEXT
       SetInitCamp 0, 3, 15, ASCI, 0, "XXXXXXXXX", ""
       SetInitCamp 1, 3, 40, ASCI, 0, "XXXXXXXXXX", "Codi client:"
       SetInitCamp 2, 3, 56, num, 0, "99/99/99", ""
       SetInitCamp 3, 4, 48, ASCI, 0, STRING$(20, "X"), "Nom:"
       SetInitCamp 4, 5, 48, ASCI, 0, STRING$(30, "X"), "Cognoms:"
       SetInitCamp 5, 5, 21, ASCI, 0, STRING$(18, "X"), "Forma de pagament:"

       COLOR 15: LOCATE 2, 18: PRINT R
       InsertValueCamp 0, FACTURA.REFFACTURA
       InsertValueCamp 1, FACTURA.CODCLIENT
       InsertValueCamp 2, FACTURA.DADA
       InsertValueCamp 3, FACTURA.PERSONA.NOM
       InsertValueCamp 4, FACTURA.PERSONA.COGNOMS
       InsertValueCamp 5, FACTURA.PERSONA.FORMAPAGO
       DisplayAllCamps
END SUB

SUB SUMARALBARANS (MAXAL, AREA2)

	   Avis.Sonor (1)
	   GetBackground 9, 10, 18, 51, sum$
	   FINESTRA 9, 10, 17, 50, 1, CAIXA1
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
	   COLOR col(2, 0), col(2, 1)
	   LOCATE 12, 11: PRINT "TOTAL TALLER....:"; : COLOR col(0, 0), col(0, 1)
	   PRINT USING "##,###,###.##"; SUMATALLER:  COLOR col(2, 0), col(2, 1)
	   LOCATE 13, 11: PRINT "TOTAL BOTIGA....:"; : COLOR col(0, 0), col(0, 1)
	   PRINT USING "##,###,###.##"; SUMABOTIGA:  COLOR col(2, 0), col(2, 1)
	   LOCATE 14, 11: PRINT "IVA's...........:"; : COLOR col(0, 0), col(0, 1)
	   PRINT USING "##,###,###.##"; SUMAIVA:  COLOR col(2, 0), col(2, 1)
	   LOCATE 15, 11: PRINT "TOTAL...........:"; : COLOR col(0, 0), col(0, 1)
	   PRINT USING "##,###,###.##"; SUMANET: COLOR col(2, 0), col(2, 1)
	   C$ = INPUT$(1)
	   PutBackground 9, 10, sum$
END SUB

