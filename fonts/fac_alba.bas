' ********************************************************************
'
' Fitxer....................: FAC_ALBA.BAS
' Titol.....................: Modul per facturar albarans
'
' ********************************************************************
'
' Data inici................: 06/06/1997 19:33:00
' Data de la darrera revisi¢: 01/11/1999 13:35:00
' Autor.....................: Tomeu Cap¢ i Cap¢
' CopyRight.................: Smart Software 1993/99 (C)
' Codi......................: MS-BASIC 7.01 (PDS)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB CarregarAlbaran (R!, MAXLIN!, AREA3F!, AREA2F!)
DECLARE SUB FacturarAlbarans (MAX!, DP$, DIRECC$, DIRECCR$, DIRECCI$, DIRECCT$, IMPRESORA!, DEVI$, MI!)
DECLARE SUB ImprimirFactura (DEVI$, DP$, MAXALBS!, AREA5!, AREA3T!, AREA2T!, AREADOC!, MI!)
DECLARE FUNCTION IniciaRef! (AREANUM!, R!, MARCAT!, REFOLD$)
DECLARE SUB InitWindows ()
DECLARE SUB PintaValors ()

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'D:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'D:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'D:\FACT3\FONTS\STOCK.BI'
'$INCLUDE: 'D:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'D:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'D:\FACT3\FONTS\WIN.BI'
'$INCLUDE: 'D:\FACT3\FONTS\CONSTS.BI'

TYPE TRA
     REGREAL AS INTEGER
     ALBARAN AS STRING * 9
END TYPE

'$DYNAMIC
DIM SHARED LIN$(100, 8)
DIM SHARED COL(2, 1)
DIM SHARED CLIENT AS CLIENT
DIM SHARED CEMPRE AS CONTROLEMP
DIM SHARED COMMENT$(7, 0)
DIM SHARED ALBARAN AS ALBAR
DIM SHARED LINALBA AS LINIES
DIM SHARED LINFACT AS LINIAFACT
DIM SHARED TRANS AS TRA
DIM SHARED CAP AS CAPSALDOCS
DIM SHARED SPOOLIMP AS SPOOL
DIM SHARED ARCHIMP AS IMPRESORA
DIM SHARED CFG AS CONFIG
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
DIM SHARED USR AS USUARIS
DIM SHARED DOCNUM AS DN
DIM SHARED FACTURA AS FACTU
DIM SHARED TD AS TIPUSDOC
DIM COLORS AS COLO
DIM PASO AS TRANS
DIM ANYS AS ANNO


'********************************************************************
'  Comprovaci¢ de la cridada del programa principal
'********************************************************************
      GOSUB ENVIRONMENT
      
      UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
      DBF$ = LTRIM$(RTRIM$(CFG.DDADE))             ' Assignar direcctoris
      SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
      MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
      HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
      SYS$ = LTRIM$(RTRIM$(CFG.DSIST))

      DIRECCF$ = DBF$               ' Subdirecctori de les base de dades
      DIRECCP$ = SCR$               ' Subdirecctori de les pantalles
      DIRECCR$ = MSK$               ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = HLP$              ' Subdirecctori de ajuda
      DIRECCI$ = UNIDAD$ + "\IMPRESOR\"  ' Subdirecctori de les impresores
      DIRECCT$ = DBF$ + "TEXTOS\"
      DIRECCH$ = DBF$ + "HISTORIC\"
      DP$ = DBF$

      MAXLINS = USR.MAXLINS
      MAXFAC = USR.LINFACT
      MAXALB = USR.LINALBA
      IVA = EMPRES.IVA
      DTO = EMPRES.DTO
      R = EMPRES.ANY
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA
      MI = USR.MODEIMPRES

      SetFormatCC (34)
      SetDirRecursos (DIRECCR$)

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
      
      CALL FacturarAlbarans(MAXFAC, DP$, DIRECCF$, DIRECCR$, DIRECCI$, DIRECCT$, IMPRESORA, DEV$, MI)
      SYSTEM


ENVIRONMENT:
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

	  IF LTRIM$(RTRIM$(CA2$)) <> "FAC_ALBA.EXE" THEN
	     tecla = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "Pitja una tecla", 0)
	  END IF

	  KILL TMP$ + "PASS.TMP": KILL TMP$ + "PASU.TMP": KILL TMP$ + "PASE.TMP": KILL TMP$ + "PROT.TMP"
	  RETURN

ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

REM $STATIC

REM $DYNAMIC
SUB CarregarAlbaran (R, MAXLIN, AREA3F, AREA2F)
    SHARED LIN$()

    GET AREA3F, R, LINFACT
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
'    GET AREA2F, R, FACTURA
END SUB

SUB FacturarAlbarans (MAX, DP$, DIRECC$, DIRECCR$, DIRECCI$, DIRECCT$, IMPRESORA, DEVI$, MI)

    ON ERROR GOTO ERRORS

    DIM MENU(3) AS STRING, MENSA$(1, 0)
    MENU(1) = "Facturar un ~client       ": MENSA$(0, 0) = "Factura tots els albarans pendents d'un client"
    MENU(2) = "Facturar ~tots els clients": MENSA$(1, 0) = "Factura tots els clients pendents de facturar"

    GOSUB OBRIRFITXERS
    CALL InitWindows

    DO
     CALL MenuBar(MENU(), MENSA$(), 0, CASO%, 5, 52, 26, 2, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
     
     SELECT CASE CASO%
	    CASE 1
		 GOSUB FACTURAR.UNIC.CLIENT
	    CASE 2
	       '  GOSUB FACTURAR.TOTS.CLIENTS
	    CASE 999, -4, -19
		 GOSUB FI
	    CASE ELSE
     END SELECT
    LOOP

'************************************************************************
' Factura tots els albarans pendents de tots els clients dins factures
' independents per cada client.
'************************************************************************


FACTURAR.TOTS.CLIENTS:
    
    FOR R = 1 TO MAXCL - 1
	GET AREA4, R, CLIENT
	CODI$ = LTRIM$(RTRIM$(CLIENT.CODICLIENT))
	NOMT$ = LTRIM$(RTRIM$(CLIENT.NOM)) + " " + LTRIM$(RTRIM$(CLIENT.COGNOMS))

	IF CLIENT.MARCAT <> "*" THEN
	   GOSUB CERCAR.ALBARANS
	END IF
    NEXT

    RETURN

'************************************************************************
' Factura tots els albarans pendents d'un client dins una factura
'************************************************************************

FACTURAR.UNIC.CLIENT:
    GetBackground 1, 1, 25, 80, vell$
    GOSUB AJUDA
    SetMaxCamps 2
    SetInitCamp 0, 9, 42, 1, 0, "XXXXXXXXXX", "Codi client:"
    SetInitCamp 1, 17, 42, 1, 0, "99/99/9999", "Data d'inici:"
    SetInitCamp 2, 18, 42, 1, 0, "99/99/9999", "Data final:"

    FOR C = 0 TO 2
	SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
    NEXT

    ShowWindow 0
    DisplayAllCamps
    GOSUB BORRA.BARRA
    CODIOLD$ = ""
    VARIMP$ = "Si"
    LOCATE 11, 25: PRINT "   -Imprimir la factura: "; VARIMP$

    FOR C = 0 TO 2
	VALUE = ReadCamp(C)
	SELECT CASE VALUE
	       CASE 0
		    IF OBERTS = 1 THEN GOSUB BORRA.TEMPS
		    GOSUB BORRA.BARRA
		    FOR R = 1 TO MAXCL - 1
			GET AREA4, R, CLIENT
			IF CLIENT.MARCAT <> "*" THEN
			   IF LTRIM$(RTRIM$(CLIENT.CODICLIENT)) = LTRIM$(RTRIM$(ValueCamp$(0))) THEN
			      CODI$ = LTRIM$(RTRIM$(CLIENT.CODICLIENT))
			      NOMT$ = LTRIM$(RTRIM$(CLIENT.NOM)) + " " + LTRIM$(RTRIM$(CLIENT.COGNOMS))
			      COLOR COL(1, 0), COL(1, 1)
			      LOCATE 12, 25: PRINT LTRIM$(RTRIM$(CLIENT.NOM))
			      LOCATE 13, 25: PRINT LTRIM$(RTRIM$(CLIENT.COGNOMS))
			      COLOR COL(0, 0), COL(0, 1)
			      EXIT FOR
			   END IF
			END IF
		    NEXT

		    IF CODI$ = "" THEN
		       tecla = Avis("ERROR:", "Aquest codi no existeix", "Pitja una tecla...", 0)
		       C = C - 1
		    ELSE
		       GOSUB CERCAR.ALBARANS

		       IF ALB > 0 THEN
			  GET AREA2T, 1, ALBARAN
			  InsertValueCamp 1, ALBARAN.DADA
			  GET AREA2T, RA - 1, ALBARAN
			  InsertValueCamp 2, ALBARAN.DADA
			  DisplayAllCamps
			  MAXALBFAC = RA - 1
		       END IF
		    END IF

	       CASE 1, 2
		    RINICI = 0: RFINAL = 0: TROBAT = 0
		    FOR RC = 1 TO MAXALBFAC
			GET AREA2T, RC, ALBARAN
			IF TROBAT = 0 THEN
			  IF ValueCamp$(1) = ALBARAN.DADA THEN
			     RINICI = RC
			     TROBAT = 999
			   END IF
			END IF
			IF ValueCamp$(2) = ALBARAN.DADA THEN RFINAL = RC
		    NEXT
	      
		    IF RINICI = 0 THEN
		       tecla = Avis("ERROR:", "En aquesta data no hi ha res!!", "Pitji una tecla...", 0)
		       C = C - 1
		    ELSE
		       IF RFINAL = 0 THEN
			  tecla = Avis("ERROR:", "En aquesta data no hi ha res!!", "Pitji una tecla...", 0)
			  C = C - 1
		       END IF
		    END IF
		    DADAI$ = ValueCamp$(1)
		    DADAF$ = ValueCamp$(2)

	       CASE F1
		    'SHELL "BKHLP FAC_ALBA"
		    C = C - 1
	       CASE F2
		    IF C = 0 THEN GOSUB LLISTA.CLIENTS
		    C = C - 1
	       CASE F3
		    COLOR COL(0, 0), COL(0, 1)
		    IF VARIMP$ = "Si" THEN VARIMP$ = "No" ELSE VARIMP$ = "Si"
		    LOCATE 11, 25: PRINT "   -Imprimir la factura: "; VARIMP$
		    C = C - 1
	       CASE F4 TO F10
		    C = C - 1
	       CASE 999
		    PutBackground 1, 1, vell$
		    RETURN
	       CASE ELSE
	END SELECT
    NEXT

    FOR q = 1 TO MAXFA - 1
	GET AREA2F, q, FACTURA
	IF FACTURA.MARCAT = "*" THEN
	   TROBAT$ = "Z"
	   EXIT FOR
	END IF
    NEXT

    IF TROBAT$ = "Z" THEN   ' SI HA TROBAT ALGUN REGISTRE MARCAT
       R = q
    ELSE
       R = MAXFA
       MAXFA = MAXFA + 1
       GET AREA, 1, CEMPRE
       CEMPRE.MAXFACTURA = MAXFA
       PUT AREA, 1, CEMPRE
    END IF

    TIPUS% = IniciaRef(AREANUM, R, FALSE, "")
    IF TIPUS% = 999 OR TIPUS% = 888 THEN GOSUB FI

    J = 1: TOTAL& = 0
    FOR RT = RINICI TO RFINAL
	Barra 10, 10, 100, J
	GET AREA2T, RT, ALBARAN

	REFACT$ = ALBARAN.REFALBARAN
	MID$(REFACT$, 1, 1) = "F"
	LINFACT.LINIA(J).REFALBARAN = ALBARAN.REFALBARAN
	LINFACT.LINIA(J).CODART = "C.A.:" + REFACT$
	LINFACT.LINIA(J).CONCEPTE = "Albaran N§: " + ALBARAN.REFALBARAN + " del " + ALBARAN.DADA
	LINFACT.LINIA(J).PREU = ALBARAN.TOTALBRUT
	LINFACT.LINIA(J).QUANTI = 1
	LINFACT.LINIA(J).DTO = 0
	LINFACT.LINIA(J).IMPORT = LINFACT.LINIA(J).PREU * LINFACT.LINIA(J).QUANTI
	LINFACT.LINIA(J).MARCAR = "*"
	TOTAL& = TOTAL& + LINFACT.LINIA(J).IMPORT
	J = J + 1
    NEXT
    LINFACT.LINIA(J).MARCAR = "-"
    FACTURA.TOTALBRUT = TOTAL&
    PUT AREA3F, R, LINFACT

'*****************************************************************************
    COLOR COL(0, 0), COL(0, 1): FINESTRA 8, 8, 14, 60, 1, CAIXA1

    PintaValors

    SetMaxCamps 1
    SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), 0, 0
    SetColorCamp 1, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), 0, 0
    InsertValueCamp 0, LTRIM$(STR$(FACTURA.DTO))
    InsertValueCamp 1, LTRIM$(STR$(FACTURA.TIPOIVA))
    SetInitCamp 0, 9, 45, NUM, 0, "999", ""
    SetInitCamp 1, 10, 45, NUM, 0, "999", ""
  
    FOR C = 0 TO 1
	VALUE = ReadCamp(C)
	SELECT CASE VALUE
	       CASE 999, F1 TO F10
		     C = C - 1
	       CASE ELSE
	END SELECT
    NEXT

    FACTURA.DTO = VAL(ValueCamp$(0))
    FACTURA.TIPOIVA = VAL(ValueCamp$(1))

    FACTURA.BASEIMPONIBLE = FACTURA.TOTALBRUT - (FACTURA.TOTALBRUT * FACTURA.DTO) / 100
    BASEIMP = FACTURA.BASEIMPONIBLE
    IVA = BASEIMP * (FACTURA.TIPOIVA) / 100
    FACTURA.TOTALIVA = IVA
    FACTURA.TOTALNET = BASEIMP + IVA

    PintaValors
'**************************************************************************************

    GET AREA2T, 1, ALBARAN
    FACTURA.DOCUMENT = "F"
    FACTURA.DADA = FormatD$(Now#, "dd/mm/yyyy")
    FACTURA.CODCLIENT = ALBARAN.CODCLIENT
    FACTURA.PERSONA.NOM = ALBARAN.PERSONA.NOM
    FACTURA.PERSONA.COGNOMS = ALBARAN.PERSONA.COGNOMS
    FACTURA.PERSONA.DNI = ALBARAN.PERSONA.DNI
    FACTURA.PERSONA.DIRECCIO = ALBARAN.PERSONA.DIRECCIO
    FACTURA.PERSONA.POBLACIO = ALBARAN.PERSONA.POBLACIO
    FACTURA.PERSONA.TELEFON1 = ALBARAN.PERSONA.TELEFON1
    FACTURA.PERSONA.TELEFON2 = ALBARAN.PERSONA.TELEFON2
    FACTURA.PERSONA.CPOSTAL = ALBARAN.PERSONA.CPOSTAL
    FACTURA.PERSONA.DTO = ALBARAN.PERSONA.DTO
    FACTURA.PERSONA.FORMAPAGO = ALBARAN.PERSONA.FORMAPAGO
    FACTURA.OBSERVA(1) = "Aquest es un resum d'albarans"
    FACTURA.OBSERVA(2) = "de la data " + DADAI$ + " a " + DADAF$
    FACTURA.MARCAT = " "
    PUT AREA2F, R, FACTURA

    GET AREANUM, 1, DOCNUM
    DOCNUM.FACTNUM(TIPUS%).MAXFACT = DOCNUM.FACTNUM(TIPUS%).MAXFACT + 1
    PUT AREANUM, 1, DOCNUM

    '*****************************************************************
    '  Marca tots els albarans facturats com a facturats
    '*****************************************************************

    FOR RF = 1 TO MAXALBFAC
	LOCK AREASEL, RF
	GET AREASEL, RF, TRANS
	GET AREA2, TRANS.REGREAL, ALBARAN
	ALBARAN.DOCUMENT = "F"
	PUT AREA2, TRANS.REGREAL, ALBARAN
	UNLOCK AREASEL, RF
    NEXT

    '*****************************************************************
    '  Imprimeix factura si ‚s necessari
    '*****************************************************************

    IF VARIMP$ = "Si" THEN
       CALL CarregarAlbaran(R, MAX, AREA3F, AREA2F)
       CALL ImprimirFactura(DEVI$, DP$, MAXALBFAC, AREA5, AREA3T, AREA2T, AREADOC, MI)
    END IF

    Avis.Sonor (1)
    tecla = Avis("AVIS:", "L'operaci¢ s'ha realitzat amb exit! :)", "Pitji una tecla ...", 0)

    GOSUB FI

'************************************************************************
' CERCAR ALBARANS EXISTENTS DEL CLIENT SELECCIONAT
'************************************************************************

CERCAR.ALBARANS:
       AREA2T = FREEFILE: OPEN DIRECC$ + "TEMP_ALB.CAB" FOR RANDOM SHARED AS AREA2T LEN = LEN(ALBARAN)
       AREA3T = FREEFILE: OPEN DIRECC$ + "TEMP_ALB.LIN" FOR RANDOM SHARED AS AREA3T LEN = LEN(LINALBA)
       AREASEL = FREEFILE: OPEN DIRECC$ + "TEMP_ALB.NDX" FOR RANDOM SHARED AS AREASEL LEN = LEN(TRANS)
       OBERTS = 1
       ALB = 0: ALBP = 0: RA = 1
       FOR R = 1 TO MAXAL
	   GET AREA2, R, ALBARAN
	   GET AREA3, R, LINALBA
	   NOMA$ = LTRIM$(RTRIM$(ALBARAN.PERSONA.NOM)) + " " + LTRIM$(RTRIM$(ALBARAN.PERSONA.COGNOMS))
	   IF LTRIM$(RTRIM$(ALBARAN.CODCLIENT)) = LTRIM$(RTRIM$(ValueCamp$(0))) THEN
	      GOSUB COMPROVA
	   ELSE
	      GOSUB COMPROVA
	   END IF
       NEXT

       IF ALB = 0 THEN
	  tecla = Avis("AVIS:", "No hi ha cap albar… d'aquest client", "Pitji una tecla...", 0)
	  GOSUB BORRA.TEMPS
	  C = C - 1
	  RETURN
       ELSE
	  IF ALBP = 0 THEN
	     tecla = Avis("AVIS:", "No hi ha cap albar… pendent de facturar", "Pitji una tecla...", 0)
	     GOSUB BORRA.TEMPS
	     C = C - 1
	     RETURN
	  END IF
	  LOCATE 14, 25: PRINT "   Albarans existents: "; ALB
	  LOCATE 15, 25: PRINT "    Albarans pendents: "; ALBP
       END IF
       RETURN

BORRA.TEMPS:
       CLOSE AREA2T, AREA3T, AREASEL
       KILL DIRECC$ + "TEMP_ALB.CAB"
       KILL DIRECC$ + "TEMP_ALB.LIN"
       KILL DIRECC$ + "TEMP_ALB.NDX"
       OBERTS = 0
       RETURN


COMPROVA:
       IF NOMA$ = NOMT$ THEN
	  ALB = ALB + 1
	  IF ALBARAN.DOCUMENT = "A" AND NOT ALBARAN.MARCAT = "*" THEN
	      PUT AREA2T, RA, ALBARAN
	      PUT AREA3T, RA, LINALBA
	      TRANS.REGREAL = R
	      TRANS.ALBARAN = ALBARAN.REFALBARAN
	      PUT AREASEL, RA, TRANS
	      RA = RA + 1
	      ALBP = ALBP + 1
	  END IF
       END IF
       RETURN

'************************************************************************
' LLISTAR CLIENTS
'************************************************************************

LLISTA.CLIENTS:
       DIM LLISTA$(1 TO MAXCL)
       GetBackground 1, 1, 24, 80, LISBUF$
       SetScoreBoard SOFF: LOCATE , , 0: R = 1
       FOR q = 1 TO MAXCL - 1
	   GET AREA4, q, CLIENT
	   IF CLIENT.MARCAT = "-" THEN
	      CODI$ = CLIENT.CODICLIENT
	      N$ = LTRIM$(RTRIM$(CLIENT.NOM))
	      C$ = LTRIM$(RTRIM$(CLIENT.COGNOMS))
	      S$ = N$ + " " + C$: NOM$ = MID$(S$, 1, 40)
	      LLISTA$(R) = CODI$ + " " + NOM$
	      R = R + 1
	   END IF
       NEXT
       R = R - 1
       ASEL = Achoice(3, 9, 20, 61, R, LLISTA$(), COL(), "Codi       Nom                                     ", 12, "")
       IF ASEL = 0 OR ASEL = -14 OR ASEL = -13 OR ASEL = 999 THEN
	  PutBackground 1, 1, LISBUF$
	  ERASE LLISTA$
	  RETURN
       END IF
       IF ASEL <= 0 THEN ASEL = 1
       GET AREA4, ASEL, CLIENT
       InsertValueCamp 0, CLIENT.CODICLIENT
       PutBackground 1, 1, LISBUF$
       DisplayAllCamps
       ERASE LLISTA$
       RETURN


'************************************************************************

FI:
    DeleteWindow 0
    PackWindows
    SYSTEM

BORRA.BARRA:
    COLOR COL(1, 0), COL(1, 1)
    LOCATE 12, 25: PRINT SPACE$(30)
    LOCATE 13, 25: PRINT SPACE$(30)
    COLOR COL(0, 0), COL(0, 1)
    RETURN



'************************************************************************
' OBRIR FITXERS DE GESTIO NECESSARIS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "ALBARAN.CAB" FOR RANDOM SHARED AS AREA2 LEN = LEN(ALBARAN)
      AREA3 = FREEFILE: OPEN DIRECC$ + "ALBARAN.LIN" FOR RANDOM SHARED AS AREA3 LEN = LEN(LINALBA)
      AREA2F = FREEFILE: OPEN DIRECC$ + "FACTURES.CAB" FOR RANDOM SHARED AS AREA2F LEN = LEN(FACTURA)
      AREA3F = FREEFILE: OPEN DIRECC$ + "FACTURES.LIN" FOR RANDOM SHARED AS AREA3F LEN = LEN(LINFACT)
      AREA4 = FREEFILE: OPEN DIRECC$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CLIENT)
      AREA5 = FREEFILE: OPEN DP$ + "PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA5 LEN = LEN(CAP)
      AREANUM = FREEFILE: OPEN DIRECC$ + "DOCUMENT.NUM" FOR RANDOM SHARED AS AREANUM LEN = LEN(DOCNUM)
      AREADOC = FREEFILE: OPEN DP$ + "PLANTILL\FAC_MSK.LST" FOR RANDOM SHARED AS AREADOC LEN = LEN(TD)

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

      AREA7 = FREEFILE: OPEN DIRECCI$ + FITXER$ FOR RANDOM SHARED AS AREA7 LEN = LEN(ARCHIMP)
      GET AREA7, 1, ARCHIMP
      CLOSE AREA7

'*********************************************************************
' INICIAR EL CONTROL DELS REGISTRES
'*********************************************************************
      GET AREA, 1, CEMPRE
      MAXFA = CEMPRE.MAXFACTURA
      MAXAL = CEMPRE.MAXALBARAN
      MAXCL = CEMPRE.MAXCLIENTS
      RETURN

AJUDA:
    LOCATE 22, 2: PRINT "<F1>=AJUDA         <F2>=LLISTA DE CLIENTS    <F3>=IMPRESIO             "
    LOCATE 23, 2: PRINT "<ESC>=SORTIR                                                        "
    LOCATE 24, 2: PRINT "                                                                    ";
    RETURN

END SUB

REM $STATIC
SUB ImprimirFactura (DEVI$, DIRPLA$, MAXALBS, AREA5, AREA3T, AREA2T, AREADOC, MI)
    SHARED DIRECCT$, CADFE$

    '$DYNAMIC
    DIM CLI(0 TO 7) AS STRING
    DIM FAC(0 TO 8) AS STRING
    DIM LINI(0 TO 5) AS STRING

    CLI(0) = FACTURA.CODCLIENT: CLI(1) = FACTURA.PERSONA.NOM
    CLI(2) = LTRIM$(RTRIM$(FACTURA.PERSONA.COGNOMS)): CLI(3) = "D.N.I. " + FACTURA.PERSONA.DNI
    CLI(4) = RTRIM$(ARCHIMP.NOENSANCHADO) + FACTURA.PERSONA.DIRECCIO: CLI(5) = LTRIM$(RTRIM$(FACTURA.PERSONA.CPOSTAL)) + " " + RTRIM$(LTRIM$(FACTURA.PERSONA.POBLACIO))
    CLI(6) = " Tels." + FACTURA.PERSONA.TELEFON1: CLI(7) = "      " + FACTURA.PERSONA.TELEFON2

    TO$ = FormatC$(FACTURA.TOTALBRUT, "##.###.###"): TOTB$ = TO$ + SPACE$(10 - LEN(TO$))
    BA$ = FormatC$(FACTURA.BASEIMPONIBLE, "##.###.###"): BASE$ = BA$ + SPACE$(10 - LEN(BA$))
    DT$ = FormatC$(FACTURA.DTO, "###"): DTO$ = DT$ + SPACE$(3 - LEN(DT$))
    IV$ = FormatC$(FACTURA.TIPOIVA, "###"): IVA$ = IV$ + SPACE$(3 - LEN(IV$))
    TI$ = FormatC$(FACTURA.TOTALIVA, "##.###.###"): TIVA$ = TI$ + SPACE$(10 - LEN(TI$))

    FAC(0) = RTRIM$(ARCHIMP.ENSANCHADO) + "Factura: " + FACTURA.REFFACTURA + " " + RTRIM$(ARCHIMP.NOENSANCHADO)
    FAC(1) = TOTB$: FAC(2) = BASE$
    FAC(3) = DTO$: FAC(4) = IVA$
    FAC(5) = TIVA$: FAC(6) = FormatD$(FACTURA.TOTALNET, "##.###.###")
    FAC(7) = FACTURA.PERSONA.FORMAPAGO: FAC(8) = ""

    GetBackground 1, 1, 25, 80, FACTU$
    COLOR 15, 2: FINESTRA 10, 30, 14, 45, 1, CAIXA1
    COLOR 15, 2: LOCATE 11, 31: PRINT "ACTUALITZANT"
    COLOR 15, 2: LOCATE 13, 31: PRINT "  FITXERS   "

    ' Crear el fitxer temporal d'impressi¢
    AREATXT = FREEFILE
    OPEN DIRECCT$ + "FACTURA.TXT" FOR OUTPUT SHARED AS AREATXT

    PAG = 1: GET AREA5, 1, CAP
    GET AREADOC, MI, TD

    LI = 1: L = 1
    SALT.PAG% = 1: MSK.PEUSP% = 0
    GOSUB PLANTILLA

    DO
	IF LI >= MAXLINS THEN
	   PRINT #AREATXT, "."
	   FOR LA = LI TO MAXLINS - 2: PRINT #AREATXT, "": NEXT
	   PRINT #AREATXT, "."
	   GOSUB Missatge
	   FOR SALT = 1 TO SALT.F%: PRINT #AREATXT, "": NEXT
	   PAG = PAG + 1: LI = 1
	   SALT.PAG% = 1: GOSUB PLANTILLA
	END IF

	IF LIN$(L, 6) = "*" THEN
	   TIPU$ = "FACTURA": GOSUB IMPRI.LINIA
	   IF UCASE$(MID$(LIN$(L, 1), 1, 6)) = "C.A.:F" AND MID$(LIN$(L, 8), 8, 2) = "-A" THEN
	      FOR RAC = 1 TO MAXALBS
		  GET AREA2T, RAC, ALBARAN
		  IF ALBARAN.REFALBARAN = LIN$(L, 8) THEN EXIT FOR
	      NEXT
	      GET AREA3T, RAC, LINALBA: lalb = 1
	      DO
		 IF LI >= MAXLINS THEN
		    GOSUB Missatge
		    FOR SALT = 1 TO SALT.F%: PRINT #AREATXT, "": NEXT
		    PAG = PAG + 1: LI = 1
		    SALT.PAG% = 1: GOSUB PLANTILLA
		 END IF
		 TIPU$ = "ALBARAN": GOSUB IMPRI.LINIA
		 lalb = lalb + 1: LI = LI + 1
	      LOOP UNTIL LINALBA.LINIA(lalb).MARCAR = "-"

	      IF LIN$(L + 1, 6) <> "-" THEN
		 PRINT #AREATXT, STRING$(76, "-")
		 LI = LI + 1
	      END IF
	   END IF
	   L = L + 1: LI = LI + 1
	END IF
    LOOP UNTIL LIN$(L, 6) = "-"
    FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT

    MSK.PEUSP% = 1: GOSUB PLANTILLA
    CLOSE #AREATXT

    COLOR 31, 2: LOCATE 11, 31: PRINT "  IMPRIMINT  "
    COLOR 31, 2: LOCATE 13, 31: PRINT "   FITXERS   "

    ImprimeixFitxerTXT DIRECCT$ + "FACTURA.TXT", DEVI$, 100
    PutBackground 1, 1, FACTU$
    ERASE FAC, CLI, LINI
    EXIT SUB

'****************************************************************
'  Bucle de lectura de la plantilla
'****************************************************************

PLANTILLA:
    AREAPLA = FREEFILE: NOMP$ = LTRIM$(RTRIM$(DIRPLA$ + "PLANTILL\" + TD.FITXER))
    OPEN NOMP$ FOR INPUT SHARED AS AREAPLA

    '****************************************************************
    DO UNTIL EOF(AREAPLA)
       LINE INPUT #AREAPLA, LINIA$
       LL = LEN(LINIA$): GOSUB BUSCA.INSTR ' Anar a la rutina per a comprovar
       IF MSK.SURT% = 1 THEN EXIT DO       ' la sintaxi.
    LOOP
    '****************************************************************
    MSK.SURT% = 0: SALT.PAG% = 0
    CLOSE #AREAPLA
    RETURN

'****************************************************************
' Rutina per a comprovar sint…xi d'instruccions :)
'****************************************************************

BUSCA.INSTR:
    CL = 1
    WHILE CL < LL
	IF MID$(LINIA$, CL, 1) = "[" THEN
	   TOPE1 = CL + 1: TOPE2 = INSTR(TOPE1, LINIA$, "]") - 1
	   LC = TOPE2 - TOPE1
	   IN$ = MID$(LINIA$, TOPE1, LC + 1)

	   IF MSK.PEUSP% = 1 THEN
	      IF MID$(IN$, 1, 10) = "PEU_PAGINA" THEN
		 MSK.PEUSP% = 0
	      END IF
	   ELSE
	      SELECT CASE UCASE$(IN$)
		  CASE "CAP€ALERA"
		       GOSUB CAPSAC
		  CASE "PAGINA"
		       PRINT #AREATXT, USING "\   \"; FormatD$(PAG, "#.###");
		  CASE "DATA"
		       PRINT #AREATXT, USING "\      \"; FACTURA.DADA;
		  CASE ELSE
		       IF MID$(IN$, 1, 3) = "MAX" THEN MAXLINS = VAL(MID$(IN$, 5, 2))
		       IF MID$(IN$, 1, 6) = "SEPARA" THEN SALT.F% = VAL(MID$(IN$, 8, 2))
		       IF MID$(IN$, 1, 6) = "CLIENT" THEN
			  PRINT #AREATXT, CLI(VAL(MID$(IN$, 8, 1)));
		       ELSE
			  IF MID$(IN$, 1, 3) = "FAC" THEN
			     PRINT #AREATXT, FAC(VAL(MID$(IN$, 5, 1)));
			  ELSE
			    IF MID$(IN$, 1, 5) = "LINIA" THEN
				  MSK.SURT% = 1: RETURN
			    END IF
			  END IF
		       END IF
	      END SELECT
	      CL = CL + LC + 2
	   END IF
	ELSE
	   IF MSK.PEUSP% = 0 AND MID$(LINIA$, 1, 12) <> "[PEU_PAGINA]" THEN PRINT #AREATXT, MID$(LINIA$, CL, 1);
	END IF
	CL = CL + 1
    WEND
    IF MSK.PEUSP% = 0 THEN PRINT #AREATXT, ""
    RETURN

IMPRI.LINIA:
    SELECT CASE TIPU$
	CASE IS = "ALBARAN"
	     LINI(0) = LINALBA.LINIA(lalb).CODART
	     LINI(1) = LINALBA.LINIA(lalb).CONCEPTE
	     LINI(2) = STR$(LINALBA.LINIA(lalb).QUANTI)
	     LINI(3) = STR$(LINALBA.LINIA(lalb).PREU)
	     LINI(4) = STR$(LINALBA.LINIA(lalb).DTO)
	     LINI(5) = STR$(LINALBA.LINIA(lalb).IMPORT)
	CASE IS = "FACTURA"
	     LINI(0) = LIN$(L, 1)
	     LINI(1) = LIN$(L, 2)
	     LINI(2) = LIN$(L, 4)
	     LINI(3) = LIN$(L, 3)
	     LINI(4) = LIN$(L, 7)
	     LINI(5) = LIN$(L, 5)
	CASE ELSE
    END SELECT

    IF UCASE$(MID$(LIN$(L, 1), 1, 6)) = "C.A.:F" AND MID$(LIN$(L, 8), 8, 2) = "-A" AND TIPU$ = "FACTURA" THEN
       PRINT #AREATXT, RTRIM$(ARCHIMP.NEGRITA) + "          " + LINI(1) + RTRIM$(ARCHIMP.NONEGRITA)
    ELSE
       IF VAL(LINI(2)) = 0 THEN
	  QUANT$ = "        "
       ELSE
	  q$ = LTRIM$(FormatD$(VAL(LINI(2)), "###.###,0")): QUANT$ = SPACE$(9 - LEN(q$)) + q$
       END IF
       p$ = FormatC$(VAL(LINI(3)), "#.###.###"): PREU$ = SPACE$(9 - LEN(p$)) + p$
       I$ = FormatC$(VAL(LINI(5)), "##.###.###"): IMPORT$ = SPACE$(10 - LEN(I$)) + I$

       PRINT #AREATXT, USING "\       \ "; QUANT$;
       PRINT #AREATXT, " " + LINI(1);
       PRINT #AREATXT, USING "\       \"; PREU$;
       PRINT #AREATXT, USING "\\"; FormatC$(VAL(LINI(4)), "##");
       PRINT #AREATXT, USING "  \        \"; IMPORT$
    END IF
    RETURN

CAPSAC:
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + RTRIM$(CAP.LINIES(0)) + RTRIM$(ARCHIMP.NOENSANCHADO)
    FOR I = 1 TO 4: PRINT #AREATXT, CAP.LINIES(I): NEXT
    RETURN

Missatge:
    PRINT #AREATXT, ""
    PRINT #AREATXT, ""
    PRINT #AREATXT, "                      SUMA I CONTINUA A LA P…GINA SEGšENT...."
    PRINT #AREATXT, ""
    PRINT #AREATXT, ""
    PRINT #AREATXT, ""
    RETURN

END SUB

FUNCTION IniciaRef (AREANUM, R, MARCAT, REFOLD$)
	  DIM OP$(1 TO 10)

	  GET AREANUM, 1, DOCNUM
	  MAXDOC% = DOCNUM.MAXNUM
	  FOR RE = 1 TO MAXDOC%: OP$(RE) = DOCNUM.FACTNUM(RE).CONFACT: NEXT
	  CALL Menu2(OP$(), CASO%, 10, 10, LEN(OP$(1)) + 2, MAXDOC%, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))

	  IF CASO% = 999 OR CASO% = 888 THEN
	     IniciaRef = CASO%
	     ERASE OP$
	     EXIT FUNCTION
	  ELSE
	     REF$ = DOCNUM.FACTNUM(CASO%).NUMFACT
	     ALB$ = LTRIM$(STR$(DOCNUM.FACTNUM(CASO%).MAXFACT))
	  END IF

	  L2 = 6
	  FOR L = LEN(ALB$) TO 1 STEP -1
	      MID$(REF$, L2, 1) = MID$(ALB$, L, 1): L2 = L2 - 1
	  NEXT
	  MID$(REF$, 1, 2) = MID$(FormatD$(Now#, "dd/mm/yy"), 7, 2)

	  IniciaRef = CASO%
	  FACTURA.REFFACTURA = REF$
	  ERASE OP$
END FUNCTION

REM $STATIC
SUB InitWindows
    IF SetInitWindows(1) THEN
       tecla = Avis("Error 001W:", "Error al inicialitzar les finestres", "Pitja una tecla", 0)
       RESET: SYSTEM
    END IF

    WIN1 = InitNewWindow(0, 7, 20, 19, 60, 1, "Facturar albarans")
    SetColorWindow 0, COL(0, 0), COL(0, 1), COL(1, 1), COL(1, 0), COL(0, 0), COL(0, 1)
    SetStyleWindow 0, 0, ""
END SUB

SUB PintaValors
    TOTEUROS = FACTURA.TOTALNET / 166.386

    COLOR COL(0, 0), COL(0, 1)
    COLOR COL(2, 0), COL(2, 1): LOCATE 9, 12: PRINT "Subtotal:";
    COLOR COL(0, 0), COL(0, 1): PRINT USING "#,###,###.##"; FACTURA.TOTALBRUT
    COLOR COL(2, 0), COL(2, 1): LOCATE 9, 33: PRINT "      DTO %:";
    COLOR COL(0, 0), COL(0, 1): PRINT FACTURA.DTO
    COLOR COL(2, 0), COL(2, 1): LOCATE 10, 33: PRINT "      IVA %:";
    COLOR COL(0, 0), COL(0, 1): PRINT FACTURA.TIPOIVA
    COLOR COL(2, 0), COL(2, 1): LOCATE 12, 33: PRINT "Total (PTS):";
    COLOR COL(0, 0), COL(0, 1): PRINT USING "#,###,###.##"; FACTURA.TOTALNET
    COLOR COL(2, 0), COL(2, 1): LOCATE 13, 33: PRINT "Total (EUR):";
    COLOR COL(0, 0), COL(0, 1): PRINT USING "#,###,###.##"; TOTEUROS
END SUB

