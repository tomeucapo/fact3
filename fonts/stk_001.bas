' ********************************************************************
'
' Fitxer....................: STK_001.BAS
' Titol.....................: Modul del control de minims en l'stock.
'                    
' ********************************************************************
'
' Data inici................: 15/02/1996
' Data de la darrera revisi¢: 25/03/1997 17:03:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB ControlMinims (DIRECCP$, DIRECC$, DIRECI$, DP$, DEV$, IMPRESORA!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STOCK.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'

COMMON SHARED DIRECC$, DIRCP$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT

DIM SHARED STOCK AS STK
DIM SHARED COLORS AS COLO
DIM SHARED EMPRES AS EMPRESA
DIM SHARED CEMPRE AS CONTROLEMP       '     "      DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CFG AS CONFIG              '     "      DEL FITXER DE CONFIGURACI¢
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAP€ALERAS DE DOCUMENTS
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED CTRL AS CONTROL
DIM SHARED COL(2, 1)
DIM SHARED USR AS USUARIS
DIM SHARED PASO AS TRANS
DIM SHARED ANYS AS ANNO

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

	  IF LTRIM$(RTRIM$(CA2$)) <> "STK_001.EXE" THEN
	     tecla = AVIS("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "Pitji una tecla...", 0)
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
      MAXFAC = USR.LINFACT
      IVA = EMPRES.IVA
      DTO = EMPRES.DTO
      R = EMPRES.ANY
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA

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

      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, R, ANYS

      DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"
      CLOSE AREAA


      CALL ControlMinims(DIRECCP$, DIRECCF$, DIRECCI$, DP$, DEV$, IMPRESORA)
      SYSTEM

SUB ControlMinims (DIRECCP$, DIRECC$, DIRECI$, DP$, DEV$, IMPRESORA)
    GOSUB OBRIRFITXERS
    DIM LINIA$(MAXST - 1)

    IF MAXST = 1 THEN
       BEEP: tecla = AVIS("AVIS:", "El fitxer del megatzem est… buit.", "Pitja una tecla...", 0)
       SYSTEM
    END IF

    L = 1: TITOL$ = "Codi Article      Descripci¢                               Stock M. Stock A. "

    FOR R = 1 TO MAXST - 1
	GET AREA3, R, STOCK
	IF STOCK.EXISTENCIA < STOCK.STOCKMIN THEN
	   exi$ = RTRIM$(STR$(STOCK.STOCKMIN))
	   LINIA$(L) = STOCK.CODI + STOCK.DESCRIPCIO + " " + exi$ + SPACE$(9 - LEN(exi$)) + LTRIM$(STR$(STOCK.EXISTENCIA))
	   L = L + 1
	END IF
    NEXT
    LOCATE , , 0
    DO
       STK = ACHOICE(4, 1, 20, 79, L - 1, LINIA$(), COL(), TITOL$, 19, "")
       SELECT CASE STK
	      CASE IS = -1
		   SHELL "BKHLP CONTROL_MINIMS"
	      CASE ELSE
       END SELECT
    LOOP UNTIL STK = 0

    ERASE LINIA$, COL
    CLOSE AREA, AREA3, AREA4
    EXIT SUB

'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE STOCK
'************************************************************************

OBRIRFITXERS:

      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA3 = FREEFILE: OPEN DIRECC$ + "STOCK.DAT" FOR RANDOM SHARED AS AREA3 LEN = LEN(STOCK)
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
      MAXAL = CEMPRE.MAXALBARAN
      MAXCL = CEMPRE.MAXCLIENTS
      MAXST = CEMPRE.MAXSTOCK
      RETURN
END SUB

