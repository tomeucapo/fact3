' ********************************************************************
'
' Fitxer....................: LAB_001.BAS
' Titol.....................: Modul per generar etiquetes dels clients.
'                  
' ********************************************************************
'
' Data inici................: 27/06/1996
' Data de la darrera revisi¢: 07/10/1997 19:27:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************
'

DECLARE SUB ImprimirEtiquetes (DIRECCP$, DIRECC$, DIRECI$, DIRECCT$, DIRECCR$, DP$, IMPRESORA!, DEV$)

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
DIM COLORS AS COLO
DIM SHARED COL(2, 1)
DIM USR AS USUARIS
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

	  IF LTRIM$(RTRIM$(CA2$)) <> "LAB_001.EXE" THEN
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


      CALL ImprimirEtiquetes(DIRECCP$, DIRECCF$, DIRECCI$, DIRECCT$, DIRECCR$, DP$, IMPRESORA, DEV$)
      SYSTEM


ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

SUB ImprimirEtiquetes (DIRECCP$, DIRECC$, DIRECI$, DIRECCT$, DIRECCR$, DP$, IMPRESORA, DEV$) STATIC
    SHARED RNDX
    SHARED COL()

    ON ERROR GOTO ERRORS
    GOSUB OBRIRFITXERS

    DIM INDEX(1 TO MAXCL) AS INDEXCLI
    DIM NDXCLIENT AS INDEXCLI
    DIM MENU(1 TO 2) AS STRING

    IF MAXCL = 1 THEN
       X = 7: R = 1: L = 1
       BEEP: tecla = AVIS("AVIS:", "El fitxer de clients est… buit.", "Piji una tecla...", 0)
       GOSUB TANCA
    END IF

    GOSUB INDEXAR
    GetBackground 1, 1, 24, 79, cli$
    LOADMASK (DIRECCR$ + "LAB_001.MSK")
						  
    SetMaxCamps 4: ES = 4: MAXC = 12: COL = 80
    FOR W = 0 TO 4: SetColorCamp W, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
    SetInitCamp 0, 10, 40, NUM, 0, "999", ""
    SetInitCamp 1, 11, 40, NUM, 0, "999", ""
    SetInitCamp 2, 12, 40, NUM, 0, "999", ""
    SetInitCamp 3, 13, 40, ASCI, 0, "999", ""
    SetInitCamp 4, 15, 22, ASCI, 0, STRING$(28, "X"), ""
    
    InsertValueCamp 0, LTRIM$(STR$(7)): InsertValueCamp 1, LTRIM$(STR$(ES))
    InsertValueCamp 2, LTRIM$(STR$(MAXC)): InsertValueCamp 4, DEV$: InsertValueCamp 3, LTRIM$(STR$(COL))
    DisplayAllCamps
    
    FOR C = 0 TO 4
	VALUE = ReadCamp(C)
	SELECT CASE VALUE
	       CASE F1 TO F10
		    C = C - 1: SOUND 50, .5
	       CASE 999
		    GOSUB TANCA
	       CASE ELSE
	END SELECT
    NEXT

    IF LTRIM$(ValueCamp$(4)) <> DEV$ THEN DEV$ = LTRIM$(ValueCamp$(4))
    IF VAL(LTRIM$(ValueCamp$(3))) <> COL THEN COL = VAL(LTRIM$(ValueCamp$(3)))

    SOUND 300, 1: SOUND 100, 2: SOUND 300, 1
    tecla = AVIS("AVIS:", "Prepari l'impresora amb paper d'etiquetar.", "Pitji una tecla...", 0)

    MENU(1) = "  SI  ": MENU(2) = "  NO  "
    FINESTRA 10, 10, 17, 33, 1, CAIXA1
    LOCATE 11, 11: PRINT "Preparat per imprimir:"
    CALL MENU2(MENU(), CASO%, 12, 14, LEN(MENU(1)) + 2, 2, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
    SELECT CASE CASO%
	   CASE 1
		GOSUB IMPRIMEIX
	   CASE 2, 999
		GOSUB TANCA
	   CASE ELSE
    END SELECT
    GOSUB TANCA

IMPRIMEIX:
    COLOR 15, 9: FINESTRA 10, 30, 14, 45, 1, CAIXA1
    COLOR 31, 9: LOCATE 11, 31: PRINT "ACTUALITZANT"
    COLOR 31, 9: LOCATE 13, 31: PRINT "  FITXERS   "

    AREATXT = FREEFILE
    OPEN DIRECCT$ + "LAB_CLI.TXT" FOR OUTPUT SHARED AS AREATXT

    R = 1: CLIS = 1
    DO
	IF CLIS >= MAXC THEN
	   PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
	   CLIS = 1
	END IF
	GOSUB CAPSA
    LOOP UNTIL R >= MAXCL - 1

    CLOSE #AREATXT

    COLOR 31, 9: LOCATE 11, 31: PRINT "  IMPRIMINT  "
    COLOR 31, 9: LOCATE 13, 31: PRINT "   FITXERS   "

    ImprimeixFitxerTXT DIRECCT$ + "LAB_CLI.TXT", DEV$, COL
    PutBackground 1, 1, cli$
    RETURN

CAPSA:
      GET AREA3, R + 1, CLIENTS
      NOM$ = CLIENTS.NOM: COG$ = CLIENTS.COGNOMS: DIRE$ = CLIENTS.DIRECCIO
      PP2$ = MID$(CLIENTS.POBLACIO, 1, 20): CP$ = CLIENTS.CPOSTAL
      PRO$ = CLIENTS.PROVINCIA
      GET AREA3, R, CLIENTS: PP1$ = MID$(CLIENTS.POBLACIO, 1, 20)
      PRINT #AREATXT, ""
      PRINT #AREATXT, "     "; CLIENTS.NOM; "                  "; NOM$
      PRINT #AREATXT, "     "; CLIENTS.COGNOMS; "        "; COG$
      PRINT #AREATXT, "     "; CLIENTS.DIRECCIO; "        "; DIRE$
      PRINT #AREATXT, "     "; PP1$; " "; CLIENTS.CPOSTAL; "          "; PP2$; " "; CP$
      PRINT #AREATXT, "     "; CLIENTS.PROVINCIA; "          "; PRO$
      R = R + 2: CLIS = CLIS + 2
      FOR S = 1 TO ES
	  PRINT #AREATXT, ""
      NEXT
      RETURN

'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE CLIENTS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "CLIENTS.NDX" FOR RANDOM SHARED AS AREA2 LEN = LEN(NDXCLIENT)
      AREA3 = FREEFILE: OPEN DIRECC$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA3 LEN = LEN(CLIENTS)
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
      EXIT SUB

END SUB

