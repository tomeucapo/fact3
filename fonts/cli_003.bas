' ********************************************************************
'
' Fitxer....................: CLI_003.BAS
' Titol.....................: Modul per ordenar els clients.
'
' ********************************************************************
'
' Data inici................: 26/07/1996
' Data de la darrera revisi¢: 24/03/1997 19:33:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************


DECLARE SUB MASCCLIS ()
DECLARE SUB OrdenarClient (DIRECCP$, DIRECC$, DIRECI$, IMPRESORA!, DP$, DEV$)
DECLARE FUNCTION FindClient% (CAMP$, INDEX() AS ANY, MAXCL!)
DECLARE SUB SortClients (INDEX() AS ANY, MAXCL!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
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

	  IF LTRIM$(RTRIM$(CA2$)) <> "CLI_003.EXE" THEN
	     TECLA = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "PITJA UNA TECLA", 0)
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

      CALL OrdenarClient(DIRECCP$, DIRECCF$, DIRECCI$, IMPRESORA, DP$, DEV$)
      SYSTEM

ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

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
    COLOR 15, 9
    FINESTRA 4, 1, 20, 80, 1, CAIXA1: COLOR 14
    LOCATE 5, 2: PRINT "Codi       Nom Client" + SPACE$(11) + "Cognoms" + SPACE$(30) + "Baixa": COLOR 15
    LOCATE 6, 2: PRINT STRING$(78, "Ä");
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<" + CHR$(25) + ">=BAIXAR     <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR   <F4>=CONSULTAR"
    LOCATE 24, 2: PRINT "<F6>=LLISTAR";
END SUB

SUB OrdenarClient (DIRECCP$, DIRECC$, DIRECI$, IMPRESORA, DP$, DEV$) STATIC
    SHARED RNDX

    ON ERROR GOTO ERRORS
    GOSUB OBRIRFITXERS
    DIM INDEX(1 TO MAXCL) AS INDEXCLI
    DIM NDXCLIENT AS INDEXCLI
    DIM MENU(1 TO 3) AS STRING, MISS$(0 TO 2, 0 TO 2)
    DIM ORDCLIENTS(1 TO MAXCL) AS ORDCLI
    DIM NOMF$(1 TO MAXCL)

    IF MAXCL = 1 THEN
       X = 7: R = 1: L = 1
       BEEP: TECLA = Avis("AVIS:", "El fitxer de clients est… buit.", "Pitja una tecla", 0)
       GOSUB TANCA
    END IF

    GOSUB INDEXAR
MENU:
  DO
    MENU(1) = "ALFABŠTIC NOM         ": MISS$(0, 0) = "Ordena els clients per ordre alfabŠtic"
    MENU(2) = "ALFABETIC COGNOMS     ": MISS$(1, 0) = "Ordena per ordre de cognoms"
    MENU(3) = "CODI (MENOR->MAJOR)   ": MISS$(2, 0) = "Ordena els clients per codi de menor a major"
    
    CALL Menu2(MENU(), CASO%, 6, 30, LEN(MENU(1)) + 2, 3, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
    SELECT CASE CASO%
	   CASE 1
		FILTRE = 0: GOSUB ORDENAR
	   CASE 3
		FILTRE = 1: GOSUB ORDENAR
	   CASE IS = 999, -4, -19
		GOSUB TANCA
	   CASE ELSE
    END SELECT
  LOOP
    

ORDENAR:
	  FOR R = 1 TO MAXCL
	      TANT% = (R * 100) / (MAXCL - 1)
	      FinestraEstat "Volcant els clients a mem•ria: " + LTRIM$(STR$(TANT%)) + "% ", 0
	      GET AREA3, R, CLIENTS
	      SELECT CASE FILTRE
		     CASE 0
			  NO$ = RTRIM$(LTRIM$(CLIENTS.NOM)) + " " + RTRIM$(LTRIM$(CLIENTS.COGNOMS))
			  N$ = MID$(NO$, 1, 40)
			  NOMF$(R) = N$
		     CASE 1
			  NO$ = RTRIM$(LTRIM$(CLIENTS.CODICLIENT))
			  NOMF$(R) = NO$

		     CASE ELSE
	      END SELECT
	  NEXT
	  COLOR COL(0, 0), COL(0, 1)
	  FINESTRA 10, 19, 14, 61, 1, CAIXA1
	  LOCATE 11, 20: PRINT "       *** Ordenant dins mem•ria ***     "
	  LOCATE 12, 20: PRINT "                                         "
	  COLOR 27, COL(0, 1): LOCATE 13, 20: PRINT "          Esperi uns minuts ...          "
	  
	  CALL DIMSort(NOMF$(), MAXCL - 1): COLOR COL(0, 0), COL(0, 1)

	  FOR R = 1 TO MAXCL - 1
	      TANT% = (R * 100) / (MAXCL - 1)
	      FinestraEstat "Col.locant els registres ordenats: " + LTRIM$(STR$(TANT%)) + "% ", 0
	      
	      FOR E = 1 TO MAXCL - 1
		  GET AREA3, E, CLIENTS
		  SELECT CASE FILTRE
			 CASE 0
			      NO$ = RTRIM$(LTRIM$(CLIENTS.NOM)) + " " + RTRIM$(LTRIM$(CLIENTS.COGNOMS))
			      N$ = MID$(NO$, 1, 40)
			      IF NOMF$(R) = N$ THEN
				 RE = E
				 EXIT FOR
			      END IF
			 CASE 1
			      N$ = RTRIM$(LTRIM$(CLIENTS.CODICLIENT))
			      IF NOMF$(R) = N$ THEN
				 RE = E
				 EXIT FOR
			      END IF

			 CASE ELSE
		  END SELECT
	      NEXT

	      PUT AREACOPIA, R, CLIENTS
	  NEXT
	  FOR RI = 1 TO MAXCL                   ' COL.LOCAR A MEM•RIA L'INDEX I LA CLAU
	      TANT% = (RI * 100) / (MAXCL)
	      FinestraEstat "Actualitzant els indexos: " + LTRIM$(STR$(TANT%)) + "% ", 0
	      GET AREACOPIA, RI, CLIENTS
	      NDXCLIENT.REGISTRE = RI
	      NDXCLIENT.CODI = CLIENTS.CODICLIENT
	      PUT AREA2, RI, NDXCLIENT
	  NEXT
	  GOSUB INDEXAR
	  RESET
	  IF DIR$(DIRECC$ + "CLIENTS.DAD") <> "" THEN KILL DIRECC$ + "CLIENTS.DAD"
	  IF DIR$(DIRECC$ + "CLIENTS.NTX") <> "" THEN KILL DIRECC$ + "CLIENTS.NTX"
	  SHELL "COPY " + DIRECC$ + "CLIENTS.DAT " + DIRECC$ + "CLIENTS.DAD >nul": KILL DIRECC$ + "CLIENTS.DAT"
	  SHELL "COPY " + DIRECC$ + "CLIENTS.NDX " + DIRECC$ + "CLIENTS.NTX >nul"
	  SHELL "COPY " + DIRECC$ + "CLIENTS.OLD " + DIRECC$ + "CLIENTS.DAT >nul"
	  IF DIR$(DIRECC$ + "CLIENTS.OLD") <> "" THEN KILL DIRECC$ + "CLIENTS.OLD"
	  GOSUB OBRIRFITXERS
	  TECLA = Avis("", "L'operaci¢ s'ha realitzat amb exit...", "Polsa una tecla per continuar", 0)
	  GOSUB TANCA
	  EXIT SUB


'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE CLIENTS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "CLIENTS.NDX" FOR RANDOM SHARED AS AREA2 LEN = LEN(NDXCLIENT)
      AREA3 = FREEFILE: OPEN DIRECC$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA3 LEN = LEN(CLIENTS)
      AREACOPIA = FREEFILE: OPEN DIRECC$ + "CLIENTS.OLD" FOR RANDOM SHARED AS AREACOPIA LEN = LEN(CLIENTS)
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
      RESET
      EXIT SUB

ACTUALIZ.INDEX:
      REDIM PRESERVE INDEX(1 TO MAXCL) AS INDEXCLI
      INDEX(RNDX).REGISTRE = RNDX
      INDEX(RNDX).CODI = CLIENTS.CODICLIENT
      RETURN
END SUB

