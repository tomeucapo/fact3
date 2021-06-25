' ********************************************************************
'
' Fitxer....................: RESET.BAS
' Titol.....................: Utilitat per a fer un reset del fitxers
'                             de gesti¢.
' ********************************************************************
'
' Data inici................: 08/11/1996 16:40:00 (Data del FACT2)
' Data de la darrera revisi¢: 22/12/1997 17:13:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/97 (C)
'                             Summer'96 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT3\FONTS\RESGUARD.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'

DIM COLORS AS COLO
DIM SHARED col(2, 1)
DIM SHARED CEMPRE AS CONTROLEMP
DIM SHARED RESGUA AS RESG
DIM SHARED CFG AS CONFIG
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
DIM SHARED FITXERS(100) AS STRING * 12
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
	  ca2$ = ""
	  FOR L% = 1 TO LEN(CAD$)
	      ca2$ = ca2$ + ENCRIPT$(MID$(CA$, L%, 1), L%)
	  NEXT

	  IF LTRIM$(RTRIM$(ca2$)) <> "RESET.EXE*+," THEN
	     tecla = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "Pitji una tecla", 0)
	     SYSTEM
	  END IF

	  KILL TMP$ + "PASS.TMP"
	  KILL TMP$ + "PASU.TMP"
	  KILL TMP$ + "PASE.TMP"
	  KILL TMP$ + "PROT.TMP"

      ON ERROR GOTO ERRORS
      UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
      IMP$ = LTRIM$(RTRIM$(CFG.DIMPO))
      DBF$ = LTRIM$(RTRIM$(CFG.DDADE))             ' Assignar direcctoris
      SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
      MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
      HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
      SYS$ = LTRIM$(RTRIM$(CFG.DSIST))

      DIRECCIM$ = IMP$     ' Subdirecctori de les base de dades
      DIRECCS$ = SYS$      ' Subdirecctori de les base de dades
      DIRECCF$ = DBF$     ' Subdirecctori de les base de dades
      DIRECCR$ = MSK$

      MAXLINS = USR.MAXLINS
      MAXFAC = USR.LINALBA
      IVA = EMPRES.IVA
      DTO = EMPRES.DTO
      R = EMPRES.ANY
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA

      SetDirRecursos (DIRECCR$)
      
      AREAC = FREEFILE: OPEN DIRECCF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
      GET AREAC, 1, COLORS
      col(0, 0) = COLORS.col(0, 0): col(0, 1) = COLORS.col(0, 1)
      col(1, 0) = COLORS.col(1, 0): col(1, 1) = COLORS.col(1, 1)
      col(2, 0) = COLORS.col(2, 0): col(2, 1) = COLORS.col(2, 1)
      CLOSE AREAC
     
      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, R, ANYS
      DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"
      CLOSE AREAA
      
      COLOR col(0, 0), col(0, 1)

      FINESTRA 5, 1, 20, 79, 1, CAIXA1
      COLOR 1, col(0, 0)
      CENTRAR 5, " Utilitat per instal.lar els fitxers de gestio ": COLOR col(0, 0), col(0, 1)
      GOSUB SINO
      COLOR col(0, 0), col(0, 1)
      FINESTRA 5, 1, 20, 79, 1, CAIXA1
      COLOR 1, col(0, 0)
      CENTRAR 5, " Utilitat per instal.lar els fitxers de gestio ": COLOR col(0, 0), col(0, 1)

      LOCATE 6, 2: PRINT "Preparant fitxers... ";
      LOCATE 7, 2: PRINT "          Canvi de direcctori";
      CHDIR MID$(DIRECCF$, 1, LEN(DIRECCF$) - 1): COLOR 26: PRINT " Ok"
      COLOR col(0, 0)
      LOCATE 8, 2: PRINT "          Processant fitxers"
      LOCATE 9, 2: PRINT "                     Buidant array";

      FOR P = 1 TO 100: FITXERS(P) = "": NEXT

      COLOR 26: PRINT " Ok": COLOR col(0, 0)
      LOCATE 10, 2: PRINT "                     Passant fitxers ";
      X$ = DIR$("*.*")
      P = 1
      DO WHILE X$ <> ""
	 FITXERS(P) = X$
	 X$ = DIR$
	 P = P + 1
      LOOP
      P = P - 1: COLOR 26: PRINT " Ok": COLOR col(0, 0)
      
      LOCATE 11, 2: PRINT "Fent una copia de seguretat ..."
      GetBackground 1, 1, 25, 79, FACTU$
      FOR COP = 1 TO P
	  TANT% = 100 * COP / P
	  LOCATE 12, 2: PRINT STRING$(70, " ");
	  LOCATE 23, 2: PRINT STRING$(70, " ");
	  LOCATE 12, 2: PRINT "Copiant " + FITXERS(COP): LOCATE 12, 29: PRINT "Fet:"; : COLOR col(2, 0): PRINT TANT%; : COLOR col(2, 0): PRINT " %"
	  SHELL DIRECCS$ + "ICOP " + DIRECCF$ + FITXERS(COP) + " " + DIRECCIM$ + FITXERS(COP)
      NEXT

      CLS
      PRINT "COMPRIMINT COPIA DE SEGURETAT ..."
      PRINT
      SHELL DIRECCS$ + "PKZIP " + DIRECCIM$ + RTRIM$(LTRIM$(STR$(DTN(FormatD$(Now#, "dd/mm/yy"))))) + ".ZIP " + DIRECCIM$ + "*.*"
      NAM$ = RTRIM$(LTRIM$(STR$(DTN(FormatD$(Now#, "dd/mm/yy"))))) + ".ZIP"
      SHELL DIRECCS$ + "ICOP " + DIRECCIM$ + NAM$ + " " + TMP$ + NAM$
      FOR D = 1 TO P
	  KILL DIRECCIM$ + FITXERS(D)
      NEXT
      SHELL DIRECCS$ + "ICOP " + TMP$ + NAM$ + " " + DIRECCIM$ + NAM$
      SHELL DIRECCS$ + "ATB " + DIRECCIM$ + RTRIM$(LTRIM$(STR$(DTN(FormatD$(Now#, "dd/mm/yy"))))) + ".ZIP /R"

      c$ = INPUT$(1)
      PutBackground 1, 1, FACTU$
      
      LOCATE 13, 2: PRINT "Copia de segetat realitzada. ": COLOR col(0, 0)
      LOCATE 14, 1: PRINT "ºBorrant els fitxers del direcctori " + DBF$;
      KILL DIRECCF$ + "*.*": COLOR 26: PRINT " Ok.": COLOR col(0, 0)
      SHELL DIRECCS$ + "ICOP " + DIRECCIM$ + "DOCUMENT.NUM " + DIRECCF$ + "DOCUMENT.NUM"

      LOCATE 16, 1: PRINT "ºInicialitzant fitxers... ";
      OPEN DIRECCF$ + "CONTROL.DSF" FOR RANDOM SHARED AS 4 LEN = LEN(CEMPRE)
      OPEN DIRECCF$ + "RESGUARD.DAT" FOR RANDOM SHARED AS 5 LEN = LEN(RESGUA)
      OPEN DIRECCF$ + "ENTRADES.DAT" FOR RANDOM SHARED AS 6 LEN = 10
      CEMPRE.MAXALBARAN = 1
      CEMPRE.MAXFACTURA = 1
      CEMPRE.MAXSTOCK = 1
      CEMPRE.MAXCLIENTS = 1
      CEMPRE.MAXPROVEID = 1
      CEMPRE.MAXCOMANDE = 1
      PUT 4, 1, CEMPRE
      RESGUA.CPY = "Fitxer dels RESGUARDS. Facturaci¢ 3.0" + CHR$(26)
      RESGUA.MAXREG = 1
      PUT 5, 1, RESGUA: NE = 1
      PUT 6, 1, NE
      CLOSE 1, 2, 3, 4, 5, 6
      LOCATE 16, 1: PRINT "ºInicialitzant fitxers... ";
      COLOR 26: PRINT "Ok.": COLOR col(0, 0): PRINT
      LOCATE 23, 2: PRINT SPACE$(78)
      LOCATE 18, 1: PRINT "ºPitja una tecla per continuar...": c$ = INPUT$(1)
      SYSTEM

SINO:
      MENSA$ = " Esta segur de fer el reset ? (S/N)......: [ ]"

      COLOR col(0, 0), col(0, 1)
      FINESTRA 10, 9, 14, 60, 1, CAIXA1
      LOCATE 12, 11: PRINT MENSA$
      SetMaxCamps 0
      SetColorCamp 0, col(1, 0), col(1, 1), col(0, 0), col(0, 1), col(2, 0), col(2, 1)
      SetInitCamp 0, 12, 11 + LEN(MENSA$) - 2, ASCI, 0, "X", ""
      DeleteCamp 0
      InsertValueCamp 0, "N"
      DisplayAllCamps

      LOCATE , , 1, 14, 15
      IF ReadCamp(0) = SALIR THEN SYSTEM
      IF UCASE$(ValueCamp(0)) = "N" THEN
	 SYSTEM
      ELSE
	 IF UCASE$(ValueCamp(0)) = "S" THEN
	    RETURN
	 ELSE
	    SYSTEM
	 END IF
      END IF
      RETURN

ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

