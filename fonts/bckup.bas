' ********************************************************************
'
' Fitxer....................: BCKUP.BAS
' Titol.....................: Modul per fer un BackUp de les Bases De
'                             Dades.
'
' ********************************************************************
'
' Data inici................: 11/09/1996
' Data de la darrera revisi¢: 04/01/1999 13:50:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/99 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE FUNCTION DirToArray! (MASK$)
DECLARE FUNCTION DISKSPACE! (DRIVE$)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'D:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'D:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'D:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'D:\FACT3\FONTS\CONSTS.BI'

TYPE CATALOGO
     CPY AS STRING * 35
     FECHA AS STRING * 8
     HORA AS STRING * 8
     CFG AS CONFIG
     CEMPRES AS EMPRESA
END TYPE

'$DYNAMIC
DIM SHARED FILE$(0 TO 1)
DIM SHARED CAT AS CATALOGO
DIM SHARED CFG AS CONFIG
DIM TEXTOSMENUP(5) AS STRING
DIM SHARED CEMPRE AS CONTROLEMP
DIM SHARED SPOOLIMP AS SPOOL
DIM SHARED ARCHIMP AS IMPRESORA
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
DIM SHARED USR AS USUARIS
DIM SHARED COLORS AS COLO
DIM SHARED COL(2, 1)
DIM SHARED PASO AS TRANS
DIM SHARED ANYS AS ANNO
      
      
      '**********************************************************
      '**********************************************************
      '**********************************************************

      GOSUB OBRI
      ON ERROR GOTO ERRORS

      DO
	TEXTOSMENUP(1) = "Fer copia de seguretat      "
	TEXTOSMENUP(2) = "Restaurar copia de seguretat"

	CALL Menu2(TEXTOSMENUP(), CASO%, 5, 31, LEN(TEXTOSMENUP(1)) + 2, 2, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
	GetBackground 1, 1, 24, 80, ME$
	SELECT CASE CASO%
	       CASE 1
		    GOSUB BCKUP
	       CASE 2
		    GOSUB RESTOREB
	       CASE 999
		    RESET
		    SYSTEM
	       CASE ELSE
	END SELECT
      LOOP
' ********************************************************************
'                              BACKUP
' ********************************************************************

BCKUP:
      DIM ANY$(1 TO M% + 1)

      FOR R = 1 TO M%
	  GET AREAA, R, ANYS
	  IF ANYS.DATAT <> "../../...." THEN FIN$ = "*" ELSE FIN$ = "<"
	  ANY$(R) = ANYS.ANY + " " + ANYS.DATAO + "  " + ANYS.HORAO + " " + FIN$
      NEXT
      
      ANY$(R) = "Un altre direcctori...        "
      GetBackground 10, 10, 18, 43, L$
      ANYSE = Achoice(10, 10, 17, 42, R, ANY$(), COL(), "Any  Data Inici  Hora Inici   ", 4, "")

      IF ANYSE = 0 OR ANYSE = -14 OR ANYSE = -13 THEN
	 ERASE ANY$
	 PutBackground 1, 1, ME$
	 RETURN
      END IF

      DOLD$ = DIRECCF$
      IF ANY$(ANYSE) <> "Un altre direcctori...        " THEN
	 GET AREAA, ANYSE, ANYS
	 DIRECCF$ = DF$ + ANYS.ANY + "\"
      ELSE
	 COLOR COL(0, 0), COL(0, 1)
	 FINESTRA 10, 9, 12, 66, 1, CAIXA1
	 LOCATE 11, 11: PRINT " Direcctori: [                                        ]"
	 LOCATE 13, 11: PRINT MENSA$
	 SetMaxCamps 0
	 SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
	 SetInitCamp 0, 11, 25, ASCI, 0, STRING$(25, "X"), ""
	 DeleteCamp 0
	 DisplayAllCamps
	 LOCATE , , 1, 14, 15
	 FOR C = 0 TO MAXCAMPS
	     VALUE = ReadCamp(C)
	     SELECT CASE VALUE
		    CASE 0
		      IF ValueCamp$(0) = "" THEN BEEP: C = C - 1
		    CASE F2 TO F10
		      C = C - 1
		    CASE 999
		      DIRECCF$ = DOLD$
		      ERASE ANY$
		      PutBackground 1, 1, ME$
		      RETURN
		    CASE ELSE
	     END SELECT
	  NEXT
	  DIRECCF$ = LTRIM$(RTRIM$(ValueCamp$(0)))
      END IF

      MENSA$ = " Esta segur de fer la copia ? (S/N)......: [ ]"
      COLOR COL(0, 0), COL(0, 1)
      FINESTRA 10, 9, 14, 60, 1, CAIXA1
      LOCATE 11, 11: PRINT " Unitat de disc a on s'ha de fer la copia: [  ]"
      LOCATE 13, 11: PRINT MENSA$
      SetMaxCamps 1
      SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
      SetColorCamp 1, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
      SetInitCamp 0, 11, 11 + LEN(MENSA$) - 2, ASCI, 0, "XX", ""
      SetInitCamp 1, 13, 11 + LEN(MENSA$) - 2, ASCI, 0, "X", ""
      DeleteCamp 0: DeleteCamp 1
      InsertValueCamp 0, "A:"
      InsertValueCamp 1, "S"
      DisplayAllCamps

      LOCATE , , 1, 14, 15
      FOR C = 0 TO MAXCAMPS
	  VALUE = ReadCamp(C)
	  SELECT CASE VALUE
		 CASE 1
		      IF UCASE$(ValueCamp$(1)) <> "S" THEN SYSTEM
		 CASE F1
		      C = C - 1
		 CASE F2 TO F10
		      C = C - 1
		 CASE 999
		      DIRECCF$ = DOLD$
		      ERASE ANY$
		      PutBackground 1, 1, ME$
		      RETURN
		 CASE ELSE
	  END SELECT
       NEXT
      
      '
      ' INICIAR FITXER DE CONTROL DE FITXERS, PER EL BACKUP
      '

      COLOR COL(0, 0), COL(0, 1)
      FINESTRA 5, 1, 20, 79, 1, CAIXA1: COLOR COL(1, 0), COL(1, 1)
      CENTRAR 5, " Smart-BackUp 2.00 ": COLOR COL(0, 0), COL(0, 1)
      LOCATE 6, 2: PRINT "Preparant fitxers... ";

      CHDRIVE MID$(DIRECCF$, 1, 2)
      CHDIR MID$(DIRECCF$, 1, LEN(DIRECCF$) - 1)

      MAXFILES = DirToArray("*.*")

      LOCATE 7, 2: PRINT "Dins el direcctori " + DIRECCF$ + " hi ha "; MAXFILES; " Fitxers."
      LOCATE 9, 2: PRINT "Preparant copia de seguretat ..."

      FOR COP = 1 TO MAXFILES
	  TANT% = 100 * COP / MAXFILES
	  LOCATE 10, 2: PRINT STRING$(70, " ");
	  LOCATE 23, 2: PRINT STRING$(70, " ");
	  LOCATE 10, 2: PRINT "Copiant " + FILE$(COP): LOCATE 10, 29: PRINT "Fet:"; : COLOR COL(2, 0): PRINT TANT%; : COLOR COL(2, 0): PRINT " %";
	  SHELL DIRECCU$ + "ICOP " + DIRECCF$ + FILE$(COP) + " " + DIRECCEX$ + FILE$(COP)
      NEXT

      SHELL DIRECCU$ + "ICOP " + UNIDAD$ + "CONFIG.FAC " + DIRECCEX$ + "CONFIG.FAC"
      SHELL DIRECCU$ + "ICOP " + UNIDAD$ + "EMPRESA.FAC " + DIRECCEX$ + "EMPRESA.FAC"
      SHELL DIRECCU$ + "ICOP " + UNIDAD$ + "USUARIS.FAC " + DIRECCEX$ + "USUARIS.FAC"

      LOCATE 10, 3: PRINT " Ok ..."
      CHDIR MID$(DIRECCEX$, 1, LEN(DIRECCEX$) - 1)
      GetBackground 1, 1, 25, 79, factu$
      CLS
      AS$ = RTRIM$(LTRIM$(STR$(DTN(FormatD$(Now#, "dd/mm/yy"))))) + ".ZIP"



      IF DIR$(AS$) <> "" THEN
	 SHELL DIRECCU$ + "ATB " + AS$ + " /F"
	 KILL AS$
      END IF

      SHELL DIRECCU$ + "PKZIP " + AS$ + " *.*"
      SHELL DIRECCU$ + "ATB " + AS$ + " /R"

      AS$ = RTRIM$(LTRIM$(STR$(DTN(FormatD$(Now#, "dd/mm/yy"))))) + ".ZIP"
      PutBackground 1, 1, factu$
      MAXFILEDEL = DirToArray("*.*")
      MSG$ = "BORRAT_MSG"
      FOR D = 1 TO MAXFILEDEL
	  TANT% = 100 * D / MAXFILEDEL
	  LOCATE 11, 2: PRINT STRING$(70, " ");
	  LOCATE 23, 2: PRINT STRING$(70, " ");
	  IF FILE$(D) <> AS$ THEN
	     LOCATE 11, 2: PRINT "Borrant " + FILE$(D): LOCATE 10, 29: PRINT "Fet:"; : COLOR COL(2, 0): PRINT TANT%; : COLOR COL(2, 0): PRINT " %"
	     KILL FILE$(D)
	  END IF
      NEXT
      BCK.UNITAT$ = ValueCamp$(0)
      COLOR COL(0, 0), COL(0, 1)
      FINESTRA 12, 10, 14, 66, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
      LOCATE 13, 12: PRINT "INSERTI UN DISKETTE BUIT A LA UNITAT " + BCK.UNITAT$ + ",PITJI <ENTER>"
      C$ = INPUT$(1)

      NOMF$ = RTRIM$(LTRIM$(STR$(DTN(FormatD$(Now#, "dd/mm/yy"))))) + ".ZIP"
      AREAA = FREEFILE: OPEN NOMF$ FOR RANDOM SHARED AS AREAA LEN = 1
      OPEN DIRECCEX$ + "CONTROL.CAT" FOR RANDOM AS 5 LEN = LEN(CAT)

      CAT.CPY = "Smart-BackUp 1996/97 (C)" + CHR$(26)
      CAT.HORA = TIME$
      CAT.FECHA = FormatD$(Now#, "dd/mm/yy")
      CAT.CFG.COPY = CFG.COPY
      CAT.CFG.MAXREG = CFG.MAXREG
      CAT.CFG.VAR = CFG.VAR
      CAT.CFG.INST = CFG.INST
      CAT.CFG.NOM = CFG.NOM
      CAT.CFG.DIREC = CFG.DIREC
      CAT.CFG.POBLA = CFG.POBLA
      CAT.CFG.TELF = CFG.TELF
      CAT.CFG.DRIVE = CFG.DRIVE
      CAT.CFG.DDADE = CFG.DDADE
      CAT.CFG.DPANT = CFG.DPANT
      CAT.CFG.DRECU = CFG.DRECU
      CAT.CFG.INSTALAT = CFG.INSTALAT
      CAT.CEMPRES.PASSWORD = EMPRES.PASSWORD
      CAT.CEMPRES.DEVICE = EMPRES.DEVICE
      CAT.CEMPRES.IMPRESORA = EMPRES.IMPRESORA
      CAT.CEMPRES.IVA = EMPRES.IVA
      CAT.CEMPRES.DTO = EMPRES.DTO
      PUT 5, 1, CAT
      
      SPAC = LOF(AREAA)                 ' ESPAI QUE OCUPA EL FITXER BACKUP
      SPAC2 = LOF(5)                    ' ESPAI QUE OCUPA EL FITXER CONTROL
      SPACT = SPAC + SPAC2              ' ESPAI QUE OCUPEN EL FITXER BACKUP I DE CONTROL
      DSPACE = DISKSPACE(BCK.UNITAT$)   ' CAPACITAT TOTAL DEL DISC

      DISP = DSPACE - SPACT - 2
      CLOSE AREA: CLOSE 5
      COLOR COL(0, 0), COL(0, 1)
      FINESTRA 6, 10, 17, 66, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
      LOCATE 7, 11: PRINT "Smart-BackUp 2.00"
      LOCATE 8, 11: PRINT STRING$(55, "Ä"); : COLOR COL(0, 0), COL(0, 1)
      LOCATE 9, 11: PRINT "Unitat de disc..: "; : COLOR COL(2, 0), COL(2, 1): PRINT BCK.UNITAT$: COLOR COL(0, 0), COL(0, 1)
      LOCATE 10, 11: PRINT "Espai disponible: "; : COLOR COL(2, 0), COL(2, 1): PRINT USING "###,###,###"; DSPACE; : COLOR COL(0, 0), COL(0, 1): PRINT " Abans del BACKUP"
      LOCATE 11, 11: PRINT MID$(AS$, 1, LEN(AS$) - 4) + ".BCK......: "; : COLOR COL(2, 0), COL(2, 1): PRINT USING "###,###,###"; SPAC; : COLOR COL(0, 0), COL(0, 1): PRINT " Backup de l'empresa"
      LOCATE 12, 11: PRINT "CONTROL.CAT.....: "; : COLOR COL(2, 0), COL(2, 1): PRINT USING "###,###,###"; SPAC2; : COLOR COL(0, 0), COL(0, 1): PRINT " Fitxer de control"
      LOCATE 13, 11: PRINT "                  -----------"
      LOCATE 14, 11: PRINT "Espai disponible: "; : COLOR 14: PRINT USING "###,###,###"; DISP; : COLOR 15: PRINT " Despres del BACKUP"
      COLOR 27
      LOCATE 16, 11: PRINT "Pitja una tecla per a procedir al BACKUP"
      C$ = INPUT$(1)

      '
      ' INICIANT COPIA DE SEGURETAT
      '

      SHELL DIRECCU$ + "ICOP CONTROL.CAT " + BCK.UNITAT$ + "\CONTROL.CAT"
      AREA = FREEFILE: OPEN NOMF$ FOR RANDOM AS AREA LEN = 1
      AREA2 = FREEFILE: OPEN BCK.UNITAT$ + "\" + MID$(AS$, 1, LEN(AS$) - 4) + ".BCK" FOR RANDOM AS AREA2 LEN = 1
      FIELD AREA, 1 AS C$
      FIELD AREA2, 1 AS BC$

      BCK.ID = 1
      RMAX# = SPAC
      FOR R = 1 TO SPAC
	  IF R > DISKSPACE(BCK.UNITAT$) - 2 THEN
	     AREA3 = FREEFILE
	     OPEN "DISK.ID" FOR RANDOM AS AREA3 LEN = 1: FIELD AREA3, 1 AS ID$
	     LSET ID$ = STR$(BCK.ID): CLOSE AREA3
	     COLOR COL(1, 0), COL(1, 1)
	     FINESTRA 11, 10, 14, 66, 1, CAIXA1: COLOR COL(1, 0), COL(1, 1)
	     LOCATE 13, 12: PRINT "INSERTI UN DISKETTE BUIT A LA UNITAT " + BCK.UNITAT$ + ",PITJI <ENTER>"
	     CQ$ = INPUT$(1)
	  END IF
	  GET AREA, R
	  Q$ = C$
	  LSET BC$ = Q$
	  PUT AREA2, R
	  Barra 10, 10, RMAX#, R
      NEXT
      CLOSE AREA, AREA2
      CHDIR UNIDAD$
      RESET
      SYSTEM

' ********************************************************************
'                              RESTORE
' ********************************************************************
      
RESTOREB:
      DIM ANY$(1 TO M% + 1)

      FOR R = 1 TO M%
	  GET AREAA, R, ANYS
	  IF ANYS.DATAT <> "../../...." THEN FIN$ = "*" ELSE FIN$ = "<"
	  ANY$(R) = ANYS.ANY + " " + ANYS.DATAO + "  " + ANYS.HORAO + " " + FIN$
      NEXT

      ANY$(R) = "Un altre direcctori...        "
      GetBackground 10, 10, 18, 43, L$
      ANYSE = Achoice(10, 10, 17, 42, R, ANY$(), COL(), "Any  Data Inici  Hora Inici   ", 4, "")
      IF ANYSE = 0 OR ANYSE = -14 OR ANYSE = -19 THEN
	 ERASE ANY$
	 PutBackground 1, 1, ME$
	 RETURN
      END IF

      DOLD$ = DIRECCF$
      IF ANY$(ANYSE) <> "Un altre direcctori...        " THEN
	 GET AREAA, R, ANYSE
	 DIRECCF$ = DF$ + ANYS.ANY + "\"
      ELSE
	 COLOR COL(0, 0), COL(0, 1)
	 FINESTRA 10, 9, 12, 66, 1, CAIXA1
	 LOCATE 11, 11: PRINT " Direcctori: [                                        ]"
	 LOCATE 13, 11: PRINT MENSA$
	 SetMaxCamps 0
	 SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
	 SetInitCamp 0, 11, 25, ASCI, 0, STRING$(25, "X"), ""
	 DeleteCamp 0: InsertValueCamp 0, LTRIM$(RTRIM$(DBF$))
	 DisplayAllCamps
	 LOCATE , , 1, 14, 15
	 FOR C = 0 TO MAXCAMPS
	     VALUE = ReadCamp(C)
	     SELECT CASE VALUE
		    CASE 0
		      IF ValueCamp$(0) = "" THEN BEEP: C = C - 1
		    CASE F1
		      SHELL "BKHLP COPIES_SEGURITAT"
		      C = C - 1
		    CASE F2 TO F10
		      C = C - 1
		    CASE 999
		      DIRECCF$ = DOLD$
		      ERASE ANY$
		      PutBackground 1, 1, ME$
		      RETURN
		    CASE ELSE
	     END SELECT
	  NEXT
	  DIRECCF$ = LTRIM$(RTRIM$(ValueCamp$(0)))
      END IF



      MENSA$ = " Esta segur de restaurar-la ? (S/N)......: [ ]"
      COLOR COL(0, 0), COL(0, 1)
      FINESTRA 10, 9, 14, 60, 1, CAIXA1
      LOCATE 11, 11: PRINT " Unitat de disc a on s'ha de restaurar-la: [  ]"
      LOCATE 13, 11: PRINT MENSA$
      SetMaxCamps 1
      SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
      SetColorCamp 1, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
      SetInitCamp 0, 11, 11 + LEN(MENSA$) - 2, ASCI, 0, "XX", ""
      SetInitCamp 1, 13, 11 + LEN(MENSA$) - 2, ASCI, 0, "X", ""
      DeleteCamp 0: DeleteCamp 1
      InsertValueCamp 0, "A:"
      InsertValueCamp 1, "S"
      DisplayAllCamps

      LOCATE , , 1, 14, 15
      FOR C = 0 TO MAXCAMPS
	  VALUE = ReadCamp(C)
	  SELECT CASE VALUE
		 CASE 1
		      IF UCASE$(ValueCamp$(1)) <> "S" THEN SYSTEM
		 CASE F1
		      SHELL "BKHLP COPIES_SEGURITAT"
		      C = C - 1
		 CASE F2 TO F10
		      C = C - 1
		 CASE 999
		      DIRECCF$ = DOLD$
		      ERASE ANY$
		      PutBackground 1, 1, ME$
		      RETURN
		 CASE ELSE
	  END SELECT
       NEXT

      ' ************************************************************
      ' Cerca i copia els fitxers del disc
      ' ************************************************************
      BCK.UNITAT$ = ValueCamp$(0)
      COLOR COL(0, 0), COL(0, 1)
      FINESTRA 5, 1, 20, 79, 1, CAIXA1: COLOR COL(1, 0), COL(1, 1)
      CENTRAR 5, " Smart-Restore 2.00 ": COLOR COL(0, 0), COL(0, 1)

      COLOR COL(0, 0), COL(0, 1)
      GetBackground 12, 10, 15, 67, F$
      FINESTRA 12, 10, 14, 66, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
      LOCATE 13, 12: PRINT "INSERTI EL DISC AMB LA INFORMACI¢ A= " + BCK.UNITAT$ + ",PITJI <ENTER>"
      C$ = INPUT$(1)
			       
      CHDRIVE BCK.UNITAT$
      PutBackground 12, 10, F$

      LOCATE 6, 2: PRINT "Preparant fitxers... ";

      CHDIR MID$(DIRECCF$, 1, LEN(DIRECCF$) - 1)

      MAXFILES = DirToArray("*.BCK")
				
      IF FILE$(1) = "" THEN
	 tecla = Avis("ERROR:", "Dins aquest disc no hi ha copies per restaurar", "Pitji una tecla...", 0)
	 PutBackground 1, 1, ME$
	 RETURN
      END IF
      
      ' ************************************************************
      ' Seleccionar el fitxer a restaurar
      ' ************************************************************

      MAXFI = MAXFILES
      GetBackground 10, 10, 18, 23, Llista$
      CPA = Achoice(10, 10, 17, 22, MAXFI, FILE$(), COL(), "Fitxer     ", 1, "")
      IF CPA = 0 THEN
	 RETURN
      END IF

      MSG$ = "NULL"
      IF MID$(FILE$(CPA), 1, 5) = "FACT2" THEN
	 tecla = Avis("AVIS:", "Aquesta copia de seguritat perteneix al Facturaci¢ 2.0", "Pitji una tecla...", 0)
	 DIRECCF$ = DBF$ + "1996\"
	 MSG$ = "GENERA_ANY"
      END IF

      PutBackground 10, 10, Llista$
      COLOR COL(0, 0), COL(0, 1)
      LOCATE 7, 2: PRINT "Dins el direcctori " + ValueCamp$(1) + "\ hi ha "; P#; " Fitxers."
      LOCATE 9, 2: PRINT "Preparant la restauraci¢ ..."

      FOR COP = 1 TO MAXFI
	  TANT% = 100 * COP / MAXFI
	  LOCATE 10, 2: PRINT STRING$(70, " ");
	  LOCATE 23, 2: PRINT STRING$(70, " ");
	  LOCATE 10, 2: PRINT "Copiant " + FILE$(COP): LOCATE 10, 29: PRINT "Fet:"; : COLOR COL(2, 0): PRINT TANT%; : COLOR COL(2, 0): PRINT " %";
	  SHELL DIRECCU$ + "ICOP " + BCK.UNITAT$ + FILE$(COP) + " " + DIRECCEX$ + FILE$(COP)
      NEXT

      LOCATE 10, 3: PRINT " Ok ..."
      CHDRIVE MID$(UNIDAD$, 1, 2)
      CHDIR MID$(DIRECCEX$, 1, LEN(DIRECCEX$) - 1)
      GetBackground 1, 1, 25, 79, factu$
      CLS : PRINT "Descompresor On..."
      NOMFIT$ = FILE$(CPA)
      SHELL DIRECCU$ + "PKUNZIP -d " + NOMFIT$
      PutBackground 1, 1, factu$

      MAXCFILE = DirToArray("*.*")

      IF MSG$ = "GENERA_ANY" THEN
	 tecla = Avis("AVIS:", "ES CREAR… UN ANY NOU PER LA FACTURACI¢ ANTIGA!!!", "Pitji una tecla...", 0)
	 ANYNOU = LOF(AREAA) / LEN(ANYS) + 1
	 ANYS.ANY = "1996"
	 ANYS.DATAO = "01/01/1996"
	 ANYS.HORAO = "..:..:.."
	 PUT AREAA, ANYNOU, ANYS
	 MKDIR DBF$ + "1996"
      END IF

      FOR D = 1 TO MAXFI
	  TANT% = 100 * D + 1 / INT(MAXFI)
	  LOCATE 11, 2: PRINT STRING$(70, " ");
	  LOCATE 23, 2: PRINT STRING$(70, " ");
	  LOCATE 11, 2: PRINT "Copiant " + FILE$(D): LOCATE 10, 29: PRINT "Fet:"; : COLOR COL(2, 0): PRINT TANT%; : COLOR COL(2, 0): PRINT " %"
	  SHELL DIRECCU$ + "ICOP " + DIRECCEX$ + FILE$(COP) + " " + DIRECCF$ + FILE$(CPA)
      NEXT

      tecla = Avis("AVIS:", "LA COPIA DE SEGURETAT HA ESTAT RESTAURADA", "Pitji una tecla...", 0)

      SYSTEM

'***********************************************************************
'                        OBRIR ELS FITXERS
'***********************************************************************

OBRI:
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

	  IF LTRIM$(RTRIM$(CA2$)) <> "BCKUP.EXE" THEN
	     tecla = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "Pitji una tecla...", 0)
	  END IF

	  KILL TMP$ + "PASS.TMP"
	  KILL TMP$ + "PASU.TMP"
	  KILL TMP$ + "PASE.TMP"
	  KILL TMP$ + "PROT.TMP"


INICIA.FITXERS:

      UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
      DBF$ = LTRIM$(RTRIM$(CFG.DDADE))             ' Assignar direcctoris
      SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
      MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
      HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
      SYS$ = LTRIM$(RTRIM$(CFG.DSIST))
      DIRECCU$ = SYS$
      EXP$ = LTRIM$(RTRIM$(CFG.DEXPO))
      IMP$ = LTRIM$(RTRIM$(CFG.DIMPO))

      DF$ = DBF$
      DIRECCF$ = DBF$     ' Subdirecctori de les base de dades
      DIRECCP$ = SCR$ + "\"      ' Subdirecctori de les pantalles
      DIRECCR$ = MSK$ + "\"     ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = HLP$ + "\"    ' Subdirecctori de ajuda
      DIRECCI$ = UNIDAD$ + "\IMPRESOR\"  ' Subdirecctori de les impresores
      DIRECCT$ = DBF$ + "TEXTOS\"
      DIRECCH$ = DBF$ + "HISTORIC\"
     
      DIRECCEX$ = EXP$
      DIRECCIM$ = IMP$

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
      M% = LOF(AREAA) \ LEN(ANYS) + 1

      DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"
      
      RETURN


ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

REM $STATIC
FUNCTION DirToArray (MASK$)
      P = 1
      X$ = DIR$(MASK$)
      DO WHILE X$ <> ""
	 FILE$(P) = X$
	 X$ = DIR$
	 IF X$ <> "" THEN
	    P = P + 1
	    REDIM PRESERVE FILE$(0 TO P)
	 END IF
      LOOP
      DirToArray = P
END FUNCTION

FUNCTION DISKSPACE (DRIVE$)

REGS.ax = &H3600
REGS.dx = ASC(UCASE$(DRIVE$)) - 64
CALL InterruptX(&H21, REGS, REGS)
 
SectorsInCluster = REGS.ax
BytesInSector = REGS.cx
IF REGS.dx >= 0 THEN
	ClustersInDrive = REGS.dx
ELSE
	ClustersInDrive = REGS.dx + 65536
END IF
IF REGS.bx >= 0 THEN
	ClustersAvailable = REGS.bx
ELSE
	ClustersAvailable = regx.bx + 65536
END IF

Freespace = ClustersAvailable * SectorsInCluster * BytesInSector
DISKSPACE = Freespace

END FUNCTION

