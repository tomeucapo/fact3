' ********************************************************************
'
' Fitxer....................: CLO_ANY.BAS
' Titol.....................: Utilitat per tancar l'any actual
'                             de facturaci¢ i obrir un nou
' ********************************************************************
'
' Data inici................: 02/01/1998 00:12:00
' Data de la darrera revisi¢:
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/98 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB PrintOut (TEXT$)

COMMON SHARED lo

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT3\FONTS\RESGUARD.BI'
'$INCLUDE: 'C:\FACT3\FONTS\WIN.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'

DIM COLORS AS COLO
DIM SHARED col(2, 1)
DIM SHARED CEMPRE AS CONTROLEMP
DIM SHARED RESGUA AS RESG
DIM SHARED CFG AS CONFIG
DIM SHARED CTRL AS CONTROL
DIM SHARED ALBARAN AS ALBAR
DIM SHARED FACTURA AS FACTU
DIM SHARED EMPRES AS EMPRESA
DIM SHARED USR AS USUARIS
DIM SHARED PASO AS TRANS
DIM SHARED ANYS AS ANNO
DIM SHARED MENUO(4) AS STRING
DIM SHARED COMENTA$(3, 0)

      GOSUB Inicialitza
      GOSUB DEFDESKTOP

'********************************************************************
'
'********************************************************************
   
    DO
      COLOR col(0, 0), col(0, 1)
      CALL MenuBar(MENUO(), COMENTA$(), 0, CASO%, 8, 55, LEN(MENUO(1)) + 1, 4, col(0, 0), col(0, 1), col(1, 0), col(1, 1))
      SELECT CASE CASO%
	     CASE 1
	     CASE 2
		  GOSUB canvi.any
	     CASE 4
		  GOSUB tanca.actual
	     CASE ELSE
      END SELECT
    LOOP UNTIL CASO% = 999 OR CASO% = -19 OR CASO% = -14
    RESET
    SYSTEM

'********************************************************************
' Seleccionar un any (de treball)
'********************************************************************

canvi.any:
      DIM ANY$(1 TO M%)

      FOR R = 1 TO M%
	  GET AREAA, R, ANYS
	  ANY$(R) = ANYS.ANY + " " + ANYS.DATAO + "  " + ANYS.HORAO
      NEXT
      
      GetBackground 10, 10, 18, 43, L$
      ANYSE = Achoice(10, 10, 17, 42, R - 1, ANY$(), col(), "Any  Data Inici  Hora Inici   ", 4, "")
      IF ANYSE = 0 OR ANYSE = -13 OR ANYSE = -14 THEN
	 ERASE ANY$
	 PutBackground 10, 10, L$
	 RETURN
      END IF
      PutBackground 10, 10, L$
      EMPRES.ANY = ANYSE
      PUT 2, CTRL.EMPRESA, EMPRES
      RETURN

'********************************************************************
' Tancament de l'any actual
'********************************************************************

tanca.actual:
      ShowWindow 0
      COLOR col(2, 0), col(2, 1)
      PrintOut " * Tancament de l'any " + ANYS.ANY
      PrintOut " "
      COLOR col(0, 0), col(0, 1)
      PrintOut " Esta segur de fer el tancament de l'any (S/N) ? "
      DO
	 C$ = UCASE$(INKEY$)
      LOOP UNTIL C$ = "S" OR C$ = "N"
      IF C$ = "N" THEN RETURN
      PrintOut " ----------------------------------------------------------------------------"
      PrintOut "  1. Totals de l'any: "
      PrintOut "                            ³ Regs.  Total Brut   Total IVA    Total Net"
      PrintOut "                            ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
      PrintOut "            - Stock       = ³ 9.999  99.999.999   99.999.999,9 99.999.999,99"
      PrintOut "            - Presuposts  = ³ 9.999  99.999.999   99.999.999,9 99.999.999,99"
      PrintOut "            - Factures    = ³ 9.999  99.999.999   99.999.999,9 99.999.999,99"
      PrintOut "            - Albarans    = ³ 9.999  99.999.999   99.999.999,9 99.999.999,99"
      PrintOut "            - Reparacions = ³ 9.999  99.999.999   99.999.999,9 99.999.999,99"
      PrintOut " ----------------------------------------------------------------------------"
      C$ = INPUT$(1)
      PrintOut "  2. Llistats (Stock, Facturaci¢, Reparacions i informe anual)"
      tecla = Avis("AVIS:", "Prepari l'impresora amb paper blanc", "Pitji una tecla ...", 0)

      RETURN


      PrintOut "      2. Treure els llistats en: "
      PrintOut "            - Stock "
      PrintOut "            - Presuposts "
      PrintOut "            - Factures "
      PrintOut "            - Albarans "
      PrintOut "            - Albarans pendents "
      PrintOut "            - Reparacions "
      PrintOut "      3. Realitzar copia de seguretat. "
      PrintOut "      4. Iniciar l'any nou. "
      PrintOut " ------------------------------------------------------------"
      PrintOut " * Pitji una tecla per a procedir..."

'********************************************************************
'
'********************************************************************

DEFDESKTOP:
    IF SetInitWindows(1) = TRUE THEN
       tecla = Avis("ERROR:", "Mem•ria insuficient!!!", "PITJA UNA TECLA...", 0)
       RETURN
    END IF

    SetMaxCamps 3

    WIN = InitNewWindow(0, 2, 1, 20, 80, 0, "Utilitat per tancar l'any actual")
    SetInitCamp 0, 12, 27, ASCI, 0, "XXXXXXXXXX", "Codi client:"
    SetInitCamp 1, 15, 27, ASCI, 0, "X", " Factures: "
    SetInitCamp 2, 16, 27, ASCI, 0, "X", " Albarans: "
    SetInitCamp 3, 17, 27, ASCI, 0, "X", "Resguards: "
    FOR C = 0 TO MAXCAMPS: SetColorCamp C, col(1, 0), col(1, 1), col(0, 0), col(0, 1), col(2, 0), col(2, 1): NEXT
    SetColorWindow 0, col(0, 0), col(0, 1), col(1, 1), col(1, 0), col(2, 0), col(2, 1)
    SetStyleWindow 0, 0, ""
    lo = 5
    RETURN

'********************************************************************
'  Comprovaci¢ de la cridada del programa principal
'********************************************************************

Inicialitza:
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

	  IF LTRIM$(RTRIM$(ca2$)) <> "CLO_ANY.EXE," THEN
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

      OPEN UNIDAD$ + "\EMPRESA.FAC" FOR RANDOM SHARED AS 2 LEN = LEN(EMPRES)
      OPEN UNIDAD$ + "\CONTROL.CFG" FOR RANDOM SHARED AS 3 LEN = LEN(CTRL)

      GET 3, 1, CTRL
      GET 2, CTRL.EMPRESA, EMPRES    ' RECULL LA CONFIGURACI¢ SECUNDARIA

      AREAC = FREEFILE: OPEN DIRECCF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
      GET AREAC, 1, COLORS
      col(0, 0) = COLORS.col(0, 0): col(0, 1) = COLORS.col(0, 1)
      col(1, 0) = COLORS.col(1, 0): col(1, 1) = COLORS.col(1, 1)
      col(2, 0) = COLORS.col(2, 0): col(2, 1) = COLORS.col(2, 1)
      CLOSE AREAC

      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, R, ANYS
      DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"

      M% = LOF(AREAA) \ LEN(ANYS) + 1


      MENUO(1) = "Estat actual del any ": COMENTA$(0, 0) = ""
      MENUO(2) = "Canvi d'any          ": COMENTA$(1, 0) = ""
      MENUO(3) = "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ": COMENTA$(2, 0) = "SEPARADOR"
      MENUO(4) = "Tancament de l'any   ": COMENTA$(3, 0) = ""
      RETURN

ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

REM $DYNAMIC
SUB PrintOut (TEXT$)
    SHARED lo
    IF lo >= 19 THEN
       lo = 19
       ScrollUp 18, 78, 6, 1, 1
       LOCATE lo, 2: PRINT STRING$(78, " ");
       LOCATE lo, 2: PRINT TEXT$;
    ELSE
       LOCATE lo, 2: PRINT STRING$(78, " ");
       LOCATE lo, 2: PRINT TEXT$;
       lo = lo + 1
    END IF
END SUB

