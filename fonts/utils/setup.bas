DECLARE SUB InitDesktop ()

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\WIN.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'

DIM CFG AS CONFIG
DIM USR AS USUARIS
DIM EMPR AS EMPRESA
DIM CTRL AS CONTROL


OPEN "CONFIG.FAC" FOR RANDOM AS 1 LEN = LEN(CFG)
OPEN "EMPRESA.FAC" FOR RANDOM AS 2 LEN = LEN(EMPR)
OPEN "USUARIS.FAC" FOR RANDOM AS 3 LEN = LEN(USR)
OPEN "CONTROL.CFG" FOR RANDOM AS 4 LEN = LEN(CTRL)
      
      InitDesktop
      ShowWindow 0
      SetMaxCamps 5
      SetInitCamp 0, 11, 26, ASCI, 0, STRING$(30, "X"), "Nom empresa:"
      SetInitCamp 1, 12, 26, ASCI, 0, STRING$(30, "X"), "Direcci¢:"
      SetInitCamp 2, 13, 26, ASCI, 0, STRING$(20, "X"), "Poblaci¢:"
      SetInitCamp 3, 14, 26, ASCI, 0, STRING$(18, "X"), "Teläfon:"
      SetInitCamp 4, 16, 26, ASCI, 0, STRING$(20, "X"), "Nom Usuari 1:"
      SetInitCamp 5, 17, 26, ASCI, 0, STRING$(10, "X"), "Clau Usuari 1:"
      
      FOR C = 0 TO MAXCAMPS: SetColorCamp C, 0, 15, 15, 9, 14, 9: NEXT
      DisplayAllCamps
      
      FOR C = 0 TO MAXCAMPS
	  VALUE = ReadCamp(C)
	  SELECT CASE VALUE

		 CASE 999
		      DeleteWindow 0
		      PackWindows
		      SYSTEM
		 CASE ELSE
		      
	  END SELECT
      NEXT
      
      CFG.COPY = "Configuraci¢ Facturaci¢ 3.3" + CHR$(26)
      CFG.MAXREG = 2
      CFG.VAR = TESTSCREEN
      CFG.NOM = ValueCamp$(0)
      CFG.DIREC = ValueCamp$(1)
      CFG.POBLA = ValueCamp$(2)
      CFG.TELF = ValueCamp$(3)
      CFG.XARXA = "N"
      CFG.DRIVE = "C:\FACT3"
      CFG.DDADE = "C:\FACT3\FACTU001\"
      CFG.DPANT = "C:\FACT3\PANTS\"
      CFG.DRECU = "C:\FACT3\RECURSOS\"
      CFG.DMAIN = "C:\FACT3\MAIN\"
      CFG.DEXPO = "C:\FACT3\EXPORT\"
      CFG.DIMPO = "C:\FACT3\IMPORT\"
      CFG.DSIST = "C:\FACT3\SYS\"
      CFG.DHELP = "C:\FACT3\HELP\"

      CADENC$ = ENCRIPT$(MID$(VERSVGA$, 1, 29), 45)    ' ENCRIPTAR-LO
      CFG.INST = CADENC$
      CFG.INSTALAT = "INSTAL.LAT"
      PUT 1, 1, CFG

      D$ = LTRIM$(RTRIM$(ValueCamp$(5)))

      FOR L = 1 TO LEN(D$)
	  IF MID$(D$, L, 1) = "/" THEN MID$(D$, L, 1) = CHR$(32)
      NEXT

      C$ = LTRIM$(D$)

      EMPR.PASSWORD = ENCRIPT$(C$, 15)
      EMPR.DEVICE = "LPT1:"
      EMPR.IMPRESORA = 1
      EMPR.DTO = 0
      EMPR.IVA = 0
      EMPR.ANY = 1
      EMPR.USUARI(0) = 1

      FOR C = 1 TO 100
	  EMPR.USUARI(C) = -999
      NEXT

      PUT 2, 1, EMPR

      USR.COPY = "Control d'usuaris 2.1" + CHR$(26)
      USR.NOM = LTRIM$(RTRIM$(UCASE$(ValueCamp$(4))))
      USR.MAXLINS = 50
      USR.LINFACT = 66
      USR.LINALBA = 66
      USR.DEVICE = "LPT1:"
      USR.IMPRESORA = 1
      USR.PASSWORD = ENCRIPT$(C$, 15)
      PUT 3, 1, USR

      CTRL.EMPRESA = 1
      CTRL.USUARI = 1
      PUT 4, 1, CTRL
      CLOSE 1, 2, 3, 4

      CAD$ = ENCRIPT$(CFG.INST, 45)          ' DESENCRIPTAR EL COPYRIGHT

      OPEN "SERIAL.NUM" FOR RANDOM AS 10 LEN = 23
      FIELD 10, 23 AS NUMSER$
      
      NUMSE$ = "9999.0707.DSS.XXXX.XXXX"
      NS$ = SPACE$(23)
      FOR L = 1 TO 23: MID$(NS$, L, 1) = ENCRIPT$(MID$(NUMSE$, L, 1), ASC(MID$(CAD$, L, 1))): NEXT
      LSET NUMSER$ = NS$
      PUT 10, 1
      CLOSE 10
      END

REM $DYNAMIC
SUB InitDesktop
    COLOR 15, 9
    CLS : FONS 177
    LOCATE 1, 1: PRINT STRING$(80, " ")
    LOCATE 25, 1: PRINT STRING$(80, " ");
    LOCATE 1, 2: PRINT "Configuraci¢ del GestPRO 3.3   ---*---  DEMO!!"
    LOCATE 25, 2: PRINT "<ESC> Sortir";
    
    IF SetInitWindows(1) THEN
       PRINT "ERROR: Memïria insuficient!"
       SYSTEM
    END IF

    win1 = InitNewWindow(0, 9, 10, 20, 60, 1, "Configuraci¢")
    SetColorWindow 0, 15, 9, 15, 9, 14, 9
    SetStyleWindow 0, 0, ""


END SUB

