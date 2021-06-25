DECLARE FUNCTION LlegeixPort$ ()
DECLARE SUB MenuFacturacio ()
DECLARE FUNCTION ReadCampTerminal$ (X!, Y!, LONGI!)
DECLARE SUB PrintPort (CAD$, CRLF!)
DECLARE SUB PrintStatus (CAD$)
DECLARE SUB PrintMessagePort (P%, F%, CAD$())
DECLARE FUNCTION DirToArray! (MASK$)
DECLARE SUB welcome ()

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'

'$DYNAMIC
'DIM SHARED FILE$(0 TO 1)
DIM SHARED ver$(10)
DIM SHARED MESS$(20)
DIM SHARED SET$(0 TO 200)

GOSUB SETVARS
GOSUB MASC
OPEN "COM2:9600,N,8,1,RS,CS0,DS0" FOR RANDOM AS #1
welcome
CLOSE #1

DO
			
  C$ = INKEY$
  
  EM$ = DecimalBinari$(INP(&H2FE), 7) ' ESTAT DEL MODEM
  EL$ = DecimalBinari$(INP(&H2FD), 7) ' ESTAT DE LINIA
  X = CSRLIN: Y = POS(0)
  VIEW PRINT 1 TO 25
  COLOR 0, 7: LOCATE 1, 70: PRINT TIME$: COLOR 7, 0

'  PrintStatus ("      STS 1.0 -" + TIME$)
  VIEW PRINT 2 TO 24
  LOCATE X, Y

  b = 1
  IF MID$(EL$, 8, 1) = "1" THEN
     lastword$ = CHR$(INP(&H2F8))
     IF lastword$ <> CHR$(8) THEN
	WORD$ = WORD$ + lastword$
	XT = XT + 1
     END IF
     'PRINT lastword$;
     SELECT CASE lastword$

	    CASE CHR$(8)
		   b = LEN(WORD$) - 1
		   XT = XT - 1
		   IF b > 0 THEN
		      WORD$ = LEFT$(WORD$, b)
		      PRINT WORD$
		   END IF
		   OUT &H2F8, 8
		   IF XT < 2 THEN OUT &H2F8, 32: XT = 2

	   CASE CHR$(13)
		 MOUNTWORD$ = LTRIM$(RTRIM$(UCASE$(WORD$)))
	       
		 SELECT CASE MID$(MOUNTWORD$, 1, LEN(MOUNTWORD$) - 1)
			CASE IS = CHR$(1) + "C"
			CASE IS = ""
			CASE IS = "CLS"
			     PRINT "Comanda CLS - dia " + DATA$(1) + " hora " + TIME$
			     OUT &H2F8, 26
			CASE IS = "VER"
			     GOSUB VERSION
			     PRINT "Comanda VER - dia " + DATA$(1) + " hora " + TIME$
			CASE IS = "DIR"
			     GOSUB DIRECTORI
			     PRINT "Comanda DIR - dia " + DATA$(1) + " hora " + TIME$
			CASE IS = "QUIT"
			     PRINT "Comanda QUIT - dia " + DATA$(1) + " hora " + TIME$
			     GOSUB QUIT
			CASE IS = "SET"
			     PRINT "Comanda SET - dia " + DATA$(1) + " hora " + TIME$
			     GOSUB SETS
			CASE IS = "READ"
			     PRINT "Comanda READ - dia " + DATA$(1) + " hora " + TIME$
			     PRINT ReadCampTerminal$(1, 1, 10)
			CASE IS = "GESTIO"
			     PRINT "Programa Gesti¢ - dia " + DATA$(1) + " hora " + TIME$
			     MenuFacturacio
			CASE ELSE
			     PrintPort CHR$(10) + "Comanda incorrecta", 1
			     PRINT "Comanda incorrecta - dia " + DATA$(1) + " hora " + TIME$
		 END SELECT
		 OUT &H2F8, 10
		 OUT &H2F8, ASC(">")
		 WORD$ = ""
	    CASE CHR$(27)
		 GOSUB QUIT
	    CASE ELSE
     END SELECT
  ELSE
  END IF
LOOP UNTIL C$ = CHR$(27)

QUIT:
      PRINT "Desactivant terminal"
      CALL PrintPort(CHR$(10) + "Sistema aturat dia " + DATA$(1) + " hora " + TIME$, 1)
      PrintStatus ("  *** NO SYSTEM ***")
      OUT &H2F8, 7
      OUT &H2F8, 19
      OUT &H2F8, 15
      CALL PrintPort(CHR$(27) + "u", 0)
      CALL PrintPort("         ******* Ara pot aturar el seu equip *******", 1)
      PRINT "Desactivada!!"
      END

VERSION:
     CALL PrintMessagePort(0, 3, ver$())
     RETURN

SETS:
     L% = 0
     DO
	  SET$(L%) = ENVIRON$(L% + 1)
	  L% = L% + 1
     LOOP UNTIL ENVIRON$(L% + 1) = ""
     OUT &H2F8, 13: OUT &H2F8, 10
     PrintMessagePort 0, L%, SET$()

     RETURN

DIRECTORI:
     FITXERS% = DirToArray("*.*")
     PrintMessagePort 1, FITXERS% - 1, FILE$()
     RETURN

MASC:
     CLS
     COLOR 0, 7
     LOCATE 1, 1: PRINT STRING$(80, " ");
     LOCATE 1, 1: PRINT "Smart Server Terminal 0.97 - Tomeu Cap¢ i Cap¢ 1997 (C)"
     LOCATE 25, 1: PRINT STRING$(80, " ");
     LOCATE 25, 2: PRINT "<ESC> Sortir";
     COLOR 7, 0
     VIEW PRINT 2 TO 23
     
     RETURN

SETVARS:
     ver$(0) = CHR$(13) + "STS 0.97 -*- Smart Terminal System"
     ver$(1) = "Petit sistema operatiu per terminals de PC"
     ver$(2) = "Smart Software 1993/97 (C)" + CHR$(10)
     ver$(3) = "Tomeu Cap¢ i Cap¢ 1997 (C)" + CHR$(10)
     RETURN

REM $STATIC
FUNCTION DirToArray (MASK$)
      P = 1
      X$ = DIR$(MASK$)
      DO WHILE X$ <> ""
	 FILE$(P) = X$
	 X$ = DIR$
	 IF X$ <> "" THEN
	    P = P + 1
	    'REDIM PRESERVE FILE$(0 TO P)
	 END IF
      LOOP
      DirToArray = P
END FUNCTION

FUNCTION LlegeixPort$


  EM$ = DecimalBinari$(INP(&H2FE), 7) ' ESTAT DEL MODEM
  EL$ = DecimalBinari$(INP(&H2FD), 7) ' ESTAT DE LINIA

  IF MID$(EL$, 8, 1) = "1" THEN
     LlegeixPort$ = CHR$(INP(&H2F8))
  ELSE
     LlegeixPort$ = ""
     EXIT FUNCTION
  END IF

END FUNCTION

SUB MenuFacturacio
    
    OUT &H2F8, 26
    PrintPort "", 1
    PrintPort "                 **** Facturaci¢ WorkStation 3.0 ***", 1
    PrintPort "", 1
    PrintPort "       1.- Manteniment Clients         5.- Manteniment Resguards", 1
    PrintPort "       2.- Manteniment Stock           6.-", 1
    PrintPort "       3.- Manteniment Albarans        7.-", 1
    PrintPort "       4.- Manteniment Factures        8.- Sortir al STS", 1
    PrintPort "", 1
    PrintPort "                        Tria una opci¢:", 0

    DO
       value$ = LlegeixPort$
       SELECT CASE value$
	      CASE IS = "1"
	      CASE ELSE
       END SELECT
    LOOP UNTIL value$ = "8"

    OUT &H2F8, 26
    PrintPort "*** Fi de la sessi¢ amb Facturaci¢ WorkStation 3.0", 1
    PrintPort "", 1
END SUB

SUB PrintMessagePort (P%, F%, CAD$())
    L = P%

    FOR L = P% TO F%
	CALL PrintPort(CAD$(L), 1)
    NEXT
    

END SUB

SUB PrintPort (CAD$, CRLF)
    L = LEN(CAD$)
    FOR D = 1 TO L
	OUT &H2F8, ASC(MID$(CAD$, D, 1))
    NEXT

    IF CRLF = 1 THEN
       OUT &H2F8, 13
       'OUT &H2F8, 10
    END IF
END SUB

SUB PrintStatus (CAD$)
    CALL PrintPort(CHR$(27) + "F" + CAD$ + CHR$(13), 0)
END SUB

FUNCTION ReadCampTerminal$ (X, Y, LONGI)

L = LONGI
XT = X: yt = Y
CAMP$ = SPACE$(L)
LA = 1
DO

  C$ = INKEY$

  EM$ = DecimalBinari$(INP(&H2FE), 7) ' ESTAT DEL MODEM
  EL$ = DecimalBinari$(INP(&H2FD), 7) ' ESTAT DE LINIA

  CX = X: CY = Y
  X = CSRLIN: Y = POS(0)
  VIEW PRINT 1 TO 25
  COLOR 0, 7: LOCATE 1, 70: PRINT TIME$: COLOR 7, 0
  VIEW PRINT 2 TO 24
  LOCATE X, Y
  Y = CY: X = CX


  IF MID$(EL$, 8, 1) = "1" THEN
     lastword$ = CHR$(INP(&H2F8))
     PRINT lastword$
     SELECT CASE lastword$
	    CASE CHR$(8)
		   PRINT XT, LA
		   IF XT <= X OR LA <= 0 THEN
		      XT = X: LA = 1
		      BEEP
		   ELSE
		      XT = XT - 1: LA = LA - 1
		      MID$(CAMP$, LA, 1) = " "
		      CALL PrintPort(CHR$(27) + "W", 0)
		   END IF
	    CASE CHR$(13)
		   ReadCampTerminal$ = CAMP$
		   EXIT FUNCTION
	    CASE CHR$(27)
		   EXIT FUNCTION
	    CASE ELSE
		 IF LA >= LONGI THEN
		    OUT &H2F8, 15
		    BEEP
		    OUT &H2F8, 14
		 ELSE
		    PRINT LA, XT
		    MID$(CAMP$, LA, 1) = lastword$
		    LA = LA + 1
		    XT = XT + 1
		 END IF
     END SELECT
  ELSE
  END IF
LOOP UNTIL C$ = CHR$(27)
END
END FUNCTION

SUB welcome
    PRINT #1, CHR$(14)
    PRINT #1, CHR$(17)
    PRINT #1, CHR$(26)
    PRINT #1, ""
    PRINT #1, "**********************************************************************"
    PRINT #1, "*                       Smart Terminal System 0.97                   *"
    PRINT #1, "*                                                                    *"
   PRINT #1, "*                    Smart Software Design  1993/97 (C)              *"
    PRINT #1, "**********************************************************************"
    'PRINT #1, "Sistema activat dia " + DATA$(1) + " hora " + TIME$ + CHR$(10)
    CALL PrintPort(">", 0)
    PRINT "Terminals activades"
END SUB

