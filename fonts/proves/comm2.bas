DECLARE FUNCTION BinariDecimal! (CAD$)
DECLARE FUNCTION LlegeixPort$ ()
DECLARE SUB MenuFacturacio ()
DECLARE FUNCTION ReadCampTerminal$ (X!, Y!, LONGI!)
DECLARE SUB PrintPort (CAD$, CRLF!)
DECLARE SUB PrintStatus (CAD$)
DECLARE SUB PrintMessagePort (P%, f%, CAD$())
DECLARE FUNCTION DirToArray! (MASK$)
DECLARE SUB welcome ()


'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\fact3\fonts\STRUCTS.BI'
'$INCLUDE: 'C:\fact3\fonts\SDK_001.BI'
'$INCLUDE: 'C:\fact3\fonts\DRAC3.BI'

port = &H2F8
'$DYNAMIC
'DIM SHARED FILE$(0 TO 1)
DIM SHARED ver$(10)
DIM SHARED MESS$(20)
DIM SHARED SET$(0 TO 200)

GOSUB MASC
'OPEN "COM2:9600,N,8,1,RS,CS0,DS0" FOR RANDOM AS #1
welcome
CLOSE #1

DO
			
  C$ = INKEY$
  
  Em$ = DecimalBinari$(INP(&H2FE), 7) ' ESTAT DEL MODEM
  El$ = DecimalBinari$(INP(&H2FD), 7) ' ESTAT DE LINIA
  X = CSRLIN: Y = POS(0)
  VIEW PRINT 1 TO 25
  COLOR 0, 7: LOCATE 1, 60: PRINT Em$: COLOR 7, 0
  COLOR 0, 7: LOCATE 1, 70: PRINT El$: COLOR 7, 0

  VIEW PRINT 2 TO 24
  LOCATE X, Y

  b = 1

  IF C$ <> "" THEN
     IF MID$(El$, 8, 1) = "0" THEN
	OUT &H2F8, ASC(MID$(C$, 1, 1))
	LOCATE , , 1: PRINT C$;
	IF C$ = CHR$(13) THEN
	   CALL PrintPort("------------------------------------", 0)
	END IF
     END IF
  END IF


  IF MID$(El$, 8, 1) = "1" THEN
     lastword$ = CHR$(INP(&H2F8))
     PRINT lastword$;
     WORD$ = WORD$ + lastword$
     XT = XT + 1
     
     SELECT CASE lastword$
	    CASE CHR$(13)
		 MOUNTWORD$ = LTRIM$(RTRIM$(UCASE$(WORD$)))
		 SELECT CASE MID$(MOUNTWORD$, 1, LEN(MOUNTWORD$) - 1)
			CASE IS = CHR$(1) + "C"
			CASE IS = ""
			CASE IS = "CLS"
			     PRINT "Comanda CLS - dia " + DATA$(1) + " hora " + TIME$
			     OUT &H2F8, 26
			CASE IS = "VER"
			     PRINT "Comanda VER - dia " + DATA$(1) + " hora " + TIME$
			CASE IS = "QUIT"
			CASE ELSE
		 END SELECT
		 WORD$ = ""
		 CALL PrintPort("PC01>", 0)
	    CASE CHR$(27)
		 PRINT "L'altre terminal s'ha desconectat."
	    CASE ELSE
     END SELECT
  ELSE
  END IF
LOOP UNTIL C$ = CHR$(27)


QUIT:
      PRINT "Desactivant terminal"
      CALL PrintPort("Sistema aturat dia " + DATA$(1) + " hora " + TIME$, 0)
      PRINT "Desactivada!!"
      END

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

REM $STATIC
FUNCTION BinariDecimal (CAD$)
	 f = 0
	 FOR SY = 7 TO 0 STEP -1
	     f = f + VAL(MID$(CAD$, SY + 1, 1)) * 2 ^ (7 - SY)
	 NEXT
	 BinariDecimal = f
END FUNCTION

FUNCTION LlegeixPort$


  Em$ = DecimalBinari$(INP(&H2FE), 7) ' ESTAT DEL MODEM
  El$ = DecimalBinari$(INP(&H2FD), 7) ' ESTAT DE LINIA

  IF MID$(El$, 8, 1) = "1" THEN
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
       VALUE$ = LlegeixPort$
       SELECT CASE VALUE$
	      CASE IS = "1"
	      CASE ELSE
       END SELECT
    LOOP UNTIL VALUE$ = "8"

    OUT &H2F8, 26
    PrintPort "*** Fi de la sessi¢ amb Facturaci¢ WorkStation 3.0", 1
    PrintPort "", 1
END SUB

SUB PrintMessagePort (P%, f%, CAD$())
    L = P%

    FOR L = P% TO f%
	CALL PrintPort(CAD$(L), 1)
    NEXT
    

END SUB

SUB PrintPort (CAD$, CRLF)
    L = LEN(CAD$)
    FOR D = 1 TO L
	WHILE MID$(El$, 8, 1) = "1"
	      El$ = DecimalBinari$(INP(&H2FD), 7)
	      VIEW PRINT 1 TO 25
	      COLOR 0, 7: LOCATE 1, 60: PRINT Em$: COLOR 7, 0
	      COLOR 1, 7: LOCATE 1, 70: PRINT El$: COLOR 7, 0
	WEND
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

  Em$ = DecimalBinari$(INP(&H2FE), 7) ' ESTAT DEL MODEM
  El$ = DecimalBinari$(INP(&H2FD), 7) ' ESTAT DE LINIA

  CX = X: CY = Y
  X = CSRLIN: Y = POS(0)
  VIEW PRINT 1 TO 25
  COLOR 0, 7: LOCATE 1, 70: PRINT TIME$: COLOR 7, 0
  VIEW PRINT 2 TO 24
  LOCATE X, Y
  Y = CY: X = CX


  IF MID$(El$, 8, 1) = "1" THEN
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
    CALL PrintPort(">", 0)
    PRINT "Terminals activades"
END SUB

