DECLARE FUNCTION ControlModem$ ()
DECLARE FUNCTION InitModem! (PORTI!, CADINIT$)
DECLARE FUNCTION StatModem$ ()

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT2\FONTS\CAMPS.BI'
'$INCLUDE: 'C:\FACT2\FONTS\VIDEOF.BI'

COMMON SHARED PORT

COLOR 15, 3: CLS
COLOR 15, 9
LOCATE 1, 1: PRINT STRING$(80, " ");
LOCATE 25, 1: PRINT STRING$(80, " ");
LOCATE 25 / 2, 1: PRINT STRING$(80, " ");
CENTRAR 25 / 2, "Ordinador Remot"

AREACOM2 = InitModem(2, "9600,N,8,1,CS2000,DS2000,CD,PE")

X = 2: Y = 1
X2 = (25 / 2) + 1: Y2 = 1

DO
    DO
      C$ = INKEY$
      COLOR 15, 9: LOCATE 1, 1, 0: PRINT "Estat: "; StatModem$; "  Control: "; ControlModem$; "   "; TIME$: COLOR 15, 3
      GOSUB RX
    LOOP UNTIL C$ <> ""
    SELECT CASE C$
	   CASE CHR$(13)
		IF X > 11 THEN
		   CALL SCROLLUP(10, 79, 2, 1, 1)
		ELSE
		   X = X + 1: Y = 1
		END IF
	   CASE ELSE

	       WHILE X AND 2 <> 2
		   OUT &H2F8 + 4, 1
		   X = INP(&H2F8 + 2)
	       WEND

	       OUT &H2F8 + 4, 3

	       WHILE X AND 1 <> 1
		   X = INP(&H2F8 + 2)
	       WEND

	       'IF MID$(StatModem$, 6, 1) = "1" THEN
		COLOR 15, 3
		IF Y > 80 THEN
		   Y = 1: X = X + 1
		   LOCATE X, Y, 1, 13, 14: PRINT C$;
		   OUT &H2F8, ASC(RIGHT$(C$, 1))
		ELSE
		   IF X > 11 THEN
		      CALL SCROLLUP(10, 79, 2, 1, 1)
		      LOCATE X, Y, 1, 13, 14: PRINT C$;
		      OUT &H2F8, ASC(RIGHT$(C$, 1))
		      Y = Y + 1
		   ELSE
		      LOCATE X, Y, 1, 13, 14: PRINT C$;
		      OUT &H2F8, ASC(RIGHT$(C$, 1))
		      Y = Y + 1
		   
		   END IF
		END IF
	       'END IF
    END SELECT

LOOP UNTIL C$ = CHR$(27)

CLOSE AREACOM2
END


RX:
    IF MID$(StatModem$, 5, 1) = "1" THEN
       IF Y2 > 80 THEN
	  Y2 = 1: X2 = X2 + 1
	  LOCATE X2, Y2: PRINT CHR$(INP(&H2F8))
       ELSE
	  IF X2 > 24 THEN
	     CALL SCROLLUP(23, 79, 12, 1, 1)
	     COLOR 14, 3
	     LOCATE 24, 1: PRINT STRING$(80, " ");
	     LOCATE X2, Y2: PRINT CHR$(INP(&H2F8))
	     Y2 = 1: X2 = 24
	  ELSE
	     LOCATE X2, Y2: PRINT CHR$(INP(&H2F8))
	     Y2 = Y2 + 1
	  END IF
       END IF


    END IF
    RETURN

FUNCTION ControlModem$
	 SELECT CASE PORT
		CASE 1
		     P = &H3FC
		CASE 2
		     P = &H2FC
		CASE ELSE
		     P = &H3FC
	 END SELECT
	 BYTE = INP(P)
	 ControlModem$ = DecimalBinari$(BYTE, 7)
END FUNCTION

FUNCTION InitModem (PORTI, CADINIT$)
    P$ = "COM" + LTRIM$(RTRIM$(STR$(PORTI))) + ":" + CADINIT$
    AREAPORT = FREEFILE
    OPEN P$ FOR RANDOM AS AREAPORT
    PORT = PORTI
    InitModem = AREAPORT
    
END FUNCTION

FUNCTION StatModem$
	 SELECT CASE PORT
		CASE 1
		     P = &H3FE
		CASE 2
		     P = &H2FE
		CASE ELSE
		     P = &H3FE
	 END SELECT
	 BYTE = INP(P)
	 StatModem$ = DecimalBinari$(BYTE, 7)
END FUNCTION

