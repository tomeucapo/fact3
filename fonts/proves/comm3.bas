'
' COMM3.BAS
' Programa de comunicacions amb cable modem nul
' Ver. 0.397
'
' 15/11/1997
' Tomeu Cap¢ i Cap¢ 1997 (C)
' Smart Software 1993/97 (C)
'

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\fact3\fonts\STRUCTS.BI'
'$INCLUDE: 'C:\fact3\fonts\SDK_001.BI'
'$INCLUDE: 'C:\fact3\fonts\DRAC3.BI'

DECLARE FUNCTION BinariDecimal! (CAD$)
DEF FNInvertCadena$ (CAD$)
		Res$ = ""
		FOR I% = LEN(CAD$) TO 1 STEP -1
		    Res$ = Res$ + MID$(CAD$, I%, 1)
		NEXT
		FNInvertCadena$ = Res$
END DEF

RESET

BASEC = &H2F8
CMODEM = &H2FB
CLINIA = &H2FC

ELINIA = &H2FD
EMODEM = &H2FE

OPEN "COM2:9600,N,8,1,CS0,DS0,PE" FOR RANDOM AS #1
CLOSE #1

     CLS : LOCATE 6, 1
     'DO
    '    C$ = INKEY$
	GOSUB WELCOME
    ' LOOP UNTIL C$ <> ""
     DO
       DO
	 C$ = INKEY$
	 GOSUB SHOWESTAT
	 IF C$ = CHR$(27) THEN GOSUB FI
	 GOSUB comprovar
       LOOP UNTIL C$ <> ""

       IF MID$(EL$, 7, 1) = "1" THEN
	  IF MID$(EM$, 5, 1) = "0" THEN
	     WHILE MID$(EM$, 2, 1) = "1"
		   GOSUB SHOWESTAT
	     WEND
	     OUT BASEC, ASC(MID$(C$, 1, 1))
	     COLOR 11: PRINT C$; : COLOR 7
	  END IF
       END IF

     LOOP UNTIL C$ = CHR$(27)

FI:
     END

WELCOME:
     CAD$ = CHR$(12) + "Benvingut al SERIAL-COMM 1.0 " + CHR$(13) + CHR$(10): GOSUB ENVIA.CADENA
     CAD$ = "----------------------------- " + CHR$(13) + CHR$(10): GOSUB ENVIA.CADENA
     RETURN

comprovar:
	 IF MID$(EL$, 1, 1) = "1" THEN
	    COLOR 14: PRINT CHR$(INP(BASEC)); : COLOR 7
	 END IF
	 IF MID$(EL$, 3, 1) = "1" THEN COLOR 12: PRINT "ERROR: de paritat": COLOR 7
	 IF MID$(EL$, 8, 1) = "1" THEN COLOR 12: PRINT "ERROR: Temp exedit": COLOR 7
	 RETURN

SHOWESTAT:
	 X = CSRLIN: Y = POS(0)
	 VIEW PRINT 1 TO 25
	 EM$ = FNInvertCadena$(DecimalBinari$(INP(EMODEM), 7))  ' ESTAT DEL MODEM
	 EL$ = FNInvertCadena$(DecimalBinari$(INP(ELINIA), 7))  ' ESTAT DE LINIA
	 CM$ = FNInvertCadena$(DecimalBinari$(INP(CMODEM), 7))  ' ESTAT DEL MODEM
	 CL$ = FNInvertCadena$(DecimalBinari$(INP(CLINIA), 7))  ' ESTAT DE LINIA

	 COLOR 15, 2: LOCATE 1, 1: PRINT "         Modem    Linia   ": COLOR 10
	 LOCATE 2, 1: PRINT "Estat  : "; EM$; " "; EL$
	 LOCATE 3, 1: PRINT "Control: "; CM$; " "; CL$: COLOR 7, 0
	 LOCATE 1, 60: PRINT TIME$
	 VIEW PRINT 6 TO 24
	 LOCATE X, Y
	 
	 RETURN

ENVIA.CADENA:
     L = 1
     DO
	   GOSUB SHOWESTAT
	   IF MID$(EL$, 7, 1) = "1" THEN
	      IF MID$(EM$, 5, 1) = "0" THEN
		 WHILE MID$(EM$, 2, 1) = "1"
		       GOSUB SHOWESTAT
		 WEND
	      
		 OUT BASEC, ASC(MID$(CAD$, L, 1))
		 COLOR 14: PRINT MID$(CAD$, L, 1); : COLOR 7
		 L = L + 1
	      END IF
	   END IF
     LOOP UNTIL L = LEN(CAD$)
     RETURN

FUNCTION BinariDecimal (CAD$)
	 f = 0
	 FOR SY = 7 TO 0 STEP -1
	     f = f + VAL(MID$(CAD$, SY + 1, 1)) * 2 ^ (7 - SY)
	 NEXT
	 BinariDecimal = f
END FUNCTION

