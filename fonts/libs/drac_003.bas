DECLARE FUNCTION DecimalBinari$ (NUM!, BITS!)
DECLARE FUNCTION Achoice! (X%, Y%, X2%, Y2%, ELEM!, CMATRIU$(), COL!(), TITOL$, POSEEK!)
DECLARE SUB Avis (TITOL$, MENSAJ$, S!)
DECLARE FUNCTION NumeroEnBase$ (NUM!, B!)
DECLARE FUNCTION DTN! (DAT$)
DECLARE SUB EstatTeclesControl (X!, Y!)
DECLARE FUNCTION FORAESPAI$ (CAD$)
DECLARE SUB GetBackground (ROW1%, COL1%, ROW2%, COL2%, buffer$)
DECLARE SUB MenuBar (TEXTOSMENUP() AS STRING, COMMENT$(), I!, CASO%, FILA%, COLUM%, ANCHO%, ITEMS%, CF!, CT!, CTS!, CFS!)
DECLARE SUB PutBackground (row%, col3%, buffer$)
DECLARE FUNCTION ROTACION! (BAJO%, ALTO%, CORRIENTE%, CASO%)
DECLARE SUB SCROLLDOWN (X!, Y!, X2!, Y2!, L!)
DECLARE SUB SCROLLUP (X!, Y!, X2!, Y2!, L!)
'
'умммммммммммммммммммммммммммммммммммммммммммммммммм╦
'Ё DRAC_003.BAS                                     Ё
'фмммммммммммммммммммммммммммммммммммммммммммммммммм╣
'Ё Biblioteca secundaria de gesti╒ (CAPA 2).        Ё
'Ё Aquesta biblioteca funciona damunt la biblioteca Ё
'Ё nucli CAMPS3.LIB                                 Ё
'фмммммммммммммммммммммммммммммммммммммммммммммммммм╣
'Ё DRAC 1993/97 (C)                                 Ё
'Ё Joan Miquel Payeras Cresp║ 1996 (C)              Ё
'Ё Tomeu Cap╒ Cap╒ 1997 (C)                         Ё
'фмммммммммммммммммммммммммммммммммммммммммммммммммм╣
'Ё Revisi╒ Summer'96  - 14/07/1996                  Ё
'Ё                      29/08/1996                  Ё
'Ё Revisi╒            - 08/02/1997                  Ё
'тмммммммммммммммммммммммммммммммммммммммммммммммммм╬
'

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT2\FONTS\WCAMPS.BI'
'$INCLUDE: 'C:\FACT2\FONTS\DRAC3.BI'

DEF FNInvertCadena$ (CAD$)
		Res$ = ""
		FOR I% = LEN(CAD$) TO 1 STEP -1
		    Res$ = Res$ + MID$(CAD$, I%, 1)
		NEXT
		FNInvertCadena$ = Res$
END DEF

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁDRAC ACHOICE 1.0                                             Ё
'ЁTomeu Cap╒ Cap╒ 1996 (C)                                     Ё
'Ё                                                             Ё
'ЁControl del cursor per el llistat amb despla┤ament de barres Ё
'Ёfunci╒ parescuda al ACHOICE de Clipper (R)                   Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
FUNCTION Achoice (X%, Y%, X2%, Y2%, ELEM, CMATRIU$(), COL(), TITOL$, POSEEK)

		COLOR COL(0, 0), COL(0, 1)

		FINESTRA X%, Y%, X2%, Y2%, 1, "им╩╨хм╪": COLOR COL(2, 0)
		LOCATE X% + 1, Y% + 1: PRINT TITOL$: COLOR COL(0, 0)
		LOCATE X% + 2, Y% + 1: PRINT STRING$(LEN(TITOL$), "д");

		XC% = X% + 3: R = 1: L = 1

		'
		' ********* MOSTRA ELS REGISTRES QUE CABIGEN EN UNA PANTALLA
		'

		GOSUB LISTA

		XC% = X% + 3: R = 1             ' INICIA CURSOR I POSICIONS A PANTALLA

		COLFONS = COL(1, 1): COLCURS = COL(1, 0): GOSUB SHOWCURSOR
		
		DO
			OP$ = INKEY$
			IF STATUS = SON THEN CALL EstatTeclesControl(25, 3)
			SELECT CASE UCASE$(OP$)
			 CASE CHR$(0) + "P"
			GOSUB BAIXA
			 CASE CHR$(0) + "H"
			GOSUB PUJA
			 CASE CHR$(0) + "M"
			Achoice = -13
			EXIT FUNCTION
			 CASE CHR$(0) + "K"
			Achoice = -14
			EXIT FUNCTION
			 CASE CHR$(0) + ";"
			Achoice = -1
			EXIT FUNCTION
			 CASE CHR$(0) + "<"
			Achoice = -2
			EXIT FUNCTION
			 CASE CHR$(0) + "="
			Achoice = -3
			EXIT FUNCTION
			 CASE CHR$(0) + ">"
			Achoice = -4
			EXIT FUNCTION
			 CASE CHR$(0) + "?"
			Achoice = -5
			EXIT FUNCTION
			 CASE CHR$(0) + "@"
			Achoice = -6
			EXIT FUNCTION
			 CASE CHR$(0) + "A"
			Achoice = -7
			EXIT FUNCTION
			 CASE CHR$(0) + "D"
			Achoice = -10
			EXIT FUNCTION
			 CASE CHR$(32)
			Achoice = -15
			EXIT FUNCTION
			 CASE CHR$(27)
			Achoice = 0
			EXIT FUNCTION
			 CASE CHR$(13)
			Achoice = R
			EXIT FUNCTION
			 CASE "A" TO "Z"
			GOSUB CERCAR
			 CASE ELSE
			END SELECT
	 LOOP
	 RETURN


PUJA:
			 IF XC% = X% + 3 THEN
		COLFONS = COL(0, 1): COLCURS = COL(0, 0): GOSUB SHOWCURSOR
		IF R = 1 THEN
			 XC% = X% + 3: R = 1
			 COLFONS = COL(1, 1): COLCURS = COL(1, 0): GOSUB SHOWCURSOR
			 RETURN
		END IF
		COLFONS = COL(0, 1): COLCURS = COL(0, 0): GOSUB SHOWCURSOR
		R = R - 1: XC% = X% + 3
		SCROLLDOWN X2% - 2, Y2% - 2, X% + 2, INT(Y%), 1
		COLFONS = COL(1, 1): COLCURS = COL(1, 0): GOSUB SHOWCURSOR
			 ELSE
		COLFONS = COL(0, 1): COLCURS = COL(0, 0): GOSUB SHOWCURSOR
		R = R - 1: XC% = XC% - 1
		COLFONS = COL(1, 1): COLCURS = COL(1, 0): GOSUB SHOWCURSOR
			 END IF
			 RETURN

BAIXA:
			 IF XC% = X2% - 1 THEN
		COLFONS = COL(0, 1): COLCURS = COL(0, 0): GOSUB SHOWCURSOR

		IF R = ELEM THEN
			 R = ELEM: XC% = X2% - 1
			 COLFONS = COL(1, 1): COLCURS = COL(1, 0): GOSUB SHOWCURSOR
			 RETURN
		END IF

		COLFONS = COL(0, 1): COLCURS = COL(0, 0): GOSUB SHOWCURSOR
		R = R + 1: XC% = X2% - 1
		Y = Y%
		SCROLLUP X2% - 2, Y2% - 2, X% + 2, INT(Y%), 1
		COLFONS = COL(1, 1): COLCURS = COL(1, 0): GOSUB SHOWCURSOR
			 ELSE
		COLFONS = COL(0, 1): COLCURS = COL(0, 0): GOSUB SHOWCURSOR
		IF R = ELEM THEN
			 R = ELEM
			 COLFONS = COL(1, 1): COLCURS = COL(1, 0): GOSUB SHOWCURSOR
			 RETURN
		END IF
		R = R + 1: XC% = XC% + 1
		COLFONS = COL(1, 1): COLCURS = COL(1, 0): GOSUB SHOWCURSOR
			 END IF
			 RETURN

'************************************************************************
' Mostra el cursor a la posici╒ que esta
'************************************************************************

SHOWCURSOR:
		COLOR COLCURS, COLFONS
		LOCATE XC%, Y% + 1: PRINT STRING$(LEN(TITOL$), " ")
		OPC$ = CMATRIU$(R)
		LOCATE XC%, Y% + 1: PRINT OPC$
		RETURN

LISTA:
		COLOR COL(0, 0), COL(0, 1)
		FOR XC% = X% + 3 TO X2% - 1
	LOCATE XC%, Y% + 1: PRINT STRING$(LEN(TITOL$), " ")
		NEXT

		FOR XC% = X% + 3 TO X2% - 1
			 IF R > ELEM THEN
		EXIT FOR
			 ELSE
		LOCATE XC%, Y% + 1: PRINT STRING$(LEN(TITOL$), " ")
		COLFONS = COL(0, 1): COLCURS = COL(0, 0): GOSUB SHOWCURSOR
		R = R + 1
			 END IF
		NEXT
		RETURN

'уммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╦
'Ё Controla i cerca en la llista elements amb la primera lletra igual queЁ
'Ё la tecla polsada.                                                     Ё
'тммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╬
CERCAR:
		TROBAT = 0
		FOR RC = 1 TO ELEM
	CBLIN$ = CMATRIU$(RC)
	IF INSTR(CBLIN$, OP$) = POSEEK THEN
		 R = RC: TROBAT = 1
		 EXIT FOR
	END IF
		NEXT

		IF TROBAT = 1 THEN
			 GOSUB LISTA: R = RC
			 XC% = X% + 3
			 COLFONS = COL(1, 1): COLCURS = COL(1, 0): GOSUB SHOWCURSOR
		END IF
		RETURN
END FUNCTION

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment per mostrar un avis o error per la pantalla          Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB Avis (TITOL$, MENSAJ$, S)
	 GetBackground 10, 3, 16, 75, AVISBUFF$
	 COLOR 15, 9
	 FINESTRA 10, 3, 14, 73, 1, "им╩╨хм╪"
	 X = 11: Y = 5: COLOR 14

	 FOR L = 1 TO LEN(TITOL$)
	     LOCATE X, Y: PRINT MID$(TITOL$, L, 1)
	     Y = Y + 1: GOSUB RENOU
	 NEXT

	 Y = 5: X = X + 2: COLOR 15
	 FOR L = 1 TO LEN(MENSAJ$)
	     LOCATE X, Y: PRINT MID$(MENSAJ$, L, 1)
	     Y = Y + 1: GOSUB RENOU
	 NEXT
	 COLOR 27: LOCATE 11, 40: PRINT "Polsa una tecla per continuar...": COLOR 15

		T$ = INPUT$(1)
		PutBackground 10, 3, AVISBUFF$
		EXIT SUB

RENOU:
	IF S = 1 THEN
		 IF MID$(MENSAJ$, L, 1) <> CHR$(32) THEN
				PLAY "L64 O0 MS P20 G"
		 ELSE
				PLAY "L64 MS P20 A"
		 END IF
	END IF
	RETURN
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment per generar distins avisos sonors                    Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB AVIS.SONOR (SO)
	 SELECT CASE SO
		CASE 0
				 BEEP
		CASE 1
				 SOUND 300, 1: SOUND 100, 1
				 SOUND 100, 1: SOUND 300, 1
		CASE 2
				 PLAY "L20GBGBGB"
		CASE 3
				 SOUND 100, 1: SOUND 50, .3
				 SOUND 200, 1: SOUND 50, .3
				 SOUND 300, 1: SOUND 50, .3
				 SOUND 400, 1: SOUND 50, .3
				 SOUND 500, 1: SOUND 50, .3
		CASE ELSE
	 END SELECT
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment per fer una barra de process                         Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB BARRA (nFila%, nCol%, nMax#, nActual)
COLOR 15, 9
IF nActual = 1 THEN
	 FOR I = nFila% TO nFila% + 4
			 LOCATE I, nCol%: PRINT SPACE$(54)
	 NEXT
	 FINESTRA nFila%, nCol%, nFila% + 4, nCol% + 54, 1, "им╩╨хм╪"
	 LOCATE nFila% + 1, nCol% + 2: PRINT "0%"
	 LOCATE nFila% + 1, nCol% + 52: PRINT "%"
	 COLOR 15
	 LOCATE nFila% + 2, nCol% + 2: PRINT STRING$(51, "╠")
END IF

nPos = INT((nActual * 100) / nMax#)
LOCATE nFila% + 1, nCol% + 49: PRINT LTRIM$(STR$(nPos))
LOCATE nFila% + 2, nCol% + 2 + (nPos / 2): PRINT "ш"

END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment per encriptar fitxers (BASTOS)                       Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB CRYPTSQR (FITXER$, CAP$)
DIM CRY AS CRIPT

AREAORG = FREEFILE: OPEN FITXER$ FOR RANDOM SHARED AS AREAORG LEN = 1
AREACRY = FREEFILE: OPEN "TEMPORAL.CRY" FOR RANDOM SHARED AS AREACRY LEN = LEN(CRY)
FIELD AREAORG, 1 AS C$

RMAX = LOF(AREAORG)

GET AREACRY, 1, CRY
IF LTRIM$(RTRIM$(CRY.CPY)) = LTRIM$(RTRIM$(CAP$)) THEN
	 BEEP: KILL "TEMPORAL.CRY"
	 PRINT "ERROR: EL FITXER " + FITXER$ + " JA HA ESTA ENCRIPTAT !!!"
	 CLOSE AREAORG, AREACRY: EXIT SUB
END IF

CRY.CPY = CAP$
CRY.CAP = RMAX
PUT AREACRY, 1, CRY
FOR R = 1 TO RMAX
		 GET AREAORG, R
		 CRY.CPY = "": CRY.CAP = 0
		 CRY.X = SQR(ASC(C$))
		 PUT AREACRY, R + 1, CRY
NEXT
CLOSE AREACRY: CLOSE AREAORG

KILL FITXER$
SHELL ("COPY TEMPORAL.CRY " + FITXER$ + " >nul")
KILL "TEMPORAL.CRY"
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁFunci╒ que converteix un nombre decimal a binari                 Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
FUNCTION DecimalBinari$ (NUM, BITS)

	DIM N(BITS) AS INTEGER
	FOR NUMBIT = BITS TO 0 STEP -1
			N(NUMBIT) = NUM MOD 2
			NUM = INT(NUM / 2)
	NEXT

	FOR D = 0 TO BITS
			Da$ = Da$ + LTRIM$(RTRIM$(STR$(N(D))))
	NEXT
	ERASE N
	DecimalBinari$ = Da$

END FUNCTION

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment per des-encriptar fitxers (BASTOS)                   Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB DESCRYPTSQR (FITXER$, CAP$)
DIM CRY AS CRIPT
AREAORG = FREEFILE: OPEN "TEMPORAL.CRY" FOR RANDOM SHARED AS AREAORG LEN = 1
AREACRY = FREEFILE: OPEN FITXER$ FOR RANDOM SHARED AS AREACRY LEN = LEN(CRY)
FIELD 1, 1 AS C$

'RMAX = LOF(AREACRY) \ LEN(CRY)
R = 1: R2 = 2

GET AREACRY, 1, CRY
IF LTRIM$(RTRIM$(CRY.CPY)) <> LTRIM$(RTRIM$(CAP$)) THEN
	 BEEP: KILL "TEMPORAL.CRY"
	 PRINT "ERROR: EL FITXER " + FITXER$ + " NO ESTA ENCRIPTAT !!!"
	 CLOSE AREAORG, AREACRY: EXIT SUB
END IF
RMAX = CRY.CAP

FOR R = 1 TO RMAX
		 GET AREACRY, R + 1, CRY
		 A = (CRY.X * CRY.X)
		 LSET C$ = CHR$(A)
		 PUT AREAORG, R
NEXT

CLOSE AREACRY, AREAORG
KILL FITXER$
SHELL ("COPY TEMPORAL.CRY " + FITXER$ + " >NULL")
KILL "TEMPORAL.CRY"
KILL "NULL"
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁOrdena una taula                                                 Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB DIMSort (ARRAY$(), MAX) STATIC
		OFFSET = MAX - 1 \ 2
		DO WHILE OFFSET > 0
			 LIMIT = MAX - 1 - OFFSET
			 DO
	 SWITCH = FALSE
	 FOR I = 1 TO LIMIT
			 IF ARRAY$(I) > ARRAY$(I + OFFSET) THEN
		SWAP ARRAY$(I), ARRAY$(I + OFFSET)
		SWITCH = I
			 END IF
	 NEXT
	 LIMIT = SWITCH
			 LOOP WHILE SWITCH
			 OFFSET = OFFSET \ 2
		LOOP
END SUB

'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁFUNCI╒ PER TRANSFORMAR LA DATA A CAMP NUM┼RIC                       Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
FUNCTION DTN (DAT$)
			 I = 1
			 RD$ = "": TRO$ = ""
			 DO
			   TRO$ = MID$(DAT$, I, 1)

			   IF TRO$ <> "/" THEN
			      RD$ = RD$ + TRO$
			   END IF
			   I = I + 1
			 LOOP UNTIL I > 8
			 DTN = VAL(FNInvertCadena$(RD$))
END FUNCTION

'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Controla la polsaci╒ de tecles de control                          Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB EstatTeclesControl (X, Y) STATIC
	 IF ACTIV.ESTAT% = 0 THEN
			COLOR 15: LOCATE X, Y - 1: PRINT "Ё       Ё";
			ACTIV.ESTAT% = 999
	 END IF
	 REGS.ax = &H1200
	 InterruptX &H16, REGS, REGS
	 CAD$ = DecimalBinari$((REGS.ax), 7)
	 INS$ = MID$(CAD$, 1, 1): BLM$ = MID$(CAD$, 2, 1): BLN$ = MID$(CAD$, 3, 1)
	 BLD$ = MID$(CAD$, 4, 1)
	 IF V = 1 AND INS$ = "0" THEN
			LOCATE X, Y + 4: PRINT "   ";
			V = 0
	 ELSE
			IF INS$ = "1" THEN
	 V = 1
	 COLOR 15: LOCATE X, Y + 4: PRINT "INS";
			END IF
	 END IF

	 IF VB = 2 AND BLM$ = "0" THEN
			LOCATE X, Y + 1: PRINT " ";
			VB = 0
	 ELSE
			IF BLM$ = "1" THEN
	 VB = 2
	 COLOR 2: LOCATE X, Y + 1: PRINT "M";
			END IF
	 END IF


	 IF VN = 2 AND BLN$ = "0" THEN
			LOCATE X, Y: PRINT " ";
			VN = 0
	 ELSE
			IF BLN$ = "1" THEN
	 VN = 2
	 COLOR 14: LOCATE X, Y: PRINT "N";
			END IF
	 END IF

	 IF VC = 2 AND BLD$ = "0" THEN
			LOCATE X, Y + 2: PRINT " ";
			VC = 0
	 ELSE
			IF BLD$ = "1" THEN
	 VC = 2
	 COLOR 4: LOCATE X, Y + 2: PRINT "D";
			END IF
	 END IF
	 COLOR 7
END SUB

'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Funci╒ que canvia els espais per un altre car┘cter                 Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
FUNCTION FORAESPAI$ (CAD$)
	 FOR L = 1 TO LEN(CAD$)
			 IF MID$(CAD$, L, 1) = CHR$(32) THEN MID$(CAD$, L, 1) = CHR$(0)
	 NEXT
	 FORAESPAI$ = CAD$
END FUNCTION

SUB GetBackground (ROW1%, COL1%, ROW2%, COL2%, buffer$) STATIC

		IF ROW1% >= 1 AND ROW2% <= MAXROW AND COL1% >= 1 AND COL2% <= MAXCOL THEN
	Wid% = COL2% - COL1% + 1
	Hei% = ROW2% - ROW1% + 1
	size% = 4 + (2 * Wid% * Hei%)
	buffer$ = SPACE$(size%)
	CALL GetCopyBox(ROW1%, COL1%, ROW2%, COL2%, buffer$)
		END IF

END SUB

'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Envia un fitxer de text a l'impresora o a un altre fitxer          Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB ImprimeixFitxerTXT (NOM$, DEVICE$, COLS)
		NOM$ = UCASE$(NOM$)
		ON LOCAL ERROR GOTO ERRORS
		AREATXT = FREEFILE: OPEN NOM$ FOR INPUT AS AREATXT
		AREAA = FREEFILE: OPEN DEVICE$ FOR OUTPUT AS AREAA

		WIDTH LPRINT COLS
		WIDTH #AREAA, COLS

		DO UNTIL EOF(AREATXT)
			 LINE INPUT #AREATXT, LINIA$
			 PRINT #AREAA, LINIA$
		LOOP

		CLOSE #AREATXT, #AREAA
		EXIT SUB

ERRORS:
		BEEP
		SELECT CASE ERR
		 CASE 68
		Avis "ERROR:", "PERIFERIC NO VALID", 0
		EXIT SUB
		 CASE 25
		Avis "ERROR:", "FALLA EL PERIFERIC " + DEVICE$, 0
		 CASE 61
		Avis "ERROR:", "EL DISC ESTA PLE", 0
		EXIT SUB
		 CASE 53
		Avis "ERROR:", "EL FITXER " + NOM$ + " NO A ESTAT TROBAT", 0
		EXIT SUB
		 CASE 27
		Avis "AVIS:", "IMPRESORA SENSE PAPER", 0
		 CASE 24
		Avis "ERROR:", "TEMPS DE ACCESS AL PERIFERIC " + DEVICE$ + " EXEDIT", 0
		EXIT SUB
		 CASE ELSE
		END SELECT
		RESUME NEXT

END SUB

'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Procediment que carrega masqueres ext┼rnes (.MSK)                  Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB LOADMASK (NOM$)
DIM PQ%(6)

NOUFITXER = FREEFILE
OPEN NOM$ FOR INPUT AS NOUFITXER
LIN$ = ""
DO UNTIL EOF(NOUFITXER)
	 INPUT #NOUFITXER, LIN$
	 SELECT CASE UCASE$(LIN$)
		CASE "//"

		CASE ";"

		CASE "#NEW_MASK"

		CASE "#SET_COLOR"
				 FOR L = 1 TO 3
			 INPUT #NOUFITXER, Sub2$
			 IF MID$(TEX$, 1, 1) = ";" OR MID$(TEX$, 1, 2) = "//" THEN

			 ELSE
					PQ%(L) = VAL(Sub2$)
			 END IF
				 NEXT
				 COLOR PQ%(1), PQ%(2), PQ%(3)

		CASE "#SET_FINESTRA"
				FOR L = 1 TO 5
				 INPUT #NOUFITXER, MAXO$
				 IF MID$(TEX$, 1, 1) = ";" OR MID$(TEX$, 1, 2) = "//" THEN

				 ELSE
			 PQ%(L) = VAL(MAXO$)
				 END IF
				NEXT
				FINESTRA PQ%(1), PQ%(2), PQ%(3), PQ%(4), INT(PQ%(5)), "им╩╨хм╪"

		CASE "#SET_LINE"
				 FOR L = 1 TO 3
			 INPUT #NOUFITXER, TEX$
			 IF MID$(TEX$, 1, 1) = ";" OR MID$(TEX$, 1, 2) = "//" THEN

			 ELSE
					PQ%(L) = VAL(TEX$)
			 END IF
				 NEXT
				 INPUT #NOUFITXER, TEX$
				 LOCATE PQ%(1), PQ%(2): PRINT STRING$(PQ%(3), TEX$);

		CASE "#SET_TEXT"
				 FOR L = 1 TO 2
			 INPUT #NOUFITXER, TEX$
			 IF MID$(TEX$, 1, 1) = ";" OR MID$(TEX$, 1, 2) = "//" THEN

			 ELSE
					PQ%(L) = VAL(TEX$)
			 END IF
				 NEXT
				 INPUT #NOUFITXER, TEX$
				 LOCATE PQ%(1), PQ%(2): PRINT TEX$;

		CASE "#END_MASK"
				 CLOSE #NOUFITXER
				 EXIT DO
		CASE ELSE

	 END SELECT
LOOP
CLOSE #NOUFITXER
END SUB

'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Visualitza un missatge centrat a la part baixa de la pantalla      Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB MENSAJE (MENSAJ$)
		CENTRAR 24, MENSAJ$
END SUB

'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Procediment per fer un sol menё, sense comentaris                  Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB MENU2 (TEXTOSMENUP() AS STRING, CASO%, FILA%, COLUM%, ANCHO%, ITEMS%, CF, CT, CTS, CFS)

		 IF TESTSCREEN = &HB000 THEN     ' MIRA A VEURE SI LA PANTALLA ES MONOCROM
	CF = 7: CT = 0: CTS = 0: CFS = 7
		 END IF
		 COLOR CF, CT, 0
		 FINESTRA FILA%, COLUM%, FILA% + 1 + ITEMS%, COLUM% + ANCHO% + 1, 1, "им╩╨хм╪"
		 FOR ANTPOS% = 1 TO ITEMS%
	 COLOR CF, CT, 0
	 LOCATE (FILA% + ANTPOS%), (COLUM% + 2): PRINT TEXTOSMENUP(ANTPOS% \ 1)
		 NEXT
		 COLOR CF, CT, 0
		 POSICION% = 1
		 ANTPOS% = 1
		 DO
			 SELECT CASE TECLA%
				CASE 75
			 ANTPOS% = POSICION%
			 POSICION% = ROTACION(1, (ITEMS%), POSICION%, -1)
				CASE 77
			 ANTPOS% = POSICION%
			 POSICION% = ROTACION(1, (ITEMS%), POSICION%, 1)
				CASE 72
			 ANTPOS% = POSICION%
			 POSICION% = ROTACION(1, (ITEMS%), POSICION%, -1)
				CASE 80
			 ANTPOS% = POSICION%
			 POSICION% = ROTACION(1, (ITEMS%), POSICION%, 1)
				CASE 13
			 SALEM% = 0
			 EXIT DO
				CASE 27
			 SALEM% = 1
			 EXIT DO
				CASE 59
			 SALEM% = 2
			 EXIT DO
				CASE ELSE
			 END SELECT

			 LOCATE (FILA% + ANTPOS%), COLUM% + 1: PRINT STRING$(ANCHO%, " ")
			 LOCATE (FILA% + ANTPOS%), (COLUM% + 2): PRINT TEXTOSMENUP(ANTPOS%)

			 COLOR CTS, CFS, 0
			 LOCATE (FILA% + POSICION%), (COLUM% + 2)
			 PRINT TEXTOSMENUP(POSICION%)
			 DO
	 K$ = INKEY$
	 IF STATUS = SON THEN CALL EstatTeclesControl(25, 3)
	 COLOR 14, CT
	 LOCATE 22, 2, 0, 0: PRINT "Hora..: "; TIME$
	 LOCATE 22, 64, 0, 0: PRINT "Data.: "; DATA$(0)
	 COLOR CF, CT, 0
			 LOOP WHILE K$ = ""
			 TECLA% = ASC(RIGHT$(K$, 1))
		 LOOP UNTIL TECLA% = RETORNO
		 IF SALEM% = 1 THEN
	CASO% = 999
		 ELSE
	IF SALEM% = 2 THEN
		 CASO% = 888
	ELSE
		 CASO% = POSICION%
	END IF
		 END IF
END SUB

'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Procediment per fer un sol menё, amb comentaris, fet tamb┌ per     Ё
'Ё anar amb el procediment PopUp Menё.                                Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB MenuBar (TEXTOSMENUP() AS STRING, COMMENT$(), I, CASO%, FILA%, COLUM%, ANCHO%, ITEMS%, CF, CT, CTS, CFS)
		 IF TESTSCREEN = &HB000 THEN     ' MIRA A VEURE SI LA PANTALLA ES MONOCROM
	CF = 7: CT = 0
	CTS = 0: CFS = 7
		 END IF
		 COLOR CF, CT, 0
		 FINESTRA FILA%, COLUM%, FILA% + 1 + ITEMS%, COLUM% + ANCHO% + 1, 1, "им╩╨хм╪"
		 FOR ANTPOS% = 1 TO ITEMS%
	 COLOR CF, CT, 0
	 LOCATE (FILA% + ANTPOS%), (COLUM% + 1): PRINT TEXTOSMENUP(ANTPOS% \ 1)
		 NEXT
		 COLOR CF, CT, 0
		 POSICION% = 1: ANTPOS% = 1
		 DO
			 SELECT CASE TECLA%
				CASE 75
			 SALEM% = -14: EXIT DO
				CASE 77
			 SALEM% = -13: EXIT DO
				CASE 72
			 ANTPOS% = POSICION%
			 IF MID$(TEXTOSMENUP(ANTPOS% - 1), 1, 1) = "д" OR MID$(TEXTOSMENUP(ANTPOS% - 1), 1, 1) = "м" THEN
					POSICION% = ROTACION(1, (ITEMS%), POSICION%, -2)
			 ELSE
					POSICION% = ROTACION(1, (ITEMS%), POSICION%, -1)
			 END IF
				CASE 80
			 ANTPOS% = POSICION%
			 IF ANTPOS% >= ITEMS% THEN
					IF MID$(TEXTOSMENUP(ANTPOS%), 1, 1) = "д" OR MID$(TEXTOSMENUP(ANTPOS%), 1, 1) = "м" THEN
			 POSICION% = ROTACION(1, (ITEMS%), POSICION%, 2)
					ELSE
			 POSCION% = ROTACION(1, (ITEMS%), POSICION%, 1)
					END IF
			 ELSE
					IF MID$(TEXTOSMENUP(ANTPOS% + 1), 1, 1) = "д" OR MID$(TEXTOSMENUP(ANTPOS% + 1), 1, 1) = "м" THEN
			 POSICION% = ROTACION(1, (ITEMS%), POSICION%, 2)
					ELSE
			 POSCION% = ROTACION(1, (ITEMS%), POSICION%, 1)
					END IF
			 END IF
				CASE 13
			 SALEM% = 0
			 EXIT DO
				CASE 27
			 SALEM% = 1
			 EXIT DO
				CASE 59
			 SALEM% = 2
			 EXIT DO
				CASE ELSE
			 END SELECT
			 LOCATE (FILA% + ANTPOS%), COLUM% + 1: PRINT STRING$(ANCHO%, " ")
			 LOCATE (FILA% + ANTPOS%), (COLUM% + 1): PRINT TEXTOSMENUP(ANTPOS%)
			 COLOR CF, CT: LOCATE 24, 2: PRINT SPACE$(78); : CENTRAR 24, COMMENT$(POSICION% - 1, I)
			 COLOR CTS, CFS, 0
			 LOCATE (FILA% + POSICION%), (COLUM% + 1): PRINT TEXTOSMENUP(POSICION%)
			 DO
	 K$ = INKEY$
	 IF STATUS = SON THEN CALL EstatTeclesControl(25, 3)
	 COLOR 14, CT
	 LOCATE 22, 2, 0, 0: PRINT "Hora..: "; TIME$
	 LOCATE 22, 64, 0, 0: PRINT "Data.: "; DATA$(0)
	 COLOR CF, CT, 0
			 LOOP WHILE K$ = ""

		 TECLA% = ASC(RIGHT$(K$, 1))
		 LOOP UNTIL TECLA% = RETORNO
		 SELECT CASE SALEM%
			CASE 0
		 CASO% = POSICION%
			CASE 1
		 CASO% = 999
			CASE 2
		 CASO% = 888
			CASE -13
		 CASO% = -19
			CASE -14
		 CASO% = -4
			CASE ELSE
		 END SELECT
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment per mostrar un error generat pel sistema             Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB MissatgeError (E, M$)
		ON LOCAL ERROR GOTO MessageError
		SOUND 100, 1: SOUND 200, 1: SOUND 100, 1
		COLOR 28, 9: CENTRAR 24, "ERROR: " + M$
		DO: LOOP WHILE INKEY$ = ""
		EXIT SUB
MessageError:
		RESUME NEXT
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment generar un menё desplegable                          Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB PopMenu (XM, YM, X2M, OPCIO%, MENU$(), COMMENT$(), CF, CT, CFS, CTS, COL())
		SHARED TMP$

		COLOR CF, CT: Y = YM
		DO
		   LOCATE XM, Y: PRINT MENU$(I, 0);
		   Y = Y + LEN(MENU$(I, 0)) + 2: I = I + 1
		LOOP UNTIL MENU$(I, 0) = ""

		MAXI = I - 1: MAXY = Y - LEN(MENU$(MAXI, 0)) - 2
		Y = YM: X = XM: I = 0

		GetBackground 1, 1, 24, 79, MENUBUFF$
		LOCATE X, Y: COLOR CFS, CTS: PRINT MENU$(I, 0); : COLOR CF, CT: CENTRAR 24, COMMENT$(I, 0)
		ACTIVA$ = "Z": GOSUB OBRIMENU

		DO
		  DO
		     T$ = INKEY$
		     IF STATUS = SON THEN CALL EstatTeclesControl(25, 3)
		     COLOR 14, CT
		     LOCATE 22, 2, 0, 0: PRINT "Hora..: "; TIME$
		     LOCATE 22, 64, 0, 0: PRINT "Data.: "; DATA$(0)
		     COLOR CF, CT, 0
		  LOOP UNTIL T$ <> ""

		  SELECT CASE T$
			 CASE CHR$(0) + "K"
			      GOSUB ESQUERRA
			 CASE CHR$(0) + "M"
			      GOSUB DRETA
			 CASE CHR$(0) + ";"
			      OPCIO% = 888
			      EXIT SUB
			 CASE CHR$(13)
			      ACTIVA$ = "Z": GOSUB OBRIMENU
			 CASE CHR$(27)
			      OPCIO% = 999
			      EXIT SUB
			 CASE ELSE
		  END SELECT
		LOOP

'****************************************************************************
' Rutines per els moviments
'****************************************************************************

DRETA:
		LOCATE X, Y: COLOR CF, CT: PRINT MENU$(I, 0);
		
		IF Y < MAXY THEN
		   Y = Y + LEN(MENU$(I, 0)) + 2: I = I + 1
		   GOSUB SHOWBLOCK
		ELSE
		   Y = YM: I = 0
		   GOSUB SHOWBLOCK
		END IF
		RETURN

ESQUERRA:
		LOCATE X, Y: COLOR CF, CT: PRINT MENU$(I, 0);
		IF Y > 2 THEN
		   Y = Y - LEN(MENU$(I - 1, 0)) - 2: I = I - 1
		   GOSUB SHOWBLOCK
		ELSE
		   Y = MAXY: I = MAXI
		   GOSUB SHOWBLOCK
		END IF
		RETURN

'****************************************************************************
' Rutina que obri el submenu
'****************************************************************************
OBRIMENU:
		PutBackground 1, 1, MENUBUFF$
		
		COLOR CF, CT
		FOR C = 0 TO 10
		    IF MENU$(C, I + 1) = "" THEN EXIT FOR
		    TMP$(C + 1) = MENU$(C, I + 1)
		NEXT

		IF C = 0 THEN RETURN
		L% = INT(Y) + INT(LEN(TMP$(1))) + 1
		IF L% >= 80 THEN
		   L% = INT(LEN(TMP$(1))) + 1
		   YM% = INT(Y) - L% + INT(LEN(MENU$(I, 0)))
		   XM% = INT(X2M)
		ELSE
		   YM% = INT(Y)
		   L% = INT(LEN(TMP$(1))) + 1
		   XM% = INT(X2M)
		END IF

		LOCATE X, Y: COLOR CFS, CTS: PRINT MENU$(I, 0);
		CALL MenuBar(TMP$(), COMMENT$(), I + 1, CASO%, XM%, YM%, L%, INT(C), CF, CT, CFS, CTS)
		SELECT CASE CASO%
		       CASE IS = -19
			    ERASE TMP$: GOSUB DRETA
		       CASE IS = -4
			    ERASE TMP$: GOSUB ESQUERRA
		       CASE IS = 999
			    ACTIVA$ = " "
			    PutBackground 1, 1, MENUBUFF$
			    ERASE TMP$: GOSUB SHOWBLOCK
		       CASE IS = 888
			    ERASE TMP$: OPCIO% = -1
			    EXIT SUB
		       CASE ELSE
			    ERASE TMP$
			    MONTA$ = LTRIM$(STR$(I + 1)) + LTRIM$(STR$(CASO%))
			    OPCIO% = VAL(MONTA$)
		       EXIT SUB
		END SELECT
		ERASE TMP$
		RETURN

SHOWBLOCK:
		IF ACTIVA$ = "Z" THEN GOSUB OBRIMENU
		COLOR CF, CT: LOCATE 24, 2: PRINT SPACE$(78); : CENTRAR 24, COMMENT$(I, 0)
		LOCATE X, Y: COLOR CFS, CTS: PRINT MENU$(I, 0);
		RETURN
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment que imprimeix un text en un espai determinat     Ё
'Ёdefinit per quatre punts.                                    Ё
'Ё                                                             Ё
'ЁJoan Miquel Payeras i Cresp║ 1996 (C)                        Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB PrintByWord (x1, y1, X2, Y2, c1, c2, Missatge$)
		COLOR c1, c2: X = x1: Y = y1
		FOR N = 1 TO LEN(Missatge$)
	p$ = MID$(Missatge$, N, 1)
	Paraula$ = Paraula$ + p$
	IF p$ = " " OR N = LEN(Missatge$) THEN
		 IF (X2 - X) < LEN(Paraula$) THEN X = x1: Y = Y + 1
		 IF Y > (Y2 + 1) THEN EXIT SUB
		 LOCATE Y, X: PRINT Paraula$; : X = X + LEN(Paraula$)
		 Paraula$ = ""
	END IF
		NEXT
END SUB

SUB PutBackground (row%, col3%, buffer$)

		IF row% >= 1 AND row% <= MAXROW AND col3% >= 1 AND col3% <= MAXCOL THEN
	CALL PutCopyBox(row%, col3%, buffer$)
		END IF

END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment restaurar una pantalla de text                       Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB RestoreScreen (FILE$, INICI)
		DEF SEG = TESTSCREEN
		BLOAD FILE$, INICI
		DEF SEG
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁFunci╒ que controla el cursos dels menёs                         Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
FUNCTION ROTACION (BAJO%, ALTO%, CORRIENTE%, CASO%)
	 CORRIENTE% = CORRIENTE% + CASO%
	 IF CORRIENTE% > ALTO% THEN CORRIENTE% = BAJO%
	 IF CORRIENTE% < BAJO% THEN CORRIENTE% = ALTO%
	 ROTACION = CORRIENTE%
END FUNCTION

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment guardar una pantalla de text dins disc               Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB SaveScreen (FILE$, INICI, FI)
		DEF SEG = TESTSCREEN
		BSAVE FILE$, INICI, FI
		DEF SEG
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment que serveix per despla┤ar una regi╒ de la pantalla   Ё
'Ёcap a baix.                                                      Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB SCROLLDOWN (X, Y, X2, Y2, L)
		CAD$ = CHR$(&HB8) + CHR$(L) + CHR$(&H7) + CHR$(&HB9) + CHR$(Y2) + CHR$(X2) + CHR$(&HBA) + CHR$(Y) + CHR$(X) + CHR$(&HB7) + CHR$(&H0) + CHR$(&HCD) + CHR$(&H10) + CHR$(&HCB)
		DIM LIS(1 TO 7)

		p = VARPTR(LIS(1))
		DEF SEG = VARSEG(LIS(1))
		FOR I = 0 TO 13
			 J = ASC(MID$(CAD$, I + 1, 1))
			 POKE (p + I), J
		NEXT I

		DEF SEG = VARSEG(LIS(1))
		CALL Absolute(VARPTR(LIS(1)))
		DEF SEG
		CAD$ = ""
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment que serveix per despla┤ar una regi╒ de la pantalla   Ё
'Ёcap adalt.                                                       Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB SCROLLUP (X, Y, X2, Y2, L)
		CAD$ = CHR$(&HB8) + CHR$(L) + CHR$(&H6) + CHR$(&HB9) + CHR$(Y2) + CHR$(X2) + CHR$(&HBA) + CHR$(Y) + CHR$(X) + CHR$(&HB7) + CHR$(&H0) + CHR$(&HCD) + CHR$(&H10) + CHR$(&HCB)
		DIM LIS(1 TO 7)

		p = VARPTR(LIS(1))
		DEF SEG = VARSEG(LIS(1))
		FOR I = 0 TO 13
			 J = ASC(MID$(CAD$, I + 1, 1))
			 POKE (p + I), J
		NEXT I

		DEF SEG = VARSEG(LIS(1))
		CALL Absolute(VARPTR(LIS(1)))
		DEF SEG
		CAD$ = ""
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment que activa la visualitzaci╒ de l'EstatTeclesControl()Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB SetScoreBoard (VALUE)
		STATUS = VALUE
END SUB

