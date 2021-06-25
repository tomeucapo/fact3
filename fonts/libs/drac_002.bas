DECLARE SUB ShowDate ()
'умммммммммммммммммммммммммммммммммммммммммммммммммм╦
'Ё DRAC_002.BAS                                     Ё
'фмммммммммммммммммммммммммммммммммммммммммммммммммм╣
'Ё Biblioteca secundaria de gesti╒ (CAPA 2).        Ё
'Ё Aquesta biblioteca funciona damunt la biblioteca Ё
'Ё nucli CAMPS2.LIB                                 Ё
'фмммммммммммммммммммммммммммммммммммммммммммммммммм╣
'Ё Smart Software 1993/99 (C)                       Ё
'Ё Joan Miquel Payeras Cresp║ 1996 (C)              Ё
'Ё Tomeu Cap╒ Cap╒ 1998 (C)                         Ё
'фмммммммммммммммммммммммммммммммммммммммммммммммммм╣
'Ё Revisi╒ Summer'96  - 14/07/1996                  Ё
'Ё Revisi╒ A            29/08/1996                  Ё
'Ё Revisi╒ B            08/02/1997                  Ё
'Ё Revisi╒ C            26/01/1998                  Ё
'тмммммммммммммммммммммммммммммммммммммммммммммммммм╬
'
'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'D:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'D:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'


DEF FNInvertCadena$ (CAD$)
		Res$ = ""
		FOR I% = LEN(CAD$) TO 1 STEP -1
		    Res$ = Res$ + MID$(CAD$, I%, 1)
		NEXT
		FNInvertCadena$ = Res$
END DEF

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁSMART ACHOICE 2.0 1996/97 (c)                                Ё
'ЁTomeu Cap╒ Cap╒ 1997 (C)                                     Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
FUNCTION Achoice (X%, Y%, x2%, y2%, ELEM, CMATRIU$(), col(), TITOL$, POSEEK, CAD$)

		COLOR col(0, 0), col(0, 1)
		FINESTRA X%, Y%, x2%, y2%, 1, "им╩╨хм╪": COLOR col(2, 0)
		LOCATE X% + 1, Y% + 1: PRINT TITOL$: COLOR col(0, 0)
		LOCATE X% + 2, Y% + 1: PRINT STRING$(LEN(TITOL$), "д");
		XC% = X% + 3: R = 1: L = 1: LLETRA = 0

		'
		' ********* MOSTRA ELS REGISTRES QUE CABIGEN EN UNA PANTALLA
		'
		IF LTRIM$(RTRIM$(CAD$)) <> "" THEN
		   GOSUB CERCAR
		ELSE
		   GOSUB LISTA
		   XC% = X% + 3: R = 1             ' INICIA CURSOR I POSICIONS A PANTALLA
		   COLFONS = col(1, 1): COLCURS = col(1, 0): GOSUB SHOWCURSOR
		END IF
		DO
		   OP$ = INKEY$
		   IF RECURSOS.BARRAESTAT.STATUS = SON THEN CALL EstatTeclesControl(25, 3)
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
			       LLETRA = 1: GOSUB CERCAR
			 CASE ELSE
		   END SELECT
		LOOP
		RETURN
PUJA:
		IF XC% = X% + 3 THEN
		   COLFONS = col(0, 1): COLCURS = col(0, 0): GOSUB SHOWCURSOR
		   IF R = 1 THEN
		      XC% = X% + 3: R = 1
		      COLFONS = col(1, 1): COLCURS = col(1, 0): GOSUB SHOWCURSOR
		      RETURN
		   END IF
		   COLFONS = col(0, 1): COLCURS = col(0, 0): GOSUB SHOWCURSOR
		   R = R - 1: XC% = X% + 3
		   ScrollDown x2% - 2, y2% - 2, X% + 2, INT(Y%), 1
		   COLFONS = col(1, 1): COLCURS = col(1, 0): GOSUB SHOWCURSOR
		ELSE
		   COLFONS = col(0, 1): COLCURS = col(0, 0): GOSUB SHOWCURSOR
		   R = R - 1: XC% = XC% - 1
		   COLFONS = col(1, 1): COLCURS = col(1, 0): GOSUB SHOWCURSOR
		END IF
		RETURN
BAIXA:
		IF XC% = x2% - 1 THEN
		   COLFONS = col(0, 1): COLCURS = col(0, 0): GOSUB SHOWCURSOR
		   IF R = ELEM THEN
		      R = ELEM: XC% = x2% - 1
		      COLFONS = col(1, 1): COLCURS = col(1, 0): GOSUB SHOWCURSOR
		      RETURN
		   END IF
		   COLFONS = col(0, 1): COLCURS = col(0, 0): GOSUB SHOWCURSOR
		   R = R + 1: XC% = x2% - 1
		   Y = Y%
		   ScrollUp x2% - 2, y2% - 2, X% + 2, INT(Y%), 1
		   COLFONS = col(1, 1): COLCURS = col(1, 0): GOSUB SHOWCURSOR
		ELSE
		   COLFONS = col(0, 1): COLCURS = col(0, 0): GOSUB SHOWCURSOR
		   IF R = ELEM THEN
		      R = ELEM
		      COLFONS = col(1, 1): COLCURS = col(1, 0): GOSUB SHOWCURSOR
		      RETURN
		   END IF
		   R = R + 1: XC% = XC% + 1
		   COLFONS = col(1, 1): COLCURS = col(1, 0): GOSUB SHOWCURSOR
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
		COLOR col(0, 0), col(0, 1)
		FOR XC% = X% + 3 TO x2% - 1
		    LOCATE XC%, Y% + 1: PRINT STRING$(LEN(TITOL$), " ")
		NEXT
		FOR XC% = X% + 3 TO x2% - 1
		    IF R > ELEM THEN
		       EXIT FOR
		    ELSE
		       LOCATE XC%, Y% + 1: PRINT STRING$(LEN(TITOL$), " ")
		       COLFONS = col(0, 1): COLCURS = col(0, 0): GOSUB SHOWCURSOR
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
		    IF LLETRA = 1 THEN
		       IF INSTR(UCASE$(CBLIN$), UCASE$(OP$)) = POSEEK THEN
			  R = RC: TROBAT = 1
			  EXIT FOR
		       END IF
		    ELSE
		       IF INSTR(LTRIM$(RTRIM$(UCASE$(CBLIN$))), LTRIM$(RTRIM$(UCASE$(CAD$)))) THEN
			  R = RC: TROBAT = 1
			  EXIT FOR
		       END IF
		    END IF
		NEXT

		IF TROBAT = 1 THEN
		   GOSUB LISTA: R = RC
		   XC% = X% + 3
		   COLFONS = col(1, 1): COLCURS = col(1, 0): GOSUB SHOWCURSOR
		END IF
		RETURN
END FUNCTION

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment per mostrar un avis o error per la pantalla          Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
FUNCTION Avis (TITOL$, MENSAJ$, MESRES$, S)
	 GetBackground 10, 3, 16, 75, AVISBUFF$
	 COLOR 15, 9
	 FINESTRA 10, 3, 14, 73, 1, "им╩╨хм╪"
	 X = 11: Y = 5

	 COLOR 14
	 FOR L = 1 TO LEN(TITOL$)
	     LOCATE X, Y: PRINT MID$(TITOL$, L, 1)
	     Y = Y + 1: GOSUB RENOU
	 NEXT
	 Y = 5: X = X + 2: COLOR 15

	 FOR L = 1 TO LEN(MENSAJ$)
	     LOCATE X, Y: PRINT MID$(MENSAJ$, L, 1)
	     Y = Y + 1: GOSUB RENOU
	 NEXT

	 COLOR 27: LOCATE 11, 40: PRINT MESRES$: COLOR 15
	 DO
	  c$ = INKEY$
	 LOOP UNTIL c$ <> ""
	 PutBackground 10, 3, AVISBUFF$
	 SELECT CASE c$
		CASE CHR$(13)
		     Avis = 1
		CASE CHR$(27)
		     Avis = 0
		CASE ELSE
		     Avis = 9
	 END SELECT
	 EXIT FUNCTION

RENOU:
	IF S = 1 THEN
		 IF MID$(MENSAJ$, L, 1) <> CHR$(32) THEN
				PLAY "L64 O0 MS P20 G"
		 ELSE
				PLAY "L64 MS P20 A"
		 END IF
	END IF
	RETURN
END FUNCTION

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment per generar distins avisos sonors                    Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB Avis.Sonor (SO)
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
SUB Barra (nFila%, nCol%, nMax#, nActual)
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
	 I = 1: RD$ = "": TRO$ = ""
	 DO
	     TRO$ = MID$(DAT$, I, 1)
	     IF TRO$ <> "/" THEN
		RD$ = RD$ + TRO$
	     END IF
	     I = I + 1
	 LOOP UNTIL I > 8
	 DTN = VAL(FNInvertCadena$(RD$))
END FUNCTION

'
'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Controla la polsaci╒ de tecles de control                          Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB EstatTeclesControl (X, Y) STATIC

	 IF RECURSOS.BARRAESTAT.ACTIVAT = 0 THEN
	    COLOR 15: LOCATE X, Y - 1: PRINT "Ё       Ё";
	    RECURSOS.BARRAESTAT.ACTIVAT = 999
	 END IF

	 REGS.ax = &H1200
	 InterruptX &H16, REGS, REGS
	 CAD$ = NumeroEnBase$((REGS.ax), 2)
	 INS$ = MID$(CAD$, 1, 1): BLM$ = MID$(CAD$, 2, 1)
	 BLD$ = MID$(CAD$, 4, 1): BLN$ = MID$(CAD$, 3, 1)

	 IF V = 1 AND INS$ = "0" THEN
	    LOCATE X, Y + 4: PRINT "   "; : V = 0
	 ELSE
	    IF INS$ = "1" THEN
	       V = 1: COLOR 15: LOCATE X, Y + 4: PRINT "INS";
	    END IF
	 END IF

	 IF VB = 2 AND BLM$ = "0" THEN
	    LOCATE X, Y + 1: PRINT " "; : VB = 0
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

SUB FinestraEstat (TITOL$, RETARD)
    SHARED OLDTITOL$

    CADANI$ = "|/-\|/-\|"

    COLOR 15, 9
    IF TITOL$ <> OLDTITOL$ THEN
       FINESTRA 10, 30, 15, 30 + LEN(TITOL$) + 1, 1, "им╩╨хм╪"
       LOCATE 11, 31: PRINT TITOL$
       OLDTITOL$ = TITOL$
    END IF

    Y = (30 + LEN(TITOL$) + 1) - 30 \ 2
    IF RECURSOS.ANIMESTAT.FRAME > 9 OR RECURSOS.ANIMESTAT.FRAME <= 0 THEN
       RECURSOS.ANIMESTAT.FRAME = 1
    END IF

    IF RECURSOS.ANIMESTAT.POSIC > 30 + LEN(TITOL$) - 1 OR RECURSOS.ANIMESTAT.POSIC <= 0 THEN
       RECURSOS.ANIMESTAT.POSIC = 31
    END IF

    LOCATE 13, Y: PRINT MID$(CADANI$, RECURSOS.ANIMESTAT.FRAME, 1)
    LOCATE 14, 31: PRINT STRING$((30 + LEN(TITOL$) + 1) - 30 - 1, ".")
    LOCATE 14, RECURSOS.ANIMESTAT.POSIC: PRINT "*"

    RECURSOS.ANIMESTAT.FRAME = RECURSOS.ANIMESTAT.FRAME + 1
    RECURSOS.ANIMESTAT.POSIC = RECURSOS.ANIMESTAT.POSIC + 1
    IF RETARD <> 0 THEN TEMPS (RETARD)

END SUB

'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Funci╒ que canvia els espais per un altre car┘cter                 Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
FUNCTION ForaEspai$ (CAD$)
	 FOR L = 1 TO LEN(CAD$)
	     IF MID$(CAD$, L, 1) = CHR$(32) THEN MID$(CAD$, L, 1) = CHR$(0)
	 NEXT
	 ForaEspai$ = CAD$
END FUNCTION

SUB GetBackground (row1%, col1%, row2%, col2%, buffer$) STATIC
    IF row1% >= 1 AND row2% <= MAXROW AND col1% >= 1 AND col2% <= MAXCOL THEN
       Wid% = col2% - col1% + 1
       Hei% = row2% - row1% + 1
       size% = 4 + (2 * Wid% * Hei%)
       buffer$ = SPACE$(size%)
       CALL GetCopyBox(row1%, col1%, row2%, col2%, buffer$)
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
		IF ShowError = 1 THEN
		   RESUME NEXT
		ELSE
		   CLOSE #AREATXT, #AREAA
		   EXIT SUB
		END IF
END SUB

FUNCTION LlegeixParaula$ (M$, I!)
   p$ = ""
   L = LEN(M$)
   fparaula! = 0
   WHILE (I <= L) AND (fparaula = 0)
      c$ = MID$(M$, I, 1)
      IF c$ = " " THEN
	 fparaula = 1
      ELSE
	 p$ = p$ + c$
      END IF
   WEND
   LlegeixParaula = p$
END FUNCTION

'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Procediment que carrega masqueres ext┼rnes (.MSK)                  Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB LoadMask (NOM$)
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
SUB Mensaje (MENSAJ$)
    CENTRAR 24, MENSAJ$
END SUB

'
'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'Ё Procediment per fer un sol menё, sense comentaris                  Ё
'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB Menu2 (TEXTOSMENUP() AS STRING, CASO%, FILA%, COLUM%, ANCHO%, ITEMS%, CF, CT, CTS, CFS)

	IF TESTSCREEN = &HB000 THEN     ' MIRA A VEURE SI LA PANTALLA ES MONOCROM
	   CF = 7: CT = 0: CTS = 0: CFS = 7
	END IF
	COLOR CF, CT, 0
	FINESTRA FILA%, COLUM%, FILA% + 1 + ITEMS%, COLUM% + ANCHO% + 1, 1, "им╩╨хм╪"
	FOR ANTPOS% = 1 TO ITEMS%
	    GOSUB IMP.OPC
	NEXT

	COLOR CF, CT, 0
	POSICION% = 1: ANTPOS% = 1
	DO
	   SELECT CASE TECLA%
		  CASE 75
		       ANTPOS% = POSICION%
		       POSICION% = Rotacion(1, (ITEMS%), POSICION%, -1)
		  CASE 77
		       ANTPOS% = POSICION%
		       POSICION% = Rotacion(1, (ITEMS%), POSICION%, 1)
		  CASE 72
		       ANTPOS% = POSICION%
		       POSICION% = Rotacion(1, (ITEMS%), POSICION%, -1)
		  CASE 80
		       ANTPOS% = POSICION%
		       POSICION% = Rotacion(1, (ITEMS%), POSICION%, 1)
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
	      COLOR 14, CT: ShowDate: COLOR CF, CT, 0
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
EXIT SUB

IMP.OPC:
	 COLOR CF, CT, 0
	 LOCATE (FILA% + ANTPOS%), (COLUM% + 2): PRINT TEXTOSMENUP(ANTPOS%)
	 RETURN
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
    FINESTRA FILA%, COLUM%, FILA% + 1 + ITEMS%, COLUM% + ANCHO% + 2, 1, "им╩╨хм╪"

    FOR ANTPOS% = 1 TO ITEMS%
	T = CT: F = CF: POSI% = ANTPOS% \ 1
	GOSUB IMP.OPCI
    NEXT
    COLOR CF, CT, 0
    POSICION% = 1: ANTPOS% = 1
    DO
       SELECT CASE TECLA%
	      CASE 75
		   CASO% = -19: EXIT SUB
	      CASE 77
		   CASO% = -20: EXIT SUB
	      CASE 72
		   ANTPOS% = POSICION%
		   IF MID$(TEXTOSMENUP(ANTPOS% - 1), 1, 1) = "д" OR MID$(TEXTOSMENUP(ANTPOS% - 1), 1, 1) = "м" THEN
		      POSICION% = Rotacion(1, (ITEMS%), POSICION%, -2)
		   ELSE
		      POSICION% = Rotacion(1, (ITEMS%), POSICION%, -1)
		   END IF
	      CASE 80
		   ANTPOS% = POSICION%
		   IF ANTPOS% >= ITEMS% THEN
		      IF MID$(TEXTOSMENUP(ANTPOS%), 1, 1) = "д" OR MID$(TEXTOSMENUP(ANTPOS%), 1, 1) = "м" THEN
			 POSICION% = Rotacion(1, (ITEMS%), POSICION%, 2)
		      ELSE
			 POSCION% = Rotacion(1, (ITEMS%), POSICION%, 1)
		      END IF
		   ELSE
		      IF MID$(TEXTOSMENUP(ANTPOS% + 1), 1, 1) = "д" OR MID$(TEXTOSMENUP(ANTPOS% + 1), 1, 1) = "м" THEN
			 POSICION% = Rotacion(1, (ITEMS%), POSICION%, 2)
		      ELSE
			 POSCION% = Rotacion(1, (ITEMS%), POSICION%, 1)
		      END IF
		   END IF
	      CASE 13
		   CASO% = POSICION%
		   EXIT SUB
	      CASE 27
		   CASO% = 999
		   EXIT SUB
	      CASE 59
		   CASO% = 888
		   EXIT SUB
	      CASE 60 TO 68
		   CASO% = ((((60 - TECLA%) * -1) + 2) * -1)
		   EXIT SUB
	      CASE ELSE
	      END SELECT

	      T = CT: F = CF: POSI% = ANTPOS%: GOSUB IMP.OPCI
	      COLOR CF, CT: LOCATE 24, 2: PRINT SPACE$(78); : CENTRAR 24, COMMENT$(POSICION% - 1, I)
	      T = CFS: F = CTS: POSI% = POSICION%: GOSUB IMP.OPCI
	      DO
		K$ = INKEY$
		IF STATUS = SON THEN CALL EstatTeclesControl(25, 3)
		COLOR 14, CT: ShowDate: COLOR CF, CT, 0
	      LOOP WHILE K$ = ""
	    
	      LLETRA$ = UCASE$(K$)
	      FOR c% = 1 TO ITEMS%
		  CAD$ = TEXTOSMENUP(c%)
		  FOR p = 1 TO LEN(CAD$)
		      IF MID$(CAD$, p, 1) = "~" THEN
			 IF LLETRA$ = UCASE$(MID$(CAD$, p + 1, 1)) THEN
			    CASO% = c%
			    EXIT SUB
			 END IF
		      END IF
		  NEXT
	      NEXT
	      TECLA% = ASC(RIGHT$(K$, 1))
       LOOP UNTIL TECLA% = RETORNO
       EXIT SUB

IMP.OPCI:
	COLOR CF, CT, 0
	CAD$ = TEXTOSMENUP(POSI%): L = 1: X = 1
	DO
	   IF MID$(CAD$, L, 1) = "~" THEN
	      COLOR 0, CF
	   ELSE
	      COLOR F, T
	   END IF
	   IF MID$(CAD$, L, 1) = "~" THEN L = L + 1
	   LOCATE (FILA% + POSI%), (COLUM% + 1) + X: PRINT MID$(CAD$, L, 1)
	   L = L + 1: X = X + 1
	LOOP UNTIL L > LEN(CAD$)
	RETURN
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment per mostrar un error generat pel sistema             Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB MissatgeError (E, M$)
    ON LOCAL ERROR GOTO MessageError
    SOUND 100, 1: SOUND 200, 1: SOUND 100, 1
    IF Avis("ERROR:", M$, "Pitja <ENTER> per resumir", 0) = 1 THEN
       EXIT SUB
    END IF
MessageError:
    RESUME NEXT
END SUB

FUNCTION NumeroEnBase$ (num AS INTEGER, b AS INTEGER)
'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁFunci╒ que converteix un nombre decimal a binari                 Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
	DIM N(20) AS INTEGER

	I! = 1
	DO
	   N(I) = num MOD b
	   num = num \ b
	   I = I + 1
	LOOP UNTIL num = 0

	da$ = ""
	DO
	   da$ = da$ + LTRIM$(RTRIM$(STR$(N(I))))
	   I = I - 1
	LOOP UNTIL I = 0

	ERASE N

	NumeroEnBase$ = da$
END FUNCTION

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment generar un menё desplegable                          Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB PopMenu (XM, YM, X2M, OPCIO%, MENU$(), COMMENT$(), CF, CT, CFS, CTS, col(), estat()) STATIC
		SHARED TMP$
		
		COLOR CF, CT: Y = YM: I = 0
		DO
		   LOCATE XM, Y: PRINT MENU$(I, 0);
		   Y = Y + LEN(MENU$(I, 0)) + 2: I = I + 1
		LOOP UNTIL MENU$(I, 0) = ""

		IF estat(0) = 999 THEN
		   MAXI = I - 1: MAXY = Y - LEN(MENU$(MAXI, 0)) - 2
		   Y = YM: X = XM: I = estat(1)
		ELSE
		   IF estat(0) = 0 THEN
		      MAXI = I - 1: MAXY = Y - LEN(MENU$(MAXI, 0)) - 2
		      Y = YM: X = XM: I = 0
		   END IF
		END IF

		GetBackground 1, 1, 24, 79, MENUBUFF$
		
		LOCATE X, Y: COLOR CFS, CTS: PRINT MENU$(I, 0); : COLOR CF, CT: CENTRAR 24, COMMENT$(I, 0)
		IF estat(0) = 999 THEN Y = estat(3)
		ACTIVA$ = "Z": GOSUB OBRIMENU
		DO
		  DO
		    T$ = INKEY$
		    IF STATUS = SON THEN CALL EstatTeclesControl(25, 3)
		    COLOR 14, CT: ShowDate: COLOR CF, CT, 0
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

OBRIMENU:
		PutBackground 1, 1, MENUBUFF$
		
		COLOR CF, CT
		FOR c = 0 TO 10
		    IF MENU$(c, I + 1) = "" THEN EXIT FOR
		    TMP$(c + 1) = MENU$(c, I + 1)
		NEXT

		IF c = 0 THEN RETURN
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
		CALL MenuBar(TMP$(), COMMENT$(), I + 1, CASO%, XM%, YM%, L%, INT(c), CF, CT, CFS, CTS)
		SELECT CASE CASO%
		       CASE IS = -20
			    ERASE TMP$: GOSUB DRETA
		       CASE IS = -19
			    ERASE TMP$: GOSUB ESQUERRA
		       CASE IS = 999
			    GOSUB AMAGAMENU
		       CASE IS = 888
			    ERASE TMP$: OPCIO% = -1
			    EXIT SUB
		       CASE ELSE
			    SELECT CASE (CASO% * -1)
				   CASE 2 TO 7
				       GOSUB AMAGAMENU
				       Ic = (CASO% * (-1)) - 1
				       I = 0: Y = YM
				       FOR OP = 0 TO Ic - 1
					   GOSUB DRETA
				       NEXT
				       I = Ic
				       ACTIVA$ = "Z": GOTO OBRIMENU
				   CASE ELSE
			    END SELECT
			    ERASE TMP$
			    MONTA$ = LTRIM$(STR$(I + 1)) + LTRIM$(STR$(CASO%))
			    OPCIO% = VAL(MONTA$)
			    estat(1) = I: estat(0) = 999: estat(2) = XM%: estat(3) = YM%
			    EXIT SUB
		END SELECT
		ERASE TMP$
		RETURN

AMAGAMENU:
		ACTIVA$ = " "
		PutBackground 1, 1, MENUBUFF$
		ERASE TMP$: GOSUB SHOWBLOCK
		RETURN

SHOWBLOCK:
		IF ACTIVA$ = "Z" THEN GOSUB OBRIMENU
		COLOR CF, CT: LOCATE 24, 2: PRINT SPACE$(78); : CENTRAR 24, COMMENT$(I, 0)
		LOCATE X, Y: COLOR CFS, CTS: PRINT MENU$(I, 0);
		RETURN
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment que imprimeix un text en un espai delimitat      Ё
'Ёper quatre punts.                                            Ё
'Ё                                                             Ё
'ЁJoan Miquel Payeras i Cresp║ 1998 (C)                        Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB PrintByWord (x1, y1, x2, y2, c1, c2, Missatge$)
    COLOR c1, c2
    X = x1
    Y = y1
    I = 1
    paraula$ = LlegeixParaula(Missatge$, I)
    L = LEN(paraula$)
    WHILE L > 0
	  IF (x2 - X) < L THEN
	     X = x1
	     Y = Y + 1
	  END IF
	  IF Y <= y2 THEN
	     LOCATE Y, X
	     PRINT paraula$;
	     X = X + L + 1
	     paraula$ = LlegeixParaula(Missatge$, I)
	     L = LEN(paraula$)
	  ELSE
	     L = 0
	  END IF
    WEND
END SUB

SUB PutBackground (row%, col3%, buffer$)
    IF row% >= 1 AND row% <= MAXROW AND col3% >= 1 AND col3% <= MAXCOL THEN
       CALL PutCopyBox(row%, col3%, buffer$)
    END IF
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁFunci╒ que controla el cursos dels menёs                         Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
FUNCTION Rotacion (BAJO%, ALTO%, CORRIENTE%, CASO%)
	 CORRIENTE% = CORRIENTE% + CASO%
	 IF CORRIENTE% > ALTO% THEN CORRIENTE% = BAJO%
	 IF CORRIENTE% < BAJO% THEN CORRIENTE% = ALTO%
	 Rotacion = CORRIENTE%
END FUNCTION

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment que serveix per despla┤ar una regi╒ de la pantalla   Ё
'Ёcap a baix.                                                      Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB ScrollDown (X, Y, x2, y2, L)
		CAD$ = CHR$(&HB8) + CHR$(L) + CHR$(&H7) + CHR$(&HB9) + CHR$(y2) + CHR$(x2) + CHR$(&HBA) + CHR$(Y) + CHR$(X) + CHR$(&HB7) + CHR$(&H0) + CHR$(&HCD) + CHR$(&H10) + CHR$(&HCB)
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
SUB ScrollUp (X, Y, x2, y2, L)
		CAD$ = CHR$(&HB8) + CHR$(L) + CHR$(&H6) + CHR$(&HB9) + CHR$(y2) + CHR$(x2) + CHR$(&HBA) + CHR$(Y) + CHR$(X) + CHR$(&HB7) + CHR$(&H0) + CHR$(&HCD) + CHR$(&H10) + CHR$(&HCB)
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

SUB SetDirRecursos (DIREC$)
    RECURSOS.direcctori = DIREC$
END SUB

'зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
'ЁProcediment que activa la visualitzaci╒ de l'EstatTeclesControl()Ё
'юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
'
SUB SetScoreBoard (VALUE)
    RECURSOS.BARRAESTAT.STATUS = VALUE
END SUB

SUB ShowDate
    'IF RECURSOS.SHOWCLOCK THEN
	LOCATE 22, 3, 0, 0: PRINT "Hora..: "; FormatD$(Now#, "hh:mm:ss AM/PM");
	LOCATE 22, 62, 0, 0: PRINT "Data.: "; FormatD$(Now#, "dd-mm-yyyy");
    'END IF
END SUB

FUNCTION ShowError
	   IF RECURSOS.direcctori = "" THEN
	      IF Avis("ERROR:", "No ha assignat el direcctori dels recursos", "Pitja una tecla...", 0) <> 10 THEN
		 EXIT FUNCTION
	      END IF
	   END IF

	   NOM$ = LTRIM$(RTRIM$(RECURSOS.direcctori)) + "ERRORS.ERR"

	   AREAERRORS = FREEFILE
	   OPEN NOM$ FOR INPUT AS #AREAERRORS

	   DO UNTIL EOF(AREAERRORS)
	      INPUT #AREAERRORS, LIN$
	      IF MID$(LIN$, 1, 1) = ";" OR MID$(LIN$, 1, 1) = " " OR MID$(LIN$, 1, 1) = "" THEN
 
	      ELSE
		 NUMER% = VAL(MID$(LIN$, 1, 2))
		 COMM$ = MID$(LIN$, 4, 30)
		 IF NUMER% = ERR THEN EXIT DO
	      END IF
	   LOOP
	   DO
	     TECLA = Avis("ERROR " + STR$(ERR) + ":", COMM$ + " (" + LTRIM$(RTRIM$(STR$(ERL))) + ")", "<ENTER> Reintentar  <ESC> Anula", 0)
	   LOOP UNTIL TECLA = 1 OR TECLA = 0
	   CLOSE #AREAERRORS
	   ShowError = TECLA
END FUNCTION

