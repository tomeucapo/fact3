'$INCLUDE: 'D:\FACT3\FONTS\CAMPS.BI'

SUB CENTRAR (X, LIN$)
    L = LEN(LIN$)
    LOCATE X, 80 / 2 - L / 2: PRINT LIN$;
END SUB

FUNCTION ENCRIPT$ (CAD$, CARENC%)
    MAXLEN% = LEN(CAD$)
    ENCRIPT$ = SPACE$(MAXLEN%)
    FOR P = 1 TO MAXLEN%
        MID$(ENCRIPT$, P, 1) = CHR$(ASC(MID$(CAD$, P, 1)) XOR CARENC%)
    NEXT
END FUNCTION

SUB FASTCOL (X, Y, COL)
    DEF SEG = TESTSCREEN
    POKE X * 2 + Y * 160 + 1, COL
    DEF SEG
END SUB

SUB FASTPRINT (Y, X, COL, CAD$)
    DEF SEG = TESTSCREEN
    LO = LEN(CAD$)

    IF LO = 1 THEN
       CA = ASC(CAD$)
       POKE X * 2 + Y * 160, CA: POKE X * 2 + Y * 160 + 1, COL
       DEF SEG
       EXIT SUB
    END IF

    FOR LC = 1 TO LO
        CA = ASC(MID$(CAD$, LC, 1))
        POKE X * 2 + Y * 160, CA
        POKE X * 2 + Y * 160 + 1, COL
        X = X + 1
    NEXT
    DEF SEG
END SUB

SUB FINESTRA (ROW1%, COL1%, ROW2%, COL2%, SHADOW, STILBOX$) STATIC
    FIN = 1

    IF ROW2% < ROW1% + 1 THEN ROW2% = ROW1% + 1
    IF ROW2% > 25 THEN ROW2% = 25
    IF COL2% - 4 < COL1% THEN
       COL2% = COL2% + 4
       BOXWIDTH = COL2% - COL1% + 1
    ELSE
       BOXWIDTH = COL2% - COL1% + 1
    END IF
    IF COL2% > 80 THEN COL2% = 78

    X = ROW1%: Y = COL1%: X2 = ROW2%: Y2 = COL2%
    LOCATE ROW1%, COL1%
    PRINT MID$(STILBOX$, 1, 1); STRING$(BOXWIDTH - 2, MID$(STILBOX$, 2, 1)); MID$(STILBOX$, 3, 1);
    
    FOR A% = ROW1% + 1 TO ROW2% - 1
        LOCATE A%, COL1%
        PRINT MID$(STILBOX$, 4, 1); STRING$(BOXWIDTH - 2, " "); MID$(STILBOX$, 4, 1);
    NEXT A%


    LOCATE ROW2%, COL1%
    PRINT MID$(STILBOX$, 5, 1); STRING$(BOXWIDTH - 2, MID$(STILBOX$, 6, 1)); MID$(STILBOX$, 7, 1);


 IF SHADOW = 1 THEN
   IF COL2% < 80 THEN
      FOR s% = ROW1% + 1 TO ROW2%
        CALL FASTCOL(INT(COL1% + BOXWIDTH - 1), INT(s% - 1), 8)
      NEXT
   END IF

    IF ROW2% < 25 THEN
       FOR L% = COL1% TO COL1% + BOXWIDTH - 1
           CALL FASTCOL(INT(L%), INT(ROW2%), 8)
       NEXT
    END IF
 END IF
END SUB

SUB FONS (CAR)
    FOR L = 1 TO 25
        LOCATE L, 1: PRINT STRING$(80, CAR);
    NEXT
END SUB

SUB LCENTER (TEXT$)
    LPRINT TAB(41 - LEN(TEXT$) / 2); TEXT$
END SUB

SUB TEMPS (time)
    FOR I = 0 TO 1000
        FOR J = 0 TO 5 * time: NEXT
    NEXT
END SUB

FUNCTION TESTSCREEN
     ' ******************************************************************
     '  TEST DE TIPO PANTALLA.
     ' ******************************************************************
         DEF SEG = &H40
         MONO.SS = (PEEK(&H10) AND &H30) = &H30
         IF MONO.SS THEN
            TESTSCREEN = &HB000
         ELSE
            TESTSCREEN = &HB800
         END IF
     ' ******************************************************************
END FUNCTION

FUNCTION TIPUSVGA$
          DEF SEG = &HC000
          FOR P = 30 TO 55
              TIPU$ = TIPU$ + CHR$(PEEK(P))
          NEXT
          DEF SEG
          TIPUSVGA$ = TIPU$
END FUNCTION

FUNCTION VERSVGA$
          DEF SEG = &HC000                      ' TARJETA GR…FICA
          FOR P = 135 TO 180
              VER$ = VER$ + CHR$(PEEK(P))
          NEXT
          DEF SEG
          VERSVGA$ = VER$
END FUNCTION

