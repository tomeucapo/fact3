'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
'³ Fitxer: CAMPS2.BAS                                          ³
'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
'³ Llibreria per gestionar camps. Versi¢ 2.0                   ³
'³ i altres procediments del nucli.                            ³
'³                                                             ³
'³ 10/08/1996                                                  ³
'³ Summer'96                                                   ³
'³                                                             ³
'³ Smart'1993/96 (C).                                          ³
'³ Tomeu Cap¢ Cap¢ 1996 (C)                                    ³
'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

'$INCLUDE: 'C:\FACT3\FONTS\CAMPS.BI'

Copyright$ = " Material propietat de Smart Software 1993-1996 (C) "

DEFINT A-Z
SUB BOX (ROW1%, COL1%, ROW2%, COL2%) STATIC
    BOXWIDTH% = COL2% - COL1% + 1

    LOCATE ROW1%, COL1%
    PRINT "Ú"; STRING$(BOXWIDTH% - 2, "Ä"); "¿";

    FOR A% = ROW1% + 1 TO ROW2% - 1
        LOCATE A%, COL1%
        PRINT "³"; SPACE$(BOXWIDTH% - 2); "³";
    NEXT A%

    LOCATE ROW2%, COL1%
    PRINT "À"; STRING$(BOXWIDTH% - 2, "Ä"); "Ù";

END SUB

DEFSNG A-Z
SUB CENTRAR (X, LIN$)
    L = LEN(LIN$)
    LOCATE X, 80 / 2 - L / 2: PRINT LIN$;
END SUB

FUNCTION DATA$ (FORMAT)
    SELECT CASE FORMAT
           CASE 0
                ME$ = MID$(DATE$, 1, 2)
                DIA$ = MID$(DATE$, 4, 2)
                ANNO$ = MID$(DATE$, 9, 2)
                DATA$ = DIA$ + "/" + ME$ + "/" + ANNO$
           CASE 1
                ME$ = MID$(DATE$, 1, 2)
                DIA$ = MID$(DATE$, 4, 2)
                ANNO$ = MID$(DATE$, 7, 4)
                DATA$ = DIA$ + "/" + ME$ + "/" + ANNO$
           CASE 2
                ME$ = MID$(DATE$, 4, 2)
                DIA$ = MID$(DATE$, 1, 2)
                ANNO$ = MID$(DATE$, 9, 4)
                DATA$ = DIA$ + "/" + ME$ + "/" + ANNO$
           CASE ELSE
                DATA$ = "99/99/9999"
    END SELECT

END FUNCTION

' *********************************************
' * VISUALITZA TOTS ELS CAMPS DEFINITS        *
' *********************************************
'
SUB DISPLAYALL
    FOR C = 0 TO MAXCAMPS
        COLOR CAMPS(C).COLORS.CTEXT1, CAMPS(C).COLORS.CTEXT2, 0
        TI$ = LTRIM$(RTRIM$(CAMPS(C).TITOL))
        YT = CAMPS(C).Y - LEN(TI$) - 1
        IF YT < 1 THEN YT = 1
        LOCATE CAMPS(C).X, YT, 0: PRINT TI$;
        COLOR CAMPS(C).COLORS.CTITOL1, CAMPS(C).COLORS.CTITOL2, 0
        LOCATE CAMPS(C).X, CAMPS(C).Y, 0: PRINT MID$(CAMPS(C).CONTI, 1, CAMPS(C).LLARG);
        CAM = C: GOSUB VISUMASKSHOW
    NEXT
    EXIT SUB

VISUMASKSHOW:
    LMAX = LEN(LTRIM$(RTRIM$(CAMPS(CAM).MASK)))
    I% = CAMPS(CAM).Y: V% = CAMPS(CAM).Y: LV = L
    FOR L = 1 TO LMAX
        LOCATE CAMPS(CAM).X, I%, 0
        SELECT CASE MID$(CAMPS(CAM).MASK, L, 1)
               CASE IS = "9"
                    IF MID$(CAMPS(CAM).CONTI, L, 1) > CHR$(47) AND NOT MID$(CAMPS(CAM).CONTI, L, 1) > CHR$(57) THEN
                       PRINT MID$(CAMPS(CAM).CONTI, L, 1)
                    END IF
                    I% = I% + 1
               CASE IS = "X"
                    PRINT MID$(CAMPS(CAM).CONTI, L, 1)
                    I% = I% + 1
               CASE CHR$(39) TO CHR$(47), ":"
                    MID$(CAMPS(CAM).CONTI, L, 1) = MID$(CAMPS(CAM).MASK, L, 1)
                    PRINT MID$(CAMPS(CAM).MASK, L, 1)
                    I% = I% + 1
               CASE ELSE
                    PRINT MID$(CAMPS(CAM).CONTI, L, 1)
                    I% = I% + 1
        END SELECT
    NEXT
    L = LV
    RETURN

END SUB

FUNCTION ENCRIPT$ (CAD$, CARENC%)
    MAXLEN% = LEN(CAD$)
    ENCRIPT$ = SPACE$(MAXLEN%)
    FOR P = 1 TO MAXLEN%
        MID$(ENCRIPT$, P, 1) = CHR$(ASC(MID$(CAD$, P, 1)) XOR CARENC%)
    NEXT
END FUNCTION

' *********************************************
' * BORRA EL CONTINGUT DELS CAMPS DEFINITS    *
' *********************************************
'
SUB ERASEFIELD (CAM)
   FOR P = 1 TO CAMPS(CAM).LLARG
       MID$(CAMPS(CAM).CONTI, P, 1) = CHR$(0)
   NEXT
   CAMPS(CAM).CONTI = ""
END SUB

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

' *********************************************
' * CREA UNA FINESTRA                         *
' *********************************************
'
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

' *********************************************
' * INICIALTZA UN CAMP                        *
' *********************************************
'
SUB INITCAMP (CAM, X, Y, TIP, CARENC, MASK$, TITOL$)
    CAMPS(CAM).X = X
    CAMPS(CAM).Y = Y
    CAMPS(CAM).LLARG = LEN(MASK$)
    CAMPS(CAM).TIPUS = TIP
    CAMPS(CAM).CARENC = CARENC
    CAMPS(CAM).MASK = MASK$
    CAMPS(CAM).TITOL = TITOL$
END SUB

' *********************************************
' * INSERTA UN VALOR A UN CAMP                *
' *********************************************
'
SUB INSERTCAMP (CAM, QUE$)
    FOR P = 1 TO CAMPS(CAM).LLARG                 ' BUIDAR EL CAMP
        MID$(CAMPS(CAM).CONTI, P, 1) = CHR$(32)
    NEXT
    CAMPS(CAM).CONTI = QUE$
END SUB

DEFINT A-Z
SUB LCENTER (TEXT$)
    LPRINT TAB(41 - LEN(TEXT$) / 2); TEXT$
END SUB

DEFSNG A-Z
' *********************************************
' * LLEGEIX UN CAMP                           *
' *********************************************
' * ACTUALITZACI¢ 2.0                         *
' * 08/10/1996                                *
' *********************************************
'
FUNCTION LLEGIRCAMP (CAM)
    
    GOSUB LLEGIRCOLORS
    QUE = CAMPS(CAM).TIPUS

    IF LEN(LTRIM$(RTRIM$(CAMPS(CAM).CONTI))) = 0 THEN
       CAMPS(CAM).CONTI = STRING$(CAMPS(CAM).LLARG, CHR$(32))
    END IF

    IF QUE = 3 THEN          ' SI EL TIPUS DE CAMP ES LA DATA
       IF CAMPS(CAM).LLARG > 10 THEN CAMPS(CAM).LLARG = 10
       COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, " ");
       IF CAMPS(CAM).CONTI = "" OR LEN(CAMPS(CAM).CONTI) = 0 THEN
          ME$ = MID$(DATE$, 1, 2)
          DIA$ = MID$(DATE$, 4, 2)
          ANNO$ = MID$(DATE$, 9, 2)
          CAMPS(CAM).CONTI = DIA$ + "/" + ME$ + "/" + ANNO$
       ELSE
          MID$(CAMPS(CAM).CONTI, 3, 1) = "/"
          MID$(CAMPS(CAM).CONTI, 6, 1) = "/"
       END IF
       GOSUB VISUMASK
       LOCATE CAMPS(CAM).X, CAMPS(CAM).Y: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
    ELSE
       GOSUB VISUMASK
       COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, " ");
       LOCATE CAMPS(CAM).X, CAMPS(CAM).Y: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
       AVIS = 999
    END IF
    
     
    LONGITUT = 0: INICI = 0
    LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1, 13, 14
    X = CAMPS(CAM).X: Y = CAMPS(CAM).Y: L = 1

    GOSUB COMPCAD
    DO
       DO
          T$ = INKEY$
          GOSUB comp: GOSUB COMPINS
       LOOP UNTIL T$ <> ""
       ' ********************************************************************************
       ' COMPROVA SI EL CAMP ES NUMERIC I S'HA DE ESBORRAR EL NOMBRE EXISTENT SI HA NI HA
       ' ********************************************************************************
       IF T$ <> CHR$(13) AND NOT T$ = CHR$(0) + "K" OR T$ = CHR$(0) + "M" OR T$ = CHR$(0) + "H" OR T$ = CHR$(0) + "P" THEN
         IF T$ > CHR$(40) AND NOT T$ > CHR$(57) THEN
          IF AVIS = 999 AND QUE = 2 AND LTRIM$(RTRIM$(CAMPS(CAM).CONTI)) <> "" THEN
             CAMPS(CAM).CONTI = STRING$(CAMPS(CAM).LLARG, CHR$(32))
             GOSUB VISUMASK
             LOCATE CAMPS(CAM).X, CAMPS(CAM).Y: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
             AVIS = 0
          END IF
         END IF
       END IF

       SELECT CASE T$
              CASE CHR$(12)
              CASE CHR$(7)
                   SOUND 50, .5
              CASE CHR$(0) + "S"
                   F = L
                   COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 0: PRINT STRING$(CAMPS(CAM).LLARG, " ");
                   BUFFER$ = CAMPS(CAM).CONTI
                   BUFFER$ = LEFT$(BUFFER$, L - 1) + RIGHT$(BUFFER$, LEN(BUFFER$) - L)
                   CAMPS(CAM).CONTI = BUFFER$
                   COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 0: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                   L = F
                   LOCATE X, Y, 0
                   GOSUB VISUMASK
                   LOCATE X, Y, 1

              CASE CHR$(0) + "R" TO CHR$(0) + "G"
              CASE CHR$(0) + "K"
                   IF L <= 1 THEN
                      L = 1
                      Y = CAMPS(CAM).Y: LOCATE X, Y
                   ELSE
                      Y = Y - 1
                      L = L - 1
                      LOCATE X, Y
                   END IF
              CASE CHR$(0) + "M"
                   IF L >= CAMPS(CAM).LLARG THEN
                      L = 1: Y = CAMPS(CAM).Y: LOCATE X, Y
                   ELSE
                      Y = Y + 1: L = L + 1: LOCATE X, Y
                   END IF
              CASE CHR$(0) + "H"
                   IF CAM = 0 THEN
                      CAM = 0
                   ELSE
                      COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, " ");
                      COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                      CAM = CAM - 1
                      GOSUB LLEGIRCOLORS
                      X = CAMPS(CAM).X: Y = CAMPS(CAM).Y
                      COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, " ");
                      COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                      LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1
                      L = 1
                   END IF
              CASE CHR$(0) + "P"
                   IF CAM >= MAXCAMPS THEN
                      CAM = MAXCAMPS
                   ELSE
                      COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, " ");
                      COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                      CAM = CAM + 1
                      GOSUB LLEGIRCOLORS
                      X = CAMPS(CAM).X: Y = CAMPS(CAM).Y
                      COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, " ");
                      COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                      LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1
                      L = 1
                   END IF
              CASE CHR$(13)
                    GOSUB LLEGIRCOLORS
                    SELECT CASE QUE
                           CASE 2
                                COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                           CASE 4
                                COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, "*");
                           CASE ELSE
                                COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                    END SELECT
                    GOSUB VISUMASK
                    LLEGIRCAMP = CAM
                    EXIT FUNCTION
              CASE CHR$(8)
                   IF L <= 1 THEN
                      L = 1
                   ELSE
                      GOSUB comp
                      GOSUB COMPDEL
                   END IF
              CASE CHR$(27)
                   COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, " ");
                   COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                   LLEGIRCAMP = 999
                   EXIT FUNCTION
              CASE CHR$(0) + CHR$(F1) TO CHR$(0) + CHR$(F10)
                   LLEGIRCAMP = ASC(MID$(T$, 2, 1))
                   EXIT FUNCTION
              CASE ELSE
                   QUE = CAMPS(CAM).TIPUS
                   SELECT CASE QUE
                          ' ******* CAMPS NORMALS *******
                          '
                          ' X = ALFANUMERIC
                          ' 9 = NUMERIC
                          '
                          CASE 1 TO 3
                               IF L <> CAMPS(CAM).LLARG + 1 THEN
                                  GOSUB COMPCAD
                                  SELECT CASE M$
                                         CASE "9"
                                              IF T$ > CHR$(40) AND NOT T$ > CHR$(57) THEN
                                                 IF T$ = "." THEN
                                                    LP = INSTR(1, CAMPS(CAM).MASK, ".")
                                                    IF LP = 0 THEN
                                                       MID$(CAMPS(CAM).CONTI, L, 1) = T$
                                                       LOCATE X, Y, 1: PRINT T$;
                                                       Y = Y + 1: L = L + 1
                                                    ELSE
                                                       GOSUB MONTA.NUM
                                                    END IF
                                                 ELSE
                                                    MID$(CAMPS(CAM).CONTI, L, 1) = T$
                                                    LOCATE X, Y, 1: PRINT T$;
                                                    Y = Y + 1: L = L + 1
                                                 END IF
                                              END IF
                                         CASE "X"
                                              LOCATE X, Y, 1: PRINT T$;
                                              MID$(CAMPS(CAM).CONTI, L, 1) = T$
                                              Y = Y + 1: L = L + 1
                                         CASE ELSE
                                  END SELECT
                                  
                               ELSE
                                  Y = CAMPS(CAM).Y + CAMPS(CAM).LLARG
                                  LOCATE X, Y
                               END IF
                          ' ******* CAMPS ENCRIPTATS *******
                          CASE 4
                               IF L > CAMPS(CAM).LLARG THEN
                                  L = CAMPS(CAM).LLARG
                                  Y = Y - 1
                               END IF
                               GOSUB COMPCAD
                               LOCATE X, Y, 1: PRINT "*";
                               IF ASC(T$) <> 32 THEN MID$(CAMPS(CAM).CONTI, L, 1) = CHR$(ASC(T$) XOR CAMPS(CAM).CARENC)
                               Y = Y + 1: L = L + 1
                          CASE ELSE
                   END SELECT
       END SELECT
    LOOP

'***********************************************************
' Si el camp est… buit l'omplim de espais
'***********************************************************

COMPCAD:
    IF LEN(CAMPS(CAM).CONTI) = 0 THEN
       CAMPS(CAM).CONTI = STRING$(CAMPS(CAM).LLARG, " "): L = 1
    END IF
    RETURN

'***********************************************************
'Vigia de la mascara de camp
'***********************************************************

comp:
    M$ = MID$(CAMPS(CAM).MASK, L, 1)
    RETURN

'***********************************************************
'Inserta car…cters d'acord amb la mascara de camp
'***********************************************************

COMPINS:
    IF M$ > CHR$(39) AND NOT M$ > CHR$(47) OR M$ = CHR$(58) THEN
       LOCATE X, Y, 1
       Y = Y + 1: L = L + 1
    END IF
    RETURN

'***********************************************************
'Borra car…cters d'acord amb la mascara de camp
'***********************************************************

COMPDEL:
    IF M$ > CHR$(39) AND NOT M$ > CHR$(47) OR M$ = CHR$(58) THEN
       LOCATE X, Y, 1
       Y = Y - 1: L = L - 1
    ELSE
       IF D = 0 THEN
          Y = Y - 1: L = L - 1
          M$ = MID$(CAMPS(CAM).MASK, L, 1)
       ELSE
          D = 0
       END IF
       IF M$ > CHR$(39) AND NOT M$ > CHR$(47) OR M$ = CHR$(58) THEN
          LOCATE X, Y, 1
          Y = Y - 1: L = L - 1
          D = 1
       ELSE
          LOCATE X, Y: PRINT " ";
          MID$(CAMPS(CAM).CONTI, L, 1) = " "
          LOCATE X, Y
       END IF
    END IF

'***********************************************************
'Llegeix els color del camp Fons+Texte
'***********************************************************

LLEGIRCOLORS:
    CT1 = CAMPS(CAM).COLORS.CTITOL1: CC1 = CAMPS(CAM).COLORS.CCAMP1
    CT2 = CAMPS(CAM).COLORS.CTITOL2: CC2 = CAMPS(CAM).COLORS.CCAMP2
    RETURN

'***********************************************************
'Visualitza la mascara del camp i el seu contingut
'***********************************************************

VISUMASK:         
    LMAX = LEN(LTRIM$(RTRIM$(CAMPS(CAM).MASK)))
    I% = CAMPS(CAM).Y: V% = CAMPS(CAM).Y: LV = L
    FOR L = 1 TO LMAX
        LOCATE CAMPS(CAM).X, I%, 1
        SELECT CASE MID$(CAMPS(CAM).MASK, L, 1)
               CASE IS = "9"
                    IF MID$(CAMPS(CAM).CONTI, L, 1) > CHR$(47) AND NOT MID$(CAMPS(CAM).CONTI, L, 1) > CHR$(57) THEN
                       PRINT MID$(CAMPS(CAM).CONTI, L, 1)
                    END IF
                    I% = I% + 1
               CASE IS = "X"
                    PRINT MID$(CAMPS(CAM).CONTI, L, 1)
                    I% = I% + 1
               CASE CHR$(39) TO CHR$(47), ":"
                    MID$(CAMPS(CAM).CONTI, L, 1) = MID$(CAMPS(CAM).MASK, L, 1)
                    PRINT MID$(CAMPS(CAM).MASK, L, 1)
                    I% = I% + 1
               CASE ELSE
                    PRINT MID$(CAMPS(CAM).CONTI, L, 1)
                    I% = I% + 1
        END SELECT
    NEXT
    L = LV
    RETURN

MONTA.NUM:
    NM$ = LEFT$((CAMPS(CAM).CONTI), L)
    J = 1
    MID$(CAMPS(CAM).CONTI, 1, L) = STRING$(L, 32)
    GOSUB VISUMASK
    COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
    MID$(CAMPS(CAM).CONTI, LP - L + 1, L) = NM$

    LE = L
    L = (L + LP) - LE + 1
    Y = (Y + LP) - LE + 1
    LOCATE X, Y, 0
    GOSUB VISUMASK
    LOCATE X, Y, 1
    RETURN
END FUNCTION

SUB OBRIRFINESTRA (X, Y, X2, Y2)

    XT = X: YT = Y: XT2 = X2: YT2 = Y2
    IF Y > 1 THEN
       YT = YT + 1: YT2 = YT2 - 1
    END IF
    DO
       YT = YT + 1: YT2 = YT2 - 1
       PRINT YT, YT2
       C$ = INPUT$(1)
    LOOP UNTIL YT = YT2 + 1

    YC = YT: YC2 = YT2

    FOR YC = YT TO Y STEP -1
        FINESTRA INT(X), INT(YC), INT(X2), INT(YC2), 1, "+Í+³+Í+"
        'TEMPS (1)
        IF YC2 = Y2 THEN  ELSE YC2 = YC2 + 1
    NEXT
    
END SUB

SUB RESETCAMPS
   FOR P = 0 TO MAXCAMPS
       FOR C = 1 TO CAMPS(P).LLARG
           MID$(CAMPS(P).CONTI, P, 1) = CHR$(0)
       NEXT
       CAMPS(P).CONTI = ""
       CAMPS(P).MASK = ""
   NEXT
   ERASE CAMPS
END SUB

SUB SETCOLORCAMPS (CAM, CC1, CC2, CT1, CT2, CTX1, CTX2)
    CAMPS(CAM).COLORS.CTITOL1 = CT1
    CAMPS(CAM).COLORS.CTITOL2 = CT2
    CAMPS(CAM).COLORS.CCAMP1 = CC1
    CAMPS(CAM).COLORS.CCAMP2 = CC2
    CAMPS(CAM).COLORS.CTEXT1 = CTX1
    CAMPS(CAM).COLORS.CTEXT2 = CTX2
END SUB

SUB SETMAXCAMPS (MAXI)
    MAXCAMPS = MAXI
END SUB

FUNCTION SHOWFIELD$ (CAM)
         QUE = CAMPS(CAM).TIPUS

         IF QUE = 4 THEN
            FOR P = 1 TO CAMPS(CAM).LLARG
                MID$(CAMPS(CAM).CONTI, P, 1) = CHR$(ASC(MID$(CAMPS(CAM).CONTI, P, 1)) XOR CAMPS(CAM).CARENC)
            NEXT
            FOR P = 1 TO CAMPS(CAM).LLARG
                IF MID$(CAMPS(CAM).CONTI, P, 1) = CHR$(CAMPS(CAM).CARENC) THEN
                   MID$(CAMPS(CAM).CONTI, P, 1) = " "
                END IF
            NEXT
         END IF
            FOR P = 1 TO CAMPS(CAM).LLARG
                IF MID$(CAMPS(CAM).CONTI, P, 1) = CHR$(0) THEN
                   MID$(CAMPS(CAM).CONTI, P, 1) = " "
                END IF
            NEXT

         SHOWFIELD$ = MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)

END FUNCTION

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

