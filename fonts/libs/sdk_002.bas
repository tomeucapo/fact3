'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
'³ Fitxer: SDK_002.BAS                                         ³
'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
'³ Llibreria per gestionar camps. Versi¢ 3.0c                  ³
'³                                                             ³
'³ 10/08/1996 - 02/07/1997                                     ³
'³ Summer'96  - Summer'97                                      ³
'³                                                             ³
'³ Revision C 01/01/1998                                       ³
'³                                                             ³
'³ Smart Software 1993/98 (C)                                  ³
'³ Tomeu Cap¢ Cap¢ 1998 (C)                                    ³
'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

'$INCLUDE: 'D:\FACT3\FONTS\SDK_001.BI'

Copyright$ = " Material propietat de Smart Software 1993-1999 (C) -*- Revision C "

' *********************************************
' * BORRA EL CONTINGUT DELS CAMPS DEFINITS    *
' *********************************************
'
SUB DeleteCamp (CAM)
   FOR P = 1 TO CAMPS(CAM).LLARG
       MID$(CAMPS(CAM).CONTI, P, 1) = CHR$(0)
   NEXT
   CAMPS(CAM).CONTI = ""
END SUB

' *********************************************
' * VISUALITZA TOTS ELS CAMPS DEFINITS        *
' *********************************************
'
SUB DisplayAllCamps
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

' *********************************************
' * INSERTA UN VALOR A UN CAMP                *
' *********************************************
'
SUB InsertValueCamp (CAM, QUE$)
    FOR P = 1 TO CAMPS(CAM).LLARG                 ' BUIDAR EL CAMP
        MID$(CAMPS(CAM).CONTI, P, 1) = CHR$(32)
    NEXT
    CAMPS(CAM).CONTI = QUE$
END SUB

' *****************************************************
' * Funci¢ per l'entrada de dades per teclat (Input)  *
' *****************************************************
' * Revisi¢ A 2.0 * Revisi¢ B 2.9  * Revisi¢ C 3.0    *
' * 08/10/1996    * 02/07/1997     * 01/01/1998       *
' *****************************************************
'
'
FUNCTION ReadCamp (CAM)

    GOSUB LLEGIRCOLORS
    QUE = CAMPS(CAM).TIPUS
    IF LEN(LTRIM$(RTRIM$(CAMPS(CAM).CONTI))) = 0 THEN
       CAMPS(CAM).CONTI = STRING$(CAMPS(CAM).LLARG, CHR$(32))
    END IF

    GOSUB VISUMASK
    COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, " ");
    LOCATE CAMPS(CAM).X, CAMPS(CAM).Y: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
    AVIS = 999
    
    LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1, 13, 14
    X = CAMPS(CAM).X: Y = CAMPS(CAM).Y: L = 1
    GOSUB COMPCAD
    DO
       DO
          T$ = INKEY$
          QUE = CAMPS(CAM).TIPUS
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
              CASE CHR$(7), CHR$(12)
                   SOUND 50, .5
              CASE CHR$(0) + "S"
                   F = L
                   LOCATE X, Y, 0
                   COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 0: PRINT STRING$(CAMPS(CAM).LLARG, " ");
                   BUFFER$ = CAMPS(CAM).CONTI
                   BUFFER$ = LEFT$(BUFFER$, L - 1) + RIGHT$(BUFFER$, LEN(BUFFER$) - L)
                   CAMPS(CAM).CONTI = BUFFER$
                   COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 0: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                   L = F
                   LOCATE X, Y, 1
              CASE CHR$(0) + "R"
                   
              CASE CHR$(0) + "O", CHR$(0) + "G"
                   SOUND 50, .5
              CASE CHR$(0) + "K"
                   IF L <= 1 THEN
                      L = 1: Y = CAMPS(CAM).Y: LOCATE X, Y
                   ELSE
                      Y = Y - 1: L = L - 1: LOCATE X, Y
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
                      COLOR CT1, CT2: GOSUB IMPRI.CAD
                      CAM = CAM - 1
                      GOSUB LLEGIRCOLORS
                      X = CAMPS(CAM).X: Y = CAMPS(CAM).Y
                      COLOR CC1, CC2: GOSUB IMPRI.CAD
                      LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: L = 1
                   END IF
              CASE CHR$(0) + "P"
                   IF CAM >= MAXCAMPS THEN
                      CAM = MAXCAMPS
                   ELSE
                      COLOR CT1, CT2: GOSUB IMPRI.CAD
                      CAM = CAM + 1: GOSUB LLEGIRCOLORS
                      X = CAMPS(CAM).X: Y = CAMPS(CAM).Y
                      COLOR CC1, CC2: GOSUB IMPRI.CAD
                      LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1
                      L = 1
                   END IF
              CASE CHR$(13)
                    GOSUB LLEGIRCOLORS
                    SELECT CASE QUE
                           CASE 2
                                COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                                GOSUB VISUMASK
                           CASE 4
                                COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, "*");
                           CASE ELSE
                                COLOR CT1, CT2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
                                GOSUB VISUMASK
                    END SELECT
                    ReadCamp = CAM
                    EXIT FUNCTION
              CASE CHR$(8)
                   IF L <= 1 THEN
                      L = 1
                   ELSE
                      GOSUB comp
                      GOSUB COMPDEL
                   END IF
              CASE CHR$(27)
                   COLOR CT1, CT2: GOSUB IMPRI.CAD
                   ReadCamp = 999
                   EXIT FUNCTION
              CASE CHR$(0) + CHR$(F1) TO CHR$(0) + CHR$(F10)
                   ReadCamp = ASC(MID$(T$, 2, 1))
                   EXIT FUNCTION
              CASE ELSE
                   GOSUB VISUMASK
                   SELECT CASE QUE
                          ' ******* Camps normals *******
                          '        X = AlfanumŠric
                          '        9 = NumŠric
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
    IF CAMPS(CAM).TIPUS = 4 THEN RETURN
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
    NM$ = LEFT$((CAMPS(CAM).CONTI), L): J = 1
    MID$(CAMPS(CAM).CONTI, 1, L) = STRING$(L, 32)
    GOSUB VISUMASK
    COLOR CC1, CC2: LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
    MID$(CAMPS(CAM).CONTI, LP - L + 1, L) = NM$

    LE = L
    L = (L + LP) - LE + 1
    Y = (Y + LP) - LE + 1
    LOCATE X, Y, 0: GOSUB VISUMASK: LOCATE X, Y, 1
    RETURN

IMPRI.CAD:
    LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT STRING$(CAMPS(CAM).LLARG, " ");
    LOCATE CAMPS(CAM).X, CAMPS(CAM).Y, 1: PRINT MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
    RETURN

END FUNCTION

SUB SetColorCamp (CAM, CC1, CC2, CT1, CT2, CTX1, CTX2)
    CAMPS(CAM).COLORS.CTITOL1 = CT1
    CAMPS(CAM).COLORS.CTITOL2 = CT2
    CAMPS(CAM).COLORS.CCAMP1 = CC1
    CAMPS(CAM).COLORS.CCAMP2 = CC2
    CAMPS(CAM).COLORS.CTEXT1 = CTX1
    CAMPS(CAM).COLORS.CTEXT2 = CTX2
END SUB

' *********************************************
' * INICIALTZA UN CAMP                        *
' *********************************************
'
SUB SetInitCamp (CAM, X, Y, TIP, CARENC, MASK$, TITOL$)
    CAMPS(CAM).X = X
    CAMPS(CAM).Y = Y
    CAMPS(CAM).LLARG = LEN(MASK$)
    CAMPS(CAM).TIPUS = TIP
    CAMPS(CAM).CARENC = CARENC
    CAMPS(CAM).MASK = MASK$
    CAMPS(CAM).TITOL = TITOL$
END SUB

SUB SetMaxCamps (MAXI)
    MAXCAMPS = MAXI
END SUB

FUNCTION ValueCamp$ (CAM)
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
         ValueCamp$ = MID$(CAMPS(CAM).CONTI, 1, CAMPS(CAM).LLARG)
END FUNCTION

