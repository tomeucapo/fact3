'
' Pant 2.0
'
' Generador de pantalles amb text
'
' COMPATIBLES AMB: C,PASCAL,CLIPPER,BASIC I D'ALTRES,ASCII
'
' Smart Software 1993/95 (C)
' Tomeu Cap¢ Cap¢ 1995 (C)    ---*---   Summer '95
'


DECLARE SUB dBOX (ROW1%, COL1%, ROW2%, COL2%, A$, B$, C$, D$, e$, F$)
DECLARE FUNCTION FASTREADCAR! (X%, Y%)
DECLARE FUNCTION READFASTCOL! (X%, Y%)
DECLARE SUB FASTCOL (X!, Y!, COL!)
DECLARE SUB FASTIMP (X!, Y!, C!)
DECLARE SUB BOTO (X%, Y%, COMM$, C!, CT!, CR!, CTR!)
DECLARE SUB BOXFILL (ROW1%, COL1%, ROW2%, COL2%, C$)
DECLARE SUB BOXFULL (ROW1%, COL1%, ROW2%, COL2%, SOMBRA%)
DECLARE SUB MENU (CASO%, FILA%, COLUM%, ANCHO%, ITEMS%, SOM%)
DECLARE FUNCTION ROTACION! (BAJO%, ALTO%, CORRIENTE%, CASO%)
DECLARE SUB DBOT (ROW1%, COL1%, ROW2%, COL2%, PITJ%)

REM $INCLUDE: 'C:\FACT2\FONTS\CAMPS.BI'
REM $INCLUDE: 'C:\fact2\fonts\dMOUSE.BI'
REM $DYNAMIC

TYPE BOTONS
     OPC AS INTEGER
END TYPE

TYPE MASC
     COL AS INTEGER
     COLF AS INTEGER
     X AS INTEGER
     Y AS INTEGER
     STIL AS INTEGER
END TYPE


CONST CF = 15, CT = 2
DIM SHARED M AS BOTONS
DIM SHARED PANTA AS MASC                     ' VARIABLES DE LA PANTALLA
DIM SHARED COLOR$(0 TO 31)                          ' MATRIU DE COLORS
DIM SHARED STIL$(0 TO 8)                            ' MATRIU DE CAIXES
DIM SHARED PANTMENU%(3, 4100)                       ' MATRIU DE MENUS
DIM SHARED Pant%(4, 4100)

DIM SHARED TEXTOSMENUP(1 TO 6) AS STRING * 20
DIM SHARED LENGUA$(4)
DIM SHARED NOM$(4)

CODECLIPP:
DATA &H50,&H41,&H4E,&H54,&H00,&H53,&H54,&H00
DATA &H02,&H00,&H02,&HC3,&HC2,&H5C,&HA2,&H5A
DATA &HA1,&H0F,&H73,&H46,&H02,&H00,&HC2,&H5C
DATA &HB2,&H5A,&HF0,&H23,&H24,&H16,&H12,&H0C
DATA &H20

MouseOff
GOSUB TIPOPANT
GOSUB DEFINEIXCOLORS
GOSUB DEFINEIXSTILS
PRIMERA% = 0
LENG = 0
LENGUA$(LENG) = "[SENSE_NOM]"
LENG = LENG + 1
GOSUB PREPARAPANT
GOSUB MENUPANT
MouseOn
MNU = 1: GOSUB SAVEMENU
GOSUB AJUDA

DO
  GOSUB DEFINEMENU
  GOSUB TESTLENG
  MouseOff
  CALL MENU(CASO%, 10, 29, 22, 6, 1)
  MNU = 1: GOSUB SAVEMENU
  MouseOn

  SELECT CASE CASO%
         CASE 1
              GOSUB SELECTPANT
         CASE 2
              GOSUB CARREGAR
         CASE 3
              GOSUB GUARDAR
         CASE 4

         CASE 5
              MouseOff
              COLOR 7, 0: CLS
              PRINT "ESCRIU EXIT PER TORNAR AL TURBO PANTS 2.0"
              PRINT
              SHELL "COMMAND.COM"
              MNU = 1: GOSUB RESTMENU
              MouseOn
         CASE 6
              MouseOff
              COLOR 7, 0
              CLS
              SYSTEM
         CASE 888
              GOSUB AJUDA
         CASE 999
              COLOR 7, 0
              CLS
              MouseOff
              SYSTEM
         CASE ELSE
  END SELECT
LOOP

END

GUARDAR:
DO
  MouseOff
  GOSUB DEFINEMENU2
  CALL MENU(CASO%, 10, 2, 22, 5, 1)
  MNU = 2: GOSUB SAVEMENU
  MouseOn

  SELECT CASE CASO%
         CASE 1
         CASE 2
         CASE 3
              GOSUB GUARDABASIC
         CASE 4
              GOSUB GUARDACLIPPER
         CASE 5
              GOSUB GUARDATEXT
         CASE 999
              MNU = 1: GOSUB RESTMENU
              RETURN
         CASE ELSE
  END SELECT
LOOP
RETURN

CARREGAR:
DO
  MouseOff
  GOSUB DEFINEMENU3
  CALL MENU(CASO%, 10, 2, 22, 5, 1)
  MNU = 2: GOSUB SAVEMENU
  MouseOn

  SELECT CASE CASO%
         CASE 1
         CASE 2
         CASE 3
              GOSUB COMPROVA
              GOSUB CARGABASIC
         CASE 4
              GOSUB COMPROVA
              GOSUB CARGACLIPPER
         CASE 5
              GOSUB COMPROVA
              GOSUB CARGATEXT
         CASE 999
              MNU = 1: GOSUB RESTMENU
              RETURN
         CASE ELSE
  END SELECT
LOOP
RETURN

SELECTPANT:
 IF CARREGAT% = 1 THEN
 S = 1
  FOR i = 0 TO 4
      TEXTOSMENUP(S) = LENGUA$(i) + " " + NOM$(i)
      S = S + 1
  NEXT
 ELSE
   TEXTOSMENUP(1) = "[SENSE_NOM]": FOR i = 2 TO 4: TEXTOSMENUP(i) = "BUIDA": NEXT
   CARREGAT% = 1
 END IF

  CALL MENU(CASO%, 18, 29, 22, 4, 1)

  IF CASO% = 999 THEN
     MNU = 1: GOSUB RESTMENU
     RETURN
  ELSE
     NUMPANT% = CASO% - 1
     GOSUB EDITAPANTA
  END IF
RETURN

'***************************************************************************
'****************************** EDITA LA PANTALLA **************************
'***************************************************************************

EDITAPANTA:
MouseOff
IF PRIMERA% = 1 OR CARREGAT% = 1 THEN
      PA% = NUMPANT%: GOSUB RESTPANT
      PRIMERA% = 1
ELSE
   COLOR 7, 0
   CLS
   PRIMERA% = 1
END IF

CU% = 0
PANTA.COL = 7
PANTA.COLF = 0
MouseOn

DO
  LOCATE , , 1, 0, 13
  DO
    A$ = INKEY$
    IF MouseDown(1) THEN LOCATE MouseY / 8 + 1, MouseX / 8 + 1
   
    LOCATE , , 1, 0, 12
  LOOP UNTIL A$ <> ""
  SELECT CASE A$
         CASE CHR$(27)                          ' SURT AL MENU I GUARDA LA PANTALLA
              MouseOff
              PA% = NUMPANT%: GOSUB SAVEPANT
              MNU = 1: GOSUB RESTMENU: MouseOn
              RETURN
         CASE CHR$(8)                           ' <-- BORRARDA
              IF POS(0) < 2 THEN LOCATE CSRLIN, 2
              LOCATE CSRLIN, POS(0) - 1: PRINT " "; : LOCATE CSRLIN, POS(0) - 1
         CASE CHR$(0) + "R"
              
         CASE CHR$(0) + "S"
        
         CASE CHR$(25)                          ' CTRL + Y
              LOCATE CSRLIN, 1: PRINT STRING$(80, " ");
              LOCATE CSRLIN - 1, 1
         CASE CHR$(0) + "I"                     ' RE PAG
              LOCATE 1, POS(0)
         CASE CHR$(0) + "Q"                     ' AV PAG
              LOCATE 25, POS(0)
         CASE CHR$(0) + "G"                     ' INICI
              LOCATE CSRLIN, 1
         CASE CHR$(0) + "O"                     ' FINAL
              LOCATE CSRLIN, 80
         CASE CHR$(0) + "H"                     ' <-- AMUNT
              LINEA% = CSRLIN
              LINEA% = LINEA% - 1
              IF LINEA% < 1 THEN LOCATE 4, POS(0): LINEA% = 25
              LOCATE LINEA%, POS(0)
         CASE CHR$(0) + "P"                     ' <-- AVALL
              LINEA% = CSRLIN
              LINEA% = LINEA% + 1
              IF LINEA% > 25 THEN LINEA% = 1: LOCATE LINEA%, POS(0): LINEA% = 1
              LOCATE LINEA%, POS(0)
         CASE CHR$(0) + "K"                     ' <-- ESQUERRA
              IF POS(0) < 2 THEN LINEA% = 80: LOCATE CSRLIN, 80
              LOCATE CSRLIN, POS(0) - 1
         CASE CHR$(0) + "M"                     ' <-- DRETA
              IF POS(0) = 80 THEN LINEA% = 1: LOCATE CSRLIN, 1
              LOCATE CSRLIN, POS(0) + 1
         CASE CHR$(0) + "<"                     ' <--- CLEAR  - F2
              COLOR PANTA.COL, PANTA.COLF
              CLS
         CASE CHR$(0) + "="                     ' CREA UNA CAIXA X,Y
              IF CU% = 1 THEN
                 BEEP
              ELSE
                 LIN% = CSRLIN: CL% = POS(0)
                 CU% = 1
                 SOUND 50, .5
                 LOCATE LIN%, CL%: PRINT "ù";
              END IF
         CASE CHR$(0) + ">"                    ' POSSA LA CAIXA X,Y,X2,Y2
              GOSUB DRAWBOX
         CASE CHR$(0) + "?"                    ' MOSTRA LES COORDENADES
              GOSUB COORDENA
         CASE CHR$(0) + "@"                    ' SELECCIONA EL COLOR DE LA TINTA
              GOSUB COLORTINTA
         CASE CHR$(0) + "A"                    ' SELECCIONA EL COLOR DEL FONS
              GOSUB COLORFONS
         CASE CHR$(0) + "B"                    ' SELECCIONA EL TIPUS DE CAIXA
              GOSUB BOXSTIL
         CASE CHR$(0) + "C"
              GOSUB TABLASCII
         CASE CHR$(0) + "D"
              GOSUB COLORPUNT
         CASE CHR$(0) + ";"
              GOSUB FASTAJUDA
         CASE ELSE                                      ' <--- ESCRIU
              COLOR PANTA.COL, PANTA.COLF
              PRINT A$;
              Y = Y + 1
         END SELECT

LOOP
RETURN

'***************************************************************************
'**************** MOSTRA LES COORDENADES DEL CURSOR ************************
'***************************************************************************


COORDENA:
              MouseOff
              PA% = NUMPANT%: GOSUB SAVEPANT
              X% = CSRLIN: Y% = POS(0)
              COLOR 4, 11
              FINESTRA 10, 10, 21, 70, 1, "ÉÍ»ºÈÍ¼"
              
              LOCATE 11, 11: PRINT "Estas a sa posici¢ (" + LTRIM$(STR$(X%)) + "," + LTRIM$(STR$(Y%)) + ")"
              COLOR 30
              LOCATE 12, 11: PRINT "                      Polsa una tecla"
              MouseOn
              DO UNTIL INKEY$ <> ""
                 IF MouseDown(1) THEN EXIT DO
              LOOP
              MouseOff
              COLOR PANTA.COL, COLF
              LOCATE X%, Y%
              PA% = NUMPANT%: GOSUB RESTPANT
              MouseOn
RETURN

'***************************************************************************
'************************ SELECCIONA EL COLOR DE LA TINTA ******************
'***************************************************************************

COLORTINTA:
              PA% = NUMPANT%: GOSUB SAVEPANT
              X% = CSRLIN: Y% = POS(0): COLVELL = PANTA.COL
              COLOR 7, 0
              FINESTRA 2, 20, 21, 70, 1, "ÉÍ»ºÈÍ¼"
              P = 3
              CO = 21
              FOR C = 0 TO 31
                  IF C = 16 THEN CO = 43: P = 3
                  COLOR C, PANTA.COLF: LOCATE P, CO: PRINT LTRIM$(STR$(C)); " "; COLOR$(C)
                  P = P + 1
              NEXT
              COLOR 7, 0
              VALE = 0
            DO
              LOCATE 20, 22: PRINT "Color:         <- CANVIA EL COLOR DE LA TINTA"
              X = 20: Y = 29: FINCA = 3: GOSUB TECLADO: PANTA.COL = VAL(CAD$)
              IF PANTA.COL > 31 THEN
                 BEEP
                 VALE = 45
              ELSE
                 VALE = 33
              END IF

              IF ESC% = 1 THEN
                 PA% = NUMPANT%: GOSUB RESTPANT
                 PANTA.COL = COLVELL
                 COLOR PANTA.COL, PANTA.COLF
                 LOCATE X%, Y%
                 RETURN
              END IF
            LOOP UNTIL VALE = 33
           
            MouseOff
            PA% = NUMPANT%: GOSUB RESTPANT
            COLOR PANTA.COL, PANTA.COLF
            LOCATE X%, Y%
            MouseOn
RETURN

'***************************************************************************
'*************************** SELECCIONA EL COLOR DEL FONS ******************
'***************************************************************************

COLORFONS:
              PA% = NUMPANT%: GOSUB SAVEPANT
              X% = CSRLIN: Y% = POS(0)
              COLOR 7, 0
              FINESTRA 2, 20, 21, 70, 1, "ÉÍ»ºÈÍ¼"
              P = 3
              CO = 21
              FOR C = 0 TO 15
                  IF C = 16 THEN CO = 43: P = 3
                  COLOR C, 0: LOCATE P, CO: PRINT LTRIM$(STR$(C)); " "; COLOR$(C)
                  P = P + 1
              NEXT
              COLOR 7, 0
              LOCATE 20, 22: PRINT "Color:         <- CANVIA EL COLOR DEL FONS"
              X = 20: Y = 29: FINCA = 3: GOSUB TECLADO: PANTA.COLF = VAL(CAD$)
              IF ESC% = 1 THEN
                 PA% = NUMPANT%: GOSUB RESTPANT
                 COLOR PANTA.COL, PANTA.COLF
                 LOCATE X%, Y%
                 RETURN
              END IF
              MouseOff
              PA% = NUMPANT%: GOSUB RESTPANT
              COLOR PANTA.COL, PANTA.COLF
              LOCATE X%, Y%
              MouseOn
RETURN

'***************************************************************************
'******************* SELECCIONA L'ESTIL DE LES CAIXES **********************
'***************************************************************************

BOXSTIL:
              MouseOff
              PA% = NUMPANT%: GOSUB SAVEPANT
              X% = CSRLIN: Y% = POS(0)
              COLOR 7, 0
              FINESTRA 2, 20, 21, 70, 1, "ÉÍ»ºÈÍ¼"
              CALL BOXFULL(2, 20, 21, 70, 1)
              P = 3
              CO = 29
              FOR C = 0 TO 7
                  COLOR 7, 0
                  IF C = 16 THEN CO = 43: P = 3
                  LOCATE P, CO: PRINT LTRIM$(STR$(C)); "-"; STIL$(C)
                  P = P + 2
              NEXT
              COLOR 7, 0
              LOCATE 20, 22: PRINT "ESTIL:        <- CANVIA L'ESTIL DE LES CAIXES"
              MouseOn
              X = 20: Y = 29: FINCA = 3: GOSUB TECLADO: PANTA.STIL = VAL(CAD$)
              IF ESC% = 1 THEN
                 PA% = NUMPANT%: GOSUB RESTPANT
                 COLOR PANTA.COL, PANTA.COLF
                 LOCATE X%, Y%
                 RETURN
              END IF
              MouseOff
              PA% = NUMPANT%: GOSUB RESTPANT
              COLOR PANTA.COL, PANTA.COLF
              LOCATE X%, Y%
              MouseOn
RETURN

'***************************************************************************
'*************************** DIBUIXA LES CAIXES ****************************
'***************************************************************************

DRAWBOX:
              RELL% = 0
              IF LIN% = CSRLIN AND CL% = POS(0) OR CU% = 0 THEN
                 BEEP
              ELSE
                 SELECT CASE PANTA.STIL
                        CASE 0
                             E1$ = "Ú"
                             E2$ = "¿"
                             L1$ = "Ä"
                             L2$ = "³"
                             E3$ = "À"
                             E4$ = "Ù"
                        CASE 1
                             E1$ = "É"
                             E2$ = "»"
                             L1$ = "Í"
                             L2$ = "º"
                             E3$ = "È"
                             E4$ = "¼"
                        CASE 2
                             E1$ = "Õ"
                             E2$ = "¸"
                             L1$ = "Í"
                             L2$ = "³"
                             E3$ = "Ô"
                             E4$ = "¾"
                        CASE 3
                             E1$ = "Ö"
                             E2$ = "·"
                             L1$ = "Ä"
                             L2$ = "º"
                             E3$ = "Ó"
                             E4$ = "½"
                        CASE 4
                             E1$ = "±"
                             E2$ = "±"
                             L1$ = "±"
                             L2$ = "±"
                             E3$ = "±"
                             E4$ = "±"
                        CASE 5
                             R$ = "±"
                             RELL% = 1
                        CASE 6
                             E1$ = "°"
                             E2$ = "°"
                             L1$ = "°"
                             L2$ = "°"
                             E3$ = "°"
                             E4$ = "°"
                        CASE 7
                             R$ = "°"
                             RELL% = 1
                        CASE ELSE
                 END SELECT
                 IF RELL% = 1 THEN
                    CALL BOXFILL(LIN%, CL%, CSRLIN, POS(0), R$)
                    CU% = 0: MOCU% = 0
                 ELSE
                       CALL dBOX(LIN%, CL%, CSRLIN, POS(0), E1$, E2$, L1$, L2$, E3$, E4$)
                       CU% = 0
                 END IF
              END IF
RETURN

                            
TABLASCII:
              MouseOff
              PA% = NUMPANT%: GOSUB SAVEPANT
              XC% = CSRLIN: YC% = POS(0)
              COLOR 7, 0
              CALL BOXFULL(1, 1, 24, 79, 0)
              LOCATE 1, 30: PRINT "[ TAULA ASCII ]"
              LOCATE 24, 3: PRINT "[                               ]";
              COLOR 28, 0: LOCATE 24, 5: PRINT "Pitja una tecla per continuar";
              COLOR 7, 0
              MouseOn
              L = 1
              CO = 1
             
              FOR C = 0 TO 255
                  IF L > 22 THEN
                     L = 2
                     CO = CO + 6
                  END IF

                  CALL FASTIMP(CO, L, C)
                  CALL FASTCOL(CO, L, 15)
                  L = L + 1
              NEXT

              L = 2
              CO = 4

              FOR C = 0 TO 255

                  IF L > 23 THEN
                       IF C > 21 THEN L = 3 ELSE L = 2
                       CO = CO + 6
                  END IF
              
                
                  COLOR 7, 0: LOCATE L, CO: PRINT LTRIM$(RTRIM$(STR$(C)));
                  L = L + 1
             
              NEXT
              

              LOCATE , , 0, 0, 0
              DO
                A$ = INKEY$
                X% = MouseX / 8
                Y% = MouseY / 8
                CA = FASTREADCAR(X%, Y%)

                XW$ = LTRIM$(STR$(X%)) + " "
                YW$ = LTRIM$(STR$(Y%)) + " "

                COR$ = XW$ + "³ " + YW$
                COLOR 5

                LOCATE 25, 1: PRINT COR$;
                COLOR 3
                LOCATE 25, 30: PRINT "Car…cter: "; CA; "  ";
              LOOP UNTIL A$ <> "" OR MouseDown(1)
              MouseOff
              PA% = NUMPANT%: GOSUB RESTPANT
              COLOR PANTA.COL, PANTA.COLF
              LOCATE XC%, YC%
              MouseOn
RETURN

COLORPUNT:
              MouseOff
              PA% = NUMPANT%: GOSUB SAVEPANT
              X% = CSRLIN: Y% = POS(0)
              COLOR 4, 11
              CALL BOXFULL(10, 10, 15, 50, 0)
              LOCATE 11, 11: PRINT "Estas a sa posici¢ (" + LTRIM$(STR$(X%)) + "," + LTRIM$(STR$(Y%)) + ")"
              LOCATE 12, 11: PRINT "Color del car…cter : "; SCREEN(X%, Y%, 2)
              COLOR 30
              LOCATE 13, 11: PRINT "                      Polsa una tecla"
              MouseOn
              DO UNTIL INKEY$ <> ""
                 IF MouseDown(1) THEN EXIT DO
              LOOP
              MouseOff
              COLOR PANTA.COL, COLF
              LOCATE X%, Y%
              PA% = NUMPANT%: GOSUB RESTPANT
              MouseOn
RETURN

'***************************************************************************
'*********************** ALTRES RUTINES ************************************
'***************************************************************************

SAVEPANT:                     ' ENMAGATZEMA LA PANTALLA DINS MEM•RIA
        DEF SEG = TESTSCREEN
        FOR P = 0 TO 4100
            Pant%(PA%, P) = PEEK(P)
        NEXT
        DEF SEG
RETURN

RESTPANT:                    ' RESTAURA LA PANTALLA ENMAGATZEMADA
        DEF SEG = TESTSCREEN
        FOR P = 0 TO 4100
            POKE P, Pant%(PA%, P)
        NEXT
        DEF SEG
RETURN

           
SAVEMENU:                         ' ENMAGATZEMA UN MENU
        DEF SEG = TESTSCREEN
        FOR P = 0 TO 4100
            PANTMENU%(MNU, P) = PEEK(P)
        NEXT
        DEF SEG
RETURN

RESTMENU:                       ' RESTAURA UN MENU
        DEF SEG = TESTSCREEN
        FOR P = 0 TO 4100
            POKE P, PANTMENU%(MNU, P)
        NEXT
        DEF SEG
RETURN


'***************************************************************************
'********************** GRAVACI¢ DE DADES DE LES PANTALLES *****************
'***************************************************************************

GUARDABASIC:                         ' BASIC
              IF PRIMERA% = 0 THEN
                 COLOR 4, 11: BEEP
                 CALL BOXFULL(10, 10, 13, 50, 0)
                 COLOR 28, 11
                 LOCATE 11, 11: PRINT "ERROR: NO S'HA GENERAT CAP PANTALLA"
                 COLOR 30, 11
                 LOCATE 12, 11: PRINT "                      Polsa una tecla"
                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
              ELSE
                 COLOR 15, 9
                 CALL BOXFULL(10, 10, 16, 50, 1)
                
                 LOCATE 13, 11: PRINT "Nom de la pantalla:"
                 Y = 32: X = 13: FINCA = 9: GOSUB TECLADO
                 IF ESC% = 1 THEN
                    MNU = 2: GOSUB RESTMENU
                    RETURN
                 ELSE
                     NOM$ = CAD$
                 END IF
                 IF SCRNSEG.SS% = &HB000 THEN
                    COD$ = CHR$(&HFD) + CHR$(&H0) + CHR$(&HB0) + CHR$(&H0) + CHR$(&H0) + CHR$(&H4) + CHR$(&H10)
                 ELSE
                    IF SCRNSEG.SS% = &HB800 THEN
                       COD$ = CHR$(&HFD) + CHR$(&H0) + CHR$(&HB8) + CHR$(&H0) + CHR$(&H0) + CHR$(&H4) + CHR$(&H10)
                    END IF
                 END IF
                 COLOR 15, 9
                 LOCATE 11, 11: PRINT "GENERANT CODIS DE CAR…CTERS"
                 LOCATE 15, 11: PRINT "                           FET:"
                 LOCATE 15, 43

                 NOM$ = NOM$ + ".BSV"
                 TANT% = 0
                 OPEN NOM$ FOR RANDOM AS 1 LEN = 7
                 FIELD 1, 7 AS L$
                 LSET L$ = COD$: PUT 1, 1
                 CLOSE 1
               
                 OPEN NOM$ FOR RANDOM AS 1 LEN = 1
                 FIELD 1, 1 AS C$
                 R = 8
                 FOR P = 0 TO 4100
                     TANT% = P / 4100 * 100
                     LOCATE 15, 43: PRINT TANT%; "%"
                     LSET C$ = CHR$(Pant%(0, P))
                     PUT 1, R
                     R = R + 1
                 NEXT
                 CLOSE 1
                 PLAY "L20GBGBGBG"
                 COLOR 27
                 LOCATE 15, 11: PRINT "PITJA UNA TECLA"
                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
              END IF
RETURN

GUARDATEXT:                          ' AMB ASCII .TXT
              IF PRIMERA% = 0 THEN
                 COLOR 4, 11: BEEP
                 CALL BOXFULL(10, 10, 13, 50, 1)
                 COLOR 28, 11
                 LOCATE 11, 11: PRINT "ERROR: NO S'HA GENERAT CAP PANTALLA"
                 COLOR 30, 11
                 LOCATE 12, 11: PRINT "                      Polsa una tecla"
                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
              ELSE
                 COLOR 15, 9
                 CALL BOXFULL(10, 10, 16, 50, 1)
               
                 LOCATE 13, 11: PRINT "Nom de la pantalla:"
                 Y = 32: X = 13: FINCA = 9: GOSUB TECLADO
                 IF ESC% = 1 THEN
                    MNU = 2: GOSUB RESTMENU
                    RETURN
                 ELSE
                     NOM$ = CAD$
                 END IF
               
                 COLOR 15, 9
                 LOCATE 11, 11: PRINT "CONVERTINT CODIS DE CAR…CTERS AMB TEXT"
                 LOCATE 15, 11: PRINT "                           FET:"
                 LOCATE 15, 43

                 NOM$ = NOM$ + ".TXT"
                 TANT% = 0
               
                 OPEN NOM$ FOR RANDOM AS 1 LEN = 1
                 FIELD 1, 1 AS C$
                 R = 1
                 FOR P = 0 TO 4100 STEP 2
                     TANT% = P / 4100 * 100
                     LOCATE 15, 43: PRINT TANT%; "%"
                     LSET C$ = CHR$(Pant%(0, P))
                     PUT 1, R
                     R = R + 1
                 NEXT
                 CLOSE 1
                 PLAY "L20GBGBGBG"
                 COLOR 27
                 LOCATE 15, 11: PRINT "PITJA UNA TECLA"
                 LENGUA$(LENG) = "ASCII ": NOM$(LENG) = NOM$
                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
              END IF
RETURN

GUARDACLIPPER:                       ' AMB CLIPPER .MEM
              
              IF PRIMERA% = 0 THEN
                 COLOR 4, 11: BEEP
                 CALL BOXFULL(10, 10, 13, 50, 1)
                 COLOR 28
                 LOCATE 11, 11: PRINT "ERROR: NO S'HA GENERAT CAP PANTALLA"
                 COLOR 30
                 LOCATE 12, 11: PRINT "                      Polsa una tecla"
                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
              ELSE
                 COLOR 15, 9
                 CALL BOXFULL(10, 10, 16, 50, 1)
                SALE = 0
                DO
                 COLOR 15, 9
                 LOCATE 13, 11: PRINT "Nom de la pantalla:"
                 Y = 32: X = 13: FINCA = 9: GOSUB TECLADO
                 IF ESC% = 1 THEN
                    MNU = 2: GOSUB RESTMENU
                    RETURN
                 ELSE
                     NOM$ = CAD$
                     IF NOM$ = "" OR LEN(NOM$) = 0 THEN
                        BEEP
                        SALE = 43
                     ELSE
                        NOM$ = CAD$
                        SALE = 99
                     END IF
                 END IF
                LOOP UNTIL SALE = 99

                 COLOR 15, 9
                 LOCATE 11, 11: PRINT "GUARDANT CODIS DE CAR…CTERS"
                 COLOR 27
                 LOCATE 15, 11: PRINT "GENERANT CODI DE CLIPPER"

                 TANT% = 0
                 NOM$ = NOM$ + ".MEM"


                 OPEN NOM$ FOR RANDOM AS 2 LEN = 1
                 FIELD 2, 1 AS C2$

                 RESTORE CODECLIPP

                 FOR R = 1 TO 33
                     READ A                          ' <--- LLEGEIX
                     LSET C2$ = CHR$(A)
                     PUT 2, R                        ' <--- ESCRIU
                 NEXT

                 IF SCRNSEG.SS% = &HB800 THEN
                    LSET C2$ = "%"
                    PUT 2, 29
                 ELSE
                    IF SCRNSEG.SS% = &HB000 THEN
                       LSET C2$ = "$"
                       PUT 2, 29
                    END IF
                 END IF
                 CLOSE 2
               
                 LOCATE 15, 11: PRINT "                          FET:"
                 OPEN NOM$ FOR RANDOM AS 1 LEN = 1
                 FIELD 1, 1 AS C$
                 R = 33
                 FOR P = 0 TO 4100
                     TANT% = P / 4100 * 100
                     LOCATE 15, 43: PRINT TANT%; "%"
                     LSET C$ = CHR$(Pant%(0, P))
                     PUT 1, R
                     R = R + 1
                 NEXT
                 CLOSE 1
                 PLAY "L20GBGBGBG"

                 COLOR 27
                 LOCATE 15, 11: PRINT "PITJA UNA TECLA"
                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
              END IF
RETURN

GUARDACOM:                                 ' AMB ASSEMBLER .COM

              RETURN

              IF PRIMERA% = 0 THEN
                 COLOR 4, 11: BEEP
                 CALL BOXFULL(10, 10, 13, 50, 1)
                 COLOR 28
                 LOCATE 11, 11: PRINT "ERROR: NO S'HA GENERAT CAP PANTALLA"
                 COLOR 30
                 LOCATE 12, 11: PRINT "                      Polsa una tecla"
                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
              ELSE
                 COLOR 15, 9
                 CALL BOXFULL(10, 10, 16, 50, 1)
                SALE = 0
                DO
                 COLOR 15, 9
                 LOCATE 13, 11: PRINT "Nom de la pantalla:"
                 Y = 32: X = 13: FINCA = 9: GOSUB TECLADO
                 IF ESC% = 1 THEN
                    MNU = 2: GOSUB RESTMENU
                    RETURN
                 ELSE
                     NOM$ = CAD$
                     IF NOM$ = "" OR LEN(NOM$) = 0 THEN
                        BEEP
                        SALE = 43
                     ELSE
                        NOM$ = CAD$
                        SALE = 99
                     END IF
                 END IF
                LOOP UNTIL SALE = 99

                 COLOR 15, 9
                 LOCATE 11, 11: PRINT "GUARDANT CODIS DE CAR…CTERS"
                 COLOR 27
                 LOCATE 15, 11: PRINT "GENERANT CODI DE CLIPPER"

                 TANT% = 0
                 NOM$ = NOM$ + ".COM"


                 OPEN NOM$ FOR RANDOM AS 2 LEN = 1
                 FIELD 2, 1 AS C2$

                 RESTORE CODECLIPP

                 FOR R = 1 TO 33
                     READ B                          ' <--- LLEGEIX
                     LSET C2$ = CHR$(A)
                     PUT 2, R                        ' <--- ESCRIU
                 NEXT

              
                 LOCATE 15, 11: PRINT "                          FET:"
                 OPEN NOM$ FOR RANDOM AS 1 LEN = 1
                 FIELD 1, 1 AS C$
                 R = 33
                 FOR P = 0 TO 4100
                     TANT% = P / 4100 * 100
                     LOCATE 15, 43: PRINT TANT%; "%"
                     LSET C$ = CHR$(Pant%(0, P))
                     PUT 1, R
                     R = R + 1
                 NEXT
                 CLOSE 1
                 PLAY "L20GBGBGBG"

                 COLOR 27
                 LOCATE 15, 11: PRINT "PITJA UNA TECLA"
                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
              END IF
RETURN

'***************************************************************************
'********************** CARREGA DE DADES A LA PANTALLA *********************
'***************************************************************************

CARGABASIC:                         ' BASIC
                 COLOR 15, 9
                 CALL BOXFULL(10, 10, 16, 50, 0)
               
                 LOCATE 13, 11: PRINT "Nom de la pantalla:           .BSV"
                 Y = 32: X = 13: FINCA = 9: GOSUB TECLADO
                 IF ESC% = 1 THEN
                    MNU = 2: GOSUB RESTMENU
                    RETURN
                 ELSE
                     NOM$ = CAD$
                 END IF
                
                 IF SCRNSEG.SS% = &HB000 THEN
                    COD$ = CHR$(&HFD) + CHR$(&H0) + CHR$(&HB0) + CHR$(&H0) + CHR$(&H0) + CHR$(&H4) + CHR$(&H10)
                 ELSE
                    IF SCRNSEG.SS% = &HB800 THEN
                       COD$ = CHR$(&HFD) + CHR$(&H0) + CHR$(&HB8) + CHR$(&H0) + CHR$(&H0) + CHR$(&H4) + CHR$(&H10)
                    END IF
                 END IF
                
                 NOM$ = NOM$ + ".BSV"
                 TANT% = 0
                 RESET

                 OPEN NOM$ FOR RANDOM AS 1 LEN = 1
                 FIELD 1, 1 AS L$
                 GET 1, 1

                 IF L$ <> CHR$(&HFD) THEN
                    MENSAJE$ = "ERROR: AQUESTA PANTALLA NO ES DE BASIC"
                    GOSUB FINESTRA
                    MNU = 2: GOSUB RESTMENU
                    RETURN
                 END IF

                 CLOSE 1

                 
                 COLOR 15, 9
                 LOCATE 11, 11: PRINT "CARREGANT CODICS DE CARACTERS"
                 LOCATE 15, 11: PRINT "                           FET:"
                 LOCATE 15, 43

                 TANT% = 0
                 
                 OPEN NOM$ FOR RANDOM AS 1 LEN = 1
                 FIELD 1, 1 AS C$

                 R = 8
                 FOR PO = 0 TO 4100
                     TANT% = PO / 4100 * 100
                     LOCATE 15, 43: PRINT TANT%; "%"
                     GET 1, R
                     Pant%(LENG, PO) = ASC(C$)
                     R = R + 1
                 NEXT
                 CLOSE 1
                 PLAY "L20GBGBGBG"
                 COLOR 27
                 LOCATE 15, 11: PRINT "PITJA UNA TECLA"

                 IF CARREGAT% = 1 THEN
                    
                    LENGUA$(LENG) = "BASIC": NOM$(LENG) = NOM$
                    LENG = LENG + 1
                 ELSE
                    LENGUA$(LENG) = "BASIC": NOM$(LENG) = NOM$
                    CARREGAT% = 1
                 END IF

                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
RETURN

CARGATEXT:                          ' AMB ASCII .TXT
                 COLOR 15, 9
                 CALL BOXFULL(10, 10, 16, 50, 0)
              
                 LOCATE 13, 11: PRINT "Nom de la pantalla:           .TXT"
                 Y = 32: X = 13: FINCA = 9: GOSUB TECLADO
                 IF ESC% = 1 THEN
                    MNU = 2: GOSUB RESTMENU
                    RETURN
                 ELSE
                     NOM$ = CAD$
                 END IF
              
                 COLOR 15, 9
                 LOCATE 11, 11: PRINT "CONVERTINT TEXT A CODIC DE CARACTERS"
                 LOCATE 15, 11: PRINT "                           FET:"
                 LOCATE 15, 43

                 NOM$ = NOM$ + ".TXT"
                 TANT% = 0
              
                 OPEN NOM$ FOR RANDOM AS 1 LEN = 1
                 FIELD 1, 1 AS C$
                 R = 1

                 FOR P = 0 TO 4100 STEP 2
                     GET 1, R
                     TANT% = P / 4100 * 100
                     LOCATE 15, 43: PRINT TANT%; "%"
                     Pant%(LENG, P) = ASC(C$)
                     R = R + 1
                 NEXT
                
                 FOR P = 1 TO 4100 STEP 2
                     Pant%(LENG, P) = 7
                 NEXT

                 CLOSE 1
                 PLAY "L20GBGBGBG"
                 COLOR 27
                 LOCATE 15, 11: PRINT "PITJA UNA TECLA"
                 IF CARREGAT% = 1 THEN
                    LENGUA$(LENG) = "ASCII": NOM$(LENG) = NOM$
                    LENG = LENG + 1
                 ELSE
                    LENGUA$(LENG) = "ASCII": NOM$(LENG) = NOM$
                    CARREGAT% = 1
                 END IF

                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
                 
RETURN

CARGACLIPPER:                       ' AMB CLIPPER .MEM
                COLOR 15, 9
                CALL BOXFULL(10, 10, 16, 50, 0)
                SALE = 0
                DO
                 COLOR 15, 9
                 LOCATE 13, 11: PRINT "Nom de la pantalla:           .MEM"
                 Y = 32: X = 13: FINCA = 9: GOSUB TECLADO
                 IF ESC% = 1 THEN
                    MNU = 2: GOSUB RESTMENU
                    RETURN
                 ELSE
                     NOM$ = CAD$
                     IF NOM$ = "" OR LEN(NOM$) = 0 THEN
                        BEEP
                        SALE = 43
                     ELSE
                        NOM$ = CAD$
                        SALE = 99
                     END IF
                 END IF
                LOOP UNTIL SALE = 99

                 COLOR 15, 9
                 LOCATE 11, 11: PRINT "CARREGANT CODIC DE CARACTERS"

                 TANT% = 0
                 NOM$ = NOM$ + ".MEM"
             

                 LOCATE 15, 11: PRINT "                          FET:"
                 OPEN NOM$ FOR RANDOM AS 1 LEN = 1
                 FIELD 1, 1 AS C$
                  R = 33
                 FOR P = 0 TO 4100
                     GET 1, R
                     TANT% = P / 4100 * 100
                     LOCATE 15, 43: PRINT TANT%; "%"
                     Pant%(LENG, P) = ASC(C$)
                     R = R + 1
                 NEXT
                 CLOSE 1
                 PLAY "L20GBGBGBG"

                 COLOR 27
                 LOCATE 15, 11: PRINT "PITJA UNA TECLA"
                 IF CARREGAT% = 1 THEN
                    LENGUA$(LENG) = "CLIPPER": NOM$(LENG) = NOM$
                    LENG = LENG + 1
                 ELSE
                    LENGUA$(LENG) = "CLIPPER": NOM$(LENG) = NOM$
                    CARREGAT% = 1
                 END IF
                 WHILE INKEY$ = "": WEND
                 MNU = 2: GOSUB RESTMENU
RETURN

FINESTRA:
             COLOR 4, 11: BEEP
             CALL BOXFULL(10, 10, 13, 50, 0)
             COLOR 28
             LOCATE 11, 11: PRINT MENSAJE$
             COLOR 30
             LOCATE 12, 11: PRINT "                      Polsa una tecla"
             WHILE INKEY$ = "": WEND
RETURN

TIPOPANT:
     REM ******************************************************************
     REM  TEST DE TIPO PANTALLA.
     REM ******************************************************************
         DEF SEG = &H40
         MONO.SS = (PEEK(&H10) AND &H30) = &H30
         IF MONO.SS THEN
            SCRNSEG.SS% = &HB000
            RE = 0
         ELSE
            SCRNSEG.SS% = &HB800
            RE = 1
         END IF
     REM ******************************************************************
RETURN

TECLADO:           ' FUNCI¢ DELS CAMPS
        ESC% = 0
        xt = Y: yt = X: CL = 0: CAD$ = ""
        LOCATE yt, xt: COLOR 0, 7, 0: PRINT STRING$(FINCA, " ")
T1:
        ccu = ccu + 1: IF ccu >= 40 THEN IF ast = 15 THEN ccu = 1: ast = 1 ELSE ast = 15: ccu = 1
        LOCATE yt, xt + CL, 1, 11, 12
        X$ = INKEY$: IF X$ = "" THEN GOTO T1
        IF X$ = CHR$(27) THEN ESC% = 1: RETURN

        IF t7 = 0 THEN IF X$ = CHR$(13) THEN ESC% = 0: LOCATE yt, xt: COLOR 15, 12, 0: PRINT STRING$(FINCA, 32); : IF ENCRIP = 1 THEN RETURN ELSE LOCATE yt, xt: COLOR 15, 12, 0: PRINT CAD$: RETURN

        IF X$ = CHR$(0) + CHR$(75) THEN
                IF CL <= 0 THEN BEEP: CL = 1: GOTO T1
                LOCATE yt, xt + CL
                CL = CL - 1
                GOTO T1
        END IF

        IF X$ = CHR$(0) + CHR$(77) THEN
                IF CL >= FINCA THEN BEEP: CAD$ = LEFT$(CAD$, 1): CL = FINCA: GOTO T1
                LOCATE yt, xt + CL
                CL = CL + 1
                GOTO T1
        END IF

        IF X$ = CHR$(8) THEN
                IF CL <= 0 THEN BEEP: CL = 1: GOTO T1
                LOCATE yt, xt + CL: PRINT " ";
                CL = CL - 1: CAD$ = LEFT$(CAD$, CL): GOTO T1
        END IF
      IF ASC(X$) = 164 THEN X$ = CHR$(165): GOTO pasa ELSE IF X$ = "¥" THEN GOTO pasa' caracter ¥
      IF ASC(X$) = 128 THEN X$ = CHR$(128): GOTO pasa ELSE IF X$ = "€" THEN GOTO pasa' caracter €

      IF ASC(X$) < 0 OR ASC(X$) > 200 THEN IF ASC(X$) <> 32 THEN SOUND 50, .5: GOTO T1
      IF ASC(X$) > 255 AND ASC(X$) < 123 THEN min = ASC(X$): X$ = CHR$(min - 32)
pasa:
      IF co5 THEN
         ma = ASC(X$)
         IF ma = 165 THEN X$ = CHR$(164) ELSE IF ma <> 32 THEN X$ = CHR$(ma + 32)
      END IF
      IF t7 THEN
        l7 = LEN(CAD$)
        IF l7 > 5 AND l7 < 9 THEN
            ma = ASC(X$)
            IF ma = 165 THEN X$ = CHR$(164) ELSE IF ma <> 32 THEN X$ = CHR$(ma + 32)
        END IF
      END IF
      IF ENCRIP = 1 THEN
         LOCATE yt, xt + CL: PRINT "*";
      ELSE
         LOCATE yt, xt + CL: PRINT X$;
      END IF
      CL = CL + 1: CAD$ = CAD$ + X$
      IF CL < FINCA THEN
          GOTO T1
      ELSE
          LOCATE yt, xt + CL: PRINT " ";
          RETURN
      END IF
RETURN

'***************************************************************************
'*********************** DEFINICI¢ DELS COLORS *****************************
'***************************************************************************


DEFINEIXCOLORS:
              COLOR$(0) = "NEGRE"
              COLOR$(1) = "BLAU FLUIX"
              COLOR$(2) = "VERD FLUIX"
              COLOR$(3) = "CYAN FLUIX"
              COLOR$(4) = "VERMELL FLUIX"
              COLOR$(5) = "LILA FLUIX"
              COLOR$(6) = "MARR¢"
              COLOR$(7) = "BLANC FLUIX"
              COLOR$(8) = "GRIS"
              COLOR$(9) = "BLAU FORT"
              COLOR$(10) = "VERD FORT"
              COLOR$(11) = "CYAN FORT"
              COLOR$(12) = "VERMELL FORT"
              COLOR$(13) = "LILA FORT"
              COLOR$(14) = "GROC"
              COLOR$(15) = "BLANC FORT"
              COLOR$(16) = "NEGRE PARPADEANT"
              COLOR$(17) = "BLAU FLUIX PARPADEANT"
              COLOR$(18) = "VERD FLUIX PARPADEANT"
              COLOR$(19) = "CYAN FLUIX PARPADEANT"
              COLOR$(20) = "VERMELL FLUIX PARPADEANT"
              COLOR$(21) = "LILA FLUIX PARPADEANT"
              COLOR$(22) = "MARR¢ PARPADEANT"
              COLOR$(23) = "BLANC FLUIX PARPADEANT"
              COLOR$(24) = "GRIS PARPADEANT"
              COLOR$(25) = "BLAU FORT PARPADEANT"
              COLOR$(26) = "VERD FORT PARPADEANT"
              COLOR$(27) = "CYAN FORT PARPADEANT"
              COLOR$(28) = "VERMELL FORT PARPADEANT"
              COLOR$(29) = "LILA FORT PARPADEANT"
              COLOR$(30) = "GROC PARPADEANT"
              COLOR$(31) = "BLANC FORT PARPADEANT"
RETURN

'***************************************************************************
'************************ DEFINICI¢ DELS ESTILS ****************************
'***************************************************************************

DEFINEIXSTILS:
              STIL$(0) = "    ÚÄ¿³ÀÄÙ  Simple"
              STIL$(1) = "    ÉÍ»ºÈÍ¼  Doble "
              STIL$(2) = "    ÕÍ¸³ÔÍ¾  Dual - 1"
              STIL$(3) = "    ÖÄ·ºÓÄ½  Dual - 2"
              STIL$(4) = "    ±±±±±±±  Simple"
              STIL$(5) = "    ±±±±±±±  Ple"
              STIL$(6) = "    °°°°°°°  Simple"
              STIL$(7) = "    °°°°°°°  Ple"
RETURN

'***************************************************************************
'******************** PANTALLA DEL MENU ************************************
'***************************************************************************

MENUPANT:
        MouseOff
        COLOR 0, 9
        CLS
         FOR L = 1 TO 25
           COLOR 8
           FOR L2 = 1 TO 80 STEP 4
               LOCATE L, L2: PRINT "ÄÄÄÅ";
               
           NEXT
         NEXT

        COLOR 15, 3: CALL BOXFULL(2, 2, 4, 30, 1)
        COLOR 15, 3: CALL BOXFULL(2, 50, 4, 78, 1): COLOR 5
        LOCATE 3, 3: PRINT "T U R B O   P A N T S  2.0": COLOR 4, 3
        LOCATE 3, 51: PRINT "  Tomeu Cap¢ Cap¢ 1995 (C)": COLOR 15, 7

        CALL BOXFULL(22, 59, 24, 78, 0)
        COLOR 15, 7
        CALL BOXFULL(22, 2, 24, 20, 0)
        COLOR 4, 7
        LOCATE 23, 60: PRINT "   <ESC> SORTIR";
        
        LOCATE 23, 3: PRINT "HORA:"
        LOCATE , , 0, 0
        MouseOn
RETURN

'***************************************************************************
'******************* DEFINICI¢ DELS MENUS **********************************
'***************************************************************************
         
DEFINEMENU:
       TEXTOSMENUP(1) = "EDITAR PANTALLA"
       TEXTOSMENUP(2) = "CARREGAR PANTALLA"
       TEXTOSMENUP(3) = "GUARDAR PANTALLA"
       TEXTOSMENUP(4) = "RESET DE PANTALLES"
       TEXTOSMENUP(5) = "DOS SHELL"
       TEXTOSMENUP(6) = "SORTIR"
RETURN

DEFINEMENU2:
       TEXTOSMENUP(1) = "GUARDAR PER C"
       TEXTOSMENUP(2) = "GUARDAR PER PASCAL"
       TEXTOSMENUP(3) = "GUARDAR PER BASIC"
       TEXTOSMENUP(4) = "GUARDAR PER CLIPPER"
       TEXTOSMENUP(5) = "GUARDAR PER TEXT"
RETURN

DEFINEMENU3:
       TEXTOSMENUP(1) = "CARREGAR DE C"
       TEXTOSMENUP(2) = "CARREGAR DE PASCAL"
       TEXTOSMENUP(3) = "CARREGAR DE BASIC"
       TEXTOSMENUP(4) = "CARREGAR DE CLIPPER"
       TEXTOSMENUP(5) = "CARREGAR DE TEXT"
RETURN

TESTLENG:
         COLOR 6, 15
         FINESTRA 10, 57, 17, 78, 1, "ÉÍ»ºÈÍ¼"
         COLOR 15, 7
         LOCATE 10, 58: PRINT "[PANTS. MEM•RIA]"
         A = 11
         COLOR 0
         FOR W = 0 TO 4
             LOCATE A, 58: PRINT LENGUA$(W); " "; NOM$(W);
             A = A + 1
         NEXT
RETURN

COMPROVA:
        IF LENG > 4 THEN
           MENSAJE$ = "ERROR: NO ES PODEN CARREGAR MES PANTALLES"
           GOSUB FINESTRA
           MNU = 2: GOSUB RESTMENU
           RETURN
        END IF
RETURN

PREPARAPANT:
           DEF SEG = TESTSCREEN

           FOR O = 1 TO 4100 STEP 2
               Pant%(0, O) = 7
           NEXT
          
           DEF SEG
RETURN


AJUDA:
      MouseOff
      COLOR 15, 7
      FINESTRA 7, 20, 20, 60, 1, "ÉÍ»ºÈÍ¼"
      COLOR 15, 7

      COLOR 14: LOCATE 8, 31: PRINT " Turbo Pants 2.0"
      COLOR 15, 7
      LOCATE 10, 21: PRINT "    Programa per generar pantalles."
      LOCATE 12, 21: PRINT "      Pot generar pantalles per:"
      LOCATE 13, 21: PRINT "      C, PASCAL, CLIPPER, BASIC,"
      LOCATE 14, 21: PRINT "      Quick Basic, ASCII, COM": COLOR 14
      LOCATE 16, 21: PRINT "       Smart Software 1995 (C)"
      MouseOn
      CALL BOTO(17, 36, " Ok", 15, 7, 15, 12)
      MouseOff
      MNU = 1: GOSUB RESTMENU
      MouseOn
RETURN



FASTAJUDA:
              MouseOff
              PA% = NUMPANT%: GOSUB SAVEPANT
              X% = CSRLIN: Y% = POS(0)
              LOCATE , , 0, 0, 0
              COLOR 15, 9
              
              CALL BOXFULL(1, 1, 24, 80, 0)
              LOCATE 1, 35: PRINT "[ AJUDA RAPIDA ]"
              COLOR 10: LOCATE 3, 2: PRINT "Tecles r…pides:": COLOR 14
              LOCATE 4, 2: PRINT "                 F1 - Aquesta pantalla."
              LOCATE 5, 2: PRINT "                 F2 - Borra la pantalla."
              COLOR 10: LOCATE 7, 2: PRINT "Tecles per constru‹r caixes:": COLOR 7
              LOCATE 8, 2: PRINT "                 F3 - Caire superior de la caixa."
              LOCATE 9, 2: PRINT "                 F4 - Caire inferior de la caixa."
              COLOR 10: LOCATE 11, 2: PRINT "Tecles de colors:": COLOR 15
              LOCATE 12, 2: PRINT "                 F6 - Canvia el color de la tinta."
              LOCATE 13, 2: PRINT "                 F7 - Canvia el color del fons."
              COLOR 10: LOCATE 15, 2: PRINT "Altres tecles:": COLOR 11
              LOCATE 12, 2: PRINT "                 F5 - Posici¢ actual del cursor."
              LOCATE 13, 2: PRINT "                 F8 - Tipus d'estils per les caixes."
              LOCATE 14, 2: PRINT "                 F9 - Taula ASCII."
              LOCATE 15, 2: PRINT "                 F10- Color actual del cursor."
              MouseOn
              CALL BOTO(21, 37, " Segeix", 15, 9, 15, 12)
              CLS
              COLOR 15, 9
              CALL BOXFULL(1, 1, 24, 80, 0)
              LOCATE 1, 35: PRINT "[ AJUDA RAPIDA ]"


              COLOR 11
              LOCATE 7, 2: PRINT "                        T U R B O    P A N T S    Ver 2.0": COLOR 10
              LOCATE 9, 2: PRINT "                              Smart Software 1995 (C)": COLOR 14
              LOCATE 12, 2: PRINT "                                     CODERS:": COLOR 15
              LOCATE 14, 2: PRINT "                  ASSEMBLER INLINE: Joan Miquel Payeras i Cresp¡"
              LOCATE 16, 2: PRINT "                       PROGRAM CODER   : Tomeu Cap¢ i Cap¢": COLOR 9
              LOCATE 18, 2: PRINT "                           SA POBLA 1995 (C) Summer '95"

              FOR i = 0 TO 20: NEXT
              CALL BOTO(21, 37, " Segeix", 15, 9, 15, 12)

              LOCATE , , 1, 0, 12
              PA% = NUMPANT%: GOSUB RESTPANT
              COLOR PANTA.COL, PANTA.COLF
              LOCATE X%, Y%
RETURN

REM $STATIC
SUB BOTO (X%, Y%, COMM$, C, CT, CR, CTR)
    M.OPC = 0
    MouseOff
    L% = LEN(COMM$) + 1
    COLOR C, CT
    CALL BOXFULL(X%, Y%, X% + 2, Y% + L% + 1, 0)
    LOCATE X% + 1, Y% + 1: PRINT COMM$
    DO
      A$ = INKEY$
      MouseOn
      X = X%
      Y = Y%: L = L%
      IF MouseRegion(2, Y - 1, X - 1, Y + L + 2, X + 3) THEN
         IF PIT% = 1 THEN
            COLOR CR, CTR
            LOCATE X + 1, Y + 1: PRINT COMM$; " ";
         ELSE
          LOCATE X + 1, Y + 1: PRINT COMM$; " ";
          CALL BOXFULL(X%, Y%, X% + 2, Y% + L% + 1, 0)
          PIT% = 1
         END IF
      ELSE
        IF PIT% = 0 THEN
           LOCATE X + 1, Y + 1: PRINT COMM$; " ";
        ELSE
         COLOR C, CT
         LOCATE X% + 1, Y% + 1: PRINT COMM$; " ";
         CALL BOXFULL(X%, Y%, X% + 2, Y% + L% + 1, 0)
         PIT% = 0
        END IF
      END IF

      IF MouseRegion(2, Y - 1, X - 1, Y + L + 2, X + 3) AND MouseDown(1) THEN
         EXIT DO
      END IF
    LOOP UNTIL A$ = CHR$(13)
    M.OPC = 1
END SUB

SUB BOXFILL (ROW1%, COL1%, ROW2%, COL2%, C$) STATIC
    BOXWIDTH = COL2% - COL1% + 1

    LOCATE ROW1%, COL1%
    PRINT C$; STRING$(BOXWIDTH - 2, C$); C$;

    FOR A% = ROW1% + 1 TO ROW2% - 1
        LOCATE A%, COL1%
        PRINT C$; STRING$(BOXWIDTH - 2, C$); C$;
    NEXT A%

    LOCATE ROW2%, COL1%
    PRINT C$; STRING$(BOXWIDTH - 2, C$); C$;

END SUB

SUB BOXFULL (ROW1%, COL1%, ROW2%, COL2%, SOMBRA%) STATIC
    BOXWIDTH = COL2% - COL1% + 1

    LOCATE ROW1%, COL1%
    PRINT "Ú"; STRING$(BOXWIDTH - 2, "Ä"); "¿";

    FOR A% = ROW1% + 1 TO ROW2% - 1
        LOCATE A%, COL1%
        PRINT "³"; STRING$(BOXWIDTH - 2, " "); "³"
    NEXT A%


    LOCATE ROW2%, COL1%
    PRINT "À"; STRING$(BOXWIDTH - 2, "Ä"); "Ù";
 IF SOMBRA% = 1 THEN
    FOR S% = ROW1% + 1 TO ROW2% + 1
     LOCATE S%, COL1% + BOXWIDTH
     COLOR 0, 0
     PRINT "  ";
    NEXT
    LOCATE ROW2% + 1, COL1% + 2: PRINT STRING$(BOXWIDTH - 2, " ");
 END IF
END SUB

SUB DBOT (ROW1%, COL1%, ROW2%, COL2%, PITJ%) STATIC
    BOXWIDTH = COL2% - COL1% + 1

    LOCATE ROW1%, COL1%
    C1 = 15: C2 = 8
    IF PITJ% = 1 THEN SWAP C1, C2

    COLOR C1
    PRINT "Ú"; STRING$(BOXWIDTH - 2, "Ä"); "¿";

    FOR A% = ROW1% + 1 TO ROW2% - 1
        LOCATE A%, COL1%
        PRINT "³";
       
    NEXT A%

    COLOR C2
    FOR A% = ROW1% + 1 TO ROW2% - 1
        LOCATE A%, COL1% + 1: PRINT SPACE$(BOXWIDTH - 2); "³";
    NEXT

    COLOR C2
    LOCATE ROW2%, COL1%
    PRINT "À"; STRING$(BOXWIDTH - 2, "Ä"); "Ù";
END SUB

SUB dBOX (ROW1%, COL1%, ROW2%, COL2%, A$, B$, C$, D$, e$, F$) STATIC
    BOXWIDTH = COL2% - COL1% + 1

    LOCATE ROW1%, COL1%
    PRINT A$; STRING$(BOXWIDTH - 2, C$); B$;

    FOR A% = ROW1% + 1 TO ROW2% - 1
        LOCATE A%, COL1%
        PRINT D$; SPACE$(BOXWIDTH - 2); D$;
    NEXT A%

    LOCATE ROW2%, COL1%
    PRINT e$; STRING$(BOXWIDTH - 2, C$); F$;
END SUB

SUB FASTIMP (X, Y, C)
    DEF SEG = TESTSCREEN

    POKE X * 2 + Y * 160, C

    DEF SEG
END SUB

FUNCTION FASTREADCAR (X%, Y%)
    DEF SEG = TESTSCREEN

    FASTREADCAR = PEEK(X% * 2 + Y% * 160)

    DEF SEG

END FUNCTION

SUB MENU (CASO%, FILA%, COLUM%, ANCHO%, ITEMS%, SOM%)
SHARED TEXTOSMENUP()  AS STRING * 20

T = 0
FOR ANTPOS% = 1 TO ITEMS%
COLOR CF, CT, 0
    LOCATE (FILA% + ANTPOS%), COLUM%: PRINT "³"; STRING$(ANCHO%, " "); "³"
    LOCATE (FILA% + ANTPOS% + 1), COLUM%
    LOCATE (FILA% + ANTPOS%), (COLUM% + 2): PRINT TEXTOSMENUP(ANTPOS% \ 1)
NEXT

    LOCATE (FILA%), COLUM%: PRINT "Ú"; STRING$(ANCHO%, "Ä"); "¿"
    LOCATE (FILA% + 1 + ITEMS%), COLUM%
    PRINT "À"; STRING$(ANCHO%, "Ä"); "Ù"
    LOCATE (FILA% + 2 + ITEMS%), COLUM% + 2
  IF SOM% = 1 THEN
    COLOR 7, 0, 0
    PRINT STRING$(ANCHO%, " "); "  ";
    FOR ANTPOS% = 1 TO ITEMS% + 1
        COLOR 7, 0, 0
        LOCATE (FILA% + ANTPOS%), COLUM% + ANCHO% + 2: PRINT "  ";
    NEXT
  END IF
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
          S = 55
          EXIT DO
       CASE 27
          S = 1
          EXIT DO
       CASE ELSE
    END SELECT
    MouseOff
    COLOR CF, CT, 0
    LOCATE (FILA% + ANTPOS%), COLUM% + 1: PRINT STRING$(ANCHO%, " ")
    LOCATE (FILA% + ANTPOS%), (COLUM% + 2): PRINT TEXTOSMENUP(ANTPOS%)

    COLOR 1, CF, 0
    LOCATE (FILA% + POSICION%), (COLUM% + 2)
    PRINT TEXTOSMENUP(POSICION%)
    COLOR CT, CF, 0

    DO
      K$ = INKEY$
      COLOR 9, 7
      LOCATE 23, 9, 0: PRINT TIME$
      'LOCATE 1, 1: PRINT MOUSEX / 8 + 1, MOUSEY / 8 + 1
     
      IF MouseRegion(2, 58, 21, 79, 24) AND MouseDown(1) THEN
         MouseOff
         COLOR 7, 0
         CLS
         SYSTEM
      END IF
     
      IF MouseRegion(2, 2, 2, 30, 4) AND MouseDown(1) THEN
         CASO% = 888
         EXIT SUB
      END IF

      IF MouseRegion(2, 59, 22, 78, 24) THEN
         COLOR 28, 0
         LOCATE 23, 60: PRINT "   <ESC> SORTIR   ";
      ELSE
         COLOR 4, 7
         LOCATE 23, 60: PRINT "   <ESC> SORTIR   ";

         IF MouseRegion(2, 2, 2, 30, 4) THEN
            COLOR 5, 0
            LOCATE 3, 3: PRINT "T U R B O   P A N T S  2.0"
         ELSE
            COLOR 5, 3
            LOCATE 3, 3: PRINT "T U R B O   P A N T S  2.0"
         END IF
      END IF

      MouseOn
    LOOP WHILE K$ = ""
    TECLA% = ASC(RIGHT$(K$, 1))

LOOP UNTIL TECLA% = RETORNO
IF S = 1 THEN
   CASO% = 999
ELSE
   CASO% = POSICION%
END IF
END SUB

FUNCTION READFASTCOL (X%, Y%)
    DEF SEG = TESTSCREEN

    READFASTCOL = PEEK(X% * 2 + Y% * 160 + 1)

    DEF SEG
END FUNCTION

