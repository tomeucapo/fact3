DECLARE FUNCTION MouseDown (B!)
DECLARE SUB MOUSEINFO ()
DECLARE FUNCTION MOUSEREGION2! (Y!, X!, Y2!, X2!)
DECLARE SUB SETBOTOXY (X!, Y!, X2!, Y2!, B!)
DECLARE SUB SETMAXBOTONS (MAX!)
DECLARE SUB SHOWBOTO (TITOL$, B!)

TYPE GEST                         ' ESTRUCTURA DE UN BOTO
     X AS INTEGER
     Y AS INTEGER
     X2 AS INTEGER
     Y2 AS INTEGER
     PIX AS INTEGER
     XPI AS INTEGER
     YPI AS INTEGER
     TITOL AS STRING * 24
END TYPE

TYPE CTRL
    MANUAL AS SINGLE
    AUTOMA AS SINGLE
END TYPE

TYPE FINC
     FINCA AS STRING * 18
     TEMP AS INTEGER
     HUMITAT AS INTEGER
     VENT AS INTEGER
     CONTROL AS CTRL
     BOTO AS GEST
END TYPE

DECLARE FUNCTION DIALOG! ()
DECLARE SUB INFODISC ()
DECLARE SUB MASCARAMENU ()
DECLARE SUB MASCARA ()
COMMON SHARED MAXBOTS


'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DMOUSE.BI'

DIM SHARED BOTS(0 TO 10) AS GEST
DIM SHARED L$(0 TO 8)


L$(0) = "ANGLŠS"
L$(1) = "FRANCES"
L$(2) = "HOLANDES"
L$(3) = "ALEMANY"
L$(4) = "SUEC"
L$(5) = "FINLANDES"
L$(6) = "ESPANYOL"
L$(7) = "PORTUGUES"
L$(8) = "ITALIA"

MouseOn
MASCARAMENU
MouseOff

FUNCTION DIALOG

DO
MouseOn
 C$ = INKEY$
 LOCATE 1, 60: PRINT TIME$
 FOR I = 0 TO MAXBOTS  ' ESCANETJA TOTS EL PUNTS DEFINITS COM A BOTONS
     LOCATE 1, 60: PRINT TIME$
     X = BOTS(I).X
     Y = BOTS(I).Y
     X2 = BOTS(I).X2
     Y2 = BOTS(I).Y2

     IF MOUSEREGION2(X, Y, X2, Y2) THEN
	EXIT FOR
     END IF
     IF C$ = CHR$(27) THEN
	DIALOG = 999
	EXIT FUNCTION
     END IF
 NEXT

 
 IF MOUSEREGION2(X, Y, X2, Y2) AND MouseDown(1) THEN
    LOCATE 1, 60: PRINT TIME$
    V = 1
    DO
       LOCATE 1, 60: COLOR 15: PRINT TIME$
       IF MOUSEREGION2(X, Y, X2, Y2) THEN
	 IF V = 1 THEN
	    MouseOff
	    X% = X: Y% = Y: X2% = X2: Y2% = Y2
	    COLOR 1: BOX X%, Y%, X2%, Y2%
	    COLOR 7: BOX X%, Y% + 1, X2%, Y2% + 1
	    LOCATE X + 1, Y + 2: PRINT BOTS(I).TITOL;
	    MouseOn
	  V = 0
	 END IF
       ELSE
	 IF V = 0 THEN
	  MouseOff
	  X% = X: Y% = Y: X2% = X2: Y2% = Y2
	  COLOR 1: BOX X%, Y% + 1, X2%, Y2% + 1
	  COLOR 15: BOX X%, Y%, X2%, Y2%
	  LOCATE X + 1, Y + 1: PRINT BOTS(I).TITOL;
	  MouseOn
	  V = 1
	 END IF
       END IF
    LOOP UNTIL MouseDown(0)

    IF MOUSEREGION2(X, Y, X2, Y2) THEN
       EXIT DO
    END IF
 END IF
LOOP

MouseOff
	  X% = X: Y% = Y: X2% = X2: Y2% = Y2
	  COLOR 1: BOX X%, Y% + 1, X2%, Y2% + 1
	  COLOR 15: BOX X%, Y%, X2%, Y2%
	  LOCATE X + 1, Y + 1: PRINT BOTS(I).TITOL;

MouseOn

DIALOG = I
END FUNCTION

SUB INFODISC
    MouseOff
    COLOR 15, 9
    FINESTRA 1, 31, 24, 79, 1, CAIXA1
    LOCATE 2, 32: PRINT "Informaci¢ de l'unitat de disc actual"
    LOCATE 3, 32: PRINT STRING$(47, "Ä");

    REGS.ax = &H1900
    CALL Interrupt(&H21, REGS, REGS)
    DRIVE$ = CHR$((REGS.ax AND &HFF) + 65) + ":"
    REGS.ax = &H3600
    REGS.dx = ASC(UCASE$(DRIVE$)) - 64
    CALL Interrupt(&H21, REGS, REGS)

    SectorsInCluster = REGS.ax
    BytesInSector = REGS.cx
    IF REGS.dx >= 0 THEN ClustersInDrive = REGS.dx ELSE ClustersInDrive = REGS.dx + 65536
    IF REGS.bx >= 0 THEN ClustersAvailable = REGS.bx ELSE ClustersAvailable = regx.bx + 65536
    Freespace = ClustersAvailable * SectorsInCluster * BytesInSector

    COLOR 14: LOCATE 4, 32: PRINT "Unitat "; DRIVE$; " "; : COLOR 15
    PRINT USING "###,###,###"; Freespace; : PRINT " Bytes Lliures."
    COLOR 14: LOCATE 6, 32: PRINT "Clusters: "; : COLOR 15: PRINT ClustersInDrive
    COLOR 14: LOCATE 7, 32: PRINT " Sectors: "; : COLOR 15: PRINT SectorsInCluster
    COLOR 14: LOCATE 8, 32: PRINT "   Bytes: "; : COLOR 15: PRINT BytesInSector
    MouseOn
END SUB

SUB MASCARAMENU
    COLOR 15, 9
    FINESTRA 1, 1, 24, 30, 1, CAIXA1
    COLOR 15, 3
    LOCATE 2, 2: PRINT "     Control Manager 1.0    "
    LOCATE 3, 2: PRINT "   Tomeu Cap¢ Cap¢ 1995 (C) "
    COLOR 14: LOCATE 4, 2: PRINT "       CEIA CB 1995 (C)     "
    COLOR 15, 9
    LOCATE 5, 2: PRINT STRING$(28, "Í");

    SETMAXBOTONS 5
    SETBOTOXY 6, 2, 8, 27, 0
    SETBOTOXY 9, 2, 11, 27, 1
    SETBOTOXY 12, 2, 14, 27, 2
    SETBOTOXY 15, 2, 17, 27, 3
    SETBOTOXY 18, 2, 20, 27, 4
    SETBOTOXY 21, 2, 23, 27, 5
    SHOWBOTO "Unitat de disc", 0
    SHOWBOTO "Mouse", 1
    SHOWBOTO "CD-ROM", 2
    SHOWBOTO "Targeta De So", 3
    SHOWBOTO "Targeta Gr…fica", 4
    SHOWBOTO "BIOS", 5
DO
    SELECT CASE DIALOG
	   CASE 0
		INFODISC
	   CASE 1
		MOUSEINFO
	   CASE 999
		MouseOff
		SYSTEM
	   CASE ELSE
    END SELECT
LOOP

END SUB

SUB MOUSEINFO
    COLOR 15, 9
    FINESTRA 1, 31, 24, 79, 1, CAIXA1
    LOCATE 2, 32: PRINT "Informaci¢ del ratoli"
    LOCATE 3, 32: PRINT STRING$(47, "Ä");
    COLOR 14: LOCATE 4, 32: PRINT "Sensibilitat del ratol¡: "
    LOCATE 5, 32: PRINT STRING$(47, "Ä");

    REGS.ax = &H1B
    Interrupt &H33, REGS, REGS

    COLOR 14: LOCATE 6, 35: PRINT "               Horitzontal:"; : COLOR 15: PRINT REGS.bx
    COLOR 14: LOCATE 7, 35: PRINT "                  Vertical:"; : COLOR 15: PRINT REGS.cx
    COLOR 14: LOCATE 8, 35: PRINT " Umbral de doble velocitat:"; : COLOR 15: PRINT REGS.dx

    REGS.ax = 35
    Interrupt 51, REGS, REGS

    COLOR 14: LOCATE 10, 35: PRINT "                Llenguatge: "; : COLOR 15: PRINT L$(REGS.bx)

    REGS.ax = 36
    Interrupt 51, REGS, REGS
    COLOR 14: LOCATE 11, 35: PRINT "                    Versi¢:"; : COLOR 15: PRINT REGS.bx
    COLOR 14: LOCATE 11, 35: PRINT "                IRQ N—mero:"; : COLOR 15: PRINT REGS.cx
END SUB

FUNCTION MOUSEREGION2 (Y, X, Y2, X2)
	 REGS.ax = 3
	 Interrupt 51, REGS, REGS
	 MOUSEREGION2 = ((REGS.cx / 8 + 1 > X) AND (REGS.cx / 8 + 1 < X2)) AND ((REGS.dx / 8 + 1 > Y) AND (REGS.dx / 8 + 1 < Y2))
END FUNCTION

SUB SETBOTOXY (X, Y, X2, Y2, B)
    BOTS(B).X = X
    BOTS(B).Y = Y
    BOTS(B).X2 = X2
    BOTS(B).Y2 = Y2
END SUB

SUB SETMAXBOTONS (MAX)
    MAXBOTS = MAX
END SUB

SUB SHOWBOTO (TITOL$, B)

    X% = BOTS(B).X
    Y% = BOTS(B).Y
    X2% = BOTS(B).X2
    Y2% = BOTS(B).Y2
    BOTS(B).TITOL = TITOL$

    BOX X%, Y%, X2%, Y2%
    LOCATE X% + 1, Y% + 1: PRINT TITOL$;

END SUB

