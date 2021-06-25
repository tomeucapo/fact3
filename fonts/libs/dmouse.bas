DECLARE FUNCTION IRQ! ()
DECLARE FUNCTION MOUSEBUTTONS! ()
DECLARE FUNCTION MouseDown! (BT!)
DECLARE FUNCTION MOUSELENG$ ()
DECLARE FUNCTION MOUSEMOTION! ()
DECLARE FUNCTION MouseRegion! (Y!, X!, y2!, x2!)
DECLARE FUNCTION MOUSEUP! (BT!)
DECLARE FUNCTION MOUSEVER! ()
DECLARE FUNCTION MouseX! ()
DECLARE FUNCTION MOUSEXT! ()
DECLARE FUNCTION MouseY! ()
DECLARE FUNCTION MOUSEYT! ()
DECLARE FUNCTION TYPEMOUSE$ ()
DECLARE FUNCTION VERSION! ()
'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
'³                   -------* Smart Mouse 1.0 *-------              ³
'³                                                                  ³
'³           LLIBRERIA PER POSSAR EL MOUSE EN QUICKBASIC 7.01       ³
'³                       TOMEU CAP¢ CAP¢ 1995 (C)                   ³
'³                         Smart 1993/95 (C)                        ³
'³                                                                  ³
'³                         Rutines Summer '95                       ³
'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
'³COMPILAR-LO:                                                      ³
'³               BC MOUSE.BAS /o;                                   ³
'³               LIB MOUSE.LIB MOUSE.OBJ;                           ³
'³               LINK /QU MOUSE+QBX.LIB,MOUSE.QLB,NUL,QBXQLB.LIB;   ³
'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ


REM $INCLUDE: 'QBX.BI'
REM $INCLUDE: 'D:\FACT3\FONTS\DMOUSE.BI'
REM $INCLUDE: 'D:\FACT3\FONTS\DRAC3.BI'

DIM SHARED LENGU$(0 TO 8), TIPU$(1 TO 5)
LENGU$(0) = "AnglŠs"
LENGU$(1) = "Franc‚s"
LENGU$(2) = "Holandes"
LENGU$(3) = "Alemany"
LENGU$(4) = "Suec"
LENGU$(5) = "Finlades"
LENGU$(6) = "Espanyol"
LENGU$(7) = "Portugues"
LENGU$(8) = "Itali…"

TIPU$(1) = "De bus"
TIPU$(2) = "En sŠrie"
TIPU$(3) = "In Port"
TIPU$(4) = "PS/2"
TIPU$(5) = "HP"

FUNCTION IRQ
    REGS.ax = &H24
    InterruptX &H33, REGS, REGS

    TI$ = STR$(REGS.cx)
    INTH$ = MID$(TI$, 3, 2)
    IRQ = VAL(INTH$)

END FUNCTION

FUNCTION MOUSEBUTTONS
       REGS.ax = 0
       REGS.bx = 0
       InterruptX &H33, REGS, REGS
       IF REGS.bx AND 2 THEN MOUSEBUTTONS = 2
       IF REGS.bx AND 3 THEN MOUSEBUTTONS = 3
       IF NOT REGS.ax THEN MOUSEBUTTONS = 0
END FUNCTION

FUNCTION MouseDown (BT)
	 MouseDown = 0

	 REGS.ax = &H5
	 REGS.bx = BT
	 CALL InterruptX(&H33, REGS, REGS)
	
	 IF REGS.ax = BT THEN
	    MouseDown = 1
	 END IF
END FUNCTION

FUNCTION MOUSELENG$
SHARED LENGU$()

       REGS.ax = &H23
       InterruptX &H33, REGS, REGS
       MOUSELENG$ = LENGU$(REGS.bx)

END FUNCTION

SUB MouseLimit (MODE, x1, y1, x2, y2)
   SELECT CASE MODE
    CASE MODE = 1
	 REGS.ax = 7
	 REGS.cx = x1 / 8 + 1
	 REGS.dx = x2 / 8 + 1
	 InterruptX &H33, REGS, REGS
	 REGS.ax = 8
	 REGS.cx = y1 / 8 + 1
	 REGS.dx = y2 / 8 + 1
	 InterruptX &H33, REGS, REGS
    CASE MODE = 2
	 REGS.ax = 7
	 REGS.cx = x1
	 REGS.dx = x2
	 InterruptX &H33, REGS, REGS
	 REGS.ax = 8
	 REGS.cx = y1
	 REGS.dx = y2
	 InterruptX &H33, REGS, REGS
    CASE ELSE
   END SELECT
END SUB

FUNCTION MOUSEMOTION
    REGS.ax = 11
    InterruptX &H33, REGS, REGS
    IF REGS.cx > 0 THEN MOUSEMOTION = 1        ' DRETA
    IF REGS.dx > 0 THEN MOUSEMOTION = 0        ' ESQUERRA
END FUNCTION

SUB MouseOff
    REGS.ax = 2
    CALL InterruptX(&H33, REGS, REGS)
END SUB

SUB MouseOn
    REGS.ax = 1
    CALL InterruptX(&H33, REGS, REGS)
END SUB

FUNCTION MouseRegion (Y, X, y2, x2)
	 REGS.ax = 3
	 InterruptX 51, REGS, REGS
	 MouseRegion = ((REGS.cx / 8 + 1 > X) AND (REGS.cx / 8 + 1 < x2)) AND ((REGS.dx / 8 + 1 > Y) AND (REGS.dx / 8 + 1 < y2))
END FUNCTION

FUNCTION MOUSEUP (BT)
    
     REGS.ax = &H5
     REGS.bx = BT

     InterruptX &H33, REGS, REGS

     IF REGS.ax = BT AND REGS.bx = 0 THEN
	MOUSEUP = 1
     END IF
       
END FUNCTION

FUNCTION MOUSEVER
     REGS.ax = &H24
     InterruptX &H33, REGS, REGS
     MOUSEVER = REGS.bx
END FUNCTION

FUNCTION MouseX
    REGS.ax = 3
    CALL InterruptX(&H33, REGS, REGS)
    MouseX = REGS.cx
END FUNCTION

FUNCTION MOUSEXT
    REGS.ax = 3
    CALL InterruptX(&H33, REGS, REGS)
    MOUSEXT = REGS.cx / 8 + 1
END FUNCTION

FUNCTION MouseY
    REGS.ax = 3
    CALL InterruptX(&H33, REGS, REGS)
    MouseY = REGS.dx
END FUNCTION

FUNCTION MOUSEYT
    REGS.ax = 3
    CALL InterruptX(&H33, REGS, REGS)
    MOUSEYT = REGS.dx / 8 + 1
END FUNCTION

FUNCTION TYPEMOUSE$
SHARED TIPU$()
    REGS.ax = &H24
    InterruptX &H33, REGS, REGS

    TI$ = STR$(REGS.cx)
    TIP$ = MID$(TI$, 1, 2)

    TYPEMOUSE$ = TIPU$(VAL(TIP$))
END FUNCTION

FUNCTION VERSION

    REGS.ax = &H24
    InterruptX &H33, REGS, REGS

    TI$ = STR$(REGS.bx)

    VER$ = MID$(TI$, 1, 2)
    VE2$ = MID$(TI$, 3, 3)
   
    V = VAL(VER$)
    V2 = VAL(VE2$)

    VER$ = VER$ + "." + VE2$
    VERSION = VAL(VER$)
END FUNCTION

