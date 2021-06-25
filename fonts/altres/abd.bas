DECLARE SUB PrintOut (TEXT$)
DECLARE SUB Welcome ()
DECLARE SUB InitPinta ()
DECLARE SUB DialegCommand ()
DECLARE SUB Pantalla ()
DECLARE SUB CreateEntorn ()
COMMON SHARED lo

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STOCK.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\WIN.BI'


CreateEntorn
InitPinta
DialegCommand

REM $DYNAMIC
SUB CreateEntorn
    IF SetInitWindows(2) THEN
       SYSTEM
    END IF

    lo = 18
    WIN = InitNewWindow(0, 1, 1, 19, 80, 0, "Output")
    WIN1 = InitNewWindow(1, 20, 1, 24, 80, 0, "Comandes")
    FOR F = 0 TO 1
	SetColorWindow F, 7, 0, 15, 0, 7, 0
	SetStyleWindow F, 0, ""
    NEXT

END SUB

REM $STATIC
SUB DialegCommand

'    DO
'      value = LlegirCamps(0)
'      SELECT CASE value
	
END SUB

REM $DYNAMIC
SUB InitPinta
    ShowWindow 0
    ShowWindow 1
    Welcome


END SUB

SUB PrintOut (TEXT$)
    IF lo >= 18 THEN
       lo = 18
       SCROLLUP 17, 78, 4, 1, 1
       LOCATE lo, 2: PRINT TEXT$;
    ELSE
       LOCATE lo, 2: PRINT TEXT$;
       lo = lo + 1
    END IF
END SUB

REM $STATIC
SUB Welcome

PrintOut "Smart-Terminal 1.0"
PrintOut "Gestor remot del programa Facturaci¢ 3.0"
PrintOut "Tomeu Cap¢ i Cap¢ 1997 (C)"
PrintOut ""

END SUB

