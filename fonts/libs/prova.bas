
'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT2\FONTS\CAMPS.BI'
'$INCLUDE: 'C:\FACT2\FONTS\VIDEOF.BI'
'$INCLUDE: 'C:\FACT2\FONTS\DMOUSE.BI'
'$INCLUDE: 'C:\FACT2\FONTS\win.bi'

    CLS
    FONS 177
    SETMAXBOTONS 1
    FOR B = 0 TO 1
	SETBOTOCOLOR 15, 9, 14, 11, B
    NEXT

    SETBOTOXY 10, 10, 12, 19, 0
    SETBOTOXY 10, 21, 14, 40, 1
    SHOWBOTO "ACEPTAR", 0
    SHOWBOTO "CANCEL.LAR", 1

DO
    SELECT CASE DIALOG(0)
	   CASE 0
		'INFODISC
	   CASE 1
		'MOUSEINFO
	   CASE 999
		MouseOff
		SYSTEM
	   CASE ELSE
    END SELECT
LOOP

