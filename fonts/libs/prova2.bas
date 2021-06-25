
'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CAMPS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DMOUSE.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'C:\FACT3\FONTS\win.bi'

ON ERROR GOTO ERRORS
SetDirRecursos ("c:\FACT3\RECURSOS\")
ERROR 11

CLS
PRINT "Today's date is "; FormatD$(Now#, "dd-mmm-yy"); "."
PRINT "The correct time is "; FormatD$(Now#, "hh:mm:ss AM/PM"); "."
 

END
ERRORS:
ShowError
RESUME NEXT


