'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'D:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'D:\FACT3\FONTS\SDK_001.BI'

FASTPRINT 0, 0, 2, "Primera prova"
FOR I = 1 TO 2000
    FinestraEstat "XXXXXXXXXXXXXXXXX" + LTRIM$(RTRIM$(STR$(I))) + " ", 1
NEXT
FASTPRINT 0, 0, 3, "Segona prova"

FOR I = 1 TO 10
    FinestraEstat "weorihjweiorhjwperhio", 1
NEXT

