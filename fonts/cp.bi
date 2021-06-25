'
' CP.BI
' Estructures de la base de dades dels Codis Postals
'
' Tomeu Cap¢ i Cap¢ 1997 (C)
'


TYPE CP
     CODI AS STRING * 12
     POBLE AS STRING * 27
     PAIS AS STRING * 27
     PROVINCIA AS STRING * 28
     MARCAT AS STRING * 1
END TYPE

TYPE INDEXCP
     CODI AS STRING * 7
     REGISTRE AS INTEGER
END TYPE





