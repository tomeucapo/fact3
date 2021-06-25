'
' STOCK.BI
'
' Estructura dels fitxers de la gesti� del stock
'
' Tomeu Cap� Cap� 1996 (C)
'

TYPE STK                       ' ESTRUCTURA PRINCIPAL STOCK.DAT
     CODI AS STRING * 18
     DESCRIPCIO AS STRING * 40
     CODIPROVEEDOR AS STRING * 7
     NOMPROVEEDOR AS STRING * 20
     EXISTENCIA AS LONG
     PREU AS LONG
     DESCOMPTE AS INTEGER
     PREUNET AS LONG
     MARGECOMER AS INTEGER

     PVPACONSE AS LONG
     IMPORT AS LONG
     TIPOIVA AS INTEGER
     TOTALIVA AS LONG
     STOCKMIN AS INTEGER
     STOCKMAX AS INTEGER
     FECHACOMP AS STRING * 8
     FAMILIA AS STRING * 23
     MARCAT AS STRING * 1
END TYPE

TYPE STK1
     CODIGO AS STRING * 7
     ARTICULO AS STRING * 40
     CODIPROVE AS STRING * 7
     NOMPROVEEDOR AS STRING * 20
     EXISTENCIA AS STRING * 8
     PRECIO AS STRING * 9
     DESCUENTO AS STRING * 5
     PREUNET AS STRING * 9
     MARGECOMER AS STRING * 2
     PVPACONSE AS STRING * 9
     IMPORTE AS STRING * 10
     TIPOIVA AS STRING * 2
     TOTALIVA AS STRING * 9

     STOCKMIN AS STRING * 9
     STOCKMAX AS STRING * 8

     FECHACOMP AS STRING * 8
     FAMILIA AS STRING * 23
     MARCAT AS STRING * 1
END TYPE

TYPE FAMI
     CODI AS STRING * 4
     DESCRIPCIO AS STRING * 23
     UBICACIO AS STRING * 40
     MARCAT AS STRING * 1
END TYPE
  
TYPE INDEXTYPE                ' ESTRUCTURA DE L'INDEX
     REGISTRE AS INTEGER
     CODI AS STRING * 18
END TYPE

TYPE INDEXFAMI
     REGISTRE AS INTEGER
     CODI AS STRING * 4
END TYPE
  
