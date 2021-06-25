'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'd:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'd:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'd:\FACT3\FONTS\STOCK.BI'
'$INCLUDE: 'd:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'd:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'd:\FACT3\FONTS\CONSTS.BI'

DIM SHARED CFG AS CONFIG
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
DIM SHARED usr AS USUARIS
DIM SHARED DOCNUM AS DN


     'OPEN "EMPRESA.FAC" FOR RANDOM SHARED AS 2 LEN = LEN(EMPR)

     OPEN "USUARIS.FAC" FOR RANDOM SHARED AS 5 LEN = LEN(usr)


     GET 5, 1, usr

     PRINT "Informaci¢ usuari " + usr.NOM + ":"
     PRINT "---------------------------------------------"
     PRINT "M xim linies d'impressi¢: ", usr.MAXLINS
     PRINT "M xim linies de albarans: ", usr.LINALBA
     PRINT "M xim linies de factures: ", usr.LINFACT
     PRINT "Dispositiu d'impressi¢: " + usr.DEVICE
     PRINT "Codi d'impressora: ", usr.IMPRESORA
     PRINT "Tipus de document d'impressi¢: ", usr.modeimpres
     PRINT "---------------------------------------------"
     PRINT "Password: " + usr.PASSWORD

     INPUT "Mode de document: ", usr.modeimpres
     PUT 5, 1, usr
     CLOSE 5

