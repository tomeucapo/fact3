'$INCLUDE: 'D:\FACT3\FONTS\STRUCTS.BI'


DIM ANYS AS ANNO
DIM CEMPRE AS CONTROLEMP

GOTO REGISTRES

AREAA = FREEFILE
OPEN "C:\FACT3\FACTU001\ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)

NOUREG = LOF(AREAA) \ LEN(ANYS) + 1

GET AREAA, NOUREG - 1, ANYS           ' Tanca la data de l'any vell
ANYS.DATAT = DATE$
ANYS.HORAT = TIME$
PUT AREAA, NOUREG - 1, ANYS

'GET AREAA, NOUREG, ANYS
ANYS.CPY = "YEARS" + CHR$(26)         ' Inicia un any nou
ANYS.ANY = MID$(DATE$, 7, 4)
ANYS.DATAO = DATE$
ANYS.HORAO = TIME$
PUT AREAA, NOUREG, ANYS

CLOSE AREAA

END


REGISTRES:

AREA = FREEFILE
OPEN "C:\FACT3\FACTU001\1999\CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)

GET AREA, 1, CEMPRE

CEMPRE.MAXALBARAN = 1
CEMPRE.MAXFACTURA = 1
CEMPRE.MAXPRESUPO = 1

PUT AREA, 1, CEMPRE

CLOSE AREA

