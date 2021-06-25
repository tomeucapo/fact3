' ********************************************************************
'
' Fitxer....................: STK_002.BAS
' Titol.....................: Modul per l'entrada de material      
'                             a l'stock.
' ********************************************************************
'
' Data inici................: 28/04/1996
' Data de la darrera revisi¢: 08/11/1996 16:40:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: DRAC Software 1993/96 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************


DECLARE SUB MantenimentStock (DIRECCO$, DIRECC$, DIRECI$, DH$, DEV$, IMPRESORA!)
DECLARE SUB SumarStock (AREA3!, MAXST!)
DECLARE SUB MASCLIST ()
DECLARE SUB SortIndex (INDEX() AS ANY, MAXST!)
DECLARE SUB LLISTAR (AREA4!, AREA3!)
DECLARE FUNCTION FindRecord% (CAMP$, INDEX() AS ANY, MAXST!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'c:\FACT2\FONTS\STOCK.BI'
'$INCLUDE: 'c:\FACT2\FONTS\FACTURA.BI'
'$INCLUDE: 'c:\FACT2\FONTS\STRUCTS.BI'
'$INCLUDE: 'c:\FACT2\FONTS\VIDEOF.BI'
'$INCLUDE: 'CAMPS.BI'


COMMON SHARED DIRECCM$, DIRCP$, DIRECCT$, DEVI$, UNIDAD$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT, TROBAT, MIDPOINT

DIM SHARED OBS$(1)                    ' LINIES D'OBSERVACIO
DIM SHARED STOCK AS stk               ' ESTRUCTURA DEL FITXER DEL STOCK
DIM SHARED STOCK1 AS STK1
DIM SHARED CEMPRE AS CONTROLEMP       '     "      DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CFG AS CONFIG              '     "      DEL FITXER DE CONFIGURACI¢
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAP€ALERAS DE DOCUMENTS
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
DIM SHARED USR AS USUARIS

      GOSUB TESTGEST
      OPEN "..\CONFIG.FAC" FOR RANDOM SHARED AS 1 LEN = LEN(CFG)
      OPEN "..\EMPRESA.FAC" FOR RANDOM SHARED AS 2 LEN = LEN(EMPRES)
      OPEN "..\CONTROL.CFG" FOR RANDOM SHARED AS 3 LEN = LEN(CTRL)
      OPEN "..\USUARIS.FAC" FOR RANDOM SHARED AS 5 LEN = LEN(USR)

      GET 3, 1, CTRL
      GET 1, CTRL.EMPRESA, CFG       ' RECULL LA CONFIGURACIO DE LA EMPRESA
      GET 2, CTRL.EMPRESA, EMPRES    ' RECULL LA CONFIGURACI¢ SECUNDARIA
      GET 5, CTRL.USUARI, USR

      UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
      DBF$ = LTRIM$(RTRIM$(CFG.DDADE))             ' Assignar direcctoris
      SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
      MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
      HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
      SYS$ = LTRIM$(RTRIM$(CFG.DSIST))
      IM$ = LTRIM$(RTRIM$(CFG.DIMPO))

      DIRECCF$ = UNIDAD$ + DBF$ + "\"      ' Subdirecctori de les base de dades
      DIRECCP$ = UNIDAD$ + SCR$ + "\"      ' Subdirecctori de les pantalles
      DIRECCR$ = UNIDAD$ + MSK$ + "\"      ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = UNIDAD$ + HLP$ + "\"     ' Subdirecctori de ajuda
      DIRECCI$ = UNIDAD$ + "\IMPRESOR\"    ' Subdirecctori de les impresores
      DIRECCT$ = UNIDAD$ + DBF$ + "\TEXTOS\"
      DIRECCS$ = UNIDAD$ + SYS$ + "\"
      DIRECCIM$ = UNIDAD$ + IM$ + "\"

      MAXLINS = USR.MAXLINS
      MAXFAC = USR.LINFACT
      IVA = EMPR.IVA
      DTO = EMPR.DTO
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA

      CLOSE 1, 2, 3, 5

      IF TESTSCREEN = &HB000 THEN
	 COL(0, 0) = 7: COL(0, 1) = 0
	 COL(1, 0) = 0: COL(1, 1) = 7
	 COL(2, 0) = 14: COL(2, 1) = 9
      ELSE
	 AREAC = FREEFILE: OPEN DIRECCF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
	 GET AREAC, 1, COLORS
	 COL(0, 0) = COLORS.COL(0, 0): COL(0, 1) = COLORS.COL(0, 1)
	 COL(1, 0) = COLORS.COL(1, 0): COL(1, 1) = COLORS.COL(1, 1)
	 COL(2, 0) = COLORS.COL(2, 0): COL(2, 1) = COLORS.COL(2, 1)
	 CLOSE AREAC
      END IF
      CALL MantenimentStock(DIRECCO$, DIRECCF$, DIRECCI$, DIRECCH$, DEV$, IMPRESORA)
      SYSTEM


TESTGEST:
      NOM$ = "..\" + STRING$(8, 255) + "." + "ÿÿÿ"
      OPEN NOM$ FOR RANDOM SHARED AS 2 LEN = 10
      FIELD 2, 10 AS LIN$
      GET 2, 1
      IF LIN$ <> ">“““““““< " THEN
	 COLOR 15, 7
	 FINESTRA 10, 20, 14, 51, 1, CAIXA1: COLOR 31, 7
	 LOCATE 12, 21: PRINT "ENTRADA INCORRECTA AL PROGRAMA"
	 LOCATE 16, 1: COLOR 7, 0: SYSTEM
      END IF
      CLOSE 2
      RETURN

FUNCTION FindRecord% (CAMP$, INDEX() AS INDEXTYPE, MAXST) STATIC
	 SHARED MIDPOINT
	 TOPRECORD = MAXST - 1
	 BottomRecord = 1

	 DO UNTIL (TOPRECORD < BottomRecord)
	    MIDPOINT = (TOPRECORD + BottomRecord) \ 2
	    TEST$ = RTRIM$(FORAESPAI$(INDEX(MIDPOINT).CODI))

	    IF TEST$ = CAMP$ THEN
	       EXIT DO
	    ELSEIF CAMP$ > TEST$ THEN
	       BottomRecord = MIDPOINT + 1
	    ELSE
	       TOPRECORD = MIDPOINT - 1
	    END IF
	 LOOP

	 IF TEST$ = CAMP$ THEN
	    LOCK AREA3, INDEX(MIDPOINT).REGISTRE
	    GET AREA3, INDEX(MIDPOINT).REGISTRE, STOCK
	    FindRecord% = TRUE
	 ELSE
	    FindRecord% = FALSE
	 END IF
END FUNCTION

SUB MantenimentStock (DIRECCO$, DIRECC$, DIRECI$, DH$, DEV$, IMPRESORA) STATIC
    SHARED DIRECCT$, DIRECCM$, MIDPOINT
    SHARED UNIDAD$
    '****** L'STOCK EST… A L'AREA NUMERO 3

    COLOR 15, 9: FINESTRA 10, 30, 14, 42, 1, CAIXA1
    COLOR 31, 9: LOCATE 12, 31: PRINT "  OBRINT..."
    GOSUB OBRIRFITXERS

    DIRECCT$ = DIRECC$ + "TEXTOS\"
    DEVI$ = DEV$: DIRECCM$ = DIRECC$

    DIM INDEX(1 TO MAXST) AS INDEXTYPE
    DIM NDXFILE AS INDEXTYPE

    IF MAXST = 1 THEN
       X = 7: R = 1: L = 1
       BEEP: AVIS "AVIS:", "El fitxer del megatzem est… buit.", 0
       EXIT SUB
    END IF

    COLOR 31, 9: LOCATE 12, 31: PRINT "INDEXANT..."
    GOSUB INDEXAR

    CALL SortIndex(INDEX(), MAXST)        ' ORDENAR L'INDEX

    GET AREA7, 1, NE
    GOSUB MASCARA

'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

READCAMPS:
     DO
       SETMAXCAMPS 6
       FOR C = 0 TO 12: SETCOLORCAMPS C, 15, 12, 15, 9, 14, 9: NEXT
       GOSUB INITCAMPS
       CORRECTE = 0
       DO
	 DO
	    ZZ$ = ""
	    SETMAXCAMPS 0: ERASEFIELD 0
	    IF LLEGIRCAMP(0) = SALIR THEN ANUL = 1: GOSUB TANCA
	    CAMPTEMP$ = FORAESPAI$(SHOWFIELD$(0))

	    IF FindRecord%(FORAESPAI$(CAMPTEMP$), INDEX(), MAXST) THEN
	       IF STOCK.MARCAT = "*" THEN
		  ZZ$ = "ZZZ"
		  EXIT DO
	       ELSE
		  ZZ$ = "ZZZ"
		  EXIT DO
	       END IF
	    END IF
	  LOOP UNTIL ZZ$ = "ZZZ"
	  R = INDEX(MIDPOINT).REGISTRE
	  GOSUB MOURECAMPS: COLOR 15
	  DISPLAYALL

	  SETMAXCAMPS 6
	  INSERTCAMP 1, FETXA$

	  FOR C = 1 TO 6
	     IF LLEGIRCAMP(C) = SALIR THEN ANUL = 1: UNLOCK AREA3, R : GOSUB TANCA
	  NEXT

	  PREUARTICLE = VAL(SHOWFIELD$(2))
	  EXISTENTS = STOCK.EXISTENCIA
	  TIPUSIVA = STOCK.TIPOIVA
	  DESCOMPT = STOCK.DESCOMPTE

	  EXISDESPUES = EXISTENTS + VAL(SHOWFIELD$(4))
	  
	  TOTALBRUT = PREUARTICLE * EXISDESPUES
	  BASEIMPONIBLE = TOTALBRUT - (TOTALBRUT * DESCOMPT) / 100
	  IVA = BASEIMPONIBLE * (TIPUSIVA) / 100
	  IMPORTE = BASEIMPONIBLE + IVA

	  LOCATE 15, 27: COLOR 15: PRINT USING "##,###,###.##"; IMPORTE; : COLOR 14
	  LOCATE 16, 29: COLOR 27: PRINT " Son correctes aquestes dades (S/N) ?"
	  COLOR 15, 9

	  DO
	   DO: T$ = INKEY$: LOOP UNTIL T$ <> ""
	   IF UCASE$(T$) = "S" THEN CORRECTE = 888: EXIT DO
	   IF UCASE$(T$) = "N" THEN CORRECTE = 0: EXIT DO
	  LOOP
       LOOP UNTIL CORRECTE = 888
       GOSUB MOUREFITXER
       PUT AREA3, R, STOCK: UNLOCK AREA3, R

       ' ACTUALITZAR CONTROL DE MOVIMENTS DE ENTRADES
       NE = NE + 1: PUT AREA7, 1, NE

       ' ACTUALITZAR HISTORIC
       GOSUB ACTUALIZHISTO

       LOCATE 16, 29: COLOR 27: PRINT "Desitges continuar amb un altre (S/N) ?"
       DO
	C$ = INKEY$
       LOOP UNTIL C$ = "S" OR C$ = "N" OR C$ = CHR$(27)
       LOCATE 16, 29: COLOR 27: PRINT "                                       "
    LOOP UNTIL UCASE$(C$) = "N" OR C$ = CHR$(27)
    GOSUB TANCA

'************************************************************************
' INICIALITZA CAMPS
'************************************************************************

INITCAMPS:
	  FOR C = 0 TO 12: ERASEFIELD C: NEXT
	  INITCAMP 0, 7, 27, ASCI, 0, STRING$(18, "X"), ""
	  INITCAMP 1, 9, 53, ASCI, 0, "99/99/99", ""
	  INITCAMP 2, 11, 27, NUM, 0, "9999999999", ""  ' PREU
	  INITCAMP 3, 11, 53, NUM, 0, "9999999999", ""  ' PVP
	  INITCAMP 4, 13, 27, NUM, 0, "9999999999", ""
	  INITCAMP 5, 12, 53, NUM, 0, "9999999999", ""  ' STOCK MAX
	  INITCAMP 6, 13, 53, NUM, 0, "9999999999", ""  ' STOCK MIN
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPS:
	  INSERTCAMP 0, STOCK.CODI
	  INSERTCAMP 1, STOCK.FECHACOMP
	  INSERTCAMP 2, LTRIM$(STR$(STOCK.PREU))
	  INSERTCAMP 3, LTRIM$(STR$(STOCK.PVPACONSE))
	  INSERTCAMP 4, LTRIM$(STR$(STOCK.EXISTENCIA))
	  INSERTCAMP 5, LTRIM$(STR$(STOCK.STOCKMAX))
	  INSERTCAMP 6, LTRIM$(STR$(STOCK.STOCKMIN))
	  LOCATE 15, 27: COLOR 15: PRINT USING "##,###,###.##"; STOCK.IMPORT; : COLOR 14
	  LOCATE 8, 27: COLOR 15: PRINT STOCK.DESCRIPCIO
	  LOCATE 9, 27: COLOR 15: PRINT STOCK.FECHACOMP: F$ = STOCK.FECHACOMP
	  LOCATE 12, 27: COLOR 15: PRINT STOCK.EXISTENCIA
	  RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXER:
	  STOCK.FECHACOMP = SHOWFIELD$(1)
	  STOCK.PREU = VAL(SHOWFIELD$(2))
	  STOCK.PVPACONSE = VAL(SHOWFIELD$(3))
	  STOCK.EXISTENCIA = EXISDESPUES
	  STOCK.STOCKMAX = VAL(SHOWFIELD$(5))
	  STOCK.STOCKMIN = VAL(SHOWFIELD$(6))
	  STOCK.IMPORT = IMPORTE
	  STOCK.MARCAT = ""
	  RETURN

'************************************************************************
' FITXA DE UN ARTICLE
'************************************************************************

MASCARA:

	  COLOR 15, 9
	  FINESTRA 4, 10, 18, 76, 1, CAIXA1: COLOR 14
	  LOCATE 5, 11: PRINT " ENTRADA DE GENERO AL STOCK                 Entrada N§:"; : COLOR 15: PRINT NE
	  LOCATE 6, 11: PRINT STRING$(65, "Ä"); : COLOR 14
	  LOCATE 7, 11: PRINT "Codi Article...:"
	  LOCATE 8, 11: PRINT "Descripci¢.....: ";
	  LOCATE 9, 11: PRINT "   Darrera Data:              Data Actual:";
	  LOCATE 11, 11: PRINT "Preu Article...: " + SPACE$(10) + " PVP Recomenat:"
	  LOCATE 12, 11: PRINT "Existencies....: " + SPACE$(10) + " Stock M…xim..:"
	  LOCATE 13, 11: PRINT SPACE$(27) + " Stock Minim..:"
	  LOCATE 13, 11: PRINT "Quant. rebuda..: "
	  LOCATE 15, 11: PRINT "Import total ..:"
	  RETURN


'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE STOCK
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECC$ + "STOCK.NDX" FOR RANDOM SHARED AS AREA2 LEN = LEN(NDXFILE)
      AREA3 = FREEFILE: OPEN DIRECC$ + "STOCK.DAT" FOR RANDOM SHARED AS AREA3 LEN = LEN(STOCK)
      AREA4 = FREEFILE: OPEN DIRECC$ + "PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CAP)
      AREA7 = FREEFILE: OPEN DIRECC$ + "ENTRADES.DAT" FOR RANDOM SHARED AS AREA7 LEN = 10
      

'************************************************************************
' DEMANAR IMPRESORA ACTIVA
'************************************************************************

      AREA5 = FREEFILE: OPEN "..\SPOOL.CFG" FOR RANDOM SHARED AS AREA5 LEN = LEN(SPOOLIMP)
      GET AREA5, IMPRESORA, SPOOLIMP
      FITXER$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
      CLOSE AREA5

'************************************************************************
' OBRIR FITXER DE LA IMPRESORA
'************************************************************************
      AREA6 = FREEFILE: OPEN DIRECI$ + FITXER$ FOR RANDOM SHARED AS AREA6 LEN = LEN(ARCHIMP)
      GET AREA6, 1, ARCHIMP
      CLOSE AREA6
'************************************************************************
' ACTUALITZAR REGISTRES
'************************************************************************
      GET AREA, 1, CEMPRE
      MAXST = CEMPRE.MAXSTOCK
      RETURN

INDEXAR:
      FOR RI = 1 TO MAXST - 1               ' COL.LOCAR A MEM•RIA L'INDEX
	  GET AREA2, RI, NDXFILE
	  INDEX(RI).REGISTRE = NDXFILE.REGISTRE
	  INDEX(RI).CODI = NDXFILE.CODI
      NEXT
      RETURN

GUARDARINDEX:
      CALL SortIndex(INDEX(), MAXST - 1)      ' ORDENAR L'INDEX
      FOR RI = 1 TO MAXST - 1
	  NDXFILE.REGISTRE = INDEX(RI).REGISTRE
	  NDXFILE.CODI = INDEX(RI).CODI
	  PUT AREA2, RI, NDXFILE
      NEXT
      RETURN
TANCA:
	  FOR C = 0 TO 12: ERASEFIELD C: NEXT
	  COLOR 15, 9: FINESTRA 10, 30, 14, 42, 1, CAIXA1
	  COLOR 31, 9: LOCATE 12, 31: PRINT " TANCANT..."
	  GOSUB GUARDARINDEX
	  FOR C = 0 TO 12: ERASEFIELD C: NEXT
	  ERASE INDEX, CAMPS
	  RESET
	  EXIT SUB

ACTUALIZHISTO:
       AREAHIS = FREEFILE: OPEN DH$ + "TEMPORAL.DAT" FOR OUTPUT AS AREAHIS
       PRINT #AREAHIS, TIME$ + " " + FETXA$ + " -  Entrada al stock N§:" + STR$(NE - 1)
       PRINT #AREAHIS, STRING$(65, "Ä")
       PRINT #AREAHIS, USING "Codi Article...: \" + SPACE$(16) + "\"; STOCK.CODI
       PRINT #AREAHIS, USING "Descripci¢.....: \" + SPACE$(38) + "\"; STOCK.DESCRIPCIO
       PRINT #AREAHIS, USING "   Darrera Data: \      \     Data Actual: \      \"; F$; STOCK.FECHACOMP
       PRINT #AREAHIS, ""
       PRINT #AREAHIS, USING "Preu Article...: #,###,###.##    PVP Recomenat: #,###,###.##"; PREUARTICLE; VAL(SHOWFIELD$(3))
       PRINT #AREAHIS, USING "Existencies....: ##,###.##       Stock M…xim..: ##,###.##"; EXISTENTS; STOCK.STOCKMAX
       PRINT #AREAHIS, USING "Quant. rebuda..: ##,###.##       Stock Minim..: ##,###.##"; EXISDESPUES; STOCK.STOCKMIN
       PRINT #AREAHIS, ""
       PRINT #AREAHIS, USING "Import total ..: #,###,###.## Pts."; STOCK.IMPORT
       PRINT #AREAHIS, STRING$(65, "Ä")
       CLOSE #AREAHIS
       CHDIR DH$
       IF DIR$("STOCK.DAT") = "" THEN
	  SHELL "COPY TEMPORAL.DAT STOCK.DAT /A /Y >nul"
       ELSE
	  SHELL "COPY STOCK.DAT+TEMPORAL.DAT /A /Y >nul"
       END IF
       KILL "TEMPORAL.DAT"
       CHDIR UNIDAD$ + "\MAIN"
       RETURN
END SUB

SUB SortIndex (INDEX() AS INDEXTYPE, MAXST) STATIC
    'SHARED INDEX() AS INDEXTYPE
    
    OFFSET = MAXST - 1 \ 2

    DO WHILE OFFSET > 0
       LIMIT = MAXST - 1 - OFFSET
       DO
	 SWITCH = FALSE
	 FOR i = 1 TO LIMIT
	     IF INDEX(i).CODI > INDEX(i + OFFSET).CODI THEN
		SWAP INDEX(i), INDEX(i + OFFSET)
		SWITCH = i
	     END IF
	 NEXT

	 LIMIT = SWITCH
       LOOP WHILE SWITCH
       OFFSET = OFFSET \ 2
    LOOP

END SUB

SUB SumarStock (AREA3, MAXST)
    'CALL SAVESCRN
    GetBackground 1, 1, 24, 79, cli$
    COLOR 15, 9: FINESTRA 9, 10, 16, 60, 1, CAIXA1: COLOR 14, 9
    LOCATE 10, 12: COLOR 14: PRINT "SUMA DEL STOCK"
    suma = 0: sumau = 0
    FOR R = 1 TO MAXST - 1
	GET AREA3, R, STOCK
	suma = suma + STOCK.IMPORT
	sumau = sumau + STOCK.PREU
	COLOR 14: LOCATE 11, 12: PRINT "Total Imports.......: "; : COLOR 15: PRINT USING "##,###,###.##"; suma; : COLOR 14: PRINT " Pts."
	COLOR 14: LOCATE 12, 12: PRINT "Total Preus Unitaris: "; : COLOR 15: PRINT USING "##,###,###.##"; sumau; : COLOR 14: PRINT " Pts."
	COLOR 14: LOCATE 14, 12: PRINT "N£mero d'articles...: "; : COLOR 15: PRINT USING "        #####"; R;
    NEXT
    SOUND 300, 2
    SOUND 100, 2
    SOUND 300, 2
    C$ = INPUT$(1)
    'CALL RESTSCRN
    PutBackground 1, 1, cli$
END SUB

SUB TRANFEREIXFITXERS (AREA2, AREA3)
    R = 1
    COLOR 15, 9
    DO WHILE NOT EOF(AREA2)
       GET AREA2, R, STOCK1
       STOCK.CODI = STOCK1.CODIGO
       STOCK.DESCRIPCIO = STOCK1.ARTICULO
       STOCK.CODIPROVEEDOR = STOCK1.CODIPROVE
       STOCK.NOMPROVEEDOR = STOCK1.NOMPROVEEDOR
       STOCK.EXISTENCIA = VAL(STOCK1.EXISTENCIA)
       STOCK.PREU = VAL(STOCK1.PRECIO)
       STOCK.DESCOMPTE = VAL(STOCK1.DESCUENTO)
       STOCK.PREUNET = VAL(STOCK1.PREUNET)
       STOCK.MARGECOMER = VAL(STOCK1.MARGECOMER)
       STOCK.PVPACONSE = VAL(STOCK1.PVPACONSE)
       STOCK.IMPORT = VAL(STOCK1.IMPORTE)
       STOCK.TIPOIVA = VAL(STOCK1.TIPOIVA)
       STOCK.TOTALIVA = VAL(STOCK1.TOTALIVA)

       STOCK.STOCKMIN = VAL(STOCK1.STOCKMIN)
       STOCK.STOCKMAX = VAL(STOCK1.STOCKMAX)
     
       STOCK.FECHACOMP = STOCK1.FECHACOMP
       STOCK.FAMILIA = STOCK1.FAMILIA
       STOCK.MARCAT = STOCK1.MARCAT
       LOCATE 1, 1: PRINT R
       PUT AREA3, R, STOCK
       R = R + 1
    LOOP
    PRINT "TRANSFERIT... Ok"
END SUB

