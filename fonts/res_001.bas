' ********************************************************************
'
' Fitxer....................: RES_001.BAS
' Titol.....................: Modul per el mateniment dels resguards
'                             per les reparacions.
' ********************************************************************
'
' Data inici................: 07/06/1996 16:30:36
' Data de la darrera revisi¢: 11/09/1999 21:30:00
' Autor.....................: Tomeu Cap¢ Cap¢
' Copyright.................: Smart Software 1993/99 (C)
'
' ********************************************************************
' Notes: MODUL AMPLIAT PER "IMATGE I SO".
'        LES COLUMNES D'IMPRESI¢ ESTAN A 80
' ********************************************************************

DECLARE SUB LLISTARESGUARDS (AREA4!, AREAR!, MAXRESG!, DEVI$)
DECLARE SUB MantenimentResguards (DTMP$, DIRECCP$, DIRECC$, DIRECI$, DP$, DEVI$, IMPRESORA!)
DECLARE SUB IMPRIMIRRESGUARD (REG!, DEVI$, AREA4!, AREAR!, AREAP!, AREAF!)
DECLARE SUB MASCREGS ()
DECLARE SUB LLISTAR (AREA4!, AREA3!, MAXLINS!)
DECLARE FUNCTION FindClient% (CAMP$, INDEX() AS ANY, MAXCL!)
DECLARE SUB MASCCLIS ()
DECLARE SUB SortClients (INDEX() AS ANY, MAXCL!)
DECLARE SUB FacturaResguard (AREAF!, EDITAR!, REGISTRE!)

COMMON SHARED DIRECC$, DIRCP$, DEVI$, DIRECCT$, DTMP$, CADFE$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5
COMMON SHARED GUARDAT, RNDX, MIDPOINT, POSANI

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'D:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'D:\FACT3\FONTS\CP.BI'
'$INCLUDE: 'D:\FACT3\FONTS\RESGUARD.BI'
'$INCLUDE: 'D:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'D:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'D:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'D:\FACT3\FONTS\WIN.BI'
'$INCLUDE: 'D:\FACT3\FONTS\CONSTS.BI'


'$DYNAMIC
DIM SHARED CEMPRE AS CONTROLEMP       ' ESTRUCTURA DEL FITXER DE CONTROL DE FITXERS
DIM SHARED CAP AS CAPSALDOCS          '     "      DE CAP€ALERAS DE DOCUMENTS
DIM SHARED SPOOLIMP AS SPOOL          '     "      DEL FITXER QUE CONTROLA LES IMPRESORES
DIM SHARED ARCHIMP AS IMPRESORA       '     "      DE LA IMPRESORA SELECCIONADA
DIM SHARED CLIENTS AS CLIENT
DIM SHARED CODISP AS CP
DIM SHARED RESGUA AS RESG
DIM SHARED FACTURAC AS LINEFACTRESC
DIM SHARED FACTURA AS LINEFACTRES
DIM SHARED CFG AS CONFIG
DIM SHARED EMPRES AS EMPRESA
DIM SHARED CTRL AS CONTROL
DIM COLORS AS COLO
DIM SHARED COL(2, 2)
DIM SHARED PP AS PIEPAGINA
DIM SHARED USR AS USUARIS
DIM SHARED DOCNUM AS DN
DIM SHARED BORRCLI AS REGBORRAT
DIM SHARED BORRREG AS REGBORRAT
DIM SHARED XXX$(1)
DIM PASO AS TRANS
DIM ANYS AS ANNO
POSANI = 1

'********************************************************************
'  Comprovaci¢ de la cridada del programa principal
'********************************************************************

	  TMP$ = ENVIRON$("TEMPORAL")
	  DEF SEG = VARSEG(CFG)
	  BLOAD TMP$ + "PASS.TMP", VARPTR(CFG)
	  DEF SEG

	  DEF SEG = VARSEG(USR)
	  BLOAD TMP$ + "PASU.TMP", VARPTR(USR)
	  DEF SEG

	  DEF SEG = VARSEG(EMPRES)
	  BLOAD TMP$ + "PASE.TMP", VARPTR(EMPRES)
	  DEF SEG

	  DEF SEG = VARSEG(PASO)
	  BLOAD TMP$ + "PROT.TMP", VARPTR(PASO)
	  DEF SEG
	  CA$ = PASO.CLAU: CAD$ = PASO.APLICACIO: CA2$ = ""

	  FOR L% = 1 TO LEN(CAD$)
	      CA2$ = CA2$ + ENCRIPT$(MID$(CA$, L%, 1), L%)
	  NEXT

	  IF LTRIM$(RTRIM$(CA2$)) <> "RES_001.EXE," THEN
	     tecla = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "Pitja una tecla...", 0)
	     SYSTEM
	  END IF

	  KILL TMP$ + "PASS.TMP"
	  KILL TMP$ + "PASU.TMP"
	  KILL TMP$ + "PASE.TMP"
	  KILL TMP$ + "PROT.TMP"
      
      UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))
      DBF$ = LTRIM$(RTRIM$(CFG.DDADE))             ' Assignar direcctoris
      SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
      MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
      HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
      SYS$ = LTRIM$(RTRIM$(CFG.DSIST))
      DTMP$ = LTRIM$(RTRIM$(CFG.DTEMP))

      DIRECCF$ = DBF$     ' Subdirecctori de les base de dades
      DIRECCP$ = SCR$ + "\"      ' Subdirecctori de les pantalles
      DIRECCR$ = MSK$ + "\"     ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = HLP$ + "\"    ' Subdirecctori de ajuda
      DIRECCI$ = UNIDAD$ + "\IMPRESOR\"  ' Subdirecctori de les impresores
      DIRECCT$ = DBF$ + "TEXTOS\"
      DIRECCH$ = DBF$ + "HISTORIC\"
      DIRECCTM$ = DTMP$
      DP$ = DBF$

      SetDirRecursos (DIRECCR$)    ' Assignar el direcctori de recursos
      SetFormatCC (34)             ' Format numŠric Espanyol

      MAXLINS = USR.MAXLINS: R = EMPRES.ANY: DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA
 
      AREAC = FREEFILE: OPEN DIRECCF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
      GET AREAC, 1, COLORS
      COL(0, 0) = COLORS.COL(0, 0): COL(0, 1) = COLORS.COL(0, 1)
      COL(1, 0) = COLORS.COL(1, 0): COL(1, 1) = COLORS.COL(1, 1)
      COL(2, 0) = COLORS.COL(2, 0): COL(2, 1) = COLORS.COL(2, 1)
      CLOSE AREAC

      AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
      GET AREAA, R, ANYS
      DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"
      CLOSE AREAA

      IF DIR$(DIRECCF$ + "CLIENTS.NDX") <> "" THEN
	 SHELL "COPY " + DIRECCF$ + "CLIENTS.NDX " + DTMP$ + " >nul"
      END IF
      CALL MantenimentResguards(DTMP$, DIRECCP$, DIRECCF$, DIRECCI$, DP$, DEV$, IMPRESORA)
      SYSTEM

ERRORS:
       IF ShowError = 1 THEN
	  RESUME NEXT
       ELSE
	  SYSTEM
       END IF

REM $STATIC
SUB FacturaResguard (AREAF, EDITAR, REGISTRE)

    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 7, 5, 21, 79, 1, CAIXA1
    LOCATE 10, 6: PRINT "ÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄ"
    LOCATE 11, 6: PRINT "QUANT. ³CONCEPTE                                ³PREU       ³IMPORT"
    LOCATE 12, 6: PRINT "ÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ"
    FOR L = 13 TO 19: LOCATE L, 6: PRINT "       ³                                        ³           ³": NEXT
    LOCATE 19, 6: PRINT "ÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ"
    LOCATE 20, 6: PRINT "                                                 Total brut:³"

    SetMaxCamps 4
    FOR C = 0 TO 4: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
    REGRESG = REGISTRE
    RMAX = LOF(AREAF) \ LEN(FACTURA)
    IF RMAX = 0 AND EDITAR = 1 THEN EDITAR = 0
    IF EDITAR = 0 THEN
       BEEP: PRINT RMAX
       LOCATE 7, 8: PRINT "( Factura N§: " + LTRIM$(STR$((RMAX \ 6) + 1)) + " )"
       FOR L = 1 TO 6
	   GOSUB CAMPSBUITS
	   PUT AREAF, RMAX + L + 1, FACTURA
       NEXT
       R = RMAX + 1: TIVA = 16: TDTO = 0
    ELSE
       R = 0
       FOR RS = 1 TO RMAX
	   GET AREAF, RS, FACTURA
	   IF FACTURA.RESGUARD = REGRESG THEN
	      R = RS: EXIT FOR
	   END IF
       NEXT
       IF R = 0 THEN
	  CALL FacturaResguard(AREAF, 0, REGISTRE)
	  EXIT SUB
       END IF
       RMAX = RS + 6
       LOCATE 7, 8: PRINT "( Factura N§: " + LTRIM$(STR$((RMAX \ 6))) + " )"
       GET AREAF, RMAX, FACTURA
       TIVA = FACTURA.TIPOIVA: TDTO = FACTURA.DTO
       X = 13: L = 1
       GOSUB CAMPS
       GOSUB MOSTRALINIES
    END IF
    TB = 0
  DO
    
    X = 13: L = 1: C = 2
    GOSUB CAMPS
    DO
	RESTA = 0
	DO
	    VALUE = ReadCamp(C)
	    SELECT CASE VALUE
		   CASE 0, 1
			IF VAL(ValueCamp$(0)) <> TIVA THEN TIVA = VAL(ValueCamp$(0))
			IF VAL(ValueCamp$(1)) <> TDTO THEN TDTO = VAL(ValueCamp$(1))
			GOSUB CALCULAFACTURA
			C = C + 1
		   CASE F2
			IF X > 13 THEN
			   DisplayAllCamps
			   RESTA = 1: L = L - 1: X = X - 1
			   R = R - 1: GET AREAF, R, FACTURA
			   GOSUB CAMPS
			   EXIT DO
			END IF
		   CASE F1, F3 TO F10
		   CASE 999
			EXIT SUB
		   CASE ELSE
			C = C + 1
	    END SELECT
	LOOP UNTIL C > 4
	IF L < 6 THEN
	   IF RESTA = 0 THEN GOSUB SALTALINIA
	ELSE
	   EXIT DO
	END IF
    LOOP UNTIL L > 6
    
    GET AREAF, R, FACTURA
    FACTURA.RESGUARD = REGISTRE
    FACTURA.TOTALBRUT = TB
    FACTURA.BASEIMPONIBLE = BASEIMP
    FACTURA.TIPOIVA = VAL(ValueCamp$(0))
    FACTURA.TOTALIVA = IVA
    FACTURA.DTO = VAL(ValueCamp$(1))
    FACTURA.TOTALNET = TN
    PUT AREAF, R, FACTURA
    COLOR 27: LOCATE 20, 7: PRINT "Son correctes aquestes dades (S/N) ?"
    COLOR COL(0, 0), COL(0, 1): CORR = 0
	  DO
	   DO
	     D$ = INKEY$
	   LOOP UNTIL D$ <> ""
	   IF UCASE$(D$) = "S" THEN
	      CORR$ = "SORTIR"
	      EXIT DO
	   ELSE
	      IF UCASE$(D$) = "N" THEN
		 CORR$ = "MODIFICAR"
		 GET AREAF, R - 5, FACTURA
		 R = R - 5
		 TB = 0: BASEIMP = 0
		 EXIT DO
	      END IF
	   END IF
	  LOOP
	  LOCATE 20, 7: PRINT "                                    "
  LOOP UNTIL CORR$ = "SORTIR"
  EXIT SUB

CAMPS:
	InsertValueCamp 0, LTRIM$(STR$(TIVA))
	InsertValueCamp 1, LTRIM$(STR$(TDTO))
	InsertValueCamp 2, LTRIM$(STR$(FACTURA.QUANTITAT))
	InsertValueCamp 3, FACTURA.CONCEPTE
	InsertValueCamp 4, LTRIM$(STR$(FACTURA.PREU))
	SetInitCamp 0, 8, 30, ASCI, 0, "999", "I.V.A. Factura %:"
	SetInitCamp 1, 9, 30, ASCI, 0, "999", "Descompte Factura %:"
	SetInitCamp 2, X, 6, ASCI, 0, "99999", ""
	SetInitCamp 3, X, 14, ASCI, 0, STRING$(40, "X"), ""
	SetInitCamp 4, X, 55, ASCI, 0, "99999999999", ""
	DisplayAllCamps
	GOSUB CALCULAFACTURA
	RETURN

CAMPSBUITS:
	FACTURA.QUANTITAT = 0
	FACTURA.CONCEPTE = ""
	FACTURA.PREU = 0
	FACTURA.IMPORT = 0
	FACTURA.BORRADA = "-"
	RETURN

SALTALINIA:
	   FACTURA.RESGUARD = REGISTRE
	   FACTURA.QUANTITAT = VAL(ValueCamp$(2))
	   FACTURA.CONCEPTE = ValueCamp$(3)
	   FACTURA.PREU = VAL(ValueCamp$(4))
	   FACTURA.IMPORT = VAL(ValueCamp$(2)) * VAL(ValueCamp$(4))
	   IF FACTURA.IMPORT = 0 THEN LOCATE X, 67: PRINT "            " ELSE LOCATE X, 67: PRINT FormatC$(FACTURA.IMPORT, "#.###.###")
	   PUT AREAF, R, FACTURA
	   L = L + 1: X = X + 1: R = R + 1
	   TB = 0
	   IF EDITAR = 0 THEN
	      FOR RST = RMAX + 1 TO R - 1
		  GET AREAF, RST, FACTURA
		  TB = TB + FACTURA.IMPORT
	      NEXT
	   ELSE
	      FOR RST = RS TO RMAX - 1
		  GET AREAF, RST, FACTURA
		  TB = TB + FACTURA.IMPORT
	      NEXT
	   END IF
	      
	   IF TB = 0 THEN LOCATE 20, 67: PRINT SPACE$(12) ELSE LOCATE 20, 67: PRINT FormatC$(TB, "#.###.###")
	   GOSUB CALCULAFACTURA
	   C = 2: GET AREAF, R, FACTURA
	   GOSUB CAMPS
	   RETURN

CALCULAFACTURA:
	   BASEIMP = TB - (TB * VAL(ValueCamp$(1))) / 100
	   IVA = BASEIMP * (VAL(ValueCamp$(0))) / 100
	   TN = BASEIMP + IVA
	   LOCATE 8, 50: PRINT "                             "
	   LOCATE 9, 50: PRINT "                             "
	   COLOR COL(2, 0), COL(2, 1): LOCATE 8, 50: PRINT "Base Imponible: "; : COLOR COL(0, 0), COL(0, 1): PRINT FormatC$(BASEIMP, "#.###.###"); "   "
	   COLOR COL(2, 0), COL(2, 1): LOCATE 9, 49: PRINT "Total Reparaci¢: "; : COLOR COL(0, 0), COL(0, 1): PRINT FormatC$(TN, "#.###.###"); "   "
	   RETURN

MOSTRALINIES:
	   FOR RSC = R TO RMAX - 2
	       GET AREAF, RSC, FACTURA
	       TB = TB + FACTURA.IMPORT
	       GOSUB CAMPS
	       GOSUB SALTALINIA
	   NEXT
	   R = RS: GET AREAF, R, FACTURA
	   RETURN
END SUB

FUNCTION FindClient% (CAMP$, INDEX() AS INDEXCLI, MAXCL) STATIC
	 SHARED RNDX
	 TOPRECORD = MAXCL
	 BottomRecord = 1

	 DO UNTIL (TOPRECORD < BottomRecord)
	    MIDPOINT = (TOPRECORD + BottomRecord) \ 2
	    TEST$ = RTRIM$(ForaEspai$(INDEX(MIDPOINT).CODI))
	    IF TEST$ = CAMP$ THEN
	       EXIT DO
	    ELSEIF CAMP$ > TEST$ THEN
	       BottomRecord = MIDPOINT + 1
	    ELSE
	       TOPRECORD = MIDPOINT - 1
	    END IF
	 LOOP
	 IF TEST$ = CAMP$ THEN
	    GET AREA3, INDEX(MIDPOINT).REGISTRE, CLIENTS
	    FindClient% = TRUE
	    RNDX = MIDPOINT
	 ELSE
	    FindClient% = FALSE
	 END IF
END FUNCTION

SUB IMPRIMIRRESGUARD (REG, DEVI$, AREA4, AREAR, AREAP, AREAF)
   SHARED DIRECCT$, CADFE$

   GetBackground 1, 1, 25, 80, prt$

   AREATXT = FREEFILE: OPEN DIRECCT$ + "RESGUARD.TXT" FOR OUTPUT SHARED AS AREATXT

   R = 0
   RMAX = LOF(AREAF) \ LEN(FACTURA)
   FOR RS = 1 TO RMAX
       FinestraEstat "Cercant l¡nies de la factura...", 0
       GET AREAF, RS, FACTURA
       IF FACTURA.RESGUARD = REG THEN
	  R = RS: EXIT FOR
       END IF
   NEXT

   IF R = 0 THEN
      tecla = Avis("AVIS:", "Aquest resguard no la factura feta, i no sortir… imprimida", "Pitja una tecla...", 0)
      MSG.G$ = "NULL"
   ELSE
      RMAX = RS + 6
      MSG.G$ = "IMPRIMEIX_LINIES"
   END IF

   PAG = 1
   GET AREA4, 1, CAP
   GET AREAP, 1, PP
   GET AREAR, REG, RESGUA           ' Carrega el registre a mem•ria

   GOSUB imprimeix

   CLOSE #AREATXT

   FinestraEstat "Imprimint fitxer del resguard...", 2
   ImprimeixFitxerTXT DIRECCT$ + "RESGUARD.TXT", DEVI$, 80

   PutBackground 1, 1, prt$
   EXIT SUB

'***************************************************************************
' Rutines d'impressi¢
'***************************************************************************

imprimeix:
    BOTA% = 5: GOSUB BOTA.LINIES
    PRINT #AREATXT, SPACE$(35) + RTRIM$(ARCHIMP.ENSANCHADO) + "Repar./Fact. comptat:" + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, SPACE$(43) + RTRIM$(ARCHIMP.ENSANCHADO) + RTRIM$(LTRIM$(RESGUA.ORDRE)) + RTRIM$(ARCHIMP.NOENSANCHADO): PRINT #AREATXT, "": PRINT #AREATXT, ""
    PRINT #AREATXT, SPACE$(33) + "Data factura: " + RESGUA.FETXAOUT: PRINT #AREATXT, "": PRINT #AREATXT, ""
    PRINT #AREATXT, SPACE$(15) + RESGUA.FETXAIN + SPACE$(5) + RESGUA.CODCLIENT + SPACE$(6) + RESGUA.PERSONA.NOM
    PRINT #AREATXT, SPACE$(44) + RESGUA.PERSONA.COGNOMS
    PRINT #AREATXT, "Aparell:             Marca:                 D.N.I. " + RESGUA.PERSONA.DNI
    PRINT #AREATXT, USING "\                  \ \                   \  \" + SPACE$(28) + "\"; RESGUA.APARELL; RESGUA.MARCA; RESGUA.PERSONA.DIRECCIO
    PRINT #AREATXT, SPACE$(44) + LTRIM$(RTRIM$(RESGUA.PERSONA.POBLACIO)) + " " + RESGUA.PERSONA.CPOSTAL
    PRINT #AREATXT, "Model:               N§ SŠrie:              Tels. " + RESGUA.PERSONA.TELEFON1
    PRINT #AREATXT, USING "\                   \\                  \         \              \"; RESGUA.MODELO; RESGUA.NSERIE; RESGUA.PERSONA.TELEFON2
    PRINT #AREATXT, "": PRINT #AREATXT, "": PRINT #AREATXT, ""
    PRINT #AREATXT, "Anomalia:  " + RESGUA.OBSERVACL(1)
    PRINT #AREATXT, "           " + RESGUA.OBSERVACL(2)
    PRINT #AREATXT, "           " + RESGUA.OBSERVACL(3)
    PRINT #AREATXT, STRING$(76, "Ä")

    IF MSG.G$ = "IMPRIMEIX_LINIES" THEN
       PRINT #AREATXT, "Quantitat             Descripci¢                     Preu           Import"
       PRINT #AREATXT, STRING$(76, "Ä")

       FOR L = 1 TO 6
	   GET AREAF, R, FACTURA
	   IF FACTURA.QUANTITAT = 0 THEN
	      QUANT$ = "         "
	   ELSE
	      QUANT$ = FormatD$(FACTURA.QUANTITAT, "##.###,0")
	   END IF

	   p$ = LTRIM$(RTRIM$(FormatD$(FACTURA.PREU, "#.###.###")))
	   I$ = LTRIM$(RTRIM$(FormatD$(FACTURA.IMPORT, "#.###.###")))
	   PREU$ = SPACE$(9 - LEN(p$)) + p$
	   IMPORT$ = SPACE$(9 - LEN(I$)) + I$

	   PRINT #AREATXT, USING "\       \ "; QUANT$;
	   PRINT #AREATXT, USING "\                                      \ "; FACTURA.CONCEPTE;
	   PRINT #AREATXT, USING "\         \ "; PREU$;
	   PRINT #AREATXT, USING "    \         \"; IMPORT$
	   R = R + 1
       NEXT

       PRINT #AREATXT, STRING$(76, "Ä"): BOTA% = 5: GOSUB BOTA.LINIES
       PRINT #AREATXT, "Subtotal DTO%           Base Imponible IVA%    Total IVA      Total Reparac."
       PRINT #AREATXT, USING "\     \"; FormatD$(FACTURA.TOTALBRUT, "###.###");
       PRINT #AREATXT, USING "  \ \"; FormatD$(FACTURA.DTO, "##");
       PRINT #AREATXT, USING SPACE$(12) + "\     \       "; FormatD$(FACTURA.BASEIMPONIBLE, "###.###");
       PRINT #AREATXT, USING " \ \     "; FormatD$(FACTURA.TIPOIVA, "##");
       PRINT #AREATXT, USING "\        \     \        \"; FormatD$(FACTURA.TOTALIVA, "###.###"); FormatD$(FACTURA.TOTALNET, "###.###");
    END IF

    PRINT #AREATXT, ""
    PRINT #AREATXT, ""
    PRINT #AREATXT, SPACE$(47) + RTRIM$(ARCHIMP.COMPRIMIDO) + RESGUA.OBSERVASE(1); RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    PRINT #AREATXT, SPACE$(47) + RTRIM$(ARCHIMP.COMPRIMIDO) + RESGUA.OBSERVASE(2); RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    PRINT #AREATXT, SPACE$(47) + RTRIM$(ARCHIMP.COMPRIMIDO) + RESGUA.OBSERVASE(3); RTRIM$(ARCHIMP.NOCOMPRIMIDO)
   
    FOR L = 1 TO 14: PRINT #AREATXT, "": NEXT
    RETURN

BOTA.LINIES:
    FOR L = 1 TO BOTA%: PRINT #AREATXT, "": NEXT
    RETURN
END SUB

SUB LLISTARESGUARDS (AREA4, AREAR, MAXRESG, DEVI$)
    SHARED DIRECCT$

    GetBackground 1, 1, 25, 80, L$
    SetMaxCamps 4
    SetInitCamp 0, 10, 26, ASCI, 0, "999999999", "Ordre Inici:"
    SetInitCamp 1, 11, 26, ASCI, 0, "999999999", "Ordre final:"
    SetInitCamp 2, 13, 26, ASCI, 0, "99/99/99", "Data Inici:"
    SetInitCamp 3, 14, 26, ASCI, 0, "99/99/99", "Data Final:"
    SetInitCamp 4, 16, 26, ASCI, 0, "XXXXXXXXXX", "Codi Client:"
    FOR C = 0 TO MAXCAMPS: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 8, 10, 17, 40, 1, CAIXA1
    DisplayAllCamps
    
    FOR C = 0 TO MAXCAMPS
	VALUE = ReadCamp(C)
	SELECT CASE VALUE
	       CASE SALIR
		    PutBackground 1, 1, L$
		    EXIT SUB
	       CASE 888, F1 TO F10
		    C = C - 1
	       CASE 0
		    IF VAL(ValueCamp$(0)) <= 0 THEN
		       INICI = 1: InsertValueCamp 0, " PRIMER ": DisplayAllCamps
		    ELSEIF VAL(ValueCamp$(0)) > MAXRESG THEN
			   tecla = Avis("ERROR:", "El nombre de resguards ‚s major", "Pitja una tecla...", 0)
			   INICI = 1: InsertValueCamp 0, " PRIMER ": DisplayAllCamps
		    ELSE
			   IF VAL(ValueCamp$(0)) = 1 THEN
			      INICI = 1: InsertValueCamp 0, " PRIMER ": DisplayAllCamps
			   ELSE
			      INICI = VAL(ValueCamp$(0))
			   END IF
		    END IF
	       CASE 1
		    IF VAL(ValueCamp$(1)) <= 0 THEN
		       FIN = MAXRESG - 1: InsertValueCamp 1, " DARRER ": DisplayAllCamps
		    ELSE
		       IF VAL(ValueCamp$(1)) > MAXRESG THEN
			  tecla = Avis("ERROR:", "El nombre de resguards ‚s major", "Pitja una tecla...", 0)
			  FIN = MAXRESG - 1: InsertValueCamp 1, " DARRER ": DisplayAllCamps
		       ELSE
			  IF VAL(ValueCamp$(1)) >= 1 THEN FIN = VAL(ValueCamp$(1))
		       END IF
		    END IF

	       CASE 2
		    IF ValueCamp$(2) <> "  /  /  " THEN
		       DATAINIC$ = ValueCamp$(2)
		    ELSE
		       InsertValueCamp 2, " PRIMER "
		       DATAINIC$ = "P"
		    END IF
		    DisplayAllCamps
	       CASE 3
		    IF ValueCamp$(3) <> "  /  /  " THEN
		       DATAFINA$ = ValueCamp$(2)
		    ELSE
		       InsertValueCamp 3, " DARRER "
		       DATAFINA$ = "F"
		    END IF
		    DisplayAllCamps
	       CASE 4
		    IF LTRIM$(RTRIM$(ValueCamp$(4))) = "" THEN
		       InsertValueCamp 4, "  TOTS  ": DisplayAllCamps
		       CODIC$ = "TOTS"
		    ELSE
		       CODIC$ = ValueCamp$(4)
		    END IF
	       CASE ELSE
	END SELECT
    NEXT
    
    AREATXT = FREEFILE
    OPEN DIRECCT$ + "RESGUARS.TXT" FOR OUTPUT AS AREATXT

    PAG = 1: L = 1
    GET AREA4, 1, CAP   ' AGAFAR CAP€ALERA DEL FITXER DE CAP€ALERES

    GOSUB CAP.LIST     ' IMPRIMIR CAP€ALERA

    ' DEFINIR MASCARA DE LES LINIES
							   
    MASCARA$ = " ³\" + SPACE$(8) + "\³\" + SPACE$(38) + "\³\      \³\      \³\" + SPACE$(18) + "\³\" + SPACE$(18) + "\³"

    MAXL = 54: TOTALB& = 0: TOTALN& = 0
    MAXF# = MAXRESG - 1

    FOR RL! = INICI TO FIN
	GET AREAR, RL!, RESGUA
	IF CODIC$ <> "TOTS" THEN
	   IF RTRIM$(LTRIM$(RESGUA.CODCLIENT)) = RTRIM$(LTRIM$(CODIC$)) THEN
	      IF RESGUA.MARCAT <> "*" THEN GOSUB PRINTLINIA
	   END IF
	ELSE
	   IF RESGUA.MARCAT <> "*" THEN GOSUB PRINTLINIA
	END IF

	IF L >= MAXL THEN
	   L = 1: PAG = PAG + 1
	   GOSUB LINIAFINAL
	   PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
	   PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
	   GOSUB CAP.LIST
	END IF
    NEXT

    GOSUB LINIAFINAL
    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT

    CALL ImprimeixFitxerTXT(DIRECCT$ + "RESGUARS.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP€ALERA DELS LLISTATS
' **************************************************************************

CAP.LIST:
    PRINT #AREATXT, " "; RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, " LLISTAT TOTAL DELS RESGUARS"
    PRINT #AREATXT, " P…gina:"; PAG
    PRINT #AREATXT, " Data..: "; FormatD$(Now#, "dd-mm-yyyy")
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""

     '                                    1         2         3         4         5
     '                           12345678901234567890123456789012345678901234567890
    PRINT #AREATXT, " ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
    PRINT #AREATXT, " ³N§Ordre   ³Nom del client                          ³Data In ³Data Out³Aparell             ³Marca               ³"
    PRINT #AREATXT, " ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
    RETURN

PRINTLINIA:
    NOMPCLIENT$ = LTRIM$(RTRIM$(RESGUA.PERSONA.NOM)) + " " + LTRIM$(RTRIM$(RESGUA.PERSONA.COGNOMS))
    IF NOMPCLIENT$ = " " THEN
       NOMPCLIENT$ = SPACE$(52)
    END IF
    PRINT #AREATXT, USING MASCARA$; RESGUA.ORDRE; NOMPCLIENT$; RESGUA.FETXAIN; RESGUA.FETXAOUT; RESGUA.APARELL; RESGUA.MARCA
    L = L + 1
    RETURN

LINIAFINAL:
       PRINT #AREATXT, " ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
       RETURN

END SUB

SUB MantenimentResguards (DTMP$, DIRECCP$, DIRECC$, DIRECI$, DP$, DEVI$, IMPRESORA) STATIC
    SHARED RNDX, DIRECCT$, CADFE$

    DIM MENU(4) AS STRING
    DIM COMMENT$(4, 4)
    ON ERROR GOTO ERRORS
    DIRECCT$ = DP$ + "TEXTOS\"
    COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "  OBRINT..."
    GOSUB OBRIRFITXERS

    MAXCB = LOF(AREAB) \ LEN(BORRREG)
    IF MAXCL = 0 OR MAXCL = 1 THEN MAXCL = 1
    DIM INDEX(1 TO MAXCL) AS INDEXCLI
    DIM NDXCLIENT AS INDEXCLI

    IF MAXCB >= 100 THEN GOSUB FILTRAR
    COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT "C. INDEX..."
    GOSUB INDEXAR

    SetScoreBoard SON
    IF MAXRE = 1 OR MAXRE = 0 THEN
       X = 7: R = 1: L = 1
       BEEP: tecla = Avis("AVIS:", "El fitxer de resguards est… buit.", "Pitja una tecla...", 0)
       GOSUB INSERTARESG
       IF ANUL = 1 THEN GOSUB TANCA
    END IF

    MASCREGS                                  ' PINTA LA MASCARA DE LA LLISTA
    X = 7: R = 1: L = 1
	 
    ' ********* MOSTRA ELS REGISTRES QUE CABIGEN EN UNA PANTALLA
    RESTA = 0: GOSUB LISTA
    X = 7: R = 1: L = 1
    GET AREAR, R, RESGUA
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR

    DO
      DO
	OP$ = INKEY$
	COLOR COL(0, 0), COL(0, 1): EstatTeclesControl 25, 3
	LOCATE 25, 70, 0: COLOR COL(1, 0), COL(1, 1): PRINT TIME$;
      LOOP UNTIL OP$ <> ""

      SELECT CASE OP$
	     CASE CHR$(0) + "P"              ' BAIXA UN REGISTRE
		  GOSUB BAIXA
	     CASE CHR$(0) + "H"              ' PUJA UN REGISTRE
		  GOSUB PUJA
	     CASE CHR$(27)
		  SetScoreBoard SOFF
		  COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 42, 1, CAIXA1
		  COLOR 31, COL(0, 1): LOCATE 12, 31: PRINT " TANCANT..."
		  GOSUB TANCA
	     CASE CHR$(13)
		  GOSUB EDITARCLIENT         ' EDITAR RESGUARD
	     CASE CHR$(0) + CHR$(60)
		  GOSUB INSERTARESG          ' INSERTAR RESGUARD
	     CASE CHR$(0) + CHR$(61)
		  IF MAXRE > 1 THEN MISS$ = " resguards " ELSE MISS$ = " resguard "
		  tecla = Avis("Saps Que:", "Hi ha " + LTRIM$(STR$(MAXRE - 1)) + MISS$ + "dins la base de dades = " + LTRIM$(STR$(LOF(AREAR))) + " Octets", "Pitja una tecla...", 0)
	     CASE CHR$(0) + CHR$(62)
		  GOSUB CONSULTARCLIENT      ' CONSULTAR CLIENT
	     CASE CHR$(0) + CHR$(63)
		  GOSUB DELETERECORD         ' MARCAR REGISTRE
	     CASE CHR$(0) + CHR$(64)
		  LLISTARESGUARDS AREA4, AREAR, MAXRE, DEVI$
	     CASE CHR$(0) + CHR$(65)
		  CALL IMPRIMIRRESGUARD(R, DEVI$, AREA4, AREAR, AREAP, AREAF)
	     CASE CHR$(0) + "Q"
		  GOSUB BOTA.AVALL
	     CASE CHR$(0) + "I"
		  GOSUB BOTA.AMUNT
	     CASE ELSE
		  LOCATE , , 0
      END SELECT
   LOOP
   RETURN

BOTA.AVALL:
   IF R = MAXRE - 1 THEN RETURN
   GOSUB ANAR.TOPE
   S = 1
   DO
	S = S + 1
	IF RB + S > MAXRE - 1 THEN
	   WHILE (RB + S > MAXRE - 1)
		 S = S - 1
	   WEND
	   EXIT DO
	END IF
   LOOP UNTIL S = 13
   RB = RB + S: R = RB: RESTA = 0: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREAR, RB, RESGUA: X = XB
   COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
   R = RB
   RETURN

BOTA.AMUNT:
   IF R = 1 THEN RETURN
   GOSUB ANAR.TOPE
   S = 1
   DO
	S = S + 1
	IF RB - S < 1 THEN
	   WHILE (RB - S < 1)
		 S = S - 1
	   WEND
	   EXIT DO
	END IF
   LOOP UNTIL S = 13
   RB = RB - S: R = RB: RESTA = 0: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREAR, RB, RESGUA: X = XB
   COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
   R = RB
   RETURN

ANAR.TOPE:
   XB = X: RB = R         ' Guarda l'estat actual
   DO UNTIL XB = 7
       XB = XB - 1        ' Resta l'estat actual fins arribar al tope del recuadre
       RB = RB - 1
   LOOP
   RETURN

'************************************************************************
' Control del cursor per el llistat amb despla‡ament de barres
' funci¢ parescuda al DBEDIT de Clipper.
'************************************************************************

PUJA:
       IF X = 7 THEN
	  GET AREAR, R, RESGUA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = 1 THEN
	     X = 7: R = 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 7
	  ScrollDown 18, 78, 6, 1, 1
	  GET AREAR, R, RESGUA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREAR, R, RESGUA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  GET AREAR, R, RESGUA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXA:
       IF X = 19 THEN
	  GET AREAR, R, RESGUA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXRE - 1 THEN
	     R = MAXRE - 1: X = 19
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = 19
	  ScrollUp 18, 78, 6, 1, 1
	  GET AREAR, R, RESGUA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREAR, R, RESGUA
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXRE - 1 THEN
	     R = MAXRE - 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  GET AREAR, R, RESGUA
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

'************************************************************************
' Mostra el cursor a la posici¢ que est…
'************************************************************************

SHOWCURSOR:
	  LOCATE X, 2: PRINT RESGUA.ORDRE; "³"; RESGUA.PERSONA.NOM; "³"; RESGUA.PERSONA.COGNOMS; "³"; RESGUA.FETXAIN; "³  "; RESGUA.MARCAT; "   "
	  RETURN

'************************************************************************
' CONSUTAR REGISTRES UN PER UN MOSTRANT LA FITXA DEL CLIENT
'************************************************************************

CONSULTARCLIENT:
	  GetBackground 1, 1, 24, 79, RESBUF$
	  GOSUB INITCAMPS: SetMaxCamps 13: RV = R
	  GET AREAR, R, RESGUA
	  GOSUB MASCARA: GOSUB MOSTRA
	  DO
	    T$ = INKEY$
	    COLOR COL(0, 0), COL(0, 1): EstatTeclesControl 25, 3
	    SELECT CASE T$
		   CASE CHR$(0) + "P"      ' BAIXA UN REGISTRE
			RV = RV + 1: GOSUB MOSTRA
		   CASE CHR$(0) + "H"      ' PUJA UN REGISTRE
			RV = RV - 1: GOSUB MOSTRA
		   CASE CHR$(27)
			DeleteWindow 0: PackWindows
			FOR C = 0 TO 13: DeleteCamp C: NEXT
			RETURN
		   CASE ELSE
	    END SELECT
	  LOOP

MOSTRA:
	  IF RV <= 1 THEN RV = 1
	  IF RV >= MAXRE - 1 THEN RV = MAXRE - 1
	  GET AREAR, RV, RESGUA: GOSUB MOURECAMPS
	  LOCATE , , 0: DisplayAllCamps: LOCATE , , 0

	  IF RESGUA.MARCAT = "*" THEN
	     LOCATE 4, 40: COLOR 28, COL(0, 1): PRINT "<< ESTA DE BAIXA >>            "
	  ELSE
	     IF RESGUA.REGALB = 0 AND RTRIM$(LTRIM$(RESGUA.ALBARAN)) = "" THEN
		LOCATE 4, 40: COLOR 28, COL(0, 1): PRINT "                               "
	     ELSE
		LOCATE 4, 40: COLOR 28, COL(0, 1): PRINT "<< ESTA FACTURAT ->"; : COLOR COL(0, 0)
		PRINT RESGUA.ALBARAN; : COLOR 28, COL(0, 1): PRINT ">>"
	     END IF
	  END IF
	  RETURN

'************************************************************************
' INSERTAR RESGUARD
'************************************************************************

INSERTARESG:
      GetBackground 1, 1, 24, 79, RESBUF$

      DO
	 ' Comprova si hi ha registres borrats

	 COTE = 0
	 IF MAXRE > 1 THEN
	  MAXCB = LOF(AREAB) \ LEN(BORRREG)
	  TROBAT$ = "XXX"
	  IF MAXCB = 0 THEN
	     T$ = "   "
	  ELSE
	     EXIST$ = "N": INIR = 0
	     FOR F = 1 TO MAXCB
		 GET AREAB, F, BORRREG
		 IF BORRREG.REGISTRE <> 0 AND BORRREG.USAT = "F" THEN
		    EXIST$ = "S"
		    EXIT FOR
		 END IF
	     NEXT
	     IF EXIST$ = "N" THEN
		T$ = "   "
	     ELSE
		IF EXIST$ = "S" THEN
		   T$ = "ZZZ": RR = BORRREG.REGISTRE: TROBAT$ = "ZZZ"
		   GET AREAR, BORRREG.REGISTRE, RESGUA
		   REF$ = RESGUA.ORDRE
		   BORRREG.USAT = "U"       ' Avis de que est… utilitzat
		   PUT AREAB, F, BORRREG
		END IF
	     END IF
	  END IF
	  
	  IF T$ = "ZZZ" THEN
	     RNOU = RR              ' SI N'HA TROBAT QUE L'EDITI D'AMUNT
	  ELSE
	     RNOU = MAXRE           ' SI NO N'HA TROBAT QUE N'AFEGESQUI UN
	  END IF

	     RV = RNOU
	  ELSE
	     RV = MAXRE: RNOU = MAXRE
	     REF$ = ""
	  END IF
	  
	  GOSUB MASCARA                     ' Pinta la fitxa del resguard
	  GOSUB MOSTRA.DARRER               ' Mostra el codi del darrer client creat                             
	  SetMaxCamps 13
	  
	  GOSUB INITCAMPS: COLOR 15, 9      ' Inicialitza els camps
	  GOSUB BUIDARCAMPS                 ' Buida els camps

	  InsertValueCamp 6, FETXA$
	  GOSUB READCAMPS                   ' Dona pas a que l'usuari pugui entrar les dades.
	  GOSUB MOUREFITXER                 ' Mou les dades entrades a l'estructura de dades
	  LOCATE , , 0: DisplayAllCamps

	  IF ANUL = 1 THEN                  ' Si s'ha pitjat <ESC> mentre s'entraven dades
	     IF EXIST$ = "S" THEN
		BORRREG.USAT = "F"          ' Avis de que ja no est… utilitzat
		PUT AREAB, F, BORRREG
	     END IF
	     PutBackground 1, 1, RESBUF$
	     FOR C = 0 TO 13: DeleteCamp C: NEXT
	     RETURN
	  END IF
	  
	  PUT AREAR, RNOU, RESGUA          ' Si no s'ha anulat guarda els valors

	  MSG.G$ = ""
	  IF RNOU = MAXRE THEN             ' SI EL REGISTRE NOU ES IGUAL AL NOMBRE M…XIM DE CLIENTS
	     GET AREAR, 1, RESGUA
	     RESGUA.CPY = "Fitxer dels RESGUARDS. Facturaci¢ 3.0" + CHR$(26)
	     RESGUA.MAXREG = MAXRE + 1
	     PUT AREAR, 1, RESGUA
	     MAXRE = MAXRE + 1
	     GET AREAD, 1, DOCNUM
	     DOCNUM.MAXRESG = DOCNUM.MAXRESG + 1
	     PUT AREAD, 1, DOCNUM
	     MSG.G$ = "CREA_FACTURA"
	  END IF

	  IF EXIST$ = "S" THEN
	     BORRREG.REGISTRE = 0
	     BORRREG.USAT = CHR$(0)
	     PUT AREAB, F, BORRREG
	  END IF

	GetBackground 7, 5, 22, 80, FACT$

	IF MSG.G$ = "CREA_FACTURA" THEN            ' Comprova si la factura s'ha d'editar o crear
	   CALL FacturaResguard(AREAF, 0, RNOU)
	ELSE
	   CALL FacturaResguard(AREAF, 1, RNOU)
	END IF

	DO
	    MENU(1) = "~Imprimir resguard"
	    MENU(2) = "~Continuar creant "
	    MENU(3) = "~Sortir           "
	    CALL MenuBar(MENU(), COMMENT$(), 0, OPCIO%, 10, 10, 20, 3, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
	
	    SELECT CASE OPCIO%
		   CASE 1
			CALL IMPRIMIRRESGUARD(RNOU, DEVI$, AREA4, AREAR, AREAP, AREAF)
		   CASE 2
			COTE = 0: EXIT DO
		   CASE 3
			COTE = 888: EXIT DO
		   CASE ELSE
			SOUND 50, .5
	    END SELECT
	LOOP
	PutBackground 7, 5, FACT$
      LOOP UNTIL COTE = 888

      PutBackground 1, 1, RESBUF$
      COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR: FOR C = 0 TO 13: DeleteCamp C: NEXT
      RETURN

'************************************************************************
' EDITAR CLIENT
'************************************************************************

EDITARCLIENT:
	  GetBackground 2, 10, 21, 77, S$
	  RV = R: GOSUB MASCARA
	  LOCK AREAR, RV
	  GET AREAR, R, RESGUA
	  SetMaxCamps 13
	  GOSUB INITCAMPS
	  GOSUB MOURECAMPS
	  LOCATE , , 0: COLOR 15, 9: CALL DisplayAllCamps: EDIT = 99
	  GOSUB READCAMPS
	  EDIT = 0
	  IF ANUL = 1 THEN           ' SI ES PITJA <ESC> A UN DELS CAMPS
	     PutBackground 2, 10, S$
	     GOSUB BARRACONTROL
	     UNLOCK AREAR, RV
	     FOR C = 0 TO 13: DeleteCamp C: NEXT
	     RETURN
	  END IF
	  GOSUB MOUREFITXER          ' MOURE LES DADES DELS CAMPS AL FITXER
	  PUT AREAR, RV, RESGUA      ' GRAVA DINS EL FITXER
	  GetBackground 7, 5, 22, 80, FACT$
	  CALL FacturaResguard(AREAF, 1, RV)
	  PutBackground 7, 5, FACT$
	  

TORNAR:
	  FOR C = 0 TO 13: DeleteCamp C: NEXT
	  PutBackground 2, 10, S$
	  GOSUB BARRACONTROL
	  UNLOCK AREAR, RV
	  GOSUB TANCAR.TEMPORAL
	  RETURN


'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

READCAMPS:
       IF EDIT = 0 THEN GOSUB INIT1
       SetMaxCamps 13: OLDVALUE$ = SPACE$(10)
       GetBackground 2, 10, 21, 77, TELBUF$
       GOSUB AJUDA
       CORR = 0
       DO
	 BUSCA$ = ""
	 FOR C = 0 TO 13
	     VALUE = ReadCamp(C)
	     SELECT CASE VALUE
		    CASE 0
			 CAMPTEMP$ = ValueCamp$(0)
			 XXX$(0) = ValueCamp$(0)
			 IF FindClient%(ForaEspai$(CAMPTEMP$), INDEX(), MAXCL) THEN
			    ZZ$ = "ZZZ": BUSCA$ = "XXX"
			    REGCLI = INDEX(MIDPOINT).REGISTRE
			    GET AREA3, REGCLI, CLIENTS
			    IF CLIENTS.MARCAT = "*" THEN
			       OLDVALUE$ = ValueCamp$(0): GOSUB INSERTARCLIENT
			       GOSUB INITCAMPS
			       GOSUB BUIDARCAMPS
			       GOSUB MOSTRA.DARRER
			       GOTO READCAMPS
			       ZZ$ = "": BUSCA$ = ""
			    END IF
			 ELSE
			    OLDVALUE$ = ValueCamp$(0): GOSUB INSERTARCLIENT
			    GOSUB INITCAMPS: GOTO READCAMPS
			    ZZ$ = "": BUSCA$ = ""
			 END IF
			 GOSUB SEEKD
		    CASE F1
			 C = C - 1
		    CASE F2
			 IF C = 0 THEN
			    COLOR COL(0, 0), COL(0, 1)
			    FINESTRA 10, 30, 13, 60, 1, CAIXA1
			    LOCATE 11, 31: PRINT "TelŠfon del client:"
			    DO
			      SetMaxCamps 0: SetInitCamp 0, 12, 31, ASCI, 0, STRING$(18, "X"), ""
			      SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
			      IF ReadCamp(0) = SALIR THEN
				 PutBackground 2, 10, TELBUF$
				 SetMaxCamps 13
				 SetInitCamp 0, 5, 29, ASCI, 0, STRING$(10, "X"), ""   ' CODI CLIENT
				 GOTO READCAMPS
			      END IF: RT = 0
			      GetBackground 1, 1, 24, 80, T$
			      FOR R = 1 TO MAXCL
				  CALL FinestraEstat("Cercant TelŠfon...", 0)
				  GET AREA3, R, CLIENTS
				  C.CF$ = RTRIM$(LTRIM$(ValueCamp$(0)))
				  C.TL$ = RTRIM$(LTRIM$(CLIENTS.TELEFON1)): C.TL2$ = RTRIM$(LTRIM$(CLIENTS.TELEFON2))
				  IF C.CF$ = C.TL$ OR C.CF$ = C.TL2$ AND CLIENTS.MARCAT <> "*" THEN
				     RT = R: BUSCA$ = "9"
				     CODCLI$ = CLIENTS.CODICLIENT
				     EXIT FOR
				  END IF
			      NEXT
			      PutBackground 1, 1, T$
			      IF RT = 0 THEN
				 BEEP: tecla = Avis("AVIS:", "Aquest client no existeix", "Pitja una tecla per a continuar...", 0)
			      END IF
			      ZZ$ = ""
			    LOOP UNTIL RT > 0

			    IF BUSCA$ = "9" THEN
			       REGCLI = RT
			       InsertValueCamp 0, CODCLI$
			       SetMaxCamps 13
			       SetInitCamp 0, 5, 29, ASCI, 0, STRING$(10, "X"), ""   ' CODI CLIENT
			       PutBackground 2, 10, TELBUF$
			    ELSEIF BUSCA$ = "XXX" THEN
			       REGCLI = INDEX(MIDPOINT).REGISTRE
			    END IF
			    GET AREA3, REGCLI, CLIENTS
			    LOCATE 6, 29: PRINT STRING$(47, " ")
			    LOCATE 6, 29: PRINT LTRIM$(RTRIM$(CLIENTS.NOM)) + " " + RTRIM$(LTRIM$(CLIENTS.COGNOMS))
			    InsertValueCamp 0, CODCLI$
			    DisplayAllCamps
			    SetMaxCamps 13
			    ANUL = 0
			 ELSE
			    C = C - 1
			    BEEP
			 END IF
		     CASE F3
			 GOSUB LLISTA.CLIENTS
		     CASE F4 TO F10
			 C = C - 1
		     CASE 999
			 ANUL = 1: RETURN
		     CASE ELSE
	     END SELECT
	 NEXT

	  COLOR 27: CENTRAR 19, "Son correctes aquestes dades (S/N) ?"
	  COLOR COL(0, 0), COL(0, 1): CORR = 0
	  DO
	   DO
	     D$ = INKEY$
	   LOOP UNTIL D$ <> ""
	   IF UCASE$(D$) = "S" THEN
	      CORR$ = "SORTIR"
	      EXIT DO
	   ELSE
	      IF UCASE$(D$) = "N" THEN
		 CORR$ = "MODIFICAR"
		 EXIT DO
	      END IF
	   END IF
	  LOOP
	  CENTRAR 19, "                                             "
       LOOP UNTIL CORR$ = "SORTIR"
       RETURN

SEEKD:
       IF BUSCA$ = "9" THEN
	  REGCLI = RT
	  InsertValueCamp 0, CLIENTS.CODICLIENT
	  SetMaxCamps 13
	  PutBackground 2, 10, TELBUF$
       ELSEIF BUSCA$ = "XXX" THEN
	  REGCLI = INDEX(MIDPOINT).REGISTRE
       END IF
       GET AREA3, REGCLI, CLIENTS
       LOCATE 6, 29: PRINT STRING$(47, " ")
       LOCATE 6, 29: PRINT LTRIM$(RTRIM$(CLIENTS.NOM)) + " " + RTRIM$(LTRIM$(CLIENTS.COGNOMS))
       InsertValueCamp 0, CLIENTS.CODICLIENT
       SetMaxCamps 13
       ANUL = 0
       RETURN

'************************************************************************
' INICIALITZA CAMPS
'************************************************************************

INIT1:
	IF TROBAT$ = "XXX" THEN
	     GET AREAD, 1, DOCNUM
	     REF$ = DOCNUM.NUMREGS: ALB$ = LTRIM$(STR$(DOCNUM.MAXRESG)): L2 = 8
	     FOR L = LEN(ALB$) TO 1 STEP -1
		 MID$(REF$, L2, 1) = MID$(ALB$, L, 1): L2 = L2 - 1
	     NEXT
	 END IF
	 InsertValueCamp 1, REF$
	 InsertValueCamp 6, FormatD$(Now#, "dd/mm/yyyy")
	 LOCATE , , 0: DisplayAllCamps
	 RETURN

INITCAMPS:
	  FOR C = 0 TO 13: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
	  FOR C = 0 TO 13: DeleteCamp C: NEXT
	  SetInitCamp 0, 5, 29, ASCI, 0, STRING$(10, "X"), ""   ' CODI CLIENT
	  SetInitCamp 1, 5, 63, ASCI, 0, STRING$(10, "X"), ""   ' N§ D'ORDRE
	  SetInitCamp 2, 8, 17, ASCI, 0, STRING$(20, "X"), ""   ' MARCA
	  SetInitCamp 3, 8, 54, ASCI, 0, STRING$(20, "X"), ""   ' MODEL
	  SetInitCamp 4, 9, 23, ASCI, 0, STRING$(20, "X"), ""   ' SERIE
	  SetInitCamp 5, 9, 56, ASCI, 0, STRING$(20, "X"), ""   ' APARELL
	  SetInitCamp 6, 10, 24, ASCI, 0, "99/99/9999", ""    ' DATA IN
	  SetInitCamp 7, 10, 61, ASCI, 0, "99/99/9999", ""     ' DATA OUT
	  SetInitCamp 8, 12, 11, ASCI, 0, STRING$(50, "X"), ""  ' OBSERVACIONS CLIENT
	  SetInitCamp 9, 13, 11, ASCI, 0, STRING$(50, "X"), ""
	  SetInitCamp 10, 14, 11, ASCI, 0, STRING$(50, "X"), ""
	  SetInitCamp 11, 16, 11, ASCI, 0, STRING$(50, "X"), ""' OBSERVACIONS SERVEI
	  SetInitCamp 12, 17, 11, ASCI, 0, STRING$(50, "X"), ""
	  SetInitCamp 13, 18, 11, ASCI, 0, STRING$(50, "X"), ""
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPS:
	  LOCATE 6, 29: PRINT STRING$(47, " ")
	  COLOR COL(0, 0): LOCATE 6, 29: PRINT LTRIM$(RTRIM$(RESGUA.PERSONA.NOM)) + " " + RTRIM$(LTRIM$(RESGUA.PERSONA.COGNOMS))
	  InsertValueCamp 0, RESGUA.CODCLIENT       ' CODI CLIENT
	  InsertValueCamp 1, RESGUA.ORDRE           ' N§ D'ORDRE
	  InsertValueCamp 2, RESGUA.MARCA           ' MARCA
	  InsertValueCamp 3, RESGUA.MODELO          ' MODEL
	  InsertValueCamp 4, RESGUA.NSERIE          ' SERIE
	  InsertValueCamp 5, RESGUA.APARELL         ' APARELL
	  InsertValueCamp 6, RESGUA.FETXAIN         ' DATA IN
	  InsertValueCamp 7, RESGUA.FETXAOUT        ' DATA OUT
	  InsertValueCamp 8, RESGUA.OBSERVACL(1)    ' OBSERVACIONS CLIENT
	  InsertValueCamp 9, RESGUA.OBSERVACL(2)
	  InsertValueCamp 10, RESGUA.OBSERVACL(3)
	  InsertValueCamp 11, RESGUA.OBSERVASE(1)   ' OBSERVACIONS SERVEI
	  InsertValueCamp 12, RESGUA.OBSERVASE(2)
	  InsertValueCamp 13, RESGUA.OBSERVASE(3)
	  RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXER:
	   RESGUA.CPY = "" + CHR$(26)
	   RESGUA.PERSONA.CODICLIENT = CLIENTS.CODICLIENT
	   RESGUA.PERSONA.NOM = CLIENTS.NOM
	   RESGUA.PERSONA.COGNOMS = CLIENTS.COGNOMS
	   RESGUA.PERSONA.DIRECCIO = CLIENTS.DIRECCIO
	   RESGUA.PERSONA.DNI = CLIENTS.DNI
	   RESGUA.PERSONA.PROVINCIA = CLIENTS.PROVINCIA
	   RESGUA.PERSONA.POBLACIO = CLIENTS.POBLACIO
	   RESGUA.PERSONA.TELEFON1 = CLIENTS.TELEFON1
	   RESGUA.PERSONA.TELEFON2 = CLIENTS.TELEFON2
	   RESGUA.PERSONA.DTO = CLIENTS.DTO
	   RESGUA.PERSONA.FORMAPAGO = CLIENTS.FORMAPAGO
	   RESGUA.PERSONA.CPOSTAL = CLIENTS.CPOSTAL
	   RESGUA.PERSONA.BANC = CLIENTS.BANC
	   RESGUA.PERSONA.COMPTE = CLIENTS.COMPTE
	   RESGUA.PERSONA.MARCAT = CLIENTS.MARCAT
	   RESGUA.CODCLIENT = ValueCamp$(0)
	   RESGUA.REGALB = 0
	   RESGUA.ALBARAN = ""
	   RESGUA.ORDRE = ValueCamp$(1)
	   RESGUA.MARCA = ValueCamp$(2)
	   RESGUA.MODELO = ValueCamp$(3)
	   RESGUA.NSERIE = ValueCamp$(4)
	   RESGUA.APARELL = ValueCamp$(5)
	   RESGUA.FETXAIN = ValueCamp$(6)
	   RESGUA.FETXAOUT = ValueCamp$(7)
	   FOR LO = 1 TO 3: RESGUA.OBSERVACL(LO) = ValueCamp$(LO + 7): NEXT
	   FOR LO = 1 TO 3: RESGUA.OBSERVASE(LO) = ValueCamp$(LO + 10): NEXT
	   RESGUA.MARCAT = "-"
	   FOR C = 0 TO 13: DeleteCamp C: NEXT
	   RETURN

'************************************************************************
' BUIDAR ELS CAMPS DEL FITXER
'************************************************************************
BUIDARCAMPS:
	   RESGUA.CODCLIENT = STRING$(LEN(CLIENTS.CODICLIENT), CHR$(32))
	   RESGUA.CPY = ""
	   RESGUA.REGALB = 0
	   RESGUA.ALBARAN = ""
	   RESGUA.ORDRE = ""
	   RESGUA.MARCA = ""
	   RESGUA.MODELO = ""
	   RESGUA.NSERIE = ""
	   RESGUA.APARELL = ""
	   RESGUA.FETXAIN = FETXA$
	   RESGUA.FETXAOUT = "  /  /  "
	   FOR LO = 1 TO 3: RESGUA.OBSERVACL(LO) = "": NEXT
	   FOR LO = 1 TO 3: RESGUA.OBSERVASE(LO) = "": NEXT
	   RESGUA.MARCAT = "-"
	   FOR C = 0 TO 13: DeleteCamp C: NEXT
	   RETURN


'************************************************************************
' LLISTA UN BLOC
'************************************************************************


LISTA:
    COLOR COL(0, 0), COL(0, 1)
    FOR X = 7 TO 19
	COLOR COL(0, 0), COL(0, 1)
	LOCATE X, 2: PRINT SPACE$(LEN(RESGUA.ORDRE)); "³"; SPACE$(LEN(RESGUA.PERSONA.NOM)); "³"; SPACE$(LEN(RESGUA.PERSONA.COGNOMS)); "³";
	PRINT SPACE$(LEN(RESGUA.FETXAIN)); "³  "; SPACE$(LEN(RESGUA.MARCAT)); "   "
    NEXT
    IF RESTA = 0 THEN a = 7: B = 19: C = 1 ELSE a = 19: B = 7: C = -1
    FOR X = a TO B STEP C
       IF R >= MAXRE OR R < 1 THEN EXIT FOR
       GET AREAR, R, RESGUA
       GOSUB SHOWCURSOR
       IF RESTA = 0 THEN R = R + 1 ELSE R = R - 1
    NEXT
    RETURN

'************************************************************************
' MARCAR UN REGISTRE COM A BORRAT
'************************************************************************

DELETERECORD:
    GET AREAR, R, RESGUA              ' CARREGAR EL REGISTRE DINS MEMORIA
    MAXCB = LOF(AREAB) \ LEN(BORRREG)
    IF MAXCB = 0 THEN
       IF RESGUA.MARCAT = "-" THEN
	  BORRREG.REGISTRE = R
	  BORRREG.USAT = "F"
	  PUT AREAB, MAXCB + 1, BORRREG
       END IF
    ELSE
	  EXIST$ = "N": INIR = 0
	  FOR F = 1 TO MAXCB
	      GET AREAB, F, BORRREG
	      IF INIR = 0 AND BORRREG.REGISTRE = 0 THEN
		 INIR = F
	      END IF
	      IF BORRREG.REGISTRE = R THEN EXIST$ = "S": EXIT FOR
	  NEXT
	  IF EXIST$ = "N" THEN
	     IF INIR <> 0 THEN
		BORRREG.REGISTRE = R
		BORRREG.USAT = "F"
		PUT AREAB, INIR, BORRREG
	     ELSE
		MAXCB = MAXCB + 1
		BORRREG.REGISTRE = R
		BORRREG.USAT = "F"
		PUT AREAB, MAXCB, BORRREG
	     END IF
	  ELSE
	     IF EXIST$ = "S" THEN
		IF BORRREG.USAT = "U" THEN
		   tecla = Avis("AVIS:", "El registre ara est… sent utilitzat i no es pot borrar", "Pitji una tecla per anular...", 0)
		   RETURN
		ELSE
		   BORRREG.REGISTRE = 0
		   BORRREG.USAT = CHR$(0)
		   PUT AREAB, F, BORRREG
		END IF
	     END IF
	  END IF
    END IF

    IF RESGUA.MARCAT = "*" THEN       ' COMPROVAR SI ESTA MARCAT
       RESGUA.MARCAT = "-"
       PUT AREAR, R, RESGUA
    ELSE                               ' SI NO ESTA MARCAT QUE EL MARQUI
      IF RESGUA.MARCAT <> "*" THEN
	 RESGUA.MARCAT = "*"
	 PUT AREAR, R, RESGUA
      END IF
    END IF
    COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR    ' TORNA A VISUALITZAR LA LINIA
    RETURN



'************************************************************************
' INSERTAR CLIENT
'************************************************************************

INSERTARCLIENT:
	  MAXCLIB = LOF(AREABC) \ LEN(BORRCLI)
	  TROBATA$ = "XXX"
	  IF MAXCLIB = 0 THEN
	     TA$ = "   "
	  ELSE
	     EXISTA$ = "N": INIRA = 0
	     FOR FA = 1 TO MAXCLIB
		 GET AREABC, FA, BORRCLI
		 IF BORRCLI.REGISTRE <> 0 AND BORRCLI.USAT = "F" THEN
		    EXISTA$ = "S"
		    EXIT FOR
		 END IF
	     NEXT
	     IF EXISTA$ = "N" THEN
		TA$ = "   "
	     ELSE
		IF EXISTA$ = "S" THEN
		   TA$ = "ZZZ": RRA = BORRCLI.REGISTRE: TROBATA$ = "ZZZ"
		   BORRCLI.USAT = "U"       ' Avis de que est… utilitzat
		   PUT AREABC, FA, BORRCLI
		END IF
	     END IF
	  END IF

	  IF TA$ = "ZZZ" THEN
	     RNOUC = RRA               ' SI N'HA TROBAT QUE L'EDITI D'AMUNT
	  ELSE
	     RNOUC = MAXCL             ' SI NO N'HA TROBAT QUE N'AFEGESQUI UN
	  END IF
	  
	  GetBackground 1, 1, 24, 80, CLIBUF$

	  RVC = RNOUC                         ' DESIGNAR EL REGISTRE NOU
	  GOSUB MASCARACLI
	  SetMaxCamps 13
	  FOR C = 0 TO 13: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT

	  GOSUB INITCAMPSCLI
	  GOSUB BUIDARCAMPSCLI              ' BUIDAR CAMPS
	  GOSUB LLEGIRCAMPSCLI              ' INTRODU‹R DADES ALS CAMPS
	  GOSUB MOUREFITXERCLI              ' MOURE LES DADES DELS CAMPS AL FITXER
	  LOCATE , , 0: DisplayAllCamps

	  IF ANUL = 1 THEN                  ' SI ES PITJA <ESC> A UN DELS CAMPS
	     PutBackground 1, 1, CLIBUF$
	     FOR C = 0 TO 13: DeleteCamp C: NEXT: ANUL = 0

	     IF TA$ = "ZZZ" THEN
		GET AREABC, FA, BORRCLI      ' Desbloqueix del registre com a usat
		BORRCLI.USAT = "F"
		PUT AREABC, FA, BORRCLI
	     END IF
	     RETURN
	  END IF
	  PUT AREA3, RNOUC, CLIENTS     ' GRAVAR DINS EL FITXER

	  IF TA$ = "ZZZ" THEN
	     BORRCLI.USAT = CHR$(0)     ' Avis de que ja no est… borrat
	     BORRCLI.REGISTRE = 0
	     PUT AREABC, FA, BORRCLI
	  END IF
	       
	  IF RNOUC = MAXCL THEN                        ' SI EL REGISTRE NOU ES IGUAL AL NOMBRE M…XIM DE CLIENTS
	     CEMPRE.MAXCLIENTS = MAXCL + 1
	     MAXCL = MAXCL + 1
	     RNDX = RNOUC: GOSUB ACTUALIZ.INDEX        ' ACTUALITZA L'INDEX
	     PUT AREA, 1, CEMPRE
	  ELSE
	     RNDX = RNOUC: GOSUB ACTUALIZ.INDEX        ' ACTUALITZA L'INDEX
	  END IF
	  PutBackground 1, 1, CLIBUF$
	  GOSUB MOSTRA.DARRER
	  RETURN


'************************************************************************
' LLEGEIX ELS CAMPS
'************************************************************************

LLEGIRCAMPSCLI:
       IF EDITA = 0 THEN InsertValueCamp 0, XXX$(0): DisplayAllCamps
       CORRECTE = 0: ANUL = 0
       SetMaxCamps 13
       DO
	  FOR C = 0 TO 13
	     VALUE = ReadCamp(C)
	     SELECT CASE VALUE
		    CASE 0
			  IF EDITA = 0 THEN
			     CAMPTEMP$ = ForaEspai$(ValueCamp$(0))
			     IF FindClient%(ForaEspai$(CAMPTEMP$), INDEX(), MAXCL) THEN
				IF CLIENTS.MARCAT <> "*" THEN
				   BEEP: LOCATE 5, 50: COLOR 28, 9: PRINT "<< AQUEST CODI EXISTEIX >>"
				   C = C - 1
				END IF
			     END IF
			  END IF
		    CASE 5, 6
			  GOSUB CERCA.CP
			  DisplayAllCamps
		    CASE 999
			 ANUL = 1
			 RETURN
		    CASE F1 TO F10
			 C = C - 1: SOUND 50, .5
		    CASE ELSE
	     END SELECT
	 NEXT
	 LOCATE 19, 29: COLOR 27: PRINT " Son correctes aquestes dades (S/N) ?"
	 COLOR 15, 9
	 DO
	   DO: T$ = INKEY$: LOOP UNTIL T$ <> ""
	   IF UCASE$(T$) = "S" THEN CORRECTE = 888: EXIT DO
	   IF UCASE$(T$) = "N" THEN CORRECTE = 0: EXIT DO
	 LOOP
       LOOP UNTIL CORRECTE = 888
       RETURN

CERCA.CP:
       CAMPTEMP$ = UCASE$(LTRIM$(RTRIM$(ValueCamp$(VALUE))))
       FOR RK = 1 TO MAXCP
	   GET AREA13, RK, CODISP
	   IF VALUE = 5 AND CAMPTEMP$ = UCASE$(LTRIM$(RTRIM$(CODISP.CODI))) THEN
	      InsertValueCamp 6, CODISP.POBLE
	      InsertValueCamp 9, CODISP.PROVINCIA
	   ELSEIF VALUE = 6 AND CAMPTEMP$ = UCASE$(LTRIM$(RTRIM$(CODISP.POBLE))) THEN
	      InsertValueCamp 5, CODISP.CODI
	      InsertValueCamp 9, CODISP.PROVINCIA
	   END IF
       NEXT
       RETURN

'************************************************************************
' MOSTRA EL CODI DEL DARRER CLIENT
'************************************************************************

MOSTRA.DARRER:
	  IF MAXCL > 1 THEN
	     GOSUB CERCAR.REGISTRE.BORRAT.CLI
	     GET AREA3, RBC, CLIENTS
	     COLOR COL(2, 0): LOCATE 4, 40: PRINT "CODI DEL DARRER CLIENT:"; : COLOR COL(0, 0): PRINT CLIENTS.CODICLIENT
	  END IF
	  RETURN

'************************************************************************
' LLISTAR CLIENTS
'************************************************************************
	     
LLISTA.CLIENTS:
       DIM LLISTA$(1 TO MAXCL)
       GetBackground 1, 1, 24, 80, LISBUF$
       SetScoreBoard SOFF: LOCATE , , 0: R = 1
       FOR Q = 1 TO MAXCL - 1
	   GET AREA3, Q, CLIENTS
	   IF CLIENTS.MARCAT = "-" THEN
	      CODI$ = CLIENTS.CODICLIENT
	      N$ = LTRIM$(RTRIM$(CLIENTS.NOM))
	      C$ = LTRIM$(RTRIM$(CLIENTS.COGNOMS))
	      S$ = N$ + " " + C$: NOM$ = MID$(S$, 1, 40)
	      LLISTA$(R) = CODI$ + " " + NOM$
	      R = R + 1
	   END IF
       NEXT
       R = R - 1: ASEL = Achoice(3, 9, 20, 61, R, LLISTA$(), COL(), "Codi       Nom                                     ", 12, "")
       IF ASEL = 0 OR ASEL = -14 OR ASEL = -13 THEN
	  PutBackground 1, 1, LISBUF$
	  ERASE LLISTA$
	  C = C - 1: RETURN
       END IF
       IF ASEL <= 0 THEN ASEL = 1
       GET AREA3, ASEL, CLIENTS
       CC$ = CLIENTS.CODICLIENT
       GOSUB INITCAMPSCLI: GOSUB MOURECAMPSCLI: GOSUB MASCARACLI
       CALL DisplayAllCamps: EDITA = 1
       GOSUB LLEGIRCAMPSCLI
       PutBackground 1, 1, LISBUF$
       GOSUB INITCAMPS: GOSUB BUIDARCAMPS
       InsertValueCamp 0, CC$
       DisplayAllCamps
       CORRECTE = 0
       ERASE LLISTA$
       GOTO READCAMPS

'************************************************************************
' INICIALITZA CAMPS
'************************************************************************
INITCAMPSCLI:
	  FOR C = 0 TO 13: DeleteCamp C: NEXT
	  SetInitCamp 0, 7, 29, ASCI, 0, "XXXXXXXXXX", ""
	  SetInitCamp 1, 8, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 2, 9, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 3, 8, 58, ASCI, 0, "XXXXXXXXXXX", ""
	  SetInitCamp 4, 11, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 5, 12, 29, ASCI, 0, "XXXXXXX", ""
	  SetInitCamp 6, 12, 48, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 7, 13, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 8, 14, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 9, 15, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 10, 17, 29, num, 0, "999", ""
	  SetInitCamp 11, 17, 58, ASCI, 0, "XXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 12, 18, 29, ASCI, 0, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", ""
	  SetInitCamp 13, 19, 29, ASCI, 0, "XXXXXXXXXX", ""
	  RETURN

'************************************************************************
' MOU ELS CAMPS DEL FITXER A CAMPS DINS MEMORIA
'************************************************************************

MOURECAMPSCLI:
	  InsertValueCamp 0, CLIENTS.CODICLIENT
	  InsertValueCamp 1, CLIENTS.NOM
	  InsertValueCamp 2, CLIENTS.COGNOMS
	  InsertValueCamp 3, CLIENTS.DNI
	  InsertValueCamp 4, CLIENTS.DIRECCIO
	  InsertValueCamp 5, CLIENTS.CPOSTAL
	  InsertValueCamp 6, CLIENTS.POBLACIO
	  InsertValueCamp 7, CLIENTS.TELEFON1
	  InsertValueCamp 8, CLIENTS.TELEFON2
	  InsertValueCamp 9, CLIENTS.PROVINCIA
	  InsertValueCamp 10, LTRIM$(STR$(CLIENTS.DTO))
	  InsertValueCamp 11, CLIENTS.FORMAPAGO
	  InsertValueCamp 12, CLIENTS.BANC
	  InsertValueCamp 13, CLIENTS.COMPTE
	  RETURN

'************************************************************************
' MOURE CAMPS DE MEMORIA CAP A FITXER
'************************************************************************

MOUREFITXERCLI:
	   CLIENTS.CODICLIENT = RTRIM$(ValueCamp(0))
	   CLIENTS.NOM = ValueCamp$(1)
	   CLIENTS.COGNOMS = ValueCamp$(2)
	   CLIENTS.DNI = ValueCamp$(3)
	   CLIENTS.DIRECCIO = ValueCamp$(4)
	   CLIENTS.CPOSTAL = ValueCamp$(5)
	   CLIENTS.POBLACIO = ValueCamp$(6)
	   CLIENTS.TELEFON1 = ValueCamp$(7)
	   CLIENTS.TELEFON2 = ValueCamp$(8)
	   CLIENTS.PROVINCIA = ValueCamp$(9)
	   CLIENTS.DTO = VAL(ValueCamp$(10))
	   CLIENTS.FORMAPAGO = ValueCamp$(11)
	   CLIENTS.BANC = ValueCamp$(12)
	   CLIENTS.COMPTE = ValueCamp$(13)
	   CLIENTS.MARCAT = "-"
	   RETURN

'************************************************************************
' BUIDAR ELS CAMPS DEL FITXER
'************************************************************************
BUIDARCAMPSCLI:
	   CLIENTS.CODICLIENT = STRING$(LEN(CLIENTS.CODICLIENT), CHR$(32))
	   CLIENTS.NOM = ""
	   CLIENTS.DNI = ""
	   CLIENTS.COGNOMS = ""
	   CLIENTS.DIRECCIO = ""
	   CLIENTS.CPOSTAL = ""
	   CLIENTS.POBLACIO = ""
	   CLIENTS.TELEFON1 = ""
	   CLIENTS.TELEFON2 = ""
	   CLIENTS.PROVINCIA = ""
	   CLIENTS.DTO = 0
	   CLIENTS.FORMAPAGO = ""
	   CLIENTS.BANC = ""
	   CLIENTS.COMPTE = ""
	   CLIENTS.MARCAT = "-"
	   FOR C = 0 TO 13: DeleteCamp C: NEXT
	   RETURN


'************************************************************************
' FITXA D'UN RESGUARD
'************************************************************************

MASCARA:
	  COLOR COL(0, 0), COL(0, 1)
	  IF SetInitWindows(1) = TRUE THEN RETURN
	  WIN1 = InitNewWindow(0, 3, 10, 20, 76, 1, "Fitxa del resguard")
	  SetAllColors COL(0, 0), COL(0, 1), COL(1, 1), COL(1, 0), COL(2, 0), COL(2, 1)
	  SetStyleWindow 0, 0, "": ShowWindow 0
	  COLOR COL(2, 0)
	  LOCATE 5, 11: PRINT "CODI CLIENT......:                       N§ D'ORDRE:"
	  LOCATE 6, 11: PRINT "NOM DEL CLIENT...:"                     '           1         2
	  LOCATE 7, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0)   '  12345678901234567890
	  LOCATE 8, 11: PRINT "MARCA:                               MODEL:                    "
	  LOCATE 9, 11: PRINT "N§ DE SERIE:                         APARELL:                    "
	  LOCATE 10, 11: PRINT "DATA ENTRADA:                        DATA SORTIDA:"
	  LOCATE 11, 11: PRINT STRING$(65, "Ä");
	  CENTRAR 11, " ANOMALIA "
	  LOCATE 15, 11: PRINT STRING$(65, "Ä");
	  CENTRAR 15, " OBSERVACIONS DEL SERVEI "
	  RETURN

'************************************************************************
' FITXA DE UN CLIENT
'************************************************************************

MASCARACLI:
	  COLOR COL(0, 0), COL(0, 1)
	  FINESTRA 4, 10, 20, 76, 1, CAIXA1: COLOR COL(2, 0)
	  LOCATE 5, 11: PRINT "Client No.: "; : COLOR COL(0, 0): PRINT RVC
	  LOCATE 6, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0)
	  LOCATE 7, 11: PRINT "Codi client......:"
	  LOCATE 8, 11: PRINT "Nom del client...:" + SPACE$(20) + " D.N.I..:"
	  LOCATE 9, 11: PRINT "Cognoms o mal nom:"
	  COLOR COL(0, 0): LOCATE 10, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0)
	  LOCATE 11, 11: PRINT "Direcci¢.........:"
	  LOCATE 12, 11: PRINT "Codi Postal......:         Poblaci¢.:"
	  LOCATE 13, 11: PRINT "TelŠfon 1........:"
	  LOCATE 14, 11: PRINT "TelŠfon 2 o FAX..:"
	  LOCATE 15, 11: PRINT "Provincia........:"
	  COLOR COL(0, 0): LOCATE 16, 11: PRINT STRING$(65, "Ä"); : COLOR COL(2, 0)
	  LOCATE 17, 11: PRINT "Descompte %......:" + SPACE$(11) + "Forma de pagament:"
	  LOCATE 18, 11: PRINT "Banc/Sucursal....:"
	  LOCATE 19, 11: PRINT "Compte...........:"
	  RETURN

'************************************************************************
' OBRIR ELS FITXERS DE CONTROL DE CLIENTS
'************************************************************************

OBRIRFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      IF DIR$(DIRECC$ + "CLIENTS.NDX") = "" THEN
	 AREA2 = FREEFILE: OPEN DIRECC$ + "CLIENTS.NDX" FOR RANDOM SHARED AS AREA2 LEN = LEN(NDXCLIENT)
      ELSE
	 AREA2 = FREEFILE: OPEN DTMP$ + "CLIENTS.NDX" FOR RANDOM SHARED AS AREA2 LEN = LEN(NDXCLIENT)
      END IF
      AREA3 = FREEFILE: OPEN DIRECC$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA3 LEN = LEN(CLIENTS)
      AREABC = FREEFILE: OPEN DIRECC$ + "CLIENTS.CRB" FOR RANDOM SHARED AS AREABC LEN = LEN(BORRCLI)
      AREA13 = FREEFILE: OPEN DIRECC$ + "CODISP.DAT" FOR RANDOM SHARED AS AREA13 LEN = LEN(CODISP)
      AREAR = FREEFILE: OPEN DIRECC$ + "RESGUARD.DAT" FOR RANDOM SHARED AS AREAR LEN = LEN(RESGUA)
      AREAF = FREEFILE: OPEN DIRECC$ + "RES_FAC.DAT" FOR RANDOM SHARED AS AREAF LEN = LEN(FACTURA)
      AREAB = FREEFILE: OPEN DIRECC$ + "RESGUARD.CRB" FOR RANDOM SHARED AS AREAB LEN = LEN(BORRREG)
      AREA4 = FREEFILE: OPEN DP$ + "PLANTILL\CAP€ALER.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CAP)
      AREAP = FREEFILE: OPEN DP$ + "PLANTILL\PEUSPAGI.DAT" FOR RANDOM AS AREAP LEN = LEN(PP)
      AREAD = FREEFILE: OPEN DIRECC$ + "DOCUMENT.NUM" FOR RANDOM SHARED AS AREAD LEN = LEN(DOCNUM)

      IF DIR$(DP$ + "PLANTILL\FESTES.DAT") <> "" THEN
	 AREAFE = FREEFILE: OPEN DP$ + "PLANTILL\FESTES.DAT" FOR INPUT AS AREAFE
	 INPUT #AREAFE, CADFE$
	 CLOSE AREAFE
      END IF

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
      GET AREAR, 1, RESGUA
      GET AREAD, 1, DOCNUM
      GET AREAF, 1, FACTURA
      MAXVI.RE = DOCNUM.MAXRESG
      MAXCL = CEMPRE.MAXCLIENTS
      MAXCP = LOF(AREA13) \ LEN(CODISP)
      MAXRE = RESGUA.MAXREG
      MAXFA = FACTURA.MAXREG
      RETURN

CERCAR.REGISTRE.BORRAT.CLI:
	  MAXCLIB = LOF(AREABC) \ LEN(BORRCLI): TROBATA$ = "XXX"
	  IF MAXCLIB = 0 THEN
	     TA$ = "   "
	  ELSE
	     EXISTA$ = "N": INIRA = 0
	     FOR FA = 1 TO MAXCLIB
		 GET AREABC, FA, BORRCLI
		 IF BORRCLI.REGISTRE <> 0 AND BORRCLI.USAT = "F" THEN
		    EXISTA$ = "S"
		    EXIT FOR
		 END IF
	     NEXT
	     IF EXISTA$ = "N" THEN
		TA$ = "   "
	     ELSE
		IF EXISTA$ = "S" THEN
		   TA$ = "ZZZ": RRA = BORRCLI.REGISTRE: TROBATA$ = "ZZZ"
		END IF
	     END IF
	  END IF

	  IF TA$ = "ZZZ" THEN
	     RBC = RRA               ' SI N'HA TROBAT QUE L'EDITI D'AMUNT
	  ELSE
	     RBC = MAXCL - 1           ' SI NO N'HA TROBAT QUE N'AFEGESQUI UN
	  END IF
	  RETURN

FILTRAR:
      AREABC = FREEFILE: OPEN DIRECC$ + "RESGUARD.RBC" FOR RANDOM SHARED AS AREABC LEN = LEN(BORRREG)
      LOCK AREAB
      FOR RC = 1 TO MAXCB
	  FinestraEstat "Filtrant borrats...", 0
	  GET AREAB, RC, BORRREG
	  IF BORRREG.REGISTRE > 0 AND BORRREG.USAT <> CHR$(0) THEN
	     PUT AREABC, RC, RORREG
	  END IF
      NEXT
      UNLOCK AREAB
      CLOSE AREABC
      CLOSE AREA, AREA2, AREA3, AREA4, AREAP, AREAR, AREAD, AREAF, AREAB, AREA13
      KILL DIRECC$ + "RESGUARD.CBR"
      SHELL "REN " + DIRECC$ + "RESGUARD.RBC " + DIRECC$ + "RESGUARD.CBR"
      GOSUB OBRIRFITXERS
      RETURN

INDEXAR:
      GetBackground 1, 1, 24, 80, TMP$
      IF MAXCL > 1 THEN
	 FOR RI = 1 TO MAXCL               ' COL.LOCAR A MEM•RIA L'INDEX I LA CLAU
	     GET AREA2, RI, NDXCLIENT
	     INDEX(RI).REGISTRE = NDXCLIENT.REGISTRE
	     INDEX(RI).CODI = NDXCLIENT.CODI
	 NEXT
      END IF
      RETURN

ACTUALIZ.INDEX:
      ERASE INDEX
      DIM INDEX(1 TO MAXCL) AS INDEXCLI
      GOSUB INDEXAR
      INDEX(RNDX).REGISTRE = RNDX
      INDEX(RNDX).CODI = CLIENTS.CODICLIENT
      GOSUB GUARDARINDEX
      RETURN

GUARDARINDEX:
      CALL SortClients(INDEX(), MAXCL)
      FOR RI = 1 TO MAXCL
	  NDXCLIENT.REGISTRE = INDEX(RI).REGISTRE
	  NDXCLIENT.CODI = INDEX(RI).CODI
	  PUT AREA2, RI, NDXCLIENT
      NEXT
      RETURN

TANCA:
      GOSUB GUARDARINDEX
      CLOSE AREA, AREA2, AREA3, AREA4  ' Per seguretat es tanquen els fitxers
      CLOSE AREAP, AREAR, AREAD, AREAF
      IF DIR$(DIRECC$ + "CLIENTS.NDX") <> "" THEN
	 DIRD$ = MID$(DIRECC$, 1, LEN(DIRECC$) - 1)
	 SHELL "COPY " + DTMP$ + "CLIENTS.NDX " + DIRD$ + " /Y >nul"
	 SHELL "DEL " + DTMP$ + "CLIENTS.NDX"
      END IF
      FOR C = 0 TO 13: DeleteCamp C: NEXT
      ERASE INDEX
      EXIT SUB

TANCAR.TEMPORAL:
      CLOSE AREA, AREA2, AREA3, AREA4  ' Per seguretat es tanquen els fitxers
      CLOSE AREAP, AREAR, AREAD, AREAF
      GOSUB OBRIRFITXERS               ' I es tornen a obrir
      RETURN

AJUDA:
    LOCATE 22, 2: PRINT "<F1>=AJUDA         <F2>=DEMANAR TELŠFON      <F3>=LLISTAT DE CLIENTS   "
    LOCATE 23, 2: PRINT "<ESC>=SORTIR                                                        "
    LOCATE 24, 2: PRINT "                                                                    ";
    RETURN

BARRACONTROL:
    LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR RESGU.  <F2>=CREAR RESGUARDS   <" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR              <F3>=SUMAR             <F4>=CONSULTAR"
    LOCATE 24, 2: PRINT "<F5>=MARCAR RESGUARD      <F6>=LLISTAR           <F7>=IMPRIMIR";
    RETURN

END SUB

REM $DYNAMIC
SUB MASCREGS
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 4, 1, 20, 80, 1, CAIXA1: COLOR COL(2, 0)
    LOCATE 5, 2: PRINT "ORDRE     ³NOM CLIENT" + SPACE$(10) + "³COGNOMS" + SPACE$(23) + "³DATA IN ³MARCAT": COLOR COL(0, 0)
    LOCATE 6, 2: PRINT "ÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄ"
    FOR L = 7 TO 19: LOCATE L, 2: PRINT "          ³                    ³                              ³        ³": NEXT
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR RESGU.  <F2>=CREAR RESGUARDS   <" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR              <F3>=SUMAR             <F4>=CONSULTAR"
    LOCATE 24, 2: PRINT "<F5>=MARCAR RESGUARD      <F6>=LLISTAR           <F7>=IMPRIMIR";
END SUB

SUB SortClients (INDEX() AS INDEXCLI, MAXCL) STATIC
    
    OFFSET = MAXCL - 1 \ 2

    DO WHILE OFFSET > 0
       LIMIT = MAXCL - 1 - OFFSET
       DO
	 SWITCH = FALSE
	 FOR I = 1 TO LIMIT
	     IF INDEX(I).CODI > INDEX(I + OFFSET).CODI THEN
		SWAP INDEX(I), INDEX(I + OFFSET)
		SWITCH = I
	     END IF
	 NEXT

	 LIMIT = SWITCH
       LOOP WHILE SWITCH
       OFFSET = OFFSET \ 2
    LOOP

END SUB

