' ********************************************************************
'
' Fitxer....................: HIS_CLI.BAS
' Titol.....................: Modul per fer un historic de moviments
'                             d'un client.
' ********************************************************************
'
' Data inici................: 17/07/1997 18:11:00
' Data de la darrera revisi╒: 18/07/1997 16:36:00
' Autor.....................: Tomeu Cap╒ Cap╒
' CopyRight.................: Smart Software 1993/97 (C)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE SUB HistoricClient (DIRECC$, DIRCP$, DIRECI$, DH$, DIRPLA$, DEVI$, IMPRESORA!)
DECLARE SUB LLISTARALBARAN (MAXALBARAN!, AREACAB!, AREAA2!, CODI$, NOMT$, DEVI$)
DECLARE SUB LLISTARESGUARDS (AREACAB!, AREAR!, MAXRESG!, CODI$, NOMT$, DEVI$)
DECLARE SUB LLISTARFactures (AREACAB!, AREAF2!, CODI$, NOMT$, DEVI$)

COMMON SHARED DIRECC$, DIRECCP$, DIRECCT$, DOCU$, DIRECCH$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5, AREANDX
COMMON SHARED GUARDAT, MIDPOINT, MAXCL, MAXAL, R, EDIT
COMMON SHARED TOTAL, MAX, NOU, TROBAT, DEVICE$, DEVI$, UNIDAD$
COMMON SHARED COL()

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'C:\FACT3\FONTS\RESGUARD.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\WIN.BI'
'$INCLUDE: 'C:\FACT3\FONTS\CONSTS.BI'

'$DYNAMIC
DIM SHARED COL(2, 1)
DIM SHARED CLIENT AS CLIENT
DIM SHARED CEMPRE AS CONTROLEMP
DIM SHARED ALBARAN AS ALBAR
DIM SHARED FACTURA AS FACTU
DIM SHARED TEXTOSMENUP(6) AS STRING
DIM SHARED DOCNUM AS DN
DIM SHARED CAP AS CAPSALDOCS
DIM SHARED SPOOLIMP AS SPOOL
DIM SHARED ARCHIMP AS IMPRESORA
DIM SHARED CFG AS CONFIG
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
DIM SHARED USR AS USUARIS
DIM COLORS AS COLO
DIM SHARED RESGUA AS RESG
DIM SHARED PASO AS TRANS
DIM SHARED ANYS AS ANNO

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
	  CA$ = PASO.CLAU
	  CAD$ = PASO.APLICACIO
	  CA2$ = ""

	  FOR L% = 1 TO LEN(CAD$)
	      CA2$ = CA2$ + ENCRIPT$(MID$(CA$, L%, 1), L%)
	  NEXT

	  IF LTRIM$(RTRIM$(CA2$)) <> "HIS_CLI.EXE," THEN
	     TELCA = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "PITJA UNA TECLA...", 0)
	     END
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

      DIRECCF$ = DBF$       ' Subdirecctori de les base de dades
      DIRECCP$ = SCR$       ' Subdirecctori de les pantalles
      DIRECCR$ = MSK$     ' Subdirecctori de mascares, errors, etc...
      DIRECCHE$ = HLP$     ' Subdirecctori de ajuda
      DIRECCI$ = UNIDAD$ + "\IMPRESOR\"    ' Subdirecctori de les impresores
      DIRECCT$ = DBF$ + "TEXTOS\"
      DIRECCH$ = DBF$ + "HISTORIC\"
      DIRECCPL$ = DBF$ + "PLANTILL\"

      MAXLINS = USR.MAXLINS
      MAXFAC = USR.LINFACT
      IVA = EMPRES.IVA
      DTO = EMPRES.DTO
      R = EMPRES.ANY
      DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
      IMPRESORA = USR.IMPRESORA

      SetDirRecursos (DIRECCR$)
      SetFormatCC (34)

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

      CALL HistoricClient(DIRECCF$, DBF$, DIRECCI$, DIRECCH$, DIRECCPL$, DEV$, IMPRESORA)
      SYSTEM

ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

REM $STATIC
SUB HistoricClient (DIRECC$, DIRCP$, DIRECI$, DH$, DIRPLA$, DEVI$, IMPRESORA)

    ON ERROR GOTO ERRORS

    GOSUB OBRIFITXERS
    GOSUB DEFDESKTOP

    ShowWindow 0
    COLOR COL(2, 0), COL(2, 1)
    LOCATE 19, 12: PRINT "* Seleccioni amb una X l'hist∙ric"
    LOCATE 20, 12: PRINT "  que vulgi treure."
    DisplayAllCamps

    FOR C = 0 TO MAXCAMPS
	VALUE = ReadCamp(C)
	SELECT CASE VALUE
	       CASE 0
		    LOCATE 13, 27: PRINT SPACE$(LEN(CLIENT.NOM))
		    LOCATE 14, 27: PRINT SPACE$(LEN(CLIENT.COGNOMS))
		    FOR R = 1 TO MAXCL - 1
			GET AREA4, R, CLIENT
			IF CLIENT.MARCAT <> "*" THEN
			   IF LTRIM$(RTRIM$(CLIENT.CODICLIENT)) = LTRIM$(RTRIM$(ValueCamp$(0))) THEN
			      CODI$ = LTRIM$(RTRIM$(CLIENT.CODICLIENT))
			      NOMT$ = LTRIM$(RTRIM$(CLIENT.NOM)) + " " + LTRIM$(RTRIM$(CLIENT.COGNOMS))
			      LOCATE 13, 27: PRINT LTRIM$(RTRIM$(CLIENT.NOM))
			      LOCATE 14, 27: PRINT LTRIM$(RTRIM$(CLIENT.COGNOMS))
			      EXIT FOR
			   END IF
			END IF
		    NEXT
		    IF CODI$ = "" THEN
		       TECLA = Avis("ERROR:", "Aquest codi no existeix", "Pitja una tecla...", 0)
		       C = C - 1
		    END IF
	       CASE F1, F3 TO F10
		    C = C - 1
	       CASE F2
		    GOSUB LLISTA.CLIENTS
		    C = C - 1
	       CASE 999
		    DeleteWindow 0
		    PackWindows
		    RESET
		    EXIT SUB
	       CASE ELSE
	END SELECT
    NEXT
    
    IF ValueCamp$(1) = "x" OR ValueCamp$(1) = "X" THEN CALL LLISTARFactures(AREACAB, AREAF2, CODI$, NOMT$, DEVI$)
    IF ValueCamp$(2) = "x" OR ValueCamp$(2) = "X" THEN CALL LLISTARALBARAN(MAXALB, AREACAB, AREAA2, CODI$, NOMT$, DEVI$)
    IF ValueCamp$(3) = "x" OR ValueCamp$(3) = "X" THEN CALL LLISTARESGUARDS(AREACAB, AREAR, MAXRESG, CODI$, NOMT$, DEVI$)

    DeleteWindow 0
    PackWindows
    EXIT SUB

'************************************************************************
' LLISTAR CLIENTS
'************************************************************************

LLISTA.CLIENTS:
       DIM LLISTA$(1 TO MAXCL)
       GetBackground 1, 1, 24, 80, LISBUF$
       SetScoreBoard SOFF: LOCATE , , 0: R = 1
       FOR Q = 1 TO MAXCL - 1
	   GET AREA4, Q, CLIENT
	   IF CLIENT.MARCAT = "-" THEN
	      CODI$ = CLIENT.CODICLIENT
	      N$ = LTRIM$(RTRIM$(CLIENT.NOM))
	      C$ = LTRIM$(RTRIM$(CLIENT.COGNOMS))
	      S$ = N$ + " " + C$: NOM$ = MID$(S$, 1, 40)
	      LLISTA$(R) = CODI$ + " " + NOM$
	      R = R + 1
	   END IF
       NEXT
       R = R - 1
       ASEL = Achoice(3, 9, 20, 61, R, LLISTA$(), COL(), "Codi       Nom                                     ", 12, "")
       IF ASEL = 0 OR ASEL = -14 OR ASEL = -13 THEN
	  PutBackground 1, 1, LISBUF$
	  ERASE LLISTA$
	  RETURN
       END IF
       IF ASEL <= 0 THEN ASEL = 1
       GET AREA4, ASEL, CLIENT
       InsertValueCamp 0, CLIENT.CODICLIENT
       PutBackground 1, 1, LISBUF$
       DisplayAllCamps
       ERASE LLISTA$
       RETURN


OBRIFITXERS:
      AREA = FREEFILE: OPEN DIRECC$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREAF2 = FREEFILE: OPEN DIRECC$ + "FACTURES.CAB" FOR RANDOM SHARED AS AREAF2 LEN = LEN(FACTURA)
      AREAA2 = FREEFILE: OPEN DIRECC$ + "ALBARAN.CAB" FOR RANDOM SHARED AS AREAA2 LEN = LEN(ALBARAN)
      AREAR = FREEFILE: OPEN DIRECC$ + "RESGUARD.DAT" FOR RANDOM SHARED AS AREAR LEN = LEN(RESGUA)
      AREA4 = FREEFILE: OPEN DIRECC$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CLIENT)
      AREACAB = FREEFILE: OPEN DIRPLA$ + "CAP─ALER.DAT" FOR RANDOM SHARED AS AREACAB LEN = LEN(CAP)
      AREANUM = FREEFILE: OPEN DIRECC$ + "DOCUMENT.NUM" FOR RANDOM SHARED AS AREANUM LEN = LEN(DOCNUM)

'*********************************************************************
' DEMANAR IMPRESORA ACTIVA
'*********************************************************************
      AREA6 = FREEFILE: OPEN "..\SPOOL.CFG" FOR RANDOM SHARED AS AREA6 LEN = LEN(SPOOLIMP)
      GET AREA6, IMPRESORA, SPOOLIMP
      FITXER$ = LTRIM$(RTRIM$(SPOOLIMP.ARCHIVO))
      CLOSE AREA6

'*********************************************************************
' OBRIR FITXER DE LA IMPRESORA
'*********************************************************************
      AREA7 = FREEFILE: OPEN DIRECI$ + FITXER$ FOR RANDOM SHARED AS AREA7 LEN = LEN(ARCHIMP)
      GET AREA7, 1, ARCHIMP
      CLOSE AREA7

'*********************************************************************
' INICIAR EL CONTROL DELS REGISTRES
'*********************************************************************

      GET AREA, 1, CEMPRE
      GET AREANUM, 1, DOCNUM
      MAXAL = CEMPRE.MAXFACTURA
      MAXCL = CEMPRE.MAXCLIENTS
      MAXALB = CEMPRE.MAXALBARAN
      GET AREAR, 1, RESGUA
      MAXRESG = RESGUA.MAXREG
      
      RETURN

DEFDESKTOP:
    IF SetInitWindows(1) = TRUE THEN
       TECLA = Avis("ERROR:", "Mem∙ria insuficient!!!", "PITJA UNA TECLA...", 0)
       RETURN
    END IF

    SetMaxCamps 3

    WIN = InitNewWindow(0, 10, 10, 22, 67, 1, "Moviments totals d'un client")
    SetInitCamp 0, 12, 27, ASCI, 0, "XXXXXXXXXX", "Codi client:"
    SetInitCamp 1, 15, 27, ASCI, 0, "X", " Factures: "
    SetInitCamp 2, 16, 27, ASCI, 0, "X", " Albarans: "
    SetInitCamp 3, 17, 27, ASCI, 0, "X", "Resguards: "
    FOR C = 0 TO MAXCAMPS: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT
    SetColorWindow 0, COL(0, 0), COL(0, 1), COL(1, 1), COL(1, 0), COL(2, 0), COL(2, 1)
    SetStyleWindow 0, 0, ""

    RETURN

END SUB

SUB LLISTARALBARAN (MAXALBARAN, AREACAB, AREAA2, CODI$, NOMT$, DEVI$)
    SHARED DIRECCT$

    AREATXT = FREEFILE
    OPEN DIRECCT$ + "HIS_ALBA.TXT" FOR OUTPUT AS AREATXT

    PAG = 1: L = 1
    GET AREACAB, 1, CAP   ' AGAFAR CAP─ALERA DEL FITXER DE CAP─ALERES

    GOSUB CAP.LIST     ' IMPRIMIR CAP─ALERA

    ' DEFINIR MASCARA DE LES LINIES
    MASCARA$ = "  \" + SPACE$(9) + "\\" + SPACE$(47) + "\    ##,###,###.## ##.## ##.## ##,###,###.## \ \"
    MAXL = 54: TOTALB& = 0: TOTALN& = 0

    FOR R! = 1 TO MAXALBARAN - 1
	FinestraEstat "Generant historic d'albarans...", 0
	GET AREAA2, R!, ALBARAN
	
	NOMF$ = LTRIM$(RTRIM$(ALBARAN.PERSONA.NOM)) + " " + LTRIM$(RTRIM$(ALBARAN.PERSONA.COGNOMS))

	IF LTRIM$(RTRIM$(ALBARAN.CODCLIENT)) = CODI$ OR NOMF$ = NOMT$ THEN
	   GOSUB PRINTLINIA
	   TOTALB& = TOTALB& + ALBARAN.TOTALBRUT
	   TOTALN& = TOTALN& + ALBARAN.TOTALNET
	   IF L >= MAXL THEN
	      L = 1: PAG = PAG + 1
	      PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + " " + STRING$(80, "д") + RTRIM$(ARCHIMP.COMPRIMIDO)
	      PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
	      GOSUB CAP.LIST
	   END IF
	END IF
    NEXT
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + " " + STRING$(79, "д") + RTRIM$(ARCHIMP.COMPRIMIDO)
    PRINT #AREATXT, USING MASCARA$; SPACE$(9); SPACE$(31) + "Suma dels Totals: "; TOTALB&; 0; 0; TOTALN&
    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT

    COLOR 15, 9: FINESTRA 10, 30, 15, 45, 1, CAIXA1
    COLOR 31, 9: LOCATE 11, 31: PRINT " IMPRIMINT  "
    COLOR 31, 9: LOCATE 13, 31: PRINT "  FITXER    "
    COLOR 31, 9: LOCATE 14, 31: PRINT " ALBARANS   "


    CALL ImprimeixFitxerTXT(DIRECCT$ + "HIS_ALBA.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP─ALERA DELS LLISTATS
' **************************************************************************

CAP.LIST:
    PRINT #AREATXT, " "; RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, " LLISTAT DELS ALBARANS DEL CLIENT: "; C$
    PRINT #AREATXT, " P┘gina:"; PAG
    PRINT #AREATXT, " Data..:"; FormatD$(Now#, "dd-mm-yyyy")
    PRINT #AREATXT, " " + STRING$(79, "д");
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""
    PRINT #AREATXT, "  Refer┼ncia Nom del client                                          Subtotal   DTO % IVA % Total Net     Facturat"
    PRINT #AREATXT, "  ---------- ------------------------------------------------------- ---------- ----- ----- ------------- --------"
    RETURN

PRINTLINIA:
    NOMPCLIENT$ = RTRIM$(ALBARAN.PERSONA.COGNOMS) + ";" + RTRIM$(ALBARAN.PERSONA.NOM)
    IF ALBARAN.DOCUMENT = "F" THEN MARCA$ = "X" ELSE MARCA$ = ""
    PRINT #AREATXT, USING MASCARA$; ALBARAN.REFALBARAN; NOMPCLIENT$; ALBARAN.TOTALBRUT; ALBARAN.DTO; ALBARAN.TIPOIVA; ALBARAN.TOTALNET; MARCA$
    L = L + 1
    RETURN
END SUB

SUB LLISTARESGUARDS (AREACAB, AREAR, MAXRESG, CODI$, NOMT$, DEVI$)
    SHARED DIRECCT$

    AREATXT = FREEFILE
    OPEN DIRECCT$ + "HIS_RESG.TXT" FOR OUTPUT AS AREATXT

    PAG = 1: L = 1
    GET AREACAB, 1, CAP   ' AGAFAR CAP─ALERA DEL FITXER DE CAP─ALERES

    GOSUB CABE.LIST     ' IMPRIMIR CAP─ALERA

    ' DEFINIR MASCARA DE LES LINIES

    MASCARA$ = " Ё\" + SPACE$(8) + "\Ё\" + SPACE$(38) + "\Ё\      \Ё\      \Ё\" + SPACE$(18) + "\Ё\" + SPACE$(18) + "\Ё"

    MAXL = 54: TOTALB& = 0: TOTALN& = 0
    MAXF# = MAXRESG - 1
    FOR RL! = 1 TO MAXRESG - 1
	FinestraEstat "Generant historic de resguards...", 0
	GET AREAR, RL!, RESGUA
	NOMF$ = LTRIM$(RTRIM$(RESGUA.PERSONA.NOM)) + " " + LTRIM$(RTRIM$(RESGUA.PERSONA.COGNOMS))
	IF CODI$ = LTRIM$(RTRIM$(RESGUA.CODCLIENT)) OR NOMF$ = NOMT$ THEN
	   IF RESGUA.MARCAT <> "*" THEN GOSUB PRINTLINIAR
	END IF

	IF L >= MAXL THEN
	   L = 1: PAG = PAG + 1
	   GOSUB LINIAFINAL
	   PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
	   PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
	   GOSUB CABE.LIST
	END IF
    NEXT
    GOSUB LINIAFINAL

    'PRINT #AREATXT, USING MASCAR2$; SPACE$(9); SPACE$(20) + "Suma dels Totals de Taller: "; TOTALB&; 0; SUMAIVT&; SUMATALLER&
    'PRINT #AREATXT, USING MASCAR2$; SPACE$(9); SPACE$(20) + "Suma dels Totals de Botiga: "; TOTALN&; 0; SUMAIVB&; SUMABOTIGA&
    'PRINT #AREATXT, "                                                                юддддддддддддддадддддаддддддддддаддддддддддддды"
    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT

    COLOR 15, 9: FINESTRA 10, 30, 15, 45, 1, CAIXA1
    COLOR 31, 9: LOCATE 11, 31: PRINT " IMPRIMINT  "
    COLOR 31, 9: LOCATE 13, 31: PRINT "  FITXERS   "
    COLOR 31, 9: LOCATE 14, 31: PRINT " RESGUARDS  "
				    
    CALL ImprimeixFitxerTXT(DIRECCT$ + "HIS_RESG.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP─ALERA DELS LLISTATS
' **************************************************************************

CABE.LIST:
    PRINT #AREATXT, " "; RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, " LLISTAT DELS RESGUARS DEL CLIENT: "
    PRINT #AREATXT, " P┘gina:"; PAG
    PRINT #AREATXT, " Data..: "; FormatD$(Now#, "dd-mm-yyyy")
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""

     '                                    1         2         3         4         5
     '                           12345678901234567890123456789012345678901234567890
    PRINT #AREATXT, " зддддддддддбддддддддддддддддддддддддддддддддддддддддбддддддддбддддддддбддддддддддддддддддддбдддддддддддддддддддд©"
    PRINT #AREATXT, " ЁN╖Ordre   ЁNom del client                          ЁData In ЁData OutЁAparell             ЁMarca               Ё"
    PRINT #AREATXT, " цддддддддддеддддддддддддддддддддддддддддддддддддддддеддддддддеддддддддеддддддддддддддддддддедддддддддддддддддддд╢"
    RETURN

PRINTLINIAR:
    NOMPCLIENT$ = LTRIM$(RTRIM$(RESGUA.PERSONA.NOM)) + " " + LTRIM$(RTRIM$(RESGUA.PERSONA.COGNOMS))
    IF NOMPCLIENT$ = " " THEN
       NOMPCLIENT$ = SPACE$(52)
    END IF
    PRINT #AREATXT, USING MASCARA$; RESGUA.ORDRE; NOMPCLIENT$; RESGUA.FETXAIN; RESGUA.FETXAOUT; RESGUA.APARELL; RESGUA.MARCA
    L = L + 1
    RETURN

LINIAFINAL:
       PRINT #AREATXT, " юддддддддддаддддддддддддддддддддддддддддддддддддддддаддддддддаддддддддаддддддддддддддддддддадддддддддддддддддддды"
       RETURN

END SUB

SUB LLISTARFactures (AREACAB, AREAF2, CODI$, NOMT$, DEVI$)
    SHARED DIRECCT$

    AREATXT = FREEFILE
    OPEN DIRECCT$ + "HIS_FACT.TXT" FOR OUTPUT AS AREATXT

    PAG = 1: L = 1
    GET AREACAB, 1, CAP   ' AGAFAR CAP─ALERA DEL FITXER DE CAP─ALERES

    GOSUB CABEF.LIST     ' IMPRIMIR CAP─ALERA

    ' DEFINIR MASCARA DE LES LINIES
    SUMAIVB& = 0: SUMAIVT& = 0
    TOTALN& = 0: TOTABN& = 0
    SUMATALLER& = 0: SUMABOTIGA& = 0
    MASCARA$ = " Ё\" + SPACE$(8) + "\Ё\" + SPACE$(47) + "\  Ё    ##,###,###Ё##.##Ё##,###,###Ё   ##,###,###Ё\      \Ё"
    MASCAR2$ = "  \" + SPACE$(8) + "\ \" + SPACE$(47) + "\  Ё    ##,###,###Ё##.##Ё##,###,###Ё   ##,###,###Ё\      \"
    MAXL = 54: TOTALB& = 0: TOTALN& = 0
    MAXF# = CEMPRE.MAXFACTURA - 1
    FOR RL! = 1 TO CEMPRE.MAXFACTURA - 1
      FinestraEstat "Generant historic de factures...", 0
      GET AREAF2, RL!, FACTURA
      NOMF$ = LTRIM$(RTRIM$(FACTURA.PERSONA.NOM)) + " " + LTRIM$(RTRIM$(FACTURA.PERSONA.COGNOMS))
      IF LTRIM$(RTRIM$(FACTURA.CODCLIENT)) = CODI$ OR NOMF$ = NOMT$ THEN
	       IF FACTURA.MARCAT <> "*" THEN GOSUB PRINTLINIAF
	       IF MID$(FACTURA.REFFACTURA, 8, 2) = "RF" THEN
		   SUMAIVT& = SUMAIVT& + FACTURA.TOTALIVA
		   TOTALB& = TOTALB& + FACTURA.TOTALBRUT
		   SUMATALLER& = SUMATALLER& + FACTURA.TOTALNET
	       ELSE
		  IF MID$(FACTURA.REFFACTURA, 8, 2) = "BF" THEN
		     SUMAIVB& = SUMAIVB& + FACTURA.TOTALIVA
		     TOTALN& = TOTALN& + FACTURA.TOTALBRUT
		     SUMABOTIGA& = SUMABOTIGA& + FACTURA.TOTALNET
		  END IF
	       END IF
      END IF
	       IF L >= MAXL THEN
		  L = 1: PAG = PAG + 1
		  PRINT #AREATXT, " юддддддддддадддддддддддддддддддддддддддддддддддддддддддддддддддаддддддддддддддадддддаддддддддддадддддддддддддадддддддды"
		  PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
		  PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
		  GOSUB CABEF.LIST
	       END IF
    NEXT
    PRINT #AREATXT, " юддддддддддадддддддддддддддддддддддддддддддддддддддддддддддддддеддддддддддддддедддддеддддддддддедддддддддддддедддддддды"
    PRINT #AREATXT, USING MASCAR2$; SPACE$(9); SPACE$(20) + "Suma dels Totals de Taller: "; TOTALB&; 0; SUMAIVT&; SUMATALLER&
    PRINT #AREATXT, USING MASCAR2$; SPACE$(9); SPACE$(20) + "Suma dels Totals de Botiga: "; TOTALN&; 0; SUMAIVB&; SUMABOTIGA&
    PRINT #AREATXT, "                                                                юддддддддддддддадддддаддддддддддаддддддддддддды"
    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT

    COLOR 15, 9: FINESTRA 10, 30, 15, 45, 1, CAIXA1
    COLOR 31, 9: LOCATE 11, 31: PRINT " IMPRIMINT  "
    COLOR 31, 9: LOCATE 13, 31: PRINT "  LLISTAT   "
    COLOR 31, 9: LOCATE 14, 31: PRINT "  FACTURES  "

    CALL ImprimeixFitxerTXT(DIRECCT$ + "HIS_FACT.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAP─ALERA DELS LLISTATS
' **************************************************************************

CABEF.LIST:
    PRINT #AREATXT, " "; RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, " LLISTAT DE LES FACTURES DEL CLIENT: "
    PRINT #AREATXT, " P┘gina:"; PAG
    PRINT #AREATXT, " Data..: "; FormatD$(Now#, "dd-mm-yyyy")
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""

     '                                    1         2         3         4         5
     '                           12345678901234567890123456789012345678901234567890
    PRINT #AREATXT, " зддддддддддбдддддддддддддддддддддддддддддддддддддддддддддддддддбддддддддддддддбдддддбддддддддддбдддддддддддддбдддддддд©"
    PRINT #AREATXT, " ЁRefer┼nciaЁNom del client                                     ЁSubtotal      ЁDTO %ЁIVA %     ЁTotal Net    ЁData    Ё"
    PRINT #AREATXT, " цддддддддддедддддддддддддддддддддддддддддддддддддддддддддддддддеддддддддддддддедддддеддддддддддедддддддддддддедддддддд╢"

    RETURN

PRINTLINIAF:
    NOMPCLIENT$ = LTRIM$(RTRIM$(FACTURA.PERSONA.NOM)) + " " + LTRIM$(RTRIM$(FACTURA.PERSONA.COGNOMS))
    IF NOMPCLIENT$ = " " THEN
       NOMPCLIENT$ = SPACE$(52)
    END IF
    PRINT #AREATXT, USING MASCARA$; FACTURA.REFFACTURA; NOMPCLIENT$; FACTURA.TOTALBRUT; FACTURA.DTO; FACTURA.TIPOIVA; FACTURA.TOTALNET; FACTURA.DADA
    L = L + 1
    RETURN
END SUB

