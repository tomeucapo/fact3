'********************************************************************
'
' Fitxer....................: ALT_ALBA.BAS
' Titol.....................: Modul per el mateniment de albarans
'
' ********************************************************************
'
' Data inici................: 30/04/1996 20:03:00  (Data del FACT2)
' Data de la darrera revisi¢: 01/11/1999 16:36:00
' Autor.....................: Tomeu Cap¢ Cap¢
' CopyRight.................: Smart Software 1993/99 (C)
' Codi......................: MS-BASIC 7.01 (PDS)
'
' ********************************************************************
' Notes:
'
' ********************************************************************

DECLARE FUNCTION ComprovaFitxersPass! (modul$)
DECLARE SUB inicialitzavars ()
DECLARE SUB inicialitzadirs ()
DECLARE SUB inicialitzacolors ()
DECLARE SUB AsignaAnyTreball ()

DECLARE SUB Missatges ()
DECLARE SUB PintaValors ()
DECLARE SUB Albarans (MAX!, DTO!, IVA!, DEVI$, IMPRESORA!, MI!)
DECLARE SUB CarregarAlbaran (R!, MAXLIN!)
DECLARE FUNCTION CercarRecord% (camp$, INDEX() AS ANY, MAXST!, AREAST!)
DECLARE SUB CrearAlbarans (MAX!, DTO!, IVA!, AREANUM!, AREA3!, AREA2!, AREA4!, AREAST!, MAXFA!, MAXST!, MAXAL!, EDIT!, DH$, DP$, MI!, DEVI$)DECLARE SUB FacturarAlbara (AREA!, AREANUM!, RG!, MAXFA!, MAXLIN!, DIRECC$)
DECLARE SUB GuardaAlbaran (RG!, MAXLIN!, ASX!)
DECLARE SUB ImprimirAlbaran (DP$, DEVI$, MAX!, AREADOC!, MI!)
DECLARE FUNCTION IniciaRef! (AREANUM!, R!, MARCAT!, REFOLD$)
DECLARE SUB InitAlbaran (MAXLIN!)
DECLARE SUB LlistarAlbarans (AREA5!, AREA2!, DEVI$)
DECLARE SUB MascAlba (MAX!)
DECLARE SUB MascAlbs ()
DECLARE FUNCTION PossaEspai$ (CAD$, C!)
DECLARE SUB ReadCalculFactura ()
DECLARE SUB ReadCapsalera (MAXAL!, MAXCL!)
DECLARE SUB ReadObserva ()
DECLARE SUB RepintaCapsalera (MAX!)
DECLARE SUB SumarAlbarans (MAXAL!, AREA2!)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'D:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'D:\FACT3\FONTS\FACTURA.BI'
'$INCLUDE: 'D:\FACT3\FONTS\STOCK.BI'
'$INCLUDE: 'D:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'D:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'D:\FACT3\FONTS\CONSTS.BI'

COMMON SHARED DIRECC$, DIRECCR$, DIRECCP$, DIRECCT$, DIRECCH$, DIRECCF$, CADFE$, DOCU$
COMMON SHARED AREA, AREA1, AREA2, AREA3, AREA4, AREA5, AREA8, AREA9, AREANDX, AREA4NDX
COMMON SHARED GUARDAT, MIDPOINT, MAXCL, MAXAL, MAXFA, R, EDIT, F, AREADOC, MI
COMMON SHARED ANY$, DIRPLA$, DIRECCI$
COMMON SHARED TOTAL, MAX, NOU, TROBAT, DEVICE$, DEVI$, UNIDAD$, DOCUM$

'$DYNAMIC
DIM SHARED FLD(6) AS CN           ' CONFIGURACI¢ DE CAMPS
DIM SHARED MASC$(6)               ' CONFIGURACI¢ DE MASCARES
DIM SHARED LIN$(100, 7)           ' LINIES DE LA FACTURA TEMPORAL
DIM SHARED OBS$(1)                ' LINIES D'OBSERVACIO
DIM SHARED COL(2, 1)
DIM SHARED estat(3)
DIM SHARED CLIENT AS CLIENT
DIM SHARED CEMPRE AS CONTROLEMP
DIM SHARED TEXTOSMENUP(7) AS STRING
DIM SHARED TEXTOSMENU$(6, 9)
DIM SHARED COMMENT$(7, 9)
DIM SHARED STOCK AS STK
DIM SHARED ALBARAN AS ALBAR
DIM SHARED LINALBA AS LINIES
DIM SHARED CAP AS CAPSALDOCS
DIM SHARED SPOOLIMP AS SPOOL
DIM SHARED ARCHIMP AS IMPRESORA
DIM SHARED CFG AS CONFIG
DIM SHARED CTRL AS CONTROL
DIM SHARED EMPRES AS EMPRESA
DIM SHARED USR AS USUARIS
DIM SHARED DOCNUM AS DN
DIM SHARED FACTURA AS FACTU
DIM SHARED LINFAC AS LINIAFACT
DIM SHARED TD AS TIPUSDOC
DIM SHARED NDXCLIENT AS INDEXCLI
DIM SHARED COLORS AS COLO
DIM SHARED PASO AS TRANS
DIM SHARED ANYS AS ANNO

'*************************************************************************
      ON ERROR GOTO ERRORS

      IF NOT ComprovaFitxersPass("ALT_ALBA.EXE") THEN
	 BEEP: PRINT "ERROR: Al llegir els fitxers de transpass"
	 SYSTEM
      END IF

      inicialitzavars
      inicialitzadirs
      inicialitzacolors
      AsignaAnyTreball
      SetDirRecursos (DIRECCR$)

      CALL Albarans(MAXFAC, INT(EMPRES.DTO), INT(EMPRES.IVA), DEV$, IMPRESORA, MI)
      SYSTEM

'***********************************************************************

ERRORS:
      IF ShowError = 1 THEN
	 RESUME NEXT
      ELSE
	 RESET
	 SYSTEM
      END IF

SUB Albarans (MAX, DTO, IVA, DEVI$, IMPRESORA, MI)
    DIM NDXFILE AS INDEXTYPE

    GOSUB OBRIFITXERS
    MascAlbs                          ' PINTA LA MASCARA DE LA LLISTA

    F = 0
    DIRECCT$ = DIRCP$
    SetScoreBoard SON                 ' ACTIVA EL VISOR D'ESTAT DE LES TECLES
				      ' DE CONTROL
    GOSUB FINAL.NOU
    
    DO
      OP$ = INKEY$
      COLOR COL(0, 0), COL(0, 1): CALL EstatTeclesControl(25, 3)
      SELECT CASE OP$
	     CASE CHR$(0) + "P"      ' BAIXA UN REGISTRE
		  GOSUB BAIXACURSOR
	     CASE CHR$(0) + "H"      ' PUJA UN REGISTRE
		  GOSUB PUJACURSOR
	     CASE CHR$(27)
		  SetScoreBoard SOFF: RESET
		  EXIT SUB
	     CASE CHR$(13)
		  ROLD = R: XOLD = X: CALL GetBackground(1, 1, 24, 80, fondo$)
		  CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXFA, MAXST, MAXAL, 1, DH$, DP$, MI, DEVI$)
		  PutBackground 1, 1, fondo$
		  X = XOLD: R = ROLD: GET AREA2, R, ALBARAN: COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     CASE CHR$(0) + CHR$(60)
		  ROLD = R: XOLD = X: CALL GetBackground(1, 1, 25, 80, fondo$)
		  CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXFA, MAXST, MAXAL, 0, DH$, DP$, MI, DEVI$)
		  PutBackground 1, 1, fondo$
		  MascAlbs
		  GOSUB FINAL.NOU
	     CASE CHR$(0) + CHR$(61)
		  SumarAlbarans MAXAL, AREA2
	     CASE CHR$(0) + CHR$(63)
		  GOSUB DELETERECORD         ' MARCAR REGISTRE
	     CASE CHR$(0) + CHR$(64)
		  GetBackground 1, 1, 24, 79, FACTU$
		  CALL LlistarAlbarans(AREA5, AREA2, DEVI$)
		  PutBackground 1, 1, FACTU$
	     CASE CHR$(0) + CHR$(65)
		  CALL CarregarAlbaran(R, MAX): DOCUM$ = "Albaran: " + ALBARAN.REFALBARAN
		  CALL ImprimirAlbaran(DP$, DEVI$, MAX, AREADOC, MI)
	     CASE CHR$(0) + "Q"
		  GOSUB BOTA.AVALL
	     CASE CHR$(0) + "I"
		  GOSUB BOTA.AMUNT: GET AREA2, R, ALBARAN: COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     CASE ELSE
      END SELECT
   LOOP
   RETURN

FINAL.NOU:
    IF MAXAL > 13 THEN
       R = MAXAL - 5: L = 1: X = 7
       GOSUB LISTA
       X = 7 + 4: R = MAXAL - 1                  ' INICIA CURSOR I POSICIONS A PANTALLA
    ELSE
       X = 7: R = 1: GOSUB LISTA: X = 7: R = 1
    END IF
    GET AREA2, R, ALBARAN: COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR

    RETURN
'************************************************************************
' Control del cursor per el llistat amb desplaáament de barres
'************************************************************************

' ********* Avaná de pÖgines (AMUNT/AVALL) *********

BOTA.AVALL:
   IF R = MAXAL - 1 THEN RETURN
   GOSUB ANAR.TOPE
   S = 1
   DO
	S = S + 1
	IF RB + S > MAXAL - 1 THEN
	   WHILE (RB + S > MAXAL - 1)
		 S = S - 1
	   WEND
	   EXIT DO
	END IF
   LOOP UNTIL S = 13
   RB = RB + S: R = RB: RESTA = 0: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREA2, RB, ALBARAN: X = XB
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
   RB = RB - S: R = RB: GOSUB LISTA
   GOSUB ANAR.TOPE
   GET AREA2, RB, ALBARAN: X = XB
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

' ********* Control de la barra (UP/DOWN)  *********

PUJACURSOR:
       IF X = 7 THEN
	  GET AREA2, R, ALBARAN
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = 1 THEN
	     X = 7: R = 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R - 1: X = 7
	  ScrollDown 18, 78, 6, 1, 1
	  GET AREA2, R, ALBARAN
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA2, R, ALBARAN
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  R = R - 1: X = X - 1
	  GET AREA2, R, ALBARAN
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

BAIXACURSOR:
       IF X = 19 THEN
	  GET AREA2, R, ALBARAN
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXAL - 1 THEN
	     R = MAXAL - 1
	     X = 19
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = 19
	  ScrollUp 18, 78, 6, 1, 1
	  GET AREA2, R, ALBARAN
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       ELSE
	  GET AREA2, R, ALBARAN
	  COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
	  IF R = MAXAL - 1 THEN
	     R = MAXAL - 1
	     COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
	     RETURN
	  END IF
	  R = R + 1: X = X + 1
	  GET AREA2, R, ALBARAN
	  COLOR COL(1, 0), COL(1, 1): GOSUB SHOWCURSOR
       END IF
       RETURN

'************************************************************************
' Mostra el cursor a la posici¢ que estÖ
'************************************************************************

SHOWCURSOR:
	  IF ALBARAN.MARCAT = "*" THEN C = 27 ELSE C = 15
	  LOCATE X, 2: PRINT ALBARAN.REFALBARAN; " ∫"; ALBARAN.PERSONA.NOM; ","; MID$(ALBARAN.PERSONA.COGNOMS, 1, 6); "∫";
	  PRINT USING "##,###,###.##∫##.##∫##.##∫##,###,###.##"; ALBARAN.TOTALBRUT; ALBARAN.DTO; ALBARAN.TIPOIVA; ALBARAN.TOTALNET
	  RETURN

LISTA:
    IF MAXAL = 1 THEN
       NOU = 2: Avis.Sonor (1)
       TECLA = Avis("AVIS:", "No hi ha cap albaran creat, ara es procedirÖ a crear-ne un", "Pitji una tecla...", 0)
       CALL CrearAlbarans(MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXFA, MAXST, MAXAL, 0, DH$, DP$, MI, DEVI$)
       IF NOU = 9 THEN
	  RESET
	  EXIT SUB
       END IF
       MascAlbs
       X = 7: R = 1: GOSUB LISTA: COLCURS = COL(0, 1): GOSUB SHOWCURSOR
    END IF

    COLOR COL(0, 0), COL(0, 1)
    FOR X = 7 TO 19: LOCATE X, 2: PRINT "          ∫                           ∫             ∫     ∫     ∫             ": NEXT
    FOR X = 7 TO 19
       IF R = MAXAL THEN EXIT FOR
       GET AREA2, R, ALBARAN
       COLOR COL(0, 0), COL(0, 1): GOSUB SHOWCURSOR
       R = R + 1
    NEXT
    RETURN

DELETERECORD:
    GET AREA2, R, ALBARAN
    IF ALBARAN.MARCAT = " " THEN
       ALBARAN.MARCAT = "*"
       PUT AREA2, R, ALBARAN
    ELSE
       ALBARAN.MARCAT = " "
       PUT AREA2, R, ALBARAN
    END IF

    GET AREA2, R, ALBARAN
    COLCURS = COL(1, 1): GOSUB SHOWCURSOR
    RETURN

'*********************************************************************
' OBRIR FITXERS
'*********************************************************************

OBRIFITXERS:
      AREA = FREEFILE: OPEN DIRECCF$ + "CONTROL.DSF" FOR RANDOM SHARED AS AREA LEN = LEN(CEMPRE)
      AREA2 = FREEFILE: OPEN DIRECCF$ + "ALBARAN.CAB" FOR RANDOM SHARED AS AREA2 LEN = LEN(ALBARAN)
      AREA3 = FREEFILE: OPEN DIRECCF$ + "ALBARAN.LIN" FOR RANDOM SHARED AS AREA3 LEN = LEN(LINALBA)
      AREA4 = FREEFILE: OPEN DIRECCF$ + "CLIENTS.DAT" FOR RANDOM SHARED AS AREA4 LEN = LEN(CLIENT)
      AREA5 = FREEFILE: OPEN DIRPLAF$ + "CAPÄALER.DAT" FOR RANDOM SHARED AS AREA5 LEN = LEN(CAP)
      AREAST = FREEFILE: OPEN DIRECCF$ + "STOCK.DAT" FOR RANDOM SHARED AS AREAST LEN = LEN(STOCK)
      AREANDX = FREEFILE: OPEN DIRECCF$ + "STOCK.NDX" FOR RANDOM SHARED AS AREANDX LEN = LEN(NDXFILE)
      AREA4NDX = FREEFILE: OPEN DIRECCF$ + "CLIENTS.NDX" FOR RANDOM SHARED AS AREA4NDX LEN = LEN(NDXCLIENT)
      AREANUM = FREEFILE: OPEN DIRECCF$ + "DOCUMENT.NUM" FOR RANDOM SHARED AS AREANUM LEN = LEN(DOCNUM)
      AREADOC = FREEFILE: OPEN DIRPLA$ + "ALB_MSK.LST" FOR RANDOM SHARED AS AREADOC LEN = LEN(TD)
      
      IF DIR$(DIRPLA$ + "FESTES.DAT") <> "" THEN
	 AREAFE = FREEFILE: OPEN DIRPLA$ + "FESTES.DAT" FOR INPUT AS AREAFE
	 INPUT #AREAFE, CADFE$
	 CLOSE AREAFE
      END IF

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

      AREA7 = FREEFILE: OPEN DIRECCI$ + FITXER$ FOR RANDOM SHARED AS AREA7 LEN = LEN(ARCHIMP)
      GET AREA7, 1, ARCHIMP
      CLOSE AREA7

'*********************************************************************
' INICIAR EL CONTROL DELS REGISTRES
'*********************************************************************
      GET AREA, 1, CEMPRE
      MAXFA = CEMPRE.MAXFACTURA
      MAXAL = CEMPRE.MAXALBARAN
      MAXCL = CEMPRE.MAXCLIENTS
      MAXST = CEMPRE.MAXSTOCK
      RETURN
END SUB

REM $STATIC
SUB AsignaAnyTreball
    AREAA = FREEFILE: OPEN DIRECCF$ + "ANYS.DAT" FOR RANDOM SHARED AS AREAA LEN = LEN(ANYS)
    GET AREAA, R, ANYS: ANY$ = LTRIM$(RTRIM$(ANYS.ANY))
    DIRECCF$ = DIRECCF$ + ANYS.ANY + "\"
    CLOSE AREAA
END SUB

REM $DYNAMIC
SUB CarregarAlbaran (R, MAXLIN)
    SHARED LIN$()

    GET AREA3, R, LINALBA
    FOR J = 1 TO MAXLIN
	LIN$(J, 1) = LINALBA.LINIA(J).CODART
	LIN$(J, 2) = LINALBA.LINIA(J).Concepte
	LIN$(J, 3) = STR$(LINALBA.LINIA(J).Preu)
	LIN$(J, 4) = STR$(LINALBA.LINIA(J).QUANTI)
	LIN$(J, 7) = STR$(LINALBA.LINIA(J).DTO)
	LIN$(J, 5) = STR$(LINALBA.LINIA(J).Import)
	LIN$(J, 6) = LINALBA.LINIA(J).MARCAR
    NEXT
    GET AREA2, R, ALBARAN
END SUB

FUNCTION CercarRecord% (camp$, INDEX() AS INDEXTYPE, MAXST, AREAST) STATIC
	 SHARED MIDPOINT

	 TOPRECORD = MAXST - 1
	 BottomRecord = 1

	 DO UNTIL (TOPRECORD < BottomRecord)
	    MIDPOINT = (TOPRECORD + BottomRecord) \ 2
	    TEST$ = RTRIM$(ForaEspai$(INDEX(MIDPOINT).Codi))

	    IF TEST$ = camp$ THEN
	       EXIT DO
	    ELSEIF camp$ > TEST$ THEN
	       BottomRecord = MIDPOINT + 1
	    ELSE
	       TOPRECORD = MIDPOINT - 1
	    END IF
	 LOOP

	 IF TEST$ = camp$ THEN
	    GET AREAST, INDEX(MIDPOINT).REGISTRE, STOCK
	    CercarRecord% = TRUE
	 ELSE
	    CercarRecord% = FALSE
	 END IF
END FUNCTION

REM $STATIC
FUNCTION ComprovaFitxersPass (modul$)

	  ComprovaFitxersPass = TRUE

	  TMP$ = ENVIRON$("TEMPORAL")
	  IF DIR$(TMP$ + "PASS.TMP") = "" THEN
	     ComprovaFitxersPass = FALSE
	  ELSE
	     DEF SEG = VARSEG(CFG)
	     BLOAD TMP$ + "PASS.TMP", VARPTR(CFG)
	     DEF SEG
	  END IF

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

	  ' Comprova que el modul que s'ha sol.licitat Çs el correcte

	  IF LTRIM$(RTRIM$(CA2$)) <> modul$ THEN
	     comprovafitxerpass = FALSE
	     TELCA = Avis("ERROR:", "ENTRADA INCORRECTA AL PROGRAMA", "PITJA UNA TECLA...", 0)
	     END
	  END IF

	  KILL TMP$ + "PASS.TMP"    ' Borra els fitxers de transpass
	  KILL TMP$ + "PASU.TMP"
	  KILL TMP$ + "PASE.TMP"
	  KILL TMP$ + "PROT.TMP"
END FUNCTION

REM $DYNAMIC
SUB CrearAlbarans (MAX, DTO, IVA, AREANUM, AREA3, AREA2, AREA4, AREAST, MAXFA, MAXST, MAXAL, EDIT, DH$, DP$, MI, DEVI$)
       SHARED LIN$(), TEXTOSMENUP() AS STRING
       SHARED UNIDAD$, DIRECCH$, DIRECC$, ANY$, F
       SHARED AREADOC

'*********************************************************************
' INDEXAR FITXER DEL STOCK
'*********************************************************************
       COLOR COL(0, 0), COL(0, 1): FINESTRA 10, 30, 14, 47, 1, CAIXA1
       COLOR 31, COL(0, 1): LOCATE 11, 31: PRINT "ESPERA UN MOMENT"
       COLOR 31, COL(0, 1): LOCATE 13, 31: PRINT " PROCESSANT ... "
       DIM INDEX(1 TO MAXST) AS INDEXTYPE
       DIM M$(2)
       DIM NDXFILE AS INDEXTYPE
       F = 0: GOSUB CARREGARINDEX

'*********************************************************************

       FLD(1).XCAMP = 2: FLD(1).LON = 18: FLD(1).TIPUS = ASCI: FLD(1).MASK = STRING$(18, "X")
       FLD(2).XCAMP = 21: FLD(2).LON = 40: FLD(2).TIPUS = ASCI: FLD(2).MASK = STRING$(40, "X")
       FLD(3).XCAMP = 51: FLD(3).LON = 9: FLD(3).TIPUS = ASCI: FLD(3).MASK = "999999999"
       FLD(4).XCAMP = 61: FLD(4).LON = 8: FLD(4).TIPUS = ASCI: FLD(4).MASK = "99999999"

'*********************************************************************
' INICIAR MASCARES DELS CAMPS
'*********************************************************************
       MASC$(1) = "\              \"             ' CODI ARTICLE
       MASC$(2) = "\" + SPACE$(27) + "\"         ' CONCEPTE
       MASC$(3) = "#,###,###"                     ' PREU
       MASC$(4) = "#,###.##"                     ' QUANTITAT
       MASC$(5) = "##,###,###"                   ' IMPORT
       MASCTOTAL$ = "##,###,###.##"              ' TOTAL

'*********************************************************************
' MIRA A VEURA SI HA DE EDITAR ALGUN REGISTRE
'*********************************************************************
       GetBackground 1, 1, 24, 79, RESBUF$
       
       TROBAT$ = "*"
       IF EDIT = 1 THEN
	  CALL CarregarAlbaran(R, MAX): q = R
	  IF ALBARAN.DOCUMENT = "A" THEN DOCU$ = "ALBARÖ"
	  IF ALBARAN.DOCUMENT = "F" THEN
	     BEEP: TECLA = Avis("AVIS:", "AlbarÖ ja facturat, no es pot tornar a modificar", "Pitji una tecla", 0)
	  END IF
       ELSE
	 FOR q = 1 TO MAXAL - 1
	      GET AREA2, q, ALBARAN
	      IF ALBARAN.MARCAT = "*" THEN
		 TROBAT$ = "Z"
		 EXIT FOR
	      END IF
	  NEXT
	 IF TROBAT$ = "Z" THEN   ' SI HA TROBAT ALGUN REGISTRE MARCAT
	     R = q
	  ELSE
	     R = MAXAL
	  END IF
	  DOCU$ = "ALBARÖ"

	  InitAlbaran (MAX)    ' POSSA A PUNT LES VARIABLES DEL ALBARAN
	  LIN$(1, 6) = "-"     ' DEIXA UNA LINIA DISPONIBLE PER EDITAR-LA
       END IF

'*********************************************************************
' INICIALITZAR VALORS DE L'ALBARAN
'*********************************************************************
       '
       ' INICIA N£MERO DE REFERäNCIA
       '
       IF EDIT = 0 THEN
	  REF$ = "0000000-A": ALB$ = LTRIM$(STR$(R)): L2 = 7
	  FOR L = LEN(ALB$) TO 1 STEP -1
	      MID$(REF$, L2, 1) = MID$(ALB$, L, 1): L2 = L2 - 1
	  NEXT
	  DA$ = MID$(DATE$, 9, 2)
	  IF VAL(ANY$) < VAL(DA$) THEN
	     MID$(REF$, 1, 2) = ANY$
	  ELSE
	     MID$(REF$, 1, 2) = DA$
	  END IF
	  ALBARAN.REFALBARAN = REF$
       END IF

       MascAlba (MAX)                               ' PINTAR MASCARA
       
       IF EDIT = 1 THEN GOSUB LLISTA.TROS.ALBARAN   ' SI L'ALBARA S'HA DE MODIFICAR QUE LLISTI UN TROS DEL ALBARA
       CALL ReadCapsalera(MAXAL, MAXCL)             ' DEMANAR DADES DE LA CAPÄALERA

       Missatges
       L = 1: C = 1: YC = 9: MODIF = 0: GUARDAT = 0
       GOSUB MARCACAMP      ' MARCA CAMP DE CODI ARTICLE

       DO
	 T$ = INKEY$
	 SELECT CASE T$
		CASE CHR$(0) + "S"
		     IF ALBARAN.DOCUMENT = "A" THEN
			GOSUB BORRA
		     END IF
		CASE CHR$(0) + "H"
		     GOSUB PUJA
		CASE CHR$(0) + "P"
		     GOSUB BAIXA
		CASE CHR$(13)
		     IF ALBARAN.DOCUMENT = "A" THEN
			GOSUB LLEGEIXCAMP
			GOSUB MARCACAMP
		     END IF
		CASE CHR$(6)
		     IF ALBARAN.DOCUMENT = "A" THEN GOSUB MENU
		CASE CHR$(27)
		     IF NOU = 2 THEN        ' SI ES EL PRIMER ALBARAN QUE ES FA
			ERASE INDEX
			NOU = 9
			PutBackground 1, 1, RESBUF$
			EXIT SUB
		     ELSE
			ERASE INDEX
			EXIT SUB
		     END IF
		CASE ELSE
		     COLOR COL(2, 0), COL(2, 1): LOCATE 20, 2: PRINT "Linia: "; : COLOR COL(0, 0), COL(0, 1): PRINT L;
		     LOCATE 20, 53: PRINT VAL(LIN$(L, 7));
		     LOCATE 20, 20: PRINT LIN$(L, 6)
	 END SELECT
       LOOP

'*********************************************************************
' MENU DELS ALBARANS
'*********************************************************************

MENU:
		     GetBackground 1, 1, 24, 79, RESBUF$
		     IF MODIF = 0 THEN
			ReadObserva                  ' EDITAR OBSERVACIONS
			ALBARAN.TOTALBRUT = TOTAL
			ALBARAN.DTO = DTO
			ALBARAN.TIPOIVA = IVA
			ReadCalculFactura            ' CALCULAR L'IVA I EL DTO DE LA FACTURA
		     ELSE
			IF MODIF = 1 THEN ReadCalculFactura  ' CALCULAR L'IVA I EL DTO DE LA FACTURA
		     END IF

		     DO
		       IF F = 1 THEN F$ = "FACTURA       " ELSE F$ = "ALBARA        "
		       TEXTOSMENUP(1) = " MODIFICAR ~CAPÄALERA    "
		       TEXTOSMENUP(2) = " MODIFICAR ~LINIES       "
		       TEXTOSMENUP(3) = " MODIFICAR ~OBSERVACIONS "
		       TEXTOSMENUP(4) = " ~GUARDAR ALBARA         "
		       TEXTOSMENUP(5) = STRING$(24, "ƒ")
		       TEXTOSMENUP(6) = " ~FACTURAR ALBARA        "
		       TEXTOSMENUP(7) = " ~IMPRIMIR " + F$
		       COLOR COL(1, 1), CCT
	    
		       CALL MenuBar(TEXTOSMENUP(), COMMENT$(), 0, CASO%, 14, 23, LEN(TEXTOSMENUP(1)), 7, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))
		       SELECT CASE CASO%
			      CASE 1
				   CALL ReadCapsalera(MAXAL, MAXCL)
			      CASE 2
				   MODIF = 1
				   PutBackground 1, 1, RESBUF$
				   RepintaCapsalera (MAXAL)
				   EXIT DO
			      CASE 3
				   PutBackground 1, 1, RESBUF$
				   RepintaCapsalera (MAXAL)
				   ReadObserva
			      CASE 4
				   IF EDIT = 1 THEN
				      RG = q
				   ELSE
				      RG = MAXAL: IF TROBAT$ = "Z" THEN RG = q
				   END IF
				   CALL GuardaAlbaran(RG, MAX, ASX)
			      CASE 6
				   IF EDIT = 1 THEN
				      RG = q
				   ELSE
				      RG = MAXAL: IF TROBAT$ = "Z" THEN RG = q
				   END IF
				   IF ASX <> 999 THEN
				      CALL FacturarAlbara(AREA, AREANUM, RG, MAXFA, MAX, DIRECC$)
				   END IF
			      CASE 7
				   IF F = 0 THEN DOCUM$ = "Albaran: " + ALBARAN.REFALBARAN
				   CALL ImprimirAlbaran(DP$, DEVI$, MAX, AREADOC, MI)
			      CASE 999
				   IF NOU = 2 THEN
				      ERASE INDEX: NOU = 9: EXIT SUB
				   ELSE
				      IF NOU = 3 THEN
					 ERASE INDEX
					 GOSUB GUARDADAR.NUMERO.ALBARA
					 PutBackground 1, 1, RESBUF$
					 EXIT SUB
				      END IF
				   END IF
				   ERASE INDEX
				   GOSUB GUARDADAR.NUMERO.ALBARA
			      CASE ELSE
		       END SELECT
		     LOOP
	  RETURN

'*********************************************************************
' CONTROL DEL CURSOR PER L'ALBARAN
'*********************************************************************

BORRA:
	 Import = VAL(LIN$(L, 5))
	 TOTAL = TOTAL - Import
	 IF TOTAL < 0 THEN TOTAL = 0
	 ALBARAN.TOTALBRUT = TOTAL

	 FOR CB = 1 TO 4
	     X = FLD(CB).XCAMP: LE = FLD(CB).LON
	     LIN$(L, CB) = SPACE$(LE)
	     COLOR COL(1, 1), CCT: LOCATE YC, X: PRINT STRING$(LE, " ");
	 NEXT
	 
	 IF LIN$(L, 6) <> "-" THEN
	    LIN$(L, 6) = "*"
	 END IF
	 LIN$(L, 5) = ""
	 IF LIN$(1, 6) = "-" THEN LIN$(1, 6) = "*"

	 LOCATE YC, 61: PRINT "        "
	 LOCATE YC, 70: PRINT "          "
	 COLOR COL(0, 0), COL(0, 1)
	 LOCATE 20, 67: PRINT USING MASCTOTAL$; TOTAL
	 GOSUB MOSTRA
	 GOSUB MARCACAMP
	 RETURN

PUJA:
	 IF L = 1 THEN SOUND 50, .5: TECLA = Avis("AVIS:", "Principi de l'albarÖ.", "Pitji una tecla...", 0): RETURN
	 X = FLD(C).XCAMP: LE = FLD(C).LON: T = FLD(C).TIPUS
	 COLOR COL(0, 0), COL(0, 1): LOCATE YC, X: PRINT STRING$(LE, " ");
	 LOCATE YC, X, 0: PRINT LIN$(L, C)
	 C = 1: YC = YC - 1: L = L - 1
	 LOCATE 20, 20: PRINT "       ";

	 IF YC < 9 THEN
	    ScrollDown 17, 78, 8, 1, 1
	    YC = 9: GOSUB MOSTRA
	    LOCATE 20, 20: PRINT "  AMUNT";
	 END IF
	 
	 GOSUB MOSTRA
	 GOSUB MARCACAMP
	 RETURN

BAIXA:
	 IF L = MAX THEN SOUND 50, .5: TECLA = Avis("AVIS:", "Final de l'albarÖ.", "Pitji una tecla...", 0): RETURN
	 X = FLD(C).XCAMP: LE = FLD(C).LON: T = FLD(C).TIPUS
	 COLOR COL(0, 0), COL(0, 1): LOCATE YC, X: PRINT STRING$(LE, " ");
	 LOCATE YC, X: PRINT LIN$(L, C)
	 LOCATE YC, X, 0
	 C = 1: YC = YC + 1: L = L + 1
	 LOCATE 20, 20: PRINT "       ";
	 IF YC > 18 THEN
	    ScrollUp 17, 78, 8, 1, 1
	    YC = 18: GOSUB MOSTRA
	    LOCATE 20, 20: PRINT "  AVALL";
	 END IF
	 
	 GOSUB MOSTRA
	 GOSUB MARCACAMP
	 RETURN

MOSTRA:
	 COLOR COL(0, 0), COL(0, 1)
	 LOCATE YC, 2: PRINT SPACE$(18); "∫"; SPACE$(29); "∫"; SPACE$(9); "∫"; SPACE$(8); "∫"; SPACE$(10)
	 IF LIN$(L, 6) = "*" OR LIN$(L, 6) = "-" THEN
	    LOCATE YC, 2: PRINT USING MASC$(1); LIN$(L, 1)
	    LOCATE YC, 2 + 18 + 1: PRINT USING MASC$(2); LIN$(L, 2)
	    LOCATE YC, 51: PRINT RTRIM$(LTRIM$(LIN$(L, 3)))
	    LOCATE YC, FLD(4).XCAMP: PRINT USING MASC$(4); VAL(LIN$(L, 4))
	    LOCATE YC, 70: PRINT USING MASC$(5); VAL(LIN$(L, 5))
	    LOCATE 20, 53: PRINT VAL(LIN$(L, 7))
	 END IF
	 RETURN

MARCACAMP:
	 X = FLD(C).XCAMP: LE = FLD(C).LON: T = FLD(C).TIPUS
	 COLOR COL(1, 0), COL(1, 1): LOCATE YC, X: PRINT STRING$(LE, " ");
	 LOCATE YC, X: PRINT LIN$(L, C)
	 LOCATE YC, X, 0
	 RETURN

'*********************************************************************
' RUTINA PER LLEGIR ELS CAMPS D'UNA LINIA
'*********************************************************************

LLEGEIXCAMP:
     COLOR COL(0, 0), COL(0, 1)
     LOCATE 23, 2: PRINT SPACE$(70);
     LOCATE 24, 2: PRINT SPACE$(70);
     LOCATE 22, 2: PRINT SPACE$(70);
     IF LIN$(L, 6) <> "*" AND LIN$(L, 6) <> "-" THEN
	LOCATE 23, 2: PRINT "VES PER ORDRE !!!": BEEP: Missatges
	RETURN
     END IF
     COLOR COL(2, 0), COL(2, 1)
     LOCATE 23, 2: PRINT "ENTER=PASSAR A L'ALTRE CAMP      ESC=SORTIR"
     SALE = 0: TROBAT = 0
      DO
	 SetMaxCamps 0: SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
	 X = FLD(C).XCAMP: T = FLD(C).TIPUS: M$ = LTRIM$(RTRIM$(FLD(C).MASK))
	 SetInitCamp 0, YC, X, T, 0, M$, ""

	 InsertValueCamp 0, LTRIM$(LIN$(L, C)): VELL$ = LIN$(L, C)

	 VALUE = ReadCamp(0)
	 SELECT CASE VALUE
		CASE F1
		     SHELL "BKHLP ALB_LINIES"
		CASE F2
		     IF C = 1 OR C = 2 THEN GOSUB LLISTA.STOCK
		CASE F3 TO F10
		     SOUND 50, .5
		CASE SALIR
		     Missatges
		     X = FLD(1).XCAMP: GOSUB MOSTRA: C = 1
		     RETURN
		CASE ELSE
	 END SELECT

	 LIN$(L, C) = ValueCamp$(0)
	 CAMPTEMP$ = ForaEspai$(ValueCamp$(0))

	    IF C = 1 THEN
	       IF CercarRecord%(CAMPTEMP$, INDEX(), MAXST, AREAST) THEN
		  IF STOCK.MARCAT <> "*" THEN
		     LIN$(L, 2) = STOCK.DESCRIPCIO
		     IF STOCK.EXISTENCIA < STOCK.STOCKMIN THEN
			COLOR 27: BEEP: LOCATE 22, 2: PRINT "Aquest article estÖ baix minims            "
		     ELSE
			IF STOCK.EXISTENCIA = 0 THEN
			   COLOR 27: BEEP: LOCATE 22, 2: PRINT "Aquest article estÖ amb existäncies 0"
			END IF
		     END IF
		     TROBAT = 999
		  END IF
	       END IF
	    END IF

	    IF C = 4 THEN
	       IF TROBAT = 999 THEN                        ' RESTAR AL STOCK
		  LOCK AREAST, INDEX(MIDPOINT).REGISTRE
		  GET AREAST, INDEX(MIDPOINT).REGISTRE, STOCK
		  SAC = STOCK.EXISTENCIA - VAL(LIN$(L, 4))
		  GOSUB ACTUALIZHISTORIC
		  STOCK.EXISTENCIA = SAC
		  PUT AREAST, INDEX(MIDPOINT).REGISTRE, STOCK
		  UNLOCK AREAST, INDEX(MIDPOINT).REGISTRE
	       END IF
	    END IF

	 IF C <= 2 THEN
	    LOCATE YC, X: PRINT USING MASC$(C); ValueCamp$(0)
	 ELSE
	     LOCATE YC, X: PRINT LTRIM$(STR$(VAL(ValueCamp$(0))))
	 END IF

	 DeleteCamp 0: C = C + 1
	 GOSUB MOSTRA

	 IF C = 3 THEN
	    IF STOCK.PVPACONSE <> 0 THEN
	       LIN$(L, 3) = LTRIM$(STR$(STOCK.PVPACONSE)): LIN$(L, 7) = ""
	    END IF
	 END IF
	 
	 IF C = 5 THEN
	    SetMaxCamps 0: SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
	    SetInitCamp 0, 20, 53, NUM, 0, "999", ""
	
	    DeleteCamp 0
	    InsertValueCamp 0, LIN$(L, 7)
	    IF ReadCamp(0) = SALIR THEN BEEP
	    LIN$(L, 7) = ValueCamp$(0)

	    Preu = VAL(LIN$(L, 3))
	    QUANT = VAL(LIN$(L, 4))
	    DESCOMPTE = VAL(LIN$(L, 7))

	    Import = QUANT * Preu - ((QUANT * Preu) * DESCOMPTE) / 100
	    LIN$(L, C) = STR$(Import)

	    SUBTOTAL = 0        ' TORNA A RECALCULAR ELS TOTALS
	    FOR J = 1 TO MAX
		SUBTOTAL = SUBTOTAL + VAL(LIN$(J, 5))
	    NEXT

	    TOTAL = SUBTOTAL
	    ALBARAN.TOTALBRUT = TOTAL
	    LOCATE YC, 70: PRINT USING MASC$(5); Import
	    COLOR COL(0, 0), COL(0, 1): LOCATE 20, 67: PRINT USING MASCTOTAL$; TOTAL
	    
	    IF L = MAX THEN
	       IF LIN$(L, 6) = "-" THEN LIN$(L, 6) = "*"
	    ELSE
	       IF LIN$(L, 6) = "-" THEN
		  LIN$(L, 6) = "*"
	       END IF
	       IF LIN$(L + 1, 6) <> "-" AND LIN$(L + 1, 6) <> "*" THEN
		  LIN$(L + 1, 6) = "-"
	       END IF
	    END IF

	    IF YC = 18 THEN
	       ScrollUp 17, 78, 8, 1, 1
	       YC = 18: L = L + 1: C = 1: SALE = 1
	       GOSUB MOSTRA
	    ELSE
	       C = 1: L = L + 1: YC = YC + 1: SALE = 1
	    END IF
	 END IF
      LOOP UNTIL SALE = 1
      COLOR COL(0, 0), COL(0, 1): LOCATE 22, 2: PRINT SPACE$(70);
      Missatges
      RETURN

LLISTA.TROS.ALBARAN:
      YC = 9
      FOR L = 1 TO 10
	  GOSUB MOSTRA: YC = YC + 1
      NEXT
      TOTAL = ALBARAN.TOTALBRUT
      COLOR COL(0, 0), COL(0, 1)
      LOCATE 20, 67: PRINT USING MASCTOTAL$; TOTAL
      RETURN

LLISTA.STOCK:
       IF MAXST = 1 THEN
	  TECLA = Avis("AVIS:", "La base de dades dels articles estÖ buida", "PITJA UNA TECLA...", 0): RETURN
       END IF
       DIM LL$(MAXST - 1)
       GetBackground 3, 9, 21, 71, LLISTA$
       SetScoreBoard SOFF: LOCATE , , 0
       FOR q = 1 TO MAXST - 1
	   GET AREAST, q, STOCK
	   IF STOCK.MARCAT = " " THEN
	      Codi$ = STOCK.Codi
	      N$ = MID$(STOCK.DESCRIPCIO, 1, 38)
	      LL$(q) = Codi$ + " " + N$
	   END IF
       NEXT: CAM$ = LTRIM$(RTRIM$(ValueCamp$(0))): IF C = 1 THEN b = 1 ELSE b = 12
       ASEL = Achoice(3, 9, 20, 70, q - 1, LL$(), COL(), "Codi               Descripci¢                               ", b, CAM$)
       IF ASEL = 0 OR ASEL = -14 OR ASEL = -13 THEN
	  PutBackground 3, 9, LLISTA$
	  ERASE LL$: RETURN
       END IF
       IF ASEL <= 0 THEN ASEL = 1
       GET AREAST, ASEL, STOCK
       LIN$(L, 1) = STOCK.Codi
       LIN$(L, 2) = STOCK.DESCRIPCIO
       LIN$(L, 3) = STR$(STOCK.PVPACONSE)
       PutBackground 3, 9, LLISTA$
       ERASE LL$
      RETURN

GUARDADAR.NUMERO.ALBARA:
      GetBackground 1, 1, 24, 79, FACTU$
      IF GUARDAT = 999 THEN
	 IF EDIT = 0 THEN
	    IF TROBAT$ <> "Z" THEN
	       CEMPRE.MAXALBARAN = MAXAL + 1
	       PUT AREA, 1, CEMPRE
	       MAXAL = CEMPRE.MAXALBARAN
	       EXIT SUB
	      
	    END IF
	 END IF
      ELSE
	 COLOR COL(0, 0), COL(0, 1)
	 FINESTRA 10, 20, 14, 65, 1, CAIXA1
	 COLOR 28, COL(0, 1)
	 LOCATE 11, 21: PRINT "         L'albaran no estÖ guardat": COLOR 14
	 LOCATE 13, 21: PRINT "    ® Estas segur que vols sortir (S/N) ?"
	   DO
	     T$ = INKEY$
	   LOOP UNTIL T$ <> ""
	   SELECT CASE UCASE$(T$)
		  CASE "S"
		       PutBackground 1, 1, RESBUF$
		       EXIT SUB
		  CASE "N"
		       PutBackground 1, 1, FACTU$
		       RETURN
		  CASE ELSE
		       PutBackground 1, 1, FACTU$
		       RETURN
	   END SELECT
      END IF
      RETURN

CARREGARINDEX:
      FOR RI = 1 TO MAXST                     ' COL.LOCAR A MEMïRIA L'INDEX
	  GET AREANDX, RI, NDXFILE
	  INDEX(RI).REGISTRE = NDXFILE.REGISTRE
	  INDEX(RI).Codi = NDXFILE.Codi
      NEXT
      RETURN

ACTUALIZHISTORIC:
       RETURN
END SUB

SUB FacturarAlbara (AREA, AREANUM, RG, MAXFA, MAXLIN, DIRECC$) STATIC
      SHARED DIRECCF$, DEVI$, DOCUM$
      SHARED F
    
      IF F = 1 THEN
	 TECLA = Avis("AVIS:", "Aquest albarÖ ja ha estat facturat!", "Pitji una tecla...", 0)
	 EXIT SUB
      ELSE
	 F = 1
      END IF

      AREA8 = FREEFILE: OPEN DIRECCF$ + "FACTURES.CAB" FOR RANDOM SHARED AS AREA8 LEN = LEN(FACTURA)
      AREA9 = FREEFILE: OPEN DIRECCF$ + "FACTURES.LIN" FOR RANDOM SHARED AS AREA9 LEN = LEN(LINFAC)

      FOR q = 1 TO MAXFA - 1
	  GET AREA8, q, FACTURA
	  IF FACTURA.MARCAT = "*" THEN
	     TROBAT$ = "Z"
	     EXIT FOR
	  END IF
      NEXT

      IF TROBAT$ = "Z" THEN   ' SI HA TROBAT ALGUN REGISTRE MARCAT
	 R = q
      ELSE
	 R = MAXFA
	 MAXFA = MAXFA + 1
	 GET AREA, 1, CEMPRE
	 CEMPRE.MAXFACTURA = MAXFA
	 PUT AREA, 1, CEMPRE
      END IF

	   TIPUS% = IniciaRef(AREANUM, R, FALSE, "")
	   IF TIPUS% = 999 OR TIPUS% = 888 THEN
	      CLOSE AREA8, AREA9
	      EXIT SUB
	   END IF
	   DOCUM$ = "Factura: " + FACTURA.REFFACTURA
	   FACTURA.DOCUMENT = "F"
	   FACTURA.DADA = FormatD$(Now#, "dd/mm/yyyy")
	   FACTURA.CODCLIENT = ALBARAN.CODCLIENT
	   FACTURA.PERSONA.NOM = ALBARAN.PERSONA.NOM
	   FACTURA.PERSONA.COGNOMS = ALBARAN.PERSONA.COGNOMS
	   FACTURA.PERSONA.DNI = ALBARAN.PERSONA.DNI
	   FACTURA.PERSONA.DIRECCIO = ALBARAN.PERSONA.DIRECCIO
	   FACTURA.PERSONA.POBLACIO = ALBARAN.PERSONA.POBLACIO
	   FACTURA.PERSONA.TELEFON1 = ALBARAN.PERSONA.TELEFON1
	   FACTURA.PERSONA.TELEFON2 = ALBARAN.PERSONA.TELEFON2
	   FACTURA.PERSONA.CPOSTAL = ALBARAN.PERSONA.CPOSTAL
	   FACTURA.PERSONA.DTO = ALBARAN.PERSONA.DTO
	   FACTURA.PERSONA.FORMAPAGO = ALBARAN.PERSONA.FORMAPAGO
	   FACTURA.DTO = ALBARAN.DTO
	   FACTURA.TIPOIVA = ALBARAN.TIPOIVA
	   FACTURA.TIPOIVA = ALBARAN.TIPOIVA
	   FACTURA.BASEIMPONIBLE = ALBARAN.BASEIMPONIBLE
	   FACTURA.TOTALBRUT = ALBARAN.TOTALBRUT
	   FACTURA.TOTALIVA = ALBARAN.TOTALIVA
	   FACTURA.TOTALNET = ALBARAN.TOTALNET
	   FACTURA.OBSERVA(1) = "Factura corresponent a l'albarÖ Nß:" + ALBARAN.REFALBARAN
	   FACTURA.OBSERVA(2) = "De la data " + ALBARAN.DADA
	   FACTURA.MARCAT = " "
	   PUT AREA8, R, FACTURA

	   FOR J = 1 TO MAXLIN
	       LINFAC.LINIA(J).REFALBARAN = ALBARAN.REFALBARAN
	       LINFAC.LINIA(J).CODART = LIN$(J, 1)
	       LINFAC.LINIA(J).Concepte = LIN$(J, 2)
	       LINFAC.LINIA(J).Preu = VAL(LIN$(J, 3))
	       LINFAC.LINIA(J).QUANTI = VAL(LIN$(J, 4))
	       LINFAC.LINIA(J).DTO = VAL(LIN$(J, 7))
	       LINFAC.LINIA(J).Import = VAL(LIN$(J, 5))
	       LINFAC.LINIA(J).MARCAR = LIN$(J, 6)
	   NEXT
	   PUT AREA9, R, LINFAC

	   GET AREANUM, 1, DOCNUM
	   DOCNUM.FACTNUM(TIPUS%).MAXFACT = DOCNUM.FACTNUM(TIPUS%).MAXFACT + 1
	   PUT AREANUM, 1, DOCNUM
    
	   Avis.Sonor (1)
	   TECLA = Avis("Avis:", "L'albarÖ s'ha facturat.", "Pitji una tecla...", 0)
	   CLOSE AREA8, AREA9
	   CALL GuardaAlbaran(RG, MAXLIN, ASX)
END SUB

SUB GuardaAlbaran (RG, MAXLIN, ASX)
    SHARED F

    IF ALBARAN.MARCAT = "*" THEN ALBARAN.MARCAT = " "
    IF F = 0 THEN
       ALBARAN.DOCUMENT = "A"
    ELSE
       IF F = 1 THEN ALBARAN.DOCUMENT = "F"
    END IF
    NOU = 3

    PUT AREA2, RG, ALBARAN
    FOR J = 1 TO MAXLIN
	LINALBA.LINIA(J).CODART = LIN$(J, 1)
	LINALBA.LINIA(J).Concepte = LIN$(J, 2)
	LINALBA.LINIA(J).Preu = VAL(LIN$(J, 3))
	LINALBA.LINIA(J).QUANTI = VAL(LIN$(J, 4))
	LINALBA.LINIA(J).DTO = VAL(LIN$(J, 7))
	LINALBA.LINIA(J).Import = VAL(LIN$(J, 5))
	LINALBA.LINIA(J).MARCAR = LIN$(J, 6)
    NEXT
    PUT AREA3, RG, LINALBA
    GUARDAT = 999

END SUB

REM $STATIC
SUB ImprimirAlbaran (DP$, DEVI$, MAX, AREADOC, MI)
    SHARED DIRECCT$, CADFE$, DOCUM$
    
    '$DYNAMIC
    DIM CLI(0 TO 7) AS STRING
    DIM ALB(0 TO 8) AS STRING
    DIM LINI(0 TO 5) AS STRING

    CLI(0) = ALBARAN.CODCLIENT: CLI(1) = ALBARAN.PERSONA.NOM
    CLI(2) = LTRIM$(RTRIM$(ALBARAN.PERSONA.COGNOMS)): CLI(3) = "D.N.I. " + ALBARAN.PERSONA.DNI
    CLI(4) = RTRIM$(ARCHIMP.NOENSANCHADO) + ALBARAN.PERSONA.DIRECCIO: CLI(5) = LTRIM$(RTRIM$(ALBARAN.PERSONA.CPOSTAL)) + " " + RTRIM$(LTRIM$(ALBARAN.PERSONA.POBLACIO))
    CLI(6) = " Tels." + ALBARAN.PERSONA.TELEFON1: CLI(7) = "      " + ALBARAN.PERSONA.TELEFON2

    TO$ = FormatC$(ALBARAN.TOTALBRUT, "##.###.###"): TOTB$ = TO$ + SPACE$(10 - LEN(TO$))
    BA$ = FormatC$(ALBARAN.BASEIMPONIBLE, "##.###.###"): BASE$ = BA$ + SPACE$(10 - LEN(BA$))
    DT$ = FormatC$(ALBARAN.DTO, "###"): DTO$ = DT$ + SPACE$(3 - LEN(DT$))
    IV$ = FormatC$(ALBARAN.TIPOIVA, "###"): IVA$ = IV$ + SPACE$(3 - LEN(IV$))
    TI$ = FormatC$(ALBARAN.TOTALIVA, "##.###.###"): TIVA$ = TI$ + SPACE$(10 - LEN(TI$))

    ALB(0) = RTRIM$(ARCHIMP.ENSANCHADO) + DOCUM$ + RTRIM$(ARCHIMP.NOENSANCHADO)
    ALB(1) = TOTB$: ALB(2) = BASE$
    ALB(3) = DTO$: ALB(4) = IVA$
    ALB(5) = TIVA$: ALB(6) = FormatD$(ALBARAN.TOTALNET, "##.###.###")
    ALB(7) = ALBARAN.PERSONA.FORMAPAGO: ALB(8) = ""

    GetBackground 1, 1, 25, 80, FACTU$
    COLOR 15, 2: FINESTRA 10, 30, 14, 45, 1, CAIXA1
    COLOR 15, 2: LOCATE 11, 31: PRINT "ACTUALITZANT"
    COLOR 15, 2: LOCATE 13, 31: PRINT "  FITXERS   "

    ' Crear el fitxer temporal d'impressi¢
    AREATXT = FREEFILE
    OPEN DIRECCT$ + "ALBARAN.TXT" FOR OUTPUT SHARED AS AREATXT

    PAG = 1
    GET AREA5, 1, CAP

    IF MI = 0 THEN
       TECLA = Avis("ERROR:", "No hi ha seleccionat cap tipus de mode d'impressi¢", "PITJA UNA TECLA...", 0)
       CLOSE AREATXT
       EXIT SUB
    END IF

    GET AREADOC, MI, TD

    LI = 1: L = 1
    SALT.PAG% = 1: MSK.PEUSP% = 0
    GOSUB PLANTILLA
    
    DO
	TECLA$ = INKEY$
	IF LI >= MAXLINS THEN
	   FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT
	   GOSUB Missatge
	   FOR SALT = 1 TO SALT.F%: PRINT #AREATXT, "": NEXT
	   PAG = PAG + 1: LI = 1
	   SALT.PAG% = 1: GOSUB PLANTILLA
	END IF
	
	IF LIN$(L, 6) = "*" THEN
	   GOSUB IMPRI.LINIA
	   L = L + 1: LI = LI + 1
	END IF
	
    LOOP UNTIL LIN$(L, 6) = "-" OR TECLA$ = CHR$(27)

    FOR LA = LI TO MAXLINS: PRINT #AREATXT, "": NEXT

    MSK.PEUSP% = 1: GOSUB PLANTILLA
    CLOSE #AREATXT

    COLOR 31, 2: LOCATE 11, 31: PRINT "  IMPRIMINT  "
    COLOR 31, 2: LOCATE 13, 31: PRINT "   FITXERS   "

    ImprimeixFitxerTXT DIRECCT$ + "ALBARAN.TXT", DEVI$, 100
    PutBackground 1, 1, FACTU$
    ERASE ALB, CLI, LINI
    EXIT SUB
'****************************************************************

PLANTILLA:
    AREAPLA = FREEFILE: NOMP$ = LTRIM$(RTRIM$(DIRPLA$ + TD.FITXER))
    OPEN NOMP$ FOR INPUT SHARED AS AREAPLA

    '****************************************************************
    DO UNTIL EOF(AREAPLA)
       LINE INPUT #AREAPLA, LINIA$
       LL = LEN(LINIA$): GOSUB BUSCA.INSTR
       IF MSK.SURT% = 1 THEN EXIT DO
    LOOP
    '****************************************************************
    MSK.SURT% = 0: SALT.PAG% = 0
    CLOSE #AREAPLA
    RETURN

BUSCA.INSTR:
    CL = 1
    WHILE CL < LL
	IF MID$(LINIA$, CL, 1) = "[" THEN
	   TOPE1 = CL + 1: TOPE2 = INSTR(TOPE1, LINIA$, "]") - 1
	   LC = TOPE2 - TOPE1
	   IN$ = MID$(LINIA$, TOPE1, LC + 1)

	   IF MSK.PEUSP% = 1 THEN
	      IF MID$(IN$, 1, 10) = "PEU_PAGINA" THEN
		 MSK.PEUSP% = 0
	      END IF
	   ELSE
	      SELECT CASE UCASE$(IN$)
		  CASE "CAPÄALERA"
		       GOSUB CAPSAC
		  CASE "MSG_FESTES"
		       PRINT #AREATXT, SPACE$((40 \ 2) - (LEN(CADFE$) \ 2)) + RTRIM$(ARCHIMP.ENSANCHADO) + CADFE$ + RTRIM$(ARCHIMP.NOENSANCHADO)
		  CASE "PAGINA"
		       PRINT #AREATXT, USING "\   \"; FormatD$(PAG, "#.###");
		  CASE "DATA"
		       PRINT #AREATXT, USING "\        \"; ALBARAN.DADA;
		  CASE ELSE
		       IF MID$(IN$, 1, 3) = "MAX" THEN MAXLINS = VAL(MID$(IN$, 5, 2))
		       IF MID$(IN$, 1, 6) = "SEPARA" THEN SALT.F% = VAL(MID$(IN$, 8, 2))
		       IF MID$(IN$, 1, 6) = "CLIENT" THEN
			  PRINT #AREATXT, CLI(VAL(MID$(IN$, 8, 1)));
		       ELSE
			  IF MID$(IN$, 1, 3) = "ALB" THEN
			     PRINT #AREATXT, ALB(VAL(MID$(IN$, 5, 1)));
			  ELSE
			    IF MID$(IN$, 1, 5) = "LINIA" THEN
				  MSK.SURT% = 1: RETURN
			    END IF
			  END IF
		       END IF
	      END SELECT
	      CL = CL + LC + 2
	   END IF
	ELSE
	   IF MSK.PEUSP% = 0 AND MID$(LINIA$, 1, 12) <> "[PEU_PAGINA]" THEN PRINT #AREATXT, MID$(LINIA$, CL, 1);
	END IF
	CL = CL + 1
    WEND
    IF MSK.PEUSP% = 0 THEN PRINT #AREATXT, ""
    RETURN

IMPRI.LINIA:
    LINI(0) = LIN$(L, 1)
    LINI(1) = LIN$(L, 2)
    LINI(2) = LIN$(L, 4)
    LINI(3) = LIN$(L, 3)
    LINI(4) = LIN$(L, 7)
    LINI(5) = LIN$(L, 5)
    
    IF VAL(LINI(2)) = 0 THEN
       QUANT$ = "        "
    ELSE
       q$ = LTRIM$(FormatC$(VAL(LINI(2)), "###.###,0")): QUANT$ = SPACE$(9 - LEN(q$)) + q$
    END IF
    
    p$ = FormatC$(VAL(LINI(3)), "#.###.###"): Preu$ = SPACE$(9 - LEN(p$)) + p$
    i$ = FormatC$(VAL(LINI(5)), "##.###.###"): Import$ = SPACE$(10 - LEN(i$)) + i$
    PRINT #AREATXT, USING "\       \ "; QUANT$;
    PRINT #AREATXT, " " + LINI(1);
    PRINT #AREATXT, USING "\       \"; Preu$;
    PRINT #AREATXT, USING "\\"; FormatC$(VAL(LINI(4)), "##");
    PRINT #AREATXT, USING "   \        \"; Import$
    RETURN

CAPSAC:
    PRINT #AREATXT, RTRIM$(ARCHIMP.ENSANCHADO) + RTRIM$(CAP.LINIES(0)) + RTRIM$(ARCHIMP.NOENSANCHADO)
    FOR i = 1 TO 4: PRINT #AREATXT, CAP.LINIES(i): NEXT
    RETURN

Missatge:
    PRINT #AREATXT, ""
    PRINT #AREATXT, ""
    PRINT #AREATXT, "                      SUMA I CONTINUA A LA PÖGINA SEGöENT...."
    PRINT #AREATXT, ""
    PRINT #AREATXT, ""
    PRINT #AREATXT, ""
    RETURN
END SUB

REM $STATIC
SUB inicialitzacolors
    AREAC = FREEFILE: OPEN DIRECCF$ + "COLORS.CFG" FOR RANDOM SHARED AS AREAC LEN = LEN(COLORS)
    GET AREAC, 1, COLORS
    COL(0, 0) = COLORS.COL(0, 0): COL(0, 1) = COLORS.COL(0, 1)
    COL(1, 0) = COLORS.COL(1, 0): COL(1, 1) = COLORS.COL(1, 1)
    COL(2, 0) = COLORS.COL(2, 0): COL(2, 1) = COLORS.COL(2, 1)
    CLOSE AREAC
END SUB

SUB inicialitzadirs
    UNIDAD$ = LTRIM$(RTRIM$(CFG.DRIVE))

    DBF$ = LTRIM$(RTRIM$(CFG.DDADE))             ' Assignar direcctoris
    SCR$ = LTRIM$(RTRIM$(CFG.DPANT))
    MSK$ = LTRIM$(RTRIM$(CFG.DRECU))
    HLP$ = LTRIM$(RTRIM$(CFG.DHELP))
    SYS$ = LTRIM$(RTRIM$(CFG.DSIST))

    DIRECCF$ = DBF$                    ' Subdirecctori de les base de dades
    DIRECCP$ = SCR$                    ' Subdirecctori de les pantalles
    DIRECCR$ = MSK$                    ' Subdirecctori de mascares, errors, etc...
    DIRECCHE$ = HLP$                   ' Subdirecctori de ajuda
    DIRECCI$ = UNIDAD$ + "\IMPRESOR\"  ' Subdirecctori de les impresores
    DIRECCT$ = DBF$ + "TEXTOS\"        ' Subdirectori on es guarden els darrers documents impresos
    DIRECCH$ = DBF$ + "HISTORIC\"
    DIRECCPL$ = DBF$ + "PLANTILL\"     ' Subdirecctori de les plantilles dels documents
    DIRPLA$ = DIRECCPL$
END SUB

SUB inicialitzavars
    SHARED MAXLINS, MAXFAC, IVA, DTO, R, DEV$, IMPRESORA
    SHARED MI

    MAXLINS = USR.MAXLINS
    MAXFAC = USR.LINALBA
    IVA = EMPRES.IVA
    DTO = EMPRES.DTO
    R = EMPRES.ANY
    DEV$ = LTRIM$(RTRIM$(USR.DEVICE))
    IMPRESORA = USR.IMPRESORA
    MI = USR.MODEIMPRES

    SetFormatCC (34)             ' Asigna el tipus de pais per la data
				 ' i els valors monetaris.
END SUB

REM $DYNAMIC
FUNCTION IniciaRef (AREANUM, R, MARCAT, REFOLD$)
	  DIM OP$(1 TO 10)

	  GET AREANUM, 1, DOCNUM
	  MAXDOC% = DOCNUM.MAXNUM
	  FOR RE = 1 TO MAXDOC%
	      OP$(RE) = DOCNUM.FACTNUM(RE).CONFACT
	  NEXT

	  CALL Menu2(OP$(), CASO%, 10, 10, LEN(OP$(1)) + 2, MAXDOC%, COL(0, 0), COL(0, 1), COL(1, 0), COL(1, 1))

	  IF CASO% = 999 OR CASO% = 888 THEN
	     IniciaRef = CASO%
	     ERASE OP$
	     EXIT FUNCTION
	  ELSE
	     REF$ = DOCNUM.FACTNUM(CASO%).NUMFACT
	     ALB$ = LTRIM$(STR$(DOCNUM.FACTNUM(CASO%).MAXFACT))
	  END IF

	  L2 = 6
	  FOR L = LEN(ALB$) TO 1 STEP -1
	      MID$(REF$, L2, 1) = MID$(ALB$, L, 1): L2 = L2 - 1
	  NEXT
	  MID$(REF$, 1, 2) = MID$(DATE$, 9, 2)

	  IniciaRef = CASO%
	  FACTURA.REFFACTURA = REF$
	  ERASE OP$
END FUNCTION

SUB InitAlbaran (MAXLIN)
    ALBARAN.DOCUMENT = "A"
    ALBARAN.REFALBARAN = CHR$(0)               ' INCIAR ALBARAN
    ALBARAN.CODCLIENT = CHR$(0)
    ALBARAN.DADA = FormatD$(Now#, "dd/mm/yyyy")
    ALBARAN.PERSONA.CODICLIENT = CHR$(0)
    ALBARAN.PERSONA.NOM = CHR$(0)
    ALBARAN.PERSONA.COGNOMS = CHR$(0)
    ALBARAN.PERSONA.DNI = CHR$(0)
    ALBARAN.PERSONA.POBLACIO = CHR$(0)
    ALBARAN.PERSONA.TELEFON1 = CHR$(0)
    ALBARAN.PERSONA.TELEFON2 = CHR$(0)
    ALBARAN.PERSONA.DTO = 0
    ALBARAN.PERSONA.FORMAPAGO = CHR$(0)
    ALBARAN.PERSONA.CPOSTAL = CHR$(0)
    ALBARAN.PERSONA.BANC = ""
    ALBARAN.PERSONA.COMPTE = ""
    ALBARAN.OBSERVA(1) = ""
    ALBARAN.OBSERVA(2) = ""
    ALBARAN.TOTALBRUT = 0
    ALBARAN.BASEIMPONIBLE = 0
    ALBARAN.TIPOIVA = 0
    ALBARAN.TOTALIVA = 0
    ALBARAN.DTO = 0
    ALBARAN.TOTALNET = 0
    TOTAL = 0
    FOR J = 1 TO MAXLIN
	LINALBA.LINIA(J).CODART = CHR$(0)
	LINALBA.LINIA(J).Concepte = CHR$(0)
	LINALBA.LINIA(J).Preu = 0
	LINALBA.LINIA(J).QUANTI = 0
	LINALBA.LINIA(J).DTO = 0
	LINALBA.LINIA(J).Import = 0
	LINALBA.LINIA(J).MARCAR = CHR$(0)
	LIN$(J, 1) = "": LIN$(J, 2) = "": LIN$(J, 3) = ""
	LIN$(J, 4) = "": LIN$(J, 5) = "": LIN$(J, 6) = "": LIN$(J, 7) = ""
    NEXT

END SUB

SUB LlistarAlbarans (AREA5, AREA2, DEVI$)
    SHARED DIRECCT$, DIRECCF$

    AREATXT = FREEFILE
    OPEN DIRECCT$ + "ALBARANS.TXT" FOR OUTPUT AS AREATXT
    AREA8 = FREEFILE: OPEN DIRECCF$ + "FACTURES.CAB" FOR RANDOM SHARED AS AREA8 LEN = LEN(FACTURA)
    AREA9 = FREEFILE: OPEN DIRECCF$ + "FACTURES.LIN" FOR RANDOM SHARED AS AREA9 LEN = LEN(LINFAC)
    RMF = LOF(AREA9) \ LEN(LINFAC)

    PAG = 1: L = 1
    GET AREA5, 1, CAP   ' AGAFAR CAPÄALERA DEL FITXER DE CAPÄALERES
    GOSUB CAP.LIST      ' IMPRIMIR CAPÄALERA

    ' DEFINIR MASCARA DE LES LINIES
    MASCARA$ = "  \" + SPACE$(9) + "\\" + SPACE$(47) + "\    ##,###,###.## ##.## ##.## ##,###,###.## \      \ \      \   !"
    MAXL = 52: TOTALB& = 0: TOTALN& = 0
    FOR Rl = 1 TO CEMPRE.MAXALBARAN - 1
	CALL FinestraEstat("Processant albarans...", 0)
	GET AREA2, Rl, ALBARAN
	GOSUB PRINTLINIA
	TOTALB& = TOTALB& + ALBARAN.TOTALBRUT
	TOTALN& = TOTALN& + ALBARAN.TOTALNET
	IF L >= MAXL THEN
	   L = 1: PAG = PAG + 1
	   PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + " " + STRING$(77, "ƒ") + RTRIM$(ARCHIMP.COMPRIMIDO)
	   PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
	   GOSUB CAP.LIST
	END IF
    NEXT
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO) + " " + STRING$(77, "ƒ") + RTRIM$(ARCHIMP.COMPRIMIDO)
    PRINT #AREATXT, USING MASCARA$; SPACE$(9); SPACE$(31) + "Suma dels Totals: "; TOTALB&; 0; 0; TOTALN&

    PRINT #AREATXT, RTRIM$(ARCHIMP.SALTOPAGINA)
    PRINT #AREATXT, RTRIM$(ARCHIMP.NOCOMPRIMIDO)
    CLOSE #AREATXT
    CLOSE AREA8, AREA9

    CALL ImprimeixFitxerTXT(DIRECCT$ + "ALBARANS.TXT", DEVI$, 180)' DIRECCIONAR EL FITXER CAP A LA IMPRESORA
    EXIT SUB

' **************************************************************************
' DEFINIR CAPÄALERA DELS LLISTATS
' **************************************************************************

CAP.LIST:
    PRINT #AREATXT, ""
    PRINT #AREATXT, " "; RTRIM$(ARCHIMP.ENSANCHADO) + CAP.LINIES(0) + RTRIM$(ARCHIMP.NOENSANCHADO)
    PRINT #AREATXT, ""
    PRINT #AREATXT, " LLISTAT DELS ALBARANS"
    PRINT #AREATXT, " PÖgina:"; PAG
    PRINT #AREATXT, " Data..:"; FormatD$(Now#, "dd-mm-yyyy")
    PRINT #AREATXT, " " + STRING$(77, "ƒ");
    PRINT #AREATXT, RTRIM$(ARCHIMP.COMPRIMIDO) + ""
    PRINT #AREATXT, "  Referäncia Nom del client                                          Subtotal   DTO % IVA % Total Net     Data     Data Fact. Fact."
    PRINT #AREATXT, "  ---------- ------------------------------------------------------- ---------- ----- ----- ------------- -------- ---------- -----"
    RETURN

PRINTLINIA:
    NOMPCLIENT$ = RTRIM$(ALBARAN.PERSONA.NOM) + " " + RTRIM$(ALBARAN.PERSONA.COGNOMS)
    DADE$ = "": FACT$ = ""
    IF ALBARAN.DOCUMENT = "F" THEN
       FACT$ = "X"
       FOR RF = 1 TO RMF
	   CALL FinestraEstat("Processant ALBA->FACT.", 0)
	   GET AREA9, RF, LINFAC
	   IF LTRIM$(RTRIM$(LINFAC.LINIA(1).REFALBARAN)) = LTRIM$(RTRIM$(ALBARAN.REFALBARAN)) THEN
	      GET AREA8, RF, FACTURA
	      DADE$ = FACTURA.DADA
	   END IF
       NEXT
    END IF
    PRINT #AREATXT, USING MASCARA$; ALBARAN.REFALBARAN; NOMPCLIENT$; ALBARAN.TOTALBRUT; ALBARAN.DTO; ALBARAN.TIPOIVA; ALBARAN.TOTALNET; ALBARAN.DADA; DADE$; FACT$
    L = L + 1
    RETURN
END SUB

SUB MascAlba (MAX)
    COLOR COL(0, 0), COL(0, 1)
    FINESTRA 1, 1, 25, 80, 0, CAIXA1
    COLOR COL(2, 0), COL(2, 1)
    LOCATE , , 1, 13, 14
    LOCATE 2, 2: PRINT " AlbarÖ n£mero:"; SPACE$(10); "   Document: "; : COLOR 15: PRINT DOCU$; : COLOR COL(2, 0): PRINT "     MÖxim de linies:"; : COLOR COL(0, 0): PRINT MAX: COLOR COL(2, 0)
    LOCATE 4, 2: PRINT " Ref. albarÖ:"
    LOCATE 3, 27: PRINT "Codi client:            Data:"
    LOCATE 4, 27: PRINT "        Nom:"
    LOCATE 5, 27: PRINT "    Cognoms:"
    COLOR COL(2, 0), COL(2, 1): LOCATE 20, 40: PRINT " DTO% Linia:": COLOR COL(0, 0), COL(0, 1)
    LOCATE 6, 1: PRINT "ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕÕπ"
    LOCATE 7, 1: PRINT "∫"; : COLOR COL(2, 0), COL(2, 1)
    PRINT "Codi d'article    "; : COLOR COL(0, 0), COL(0, 1)
    PRINT "∫"; : COLOR COL(2, 0), COL(2, 1): PRINT "Concepte                     "; : COLOR COL(0, 0), COL(0, 1)
    PRINT "∫"; : COLOR COL(2, 0), COL(2, 1): PRINT "Preu     "; : COLOR COL(0, 0), COL(0, 1)
    PRINT "∫"; : COLOR COL(2, 0), COL(2, 1): PRINT "Quantit."; : COLOR COL(0, 0), COL(0, 1)
    PRINT "∫"; : COLOR COL(2, 0), COL(2, 1): PRINT "Import    "; : COLOR COL(0, 0), COL(0, 1): PRINT "∫"
    LOCATE 8, 1: PRINT "ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕŒÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕŒÕÕÕÕÕÕÕÕÕŒÕÕÕÕÕÕÕÕŒÕÕÕÕÕÕÕÕÕÕπ"
    LOCATE 19, 1: PRINT "ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕÕπ"
    FOR L = 9 TO 18: LOCATE L, 2: PRINT SPACE$(18); "∫"; SPACE$(29); "∫"; SPACE$(9); "∫"; SPACE$(8); "∫": NEXT
    COLOR COL(2, 0), COL(2, 1): LOCATE 20, 61: PRINT "Total:": COLOR COL(0, 0), COL(0, 1)
    LOCATE 21, 2: PRINT STRING$(78, "Õ");
END SUB

SUB MascAlbs
    COLOR COL(0, 0), COL(0, 1): FINESTRA 4, 1, 20, 80, 0, CAIXA1
    LOCATE 5, 2: PRINT "Referäncia∫Client" + SPACE$(21) + "∫Total brut   ∫DTO% ∫IVA% ∫Total Net"
    LOCATE 6, 1: PRINT "Ã"; STRING$(78, "Õ"); "π"
    LOCATE 4, 12: PRINT "À": LOCATE 4, 40: PRINT "À": LOCATE 4, 54: PRINT "À": LOCATE 4, 60: PRINT "À": LOCATE 4, 66: PRINT "À"
    LOCATE 6, 12: PRINT "Œ": LOCATE 6, 40: PRINT "Œ": LOCATE 6, 54: PRINT "Œ": LOCATE 6, 60: PRINT "Œ": LOCATE 6, 66: PRINT "Œ"
    LOCATE 20, 12: PRINT " ": LOCATE 20, 40: PRINT " ": LOCATE 20, 54: PRINT " ": LOCATE 20, 60: PRINT " ": LOCATE 20, 66: PRINT " "
    FINESTRA 21, 1, 25, 80, 0, CAIXA1
    COLOR COL(2, 0), COL(2, 1)
    LOCATE 22, 2: PRINT "<ENTER>=MODIFICAR ALBARAN <F2>=INSERTAR ALBARAN  <" + CHR$(25) + ">=BAIXAR   <" + CHR$(24) + ">=PUJAR"
    LOCATE 23, 2: PRINT "<ESC>=SORTIR              <F3>=SUMAR ALBARANS    <F4>=CONSULTAR"
    LOCATE 24, 2: PRINT "<F5>=MARCAR ALBARAN       <F6>=LLISTAR           <F7>=IMPRIMIR";
END SUB

REM $STATIC
SUB Missatges
    COLOR COL(2, 0), COL(2, 1)
    LOCATE 23, 2: PRINT "ENTER=EDITAR LINIA    ESC=ANULAR    SUPR=SUPRIMIR LINIA";
    LOCATE 24, 2: PRINT "<" + CHR$(25) + ">= BAIXAR           <" + CHR$(24) + ">= PUJAR    CTRL+F=ACABAR L'ALBARÖ";
    COLOR COL(0, 0), COL(0, 1)
END SUB

REM $DYNAMIC
SUB OrdenarIndex (INDEX() AS INDEXTYPE, MAXST) STATIC
    OFFSET = MAXST - 1 \ 2
    DO WHILE OFFSET > 0
       LIMIT = MAXST - 1 - OFFSET
       DO
	 SWITCH = FALSE
	 FOR i = 1 TO LIMIT
	     IF INDEX(i).Codi > INDEX(i + OFFSET).Codi THEN
		SWAP INDEX(i), INDEX(i + OFFSET)
		SWITCH = i
	     END IF
	 NEXT

	 LIMIT = SWITCH
       LOOP WHILE SWITCH
       OFFSET = OFFSET \ 2
    LOOP
END SUB

SUB PintaValors
    TOTEUROS = ALBARAN.TOTALNET / 166.386

    COLOR COL(0, 0), COL(0, 1)
    COLOR COL(2, 0), COL(2, 1): LOCATE 8, 12: PRINT "Subtotal:";
    COLOR COL(0, 0), COL(0, 1): PRINT USING "#,###,###.##"; ALBARAN.TOTALBRUT
    COLOR COL(2, 0), COL(2, 1): LOCATE 8, 33: PRINT "      DTO %:";
    COLOR COL(0, 0), COL(0, 1): PRINT ALBARAN.DTO
    COLOR COL(2, 0), COL(2, 1): LOCATE 9, 33: PRINT "      IVA %:";
    COLOR COL(0, 0), COL(0, 1): PRINT ALBARAN.TIPOIVA
    COLOR COL(2, 0), COL(2, 1): LOCATE 11, 33: PRINT "Total (PTS):";
    COLOR COL(0, 0), COL(0, 1): PRINT USING "#,###,###.##"; ALBARAN.TOTALNET
    COLOR COL(2, 0), COL(2, 1): LOCATE 12, 33: PRINT "Total (EUR):";
    COLOR COL(0, 0), COL(0, 1): PRINT USING "#,###,###.##"; TOTEUROS
END SUB

FUNCTION PossaEspai$ (CAD$, C)
	 FOR L = 1 TO LEN(CAD$)
	     IF MID$(CAD$, L, 1) = CHR$(C) THEN MID$(CAD$, L, 1) = CHR$(32)
	 NEXT
	 PossaEspai$ = CAD$
END FUNCTION

SUB ReadCalculFactura
  GetBackground 1, 1, 24, 79, RESBUF$
  COLOR COL(0, 0), COL(0, 1): FINESTRA 7, 8, 13, 60, 1, CAIXA1

  PintaValors

  SetMaxCamps 1
  SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), 0, 0
  SetColorCamp 1, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), 0, 0
  InsertValueCamp 0, LTRIM$(STR$(ALBARAN.DTO))
  InsertValueCamp 1, LTRIM$(STR$(ALBARAN.TIPOIVA))
  SetInitCamp 0, 8, 45, NUM, 0, "999", ""
  SetInitCamp 1, 9, 45, NUM, 0, "999", ""

  acabar = FALSE: anular = FALSE: i = 0
  WHILE acabar = FALSE AND i <= 1
	IF ReadCamp(i) = SALIR THEN
	   acabar = TRUE: anular = TRUE
	END IF
	i = i + 1
  WEND

  IF anular = FALSE THEN
     ALBARAN.DTO = VAL(ValueCamp$(0))
     ALBARAN.TIPOIVA = VAL(ValueCamp$(1))
     ALBARAN.BASEIMPONIBLE = ALBARAN.TOTALBRUT - (ALBARAN.TOTALBRUT * ALBARAN.DTO) / 100
     BASEIMP = ALBARAN.BASEIMPONIBLE
     IVA = BASEIMP * (ALBARAN.TIPOIVA) / 100
     ALBARAN.TOTALIVA = IVA
     ALBARAN.TOTALNET = BASEIMP + IVA
     PintaValors
  END IF

END SUB

SUB ReadCapsalera (MAXAL, MAXCL)
       DIM LLISTA$(MAXCL - 1)

       FOR C = 0 TO 10: DeleteCamp C: NEXT C

       RepintaCapsalera (MAXAL)
       FOR C = 0 TO 4
	   VALUE = ReadCamp(C)
	   SELECT CASE VALUE
		  CASE 1
		       TOPRECORD = MAXCL - 1
		       BottomRecord = 1
		       camp$ = RTRIM$(ForaEspai$(ValueCamp$(1)))
		       DO UNTIL (TOPRECORD < BottomRecord)
			  MIDPOINT = (TOPRECORD + BottomRecord) \ 2
			  GET AREA4NDX, MIDPOINT, NDXCLIENT
			  TEST$ = RTRIM$(ForaEspai$(NDXCLIENT.Codi))

			  IF TEST$ = camp$ THEN
			     EXIT DO
			  ELSEIF camp$ > TEST$ THEN
			     BottomRecord = MIDPOINT + 1
			  ELSE
			     TOPRECORD = MIDPOINT - 1
			  END IF
		       LOOP

		       IF TEST$ = camp$ THEN
			  GET AREA4NDX, MIDPOINT, NDXCLIENT
			  GET AREA4, NDXCLIENT.REGISTRE, CLIENT
			  FindClient% = TRUE
		       ELSE
			  FindClient% = FALSE
		       END IF

		       IF FindClient% = TRUE THEN
			  GOSUB TRANSPASSA
			  DisplayAllCamps
			  ALBARAN.REFALBARAN = ValueCamp$(0)
			  ALBARAN.CODCLIENT = ValueCamp$(1)
		       END IF
		  CASE F1
		       SHELL "BKHLP ALB_CAPSALERA"
		       C = C - 1
		  CASE F2
		       IF C = 1 OR C = 3 THEN GOSUB LLISTA.CLIENTS
		       C = C - 1
		  CASE F3 TO F10
		       C = C - 1
		  CASE 999
		       EXIT SUB
		  CASE ELSE
	   END SELECT
       NEXT
       ALBARAN.DADA = ValueCamp$(2)
       ERASE LLISTA$
       EXIT SUB

LLISTA.CLIENTS:
       IF MAXCL = 1 THEN
	  TECLA = Avis("AVIS:", "La base de dades dels clients estÖ buida", "Pitja una tecla...", 0)
	  RETURN
       END IF

       GetBackground 1, 1, 24, 79, LISBUF$
       SetScoreBoard SOFF: LOCATE , , 0: R = 1
       FOR q = 1 TO MAXCL - 1
	   GET AREA4, q, CLIENT
	   IF CLIENT.MARCAT = "-" THEN
	      Codi$ = CLIENT.CODICLIENT
	      N$ = LTRIM$(RTRIM$(CLIENT.NOM))
	      C$ = LTRIM$(RTRIM$(CLIENT.COGNOMS))
	      S$ = N$ + " " + C$: NOM$ = MID$(S$, 1, 40)
	      LLISTA$(R) = Codi$ + " " + NOM$
	      R = R + 1
	   END IF
       NEXT
      
       ASEL = Achoice(3, 9, 20, 61, R - 1, LLISTA$(), COL(), "Codi       Nom                                     ", 12, LTRIM$(RTRIM$(ValueCamp$(3))))
       IF ASEL = 0 OR ASEL = -14 OR ASEL = -13 THEN
	  PutBackground 1, 1, LISBUF$
	  RETURN
       END IF
       IF ASEL <= 0 THEN ASEL = 1
       GET AREA4, ASEL, CLIENT
       GOSUB TRANSPASSA
       InsertValueCamp 1, MID$(LLISTA$(ASEL), 1, 10)
       PutBackground 1, 1, LISBUF$
       CALL DisplayAllCamps
       RETURN

TRANSPASSA:
       InsertValueCamp 3, CLIENT.NOM
       InsertValueCamp 4, CLIENT.COGNOMS
       ALBARAN.PERSONA.NOM = CLIENT.NOM
       ALBARAN.PERSONA.COGNOMS = CLIENT.COGNOMS
       ALBARAN.PERSONA.DNI = CLIENT.DNI
       ALBARAN.PERSONA.DIRECCIO = CLIENT.DIRECCIO
       ALBARAN.PERSONA.POBLACIO = CLIENT.POBLACIO
       ALBARAN.PERSONA.TELEFON1 = CLIENT.TELEFON1
       ALBARAN.PERSONA.TELEFON2 = CLIENT.TELEFON2
       ALBARAN.PERSONA.CPOSTAL = CLIENT.CPOSTAL
       ALBARAN.PERSONA.DTO = CLIENT.DTO
       ALBARAN.PERSONA.FORMAPAGO = CLIENT.FORMAPAGO
       RETURN
END SUB

SUB ReadObserva
    SHARED OBS$()
    LOCATE 22, 2: PRINT SPACE$(70);
    LOCATE 23, 2: PRINT SPACE$(70);
    LOCATE 24, 2: PRINT SPACE$(70);

    SetMaxCamps 1
    SetColorCamp 0, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
    SetColorCamp 1, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1)
    
    SetInitCamp 0, 22, 20, ASCI, 0, STRING$(50, "X"), ""
    SetInitCamp 1, 23, 20, ASCI, 0, STRING$(50, "X"), ""

    COLOR COL(2, 0), COL(2, 1)
    LOCATE 24, 2: PRINT "ENTER=SORTIR";
    LOCATE 22, 2: PRINT "Observacions: ";
    InsertValueCamp 0, ALBARAN.OBSERVA(1): InsertValueCamp 1, ALBARAN.OBSERVA(2)

    IF ReadCamp(0) = SALIR THEN
       OBS$(0) = ValueCamp$(0)
       EXIT SUB
    END IF
    IF ReadCamp(1) = SALIR THEN
       OBS$(1) = ValueCamp$(1)
       EXIT SUB
    END IF
    OBS$(0) = ValueCamp$(0)
    OBS$(1) = ValueCamp$(1)
    ALBARAN.OBSERVA(1) = OBS$(0)
    ALBARAN.OBSERVA(2) = OBS$(1)
    LOCATE 22, 2: PRINT SPACE$(70);
    Missatges
END SUB

SUB RepintaCapsalera (MAX)
       FOR C = 0 TO 10: DeleteCamp C: NEXT C

       SetMaxCamps 4
       FOR C = 0 TO 4: SetColorCamp C, COL(1, 0), COL(1, 1), COL(0, 0), COL(0, 1), COL(2, 0), COL(2, 1): NEXT

       SetInitCamp 0, 4, 15, ASCI, 0, "XXXXXXXXX", ""
       SetInitCamp 1, 3, 39, ASCI, 0, "XXXXXXXXXX", ""
       SetInitCamp 2, 3, 56, NUM, 0, "99/99/9999", ""
       SetInitCamp 3, 4, 39, ASCI, 0, STRING$(20, "X"), ""
       SetInitCamp 4, 5, 39, ASCI, 0, STRING$(30, "X"), ""

       COLOR 15: LOCATE 2, 18: PRINT R

       InsertValueCamp 0, ALBARAN.REFALBARAN
       InsertValueCamp 1, ALBARAN.CODCLIENT
       InsertValueCamp 2, ALBARAN.DADA
       InsertValueCamp 3, ALBARAN.PERSONA.NOM
       InsertValueCamp 4, ALBARAN.PERSONA.COGNOMS
       DisplayAllCamps

END SUB

SUB SumarAlbarans (MAXAL, AREA2)
	   GetBackground 1, 1, 24, 79, FACTU$
	   COLOR COL(0, 0), COL(0, 1): FINESTRA 9, 10, 16, 60, 1, CAIXA1: COLOR COL(2, 0), COL(2, 1)
	   FOR S = 1 TO MAXAL
	       GET AREA2, S, ALBARAN
	       SUMABRUTS = SUMABRUTS + ALBARAN.TOTALBRUT
	       SUMABI = SUMABI + ALBARAN.BASEIMPONIBLE
	       SUMANET = SUMANET + ALBARAN.TOTALNET
	   NEXT
	   LOCATE 10, 11: PRINT "Suma dels totals:"
	   LOCATE 12, 11: PRINT "Subtotals.......:"; : COLOR COL(0, 0), COL(0, 1)
	   PRINT USING "##,###,###.##"; SUMABRUTS:  COLOR COL(2, 0), COL(2, 1)
	   LOCATE 13, 11: PRINT "Bases Imponibles:"; : COLOR COL(0, 0), COL(0, 1)
	   PRINT USING "##,###,###.##"; SUMABI:  COLOR COL(2, 0), COL(2, 1)
	   LOCATE 14, 11: PRINT "IVA's...........:"; : COLOR COL(0, 0), COL(0, 1)
	   PRINT USING "##,###,###.##"; SUMABI:  COLOR COL(2, 0), COL(2, 1)
	   LOCATE 15, 11: PRINT "Totals Net......:"; : COLOR COL(0, 0), COL(0, 1)
	   PRINT USING "##,###,###.##"; SUMANET: COLOR COL(2, 0), COL(2, 1)
	   C$ = INPUT$(1)
	   PutBackground 1, 1, FACTU$

END SUB
