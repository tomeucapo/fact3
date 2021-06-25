
'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'

DIM MENUS AS CMENU

       AREAMENU = FREEFILE
       OPEN "C:\FACT3\RECURSOS\MENU_OP.DAT" FOR RANDOM AS AREAMENU LEN = LEN(MENUS)
       OP = 0: GET AREAMENU, 1, MENUS

       MENUS.CONTROL(4) = 9

	MENUS.OPCIO(0, 2) = "~Manteniment del magatzemÿ"
	MENUS.OPCIO(1, 2) = "~Control de m¡nims       ÿ "
	MENUS.OPCIO(2, 2) = "Manteniment de ~fam¡lies ÿ "
	MENUS.OPCIO(3, 2) = "~Hist•ric d'articles     ÿ"
	MENUS.OPCIO(4, 2) = "Manteniment de ~prove‹dors "
	
'
'         MENUS.OPCIO(4, 4) = STRING$(21, "Ä")
'         MENUS.OPCIO(5, 3) = "Codis Postals        ÿ"
'         MENUS.OPCIO(6, 3) = "Documents            ÿ"

'        MENUS.OPCIO(0, 4) = "~Albarans            >"
'        MENUS.OPCIO(1, 4) = "~Factures            >"
'        MENUS.OPCIO(2, 4) = "~Comandes            ÿ"
'        MENUS.OPCIO(3, 4) = "~Presupostos         ÿ"
'        MENUS.OPCIO(7, 4) = STRING$(22, "Ä")
'        MENUS.OPCIO(4, 4) = STRING$(22, "Ä")
'        MENUS.OPCIO(5, 4) = "Relaci¢ de factures ÿ"
'        MENUS.OPCIO(6, 4) = "Relaci¢ d'albarans  ÿ"
'        MENUS.OPCIO(8, 4) = "C~obros              >"
'        MENUS.MISSATGE(8, 4) = "Controlar els cobros de les factures"
	'        MENUS.PROC(8, 4) = "MANT_COBROS"

	'  MENUS.OPCIO(5, 0) = " COMPRES"
	PUT AREAMENU, 1, MENUS
	CLOSE AREAMENU
       END

       FOR MENU = 0 TO 6
	   DO
	     'IF MENU = 0 THEN

	     '   PRINT RTRIM$(MENUS.OPCIO(OP, MENU)); " "; RTRIM$(MENUS.Missatge(OP, MENU))
	     '   C$ = INPUT$(1)
	      '  OP = OP + 1
	     'ELSE
		PRINT
		PRINT RTRIM$(MENUS.OPCIO(OP, MENU)); OP; MENU
		PRINT RTRIM$(MENUS.Missatge(OP, MENU))
		PRINT RTRIM$(MENUS.PROC(OP, MENU))
		C$ = INPUT$(1)
		IF C$ = CHR$(13) THEN
		   LINE INPUT "Opcio Visual: ", M$
		   'LINE INPUT "Missatge....: ", MIS$
		   GET AREAMENU, 1, MENUS
		   'LINE INPUT "Programa....:", PROC$

		    MENUS.OPCIO(OP, MENU) = M$
		   ' MENUS.Missatge(OP, MENU) = MIS$
		   ' MENUS.PROC(OP, MENU) = PROC$
		    PUT AREAMENU, 1, MENUS
		END IF
		OP = OP + 1
	     'END IF
	   LOOP UNTIL OP = MENUS.CONTROL(MENU)
	   OP = 0
       NEXT
       CLOSE AREAMENU

