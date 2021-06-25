'
' UNLOCK.BAS
' Prepara el programa GESTIO.EXE per soportar l'ordinador
' amb el que s'ha de fer feina
'
' Tomeu Cap¢ Cap¢ 1996 (C)
'

'$INCLUDE: 'C:\FACT2\FONTS\STRUCTS.BI'
'$INCLUDE: 'CAMPS.BI'

DIM CFG AS CONFIG
DIM EMPR AS EMPRESA
      PRINT
      COLOR 14
      PRINT "SMART Unlock 1.0": COLOR 7
      PRINT
      PRINT "Inicialitzant "; : x = CSRLIN
      FOR I = 15 TO 30
          COLOR 15
          TEMPS (1)
          LOCATE x, I: PRINT ".";
      NEXT
      
      D$ = DIR$("UNLOCKED.EXE")
      IF D$ = "" THEN
         PRINT : BEEP: COLOR 12:
         PRINT "ERROR: Programa manipulat per un usuari extärn"
         PRINT "       per seguretat aquest programa s'atura. ;-((("
         CLOSE 1: SYSTEM
      END IF
      
      PRINT ":-))"
      OPEN "CONFIG.FAC" FOR RANDOM SHARED AS 1 LEN = LEN(CFG)
      GET 1, 1, CFG
      CFG.INST = ""
      COLOR 15, 9
      FINESTRA 10, 20, 17, 51, 1, CAIXA1
      COLOR 14: LOCATE 10, 21: PRINT " UNLOCK 1.0 ": COLOR 13
      LOCATE 11, 21: PRINT "Creant n£mero de särie:": COLOR 15, 9

      CADENC$ = ENCRIPT$(MID$(VERSVGA$, 1, 29), 45)    ' ENCRIPTAR-LO
      NM$ = ""
      FOR I = 21 TO 30
          TEMPS (1)
          LOCATE 12, I: PRINT ".";
          N% = RND * 255
          NM$ = NM$ + RTRIM$(LTRIM$(STR$(N%)))
      NEXT
      COLOR 14: LOCATE 13, 21: PRINT NM$

      CFG.INST = CADENC$
      CFG.INSTALAT = "INSTAL.LAT"
      PUT 1, 1, CFG
      CLOSE 1, 2: SOUND 100, 1: SOUND 200, 2: SOUND 100, 1
      COLOR 15: LOCATE 15, 21: PRINT "Programa desbloquetjat !!!"
      'COLOR 29: LOCATE 16, 21: PRINT "Autodestrucci¢ de l'UNLOCK !!!"
      'KILL "UNLOCKED.EXE"
      SYSTEM

