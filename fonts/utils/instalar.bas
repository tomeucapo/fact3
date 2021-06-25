COLOR 14: PRINT "DRAC Instal.lador 1.0. Tomeu Cap¢ Cap¢ 1996 (C)"
COLOR 2: PRINT "Programa per instal.lar el FACTURACIO 2.0 al DISC DUR"
COLOR 7
PRINT
PRINT "Avis: Per instal.lar aquest programa ha de tenir un disc dur"
PRINT "      amb al menys 2 Mb de capacitat lliures."
PRINT
COLOR 15
PRINT "Aviso: Para instalar este programa tiene que tener un disco duro"
PRINT "       con al menos 2 Megas de capacidad libres."
PRINT
COLOR 7
PRINT "Pitja <ENTER> per comen‡ar /"; : COLOR 15: PRINT " Pulsa <ENTER> para comenzar ": COLOR 7
PRINT "Pitja <ESC> per sortir     /"; : COLOR 15: PRINT " Pulsa <ESC> para salir ": COLOR 7
DO
   C$ = INKEY$
   IF C$ = CHR$(27) THEN
      PRINT : PRINT "*** Instal.lador anulat / "; : COLOR 15: PRINT "*** Instalador anulado": COLOR 7
      PRINT
      SYSTEM
   END IF
LOOP UNTIL C$ = CHR$(13)
PRINT
INPUT "Lletra del DISC DUR, sense els dos punts:", UNIDAD$
PRINT
IF UNIDAD$ = "" OR UCASE$(UNIDAD$) <> "C" AND UCASE$(UNIDAD$) <> "D" THEN
   SYSTEM
END IF
PRINT "Creant direcctori "; UNIDAD$; ":\FACTURAS /"; : COLOR 15: PRINT " Creando directorio "; UNIDAD$; ":\FACTURAS": COLOR 7
MKDIR UNIDAD$ + ":\FACTURAS"
CHDIR UNIDAD$ + ":\FACTURAS"
PRINT "Copiant fitxer descompresor ..."
SHELL "COPY AIN.EXE " + UNIDAD$ + ":\FACTURAS > nul"
PRINT "Copiant arxiu comprimit ..."
SHELL "COPY FACT2.AIN " + UNIDAD$ + ":\FACTURAS > nul"
SHELL UNIDAD$ + ":"
SHELL "AIN X -GMAX FACT2.AIN"
PRINT "Eliminat arxiu comprimit ..."
SHELL "DEL FACT2.AIN"
SHELL "DEL AIN.EXE"
PRINT "Procedint a la desprotecci¢ ..."
PRINT : X = CSRLIN: Y = POS(0)
SHELL "UNLOCKED.EXE"
OPEN UNIDAD$ + ":\GESTIO.BAT" FOR OUTPUT AS #1
PRINT #1, "CD \FACTURAS"
PRINT #1, "GESTIO.EXE"
PRINT #1, "CD \"
CLOSE #1
LOCATE X, Y: PRINT "Pitja una tecla per continuar / "; : COLOR 15: PRINT "Pulsa una tecla para continuar": COLOR 7
CLS
PRINT "Instal.laci¢ completada !!! / "; : COLOR 15: PRINT "Instalaci¢n completada !!!": COLOR 7
PRINT
PRINT "La clau d'access es: SA MATA  / "; : COLOR 15: PRINT " La clave de acceso es: SA MATA": COLOR 7
PRINT
PRINT "Per posar en marxa el programa escriu GESTIO": COLOR 15
PRINT "Para poner en marcha el programa escribe GESTIO"
CHDIR "\"
SYSTEM




