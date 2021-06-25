DECLARE SUB InitDesktop ()
DECLARE FUNCTION EncriptaWord$ (WORD$, CLAU$)

DECLARE FUNCTION BinariDecimal! (CAD$)

'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'DATIM.BI'
'$INCLUDE: 'FORMAT.BI'
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\WIN.BI'

TYPE CAPSAL               ' CAP€ALERA DEL FITXER ENCRIPTAT
     CPY AS STRING * 8
     CLAU AS STRING * 10
     SERIAL AS DOUBLE
END TYPE

DIM SHARED CAB AS CAPSAL
DIM SHARED TROS$(1), TROSC$(1)
DIM SHARED TROSE$(1)
DIM SHARED MENUP(10) AS STRING
DIM SHARED COMMENT$(0 TO 10)

	   CLS
	   SetFormatCC (34)
	   CALL InitDesktop
	   GOSUB DEFMENU
	   CALL MenuBar(MENUP(), COMMENT$(), 1, CASO%, 10, 20, 20, 7, 0, 15, 0, 2)
	   END



'*********************************************************************
' CODI VELL
'*********************************************************************
CLS
     PRINT WORD$
	  PRINT final$
	  TROS$(0) = ""
	  TROS$(1) = ""
	  TROSC$(0) = ""
	  TROSC$(1) = ""

     pc = 1
     FOR p = 1 TO LEN(final$)
	  IF pc > LEN(CLAU$) THEN pc = 1
	  BITSWORD$ = DecimalBinari$(ASC(MID$(final$, p, 1)), 7)
	  BITSCLAU$ = DecimalBinari$(ASC(MID$(CLAU$, pc, 1)), 7)

	  ' PARAULA

	  FOR L = 1 TO 4
	      TROS$(0) = TROS$(0) + MID$(BITSWORD$, L, 1)
	  NEXT

	  FOR L = 5 TO 8
	      TROS$(1) = TROS$(1) + MID$(BITSWORD$, L, 1)
	  NEXT

	  ' CLAU

	  FOR L = 1 TO 4
	      TROSC$(0) = TROSC$(0) + MID$(BITSCLAU$, L, 1)
	  NEXT

	  FOR L = 5 TO 8
	      TROSC$(1) = TROSC$(1) + MID$(BITSCLAU$, L, 1)
	  NEXT

	  ' CODIFICAR
	  TROSE$(0) = "    ": TROSE$(1) = "    "

	  FOR L = 1 TO 4
	      MID$(TROSE$(0), L, 1) = LTRIM$(STR$(VAL(MID$(TROS$(0), L, 1)) XOR VAL(MID$(TROSC$(1), L, 1)) / L * L))
	      MID$(TROSE$(1), L, 1) = LTRIM$(STR$(VAL(MID$(TROS$(1), L, 1)) XOR VAL(MID$(TROSC$(0), L, 1)) / L * L))
	      SWAP TROSE$(0), TROSE$(1)
	  NEXT
	  
	  CADFINAL$ = TROSE$(1) + TROSE$(0)
	  crypt = BinariDecimal(CADFINAL$)
	  CLAUD = BinariDecimal(BITSCLAU$)
	  CF = (CLAUD XOR crypt)
	  PRINT CHR$(CF);

	  TROS$(0) = ""
	  TROS$(1) = ""
	  TROSC$(0) = ""
	  TROSC$(1) = ""
	  pc = pc + 1
     NEXT



DEFMENU:
     MENUP(1) = "Encriptar fixter    ": COMMENT$(0) = "ERTHERTH"
     MENUP(2) = STRING$(20, "Ä"): COMMENT$(1) = "GHRTH"
     MENUP(3) = "Desencriptar fixter ": COMMENT$(2) = "ERT"
     MENUP(4) = STRING$(20, "Ä"): COMMENT$(3) = "ERTHE"
     MENUP(5) = "Utilitats           ": COMMENT$(4) = "ERTH"
     MENUP(6) = STRING$(20, "Ä"): COMMENT$(5) = "ERTH"
     MENUP(7) = "Sortir              ": COMMENT$(6) = "RTHERT"
     RETURN

REM $DYNAMIC
FUNCTION BinariDecimal (CAD$)
	 f = 0
	 FOR SY = 7 TO 0 STEP -1
	     f = f + VAL(MID$(CAD$, SY + 1, 1)) * 2 ^ (7 - SY)
	 NEXT
	 BinariDecimal = f
END FUNCTION

FUNCTION EncriptaWord$ (WORD$, CLAU$)

     final$ = "": pc = 1
     FOR p = 1 TO LEN(WORD$)
	  IF pc > LEN(CLAU$) THEN pc = 1
	  BITSWORD$ = DecimalBinari$(ASC(MID$(WORD$, p, 1)), 7)
	  BITSCLAU$ = DecimalBinari$(ASC(MID$(CLAU$, pc, 1)), 7)

	  ' PARAULA

	  FOR L = 1 TO 4
	      TROS$(0) = TROS$(0) + MID$(BITSWORD$, L, 1)
	  NEXT

	  FOR L = 5 TO 8
	      TROS$(1) = TROS$(1) + MID$(BITSWORD$, L, 1)
	  NEXT

	  ' CLAU

	  FOR L = 1 TO 4
	      TROSC$(0) = TROSC$(0) + MID$(BITSCLAU$, L, 1)
	  NEXT

	  FOR L = 5 TO 8
	      TROSC$(1) = TROSC$(1) + MID$(BITSCLAU$, L, 1)
	  NEXT

	  ' CODIFICAR
	  TROSE$(0) = "    ": TROSE$(1) = "    "

	  FOR L = 1 TO 4
	      MID$(TROSE$(0), L, 1) = LTRIM$(STR$(VAL(MID$(TROS$(0), L, 1)) XOR VAL(MID$(TROSC$(1), L, 1)) / L * L))
	      MID$(TROSE$(1), L, 1) = LTRIM$(STR$(VAL(MID$(TROS$(1), L, 1)) XOR VAL(MID$(TROSC$(0), L, 1)) / L * L))
	      SWAP TROSE$(0), TROSE$(1)
	  NEXT

	  CADFINAL$ = TROSE$(1) + TROSE$(0)
	  crypt = BinariDecimal(CADFINAL$)
	  CLAUD = BinariDecimal(BITSCLAU$)

	  CF = (CLAUD XOR crypt)

	  final$ = final$ + CHR$(CF)

	  TROS$(0) = ""
	  TROS$(1) = ""
	  TROSC$(0) = ""
	  TROSC$(1) = ""

	  pc = pc + 1
     NEXT
     EncriptaWord$ = final$
END FUNCTION

SUB InitDesktop
    COLOR 15, 7
    FONS 177
    LOCATE 1, 1: PRINT STRING$(80, " ");
    LOCATE 25, 1: PRINT STRING$(80, " ");
    COLOR 0, 7: LOCATE 1, 60: PRINT "Smart-Crypto 1.0"

END SUB

