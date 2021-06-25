TYPE NUMS
     NOMBRE AS STRING * 20
     SEPARA AS STRING * 1
END TYPE


DECLARE FUNCTION NAT$ (NUM!, LLENG!)
DIM SHARED NUM AS NUMS
DIM SHARED NOMBRE$(1000, 2)

NOMBRE$(0, 1) = "ZERO": NOMBRE$(0, 2) = "CERO"
NOMBRE$(1, 1) = "UN": NOMBRE$(1, 2) = "UNO"
NOMBRE$(2, 1) = "DOS": NOMBRE$(2, 2) = "DOS"
NOMBRE$(3, 1) = "TRES": NOMBRE$(3, 2) = "TRES"
NOMBRE$(4, 1) = "QUATRE": NOMBRE$(4, 2) = "CUATRO"
NOMBRE$(5, 1) = "CINC": NOMBRE$(5, 2) = "CINCO"
NOMBRE$(6, 1) = "SIS": NOMBRE$(6, 2) = "SEIS"
NOMBRE$(7, 1) = "SET": NOMBRE$(7, 2) = "SIETE"
NOMBRE$(8, 1) = "VUIT": NOMBRE$(8, 2) = "OCHO"
NOMBRE$(9, 1) = "NOU": NOMBRE$(9, 2) = "NUEVE"
NOMBRE$(10, 1) = "DEU": NOMBRE$(10, 2) = "DIEZ"
NOMBRE$(11, 1) = "ONZE": NOMBRE$(11, 2) = "ONCE"
NOMBRE$(12, 1) = "DOTZE": NOMBRE$(12, 2) = "DOCE"
NOMBRE$(13, 1) = "TRETZE": NOMBRE$(13, 2) = "TRECE"
NOMBRE$(14, 1) = "CATORZE": NOMBRE$(14, 2) = "CATORZE"
NOMBRE$(15, 1) = "QUINZE": NOMBRE$(15, 2) = "QUINCE"
NOMBRE$(16, 1) = "SETZE": NOMBRE$(16, 2) = "DIECISEIS"
NOMBRE$(17, 1) = "DISSET": NOMBRE$(17, 2) = "DIECISIETE"
NOMBRE$(18, 1) = "DIVUIT": NOMBRE$(18, 2) = "DIECIOCHO"
NOMBRE$(19, 1) = "DINOU": NOMBRE$(19, 2) = "DIECINUEVE"
NOMBRE$(20, 1) = "VINT": NOMBRE$(20, 2) = "VEINTE"
NOMBRE$(30, 1) = "TRENTA": NOMBRE$(30, 2) = "TREINTA"
NOMBRE$(40, 1) = "QUARANTA": NOMBRE$(40, 2) = "CUARENTA"
NOMBRE$(50, 1) = "CINQUANTA": NOMBRE$(50, 2) = "CINCUENTA"
NOMBRE$(60, 1) = "SEIXANTA": NOMBRE$(60, 2) = "SESENTA"
NOMBRE$(70, 1) = "SETANTA": NOMBRE$(70, 2) = "SETENTA"
NOMBRE$(80, 1) = "VUITANTA": NOMBRE$(80, 2) = "OCHENTA"
NOMBRE$(90, 1) = "NORANTANTA": NOMBRE$(90, 2) = "NOVENTA"
NOMBRE$(100, 1) = "CENT": NOMBRE$(100, 2) = "CIEN"
NOMBRE$(200, 1) = "DOSCENTS": NOMBRE$(200, 2) = "DOSCIENTOS"
NOMBRE$(300, 1) = "TRECENTS": NOMBRE$(300, 2) = "TRESCIENTOS"
NOMBRE$(400, 1) = "QUATRECENTS": NOMBRE$(400, 2) = "CUATROCIENTOS"
NOMBRE$(500, 1) = "CINC-CENTS": NOMBRE$(500, 2) = "QUINIENTOS"
NOMBRE$(600, 1) = "SISCENTS": NOMBRE$(600, 2) = "SEISCIENTOS"
NOMBRE$(700, 1) = "SETCENTS": NOMBRE$(700, 2) = "SETECIENTOS"
NOMBRE$(800, 1) = "VUITCENTS": NOMBRE$(800, 2) = "OCHOCIENTOS"
NOMBRE$(900, 1) = "NOUCENTS": NOMBRE$(900, 2) = "NOVECIENTOS"
NOMBRE$(1000, 1) = "MIL": NOMBRE$(1000, 2) = "MIL"

FOR I = 0 TO 1000
    PRINT NAT$(I, 1); I
    C$ = INPUT$(1)
NEXT

FUNCTION NAT$ (NUM, LLENG)
         SHARED NOMBRE$()
         N$ = LTRIM$(RTRIM$(STR$(NUM)))
         L = LEN(N$)
         TEXTE$ = ""
        
         SELECT CASE L
                CASE 1
                     TEXTE$ = NOMBRE$(NUM, LLENG)
                     NAT$ = TEXTE$
                     EXIT FUNCTION
                CASE 2
                     IF MID$(N$, 2, 1) = "0" OR MID$(N$, 1, 1) = "1" THEN
                        TEXTE$ = NOMBRE$(NUM, LLENG)
                        NAT$ = TEXTE$
                        EXIT FUNCTION
                     ELSE
                        TEXTE$ = NOMBRE$(VAL(MID$(N$, 1, 1) + "0"), LLENG) + "-" + NOMBRE$(VAL(MID$(N$, 2, 1)), LLENG)
                        NAT$ = TEXTE$
                        EXIT FUNCTION
                     END IF
                CASE 3
                     IF MID$(N$, 2, 2) = "00" THEN
                        TEXTE$ = NOMBRE$(NUM, LLENG)
                        NAT$ = TEXTE$
                        EXIT FUNCTION
                     ELSE
                        TEXTE$ = NOMBRE$(VAL(MID$(N$, 1, 1) + "00"), LLENG)
                        IF MID$(N$, 2, 2) <> "00" THEN
                          IF VAL(MID$(N$, 2, 2)) > VAL("19") THEN
                             TEXTE$ = TEXTE$ + "-" + NOMBRE$(VAL(MID$(N$, 2, 1) + "0"), LLENG)
                             IF MID$(N$, 3, 1) <> "0" THEN
                                TEXTE$ = TEXTE$ + "-" + NOMBRE$(VAL(MID$(N$, 3, 1)), LLENG)
                                NAT$ = TEXTE$
                                EXIT FUNCTION
                             END IF
                             NAT$ = TEXTE$
                             EXIT FUNCTION
                          ELSE
                             TEXTE$ = TEXTE$ + "-" + NOMBRE$(VAL(MID$(N$, 2, 2)), LLENG)
                             NAT$ = TEXTE$
                             EXIT FUNCTION
                          END IF
                        END IF
                     END IF
                CASE 4
                     TEXTE$ = NOMBRE$(VAL(MID$(N$, 1, 1)), LLENG)

                CASE 5
                CASE 6
                CASE 7
                CASE 8
                CASE 9
                CASE 10
                CASE ELSE
         END SELECT
END FUNCTION

