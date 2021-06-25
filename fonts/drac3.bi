'
' DRAC3.BI
'
' Definici¢ de funcions de la segona capa que han de funcionar amb
' CAMPS3.LIB la llibreria prinicipal (KERNEL)
'
' Tomeu Cap¢ i Cap¢ 1997 (C)
'

DECLARE SUB Barra (nFila%, nCol%, nMax#, nActual!)
DECLARE SUB LoadMask (NOM$)
DECLARE SUB Mensaje (MENSAJ$)
DECLARE SUB Menu2 (TEXTOSMENUP() AS STRING, CASO%, FILA%, COLUM%, ANCHO%, ITEMS%, CF, CT, CTS, CFS)
DECLARE SUB MenuBar (TEXTOSMENUP() AS STRING, COMMENT$(), I!, CASO%, FILA%, COLUM%, ANCHO%, ITEMS%, CF!, CT!, CTS!, CFS!)
DECLARE SUB PopMenu (XM, YM, X2M, OPCIO%, MENU$(), COMMENT$(), CF, CT, CFS, CTS, col(), estat())
DECLARE SUB ScrollUp (X, Y, x2, y2, L)
DECLARE SUB ScrollDown (X, Y, x2, y2, L)
DECLARE SUB ImprimeixFitxerTXT (NOM$, DEVICE$, COLS)
DECLARE SUB DIMSort (ARRAY$(), MAX)
DECLARE SUB MissatgeError (E, M$)
DECLARE SUB EstatTeclesControl (X, Y)
DECLARE SUB FinestraEstat (TITOL$, RETARD)
DECLARE SUB SetDirRecursos (DIREC$)
DECLARE SUB SetScoreBoard (VALUE)
DECLARE SUB PrintByWord (x1, y1, x2, y2, c1, c2, Missatge$)
DECLARE SUB Avis.Sonor (SO)
DECLARE SUB Clr ()
DECLARE SUB GetCopyBox (row1%, col3%, row2%, col2%, buffer$)
DECLARE SUB PutCopyBox (row1%, col3%, buffer$)
DECLARE SUB AttrBox (row1%, col1%, row2%, col2%, attr)
DECLARE SUB PutBackground (row1%, col3%, buffer$)
DECLARE SUB GetBackground (row1%, col1%, row2%, col2%, buffer$)
DECLARE FUNCTION Avis! (TITOL$, MENSAJ$, MESRES$, S!)
DECLARE FUNCTION DirToArray! (MASK$)
DECLARE FUNCTION BSHL% (BYVAL VALUE, BYVAL BITS)
DECLARE FUNCTION BSHR% (BYVAL VALUE, BYVAL BITS)
DECLARE FUNCTION Rotacion (BAJO%, ALTO%, CORRIENTE%, CASO%)
DECLARE FUNCTION ShowError ()
DECLARE FUNCTION DTN! (DAT$)
DECLARE FUNCTION ForaEspai$ (CAD$)
DECLARE FUNCTION Achoice (X%, Y%, x2%, y2%, ELEM, CMATRIU$(), col(), TITOL$, POSEEK, CAD$)
DECLARE FUNCTION LlegeixParaula$ (M$, I!)
DECLARE FUNCTION NumeroEnBase$ (num AS INTEGER, b AS INTEGER)
                
TYPE BARRASTATUS
     ACTIVAT AS CURRENCY
     STATUS AS CURRENCY
     CADENA AS STRING * 30
END TYPE

TYPE ESTATMENUS
     OLDMENU AS CURRENCY
     ACT AS CURRENCY
     XOLD AS CURRENCY
     YOLD AS CURRENCY
END TYPE

TYPE ESTATANIMA
     FRAME AS CURRENCY
     POSIC AS CURRENCY
END TYPE

TYPE RECUR
     direcctori AS STRING * 30
     BARRAESTAT AS BARRASTATUS
     MENUESTAT AS ESTATMENUS
     ANIMESTAT AS ESTATANIMA
END TYPE

COMMON SHARED OLDTITOL$
DIM SHARED REGS AS RegTypeX     ' Assignar la variable global per els registres ASM
DIM SHARED TMP$(10)             ' Array temporal per volcar menus
DIM SHARED RECURSOS AS RECUR    ' Assignar la variable global per els recursos
'DIM SHARED FILE$(0 TO 1)        ' Array a on es guarden els direcctoris (temporals)

CONST SON = 45, SOFF = 999      ' Definici¢ de constats
CONST MINROW = 2
CONST MAXROW = 25
CONST MINCOL = 1
CONST MAXCOL = 80
CONST MAXITEM = 20

