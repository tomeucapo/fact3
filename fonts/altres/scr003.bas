'DIM SCR AS STRING * 4000
DIM ASM AS STRING * 50

CLS

RESTORE LLISTAT

DEF SEG = VARSEG(ASM)
VASM = VARPTR(ASM)
FOR P = 0 TO 39
    READ A
    POKE VASM + P, A
NEXT
DEF SEG
VASM = VASM + P - 1

SG = VARSEG(SCR)
OF = VARPTR(SCR)

'DEF SEG = VARSEG(ASM)
'POKE VASM, 0
'POKE VASM + 1, &HB8
'POKE VASM + 2, 0
'POKE VASM + 3, 0
'POKE VASM + 4, SG AND 255
'POKE VASM + 5, SG \ 256
'POKE VASM + 6, OF AND 255
'POKE VASM + 7, OF \ 256
'POKE VASM + 8, 2000 AND 255
'POKE VASM + 9, 2000 \ 256
'DEF SEG

DEF SEG = VARSEG(ASM)
CALL Absolute(VARPTR(ASM))
DEF SEG

END

LLISTAT:
DATA &H0FC,&H08B,&H036,&H01C,&H1,&H08E,&H0DE,&H08B
DATA &H036,&H01E,&H1,&H08B,&H03E,&H020,&H1,&H08E
DATA &H0C7,&H08B,&H03E,&H022,&H1,&H08B,&H0E,&H024
DATA &H1,&H0F3,&HA5,&H0C2,&H05,&H00
DATA &H00,&HB8
DATA &H00,&H00
DATA &H00,&HB8
DATA &H20,&H00
DATA &H10,&H00

