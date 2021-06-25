;
; UTILS.ASM
;
; Utilitats amb ensamblador per basic
;
; Tomeu Cap¢ Cap¢ 1996 (C). Rutines Summer'96 (C)
;
               NAME DRAC_UTILS
               .MODEL MEDIUM

               PUBLIC BSHL,BSHR,CLR
               ASSUME DS:DGROUP, ES:DGROUP

               .DATA

BUF            DW 001Eh,0040h

               .CODE

BSHL           PROC FAR
               MOV  BX,SP
               MOV  AX,[BX+06]
               MOV  CX,[BX+04]
               SHL  AX,CL
               RETF 4
BSHL           ENDP

BSHR           PROC FAR
               MOV  BX,SP
               MOV  AX,[BX+06]
               MOV  CX,[BX+04]
               SHR  AX,CL
               RETF 4
BSHR           ENDP

CLR            PROC FAR
               PUSH      BP                       ;Guarda el punter per el BASIC
               MOV       BP,SP                    ;set up Base Pointer
               PUSH     