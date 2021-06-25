{
  WINDES.PAS
  Finestra per desviar programes dins ella

  Tomeu Cap¢ Cap¢ 1996 (C)
}

{$A+}
{$B+}
{$S+}
{$M 16384,0,0}

USES DOS,CRT;

{$F+}
PROCEDURE NOVAINT29(FLAGS,CS,IP,AX,BX,CX,DX,SI,DI,DS,ES,BP:WORD);INTERRUPT;
BEGIN
     ASM
        CLI
     END;
     WRITE(CHAR(LO(AX)));
     ASM
        STI
     END
END;
{$F-}


VAR VELLAINT29:PROCEDURE;
    X,Y:BYTE;
BEGIN
     GETINTVEC($29,@VELLAINT29);
     SETINTVEC($29,@NOVAINT29);
     TEXTCOLOR(14);TEXTBACKGROUND(9);
      WINDOW(1,5,80,22);
     WRITE('É');
     FOR X:=2 TO 80-1 DO WRITE('Í');
     WRITE('»');
     FOR X:=6 TO 22-1 DO BEGIN
         WRITE('º');
         FOR Y:=2 TO 80-1 DO WRITE(' ');
         WRITE('º');
     END;
     WRITE('È');
     FOR X:=2 TO 80-1 DO WRITE('Í');
     INC(WindMax);
     WRITE('¼');
     DEC(WindMax);
     TEXTCOLOR(WHITE);TEXTBACKGROUND(BLACK);
     WINDOW(2,6,79,21);
     CLRSCR;
     SWAPVECTORS;
     EXEC(PARAMSTR(1),'');
     WINDOW(1,1,80,25);
     SWAPVECTORS;
     SETINTVEC($29,@VELLAINT29);
END.
