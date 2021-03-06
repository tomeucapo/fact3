{
 ICOP.PAS

 Programa per copiar fitxers, per l'us intern del Facturaci? 2.0
 DSOFT 1993/96 (C)

 Tomeu Cap? Cap? 1996 (C)
}

USES CRT;

VAR
  FROMF, TOF: FILE;
  NUMREAD, NUMWRITTEN: WORD;
  BUF:ARRAY[1..2048]OF CHAR;

BEGIN
  ASSIGN(FROMF,PARAMSTR(1));
  RESET(FROMF,1);
  ASSIGN(TOF,PARAMSTR(2));
  REWRITE(TOF, 1);

  GOTOXY(2,23);TEXTCOLOR(15);TEXTBACKGROUND(9);
  WRITELN('Copiant ', FILESIZE(FROMF), ' bytes...');
  GOTOXY(2,19);

  REPEAT
        BLOCKREAD(FROMF,BUF,SIZEOF(BUF),NUMREAD);
        BLOCKWRITE(TOF,BUF,NUMREAD,NUMWRITTEN);
  UNTIL (NUMREAD=0) OR (NUMWRITTEN<>NUMREAD);

  CLOSE(FROMF);
  CLOSE(TOF);
END.



