
int far ClearTypeHead(int c)
{
  asm {
    MOV DI,0x0040
    MOV ES,DI
    MOV DI,0x001E
    MOV CX,0x10
    XOR AX,AX
    REP STOSW
  }
  return 0;
}


