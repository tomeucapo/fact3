'$INCLUDE: 'QBX.BI'
'$INCLUDE: 'D:\fact3\fonts\ipx.bi'
'$INCLUDE: 'D:\FACT3\FONTS\DRAC3.BI'

DEFSNG A-Z
SUB CloseSocket (Socket%)
   inreg.bx = 1
   inreg.ax = 0
   inreg.dx = Socket
   CALL InterruptX(&H7A, inreg, outreg)
END SUB

SUB GetMyAddress (MyNetwork$, MyNode$, MyNetworkHex$, MyNodeHex$)
   inreg.bx = &H9
   inreg.es = VARSEG(GetMyAdd)
   inreg.si = VARPTR(GetMyAdd)
   CALL InterruptX(&H7A, inreg, outreg)
   MyNetwork$ = GetMyAdd.NetWork
   MyNode$ = GetMyAdd.Node
   MyNetworkHex$ = TurnToHex$(MyNetwork$)
   MyNodeHex$ = TurnToHex$(MyNode$)
END SUB

FUNCTION HexToBinary$ (Variable$)
   IF Variable$ = "" THEN
      HexToBinary$ = ""
   ELSE
      A = LEN(Variable$) MOD 2
      IF A = 1 THEN
	 HexToBinary$ = ""
      ELSE
	 Temp$ = ""
	 FOR A = 1 TO LEN(Variable$) STEP 2
	    Temp! = VAL("&H" + MID$(Variable$, A, 2))
	    Temp$ = Temp$ + CHR$(Temp!)
	 NEXT
	 HexToBinary$ = Temp$
      END IF
   END IF
END FUNCTION

SUB IPXCancel (CompleteCode%)
   inreg.bx = 6
   inreg.es = VARSEG(ECBS)
   inreg.si = VARPTR(ECBS)
   CALL InterruptX(&H7A, inreg, outreg)
   CompleteCode = SplitWordLo%(outreg.ax)
END SUB

SUB IPXDisconnect (DNet$, DNode$, DSock$)
   Disconnect.NetWork = DNet$
   Disconnect.Node = DNode$
   Disconnect.Socket = DSock$
   inreg.bx = &HB
   inreg.es = VARSEG(Disconnect)
   inreg.si = VARPTR(Disconnect)
   CALL InterruptX(&H7A, inreg, outreg)
END SUB

FUNCTION IPXInstalled%
   inreg.ax = &H7A00
   CALL InterruptX(&H2F, inreg, outreg)
   AL = SplitWordLo(outreg.ax)
   IF AL = &HFF THEN IPXInstalled = 1 ELSE IPXInstalled = 0
END FUNCTION

SUB IPXMarker (Interval%)
   inreg.bx = 8
   CALL InterruptX(&H7A, inreg, outreg)
   Interval = outreg.ax
END SUB

SUB IPXSchedule (DelayTicks%)
   inreg.ax = DelayTicks%
   inreg.bx = 5
   inreg.es = VARSEG(ECBS)
   inreg.si = VARPTR(ECBS)
   CALL InterruptX(&H7A, inreg, outreg)
   CompleteCode = ASC(ECBS.CompCode)
   InUseFlag = ASC(ECBS.InUse)
END SUB

SUB OpenSocket (Socket%, STATUS%, SocketNumberReturned%)
   inreg.bx = 0
   inreg.ax = 0
   inreg.dx = Socket
   CALL InterruptX(&H7A, inreg, outreg)
   STATUS = SplitWordLo(outreg.ax)
   SocketNumberReturned = outreg.dx
   '
   '           Completion status
   '                    00 successful
   '                    FF open already
   '                    FE socket table is full
END SUB

SUB RelenquishControl
   DEFINT A-Z
   inreg.ax = 0
   inreg.bx = &HA
   CALL InterruptX(&H7A, inreg, outreg)
END SUB

SUB SendPacket (CompleteCode%, InUseFlag%)
   inreg.bx = 3
   inreg.es = VARSEG(ECBS)
   inreg.si = VARPTR(ECBS)
   CALL InterruptX(&H7A, inreg, outreg)
   CompleteCode = ASC(ECBS.CompCode)
   InUseFlag = ASC(ECBS.InUse)
   '
   '        Error codes:
   '              00    sent
   '              FC    canceled
   '              FD    malformed packet
   '              FE    no listener (undelivered)
   '              FF    hardware failure
END SUB

SUB SocketListen
   inreg.bx = 4
   inreg.es = VARSEG(ECBR)
   inreg.si = VARPTR(ECBR)
   CALL InterruptX(&H7A, inreg, outreg)
   '
   '        Completion codes:
   '              00    received
   '              FC    canceled
   '              FD    packet overflow
   '              FF    socket was closed
END SUB

FUNCTION SplitWordHi (TheWord%)
   SplitWordHi = (TheWord% AND &HFF00) / 256
END FUNCTION

FUNCTION SplitWordLo (TheWord%)
   SplitWordLo = (TheWord% AND &HFF)
END FUNCTION

FUNCTION TurnToHex$ (Variable$)
   Temp$ = ""
   FOR Byte = 1 TO LEN(Variable$)
      VALUE! = ASC(MID$(Variable$, Byte, 1))
      IF VALUE! < 15 THEN
	 Temp$ = Temp$ + "0" + HEX$(VALUE!)
      ELSE
	 Temp$ = Temp$ + HEX$(VALUE!)
      END IF
   NEXT
   TurnToHex$ = Temp$
END FUNCTION

