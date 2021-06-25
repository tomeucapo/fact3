'************************************************************************
'* PANEL2.BAS                                                           *
'************************************************************************
'* Aixï Çs una prova del que poren fer les noves llibreries de          *
'* Smart Libraries 3.0x                                                 *
'*                                                                      *
'* Que inclouen control de finästres i botons amb el ratol°             *
'*                                                                      *
'* Smart Software 1993/97 (C)   --*--   23/02/1997                      *
'* Tomeu Cap¢ i Cap¢ 1997 (C)                                           *
'*                                                                      *
'************************************************************************

DECLARE SUB DefineixFinestres ()
DECLARE SUB DefineixBotons ()

'$INCLUDE: 'QBX.BI'                          ' Headers
'$INCLUDE: 'C:\FACT3\FONTS\SDK_001.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DRAC3.BI'
'$INCLUDE: 'C:\FACT3\FONTS\STRUCTS.BI'
'$INCLUDE: 'C:\FACT3\FONTS\DMOUSE.BI'
'$INCLUDE: 'C:\FACT3\FONTS\WIN.BI'

    CLS
    COLOR 15, 9
    CALL FONS(177)

    CALL DefineixFinestres        ' Crea les finestres a utilitzar
    ShowWindow 0
    CALL DefineixBotons           ' Crea i pinta els botons del panell

    DO
      SELECT CASE Dialog
	     CASE 0
		  GOSUB CAMP1
	     CASE 5, 999
		  GOSUB sortir
	     CASE ELSE
      END SELECT
    LOOP

'----------------------------------------------------------------------
'Programa  :-)
'______________________________________________________________________

CAMP1:

    RETURN

sortir:
    ShowWindow 1           ' Pinta la finestra

    FOR B = 0 TO 5         ' Borra els botons del panell
	check = DeleteBoto(B)
    NEXT
    PackBotons             ' Els elimina definitivament de la memïria

    IF SetMaxBotons(1) THEN  ' Inicialitza els nous botons
       BEEP
    END IF

    bot = SetInitBoto(14, 30, 16, 38, "ˇ Si ", 0)
    Bot1 = SetInitBoto(14, 40, 16, 48, "ˇ No ", 1)

    FOR B = 0 TO 1
	CALL SetBotoColor(15, 2, 14, 3, B)
	ShowBoto B
    NEXT

    LOCATE 12, 23: PRINT "Vols Sortir al sistema operatiu?"
    DO
      SELECT CASE Dialog
	     CASE 0
		check = DeleteBoto(0)     ' Marca els botons com a borrats
		check = DeleteBoto(1)

		DeleteWindow 0            ' Marca les finästres com a borrades
		DeleteWindow 1
		DeleteWindow 2

		PackBotons                ' Matxaca els botons i les finästres de memïria
		PackWindows
		COLOR 15, 9: FONS 177
		MouseOff                  ' Atura el ratol°
		SYSTEM
	   CASE 1
		check = DeleteBoto(0)     ' Borra els botons
		check = DeleteBoto(1)
		PackBotons
		DeleteWindow 1            ' Borra les finestres
		DefineixBotons
		RETURN
	   CASE 999
		MouseOff
		SYSTEM
	   CASE ELSE
    END SELECT
LOOP
RETURN

REM $DYNAMIC
SUB DefineixBotons STATIC

    IF SetMaxBotons(5) THEN
       TECLA = Avis("ERROR 001B:", "Memïria insufisicient!!!", "PT", 0)
       SYSTEM
    END IF

    bot = SetInitBoto(6, 3, 8, 27, "Unitat de disc", 0)
    Bot1 = SetInitBoto(9, 3, 11, 27, "Mouse", 1)
    bot2 = SetInitBoto(12, 3, 14, 27, "CD-ROM", 2)
    bot3 = SetInitBoto(15, 3, 17, 27, "Targeta De So", 3)
    bot4 = SetInitBoto(18, 3, 20, 27, "Targeta GrÖfica", 4)
    bot5 = SetInitBoto(21, 3, 23, 27, "Sortir", 5)

    '**************************************************************
    '* Check-Sum del botons                                       *
    '**************************************************************
    ERRB = 0
    IF bot THEN
       ERRB = ERRB + 1
    ELSE
       IF bot2 THEN
	  ERRB = ERRB + 1
       ELSE
	  IF bot3 THEN
	     ERRB = ERRB + 1
	  ELSE
	     IF bot4 THEN
		ERRB = ERRB + 1
	     ELSE
		IF bot5 THEN
		   ERRB = ERRB + 1
		END IF
	     END IF
	  END IF
       END IF
    END IF

    '**************************************************************
    '* Comprovar el Check-Sum                                     *
    '**************************************************************

    IF ERRB > 0 THEN
       TECLA = Avis("ERROR 002B:", "Hi ha hagut un error inicialitzant els botons", "PT", 0)
       SYSTEM
    END IF


    FOR B = 0 TO 5
	CALL SetBotoColor(15, 9, 14, 3, B)
	ShowBoto B
    NEXT
    
END SUB

SUB DefineixFinestres STATIC

    '**************************************************************
    '* Dispondre de memïria per les finestres                     *
    '**************************************************************

    IF SetInitWindows(2) THEN
       PT = Avis("ERROR 001W:", "Memïria insufisicient!!!", "PT", 0)
       SYSTEM
    END IF

    '**************************************************************
    '* Inicialitzar les finästres                                 *
    '**************************************************************
    
    WIN1 = InitNewWindow(0, 3, 1, 24, 30, 1, "Panell de control")
    WIN2 = InitNewWindow(1, 10, 20, 17, 60, 1, "Dialog")
    WIN3 = InitNewWindow(2, 10, 20, 17, 60, 1, "Finestra 3")

    '**************************************************************
    '* Check-Sum de les finästres                                 *
    '**************************************************************

    ERRW = 0
    IF WIN1 THEN
       ERRW = ERRW + 1
    ELSE
       IF WIN2 THEN
	  ERRW = ERRW + 1
       ELSE
	  IF WIN3 THEN
	     ERRW = ERRW + 1
	  END IF
       END IF
    END IF

    '**************************************************************
    '* Comprovar el Check-Sum                                     *
    '**************************************************************

    IF ERRW > 0 THEN
       PT = Avis("ERROR 002W:", "Hi ha hagut un error inicialitzant les finänstres", "PT", 0)
       SYSTEM
    END IF

    '**************************************************************
    '* Definir el tipus de finästres                              *
    '**************************************************************

    SetColorWindow 0, 15, 9, 14, 11, 7, 9    ' Color
    SetColorWindow 1, 15, 2, 14, 11, 7, 2
    SetColorWindow 2, 15, 2, 14, 11, 7, 4

    SetStyleWindow 0, 1, ""                  ' Estil
    SetStyleWindow 1, 3, ""
    SetStyleWindow 2, 0, ""

END SUB

