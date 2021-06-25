    IF C% = 2 THEN GOSUB MENU.USUAR

    MASCCFG (C%)
    FOR R = 1 TO MAXEM - 1
        GET AREA1, R, CFG
        REG$ = LTRIM$(RTRIM$(STR$(R)))
        IF LEN(REG$) = 1 THEN REG$ = "00" + REG$
        IF LEN(REG$) = 2 THEN REG$ = "0" + REG$
        EMPRESA$(R) = CFG.NOM + REG$
    NEXT
    L = LEN(CFG.NOM)

    DO
      LOCATE , , 0
      EMP = ACHOICE(4, 1, 20, 40, MAXEM - 1, EMPRESA$(), col(), "Nom empresa                   N£mero  ", 1)

      SELECT CASE EMP
             CASE 0
                  CLOSE AREA, AREA1, AREA2, AREA3
                  ERASE EMPRESA$
                  PutBackground 1, 1, factsu$
                  EXIT DEF
             CASE -2
                  IF C% = 1 THEN R = MAXEM: GOSUB INSERTAR
             CASE -4
                  GOSUB CONSULTAR
             CASE -14, -13, -3, -5, -6, -7, -8, -9, -10
                  BEEP
             CASE ELSE
                  SELECT CASE C%
                         CASE 1
                              R = EMP: GOSUB EDITAR
                         CASE 2
                              R = EMP: GOSUB MENU.USUAR
                         CASE 4
                              R = EMP: GOSUB IMPRESORA
                         CASE 6
                              R = EMP: GOSUB CAPSALERA
                         CASE 7
                              R = EMP: GOSUB PIEPAG
                         CASE 9
                              R = EMP: GOSUB CONF.COLORS
                         CASE ELSE
                  END SELECT

      END SELECT
    LOOP

