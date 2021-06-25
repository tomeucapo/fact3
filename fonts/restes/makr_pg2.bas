


FOR L = 1 TO 65
    C1 = 1
    FOR CAR = 1 TO 80
        IF C1 > 9 THEN C1 = 0
        LPRINT LTRIM$(STR$(C1));
        C1 = C1 + 1
    NEXT
NEXT

