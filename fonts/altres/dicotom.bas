CONST max! = 100
DIM taula(max)

FOR i = 1 TO max
    a = a + ABS(INT(RND * 10))
    taula(i) = a
NEXT

CLS
INPUT "Quä vols cercar?", que!

i = 1
j = max
trobat = 0
DO WHILE (i <= j) AND trobat = 0
   m = (i + j) \ 2
   IF taula(m) = que THEN
      trobat = 1
   ELSEIF que < taula(m) THEN
      j = m - 1
   ELSE
      i = m + 1
   END IF
LOOP

IF trobat THEN
   PRINT "Trobat a", m
ELSE
   PRINT "No trobat"
END IF

FOR i = 1 TO max
    PRINT i, taula(i)
    WHILE INKEY$ = ""
    WEND
NEXT

