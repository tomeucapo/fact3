
'$DYNAMIC
DIM SHARED FILE$(0)


MAX = DirToArray("*.*")

'MAX = DirToArray("*.BAS")


CLS
SCREEN 12
FOR r = 0 TO MAX
    PRINT FILE$(r)
NEXT

