@ECHO OFF
REM *****************************************************************
REM * FACT.BAT
REM * Script d'execuci¢ principal del programa de facturaci¢
REM *
REM * Modul disenyat per que el programa no ocupi molta de memoria
REM * descarga i carrega de moduls autom…ticament
REM *
REM * Tomeu Cap¢ i Cap¢ 1997 (C)

SET TEMPORAL=C:\TEMP\
:BUCLE
ECHO Carregant...
GESTIO.EXE
IF ERRORLEVEL=2 GOTO FI
CALL CRIDADA.BAT
DEL CRIDADA.BAT

GOTO BUCLE

:FI
ECHO Programa descarregat...
