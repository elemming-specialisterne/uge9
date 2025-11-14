@echo off
REM Wrapper til GnuCOBOL på Windows
REM Sætter include- og lib-stier automatisk

set COBINC=C:\GnuCOBOL\include
set COBLIB=C:\GnuCOBOL\lib

REM Tilføj bin-mappen til PATH midlertidigt
set PATH=C:\GnuCOBOL\bin;%PATH%

REM Set UTF-8 environment for Danish characters
set LANG=da_DK.UTF-8
set LC_ALL=da_DK.UTF-8
set LC_CTYPE=da_DK.UTF-8

REM Kald cobc med de rigtige flags
cobc -I"%COBINC%" -L"%COBLIB%" -x %*.cob -o %*.exe -lcob

%*