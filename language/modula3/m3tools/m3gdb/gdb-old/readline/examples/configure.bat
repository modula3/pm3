@echo off
if "%1" == "go32" goto h8300
if "%1" == "h8/300" goto h8300
echo Specify one of [ go32 h8/300 ] on command line
goto exit

:go32
echo Configuring readline/examples for go32
copy Makefile.dos Makefile
goto exit

:h8300
echo Configuring readline/examples for H8/300
copy Makefile.dos Makefile

:exit
