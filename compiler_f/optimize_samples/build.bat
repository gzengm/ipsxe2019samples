@echo off
::==============================================================
::
:: SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
:: http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
::
:: Copyright 2017 Intel Corporation
::
:: THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
:: NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
:: PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
::
:: =============================================================
::
::
::******************************************************************************

@echo off

Rem Build script for optimize sample

if "%1"=="" (set FLAG=/O2 
   goto compile)
if "%1"=="O1" (set FLAG=/O1 
   goto compile)
if "%1"=="O2" (set FLAG=/O2 
   goto compile)
if "%1"=="O3" (set FLAG=/O3 
   goto compile)
if "%1"=="clean" goto clean
goto error

:compile
echo building at %FLAG% ...
ifort /nologo %FLAG% src/int_sin.f90
goto eof

:error
echo invalid entry
echo Syntax:
echo      build - compile at /O2
echo      build O1 - compile at /O1
echo      build O2 - compile at /O2
echo      build O3- compile at /O3
echo      build clean - clean build directory
goto eof

:clean
echo removing files...
del *.exe
del *.obj

:eof

