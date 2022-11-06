@echo off
setlocal
::=============================================================================
::
:: SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
:: http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
::
:: Copyright 2016 Intel Corporation
::
:: THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
:: NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
:: PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
::
::=============================================================================
::
::
::******************************************************************************
:: Content:
::      Build script for Cube Puzzle sample for Intel Visual Fortran
::******************************************************************************

if /i "%1" == "/?" goto Usage
if /i "%1" == "" goto Build
if /i "%1" == "build" goto Build
if /i "%1" == "clean" goto Clean
goto Usage

:Clean
rmdir /S /Q obj > nul 2>&1
del *.exe *.pdb *.dll *.exp *.lib > nul 2>&1
exit /B 0

:: Build program
:Build
set Build_mode=%1
if /i "%1" == "" set Build_mode=Build

if NOT exist "obj" mkdir obj

:: Display compiler version
ifort /what 2>&1 | findstr /C:Fortran

@echo on

rc /fo.\obj\menu.res src\menu.rc
deftofd src\resource.h obj\resource.fd
ifort /nologo /c /object:obj\ /module:obj\ src\sharedat.f90
ifort /nologo /c /object:obj\ /module:obj\ src\cube.f90
ifort /nologo /winapp /exe:puzzle.exe /object:obj\ /module:obj\ src\main.f90 obj\cube.obj obj\sharedat.obj obj\menu.res

@echo off
exit /B 0

:Usage
echo .....
echo Build Puzzle - Cube sample: %~nx0 [build ^| clean]
exit /B 1

endlocal