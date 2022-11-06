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
::      Build script for DynamicLoad sample for Intel Visual Fortran
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

:: Build DLL and Main program
:Build
set Build_mode=%1
if /i "%1" == "" set Build_mode=Build

if NOT exist "obj" mkdir obj

:: Display compiler version
ifort /what 2>&1 | findstr /C:Fortran

@echo on

ifort /nologo /object:obj\ /module:obj\ /dll /libs:dll /exe:USERDLL.dll src\USERFUNC.f90
ifort /nologo /object:obj\ /module:obj\ /libs:dll src\DynamicLoad.f90

@echo off
echo .....Build complete
exit /B 0

:Usage
echo .....
echo Build DynamicLoad sample: %~nx0 [build ^| clean]
exit /B 1

endlocal
