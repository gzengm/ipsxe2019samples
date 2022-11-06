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
::      Build script for OpenGL samples for Intel Visual Fortran
::******************************************************************************

if /i "%1" == "/?" goto Usage
if /i "%1" == "" goto Build
if /i "%1" == "build" goto Build
if /i "%1" == "clean" goto Build
goto Usage

:Build
set Build_mode=%1
if /i "%1" == "" set Build_mode=Build

@echo on
:: AnimateGL
cd AnimateGL
call build.bat %Build_mode%
cd ..

:: Rings
@echo on
cd Rings
call build.bat %Build_mode%
cd ..

:: Puzzle
@echo on
cd Puzzle
call build.bat %Build_mode%
cd ..

@echo off
exit /B 0

:Usage
echo .....
echo Build OpenGL samples: %~nx0 [build ^| clean]
exit /B 1

endlocal