@echo off
::==============================================================
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
:: =============================================================
::
::
::******************************************************************************
:: Content:
::
::  Intel(R) Parallel Studio XE
::  Build for AutoDice sample
::******************************************************************************

set output_dir=release
set src_dir=src
if not exist %output_dir%  mkdir release

if /i "%1"=="" goto Build
if /i "%1"=="build" goto Build
if /i "%1"=="clean" goto Clean
if /i "%1"=="run" goto Run
goto Syntax

:Clean
if exist %output_dir% del /f /q %output_dir%\*.obj %output_dir%\*.exe %output_dir%\*.res %output_dir%\*.mod
exit /B 0

:Build
@echo on
ifort /c /I%output_dir%\ /module:%output_dir%\ /object:%output_dir%\ %src_dir%\EXCEL.f90
ifort /c /I%output_dir%\ /module:%output_dir%\ /object:%output_dir%\ %src_dir%\ADOobjs.f90
ifort /exe:%output_dir%\AutoDice.exe /I%output_dir%\ /module:%output_dir%\ /object:%output_dir%\ %src_dir%\AutoDice.f90 %output_dir%\ADOobjs.obj %output_dir%\excel.obj
@echo off
exit /B 0

:Run
@echo on
cd msvs
..\%output_dir%\AutoDice.exe
cd ..
@echo off
exit /B 0

:Syntax
echo build.bat [build ^| clean ^|run]
echo build is default
exit /B 1

