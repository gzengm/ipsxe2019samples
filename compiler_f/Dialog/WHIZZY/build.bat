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
::  Build for WHIZZY
::******************************************************************************

if /i "%1"=="" goto Build
if /i "%1"=="build" goto Build
if /i "%1"=="clean" goto Clean
goto Syntax

:Clean
if exist "*.obj" del *.obj
if exist "*.exe" del *.exe
if exist "*.res" del *.res
if exist "*.fd" del *.fd
exit /B 0

:Build
@echo on
rc /fo.\whizzy.res src\whizzy.rc
deftofd src\whizzy.h whizzy.fd

ifort /nologo  /winapp src\whizzy.f90 whizzy.res
exit /B 0

:Syntax
echo %~nx0 [build ^| clean]
echo build is default
exit /B 1

