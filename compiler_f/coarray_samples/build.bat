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
:: Content:
::
::  Intel(R) Parallel Studio XE
::  Build for coarray sample
::******************************************************************************

if "%1"=="" goto compile
if "%1"=="run" goto run
if "%1"=="clean" goto clean

:compile
echo Building...
ifort /nologo /Qcoarray:shared src/mcpi_coarray_final.f90 -o mcpi_coarray_final.exe
goto eof

:run
mcpi_coarray_final.exe
goto eof

:clean
echo Removing files...
del *.exe
del *.obj

:eof

