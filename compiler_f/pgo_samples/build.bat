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

Rem Build script for pgo_samples sample

if "%1"=="clean" goto clean
if "%1"=="" goto build

goto error

:build
echo ifort /nologo /Qprof-gen src/pgo_sample.f90
ifort /nologo /Qprof-gen src/pgo_sample.f90
echo pgo_sample.exe
pgo_sample.exe
echo ifort /nologo /Qprof-use /Qopt-report:1 /Qopt-report-phase:pgo src/pgo_sample.f90
ifort /nologo /Qprof-use /Qopt-report:1 /Qopt-report-phase:pgo src/pgo_sample.f90
goto eof

:error
echo invalid entry
echo Syntax
echo      build - compile with /Qprof-gen, generate profile, compile with /Qprof-use
echo      build clean - clean build directory
goto eof

:clean
echo removing files...
del *.exe
del *.obj
del *.optrpt
del *.dpi*
del *.dyn
del *.mod

:eof