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
rem Build script for Openmp sample
@ECHO OFF

if "%1" == "clean"    goto clean
if "%1" == "release"  goto release
if "%1" == "debug"    goto debug
if "%1" == "run"      goto run

@ECHO ON
rem No action taken...
rem syntax: build.bat [clean|debug|release|run]
@ECHO OFF
goto done

:clean
if exist "debug" del /F /Q debug\*.*
if exist "debug" rmdir debug
if exist "release" del /F /Q release\*.*
if exist "release" rmdir release
goto done

:release
if exist "release" goto nextRelease
mkdir release
:nextRelease
set BinOut=.\release

@ECHO ON
ifort /c /O2 /Qopenmp /fpp /Fo:%BinOut%\openmp_sample.obj .\src\openmp_sample.f90
ifort %BinOut%\openmp_sample.obj /O2 /Qopenmp /link /out:%BinOut%\openmp_sample.exe

@ECHO OFF
goto done

:debug
if exist "debug" goto nextDebug
mkdir debug
:nextDebug
set BinOut=.\debug

@ECHO ON
ifort /c /Od /Qopenmp /fpp /Fo:%BinOut%\openmp_sample.obj .\src\openmp_sample.f90
ifort %BinOut%\openmp_sample.obj /Od /Qopenmp /link /out:%BinOut%\openmp_sample.exe

@ECHO OFF
goto done

:run
@ECHO OFF
if exist "debug" (set BinOut=.\debug) else goto checkRelease
@ECHO ON
rem Running debug executables...
%BinOut%\openmp_sample.exe

:checkRelease
@ECHO OFF
if exist "release" (set BinOut=.\release) else goto done
@ECHO ON
rem Running release executables...
%BinOut%\openmp_sample.exe

:done