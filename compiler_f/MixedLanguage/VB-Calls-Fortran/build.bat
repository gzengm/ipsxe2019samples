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
::******************************************************************************
rem Build script for VB-Calls-Fortran sample
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
del *.pdb
if exist "release" del /F /Q release\*.*
if exist "release" rmdir release
goto done

:release
if exist "release" goto nextRelease
mkdir release
:nextRelease
set BinOut=.\release

@ECHO ON
ifort /O2 /nologo /dll .\src\fcall.f90 /Fo:%BinOut%\Fcall.obj /link -out:%BinOut%\Fcall.dll -implib:%BinOut%\Fcall.lib
@ECHO OFF
goto done

:debug
if exist "debug" goto nextDebug
mkdir debug
:nextDebug
set BinOut=.\debug

@ECHO ON
ifort /Od /nologo /debug:full /dll .\src\fcall.f90 /Fo:%BinOut%\Fcall.obj /link -out:%BinOut%\Fcall.dll -implib:%BinOut%\Fcall.lib
@ECHO OFF
goto done

:run
@ECHO ON
rem See included ReadMe.html for instructions on running this

:done