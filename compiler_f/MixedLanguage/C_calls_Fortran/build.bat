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

rem Build script for C_Calls_Fortran sample
@ECHO OFF
rem if using the Platform SDK on Intel(R) 64 architecture need to add a library
set COOKIE_LIB=
if "%MSVCVer%"=="Win64" set COOKIE_LIB=bufferoverflowu.lib

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
rem Non-DLL
ifort /c /O2 /nologo /Fo:%BinOut%\fsub.obj .\src\fsub.f90
cl  /nologo /c /D_CRT_SECURE_NO_DEPRECATE .\src\cmain.cpp /Fo%BinOut%\cmain.obj
cl  /nologo /D_CRT_SECURE_NO_DEPRECATE %BinOut%\fsub.obj %BinOut%\cmain.obj libifcoremd.lib %COOKIE_LIB% /link -out:%BinOut%\uselib.exe

rem DLL
ifort /O2 /nologo /dll .\src\fsub.f90 /Fo:%BinOut%\fsub.obj /link -out:%BinOut%\fsub.dll -implib:%BinOut%\fsub.lib
cl  /nologo /c /D_CRT_SECURE_NO_DEPRECATE .\src\cmain.cpp /Fo%BinOut%\cmain.obj
cl /nologo /DUSE_DLL /D_CRT_SECURE_NO_DEPRECATE %BinOut%\cmain.obj %BinOut%\fsub.lib libifcoremd.lib %COOKIE_LIB% /link -out:%BinOut%\usedll.exe

@ECHO OFF
goto done

:debug
if exist "debug" goto nextDebug
mkdir debug
:nextDebug
set BinOut=.\debug

@ECHO ON
rem Non-DLL
ifort /c /Od /nologo /Fo:%BinOut%\fsub.obj .\src\fsub.f90
cl  /nologo /Od /c /D_CRT_SECURE_NO_DEPRECATE .\src\cmain.cpp /Fo%BinOut%\cmain.obj
cl  /nologo /Od /D_CRT_SECURE_NO_DEPRECATE %BinOut%\fsub.obj %BinOut%\cmain.obj libifcoremdd.lib %COOKIE_LIB% /link -out:%BinOut%\uselib.exe

rem DLL
ifort /Od /nologo /dll .\src\fsub.f90 /Fo:%BinOut%\fsub.obj /link -out:%BinOut%\fsub.dll -implib:%BinOut%\fsub.lib
cl  /nologo /c /D_CRT_SECURE_NO_DEPRECATE .\src\cmain.cpp /Fo%BinOut%\cmain.obj
cl /nologo /DUSE_DLL /D_CRT_SECURE_NO_DEPRECATE %BinOut%\cmain.obj %BinOut%\fsub.lib libifcoremdd.lib %COOKIE_LIB% /link -out:%BinOut%\usedll.exe


@ECHO OFF
goto done

:run
@ECHO OFF
if exist "debug" (set BinOut=.\debug) else goto checkRelease
@ECHO ON
rem Running debug executables...
%BinOut%\uselib.exe
%BinOut%\usedll.exe

:checkRelease
@ECHO OFF
if exist "release" (set BinOut=.\release) else goto done
@ECHO ON
rem Running release executables...
%BinOut%\uselib.exe
%BinOut%\usedll.exe

:done