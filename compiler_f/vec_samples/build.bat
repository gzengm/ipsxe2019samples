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
rem Build script for Vec sample
set EXEC=Matvec.EXE
set SRCDIR=src\
set DESTDIR=release\
set EXTRA_INCLUDE=
set EXTRA_LIB=

rem PRODUCT_NAME is only defined in Intel environment
rem if in Visual Studio environment
if /i "%PRODUCT_NAME%"=="" (
	set CC=cl
	set CC_FLAGS=/Ox /arch:AVX /GL /fp:fast /EHsc /MT
	set SRCFILES=%SRCDIR%cell_pool.cpp %SRCDIR%fluid_animate.cpp %SRCDIR%IO.cpp %SRCDIR%main.cpp %SRCDIR%timer.cpp
	echo "Set up proper Intel Environment"
	goto eof
)
rem else if in Intel environment
if /i NOT "%PRODUCT_NAME%"=="" (
	set FC=ifort
	set FC_FLAGS= /O2 /Qopt-report:3 /Qopt-report-phase:vec /Qopt-report-routine:matvec 
	set SRCFILES=%SRCDIR%matvec.f90 %SRCDIR%driver.f90 
)

set LINK_FLAGS=/INCREMENTAL:NO /SUBSYSTEM:CONSOLE /MANIFEST:NO

if /i "%1"=="clean" goto clean
if /i "%1"=="run" goto run
if /i "%1"=="debug" goto debug
if /i "%1"=="rundebug" goto rundebug

:options
if "%1"=="" goto compile
if /i "%1"=="perf_num" set FC_FLAGS=%FC_FLAGS% -D PERF_NUM
shift
goto options

:compile
mkdir %DESTDIR% 2>nul
echo on
%FC% %FC_FLAGS% %EXTRA_INCLUDE% /Fo%DESTDIR% %SRCFILES% /link %LINK_FLAGS% %EXTRA_LIB% /out:%DESTDIR%%EXEC%
@echo off
goto eof

:run
shift
set args=
:collectargs
if /i NOT "%1"=="" (
	set args=%args% %1
	shift
	goto collectargs
)
%DESTDIR%%EXEC% %args%
goto eof

:rundebug
shift
set args=
:collectargs
if /i NOT "%1"=="" (
	set args=%args% %1
	shift
	goto collectargs
)
set DESTDIR=Debug\
%DESTDIR%%EXEC% %args%
goto eof

:clean
echo removing files...
rmdir /Q /S %DESTDIR% 2>nul
rmdir /Q /S debug\ 2>nul
goto eof

:debug
set DESTDIR=Debug\
set FC_FLAGS= /debug:full /Od /debug-parameters:used /warn:interfaces /Qopt-report:3 /Qopt-report-phase:vec /Qopt-report-routine:matvec /debug:full /Od /debug-parameters:used /warn:interfaces /Qopt-report:3 /Qopt-report-phase:vec /Qopt-report-routine:matvec
mkdir %DESTDIR% 2>nul
cd Debug\
set DESTDIR=" "
set SRCDIR=..\src\
set SRCFILES=%SRCDIR%matvec.f90 %SRCDIR%driver.f90 
echo on
%FC% %FC_FLAGS% %EXTRA_INCLUDE%  %SRCFILES% /link %LINK_FLAGS% %EXTRA_LIB% /out:%EXEC%

@echo off
cd ..
goto eof
:eof
