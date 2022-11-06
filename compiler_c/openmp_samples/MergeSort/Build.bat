@echo off

rem CommandPromptType is set in both VS and Intel, but not in normal DOS environment
if /i "%VCINSTALLDIR%"=="" (
	echo "environment not set up (needs either Visual Studio or Intel environment)"
	goto eof
)

set EXEC=MergeSort.exe
set SRCDIR=src\
set DESTDIR=release\
set EXTRA_INCLUDE=
set EXTRA_LIB=

set LINK_FLAGS=/INCREMENTAL:NO /SUBSYSTEM:CONSOLE /MANIFEST:NO

if /i "%1"=="clean" goto clean
if /i "%1"=="run" goto run
if /i "%1"=="help" goto help

set CC_FLAGS=
set USE_INTEL=1
rem PRODUCT_NAME is only defined in Intel environment
if /i "%PRODUCT_NAME%"=="" set USE_INTEL=0
:options
if /i NOT "%1"=="" (
	if /i "%1"=="vc" set USE_INTEL=0
	if /i "%1"=="vs" set USE_INTEL=0
	if /i "%1"=="cl" set USE_INTEL=0
	if /i "%1"=="perf_num" set CC_FLAGS=-D PERF_NUM %CC_FLAGS% 
	shift
	goto options
)

rem if using Visual Studio compiler
if /i "%USE_INTEL%"=="0" (
	set CC=cl
	set CC_FLAGS=/O2 /GL %CC_FLAGS% /openmp
)
rem else if using Intel compiler
if /i "%USE_INTEL%"=="1" (
	set CC=icl
	set CC_FLAGS=/O2 /Qipo %CC_FLAGS% /Qopenmp /Qstd=c++11
)

:compile
mkdir %DESTDIR% 2>nul
echo on
%CC% %CC_FLAGS% %EXTRA_INCLUDE% /Fo%DESTDIR% %SRCDIR%*.cpp /link %LINK_FLAGS% %EXTRA_LIB% /out:%DESTDIR%%EXEC%
@echo off
goto eof

:run
%DESTDIR%%EXEC% %2
goto eof

:help
echo usage
echo building with VC:  build ^[vc^|cl^|vs^]
echo building with ICL: build
echo clean:             build clean
echo run:               build run ^[0^|1^|2^]
echo.
goto eof

:clean
echo removing files...
rmdir /Q /S %DESTDIR% 2>nul

:eof

