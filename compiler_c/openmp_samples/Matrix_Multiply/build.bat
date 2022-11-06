@echo off
SETLOCAL

set EXEC=openmp_sample.exe
set SRCDIR=src\
set DESTDIR_DEBUG=debug\
set DESTDIR_RELEASE=release\
set DESTDIR_SERIAL=release_serial\
set DESTDIR_RUN=release\
set EXTRA_INCLUDE=
set EXTRA_LIB=
set SRCFILES=%SRCDIR%openmp_sample.c

rem PRODUCT_NAME is only defined in Intel environment
rem if in Visual Studio environment
if /i "%PRODUCT_NAME%"=="" (
	set CC=cl
	set CC_FLAGS_DEBUG=/Od /ZI /openmp /F256000000
	set CC_FLAGS_RELEASE=/O2 /openmp /F256000000
	set CC_FLAGS_RELEASE_SERIAL=/O2 /F256000000
)

rem else if in Intel environment
if /i NOT "%PRODUCT_NAME%"=="" (
	set CC=icl
	set CC_FLAGS_DEBUG=/nologo /Od /ZI /Qstd=c99 /Qopenmp /F256000000
	set CC_FLAGS_RELEASE=/nologo /O2 /Qstd=c99 /Qopenmp /F256000000
	set CC_FLAGS_RELEASE_SERIAL=/nologo /O2 /Qstd=c99 /Qwd3180 /F256000000
)

set LINK_FLAGS=/INCREMENTAL:NO /SUBSYSTEM:CONSOLE /MANIFEST:NO

if /i "%1"=="debug" goto debug
if /i "%1"=="release" goto release
if /i "%1"=="" goto release
if /i "%1"=="serial" goto serial
if /i "%1"=="clean" goto clean
if /i "%1"=="run" goto run
if /i "%1"=="help" goto helpmsg
goto helpmsg

:debug
set DESTDIR_RUN=%DESTDIR_DEBUG%
mkdir %DESTDIR_DEBUG% 2>nul
echo %CC% %CC_FLAGS_DEBUG% %EXTRA_INCLUDE% /Fo%DESTDIR_DEBUG% %SRCFILES% /link %LINK_FLAGS% %EXTRA_LIB% /out:%DESTDIR_DEBUG%%EXEC%
%CC% %CC_FLAGS_DEBUG% %EXTRA_INCLUDE% /Fo%DESTDIR_DEBUG% %SRCFILES% /link %LINK_FLAGS% %EXTRA_LIB% /out:%DESTDIR_DEBUG%%EXEC%
goto eof

:release
set DESTDIR_RUN=%DESTDIR_RELEASE%
mkdir %DESTDIR_RELEASE% 2>nul
echo %CC% %CC_FLAGS_RELEASE% %EXTRA_INCLUDE% /Fo%DESTDIR_RELEASE% %SRCFILES% /link %LINK_FLAGS% %EXTRA_LIB% /out:%DESTDIR_RELEASE%%EXEC%
%CC% %CC_FLAGS_RELEASE% %EXTRA_INCLUDE% /Fo%DESTDIR_RELEASE% %SRCFILES% /link %LINK_FLAGS% %EXTRA_LIB% /out:%DESTDIR_RELEASE%%EXEC%
goto eof

:serial
set DESTDIR_RUN=%DESTDIR_SERIAL%
mkdir %DESTDIR_SERIAL% 2>nul
echo %CC% %CC_FLAGS_RELEASE_SERIAL% %EXTRA_INCLUDE% /Fo%DESTDIR_SERIAL% %SRCFILES% /link %LINK_FLAGS% %EXTRA_LIB% /out:%DESTDIR_SERIAL%%EXEC%
%CC% %CC_FLAGS_RELEASE_SERIAL% %EXTRA_INCLUDE% /Fo%DESTDIR_SERIAL% %SRCFILES% /link %LINK_FLAGS% %EXTRA_LIB% /out:%DESTDIR_SERIAL%%EXEC%
goto eof

:run
shift
if /i "%1"=="" set DESTDIR_RUN=%DESTDIR_RELEASE%
if /i "%1"=="debug" set DESTDIR_RUN=%DESTDIR_DEBUG%
if /i "%1"=="release" set DESTDIR_RUN=%DESTDIR_RELEASE%
if /i "%1"=="serial" set DESTDIR_RUN=%DESTDIR_SERIAL%
if not exist %DESTDIR_RUN%%EXEC% (
  echo cannot find executable from %destdir%%exec%
  goto helpmsg
)
%DESTDIR_RUN%%EXEC%
goto eof

:helpmsg
echo Syntax: build [^|debug^|release^|serial^|run^|clean^|help]
echo      build debug   - Building openmp_sample with debug configuration.
echo      build release - Building openmp_sample with release configuration. (default)
echo      build serial  - Building openmp_sample with release configuration, without OpenMP.
echo      build run [debug^|release^|serial] - Run openmp_sample.
echo      build clean   - Clean build directories.
echo      build help    - Show this help message.
goto eof

:clean
echo removing files...
rmdir /Q /S %DESTDIR_DEBUG% %DESTDIR_RELEASE% %DESTDIR_SERIAL% 2>nul

:eof
ENDLOCAL
