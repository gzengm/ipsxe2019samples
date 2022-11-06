@echo off

set EXEC=NQueen.exe
set SRCDIR=src\
set SRCDIR_COMMON=src\common\
set EXTRA_INCLUDE=/I %SRCDIR_COMMON%
set EXTRA_LIB=
set SRCFILES_COMMON=%SRCDIR%common\stdafx.cpp %SRCDIR%common\nq-serial.cpp %SRCDIR%common\nq-main.cpp %SRCDIR%common\nq-Dlg.cpp

rem PRODUCT_NAME is only defined in Intel environment
rem if in Visual Studio environment
if /i "%PRODUCT_NAME%"=="" (
        echo Need Intel C++ Compiler to build complete sample.
        goto eof
)

rem else if in Intel environment
if /i NOT "%PRODUCT_NAME%"=="" (
	set CC=icl
)

:options
if /i "%1"=="debug" goto debug
if /i "%1"=="release" goto release
if /i "%1"=="clean" goto clean
if /i "%1"=="run" goto run
if /i "%1"=="help" goto helpmsg

:release
set CC_FLAGS=/O2 /Qdiag-disable:1885 /Qipo /EHsc /MD /GS /fp:fast /W3 /Zi -D _UNICODE -D UNICODE -D _AFXDLL 
set LINK_FLAGS=winmm.lib /incremental:no /allowisolation /TLBID:1 /debug /subsystem:windows /opt:ref /opt:icf /ENTRY:"wWinMainCRTStartup" /dynamicbase /nxcompat /ignore:4068
goto compile

:debug
set CC_FLAGS=/Od /Qdiag-disable:1885 /EHsc /RTC1 /MDd /GS /Qftz- /W3 /Zi -D _UNICODE -D UNICODE -D _AFXDLL
set LINK_FLAGS=winmm.lib /incremental /allowisolation /TLBID:1 /debug /subsystem:windows /ENTRY:"wWinMainCRTStartup" /dynamicbase /nxcompat /ignore:4068

:compile
rem Build and run script for NQueens samples
echo rc.exe /d "NDEBUG" /d "_UNICODE" /d "UNICODE" /d "_AFXDLL" /l 0x409 /fo"nq.res" %SRCDIR_COMMON%nq.rc
rc.exe /d "NDEBUG" /d "_UNICODE" /d "UNICODE" /d "_AFXDLL" /l 0x409 /fo"nq.res" %SRCDIR_COMMON%nq.rc

rem nq-win32api
echo %CC% %CC_FLAGS% %EXTRA_INCLUDE% /I %SRCDIR%\nq-win32api -D __NQ_WIN32API %SRCFILES_COMMON% %SRCDIR%nq-win32api\nq-win32api.cpp /link %LINK_FLAGS% /out:nq-win32api.exe /pdb:nq-win32api.pdb nq.res
%CC% %CC_FLAGS% %EXTRA_INCLUDE% /I %SRCDIR%\nq-win32api -D __NQ_WIN32API %SRCFILES_COMMON% %SRCDIR%nq-win32api\nq-win32api.cpp /link %LINK_FLAGS% /out:nq-win32api.exe /pdb:nq-win32api.pdb nq.res

rem nq-tbb-lambda
echo %CC% %CC_FLAGS% %EXTRA_INCLUDE% /Qtbb /I %SRCDIR%nq-tbb-lambda -D __NQ_TBB_LAMBDA -D INTEL_SUITE_VERSION=PE130_17 %SRCFILES_COMMON% %SRCDIR%nq-tbb-lambda\nq-tbb-lambda.cpp /link %LINK_FLAGS% /out:nq-tbb-lambda.exe /pdb:nq-tbb-lambda.pdb nq.res
%CC% %CC_FLAGS% %EXTRA_INCLUDE% /Qtbb /I %SRCDIR%nq-tbb-lambda -D __NQ_TBB_LAMBDA -D INTEL_SUITE_VERSION=PE130_17 %SRCFILES_COMMON% %SRCDIR%nq-tbb-lambda\nq-tbb-lambda.cpp /link %LINK_FLAGS% /out:nq-tbb-lambda.exe /pdb:nq-tbb-lambda.pdb nq.res

rem nq-tbb-intel
echo %CC% %CC_FLAGS% %EXTRA_INCLUDE% /Qtbb /I %SRCDIR%nq-tbb-intel -D __NQ_TBB_INTEL  %SRCFILES_COMMON% %SRCDIR%nq-tbb-intel\nq-tbb-intel.cpp /link %LINK_FLAGS% /out:nq-tbb-intel.exe /pdb:nq-tbb-intel.pdb nq.res 
%CC% %CC_FLAGS% %EXTRA_INCLUDE% /Qtbb /I %SRCDIR%nq-tbb-intel -D __NQ_TBB_INTEL  %SRCFILES_COMMON% %SRCDIR%nq-tbb-intel\nq-tbb-intel.cpp /link %LINK_FLAGS% /out:nq-tbb-intel.exe /pdb:nq-tbb-intel.pdb nq.res

rem nq-serial
echo %CC% %CC_FLAGS% %EXTRA_INCLUDE% /I %SRCDIR%nq-serial -D __NQ_SERIAL %SRCFILES_COMMON% /link %LINK_FLAGS% /out:nq-serial.exe /pdb:nq-serial.pdb nq.res
%CC% %CC_FLAGS% %EXTRA_INCLUDE% /I %SRCDIR%nq-serial -D __NQ_SERIAL %SRCFILES_COMMON% /link %LINK_FLAGS% /out:nq-serial.exe /pdb:nq-serial.pdb nq.res
goto eof

:run
shift
if /i "%1"=="" goto helpmsg
if /i "%1"=="nq-win32api" set EXEC=nq-win32api.exe
if /i "%1"=="nq-tbb-lambda" set EXEC=nq-tbb-lambda.exe
if /i "%1"=="nq-tbb-intel" set EXEC=nq-tbb-intel.exe
if /i "%1"=="nq-serial" set EXEC=nq-serial.exe
if NOT exist %EXEC% (
  echo Cannot find executable from %EXEC%
  goto helpmsg
)
%EXEC%
goto eof

:helpmsg
echo "Syntax: build [|debug|release|run|clean]"
echo "     build debug - Building NQueens in Debug configuration"
echo "     build release - Building NQueens in Release configuration"
echo "     build run [nq-win32api|nq-tbb-lambda|nq-tbb-intel|nq-serial] - run NQueens"
echo "     build clean - clean build directory"
goto eof

:clean
del *.obj *.exe *.pdb *.res *.manifest *.ilk > nul 2>&1

:eof
