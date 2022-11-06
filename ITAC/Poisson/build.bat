@ECHO OFF
setlocal
rem ****************************************************************************
rem                      
rem SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
rem http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
rem                                                                                     
rem Copyright 2018 Intel Corporation                                                    
rem                                                                                     
rem THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT 
rem NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
rem PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
rem
rem ****************************************************************************
rem ****************************************************************************
rem Content:
rem      Windows Build Script for ITAC: Poisson Fortran90 sample
rem ****************************************************************************
set clean=
set release=
set run=

set args_for_processing=%*
set "args_for_processing=%args_for_processing:"=%"
:LOOP
set "args_for_processing=%args_for_processing:)=%"
for /F "tokens=1,2 delims= " %%G in ("%args_for_processing%") do (
    set argt=%%G
    set paramt=%%H
    call set arg_rest=!paramt!
)
if "%argt%" == "clean" (
    set clean=true
) else if "%argt%" == "release" (
    set release=true
) else if "%argt%" == "run" (
    set run=true
)
:AFTER_CYCLE
if "x%paramt%" == "x" goto AFTERLOOP
set args_for_processing=%args_for_processing:* =%
goto LOOP
:AFTERLOOP

if "x%clean%" == "xtrue" (
    if exist x64 (
        del /S /Q x64  > nul 2>&1
        rmdir /S /Q x64\Debug > nul 2>&1
        rmdir /S /Q x64\Release > nul 2>&1
        rmdir /S /Q x64 > nul 2>&1
    )
)
if "x%release%" == "xtrue" (
    @ECHO ON
    devenv.exe poisson.sln /rebuild Release
    copy x64\poisson_sendrecv\Release\poisson_sendrecv.exe poisson_sendrecv.exe
    copy x64\poisson_icomm\Release\poisson_icomm.exe poisson_icomm.exe
    @ECHO OFF
)
if "x%run%" == "xtrue" (
    @ECHO ON
    set VT_CONFIG=data\VT_CONFIG
    mpiexec.exe -n 16 poisson_sendrecv.exe
    mpiexec.exe -n 16 poisson_icomm.exe
    @ECHO OFF
)

endlocal