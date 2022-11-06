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
rem      Windows Build Script for ITAC: Data type mismatch sample
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
    MSBuild.exe /t:Build /p:Configuration=Release /p:Platform=x64 /p:SkipLink=false
    @ECHO OFF
)
if "x%run%" == "xtrue" (
    @ECHO ON
    mpiexec.exe -check_mpi -genv VT_DEADLOCK_TIMEOUT 5s -n 2 x64\Release\size_mismatch.exe
    @ECHO OFF
)

endlocal