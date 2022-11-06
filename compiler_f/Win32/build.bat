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
rem Build script for Win32 samples

rem Angle
cd Angle
call build.bat
cd ..

rem Crypto
cd Crypto
call build.bat
cd ..

rem Generic
cd Generic
call build.bat
cd ..

rem GetOpenFileName
cd GetOpenFileName
call build.bat
cd ..

rem MemoryStatus
cd MemoryStatus
call build.bat
cd ..

rem Platform
cd Platform
call build.bat
cd ..

rem ProcessorInfo
cd ProcessorInfo
call build.bat
cd ..

rem ProcessStatus
cd ProcessStatus
call build.bat
cd ..


rem End Win32