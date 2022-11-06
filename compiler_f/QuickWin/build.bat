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

rem Build script for QuickWin samples

rem Calendar
cd Calendar
call build.bat
cd ..

rem Poker
cd Poker
call build.bat
cd ..

rem QWPaint
cd QWPaint
call build.bat
cd ..

rem Scigraph
cd Scigraph
call build.bat
cd ..