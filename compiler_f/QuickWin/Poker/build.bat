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

rem Build file for Poker sample

cd src/
Rc /l 0x0409 /fo Poker.res Poker.rc
deftofd resource.h resource.fd

ifort /nologo /libs:qwin /c Descript.f90
ifort /nologo /libs:qwin /c Bitmap.f90
ifort /nologo /libs:qwin /c Cards.f90
ifort /nologo /libs:qwin /c Button.f90
ifort /nologo /libs:qwin /c Misc.f90
ifort /nologo /libs:qwin /c Scoring.f90
ifort /nologo /libs:qwin /c Registry.f90
ifort /nologo /libs:qwin /c Subclass.f90
ifort /nologo /libs:qwin /c Poker.f90

ifort /nologo /libs:qwin poker.obj descript.obj bitmap.obj cards.obj button.obj misc.obj scoring.obj registry.obj subclass.obj poker.res

move poker.exe ..
cd ..