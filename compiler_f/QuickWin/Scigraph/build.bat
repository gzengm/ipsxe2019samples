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

rem Build script for Scigraph sample

rem Build library

ifort /nologo /c /libdir:noauto src/sgdata.f90
ifort /nologo /c /libdir:noauto src/sglowlvl.f90
ifort /nologo /c /libdir:noauto src/sgdraw.f90
ifort /nologo /c /libdir:noauto src/sgplot.f90
ifort /nologo /c /libdir:noauto src/sgadmin.f90
ifort /nologo /c /libdir:noauto src/scigraph.f90
lib /nologo /out:scigraph.lib sgdata.obj sglowlvl.obj sgdraw.obj sgplot.obj sgadmin.obj scigraph.obj

rem build demo

ifort /nologo /libs:qwin src/sgdemo.f90 scigraph.lib
