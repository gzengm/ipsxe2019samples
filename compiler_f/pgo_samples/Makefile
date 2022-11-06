##==============================================================
##
## SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
## http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
##
## Copyright 2017 Intel Corporation
##
## THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
## NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
## PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
##
## =============================================================
##
##
##******************************************************************************
## Content:
##
##  Intel(R) Parallel Studio XE
##  Build for pgo_sample
##******************************************************************************
#
FC = ifort -prof-gen 
FC2 = ifort -prof-use -qopt-report=1 -qopt-report-phase=pgo -qopt-report-file=stderr

all: pgo_gen.exe genprofile pgo_use.exe

pgo_gen.exe:
	$(FC) src/pgo_sample.f90 -o $@

genprofile:
	./pgo_gen.exe

pgo_use.exe:
	$(FC2) src/pgo_sample.f90 -o $@

clean:
	/bin/rm -f *.exe *.mod *.dyn *.dpi *.lock

