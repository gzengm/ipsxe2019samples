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
##  Build for vec_sample
##****************************************************************************** 
#
FC=ifort
FFLAGS=-O2 -qopt-report-phase=vec -qopt-report=2
OBJ=src/driver.o src/matvec.o

all : matvec

run : matvec
	./matvec

src/%.o: src/%.f90
	$(FC) $(FFLAGS) -c $< -o $@

matvec: $(OBJ)
	$(FC) -V $^ -o matvec

clean:
	-rm -f matvec $(OBJ) src/*.optrpt
