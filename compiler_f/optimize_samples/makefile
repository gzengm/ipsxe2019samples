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
#Build script for optimize sample
#Uncomment one of the following with which you wish to compile
FC = ifort -O0 
#FC = ifort -O1 
#FC = ifort -O2 
#FC = ifort -O3 

OBJS = int_sin.o

all:int_sin

int_sin: $(OBJS)
	ifort $^ -o $@

%.o: src/%.f90
	$(FC) $^ -c

clean:
	/bin/rm -f core.* $(OBJS) int_sin

