##==============================================================
##
## SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
## http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
##
## Copyright 2019 Intel Corporation
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
##  Build for coarray sample
##******************************************************************************
FC = ifort

release: hello_image.exe

debug: hello_image_dbg.exe

run: release ; ./hello_image.exe

debug_run: debug ; ./hello_image_dbg.exe

hello_image.exe: hello_image.o
	$(FC) -O2 -coarray=shared -coarray-num-images=4 $^ -o $@

hello_image_dbg.exe: hello_image_dbg.o
	$(FC) -O0 -coarray=shared -coarray-num-images=4 $^ -o $@

%.o: src/%.f90
	$(FC) -O2 -c -coarray=shared -coarray-num-images=4 -o $@  $<

%_dbg.o: src/%.f90
	$(FC) -O0 -c -coarray=shared -coarray-num-images=4 -o $@  $<

clean:
	/bin/rm -f core.* *.o *.exe
