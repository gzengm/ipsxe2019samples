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
##  Build for ipo_sample
##******************************************************************************

NAME :=	ipo_sample
SRCS := ipo_sample_main.f90 ipo_sample_init.f90 ipo_sample_sum.f90
OBJS := $(SRCS:.f90=.o)
RPT  := $(NAME).optrpt
EXE  := $(NAME).out
FC   := ifort
LD   := ifort
FFLAGS := -ipo
LDFLAGS := -qopt-report=3 -qopt-report-file=$(RPT)
vpath %.f90 src


all:	$(EXE)
	@echo "...."
	@echo "Build complete"
	@echo "Open in text editor : "$(RPT)

help:
	@echo "Usage:"
	@echo "    Build		: make"
	@echo 

$(EXE) : $(OBJS)
	$(LD) -V $^ $(FFLAGS) $(LDFLAGS) -o $@

%.o : %.f90 
	$(FC) $(FFLAGS) -c $< -o $@

run : $(EXE)
	./$(EXE)

clean :	
	rm -fr $(EXE) $(OBJS) $(RPT)
