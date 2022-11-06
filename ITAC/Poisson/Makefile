#
# SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
# http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
#
# Copyright 2018 Intel Corporation
#
# THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
# NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
#
all: release

FC          = mpiifort
CFLAGS      = -O3
F_COMPILE   = $(FC) $(CFLAGS)
LDFLAGS     = -trace
srcdir      = src
tests       = poisson_sendrecv poisson_icomm

POISSON_SENDRECV_SRCS = ErrHnd.f90 types.f90 boundary_functions.f90 pardat_sendrecv.f90 output.f90  red_black.f90
POISSON_SENDRECV_OBJS = $(POISSON_SENDRECV_SRCS:.f90=.o)
POISSON_ICOMM_SRCS = ErrHnd.f90 types.f90 boundary_functions.f90 pardat_icomm.f90 output.f90 red_black.f90  
POISSON_ICOMM_OBJS = $(POISSON_ICOMM_SRCS:.f90=.o)


VPATH = .:src

.SUFFIXES:
.SUFFIXES: .o .f90
%.o: %.f90
	@if [ "x$(VERBOSE)" != "x1" ] ; then \
	  echo "  FC              $<" ; \
	else \
	  echo $(F_COMPILE) -c $< ; \
	fi
	@$(F_COMPILE) -c $<

poisson_sendrecv: $(POISSON_SENDRECV_OBJS)
	@if [ "x$(VERBOSE)" != "x1" ] ; then \
	  echo "  FC              $<" ; \
	else \
	  echo $(F_COMPILE) $(POISSON_SENDRECV_OBJS) -o poisson_sendrecv $(LDFLAGS) ; \
	fi
	@$(F_COMPILE) $(POISSON_SENDRECV_OBJS) -o poisson_sendrecv $(LDFLAGS)

poisson_icomm: $(POISSON_ICOMM_OBJS)
	@if [ "x$(VERBOSE)" != "x1" ] ; then \
	  echo "  FC              $<" ; \
	else \
	  echo $(F_COMPILE) $(POISSON_ICOMM_OBJS) -o poisson_icomm $(LDFLAGS) ; \
	fi
	@$(F_COMPILE) $(POISSON_ICOMM_OBJS) -o poisson_icomm $(LDFLAGS)

release: $(tests)

run:
	VT_CONFIG=data/VT_CONFIG mpiexec.hydra -n 16 ./poisson_sendrecv
	VT_CONFIG=data/VT_CONFIG mpiexec.hydra -n 16 ./poisson_icomm

clean:
	rm -rf $(tests) poisson_sendrecv.* poisson_icomm.* *.o *.mod
