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
all: debug

CC          = mpiicc
CFLAGS      = -g -O3
C_COMPILE   = $(CC) $(CFLAGS)
srcdir      = src
test        = MPI_Reduce

VPATH = .:src

.SUFFIXES:
.SUFFIXES: .o .c
.c.o:
	@if [ "x$(VERBOSE)" != "x1" ] ; then \
	  echo "  CC              $<" ; \
	else \
	  echo $(C_COMPILE) -c $< ; \
	fi
	@$(C_COMPILE) -c $<
.c:
	@if [ "x$(VERBOSE)" != "x1" ] ; then \
	  echo "  CC              $<" ; \
	else \
	  echo $(C_COMPILE) -o $* $< $(LDFLAGS) ; \
	fi
	@$(C_COMPILE) -o $* $< $(LDFLAGS)

debug: $(test)

run:
	mpiexec.hydra -check_mpi -genv VT_DEADLOCK_TIMEOUT 5s -genv VT_CHECK_MAX_ERRORS 0 -n 2 ./$(test)

clean:
	-rm -f $(test).o $(test)
	-rm -f $(test).prot
