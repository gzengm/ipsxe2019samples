# ==============================================================
#
# SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
# http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
#
# Copyright 2005-2019 Intel Corporation
#
# THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
# NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
#
# =============================================================

# Common Makefile that builds and runs example.

# Just specify your program basename
PROG=Cholesky
ARGS=4 2

# Trying to find if icl.exe is set
CXX1 = $(TBB_CXX)-
CXX2 = $(CXX1:icl.exe-=icl.exe)
CXX  = $(CXX2:-=cl.exe)

# The C++ compiler options
MYCXXFLAGS = /TP /EHsc /W3 /nologo /D _CONSOLE /D _MBCS /D WIN32 /D _WIN32_WINNT=0x0501 $(CXXFLAGS)
MYLDFLAGS = /INCREMENTAL:NO /NOLOGO /DEBUG /FIXED:NO $(LDFLAGS)

# MKL support
MKL_LIBS1 = $(TBB_TARGET_ARCH)
MKL_LIBS2 = $(MKL_LIBS1:ia32=mkl_core.lib mkl_sequential.lib mkl_intel_c.lib)
MKL_LIBS  = $(MKL_LIBS2:intel64=mkl_core.lib mkl_sequential.lib mkl_intel_lp64.lib)

all: release test
release: compiler_check
	$(CXX) *.cpp /MD /O2 /D NDEBUG $(MYCXXFLAGS) /link tbb.lib $(MKL_LIBS) $(LIBS) $(MYLDFLAGS) /OUT:$(PROG).exe
debug: compiler_check
	$(CXX) *.cpp /MDd /Od /Zi /D TBB_USE_DEBUG /D _DEBUG $(MYCXXFLAGS) /link tbb_debug.lib $(MKL_LIBS) $(LIBS) $(MYLDFLAGS) /OUT:$(PROG).exe
profile: compiler_check
	$(CXX) *.cpp /MD /O2 /Zi /D NDEBUG $(MYCXXFLAGS) /D TBB_USE_THREADING_TOOLS /link tbb.lib $(MKL_LIBS) $(LIBS) $(MYLDFLAGS) /OUT:$(PROG).exe
clean:
	@cmd.exe /C del $(PROG).exe *.obj *.?db *.manifest
test:
	$(PROG) $(ARGS)
compiler_check:
	@echo compiler_test>compiler_test && @$(CXX) /E compiler_test >nul 2>&1  || echo "$(CXX) command not found. Check if CXX=$(CXX) is set properly"
	@cmd.exe /C del compiler_test
