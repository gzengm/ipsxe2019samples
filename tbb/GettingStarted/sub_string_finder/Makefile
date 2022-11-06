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

# Common Makefile that builds and runs examples.
PROG=sub_string_finder_extended
ARGS=
LIGHT_PROG=sub_string_finder

# The C++ compiler options
# Trying to find if icl.exe is set
CXX1 = $(TBB_CXX)-
CXX2 = $(CXX1:icl.exe-=icl.exe)
CXX  = $(CXX2:-=cl.exe)

offload1 = $(offload)+
OFFLOADFLAGS1 = $(offload1:mic+=/Qoffload-arch=mic)
OFFLOADFLAGS2 = $(OFFLOADFLAGS1:mic-avx512+=/Qoffload-arch=mic-avx512)
offload2 = $(offload:mic-avx512=mic)
OFFLOADFLAGS = $(OFFLOADFLAGS2:+=) $(offload2:mic=/D__TBB_MIC_OFFLOAD /Qdiag-disable:3335,3440,3441)

MYCXXFLAGS = /EHsc /W3 /nologo /D _CONSOLE /D _MBCS /D WIN32 $(OFFLOADFLAGS) $(CXXFLAGS)
MYLDFLAGS =/INCREMENTAL:NO /NOLOGO /DEBUG /FIXED:NO $(LDFLAGS)

TBB_MIC_LIB = $(offload2:mic=/Qtbb)
TBB_DEBUG_MIC_LIB = $(offload2:mic=/Qoffload-option,mic,link,"-ltbb_debug")

all: release test
release: compiler_check
	$(CXX) sub_string_finder.cpp /MD /O2 /D NDEBUG $(MYCXXFLAGS) $(TBB_MIC_LIB) /link tbb.lib $(LIBS) $(MYLDFLAGS) /OUT:sub_string_finder.exe
	$(CXX) sub_string_finder_extended.cpp /MD /O2 /D NDEBUG $(MYCXXFLAGS) $(TBB_MIC_LIB) /link tbb.lib $(LIBS) $(MYLDFLAGS) /OUT:sub_string_finder_extended.exe
	$(CXX) sub_string_finder_pretty.cpp /MD /O2 /D NDEBUG $(MYCXXFLAGS) $(TBB_MIC_LIB) /link tbb.lib $(LIBS) $(MYLDFLAGS) /OUT:sub_string_finder_pretty.exe
debug: compiler_check
	$(CXX) sub_string_finder.cpp /MDd /Od /Zi /D TBB_USE_DEBUG /D _DEBUG $(MYCXXFLAGS) $(TBB_DEBUG_MIC_LIB) /link tbb_debug.lib $(LIBS) $(MYLDFLAGS) /OUT:sub_string_finder.exe
	$(CXX) sub_string_finder_extended.cpp /MDd /Od /Zi /D TBB_USE_DEBUG /D _DEBUG $(MYCXXFLAGS) $(TBB_DEBUG_MIC_LIB) /link tbb_debug.lib $(LIBS) $(MYLDFLAGS) /OUT:sub_string_finder_extended.exe
	$(CXX) sub_string_finder_pretty.cpp /MDd /Od /Zi /D TBB_USE_DEBUG /D _DEBUG $(MYCXXFLAGS) $(TBB_DEBUG_MIC_LIB) /link tbb_debug.lib $(LIBS) $(MYLDFLAGS) /OUT:sub_string_finder_pretty.exe
clean:
	@cmd.exe /C del sub_string_finder*.exe *.obj *.?db *.manifest
test:
	$(PROG) $(ARGS)
light_test:
	$(LIGHT_PROG) $(ARGS)
compiler_check:
	@echo compiler_test>compiler_test && @$(CXX) /E compiler_test >nul 2>&1  || echo "$(CXX) command not found. Check if CXX=$(CXX) is set properly"
	@cmd.exe /C del compiler_test
