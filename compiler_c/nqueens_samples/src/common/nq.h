//==============================================================
//
// SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
// http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
//
// Copyright Intel Corporation
//
// THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
// NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
//
// =============================================================
// nq.h : class definition for CNQueen

#pragma once

#include "nq-serial.h"
CString nqGetSolutions(int nSize);
CString GetStampString();

#if defined(__NQ_TBB_INTEL)
#include "nq-tbb-intel.h"

#elif defined(__NQ_TBB_LAMBDA)
#include "nq-tbb-lambda.h"

#elif defined(__NQ_WIN32API)
#include "nq-win32api.h"

#elif defined(__NQ_STL)
#include "nq-stl.h"

#endif

