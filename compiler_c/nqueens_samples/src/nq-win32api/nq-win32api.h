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
//
// nq-win32api.h : class definition for CNQueen
//
#pragma once

#define _MULTI_THREADED
#define _MAXSIZE 24

#define _USYNC

typedef struct _thr_params {
	int start;
	int end;
	int id;
	// int _queen[];		// not used
	CString msg;
} thr_params;

#ifdef _USYNC
	extern int _nbrsol[_MAXSIZE] ;
#endif

extern CRITICAL_SECTION glock;  // declare a global CS object
