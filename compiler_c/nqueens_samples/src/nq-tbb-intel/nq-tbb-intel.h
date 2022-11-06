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

// nq-tbb-intel.h : class definition for CNQueen
//
#pragma once



#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/spin_mutex.h"
using namespace tbb;

void SetQueen(int queens[], int nsize, int row, int col) ;

class nqTBBfor
{
public:
	nqTBBfor(int nsize): _nsize(nsize) {};
	void operator()( const blocked_range<size_t> & r ) const {
		for( size_t i=r.begin(); i!=r.end(); ++i ) {
			int *pNQ = new int[_nsize];
			SetQueen(pNQ, _nsize, 0, (int)i);

			delete(pNQ);
		}
	}
	int _nsize;
};

