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

// [DESCRIPTION]
// Solve the NQueens problem, Intel(R) Threading Building Blocks (Intel(R) TBB)
// version
//
// [EXPECTED OUTPUT]
// Board Size   Number of Solutions
//     4                2
//     5               10
//     6                4
//     7               40
//     8               92
//     9              352
//    10              724
//    11             2680
//    12            14200
//    13            73712
//    14           365596
//    15          2279184
//=============================================================
#include "stdafx.h"
#include <mmsystem.h>
#include <stdlib.h>
#include "nq.h"

// define tbb mutexes
 spin_mutex mutexforSolutions;

void SetQueen(int queens[], int nsize, int row, int col) 
{
	int i = 0;
	for (i=0; i<row; i++) {
		// vertical attacks
		if (queens[i]==col)
			return;
		// diagonal attacks
		if (abs(queens[i]-col) == (row-i) )
			return;
	}

	// column is ok, set the queen
	queens[row]=col;

	if (row==nsize-1) 
	{
		{ 
			// increment is not atomic, so setting a lock is required here
			 spin_mutex::scoped_lock mylock(mutexforSolutions);

			g_nSolutions++;
		} // closing block invokes scoped_lock destructor which releases the lock
	}
	else
		// try to fill next row (recursion)
		for (i=0; i<nsize; i++) 
			SetQueen(queens, nsize, row+1, i);
}

void nqSolve(int nsize) 
{
	// do a parallel for over the n positions in the first row.
	// Let the scheduler decide how the n tasks are distrubuted 
	// on the different threads
	 parallel_for(blocked_range<size_t>(0, nsize, 1), nqTBBfor(nsize) );

	/*
	 { 
	int* pNQ = new int[nsize];
	for(int i=0; i<nsize; i++)
		// try all positions in first row
		// create separate array for each recursion
		SetQueen(pNQ, nsize, 0, i);
	delete pNQ;
	}
	*/
}

CString nqGetSolutions(int nSize)
{
	CString strStamp = GetStampString();
	CString strOutText("Starting TBB solver for size "); 
	strOutText.AppendFormat(L"%d, starting %s\r\n", nSize, strStamp);

	DWORD startTime=timeGetTime();

	// reset g_nSolutions
	g_nSolutions = 0;

	// calc the solutions
	nqSolve(nSize);

	DWORD dwTimeUsed = timeGetTime()-startTime;

	strOutText.AppendFormat(L"    Number of solutions: %d\r\n    Calculations took: %u ms.\r\n\r\n", g_nSolutions, dwTimeUsed);

	return strOutText;
}
