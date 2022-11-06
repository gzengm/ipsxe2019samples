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
// [DESCRIPTION]
// Solve the NQueens problem, win32 api version
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

// Thread stuff
int n_threads=_MAXSIZE;

#ifdef _USYNC
int nbrsol[_MAXSIZE] ;
#endif

CRITICAL_SECTION glock;  // declare a global CS object

void SetQueen(int queens[], int row, int col, int id) 
{
	int lid;

	lid=id;

	for(int i=0; i < row; i++) {
		// vertical attacks
		if (queens[i] == col) 
		  return;

		// diagonal attacks
		if (abs(queens[i]-col) == (row-i) )
		  return;

	}

	// column is ok, set the queen
	queens[row] = col;

	if(row == g_nsize-1) {

		#ifdef _USYNC
		nbrsol[lid]++ ;    

		#else   
		//  InterlockedIncrement(&nrOfSolutions);
		EnterCriticalSection(&glock); // start critical section
		nrOfSolutions++;
		LeaveCriticalSection(&glock); // end of critical section

		#endif
	}
	else {
		// try to fill next row
		for(int i=0; i < g_nsize; i++)
			SetQueen(queens, row+1, i, lid);
	}
}

// Worker thread function (to executed by each thread in parallel)
DWORD WINAPI run_solve (void* param) 
{
	// Retrieve arguments passed to this thread: param is a pointer to 
	// void, static_cast converts is to the pointer to thr_params
	thr_params* params = static_cast<thr_params*> (param); 

	// Print messages
	for (int i = params->start; i < params->end; ++i) {
		int* pQ = new int[g_nsize]; 
		// create separate array for each recursion
		SetQueen(pQ, 0, i, params->id);

		delete pQ;
	} 
	return 0;
}

void nqSolve (int num_thr, int nsize) 
{
	g_nsize = nsize;
  
	HANDLE* threads = new HANDLE[num_thr];
	thr_params* params = new thr_params[num_thr];

	for (int i = 0; i < num_thr; ++i) {
		// Give each thread equal number of rows
		params[i].start = i * (nsize / num_thr);
		params[i].end   = params[i].start + (nsize / num_thr);
		params[i].id     = i;  
		// params[i].queens = _queens;

		// Construct a message:
		params[i].msg.AppendFormat(L"Thread #%d", i);

		// Start a thread: it's important to pass argument-pointer to a different 
		// memory for each thread's parameter to avoid data races
		threads[i] = CreateThread (NULL, 0, run_solve, static_cast<void *> (&params[i]), 0, NULL);
	}

	// Join threads: wait until all threads are done
	WaitForMultipleObjects (num_thr, threads, true, INFINITE);

	// Free memory
	delete[] params;
	delete[] threads;
}

// -------------
CString nqGetSolutions(int nSize)
{
	InitializeCriticalSection(&glock);  // allow a single thread access to the CS

	CString strOutText; 
#ifdef _USYNC  
	strOutText = "Starting *unsynchronised* recursive win32 thread solver for size "; 
	for (int i = 0; i < _MAXSIZE; ++i) 
		nbrsol[i]=0;
#else
	strOutText = "Starting synchronised recursive win32 thread solver for size "; 
#endif 
	CString strStamp = GetStampString();

	n_threads = min(nSize, _MAXSIZE);
	strOutText.AppendFormat(L"%d with %d threads. \r\n\tStarting %s\r\n", nSize, n_threads, strStamp);
  
	DWORD startTime=timeGetTime();

	// reset g_nSolutions
	g_nSolutions = 0;
	nqSolve (n_threads, nSize);

	DWORD dwTimeUsed = timeGetTime()-startTime;
  
#ifdef _USYNC
	for (int i = 0; i < n_threads; ++i) {
		g_nSolutions = g_nSolutions + nbrsol[i];
	}
#endif  
	strOutText.AppendFormat(L"    Number of solutions: %d\r\n    Calculations took: %u ms.\r\n\r\n", g_nSolutions, dwTimeUsed);
  
	DeleteCriticalSection(&glock);

	return strOutText;
}
