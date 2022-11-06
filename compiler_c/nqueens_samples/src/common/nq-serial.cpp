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
// Solve the NQueens problem, serial version
// 
// [RUN]
// From the command line:
//      nq-serial.exe boardSize
// where boardSize is the size of the board, in the range 4-20
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
#include "nq-serial.h"

CString GetStampString()
{
	SYSTEMTIME curTime;
    GetLocalTime(&curTime);

	CString strOutText(""); 
	strOutText.AppendFormat(L"%02d/%02d, %02d:%02d:%02d", curTime.wMonth, curTime.wDay, curTime.wHour, curTime.wMinute, curTime.wSecond);

	return strOutText; 
}

void nqSerialSetQueen(int queens[], int row, int col) 
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

	if (row==g_nsize-1)
	{
		g_nSolutions++;
	}
	else {
		// try to fill next row
		for (i=0; i<g_nsize; i++)
		  nqSerialSetQueen(queens, row+1, i);
	}
}



void nqSerialSolve(int nsize)
{
	g_nsize = nsize; 


	for(int i=0; i<g_nsize; i++)
	{
		// create separate array for each recursion
		int* pNQ = new int[g_nsize];

		// try all positions in first row
		nqSerialSetQueen(pNQ, 0, i);

		delete pNQ;
	}

}


CString nqSerialGetSolutions(int nSize)
{
	CString strStamp = GetStampString();
	CString strOutText("Starting serial recursive solver for size "); 
	strOutText.AppendFormat(L"%d, starting %s\r\n", nSize, strStamp);

	DWORD startTime=timeGetTime();

	// reset g_nSolutions
	g_nSolutions = 0;

	// calc the solutions
	nqSerialSolve(nSize);

	DWORD dwTimeUsed = timeGetTime()-startTime;

	strOutText.AppendFormat(L"    Number of solutions: %d\r\n    Calculations took: %u ms.\r\n\r\n", g_nSolutions, dwTimeUsed);

	return strOutText;
}
