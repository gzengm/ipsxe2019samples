/*
 * SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
 * http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
 *
 * Copyright Intel Corporation
 *
 * THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
 *
 * [DESCRIPTION]
 * Each element of the product matrix c[i][j] is 
 * computed from a unique row and
 * column of the factor matrices, a[i][k] and b[k][j].
 *
 * In the multithreaded implementation, each thread can
 * concurrently compute some submatrix of the product without
 * needing OpenMP data or control synchronization.
 *
 * The algorithm uses OpenMP* to parallelize the outer-most loop,
 * using the "i" row index.
 *
 * Both the outer-most "i" loop and middle "k" loop are manually
 * unrolled by 4.  The inner-most "j" loop iterates one-by-one
 * over the columns of the product and factor matrices.
 *
 * [COMPILE]
 * Use the following compiler options to compile both multi- and 
 * single-threaded versions.
 *
 * Parallel compilation:
 *  You must set the stacksize to an appropriate size; otherwise,
 *  the application will generate a segmentation fault. 
 *  Linux* and macOS*: appropriate ulimit commands are shown for 
 *  bash shell.
 *
 *  Windows*: /Qstd=c99 /Qopenmp /F256000000
 *            In Microsoft Visual Studio, /F256000000 is set by
 *			  Linker > System > Stack Reserve Size > 256000000
 *
 *  Linux*:   ulimit -s unlimited
 *            -std=c99 -qopenmp
 * 
 *  macOS*:  ulimit -s 64000
 *            -std=c99 -qopenmp
 *
 * Serial compilation:
 *
 *  Use the same command, but omit the -qopenmp (Linux and macOS)
 *  or /Qopenmp (Windows) option.
 *
 */

#include <stdio.h>
#include <float.h>
#include <math.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#ifdef _WIN32
#include <Windows.h>
#include <intrin.h>
#else
#include <sys/time.h>
#endif
#define bool _Bool
#define true 1
#define false 0

// Matrix size constants
// Be careful to set your shell's stacksize limit to a high value if you
// wish to increase the SIZE.
#define SIZE     4800     // Must be a multiple of 8.
#define M        SIZE/8
#define N        SIZE/4
#define P        SIZE/2
#define NTIMES   20        // product matrix calculations

// Description:
// Return the current time value in seconds
// Microsoft Windows* uses QueryPerformanceFrequency/QueryPerformanceCounter for time
// Linux*/macOS* uses get_timeofday for time
double get_time() {
#ifdef _WIN32

	// Time
        unsigned __int64 m_frequency;
	// QueryPerformanceFrequency works with QueryPerformanceCounter to return a human-readable time, provided in Windows.h
	QueryPerformanceFrequency((LARGE_INTEGER *)&m_frequency);
	unsigned __int64 now;
	QueryPerformanceCounter((LARGE_INTEGER *)&now);
	// Divide the raw counter by m_frequency for time in seconds
	return ((double)(now) / m_frequency);
#else

	// Time
	struct timeval now;
	gettimeofday(&now, 0); //Returns the time of the day
	//tv_sec records time in seconds and tv_usec records time in micro seconds
	return ((double) now.tv_sec + (double) now.tv_usec/1000000.0); 
#endif
}

int main(void)
{
  double a[M][N], b[N][P], c[M][P], start, walltime;
  bool nthr_checked=false;

  int i, j, k, l, nthr=1;

  printf("Problem size: c(%d,%d) = a(%d,%d) * b(%d,%d)\n",
         M, P, M, N, N, P);
  printf("Calculating product %d time(s)\n", NTIMES);

  // a is identity matrix
  for (i=0; i<M; i++)
    for (j=0; j<N; j++)
      a[i][j] = 1.0;

  // each column of b is the sequence 1,2,...,N
  for (i=0; i<N; i++)
    for (j=0; j<P; j++)
      b[i][j] = i+1.;

  start = get_time();

  for (l=0; l<NTIMES; l++) {
	#pragma omp parallel private(i,j,k) 
	  {
		#pragma omp single nowait
		if (!nthr_checked) {
#ifdef _OPENMP
			nthr = omp_get_num_threads();
#endif
			printf( "\nWe are using %d thread(s)\n", nthr);
			nthr_checked = true;
		}

	    // Initialize product matrix
		#pragma omp for nowait
		for (i=0; i<M; i++)
			for (j=0; j<P; j++)
				c[i][j] = 0.0;

		// Parallelize by row.  The threads don't need to synchronize at
		// loop end, so "nowait" can be used.
		#pragma omp for nowait
		for (i=0; i<M; i++) {
			for (k=0; k<N; k++) {
				// Each element of the product is just the sum 1+2+...+n
				for (j=0; j<P; j++) {
					c[i][j]  += a[i][k]  * b[k][j];
				}
			}
		}
	} // #pragma omp parallel private(i,j,k)
  } // l=0,...NTIMES-1

  walltime = get_time() - start;
  printf("\nFinished calculations.\n");
  printf("Matmul kernel wall clock time = %.2f sec\n", walltime);
  printf("Wall clock time/thread = %.2f sec\n", walltime/nthr);
  printf("MFlops = %f\n",
      (double)(NTIMES)*(double)(N*M*2)*(double)(P)/walltime/1.0e6);

  return 0;
}
