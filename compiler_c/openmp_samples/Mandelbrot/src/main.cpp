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
// ===============================================================

// Initial conditions: rectangle (for image) = { (-2.5, -0.875), (1, 0.875) }
//                     height = 1024
//                     width = 2048
//                     max_depth = 100
//
// Finds the mandelbrot set given initial conditions, and saves results to a bmp image.
// The real portion of the complex number is the x-axis, and the imaginary portion is the y-axis
//
// You can optionally compile with GCC and MSC, but just the linear, scalar version will compile
// and it will not have all optimizations

#include <string>
#include <cmath>
#include <complex>
#include "tbb/tbb.h"
#include "tbb/parallel_for.h"
#include "mandelbrot.h"
#include "bmp_image.h"
#include <algorithm>
#include <vector>
#include <chrono>
#include <stdio.h>
#include <stdlib.h>
#include <emmintrin.h>

using namespace tbb;

int main(int argc, char* argv[]) {
    double x0 = -2.5;
    double y0 = -0.875;
    double x1 = 1;
    double y1 = 0.875;
    int height = 10240;
    // Width should be a multiple of 8
    int width = 20480;
    assert(width%8==0);
    int max_depth = 100;
    io::BMPImage image(width, height, 8);
    unsigned char* output;
    int option = 0;

// If PERF_NUM is defined, then no options taken...run all tests
#ifndef PERF_NUM
    // Checks to see if option was given at command line
    if(argc>1) {
        // Prints out instructions and quits
        if(argv[1][0] == 'h') {
            printf("This example will check how many iterations of z_n+1 = z_n^2 + c a complex set will remain bounded. Pick which parallel method you would like to use.\n");
            printf("[0] all tests\n[1] serial/scalar\n[2] serial/vectorization\n[3] tbb::parallel_for/scalar\n[4] tbb::parallel_for/vectorization\n");
#ifdef _WIN32
            system("PAUSE");
#endif
            return 0;
        }
        else {          
            option = atoi(argv[1]);
        }
    }
    // If no options are given, prompt user to choose an option
    else {
        printf("This example will check how many iterations of z_n+1 = z_n^2 + c a complex set will remain bounded. Pick which parallel method you would like to use.\n");
        printf("[0] all tests\n[1] serial/scalar\n[2] serial/vectorization\n[3] tbb::parallel_for/scalar\n[4] tbb::parallel_for/vectorization\n  > ");
        scanf("%i", &option);
    }
#endif // !PERF_NUM
    
    std::chrono::time_point<std::chrono::system_clock> timer_start, timer_stop;
    std::chrono::duration<double> final_time;

    // initialization for Intel TBB 
    int n, nthreads = tbb::task_scheduler_init::automatic;
    char *nthreads_str = getenv("TBB_NUM_THREADS");
    if (nthreads_str && (sscanf(nthreads_str, "%d", &n) > 0) && (n > 0)) nthreads = n;
    tbb::task_scheduler_init init(nthreads);

        // Simulation
    switch (option) {
    case 0:
    {
#ifdef PERF_NUM
        std::chrono::duration<double> avg_time[4];
        for (int i = 0; i < 5; ++i) {
#endif
        printf("\nRunning all tests\n");

        printf("Starting serial, scalar Mandelbrot...\n");
        timer_start = std::chrono::system_clock::now();
        output = serial_mandelbrot(x0, y0, x1, y1, width, height, max_depth);
        timer_stop = std::chrono::system_clock::now();
        std::chrono::duration<double> serial_time = timer_stop - timer_start;
        printf("Calculation finished. Time taken is %.0f ms\n", serial_time.count()*1000.0);
        printf("Saving image...\n");
        image.from_gray(output);
        image.save("mandelbrot_serial.bmp");
        image.valsig("mandelbrot_serial.valsig");
        _mm_free(output);

        printf("\nStarting pragma omp simd Mandelbrot...\n");
        timer_start = std::chrono::system_clock::now();
        output = simd_mandelbrot(x0, y0, x1, y1, width, height, max_depth);
        timer_stop = std::chrono::system_clock::now();
        std::chrono::duration<double> vec_time = timer_stop - timer_start;
        printf("Calculation finished. Time taken is %.0f ms\n", vec_time.count()*1000.0);
        printf("Saving image...\n");
        image.from_gray(output);
        image.save("mandelbrot_simd.bmp");
        image.valsig("mandelbrot_simd.valsig");
        _mm_free(output);

        printf("\nStarting tbb::parallel_for Mandelbrot...\n");
        timer_start = std::chrono::system_clock::now();
        output = tbb_mandelbrot(x0, y0, x1, y1, width, height, max_depth);
        timer_stop = std::chrono::system_clock::now();
        std::chrono::duration<double> tbb_time = timer_stop - timer_start;
        printf("Calculation finished. Time taken is %.0f ms\n", tbb_time.count()*1000.0);
        printf("Saving image...\n");
        image.from_gray(output);
        image.save("mandelbrot_tbb.bmp");
        image.valsig("mandelbrot_tbb.valsig");
        _mm_free(output);

        printf("\nStarting tbb::parallel_for + pragma omp simd Mandelbrot...\n");
        timer_start = std::chrono::system_clock::now();
        output = tbb_simd_mandelbrot(x0, y0, x1, y1, width, height, max_depth);
        timer_stop = std::chrono::system_clock::now();
        std::chrono::duration<double> tbb_vec_time = timer_stop - timer_start;
        printf("Calculation finished. Time taken is %.0f ms\n", tbb_vec_time.count()*1000.0);
        printf("Saving image...\n");
        image.from_gray(output);
        image.save("mandelbrot_tbb_simd.bmp");
        image.valsig("mandelbrot_tbb_simd.valsig");
        _mm_free(output);
#ifdef PERF_NUM 
        avg_time[0] += serial_time;
        avg_time[1] += vec_time;
        avg_time[2] += tbb_time;
        avg_time[3] += tbb_vec_time;
        }
        printf("Serial and scalar test avg time: %.0f ms\n", avg_time[0].count()*1000.0/5);
        printf("SIMD test avg time: %.0f ms\n", avg_time[1].count()*1000.0/5);
        printf("TBB Parallel test avg time: %.0f ms\n", avg_time[2].count()*1000.0/5);
        printf("SIMD and TBB Parallel test avg time: %.0f ms\n", avg_time[3].count()*1000.0/5);
#endif
    }
    break;

    case 1:
    {
        printf("Starting serial, scalar Mandelbrot...\n");
        timer_start = std::chrono::system_clock::now();
        output = serial_mandelbrot(x0, y0, x1, y1, width, height, max_depth);
        timer_stop = std::chrono::system_clock::now();
        final_time = timer_stop - timer_start;
        printf("Calculation finished. Time taken is %.0f ms\n", final_time.count()*1000.0);
        printf("Saving image...\n");
        image.from_gray(output);
        image.save("mandelbrot_serial.bmp");
        image.valsig("mandelbrot_serial.valsig");
        _mm_free(output);
    }
    break;

    case 2:
    {
        printf("\nStarting pragma omp simd Mandelbrot...\n");
        timer_start = std::chrono::system_clock::now();
        output = simd_mandelbrot(x0, y0, x1, y1, width, height, max_depth);
        timer_stop = std::chrono::system_clock::now();
        final_time = timer_stop - timer_start;
        printf("Calculation finished. Time taken is %.0f ms\n", final_time.count()*1000.0);
        printf("Saving image...\n");
        image.from_gray(output);
        image.save("mandelbrot_simd.bmp");
        image.valsig("mandelbrot_simd.valsig");
        _mm_free(output);
    }
    break;

    case 3:
    {
        printf("\nStarting tbb::parallel_for Mandelbrot...\n");
        timer_start = std::chrono::system_clock::now();
        output = tbb_mandelbrot(x0, y0, x1, y1, width, height, max_depth);
        timer_stop = std::chrono::system_clock::now();
        final_time = timer_stop - timer_start;
        printf("Calculation finished. Time taken is %.0f ms\n", final_time.count()*1000.0);
        printf("Saving image...\n");
        image.from_gray(output);
        image.save("mandelbrot_tbb.bmp");
        image.valsig("mandelbrot_tbb.valsig");
        _mm_free(output);
    }
    break;

    case 4:
    {
        printf("\nStarting tbb::parallel_for + pragma omp simd Mandelbrot...\n");
        timer_start = std::chrono::system_clock::now();
        output = tbb_simd_mandelbrot(x0, y0, x1, y1, width, height, max_depth);
        timer_stop = std::chrono::system_clock::now();
        final_time = timer_stop - timer_start;
        printf("Calculation finished. Time taken is %.0f ms\n", final_time.count()*1000.0);
        printf("Saving image...\n");
        image.from_gray(output);
        image.save("mandelbrot_tbb_simd.bmp");
        image.valsig("mandelbrot_tbb_simd.valsig");
        _mm_free(output);
    }
    break;

    default:
        printf("Please pick a valid option\n");
        break;
    }

#ifdef _WIN32
    system("PAUSE");
#endif
    return 0; 
}
