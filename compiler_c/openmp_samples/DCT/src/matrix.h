//=======================================================================================
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
// ======================================================================================

#define ALIGNMENT 32 //Set to 16 bytes for SSE architectures and 32 bytes for AVX architectures
#include<iostream>
#include<xmmintrin.h>
#include<string.h>
using namespace std;

class matrix_serial {
public: 
    float *ptr;
    int row_size;
    matrix_serial(int);
    ~matrix_serial();
    matrix_serial(matrix_serial&);
    void create_identity();
    matrix_serial operator*(matrix_serial &);
    matrix_serial &operator=(const matrix_serial &);
    matrix_serial operator-(int);
    void transpose(matrix_serial &);
    //friend ostream& operator<<(ostream &, matrix_serial &);
};

class matrix_SIMD {
public:
    float *ptr;
    int row_size;
    matrix_SIMD(int);
    ~matrix_SIMD();
    matrix_SIMD(matrix_SIMD&);
    void create_identity();
    matrix_SIMD operator*(matrix_SIMD &);
    matrix_SIMD &operator=(const matrix_SIMD &);
    matrix_SIMD operator-(int);
    void transpose(matrix_SIMD &);
    //friend ostream& operator<<(ostream &, matrix_SIMD &);
};
