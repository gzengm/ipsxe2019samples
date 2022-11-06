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

#include"matrix.h"

    matrix_SIMD::matrix_SIMD(int size) {
        ptr = (float *)_mm_malloc(sizeof(float)*size*size, ALIGNMENT);
        memset(ptr, 0, sizeof(float)*size*size);
        row_size = size;
    }
    matrix_SIMD::~matrix_SIMD(){
        _mm_free(ptr);
    }

    void matrix_SIMD::create_identity(){
        int size = row_size * row_size;
        #pragma omp simd
        for(int i = 0; i < row_size; i++)
        {
            *(ptr + (i * row_size) + i) = 1;
        }
        return;
    }

    matrix_SIMD::matrix_SIMD(matrix_SIMD &m) {
        ptr = (float *)_mm_malloc(sizeof(float)*m.row_size*m.row_size, ALIGNMENT);
        memcpy(ptr, m.ptr, sizeof(float)*m.row_size*m.row_size);
        row_size = m.row_size;
    }

     matrix_SIMD matrix_SIMD::operator*(matrix_SIMD &y){
        int size = y.row_size;
        matrix_SIMD temp(size);
        auto ptr_copy = ptr;
        auto tempptr_copy = temp.ptr;
        auto yptr_copy = y.ptr;

        for(int i = 0; i < size; i++)
        {
            #pragma omp simd
            for(int j = 0; j < size; j++)
            {
                tempptr_copy[(i * size) + j] = 0;
                for(int k = 0; k < size; k++)
                    tempptr_copy[(i * size) + j] += (ptr_copy[(i * size) + k] * yptr_copy[(k * size) + j]);
            }
        }
        return temp;
    }

    matrix_SIMD &matrix_SIMD::operator=(const matrix_SIMD &temp){
        int row_stride = temp.row_size;
        int size = (row_stride * row_stride);
        #pragma omp simd
        for(int i = 0; i < row_stride; i++)
        {
            for(int j = 0; j < row_stride; j++)
                ptr[(i * row_stride) + j] = temp.ptr[(i * row_stride) + j];
        }
        return *this;
    }

    matrix_SIMD matrix_SIMD::operator-(int num){
        matrix_SIMD temp(row_size);
        int size = (row_size * row_size);
        #pragma omp simd
        for (int i = 0; i < size; i++)
            temp.ptr[i] -= num;
        return temp;
    }

    void matrix_SIMD::transpose(matrix_SIMD &output){
        int size = row_size;
        #pragma omp simd
        for(int i = 0; i < size; i++)
        {
            for(int j = 0; j < size; j++)
                output.ptr[(j * size) + i] = ptr[(i * size) + j];
        }
        return;
    }


    matrix_serial::matrix_serial(int size) {
        ptr = (float *)_mm_malloc(sizeof(float)*size*size, ALIGNMENT);
        memset(ptr, 0, sizeof(float)*size*size);
        row_size = size;
    }
    matrix_serial::~matrix_serial(){
        _mm_free(ptr);
    }

    void matrix_serial::create_identity(){
        int size = row_size * row_size;
        for(int i = 0; i < row_size; i++)
        {
            *(ptr + (i * row_size) + i) = 1;
        }
        return;
    }

    matrix_serial::matrix_serial(matrix_serial &m) {
        ptr = (float *)_mm_malloc(sizeof(float)*m.row_size*m.row_size, ALIGNMENT);
        memcpy(ptr, m.ptr, sizeof(float)*m.row_size*m.row_size);
        row_size = m.row_size;
    }

    matrix_serial matrix_serial::operator*(matrix_serial &y){
        int size = y.row_size;
        matrix_serial temp(size);
        for(int i = 0; i < size; i++)
        {
            for(int j = 0; j < size; j++)
            {
                temp.ptr[(i * size) + j] = 0;
                for(int k = 0; k < size; k++)
                    temp.ptr[(i * size) + j] += (ptr[(i * size) + k] * y.ptr[(k * size) + j]);
            }
        }
        return temp;
    }
    matrix_serial &matrix_serial::operator=(const matrix_serial &temp){
        int row_stride = temp.row_size;
        int size = (row_stride * row_stride);
        for(int i = 0; i < row_stride; i++)
        {
            for(int j = 0; j < row_stride; j++)
                ptr[(i * row_stride) + j] = temp.ptr[(i * row_stride) + j];
        }
        return *this;
    }
    matrix_serial matrix_serial::operator-(int num){
        matrix_serial temp(row_size);
        int size = (row_size * row_size);
        for (int i = 0; i < size; i++)
            temp.ptr[i] -= num;
        return temp;
    }
    void matrix_serial::transpose(matrix_serial &output){
        int size = row_size;
        for(int i = 0; i < size; i++)
        {
            for(int j = 0; j < size; j++)
                output.ptr[(j * size) + i] = ptr[(i * size) + j];
        }
        return;
    }
