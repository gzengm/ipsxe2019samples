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

#include <cstdio>
#include <cstdlib>
#include <cmath>
#include "DCT.h"
#include "matrix.h"
#include "timer.h"
#include "tbb/tbb.h"

using namespace tbb;

// Rename ALIGN to DCTALIGN because ALIGN is pre-defined on macOS SDK
#ifdef _WIN32
#include <intrin.h>
#define DCTALIGN __declspec(align(ALIGNMENT))
#else
#define DCTALIGN __attribute__((aligned(ALIGNMENT)))
#endif

//API for creating 8x8 DCT matrix
void create_DCT_SIMD(matrix_SIMD &x)
{
    int size = x.row_size;
    int temp[8];

    for(int i = 0; i < size; i++)
    temp[i] = i;

    for(int i = 0; i < size; i++)
    {
        for(int j = 0; j < size; j++)
        {
            if (i == 0)
                x.ptr[(i * size) + j] = (1/sqrt((float)size));
            else
                x.ptr[(i * size) + j] = sqrt((float)2/size) * cosf(((((float)2*temp[j]) + 1)*i*3.14f)/(2*size));
        }
    }
    return;
}

void create_DCT_serial(matrix_serial &x)
{
    int size = x.row_size;
    int temp[8];
    for(int i = 0; i < size; i++)
        temp[i] = i;
    for(int i = 0; i < size; i++)
    {
        for(int j = 0; j < size; j++)
        {
            if(i == 0)
                x.ptr[(i * size) + j] = (1/sqrt((float)size));
            else
                x.ptr[(i * size) + j] = sqrt((float)2/size) * cosf(((((float)2*temp[j]) + 1)*i*3.14f)/(2*size));
        }
    }
    return;
}

void process_image_SIMD(rgb *indataset, rgb *outdataset, int startindex) {
    int size = 8;
    int size_of_array = size * size;
    matrix_SIMD dct(size), dctinv(size), interim(size), interim1(size), product(size), redinput(size), blueinput(size), greeninput(size), quant(size);

    //Quantization matrix which does 50%, 90% and 10% quantization
    float quant50[64] = {16.f, 11.f, 10.f, 16.f, 24.f, 40.f, 51.f, 61.f, 12.f, 12.f, 14.f, 19.f, 26.f, 58.f, 60.f, 55.f, 14.f, 13.f, 16.f, 24.f, 40.f, 57.f, 69.f, 56.f, 14.f, 17.f, 22.f, 29.f, 51.f, 87.f, 80.f, 62.f, 18.f, 22.f, 37.f, 56.f, 68.f, 109.f, 103.f, 77.f, 24.f, 35.f, 55.f, 64.f, 81.f, 104.f, 113.f, 92.f, 49.f, 64.f, 78.f, 87.f, 103.f, 121.f, 120.f, 101.f, 72.f, 92.f, 95.f, 98.f, 112.f, 100.f, 103.f, 99.f };
    float quant90[64] = {3, 2, 2, 3, 5, 8, 10, 12, 2, 2, 3, 4, 5, 12, 12, 11, 3, 3, 3, 5, 8, 11, 14, 11, 3, 3, 4, 6, 10, 17, 16, 12, 4, 4, 7, 11, 14, 22, 21, 15, 5, 7, 11, 13, 16, 12, 23, 18, 10, 13, 16, 17, 21, 24, 24, 21, 14, 18, 19, 20, 22, 20, 20, 20 };
    float quant10[64] = {80, 60, 50, 80, 120, 200, 255, 255, 55, 60, 70, 95, 130, 255, 255, 255, 70, 65, 80, 120, 200, 255, 255, 255, 70, 85, 110, 145, 255, 255, 255, 255, 90, 110, 185, 255, 255, 255, 255, 255, 120, 175, 255, 255, 255, 255, 255, 255, 245, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 };

    //memcpy(quant.ptr, quant50, sizeof(float)*64);
    memcpy(quant.ptr, quant90, sizeof(float) * 64);

    //Creation of 8x8 DCT matrix
    create_DCT_SIMD(dct);

    //Creating a transpose of DCT matrix
    dct.transpose(dctinv);

    //Translating the pixels values from 0 - 255 range to -128 to 127 range
#pragma omp simd
    for (int i = 0; i < 64; i++) 
    {
        redinput.ptr[i] = indataset[startindex + i].red;
        redinput.ptr[i] -= 128;
    }

    //Computation of the discrete cosine transform of the image section of size 8x8 for red values
    interim = dct * redinput *dctinv;

    //Computation of quantization phase using the quantization matrix
#pragma omp simd
    for (int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i] / quant.ptr[i]) + 0.5f);

    //Computation of dequantizing phase using the same above quantization matrix
#pragma omp simd
    for (int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i] * quant.ptr[i]) + 0.5f);

    //Computation of Inverse Discrete Cosine Transform (IDCT)
    product = dctinv * interim * dct;

#pragma omp simd
    for (int i = 0; i < 64; i++)
    {
        float temp = (product.ptr[i] + 128);
        outdataset[startindex + i].red = (temp > 255.f) ? 255 : (unsigned char)temp;
    }

    ///////////////////////////
    //Translating the pixels values from 0 - 255 range to -128 to 127 range
#pragma omp simd
    for (int i = 0; i < 64; i++)
    {
        blueinput.ptr[i] = indataset[startindex + i].blue;
        blueinput.ptr[i] -= 128;
    }

    //Computation of the discrete cosine transform of the image section of size 8x8 for blue values
    interim = dct * blueinput *dctinv;

    //Computation of quantization phase using the quantization matrix
#pragma omp simd
    for (int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i] / quant.ptr[i]) + 0.5f);

    //Computation of dequantizing phase using the same above quantization matrix
#pragma omp simd
    for (int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i] * quant.ptr[i]) + 0.5f);

    //Computation of Inverse Discrete Cosine Transform (IDCT)
    product = dctinv * interim * dct;
#pragma omp simd
    for (int i = 0; i < 64; i++)
    {
        float temp = product.ptr[i] + 128;
        outdataset[startindex + i].blue = (temp > 255.f) ? 255 : (unsigned char)temp;
    }

    ////////////////////////////
    //Translating the pixels values from 0 - 255 range to -128 to 127 range
#pragma omp simd
    for (int i = 0; i < 64; i++)
    {
        greeninput.ptr[i] = indataset[startindex + i].green;
        greeninput.ptr[i] -= 128;
    }

    //Computation of the discrete cosine transform of the image section of size 8x8 for green values
    interim = dct * greeninput *dctinv;

    //Computation of quantization phase using the quantization matrix
#pragma omp simd
    for (int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i] / quant.ptr[i]) + 0.5f);

    //Computation of dequantizing phase using the same above quantization matrix
#pragma omp simd
    for (int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i] * quant.ptr[i]) + 0.5f);

    //Computation of Inverse Discrete Cosine Transform (IDCT)
    product = dctinv * interim * dct;
#pragma omp simd
    for (int i = 0; i < 64; i++)
    {
        float temp = product.ptr[i] + 128;
        outdataset[startindex + i].green = (temp > 255.f) ? 255 : (unsigned char)temp;
    }
    return;
}

void process_image_serial(rgb *indataset, rgb *outdataset, int startindex)
{
    int size = 8;
    int size_of_array = size * size;
    matrix_serial dct(size), dctinv(size), interim(size), interim1(size), product(size), redinput(size), blueinput(size), greeninput(size), quant(size);

    //Quantization matrix which does 50%, 90% and 10% quantization
    float quant50[64] = {16.f, 11.f, 10.f, 16.f, 24.f, 40.f, 51.f, 61.f, 12.f, 12.f, 14.f, 19.f, 26.f, 58.f, 60.f, 55.f, 14.f, 13.f, 16.f, 24.f, 40.f, 57.f, 69.f, 56.f, 14.f, 17.f, 22.f, 29.f, 51.f, 87.f, 80.f, 62.f, 18.f, 22.f, 37.f, 56.f, 68.f, 109.f, 103.f, 77.f, 24.f, 35.f, 55.f, 64.f, 81.f, 104.f, 113.f, 92.f, 49.f, 64.f, 78.f, 87.f, 103.f, 121.f, 120.f, 101.f, 72.f, 92.f, 95.f, 98.f, 112.f, 100.f, 103.f, 99.f};
    float quant90[64] = {3, 2, 2, 3, 5, 8, 10, 12, 2, 2, 3, 4, 5, 12, 12, 11, 3, 3, 3, 5, 8, 11, 14, 11, 3, 3, 4, 6, 10, 17, 16, 12, 4, 4, 7, 11, 14, 22, 21, 15, 5, 7, 11, 13, 16, 12, 23, 18, 10, 13, 16, 17, 21, 24, 24, 21, 14, 18, 19, 20, 22, 20, 20, 20};
    float quant10[64] = {80, 60, 50, 80, 120, 200, 255, 255, 55, 60, 70, 95, 130, 255, 255, 255, 70, 65, 80, 120, 200, 255, 255, 255, 70, 85, 110, 145, 255, 255, 255, 255, 90, 110, 185, 255, 255, 255, 255, 255, 120, 175, 255, 255, 255, 255, 255, 255, 245, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255};

    //memcpy(quant.ptr, quant50, sizeof(float)*64);
    memcpy(quant.ptr, quant90, sizeof(float)*64);

    //Creation of 8x8 DCT matrix
    create_DCT_serial(dct);

    //Creating a transpose of DCT matrix
    dct.transpose(dctinv);

    //Translating the pixels values from 0 - 255 range to -128 to 127 range
    for(int i = 0; i < 64; i++)
    {
        redinput.ptr[i] = indataset[startindex+i].red;
        redinput.ptr[i] -= 128;
    }

    //Computation of the discrete cosine transform of the image section of size 8x8 for red values
    interim = dct * redinput *dctinv;

    //Computation of quantization phase using the quantization matrix
    for(int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i]/quant.ptr[i]) + 0.5f);

    //Computation of dequantizing phase using the same above quantization matrix
    for(int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i]*quant.ptr[i]) + 0.5f);

    //Computation of Inverse Discrete Cosine Transform (IDCT)
    product = dctinv * interim * dct;
    for(int i = 0; i < 64; i++)
    {
        float temp = (product.ptr[i] + 128);
        outdataset[startindex+i].red = (temp > 255.f)?255:(unsigned char)temp;
    }

    ///////////////////////////
    //Translating the pixels values from 0 - 255 range to -128 to 127 range
    for(int i = 0; i < 64; i++)
    {
        blueinput.ptr[i] = indataset[startindex+i].blue;
        blueinput.ptr[i] -= 128;
    }

    //Computation of the discrete cosine transform of the image section of size 8x8 for blue values
    interim = dct * blueinput *dctinv;

    //Computation of quantization phase using the quantization matrix
    for(int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i]/quant.ptr[i]) + 0.5f);

    //Computation of dequantizing phase using the same above quantization matrix
    for(int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i]*quant.ptr[i]) + 0.5f);

    //Computation of Inverse Discrete Cosine Transform (IDCT)
    product = dctinv * interim * dct;
    for(int i = 0; i < 64; i++)
    {
        float temp = product.ptr[i] + 128;
        outdataset[startindex+i].blue = (temp > 255.f)?255:(unsigned char)temp;
    }

    ////////////////////////////
    //Translating the pixels values from 0 - 255 range to -128 to 127 range
    for(int i = 0; i < 64; i++)
    {
        greeninput.ptr[i] = indataset[startindex+i].green;
        greeninput.ptr[i] -= 128;
    }

    //Computation of the discrete cosine transform of the image section of size 8x8 for green values
    interim = dct * greeninput *dctinv;

    //Computation of quantization phase using the quantization matrix
    for(int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i]/quant.ptr[i]) + 0.5f);

    //Computation of dequantizing phase using the same above quantization matrix
    for(int i = 0; i < 64; i++)
        interim.ptr[i] = floor((interim.ptr[i]*quant.ptr[i]) + 0.5f);

    //Computation of Inverse Discrete Cosine Transform (IDCT)
    product = dctinv * interim * dct;
    for(int i = 0; i < 64; i++)
    {
        float temp = product.ptr[i] + 128;
        outdataset[startindex+i].green = (temp > 255.f)?255:(unsigned char)temp;
    }
    return;
}


//This API does the reading and writing from/to the .bmp file. Also invokes the image processing API from here
int read_process_write(char* input, char *output, int choice)
{
    FILE *fp,*out;
    bitmap_header* hp;
    size_t n;
    CUtilTimer t;
    #ifdef PERF_NUM
    vector<double> avg_timersecs(4);
    vector<long long> avg_ticks(4);
    #endif

// initialization for Intel TBB 
    int num, nthreads = tbb::task_scheduler_init::automatic;
    char *nthreads_str = getenv("TBB_NUM_THREADS");
    if (nthreads_str && (sscanf(nthreads_str, "%d", &num) > 0) && (num > 0)) nthreads = num;
    tbb::task_scheduler_init init(nthreads);

    // Making sure the AOS alignes to an address which is multiple of 16 to support vectorization 
    DCTALIGN rgb *indata, *outdata;

    // Instantiating a file handle to open a input BMP file in binary mode
    fp = fopen(input, "rb");
    if(fp==NULL){
        cout<<"The input file could not be opened. Program will be exiting\n";
        return 0;
    }

    // Allocating memory for storing the bitmap header information which will be retrived from input image file
    hp=(bitmap_header*)malloc(sizeof(bitmap_header));
    if(hp==NULL)
    {
        cout<<"Unable to allocate the memory for bitmap header\n";
        return 0;
    }

    // Reading from input file the bitmap header information which is inturn stored in memory allocated in the previous step
    n=fread(hp, sizeof(bitmap_header), 1, fp);
    if(n<1){
        cout<<"Read error from the file. No bytes were read from the file. Program exiting \n";
        return 0;
    }

    if(hp->bitsperpixel != 24){
        cout<<"This is not a RGB image\n";
        return 0;
    }

    //Size of the image in terms of number of pixels
    int size_of_image = hp->width * hp->height;

    // Allocate memory for loading the bitmap data of the input image
    indata = (rgb *)_mm_malloc((sizeof(rgb)*(size_of_image)), ALIGNMENT);
    if (indata==NULL){
        cout<<"Unable to allocate the memory for bitmap date\n";
        return 0;
    }

    // Setting the File descriptor to the starting point in the input file where the bitmap data(payload) starts
    fseek(fp,sizeof(char)*hp->fileheader.dataoffset,SEEK_SET);

    // Reading the bitmap data from the input bmp file to the memory allocated in the previous step
    n=fread(indata, sizeof(rgb), (size_of_image), fp);
    if(n<1){
        cout<<"Read error from the file. No bytes were read from the file. Program exiting \n";
        return 0;
    }

    // Allocate memory for storing the bitmap data of the processed image
    outdata = (rgb *)_mm_malloc((sizeof(rgb)*(size_of_image)), ALIGNMENT);
    if(outdata==NULL){
        cout<<"Unable to allocate the memory for bitmap date\n";
        return 0;
    }

    // Invoking the DCT/Quantization API which does some manipulation on the bitmap data read from the input .bmp file
    switch(choice){
        case 1: t.start();
            for(int i = 0; i < (size_of_image)/64; i++)
            {
                int startindex = (i * 64);
                process_image_serial(indata, outdata, startindex);
            }
            t.stop();
            break;
        case 2: t.start();
            for(int i = 0; i < (size_of_image)/64; i++)
            {
                int startindex = (i * 64);
                process_image_SIMD(indata, outdata, startindex);
            }
            t.stop();
            break;
        case 3: t.start();
            tbb::parallel_for(int(0), (size_of_image)/64, [&](int i)
            {
                int startindex = (i * 64);
                process_image_serial(indata, outdata, startindex);
            });
            t.stop();
            break;
        case 4: t.start();
            tbb::parallel_for(int(0), (size_of_image)/64, [&](int i)
            {
                int startindex = (i * 64);
                process_image_SIMD(indata, outdata, startindex);
            });
            t.stop();
            break;
        case 0:
#ifdef PERF_NUM
        cout<<"Run all tests...\n\n";
        for (int j = 0; j < 5; j++)
        {
#endif
            cout<<"Start scalar version without parallelism...\n";
            t.start();
            for(int i = 0; i < (size_of_image)/64; i++)
            {
                int startindex = (i * 64);
                process_image_serial(indata, outdata, startindex);
            }
            t.stop();
#ifdef PERF_NUM
            avg_ticks[0] += t.get_ticks();
            avg_timersecs[0] += t.get_time();
#endif
            cout<<"The time taken in number of ticks is "<<t.get_ticks()<<"\n";
            cout<<"The time taken is "<<t.get_time()<<" seconds\n";
            cout<<"Start SIMD version without parallelism...\n";
            t.start();
            for(int i = 0; i < (size_of_image)/64; i++)
            {
                int startindex = (i * 64);
                process_image_SIMD(indata, outdata, startindex);
            }
            t.stop();
#ifdef PERF_NUM
            avg_ticks[1] += t.get_ticks();
            avg_timersecs[1] += t.get_time();
#endif
            cout<<"The time taken in number of ticks is "<<t.get_ticks()<<"\n";
            cout<<"The time taken is "<<t.get_time()<<" seconds\n";
            cout<<"Start scalar version with TBB parallelism...\n";
            t.start();
            tbb::parallel_for(int(0), (size_of_image)/64, [&](int i)
            {
                int startindex = (i * 64);
                process_image_serial(indata, outdata, startindex);
            });
            t.stop();
#ifdef PERF_NUM
            avg_ticks[2] += t.get_ticks();
            avg_timersecs[2] += t.get_time();
#endif
            cout<<"The time taken in number of ticks is "<<t.get_ticks()<<"\n";
            cout<<"The time taken is "<<t.get_time()<<" seconds\n";
            cout<<"Start SIMD version with TBB parallelism...\n";
            t.start();
            tbb::parallel_for(int(0), (size_of_image)/64, [&](int i)
            {
                int startindex = (i * 64);
                process_image_SIMD(indata, outdata, startindex);
            });
            t.stop();
            cout<<"The time taken in number of ticks is "<<t.get_ticks()<<"\n";
            cout<<"The time taken is "<<t.get_time()<<" seconds\n";
#ifdef PERF_NUM
            avg_ticks[3] += t.get_ticks();
            avg_timersecs[3] += t.get_time();
        }
#endif
            break;
        default: cout<<"Wrong choice\n";
            free(hp);
            _mm_free(indata);
            _mm_free(outdata);
            return 0;
    }
    // Opening an output file to which the processed result will be written
    out = fopen(output, "wb");
    if(out==NULL){
        cout<<"The file could not be opened. Program will be exiting\n";
        return 0;
    }

    // Writing the bitmap header which we copied from the input file to the output file. We need not make any changes because we haven't made any change to the image size or compression type.
    n=fwrite(hp,sizeof(char),sizeof(bitmap_header),out);
    if(n<1){
        cout<<"Write error to the file. No bytes were wrtten to the file. Program exiting \n";
        return 0;
    }

    // Setting the file descriptor to point to the location where the bitmap data is to be written
    fseek(out,sizeof(char)*hp->fileheader.dataoffset,SEEK_SET);

    // Writing the bitmap data of the processed image to the output file
    n=fwrite(outdata,sizeof(rgb),(size_of_image),out);
    if(n<1){
        cout<<"Write error to the file. No bytes were wrtten to the file. Program exiting \n";
        return 0;
    }

#ifdef PERF_NUM
    cout<<"\nAverage time for serial version:\n";
    cout<<"The time taken in number of ticks is "<<avg_ticks[0]/5<<"\n";
    cout<<"The time taken is "<<avg_timersecs[0]/5.0d<<" seconds\n";
    cout<<"Average time for SIMD version:\n";
    cout<<"The time taken in number of ticks is "<<avg_ticks[1]/5<<"\n";
    cout<<"The time taken is "<<avg_timersecs[1]/5.0d<<" seconds\n";
    cout<<"Average time for scalar version with TBB:\n";
    cout<<"The time taken in number of ticks is "<<avg_ticks[2]/5<<"\n";
    cout<<"The time taken is "<<avg_timersecs[2]/5.0d<<" seconds\n";
    cout<<"Average time for SIMD version with TBB:\n";
    cout<<"The time taken in number of ticks is "<<avg_ticks[3]/5<<"\n";
    cout<<"The time taken is "<<avg_timersecs[3]/5.0d<<" seconds\n";
#else
    if (choice) {
        cout<<"The time taken in number of ticks is "<<t.get_ticks()<<"\n";
        cout<<"The time taken is "<<t.get_time()<<" seconds\n";
    }
#endif
    // Closing all file handles and also freeing all the dynamically allocated memory
    fclose(fp);
    fclose(out);
    free(hp);
    _mm_free(indata);
    _mm_free(outdata);
    return 0;
}

int main(int argc, char *argv[])
{
    if(argc < 3){
        cout<<"Program usage is <modified_program> <inputfile.bmp> <outputfile.bmp>\n";
        return 0;
    }

    int choice;
// If PERF_NUM is defined, then no options taken...run all tests
#ifndef PERF_NUM
    cout<<"Please enter the version you want to execute:\n";
    cout<<"0) All versions\n";
    cout<<"1) Scalar version without parallelism\n";
    cout<<"2) OpenMP SIMD without parallelism\n";
    cout<<"3) Scalar version with TBB parallelism\n";
    cout<<"4) OpenMP SIMD with TBB parallelism\n";
    cin>>choice;
#else
    choice = 0;
#endif

    read_process_write(argv[1], argv[2], choice);
#ifdef _WIN32
    system("PAUSE");
#endif

    return 0;
}
