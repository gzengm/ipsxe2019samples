//==============================================================
//
// SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
// http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
//
// Copyright 2016 Intel Corporation
//
// THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
// NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
//
//=============================================================
/* C routine called by Fortran main program
**
** Converts integer input argument to text, appends
** the text to string input argument and stores the
** result in the string output argument
*/

#include <stdio.h>

extern "C" void c_routine (
                        int int_arg, // integer to convert
                        char* input_text, // text to prepend to converted integer
                        char* output_text, // output buffer
						int output_text_len // length of output buffer
                        )

{
    sprintf_s(output_text,output_text_len,"%s%i ",input_text,int_arg);
}
