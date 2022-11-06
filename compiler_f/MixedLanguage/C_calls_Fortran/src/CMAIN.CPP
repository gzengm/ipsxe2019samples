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
/* 
   Main program written in C++ that calls a Fortran routine.
   Requires Intel Fortran Compiler 16.0 or later
 */
#include <stdio.h>
#include <string.h>

/*
   Include the Fortran C Interoperability declarations
   See the README.TXT for how to configure C++ to
   reference Fortran include files and libraries.
   Any compiler warnings about zero-sized arrays being
   non-standard can be ignored.
*/
#include "ISO_Fortran_binding.h"

/* 
   Declare the Fortran routine. 

   The "C" attribute prevents C++ name mangling  Remove it
   if the file type is .c
*/

#ifdef __cplusplus
extern "C" 
#endif

/* 
   This source file can be used with the Fortran routine
   being in a DLL or in a library.  The 
   USEDLL project defines a USEDLL preprocessor symbol to
   indicate that a DLL is being used; the USELIB project 
   omits this.  If we're using the Fortran DLL, add the
   dllimport specification.
*/

#ifdef USEDLL
__declspec(dllimport)
#endif

/* 
   The Fortran subroutine uses the Fortran 2015 standard
   C interoperability features. The BIND(C) in the Fortran
   code specifies that it is an "interoperable" procedure
   and the external name is downcased by default.

   The two CHARACTER(*) dummy arguments must be passed by
   "C descriptor". The character lengths are part of the
   descriptor.
*/

void fsub 
                     (int INT_ARG,
                      CFI_cdesc_t *STR_IN,
                      CFI_cdesc_t *STR_OUT
                     );

int main (int argc, char *argv[])
{
char instring[40];
char outstring[40];
int intarg;
CFI_CDESC_T(0) instring_desc, outstring_desc; // Ignore warning for zero-sized array
int status;

strcpy_s(instring,"Testing...");
intarg = 123;

/* 
   Initialize descriptors. We'll set the length of the input string to the NUL-terminated length,
   which will be picked up by Fortran.
*/

status = CFI_establish(
    (CFI_cdesc_t *) &instring_desc, // Descriptor
    &instring, // Base address
    CFI_attribute_other, // Not allocatable or pointer
    CFI_type_char, // Character type
    strlen(instring), // Element size
    0, // Number of dimensions (0 = scalar)
    NULL // Extents (not used here)
);
status = CFI_establish((CFI_cdesc_t *) &outstring_desc, &outstring, CFI_attribute_other, CFI_type_char, sizeof(outstring), 0, NULL);

/* Call Fortran routine, passing descriptors. Note that intarg is passed by value */

fsub(intarg,(CFI_cdesc_t *)&instring_desc,(CFI_cdesc_t *)&outstring_desc);

printf("%s\n",outstring); // Should print "Testing...0123"
return(0);
} 
