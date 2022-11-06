ReadMe for C_calls_Fortran example
Part of the Intel(R) Parallel Studio XE product

This solution demonstrates a mixed-language application in
which a C (or C++) main program calls a Fortran subroutine.
It is presented in two different styles, one where the 
Fortran code is built as a static library (FLIB/C_USELIB) and
one where the Fortran code is built as a DLL (FDLL/C_USEDLL).


Pre-Build Instructions
======================
 
This project requires that you have Microsoft Visual C++ installed.  It will not work if you 
are using the included Microsoft Visual Studio Shell.
 
You may need to do a one-time configuration of Visual C++ to properly link in Fortran libraries.
For details, please see:
 
http://software.intel.com/en-us/articles/configuring-visual-studio-for-mixed-language-applications


Build and Run Instructions
==========================

To build the solution, right click on the Solution in the Solution Explorer pane and
select Build Solution.

To run the static library version, right click on project USELIB
and select "Set as Startup Project". Then select Debug > Start without debugging.

To run the DLL version, right click on project USEDLL and select 
"Set as Startup Project".  Then select Debug > Start without debugging.

 
Detailed Explanation
====================

The C main program assigns the string "Testing..." to a
string variable and 123 to an integer variable.  These are
then passed to a Fortran subroutine along with a string
output argument.  The Fortran routine converts the integer
to a decimal string, concatenates it with the input string,
and stores the result in the output string argument.  Upon
return, the C main program displays the output string. 
This example also demonstrates how to pass character/string
arguments to Fortran using the Fortran 2015 C Interoperability
features.

Project FLIB builds the Fortran source as a static library.
By default, when a Fortran static library project is created,
the option "Libraries, Disable default library search rules"
is set to "Yes", so that the library can be used from a variety
of Fortran project types.  When used from non-Fortran code,
though, this setting prevents the Fortran run-time libraries 
from being linked in.  The solution to this is to either set
to "No", the choice made here, or to explicitly add the required
libraries to the non-Fortran project.  Note that it is also
required that you add the path to the Intel Fortran LIB folder
as described above.

Project USELIB contains the C++ main program and is dependent
on the FLIB project.

Project FDLL builds the Fortran source as a DLL.Note the use of 
the predefined preprocessor symbol _DLL in the Fortran code to 
conditionally cause the declaration of FSUB to be exported.

Project USEDLL contains the C++ main program and has a
dependency on the FDLL subproject.  The non-default 
settings for this project are to add a USEDLL preprocessor
symbol to C/C++..Preprocessor..Preprocessor Definitions
(this is used to include the __declspec(dllimport)
directive), and C/C++..Code Generation..Use run-time
library..Multithreaded DLL.  Whenever building a main
program that links against a Fortran or C DLL, you should
make sure that the main program also links against the
DLL forms of the run-time libraries.

The C++ projects explicitly name the corresponding Fortran project's
output library file as an input to the linker.

The C_USEDLL project also contains a Post-Build step to copy the DLL
created by FDLL into the folder containing the executable.


Copyright (C) 2016 Intel Corporation.  All rights reserved.