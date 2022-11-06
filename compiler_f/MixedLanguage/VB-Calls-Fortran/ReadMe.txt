VB-Calls-Fortran sample
Part of Intel(R) Visual Fortran Composer XE

Description
===========

This sample demonstrates how to call a Fortran DLL from Visual BASIC.NET
and how to pass simple arguments.

Build Instructions
==================
 
This project requires that you have Microsoft Visual BASIC.NET installed.  It
will not work if you are using the included Microsoft Visual Studio
Shell.
 
If project "VBCallsFortran" is not shown in boldface type in the Solution Explorer,
right-click on project "VBCallsFortran" and select "Set as StartUp Project".

To enable debugging of Fortran code called from Visual Basic: right click on project "VBCallsFortran" and choose Properties.  
In the left column, select "Debug" tab and check the box labeled "Enable native code debugging", 
close the properties page to save the changes.

Build the solution and then run it. A "Do It" button will be displayed along
with four input values.  Click the button.  The VB code will call the Fortran
routine DLL_ROUT and pass it a double precision real input array, a string,
and a double-precision output array.  The Fortran code uses an internal READ
to convert the string to a number, multiplies it by the values in the input
array, and then stores the result in the output array.

To view the Visual BASIC code, open Form1.vb and double-click on the Do It
button.  

Copyright (C) 2019 Intel Corporation.  All Rights Reserved.