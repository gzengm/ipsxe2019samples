VB.NET-Safearrays sample
Part of Intel(R) Visual Fortran 

Description
===========

This sample demonstrates how to pass an array of strings from Visual BASIC.NET
and manipulate it in Fortran using the SafeArray and BSTR routines in module
IFCOM.

Build Instructions
==================
 
This project requires that you have Microsoft Visual BASIC.NET installed.  It
will not work if you are using the included Microsoft Visual Studio
Shell.
 
If project "ARRAY" is not shown in boldface type in the Solution Explorer,
right-click on project "ARRAY" and select "Set as StartUp Project".

To enable debugging of Fortran code called from Visual Basic, right click
on project "ARRAY" and choose Properties.  
In the left column, select "Debug" tab and check the box labeled "Enable native code debugging", 
close the properties page to save the changes.

Build the solution and then run it. A form will be displayed containing a
"Push Me" button, an input values box and an output values box.  The input
value box has the string values "One", "Two", "Three", ... "Twelve". When the 
button is pushed, Visual BASIC will call the Fortran routine passing it an
array of the input strings.

The Fortran code has three parts:

1. The file test.txt is opened in the project folder and the bounds of the
   array are written to the file. This demonstrates the use of the SafeArray
   routines to access the bounds information.
   
2. The file testout.txt is opened in the project folder and the contents of the
   array are written to the file. This demonstrates retrieving elements from a
   SafeArray and converting the BSTR string to a Fortran string.
   
3. Each BSTR element of the array has the text " potato" appended to it so that
   the modified array is returned to Visual BASIC.  This demonstrates manipulating
   BSTR values.
   
The Fortran routine then returns and the Visual BASIC code writes the new array
values into the output box.

Note that the Fortran code makes no assumptions about the shape of the VB array.

To view the Visual BASIC code, open Array.vb and double-click on the Push Me
button.