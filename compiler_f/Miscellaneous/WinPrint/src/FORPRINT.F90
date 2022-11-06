!==============================================================
!
! SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
! http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!
! Copyright 2016 Intel Corporation
!
! THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
! NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
!
! =============================================================
!
! Example Forprint
!
! Demonstrates use of the Fortran_WinPrint module for printing
! text files on a Windows printer
!

program forprint
use Fortran_WinPrint
integer istat
character*3 file_path

#ifdef MSVS_BLD    
    file_path="..\"
#else
    file_path=".\"
#endif

open (unit=1,file=TRIM(file_path)//'src\forprint.f90',form='formatted',status='old',action='read')
istat =  Print_Unit (1, Default_Printer=.FALSE.,Font="Verdana")
close (1)
end