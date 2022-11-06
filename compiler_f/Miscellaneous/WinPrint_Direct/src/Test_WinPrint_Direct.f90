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
! The test.prn file
!

program Test_WinPrint_Direct
use WinPrint_Direct
integer istat
character*8 file_path

#ifdef MSVS_BLD    
    file_path="..\data\"
#else
    file_path=".\data\"
#endif

open (unit=1,file=TRIM(file_path)//'test.prn',form='formatted',status='old',action='read')
istat =  Print_Direct (1, Default_Printer=.FALSE.)
close (1)
end