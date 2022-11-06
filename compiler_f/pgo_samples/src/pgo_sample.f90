 !==============================================================
 !
 ! SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
 ! http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
 !
 ! Copyright 2016-2017 Intel Corporation
 !
 ! THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
 ! NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 ! PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
 !
 ! =============================================================
 ! 
 ! [DESCRIPTION] 
 ! Demonstrates how to use Profile-guided Optimization (PGO) 
 ! to optimize performance. 
 ! 
 ! [COMPILE]
 ! See the Intel(R) Fortran Compiler Documentation for information 
 ! on using the PGO tools and using PGO to compile applications.
 ! Use the following general options, in the order shown,
 ! to profile an application:
 ! 
 ! Windows*:             /Qprof-gen
 !                       [run program]
 !                       /Qprof-use   
 ! 
 ! Linux* and macOS*:     -prof-gen
 !                       [run program]
 !                       -prof-use   
 !
 
 module addermod 
   implicit none 
   contains 
   subroutine mod_add(i1, i2, i4, i8, res) 
     integer(1), intent(in) :: i1 
     integer(2), intent(in) :: i2 
     integer(4), intent(in) :: i4 
     integer(8), intent(in) :: i8 
     integer, intent(out) :: res 
     res = i1 + i2 + i4 + i8 
   end subroutine mod_add 
 end module addermod 
  
 subroutine delegate(non_existent, res)      
   implicit none 
   external non_existent 
   integer, intent(in) :: res 
   call non_existent(1_1, 2_2, 4_4, 8_8, res) 
 end subroutine delegate 
  
 program main 
   use addermod 
   implicit none 
   integer :: res 
   call delegate(mod_add, res)               
   if (res /= 15) print *, 'mod_add failed' 
   call delegate(main_add, res) 
   if (res /= 15) print *, 'main_add failed' 
   print *, "Done"
   contains 
   subroutine main_add(i1, i2, i4, i8, res) 
     integer(1), intent(in) :: i1 
     integer(2), intent(in) :: i2 
     integer(4), intent(in) :: i4 
     integer(8), intent(in) :: i8 
     integer, intent(out) :: res 
     res = i1 + i2 + i4 + i8 
   end subroutine main_add 
 end program main
