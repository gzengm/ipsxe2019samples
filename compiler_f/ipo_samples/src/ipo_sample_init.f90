!==============================================================
!
! SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
! http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!
! Copyright 2007-2017 Intel Corporation
!
! THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
! NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
!
! =============================================================
!
! [DESCRIPTION]
! One of three source files, and one library file, used
! to demonstrate how to use Interprocedural Optimization (IPO)
! during compilation and linking.
!
!
! Please see readme.html in the sample folder for more information.

! Initialize array 'a' to REALs.

subroutine init(a, n)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a(n) 
  integer :: i
  do i = 1, n 
    a(i) = real(i) 
  end do 
end subroutine init


