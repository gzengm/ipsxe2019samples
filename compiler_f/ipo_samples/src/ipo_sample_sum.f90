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
! One of three source files, used
! to demonstrate how to use Interprocedural Optimization (IPO)
! during compilation and linking.
!
! Please see readme.html in the sample folder for more information.

! This function adds 3 to its argument.
function add3(x)
  implicit none
  real :: add3
  real, intent(in) :: x
  add3 = x + 3.
    end function add3

! This function returns a sum of 2x+3 where each
! x is an element of array 'a'.
function mysum(a, n)
  implicit none
  real :: mysum, add3
  integer, intent(in) :: n  ! Number of elements in a
  real, intent(in), dimension(n) :: a  ! Array of values to be summed
  integer :: i
  mysum = 0.
  do i = 1, n 
    mysum = mysum + a(i) + add3(a(i))
  end do 
end function mysum
