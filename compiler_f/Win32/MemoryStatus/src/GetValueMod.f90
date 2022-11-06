! ==============================================================
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

module GetValueMod

! Contains a routine gv that accepts a value and returns a string
! with a text representation of that value in storage units

private
public :: gv

! The fields of type T_MEMORYSTATUSEX may be of type integer(DWORDLONG)
! or type T_LARGE_INTEGERX, which consists of two integer(4) components.  
! To account for this, create a generic for our "get value" routine gv 
! to handle either type.

interface gv
  module procedure gv_dww
  module procedure gv_lix
end interface gv
contains

function gv_dww (val)
use ifwinty, only: DWORDLONG
character(30) gv_dww
integer(DWORDLONG), intent(in) :: val
integer(8), parameter :: DIV = 1024
integer(8) :: lval

lval = zext(val,8)
if (val == -1) then
  gv_dww = '(unrepresentable)'
else if (lval < 1024) then
  write (gv_dww, 101) lval, ' bytes'
101 format (I0,A)
else if (lval < (DIV*DIV)) then
  write (gv_dww,101) lval/DIV, 'KB'
else if (lval < (DIV*DIV*DIV)) then
  write (gv_dww,101) lval/(DIV*DIV), 'MB'
else 
  write (gv_dww,102) REAL(lval/(DIV*DIV))/REAL(DIV), 'GB'
102 format (F20.2,A)
  gv_dww = adjustl(gv_dww)
end if
end function gv_dww

function gv_lix (val)
use ifwinty, only: T_LARGE_INTEGERX, DWORDLONG
character(30) gv_lix
type(T_LARGE_INTEGERX), intent(IN) :: val
! Convert val to DWORDLONG and call gv_dww
gv_lix = gv_dww(transfer(val, 0_DWORDLONG))
end function gv_lix

end module GetValueMod