! ==============================================================
!
! SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
! http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!
! Copyright 2017 Intel Corporation
!
! THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
! NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
!
! =============================================================
!
! Part of the vec_samples tutorial. For information, please read
! Tutorial: Auto-vectorization in the Getting Started Tutorials document
!

subroutine matvec(size1,size2,a,b,c)
  implicit none
  integer,                      intent(in)  :: size1,size2
  real, dimension(size1,size2), intent(in)  :: a
  real, dimension(size2),       intent(in)  :: b
  real, dimension(size1),       intent(out) :: c
  integer                                   :: i,j,k

  c=0.
  do j=1,size2

!DIR$ IF DEFINED(ALIGNED)
!DIR$ vector aligned
!DIR$ END IF
     do i=1,size1
        c(i) = c(i) + a(i,j) * b(j)
     enddo
  enddo

end subroutine matvec