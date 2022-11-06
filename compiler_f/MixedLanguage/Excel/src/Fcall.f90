!==============================================================
!
! SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE 
! LICENSE AGREEMENT,
! http://software.intel.com/en-us/articles/intel-sample-source-
! code-license-agreement/
!
! Copyright 2016 Intel Corporation
!
! THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR 
! IMPLIED, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTY OF 
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NON-
! INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
!
! =============================================================
!
! Example of Excel calling Intel Visual Fortran
! Excel code passes an integer and a character string to FortranCall
! The integer is doubled, converted to a string and returned in the second argument
! Excel calls always use the STDCALL mechanism
!
subroutine FortranCall (r1, num)
!DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:"FortranCall" :: FortranCall
integer, intent(in) :: r1
character(10), intent(out) :: num
!DEC$ ATTRIBUTES REFERENCE :: num

num = ''
write (num,'(i0)') r1 * 2

return
end subroutine FortranCall