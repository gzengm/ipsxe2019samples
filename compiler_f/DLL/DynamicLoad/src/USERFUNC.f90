!#==============================================================================
!#
!#  SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
!#  http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!#
!#  Copyright 2007-2016 Intel Corporation
!#
!#  THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED,
!#  INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY,
!#  FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL
!#  PROPERTY RIGHTS.
!#
!#==============================================================================
!#
!#
!#******************************************************************************
!# Content:
!#      DynamicLoad sample program for Intel Visual Fortran
!#******************************************************************************

! Example user function called by the DynamicLoad program
!
! This must be a function taking one default integer argument
! and returning a default integer result.  The global name of the
! routine must be 'USERFUNC' and it must be DLLEXPORTed
!
! Note that the executable project copies this project's DLL into its output directory
! as a post-build step
!

function USERFUNC (arg)
implicit none

integer USERFUNC
! The following directive exports the routine and sets the global name
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"USERFUNC" :: USERFUNC
integer, intent(IN) :: arg

write (*,'(A,G0)') "In USERFUNC with argument ", arg
USERFUNC = 3 * arg
return
end function USERFUNC