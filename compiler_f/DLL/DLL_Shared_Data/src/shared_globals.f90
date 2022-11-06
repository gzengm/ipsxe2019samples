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
!#      DLL_Shared_Data sample program for Intel Visual Fortran
!#******************************************************************************

! This module contains the data that is shared between the parent and child process. It is built
! into a DLL that is then linked into the application.
!
! In order for the data to be read-write shared, the following must be done:
!
! 1. The variables must be initialized to a non-zero value so that they are placed
!    in the .data image section
!
! 2. The linker command line options are modified to add: /section:.data,RWS
!    This tells the linker to make the .data section of the DLL read-write shared.  If this
!    isn't done, then each process will have its own copy of the data.
!
module shared_globals
implicit none

real :: shared_variable = 999.0  ! Must initialize to non-zero value
                                 ! in order to be placed in .data section
!dec$ attributes dllexport :: shared_variable
logical :: not_dead_yet = .TRUE. ! Used to detect that the other process is alive
!dec$ attributes dllexport :: not_dead_yet

end module shared_globals 