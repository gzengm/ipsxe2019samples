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
!  CopyFolder.f90 
!
!  FUNCTIONS:
!  CopyFolder      - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: CopyFolder
!
!  PURPOSE:  Driver program for Copy_Folder routine that demonstrates using
!            Win32 APIs to recursively copy a file folder
!
!****************************************************************************

    program CopyFolder

    use Copy_Folder_mod
    implicit none

    ! Variables
    integer ret
	character*8 file_path

    ! Body of CopyFolder

#ifdef MSVS_BLD    
    file_path="..\data\"
#else
    file_path=".\data\"
#endif

    ret = Copy_Folder (TRIM(file_path)//'sourcefiles',TRIM(file_path)//'destfiles')
    
    if (ret == 0) then
      write (*,*) "Copy succeeded"
    else
      write (*,*) "Copy failed with status ", ret
    end if

    end program CopyFolder