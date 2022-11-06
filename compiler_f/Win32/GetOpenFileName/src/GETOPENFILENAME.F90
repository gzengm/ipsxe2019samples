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
!
program fileopen
use comdlg32
implicit none

! Declare structure used to pass and receive attributes
!
type(T_OPENFILENAME) ofn

! Declare filter specification.  This is a concatenation of
! pairs of null-terminated strings.  The first string in each pair
! is the file type name, the second is a semicolon-separated list
! of file types for the given name.  The list ends with a trailing
! null-terminated empty string.
!
character(*),parameter :: filter_spec = &
  "Text Files"C//"*.txt"C// &
  "Fortran Files"C//"*.f90;*.f"C//""C

! Declare string variable to return the file specification.
! Initialize with an initial filespec, if any - null string
! otherwise
!
character(512) :: file_spec = ""C
integer status,ilen
ofn%lStructSize = SIZEOF(ofn)
ofn%hwndOwner = NULL
ofn%hInstance = NULL  ! For Win32 applications, you
                      ! can set this to the appropriate
                      ! hInstance
                      !
ofn%lpstrFilter = loc(filter_spec)
ofn%lpstrCustomFilter = NULL
ofn%nMaxCustFilter = 0
ofn%nFilterIndex = 1 ! Specifies initial filter value
ofn%lpstrFile = loc(file_spec)
ofn%nMaxFile = sizeof(file_spec)
ofn%nMaxFileTitle = 0
ofn%lpstrInitialDir = NULL  ! Use Windows default directory
ofn%lpstrTitle = loc(""C)
ofn%Flags = OFN_PATHMUSTEXIST
ofn%lpstrDefExt = loc("txt"C)
ofn%lpfnHook = NULL
ofn%lpTemplateName = NULL

! Call GetOpenFileName and check status
! 
status = GetOpenFileName(ofn)
if (status .eq. 0) then
  type *,'No file name specified'
else
  ! Get length of file_spec by looking for trailing NUL
  ilen = INDEX(file_spec,CHAR(0))
  type *,'Filespec is ',file_spec(1:ilen-1)
  ! Example of how to see if user said "Read Only"
  !  
  if (IAND(ofn%flags,OFN_READONLY) /= 0) &
    type *,'Readonly was requested'
end if
end program fileopen

