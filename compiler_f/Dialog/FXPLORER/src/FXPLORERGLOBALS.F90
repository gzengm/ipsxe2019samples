!==============================================================
!
! SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
! http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!
! Copyright 2007-2016 Intel Corporation
!
! THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
! NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
!
! =============================================================
    
!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module fxplorerGlobals
use ifwinty
use iflogm

implicit none

!  Parameters

integer, parameter, public :: SIZEOFAPPNAME = 100

!  Global data

integer(HANDLE)		ghInstance
integer(HANDLE)		ghModule
integer(HANDLE)		ghwndMain
integer(HANDLE)		ghMenu
integer(HANDLE)		ghFileSubMenu
integer(HANDLE)		ghNavigateSubMenu
integer(HANDLE)		ghHelpSubMenu
integer(HANDLE)     ghwndFocus

type (dialog) gdlg

end module
