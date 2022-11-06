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

module anginc
use ifwinty
implicit none


! anginc.f90 - header file for the AngleArc() demonstration program.


!/* Top dialog item IDs */
integer, parameter, public :: DID_X      =  101
integer, parameter, public :: DID_Y      =  102
integer, parameter, public :: DID_RADIUS =  103
integer, parameter, public :: DID_START  =  104
integer, parameter, public :: DID_SWEEP  =  105
integer, parameter, public :: DID_DRAW   =  200

integer, parameter, public :: MAXCHARS   =  32

!/* Misc. defines for size, color, and appearance of drawing. */
integer, parameter, public :: GRIDCOLOR     =  Z'01000006'
integer, parameter, public :: TICKSPACE     =  20
integer, parameter, public :: DIALOGHEIGHT  =  60

integer(HANDLE)   hInst
integer(HANDLE)   hwnddlg
integer(HANDLE)   hWndMain

end module anginc






