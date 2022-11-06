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
! Fortran part of a VB-Fortran DLL example. This
! routine DLL_ROUT is called from a VB.NET executable
! program.
SUBROUTINE DLL_ROUT (DBL_IN, STRING_IN, DBL_OUT)
IMPLICIT NONE
! Specify that DLL_ROUT is exported to a DLL
! and that the external name is 'DLL_ROUT'
!DEC$ ATTRIBUTES DLLEXPORT, STDCALL :: DLL_ROUT
!DEC$ ATTRIBUTES ALIAS:'DLL_ROUT' :: DLL_ROUT

REAL(8), INTENT(IN) :: DBL_IN(0:3)
CHARACTER(10), INTENT(IN) :: STRING_IN
!DEC$ ATTRIBUTES REFERENCE :: STRING_IN
! When VB passes in a "ByVal String", it passes the address
! of a NUL-terminated string, similar to what C would do,
! and no separate length.  The REFERENCE attribute tells
! Fortran not to expect a length.  In order to use a function
! such as INDEX, we need to supply some maximum length to Fortran
! which should be at least as long as the longest expected string.
REAL(8), INTENT(OUT) :: DBL_OUT(4)

REAL(8) STRVAL
INTEGER IOS, STRLEN

STRLEN = INDEX(STRING_IN, CHAR(0)) - 1

! Convert STRING_IN to a double.  If we get an error, we'll
! supply 1.0 as a default

READ (STRING_IN(1:STRLEN), *, IOSTAT=IOS) STRVAL
IF (IOS /= 0) STRVAL = 1.0
DBL_OUT = STRVAL * DBL_IN
RETURN
END 