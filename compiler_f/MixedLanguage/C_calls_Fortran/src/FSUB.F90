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
SUBROUTINE FSUB (INT_ARG, STR_IN, STR_OUT) BIND(C)

! BIND(C) specifies that this procedure is "interoperable with C".
! Since the optional NAME= specifier was not used, the external
! name is downcased to "fsub". Any platform-specific name
! decoration is automatically added to match the companion C
! processor.

USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

! If we're building a DLL, specify that DLL_ROUT is 
! exported to a DLL.  Key this off of the preprocessor
! symbol _DLL which is automatically defined if we're
! building a DLL.

!DEC$ IF DEFINED (_DLL)
!DEC$ ATTRIBUTES DLLEXPORT :: FSUB
!DEC$ END IF

INTEGER, VALUE, INTENT(IN) :: INT_ARG ! Passed by value
CHARACTER(*), INTENT(IN) :: STR_IN
CHARACTER(*), INTENT(OUT) :: STR_OUT

! This routine converts INT_ARG to a decimal string.
! appends the string value to STR_IN and stores it
! in STR_OUT. A trailing NUL is added to keep C
! happy.
!
! Fortran 2015 C Interoperability features are used in
! this sample. Because CHARACTER(*) dummy arguments are
! used, the C code must pass a "C Descriptor" for these
! arguments. The descriptors are automatically interpreted
! by Fortran. STR_IN will have the right length.

CHARACTER*5 INT_STR

WRITE (INT_STR,'(I5.5)')INT_ARG

STR_OUT = STR_IN // INT_STR // C_NULL_CHAR

RETURN
END 
