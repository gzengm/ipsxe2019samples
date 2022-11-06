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
PROGRAM fmain
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

! This is an example of a Fortran main program calling
! a C routine. It uses the C Interoperability features
! of Fortran 2003

! Declare the interface for the C routine we'll call
!
INTERFACE
    ! The BIND(C) tells the compiler that this is an "interoperable"
    ! procedure.  The compiler adjusts the naming conventions as
    ! appropriate for the companion C processor.
    SUBROUTINE c_routine (int_arg, str_in, str_out, str_out_len) BIND(C)
    IMPORT ! Use declarations from host

    ! First argument is a C "int", passed by value
    INTEGER(C_INT), VALUE,INTENT(IN) :: int_arg
    ! Second and third arguments are C "char *", represented
    ! in Fortran by an array of single characters of kind C_CHAR.
    ! Note that the language allows passing a regular CHARACTER
    ! variable to such an argument. No hidden length is passed.
    CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: str_in
    CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: str_out
    ! Fourth argument is where we pass the length of str_out
    INTEGER(C_INT), VALUE, INTENT(IN) :: str_out_len
    END SUBROUTINE c_routine
END INTERFACE

CHARACTER(80) OUTPUT_TEXT
INTEGER IN_ARG, OUTPUT_LEN
CHARACTER(80) INPUT_TEXT

INPUT_TEXT = "Testing..."//C_NULL_CHAR 
IN_ARG = 123

! Call c_routine. It will return text in OUTPUT_TEXT
!
CALL c_routine (IN_ARG, INPUT_TEXT, OUTPUT_TEXT, LEN(OUTPUT_TEXT))

! Find the length of the output text, looking
! for the trailing blank
!
OUTPUT_LEN = INDEX(OUTPUT_TEXT," ")
IF (OUTPUT_LEN == 0) OUTPUT_LEN = len(OUTPUT_TEXT)

! Write the string to the console
!
WRITE (*,*) OUTPUT_TEXT(1:OUTPUT_LEN)

END