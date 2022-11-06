! Copyright (C) 2016 Intel Corporation. All Rights Reserved. 
!
! The source code contained or described herein and all documents related to the source code 
! ("Material") are owned by Intel Corporation or its suppliers or licensors. Title to the 
! Material remains with Intel Corporation or its suppliers and licensors.  The Material is 
! protected by worldwide copyright laws and treaty provisions. No part of the Material may be 
! used, copied, reproduced, modified, published, uploaded, posted, transmitted, distributed, 
! or disclosed in any way except as expressly provided in the license provided with the 
! Materials.  No license under any patent, copyright, trade secret or other intellectual 
! property right is granted to or conferred upon you by disclosure or delivery of the 
! Materials, either expressly, by implication, inducement, estoppel or otherwise, except as 
! expressly provided in the license provided with the Materials.

!!!  filename : arrays.f90 

!!! This subroutine takes as input an array of strings from Visual Basic.NET,
!!! and writes each string out to a datafile.
!!! It also writes various pieces of information about the array to that
!!! file, for illustrative purposes.
!!!
!!! Original example by Lorri Menard, Intel Compiler Engineering
!!! Enhancements by John Termine, Swiss Re Investors
!!! Rewritten for VB.NET and multidimensional arrays by Steve Lionel, Intel Compiler Support

subroutine ForCall (VBArray)

  ! Set the attributes needed for compatibility with VB.NET.  VB always uses STDCALL.
  !
  !dec$ attributes dllexport, stdcall, reference, alias : "ForCall" :: ForCall

  use ifcom ! Declare SafeArray and BSTR interfaces

  implicit none
  
  integer(int_ptr_kind()), intent(inout) :: VBArray  !Pointer to a SafeArray structure  

  integer, parameter :: LONG_ENOUGH_BUFFER = 2048 ! Assume we won't get a string longer than this
  character(LEN=LONG_ENOUGH_BUFFER)  mystring  ! Fortran string converted to/from BSTR
  integer(int_ptr_kind()) :: BSTRptr  ! Receives a pointer to a BSTR
    
  ! Array in which we will keep track of array bounds
  type bounds_type
    integer lb  ! Lower Bound
    integer ub  ! Upper Bound
    end type bounds_type
  integer nbounds  ! Number of bounds
  type(bounds_type), allocatable :: bounds(:)
  integer, allocatable :: indexes(:)  ! Array to hold current element indexes
  
  integer :: i, iRes, length


  ! First, we'll get the bounds of the array. This code makes no assumptions about the number of
  ! dimensions.
  !
  nbounds = SafeArrayGetDim (VBArray)
  allocate (bounds(nbounds), indexes(nbounds))
  
  do i=1,nbounds
    ires = SafeArrayGetLbound (VBArray, i, bounds(i)%lb)
    ires = SafeArrayGetUbound (VBArray, i, bounds(i)%ub)
    end do
    

  ! Example 1 - write to a text file (since we don't have a console) the
  ! bounds of the array.  You'll find this file in the BIN subfolder of
  ! the VB.NET project folder.

  open (2, file="testout.txt", status="unknown")

  write (2, *) "Shape of the array passed by VB:"  
  write (2,'("  (")',advance='no')
  do i=1,nbounds
    write (2,'(I0,":",I0)',advance='no') bounds(i)
    if (i < nbounds) write(2,'(",")',advance='no')
    end do
  write (2,'(")")')


  ! Example 2 - Write the values of the string elements to the file. This code
  ! also makes no assumptions about the number of dimensions. 
  !
  ! For each element we:
  !  1) Call SafeArrayGetElement to return a pointer to a BSTR element
  !  2) Convert the BSTR to a Fortran string (which we then write to the file)
  !  3) Free the string which we retrieved
  !
  ! Note that the current interface to SafeArrayGetElement has the second "indices"
  ! argument defined as a scalar - we work around that by passing the first element
  ! by reference.
  !
  write (2, *) "Strings from the array:"
  
  indexes = bounds%lb  ! Initialize to all lower bounds
  readloop: do
    ires = SafeArrayGetElement (VBArray, indexes(1), loc(BSTRPtr))
    length = ConvertBSTRToString (BSTRPtr, mystring)
    call SysFreeString(BSTRPtr)
    write (2, '("  A(")', advance='no')
    write (2, '(100(I0,:,","))', advance='no') (indexes(i),i=1,nbounds)
    write (2, '(") = ", A)') mystring(1:length)
    
    ! Determine what the next element is.  We increment the last index,
    ! and if it is greater than the upper bound, reset it to the lower bound and
    ! repeat for the next lower index.  If we run out of indexes, we're done.
    do i = nbounds, 1, -1
      indexes(i) = indexes(i) + 1
      if (indexes(i) <= bounds(i)%ub) exit
      indexes(i) = bounds(i)%lb
      if (i == 1) exit readloop
      end do
    end do readloop
        
  close (2) ! We're done with the file

  ! Example 3 - Modifying BSTR elements in a SafeArray.  Here we append
  ! " potato" to each of the elements.
  !
  indexes = bounds%lb  ! Initialize to all lower bounds
  writeloop: do
    ires = SafeArrayGetElement (VBArray, indexes(1), loc(BSTRPtr))
    length = ConvertBSTRToString (BSTRPtr, mystring)
    call SysFreeString (BSTRPtr)

    ! Append "potato" to the element
    mystring = trim(mystring) // " potato"
    
    ! Convert it back to a BSTR    
    BSTRptr = ConvertStringToBSTR (trim(mystring))
    
    ! Write it back to the array
    ires = SafeArrayPutElement (VBArray, indexes(1), BSTRptr)
    call SysFreeString (BSTRPtr)  ! Free our copy
        
    ! Determine what the next element is.  We increment the last index,
    ! and if it is greater than the upper bound, reset it to the lower bound and
    ! repeat for the next lower index.  If we run out of indexes, we're done.
    do i = nbounds, 1, -1
      indexes(i) = indexes(i) + 1
      if (indexes(i) <= bounds(i)%ub) exit
      indexes(i) = bounds(i)%lb
      if (i == 1) exit writeloop
      end do
    end do writeloop

  ! Deallocate our local arrays, though this would be done implicitly anyway
  !
  deallocate (bounds)
  deallocate (indexes)
  
  return  
end subroutine ForCall
