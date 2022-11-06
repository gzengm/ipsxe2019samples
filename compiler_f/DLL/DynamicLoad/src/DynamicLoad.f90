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

!  DynamicLoad.f90 
!
!  Example demonstrating the use of the Win32 APIs LoadLibrary and
!  GetProcAddress to dynamically load a DLL, look up a routine, and
!  call the routine
!
!  The program will load a DLL called 'USERDLL' and look for a routine
!  called USERFUNC.  This function must accept an integer argument and
!  return an integer result.  If the function is found, it is called
!  ten times with arguments 1-10, and the result displayed.  At the end,
!  the library is unloaded.
!
!  This program also illustrates use of Fortran 2003 features such as abstract
!  interfaces and procedure pointers.
!
!  A sample DLL project USERDLL is also provided.  
!****************************************************************************

program DynamicLoad

use kernel32  ! Declares Windows API routines
use, intrinsic :: iso_c_binding
implicit none

! Declare the interface to function USERFUNC.  Because it is an abstract interface,
! it does not refer to any specific procedure.
! NOTE: If you are loading a 32-bit DLL, you will need to know whether the routine
! you are calling uses the C or STDCALL calling convention.  Intel Fortran assumes
! the C convention by default.  If you know that you are calling a STDCALL procedure,
! such as a Windows API routine or a Visual Basic function, you will need to add
! a directive "!DEC$ ATTRIBUTES STDCALL,REFERENCE :: funcname" after the "function"
! statement in the interface, changing "funcname" to be the name of the function.

abstract interface
  function USERFUNC_int (arg)
  integer USERFUNC_int
  integer, intent(IN) :: arg
  end function USERFUNC_int
end interface

! Declare a procedure pointer that will be used to call the function,
! based on the abstract interface declared above.
procedure(USERFUNC_int), pointer :: USERFUNC

! Finally, a variable that will get the return value of GetProcAddress
! This must be an address_sized integer
integer(C_INTPTR_T) :: p_USERFUNC

integer(HANDLE) :: dll_handle
integer(BOOL) :: free_status
integer i, ret

! Load the DLL and get a "handle" to it.  Note that the DLL name must
! be a C string and that DLLs are looked for in the following locations:
! 1) Current directory
! 2) Directory containing this .EXE
! 3) Directories on PATH
! 4) Windows directory
! 5) Windows System directory
!
! This project has a post-build step that copies the DLL into this project's
! output directory.
!
write (*,'(A)') "Loading library..."
dll_handle = LoadLibrary (lpLibFileName="USERDLL.dll"//C_NULL_CHAR)
! Check for errors
if (dll_handle == NULL) then
  ! Failure
  call print_error ('loading DLL')
  stop
  end if

! Look up the routine address.  Note that this example requires that
! the function be named "USERFUNC" without any decoration such as a
! leading underscore.  This may require an ALIAS directive or similar
! in the DLL routine.
!
write (*,'(A)') "Getting routine address..."
p_USERFUNC = GetProcAddress (hModule=dll_handle, lpProcName="USERFUNC"//C_NULL_CHAR)
if (p_USERFUNC == NULL) then
  ! Failure
  call print_error ('looking up routine')
  stop
  end if
  
! At this point we have the address of the function in an integer.  It must
! be converted to a procedure pointer.  We do this in two steps.  First, the
! integer address is converted to a C_FUNPTR using TRANSFER (this is safe because
! we know that the two are the same size), and then to a procedure pointer using
! C_F_PROCPOINTER
!
call C_F_PROCPOINTER (TRANSFER(p_USERFUNC, C_NULL_FUNPTR), USERFUNC)
  
! Now call the function in a loop
!
do i=1,10
  write (*,'(/A, G0)') "Calling USERFUNC with argument ", i
  ret = USERFUNC(i)
  write (*,'(A,G0)') "USERFUNC returned ", ret
  end do
  
! Unload the library.  This will be done automatically on program
! exit but it's good practice anyway
!
write(*,'(/A)') "Unloading library..."
free_status = FreeLibrary (hLibModule=dll_handle)
if (free_status == 0) call print_error ('unloading DLL')

! End of main program

contains

! Error processing routine.  Gets the system error and
! its corresponding string, prints a message, then stops
! execution
!
subroutine print_error (string)
use kernel32
use, intrinsic :: iso_c_binding
implicit none

character(*), intent(IN) :: string

integer(DWORD) :: last_error
integer(DWORD) :: nTchars
integer(HANDLE) :: ret
type(C_PTR) :: message_buffer_cptr
character, pointer :: message_buffer(:)

! Get the actual system error code
!
last_error = GetLastError ()

! Get the string corresponding to this error
! Use the option to have Windows allocate the message buffer - it puts the
! address in the lpBuffer argument. Here we pass it the C_PTR message_buffer_cptr,
! using TRANSFER to cast the address to an LPVOID.  FORMAT_MESSAGE_IGNORE_INSERTS
! is used so that it doesn't try looking for arguments - a possible security violation.
! Again, we're using C interoperability features.
!
nTchars = FormatMessage ( &
  dwFlags=IANY([FORMAT_MESSAGE_FROM_SYSTEM, FORMAT_MESSAGE_IGNORE_INSERTS, FORMAT_MESSAGE_ALLOCATE_BUFFER]), &
  lpSource=NULL, & ! ignored
  dwMessageId=last_error, &
  dwLanguageId=0, & ! will use a default
  lpBuffer=TRANSFER(C_LOC(message_buffer_cptr), 0_LPVOID), &
  nSize=100, & ! minimum size to allocate
  arguments=NULL)

if (nTchars == 0) then
  write (*,'(A,Z8.8,3A,Z8.8)') "Format message failed for status ", last_error, " while ", &
    string, ": error status = ", GetLastError()
else
  ! message_buffer_cptr is now pointing to the message. Use C_F_POINTER to convert
  ! this to an array of characters. Note the use of the Fortran 2008 unlimited
  ! repeat count specifier.
  call C_F_POINTER (message_buffer_cptr, message_buffer, [nTchars])
  write (*,'(3A,*(A))') "Error while ", string, ": ", message_buffer
  ! Free the memory Windows allocated for the message string
  ret = LocalFree (hMem=TRANSFER(message_buffer_cptr, 0_HANDLE))
  end if
  
stop

end subroutine print_error

end program DynamicLoad

