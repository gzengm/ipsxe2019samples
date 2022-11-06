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

! This example demonstrates how to build and use a DLL to share data across multiple
! executables on an application. It also shows the use of a mutex to synchronize access
! across processes.

program DLL_Shared_Data
use ifwin
use ifport, only: sleepqq
use shared_globals
implicit none

integer(HANDLE) :: mutex
integer(HANDLE) :: self
integer(HANDLE) :: child
integer(DWORD) ret
integer ios, i
character(MAX_PATH) :: self_filename

! Create the mutex object.  It may already be created.
!
write (*,'(A)') 'Creating/opening mutex'
mutex = CreateMutex (NULL, & ! Security attributes
                     TRUE, & ! If we created the mutex, take ownership
                     'DLL_Shared_Data_MUTEX'C) ! Name
ret = GetLastError()
if (mutex == NULL) then
  write (*,'(A,I0)') 'CreateMutex failed, status = ', ret
  stop
  end if

! Choose which mode to run in depending on whether or not we created the
! mutex
!
if (ret /= ERROR_ALREADY_EXISTS) then
  ! We're first.
  write (*,'(A,I0)') 'Parent process, mutex ID = ', mutex
  
  ! Get the name of our executable and run another copy
  !
  self = GetModuleHandle (NULL)
  if (self == NULL) then
    write (*,'(A,I0)') 'GetModuleHandle failed, status = ', GetLastError()
    stop
    end if
  ret = GetModuleFileName (self, self_filename, len(self_filename))
  ! ret gets length of string (not including trailing NUL)
  if (ret == 0) then
    write (*, '(A,I0)') 'GetModuleFileName failed, status = ', GetLastError()
    stop
    end if
  write (*, '(A,A)') 'Our file name is ', self_filename(1:ret)
  
  ! Run a copy of ourself. The copy will find that the mutex already exists,
  ! and will fall into the alternate codepath
  child = ShellExecute (NULL, 'open'C, self_filename, &
    NULL, NULL, SW_SHOWNORMAL)
  if (child <= 32) then
    write (*, '(A,I0)') 'ShellExecute failed, status = ', child
    stop
    end if
  write (*, '(A)') 'Child process successfully started - waiting three seconds'
  call sleepqq(3000)
  
  ! Main user loop.
  
  mainloop: do
    ! Prompt user for a value.
    write (*,'(A)',advance='NO') 'Enter positive real value to be sent to child, 0 to exit: '
    read (*,'(G10.0)',iostat=ios) shared_variable
	call sleepqq(1000)   !slow update of shared variable to prevent read after write race condition
    if ((ios > 0) .or. (shared_variable < 0.0)) then
      write (*, '(A)') 'Invalid value - please re-enter'
      cycle mainloop
    else if ((ios < 0) .or. (shared_variable == 0.0)) then
      exit mainloop
      end if
  
    ! Release the mutex
  
    ret = ReleaseMutex (mutex)
    if (ret == 0) then
      write (*,'(A,I0)') 'ReleaseMutex failed, status = ', ret
      stop
      end if
    
    pwaitloop: do i=10, 0, -1
      ! Wait for a response.  Use a one-second timeout so that we
      ! can keep the user informed and detect a failure.
      !
      write (*,'(A,I0)') 'Waiting for response...',i
      ret = WaitForSingleObject (mutex, 1000)
      select case (ret)
    
        case (WAIT_ABANDONED)
          write (*,*) 'Child process unexpectedly terminated'
          exit mainloop
        case (WAIT_TIMEOUT)
          if (i > 0) cycle pwaitloop
          write (*,'(A)') 'Child process did not respond - exiting'
          exit mainloop
        case (WAIT_FAILED)
          write (*, '(A,I0)') 'Wait failed, status = ', GetLastError()
          exit mainloop
        case (WAIT_OBJECT_0)  ! Normal
        end select

        exit pwaitloop  ! Wait succeeded

      end do pwaitloop
    
    ! If we get here, the wait succeeded
    ! Check to see if the child reset the flag
    ! which we left set as true.
    if (not_dead_yet) then
      write (*,'(A)') 'Child process seems to be gone - exiting'
      stop
      end if
      
    write (*,'(A,G0)') 'Child process returned SQRT value of ', shared_variable
    
    ! Reset the flag to show that we are still alive
    not_dead_yet = .TRUE.

    ! Prompt for a new value
    end do mainloop
    
  ! All done - clean up
  shared_variable = 0.0
  not_dead_yet = .TRUE.
  ret = ReleaseMutex (mutex)
  
  ! All done
  
else ! (ret /= ERROR_ALREADY_EXISTS)
  ! We're not first
  write (*,'(A,I0)') 'Child process, mutex ID = ', mutex

  childloop: do
    cwaitloop: do i=100, 0, -1
      ! Wait for a response.  Use a one-second timeout so that we
      ! can keep the user informed and detect a failure.
      !
      write (*,'(A,I0)') 'Waiting for value from parent...',i
      ret = WaitForSingleObject (mutex, 1000)
      select case (ret)
    
        case (WAIT_ABANDONED)
          write (*,*) 'Parent process unexpectedly terminated'
          exit childloop
        case (WAIT_TIMEOUT)
          if (i > 0) cycle cwaitloop
          write (*,'(A)') 'Timeout - giving up on parent'
          exit childloop
        case (WAIT_FAILED)
          write (*, '(A,I0)') 'Wait failed, status = ', GetLastError()
          exit childloop
        case (WAIT_OBJECT_0)
          exit cwaitloop
        end select
      end do cwaitloop
    
    ! If we get here, the wait succeeded
    ! Check to see if the parent  reset the flag
    ! which we left set as false.
    if (.not. not_dead_yet) then
      write (*,'(A)') 'Parent process seems to be gone'
      exit childloop
      end if  
      
    ! Validate the value
    if (shared_variable <= 0.0) then
      write (*,'(A)') 'Received exit value'
      exit childloop
      end if
      
    write (*,'(A,G0)') 'Received value ', shared_variable
    ! Take the square root and tell the parent we've done it
    shared_variable = sqrt(shared_variable)
    not_dead_yet = .false.
    write (*,'(A)') 'Releasing mutex'
    ret = ReleaseMutex (mutex)
    call sleepqq(1000)
    end do childloop
     
  end if  ! Child process

! Common exit
write (*,'(A)') 'Press return to exit'
accept *     

end