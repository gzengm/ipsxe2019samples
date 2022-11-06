!#==============================================================================
!#
!#  SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
!#  http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!#
!#  Copyright 2012-2016 Intel Corporation
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
!#      NQueens - OpenMP sample program for Intel Visual Fortran
!#******************************************************************************
!

program NQueens

! Solve the nqueens problem - OpenMP version
!
! Original C++ code by Ralf Ratering & Mario Deilmann
! Fortran version by Steve Lionel
!
! Read http://en.wikipedia.org/wiki/Nqueens for background
!
! To set command line argument in Visual Studio, right click on the project name and select Properties.
! Under Debugging, enter the argument (board size) in Command Arguments.
!
use omp_lib
implicit none

! To use the version that does not require synchronization across
! the threads, define the following preprocessor symbol.  Otherwise,
! comment it out
!DEC$ DEFINE _NOSYNC

integer :: nrOfSolutions = 0 
integer :: size = 0

character(2) :: cmdarg
integer :: stat
integer :: time_start, time_end, count_rate

! Declare an array for solution counts,
! one element per thread, used for NOSYNC version.  
! Note that if OpenMP is disabled, then nthreads will be 1
integer, allocatable :: solcnt(:)
integer :: nthreads = 1

! By default, OpenMP will use the number of cores for the
! maximum number of threads.  If you want to set this manually,
! remove the "!!" (not the "!$") from the line below
!$ !! call omp_set_num_threads(2)

! Following line will be compiled only if OpenMP enabled
!$ nthreads = omp_get_max_threads()

!DEC$ IF DEFINED(_NOSYNC)
allocate (solcnt(0:nthreads-1),source=0)
!DEC$ END IF

! Get command line argument
if (command_argument_count() < 1) then
  print *, "Usage: nqueens-openmp boardSize"
  stop
  end if
call get_command_argument (1, cmdarg, status=stat)
if (stat /= 0) then
  print *, "Invalid boardSize"
  stop
  end if
read (cmdarg,*,iostat=stat) size
if ((stat /= 0) .or. (size < 1)) then
  print *, "Error: boardSize must be between 1 and 99"
  stop
  end if

! Begin
!DEC$ IF DEFINED(_NOSYNC)
print 101, "Starting unsynchronized OpenMP solver for size ", size, &
  " with ", nthreads, " thread(s)"
!DEC$ ELSE
print 101, "Starting OpenMP solver for size ", size, &
  " with ", nthreads, " thread(s)"
!DEC$ END IF
101 format (A,I0,A,I0,A)
call system_clock (time_start)
call solve (size)
call system_clock (time_end, count_rate)
print 101, "Number of solutions: ", nrOfSolutions
print 101, "Calculations took ", (time_end-time_start) / (count_rate/1000), "ms."
if (allocated(solcnt)) deallocate (solcnt)

contains

! Routine to print the board

subroutine print (queens)
  implicit none
  integer, intent(in) :: queens(:)
  integer :: row, col

  do row=1,size
    do col=1,size
      if (queens(row) == col) then
        write (*,'(A)',advance='no') "Q"
      else
        write (*,'(A)',advance='no') "-"
      end if
    end do
  write (*,'(A)')
  end do
write (*,*)
end subroutine print

! Recursive routine to set a queen on the board

recursive subroutine setQueen (queens, row, col, id)
  implicit none
  integer, intent(inout) :: queens(:)
  integer, intent(in) :: row, col, id
  integer :: i
  
  do i=1,row-1
    ! vertical attacks
    if (queens(i) == col) return
    ! diagonal attacks
    if (abs(queens(i)-col) == (row-i)) return
    end do
    
  ! column is ok, set the queen
  queens(row) = col
  
  if (row == size) then

!DEC$ IF DEFINED(_NOSYNC)
    ! No need to synchronize thread-specific count
    solcnt(id) = solcnt(id) + 1
!DEC$ ELSE
    !$OMP ATOMIC
    nrOfSolutions = nrOfSolutions + 1
!DEC$ END IF
!DEC$ IF DEFINED(PRINT)
    ! Only one thread can print at a time
    !$OMP CRITICAL 
    call print(queens)
    !$OMP END CRITICAL 
!DEC$ END IF
  else
    ! try to fill next row
    do i=1,size
      call setQueen (queens, row+1, i, id)
    end do
  end if
end subroutine SetQueen

! Main solver routine

subroutine solve (size)
  implicit none
  integer, intent(in) :: size
  integer, allocatable :: queens(:)
  integer :: i
  integer :: myid = 0

!$OMP PARALLEL DO FIRSTPRIVATE(myid) PRIVATE(queens)
  do i=1,size
    ! Following statements will be executed only if
    ! OpenMP (and its conditional compilation) is enabled
!$  myid = omp_get_thread_num()

    ! try all positions in first row
    allocate(queens(size),source=0)
    call SetQueen (queens, 1, i, myid)
    deallocate(queens)
  end do
!DEC$ IF DEFINED(_NOSYNC)
nrOfSolutions = sum(solcnt)
!DEC$ END IF
end subroutine solve

end program nQueens