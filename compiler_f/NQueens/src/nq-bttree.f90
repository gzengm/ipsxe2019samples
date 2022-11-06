!#==============================================================================
!#
!#  SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
!#  http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!#
!#  Copyright 2008-2016 Intel Corporation
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
!#      NQueens - BtTree sample program for Intel Visual Fortran
!#******************************************************************************
!

program NQueens

! Solve the nqueens problem using the Backtrack Tree method
! described by Martin Richards in
! "Backtracking Algorithms in MCPL using Bit Patterns and Recursion"
!
! Original C++ code by Ralf Ratering & Mario Deilmann
! Fortran version by Steve Lionel
!
! Read http://en.wikipedia.org/wiki/Nqueens for background
!
! To set command line argument in Visual Studio, right click on the project name and select Properties.
! Under Debugging, enter the argument (board size) in Command Arguments.


implicit none

integer :: nrOfSolutions = 0 
integer :: size = 0

character(2) :: cmdarg
integer :: size_mask
integer :: stat
integer :: time_start, time_end, count_rate

! Get command line argument
if (command_argument_count() < 1) then
  print *, "Usage: nq-bttree boardSize"
  stop
  end if
call get_command_argument (1, cmdarg, status=stat)
if (stat /= 0) then
  print *, "Invalid boardSize"
  stop
  end if
read (cmdarg,*,iostat=stat) size
if ((stat /= 0) .or. (size < 1) .or. (size > 32)) then
  print *, "Error: boardSize must be between 1 and 32"
  stop
  end if

! Begin
print 101, "Starting backtrack tree solver for size ", size
101 format (A,I0,A)
! Create bitmask with low order "size" bits set to 1
size_mask = (2**size) - 1

!Begin
call system_clock (time_start)
call solve (0,0,0)
call system_clock (time_end, count_rate)
print 101, "Number of solutions: ", nrOfSolutions
print 101, "Calculations took ", (time_end-time_start) / (count_rate/1000), "ms."

contains

recursive subroutine solve (row, leftdiag, rightdiag)
  implicit none
  integer, intent(in) :: row, leftdiag, rightdiag
  integer :: position, p

  if (row == size_mask) then
    nrOfSolutions = nrOfSolutions + 1
  else
    position = iand(size_mask, inot(ior(row, ior(leftdiag,rightdiag))))
    do while (position /= 0)
      p = iand(position,-position)
      position = position - p
      call solve (row+p, ishft(leftdiag+p,1), ishft(rightdiag+p,-1))
    end do
  end if
end subroutine solve

end program NQueens