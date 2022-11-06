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
!#      NQueens - Serial sample program for Intel Visual Fortran
!#******************************************************************************
!

program NQueens

! Solve the nqueens problem - base serial version
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
integer :: stat
integer :: time_start, time_end, count_rate

! Get command line argument
if (command_argument_count() < 1) then
  print *, "Usage: nqueens-serial boardSize"
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
print 101, "Starting serial recursive solver for size ", size
101 format (A,I0,A)
call system_clock (time_start)
call solve (size)
call system_clock (time_end, count_rate)
print 101, "Number of solutions: ", nrOfSolutions
print 101, "Calculations took ", (time_end-time_start) / (count_rate/1000), "ms."

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

recursive subroutine setQueen (queens, row, col)
  implicit none
  integer, intent(inout) :: queens(:)
  integer, intent(in) :: row, col
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
    nrOfSolutions = nrOfSolutions + 1
!DEC$ IF DEFINED(PRINT)
    call print(queens)
!DEC$ END IF
  else
    ! try to fill next row
    do i=1,size
      call setQueen (queens, row+1, i)
    end do
  end if
end subroutine SetQueen

! Main solver routine

subroutine solve (size)
  implicit none
  integer, intent(in):: size
  integer, allocatable :: lcl_queens(:)
  integer i

  do i=1,size
    ! try all positions in first row
!DEC$ IF DEFINED(PRINT)
    print '(A,I0)', "Call SetQueen with: row:1, column: ", i
!DEC$ END IF
    allocate(lcl_queens(size),source=0)
    call SetQueen (lcl_queens, 1, i)
    deallocate(lcl_queens)
  end do
end subroutine solve

end program nQueens