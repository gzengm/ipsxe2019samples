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

program MemoryStatus
use kernel32
use GetValueMod ! Defines generic function gv
implicit none

type (T_MEMORYSTATUSEX) :: stat
integer(BOOL) :: ret

! Set the length field in structure stat

stat%dwlength = sizeof(stat)

! Get the statistics and then display the results
ret = GlobalMemoryStatusEx (stat)
if (ret == 0) then
  print *, "GlobalMemoryStatusEx call failed, error code = ", GetLastError()
  stop
  end if

write (*,'(I0,A)') stat%dwMemoryLoad, '% of memory is in use'
write (*,'(A,A)') trim(gv(stat%ullTotalPhys)), ' total physical memory'
write (*,'(A,A)') trim(gv(stat%ullAvailPhys)), ' available physical memory'
write (*,'(A,A)') trim(gv(stat%ullTotalPageFile)), ' total pageable memory'
write (*,'(A,A)') trim(gv(stat%ullAvailPageFile)), ' available pageable memory'
write (*,'(A,A)') trim(gv(stat%ullTotalVirtual )), ' total virtual memory'
write (*,'(A,A)') trim(gv(stat%ullAvailVirtual  )), ' available virtual memory'

end program MemoryStatus