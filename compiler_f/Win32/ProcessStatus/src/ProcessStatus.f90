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

!  ProcessStatus.f90 
!
! Demonstrate use of some of the Process Status Win32 API (PSAPI) routines.
!
! Part of the Intel Visual Fortran samples
!
! The program lists all of the pagefiles on the system and then displays
! memory usage information for every process on the system
!

program ProcessStatus
use psapi
use kernel32
implicit none

integer(DWORD), dimension(1024) :: Processes
integer(DWORD) :: cbNeeded, cProcesses
integer(BOOL) :: ret
integer i

! Enumerate pagefiles
! The PageFileCallBackProc function will be called for each
! pagefile by EnumPageFiles
!
print *, "Pagefile information"
ret = EnumPageFiles (PageFileCallBackProc, 0)  ! Context not used

print *, ""

! Get list of process identifiers

ret = EnumProcesses (Processes, int(sizeof(Processes),DWORD), cbNeeded)
if (ret == 0) then
  print *, "EnumProcesses failed, status = ", GetLastError()
  stop
  end if
  
! Calculate how many process identifiers were returned

cProcesses = cbNeeded / sizeof(0_DWORD)

! Print the memory usage for each process

do i=1,cProcesses
  call PrintMemoryInfo (Processes(i))
  end do

contains

function PageFileCallBackProc (pContext, pPageFileInfo, lpFilename)
use psapi
integer(BOOL) :: PageFileCallBackProc
!DEC$ ATTRIBUTES DEFAULT,STDCALL :: PageFileCallBackProc
integer(LPVOID), intent(IN) :: pContext
type(T_ENUM_PAGE_FILE_INFORMATION), intent(IN) :: pPageFileInfo
!DEC$ ATTRIBUTES REFERENCE :: pPageFileInfo
character(MAX_PATH), intent(IN) :: lpFilename ! NUL terminated
!DEC$ ATTRIBUTES REFERENCE :: lpFilename

integer ilen

! Find length of pagefile name
ilen = index(lpFilename, CHAR(0))

! Display information about this pagefile
!
write (*, '(1X,A/(2X,A,1X,I0))') lpFileName(1:ilen), &
  'TotalSize:', pPageFileInfo%TotalSize, &
  'TotalInUse:', pPageFileInfo%TotalInUse, &
  'PeakUsage:', pPageFileInfo%PeakUsage

PageFileCallBackProc = TRUE
return
end function PageFileCallBackProc
  
  
subroutine PrintMemoryInfo (processID)
use psapi
use kernel32
implicit none

integer(DWORD), intent(IN) :: processID

integer(HANDLE) :: hProcess
type (T_PROCESS_MEMORY_COUNTERS) :: pmc
integer(BOOL) :: ret

! Print the process identifier

write (*,'(" Process ID: ", I0)') processID

! Print information about the memory usage of the process

hProcess = OpenProcess (IOR(PROCESS_QUERY_INFORMATION, PROCESS_VM_READ), &
    FALSE, processID)
if (hProcess == NULL) return  ! Will return null for the idle process
ret = GetProcessMemoryInfo (hProcess, pmc, int(sizeof(pmc),DWORD))
if (ret /= 0) &
  write (*, '(2X,A,1X,I0)') &
    "PageFaultCount:", pmc%PageFaultCount, &
    "PeakWorkingSetSize:", pmc%PeakWorkingSetSize, &
    "WorkingSetSize:", pmc%WorkingSetSize, &
    "QuotaPeakPagedPoolUsage:", pmc%QuotaPeakPagedPoolUsage, &
    "QuotaPeakNonPagedPoolUsage:", pmc%QuotaPeakNonPagedPoolUsage, &
    "QuotaNonPagedPoolUsage:", pmc%QuotaNonPagedPoolUsage, &
    "PagefileUsage:", pmc%PagefileUsage, &
    "PeakPagefileUsage:", pmc%PeakPagefileUsage
    
ret = CloseHandle (hprocess)
return

end subroutine PrintMemoryInfo

end program ProcessStatus