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

!  Platform.f90
!
!  Demonstrates use of the Windows API to determine which platform and version
!  of Windows we are on.
!
!  Build with ifort /winapp platform.f90
!
!****************************************************************************

integer(DWORD) function WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'WinMain' :: WinMain

use kernel32
use user32

implicit none
    
integer(HANDLE) hInstance
integer(HANDLE) hPrevInstance
integer(LPVOID) lpszCmdLine
integer(DWORD) nCmdShow

type(T_OSVERSIONINFO) :: ovi
type(T_SYSTEM_INFO) :: sysi
integer(BOOL) :: bret
integer(DWORD) :: ret
integer(DWORD) :: lasterr
character(1000) :: message
integer(HANDLE) :: hwnd
integer ilen
character(50) unknownver
character(50) nprocessors
character(2), parameter :: CRLF = CHAR(13)//CHAR(10)

! GetNativeSystemInfo is not yet defined in module KERNEL32
! Declare our own version here.  The difference between this
! and GetSystemInfo is that GetSystemInfo will say the platform
! is IA-32 architecture if it is a 32-bit application run on
! a 64-bit system, whereas GetNativeSystemInfo will report the
! system's native architecture
interface
subroutine GetNativeSystemInfo$ (lpSystemInfo)
import
!DEC$ attributes stdcall, reference, decorate, alias:"GetNativeSystemInfo" :: GetNativeSystemInfo$
type(T_SYSTEM_INFO), intent(INOUT) :: lpSystemInfo
end subroutine GetNativeSystemInfo$
end interface

hwnd = NULL
WinMain = 0

! Set size of OSVERSION structure and get OS Version info
ovi%dwOSVersionInfoSize = sizeof(ovi)
bret = GetVersionEx(ovi)
if (bret == 0) then
  lasterr = GetLastError()
  write (message,'(A,I0,A)') "GetVersion call failed with status ", lasterr, CHAR(0)
  ret = MessageBox (hwnd, message, "Platform Error"C, IOR(MB_OK,MB_ICONEXCLAMATION))
  return
  end if
! In case of error,write out major and minor version
write (unknownver, '(A,I0,A,I0)') ' Unknown ',ovi%dwMajorVersion,'-',ovi%dwMinorVersion

! Get system info
call GetNativeSystemInfo$ (sysi)

! Now put together the message

message = 'Platform information:' // CRLF

! Processor type

message = trim(message) // '  Processor Architecture:'
select case (sysi%wProcessorArchitecture)

  case(PROCESSOR_ARCHITECTURE_INTEL)
    message = trim(message) // ' IA-32' // CRLF
  case(PROCESSOR_ARCHITECTURE_IA64)
    message = trim(message) // ' IA-64 [Intel(R) Itanium(R)]' // CRLF
  case(PROCESSOR_ARCHITECTURE_AMD64)
    message = trim(message) // ' Intel(R) 64 or AMD64*' // CRLF
  case default
    message = trim(message) // 'Unknown' // CRLF
  end select

! Number of processors (can't distinguish among physical CPUs,
! cores or logical processors)

write (nprocessors, '(A, I0)') '  Number of logical processors: ', sysi%dwNumberOfProcessors
message = trim(message) // trim(nprocessors) // CRLF

! Windows version

message = trim(message) // '  Windows* Version:'
select case (ovi%dwMajorVersion)
  case (4) ! Windows NT 4.0, Windows Me, Windows 98, or Windows 95
    select case (ovi%dwMinorVersion)
      case (0) ! Windows NT 4.0, or Windows 95.
      if (ovi%dwPlatformID == VER_PLATFORM_WIN32_NT) then
        message = trim(message) // ' Windows NT 4.0' 
      else
        message = trim(message) // ' Windows 95' 
        end if
      case (10)
        message = trim(message) // ' Windows 98' 
      case (90)
        message = trim(message) // ' Windows Me'
      case default
        message = trim(message) // unknownver
      end select
   case(5) ! Windows Server 2003 R2, Windows Server 2003, Windows XP, or Windows 2000.
     select case (ovi%dwMinorVersion)
       case(0)
         message = trim(message) // ' Windows 2000'
       case(1)
         message = trim(message) // ' Windows XP'
       case(2)
         message = trim(message) // ' Windows Server 2003'
       case default
         message = trim(message) // unknownver
       end select
    case(6)
      select case (ovi%dwMinorVersion)
        case (0)
          message = trim(message) // ' Windows Vista or Windows Server 2008'
        case (1)
          message = trim(message) // ' Windows 7'
        case (2)
          message = trim(message) // ' Windows 10 or Windows Server 2012'
        case default
          message = trim(message) // unknownver
        end select
    case default
      message = trim(message) // unknownver
    end select
    
! Is there service pack info?  If so, add it
ilen = index(ovi%szCSDVersion, char(0)) - 1
if (ilen > 0) message = trim(message) // ' (' // ovi%szCSDVersion(1:ilen) // ')'
    
! Display message
message = trim(message) //CHAR(0)
ret = MessageBox (hwnd, message, 'Platform'C, MB_OK)

end function WinMain