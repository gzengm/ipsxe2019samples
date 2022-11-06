!#==============================================================================
!#
!#  SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
!#  http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!#
!#  Copyright 2009-2016 Intel Corporation
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
!#      Rings sample program for Intel Visual Fortran
!#******************************************************************************
!

! Utility module replacing use of the GLAUX routines to create a window for OpenGL samples
!
module GLUtilsMod
use IFWINA ! Alternate version of IFWIN that renames MSFWIN$ routines that conflict with QuickWin
use, intrinsic :: ISO_C_BINDING

implicit none

private
public :: CreateOpenGLWindow, hInstance, hPalette

! Global variable for the instance
! These are visible to the user of the module
integer(HANDLE) :: hInstance = NULL
integer(HANDLE) :: hPalette = NULL

contains

! Adapted from example at opengl.org

function CreateOpenGLWindow (title, x, y, width, height, pixelType, flags, WindowProc) result(hWnd)
implicit none

integer(HANDLE) :: hWnd  ! Function result

! Arguments
! title [IN] - Character string title for the window.
character(*), intent(IN) :: title
! x,y,width,height [IN] - Inital position and size of the window
integer(SINT), intent(IN) :: x, y, width, height
! pixelType [IN] - Pixel Format Descriptor (PFD_) value for iPixelType
integer(BYTE), intent(IN) :: pixelType
! flags [IN] - Pixel Format Descriptor (PFD_) flags for dwFlags value
integer(DWORD), intent(IN) :: flags
! WindowProc [procedure] - Application's WindowProc procedure
interface
  integer(LONG) function WindowProc (hWnd, uMsg, wParam, lParam)
  !DEC$ ATTRIBUTES STDCALL :: WindowProc
  import
  integer(HANDLE) :: hWnd
  integer(UINT) :: uMsg
  integer(fWPARAM) :: wParam
  integer(fLPARAM) :: lParam
  end function WindowProc
end interface


! Declare our own type for LOGPALETTE - the one in IFWINTY has just
! one element.  The C code used malloc, and there are various Fortran
! ways we could fudge this, but the following seems simplest.  Note that
! there is a 256-entry limit to a palette.
type OUR_LOGPALETTE
  SEQUENCE
  integer(WORD) palVersion
  integer(WORD) palNumEntries 
  TYPE (T_PALETTEENTRY) palPalEntry(0:255)
  end type OUR_LOGPALETTE

! Locals
type(T_WNDCLASSEX) :: wc
type(OUR_LOGPALETTE) :: lPal
! Trick to make the call to CreatePalette work - see use below
  type(T_LOGPALETTE), pointer :: lPal2
type(T_PIXELFORMATDESCRIPTOR) :: pfd
integer(HANDLE) :: hDC, hRet
character(LEN(title)+1) :: lcl_title
integer(DWORD) :: ret
integer(SINT) :: n, pf, i, redmask, greenmask, bluemask
integer(USHORT) :: class_atom
character(*), parameter :: szClassName = "OpenGL"C

! Register the window class once only - use hInstance as a flag

if (hInstance == NULL) then
  hInstance = GetModuleHandle(NULL)
  wc%cbSize = sizeof(wc)
  wc%style = CS_OWNDC
  wc%lpfnWndProc = loc(WindowProc)
  wc%cbClsExtra = 0
  wc%cbWndExtra = 0
  wc.hInstance = hInstance
  wc.hIcon = LoadIcon(0_HANDLE, int(IDI_WINLOGO,LPVOID))
  wc.hCursor = LoadCursor(0_HANDLE, int(IDC_ARROW,LPVOID))
  wc.hbrBackground = (COLOR_WINDOW + 1) !NULL
  wc.lpszMenuName = NULL
  wc.lpszClassName = loc(szClassName)
  wc.hIconSm = NULL
  
  class_atom = RegisterClassEx(wc)
  if (class_atom == 0) then
    ret = MessageBox(NULL, "RegisterClass() failed: Cannot register window class."C, "Error"C, MB_OK)
    hWnd = NULL
    return
    end if
  end if

! Create the window

lcl_title = trim(title) // CHAR(0)
hWnd = CreateWindowEx(0,szClassName, &
                    lcl_title, &
                    IOR(WS_OVERLAPPEDWINDOW,IOR(WS_CLIPSIBLINGS,WS_CLIPCHILDREN)), &
                    x, y, width, height, &
                    NULL, NULL, &
                    hInstance, NULL)
if (hWnd == NULL) then
  ret = GetLastError()
  ret = MessageBox(NULL, "CreateWindow() failed:  Cannot create a window."C, "Error"C, MB_OK);
  return
  end if

hDC = GetDC(hWnd)
  
! Zero out all fields of the pixel format descriptor then set the header

call ZeroMemory (loc(pfd), sizeof(pfd))
pfd%nSize = sizeof(pfd)
pfd%nVersion = 1
pfd%dwFlags = ior(PFD_DRAW_TO_WINDOW, PFD_SUPPORT_OPENGL)
pfd%dwFlags = ior(pfd%dwFlags, flags)
pfd%iPixelType = pixelType
pfd%cColorBits = 32

pf = ChoosePixelFormat (hDC, pfd)
if (pf == 0) then
  ret = MessageBox(NULL, "ChosePixelFormat() failed: Cannot find a suitable pixel format."C, &
                         "Error"C, MB_OK)
  hWnd = 0
  return
  end if
  
ret = SetPixelFormat (hDC, pf, pfd)
if (ret == 0) then
  ret = MessageBox(NULL, "SetPixelFormat() failed: Cannot set format specified."C, &
                         "Error"C, MB_OK)
  hWnd = 0
  return
  end if

ret = DescribePixelFormat (hDC, pf, int(sizeof(pfd),UINT), pfd)

! Do we need to set up a palette?
!
if ((iand(pfd%dwFlags, PFD_NEED_PALETTE) /= 0) .or. (pfd%iPixelType == PFD_TYPE_COLORINDEX)) then
  n = ishft(1,pfd%cColorBits)
  if (n > 256) n = 256 ! See note for declaration of OUR_LOGPALETTE above
  
  call ZeroMemory (loc(lPal), sizeof(lPal))
  lPal%palVersion = Z'300'
  lPal%palNumEntries = n
  
  ret = GetSystemPaletteEntries(hDC, 0, n, lPal%palPalEntry(0))
  
  ! If the pixel type is RGBA then create an RGB ramp, otherwise
  ! (color index) set individual colors
  if (pfd%iPixelType == PFD_TYPE_RGBA) then
    redmask = ishft(1,pfd%cRedBits) - 1
    greenmask = ishft(1,pfd%cGreenBits) - 1
    bluemask = ishft(1,pfd%cBlueBits) - 1
    
    ! Fill in the entries with an RGB color ramp
    
    do i=0,n-1
      lPal%palPalEntry(i)%peRed = (iand(ishft(1,pfd%cRedShift),redmask) * 255) / redmask
      lPal%palPalEntry(i)%peGreen = (iand(ishft(1,pfd%cGreenShift),greenmask) * 255) / greenmask
      lPal%palPalEntry(i)%peBlue = (iand(ishft(1,pfd%cBlueShift),bluemask) * 255) / bluemask
    end do
  else
    lPal%palPalEntry(0)%peRed = 0;
    lPal%palPalEntry(0)%peGreen = 0;
    lPal%palPalEntry(0)%peBlue = 0;
    lPal%palPalEntry(0)%peFlags = PC_NOCOLLAPSE;
    lPal%palPalEntry(1)%peRed = 255;
    lPal%palPalEntry(1)%peGreen = 0;
    lPal%palPalEntry(1)%peBlue = 0;
    lPal%palPalEntry(1)%peFlags = PC_NOCOLLAPSE;
    lPal%palPalEntry(2)%peRed = 0;
    lPal%palPalEntry(2)%peGreen = 255;
    lPal%palPalEntry(2)%peBlue = 0;
    lPal%palPalEntry(2)%peFlags = PC_NOCOLLAPSE;
    lPal%palPalEntry(3)%peRed = 0;
    lPal%palPalEntry(3)%peGreen = 0;
    lPal%palPalEntry(3)%peBlue = 255;
    lPal%palPalEntry(3)%peFlags = PC_NOCOLLAPSE;   
  end if
  
  ! Second part of the trick to make this call work, since
  ! CreatePalette wants type T_LOGPALETTE
  ! This essentially "casts" lPal as an object of type T_LOGPALETTE
  call c_f_pointer(c_loc(lPal), lPal2)
  hPalette = CreatePalette(lPal2)
  if (hPalette /= NULL) then
    hret = SelectPalette(hDC, hPalette, FALSE) ! Name in GDI32
    ret = RealizePalette(hDC)
  end if 
  
end if

ret = ReleaseDC(hDC, hWnd)  
  
return
end function CreateOpenGLWindow


end module GLUtilsMod
