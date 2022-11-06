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
!#      AnimateGL sample program for Intel Visual Fortran
!#******************************************************************************
!

! Global procedures and variables for AnimateGL sample
!
module AnimateMod

! WindowProc and the routine it calls, Display, need to be in a separate module rather than
! contained procedures, so that Windows can call them from any context.
!

use IFWINA
use IFOPNGL
use GLUtilsMod

implicit none

private

public :: animate, run, hdc, Display, WindowProc

! Global variables
logical :: animate = .TRUE.  ! Rotate triangle while true
logical :: run = .TRUE. ! Keep running while true
integer(HANDLE) :: hDC  ! Device Context

contains

  ! WindowProc that is called by Windows to handle messages for our OpenGL Window
  !
  ! The interface to this is standardized and it must use the STDCALL calling mechanism
  ! on IA-32 architecture systems. (On other platforms, the STDCALL attribute serves to
  ! change the argument passing default to be by-value.)
  !
  integer(LONG) function WindowProc (hWnd, uMsg, wParam, lParam)
  !DEC$ ATTRIBUTES STDCALL :: WindowProc
  integer(HANDLE) :: hWnd    ! Window handle
  integer(UINT) :: uMsg      ! Message
  integer(fWPARAM) :: wParam ! Message parameter
  integer(fLPARAM) :: lParam ! Message parameter
  
  ! Locals
  type(T_PAINTSTRUCT) :: ps
  integer(HANDLE) :: hret
  integer(BOOL) :: bret
  
  WindowProc = FALSE
  
  ! Please refer to MSDN or other Win32 programming documentation for an explanation
  ! of the message types
  select case (uMsg)
  
  case (WM_PAINT)
    call display
    hret = BeginPaint(hWnd, ps)
    hret = EndPaint(hWnd, ps)
    return
    
  case (WM_SIZE)
    call fglViewport (0, 0, int(ibits(lParam,0,16),UINT), int(ibits(lParam,16,16),UINT))
    bret = PostMessage(hWnd, WM_PAINT, 0, 0)
    return 
    
  case (WM_CHAR)
    select case (wParam)
    
      case (ichar('q')) ! q for quit
      run = .false.
      call PostQuitMessage(0)
      return
      case (ichar(' ')) ! Blank
        animate = .NOT. animate
      return
      case default ! Anything else
      return
      end select
    
  case (WM_PALETTECHANGED)
    if (hWnd == ZEXT(wParam,HANDLE)) then
      bret = UnrealizeObject(hPalette) ! hPalette is from GLUtilsMod
      bret = SelectPalette(hDC, hPalette, FALSE)
      bret = RealizePalette(hDC)
      WindowProc = TRUE
      return
    end if
    return 
    
  case (WM_QUERYNEWPALETTE)
    if (hPalette /= NULL) then
      bret = UnrealizeObject(hPalette) ! hPalette is from GLUtilsMod
      bret = SelectPalette(hDC, hPalette, FALSE)
      bret = RealizePalette(hDC)
      WindowProc = TRUE
      return
    end if
    return
        
  case (WM_CLOSE)
    run = .false.
    call PostQuitMessage(0)
    return
    
  end select
    
  ! Not ours to handle  
  WindowProc = DefWindowProc( hWnd, uMsg, wParam, lParam )
  
  end function WindowProc
  
  subroutine display
  ! Makes the OpenGL calls to rotate the triangle
  
  integer(BOOL) :: bret
  
  ! rotate a triangle 
  call fglClear(GL_COLOR_BUFFER_BIT);
  if (animate) &
    call fglRotatef(1.0, 0.0, 0.0, 1.0)
  call fglBegin(GL_TRIANGLES)
  call fglIndexi(1)
  call fglColor3f(1.0, 0.0, 0.0)
  call fglVertex2i(0,  1)
  call fglIndexi(2)
  call fglColor3f(0.0, 1.0, 0.0)
  call fglVertex2i(-1, -1)
  call fglIndexi(3)
  call fglColor3f(0.0, 0.0, 1.0)
  call fglVertex2i(1, -1)
  call fglEnd
  call fglFlush
  bret = SwapBuffers(hDC)	!no-op if singlebuffered
  end subroutine display
end module AnimateMod