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

! Example demonstrating animation of colored rings. 
!
! USAGE:
!
!   rings [-sb] [-ci] [-h]
!
!   -sb : Use single buffering (double buffer default)
!   -ci : Use indexed color palette (default is to use all colors)
!   -h  : Display usage information
!
!   While running, press the space bar to generate a new set of rings, press q to quit.
!
!   By using -sb, you can see the advantage of double-buffering as the single-buffered display
!   is sometimes streaky.  -ci uses at most 256 colors rather than the full color depth
!

! Entry point for Rings
function WinMain (hCurrentInst, hPreviousInst, lpszCmdLine, nCmdShow)
!DEC$ ATTRIBUTES STDCALL,DECORATE,ALIAS:"WinMain" :: WinMain
use IFWINA ! Alternate version of ifwin that renames MSFWIN$ routines that conflict with QuickWin
use IFOPNGL
use GLUtilsMod
use RingsMod

implicit none

integer(SINT) :: WinMain
integer(HANDLE), intent(IN) :: hCurrentInst, hPreviousInst
integer(LPCSTR), intent(IN) :: lpszCmdLine
integer(SINT), intent(IN) :: nCmdShow

! Locals

integer(HANDLE) :: hRC  ! OpenGL Context
integer(HANDLE) :: hWnd ! Window
type(T_MSG) :: msg      ! Message
integer(DWORD) :: buffer  = PFD_DOUBLEBUFFER  ! Buffering type
integer(BYTE) :: color = PFD_TYPE_RGBA  ! Color type
character(100):: CmdLine
integer(SINT) :: ret
integer(HANDLE) :: hret
integer(BOOL) :: bret

WinMain = 0

! Look at command line arguments
call get_command (CmdLine)
if (index(CmdLine, '-sb') > 0) then
  buffer = 0
end if
if (index(CmdLine, '-ci') > 0) then
  color = PFD_TYPE_COLORINDEX
  rgbmode = .false.
end if
if (index(CmdLine, '-h') > 0) then
  ret = MessageBox(NULL, "rings [-ci] [-sb]\n &
                         &  -sb  single buffered\n &
                         &  -ci  color index\n"C,"Usage Help"C,MB_ICONINFORMATION)
  return
end if

! In the past, we would use the GLAUX routines to simplify creation of the window
! and handle common tasks, but Microsoft removed glaux.lib in Visual Studio 2008,
! so we have to do it the "hard way".  CreateOpenGLWindow does most of the "grunt work"
! of opening the window, but we still have to provide a WindowProc and a message loop
!
! See comments in CreateOpenGLWindow (GLUtilsMod.f90) for more details
!
hWnd = CreateOpenGLWindow("Rings - press spacebar to reset, q to exit", 0, 0, 512, 256, color, buffer, WindowProc)
if (hwnd == NULL) return

! Set up the OpenGL context
!
hDC = GetDC(hWnd)
hRC = fwglCreateContext(hDC)
hret = fwglMakeCurrent(hDC, hRC)
bret = ShowWindow(hWND, SW_SHOW)
bret = UpdateWindow(hWnd)
call Init

! Message loop.  We use PeekMessage so we can do the animation while the window is
! not asking to do something else. 
!
mainloop: do while (run)
  if (PeekMessage(msg, hWnd, 0, 0, PM_REMOVE) /= 0) then
    if (msg%message == WM_QUIT) exit mainloop
      bret = TranslateMessage(msg)
      bret = DispatchMessage(msg)
  end if
call DrawScene
call sleep(10) ! sleep for 10ms
end do mainloop

! Quitting - close down

hret = fwglMakeCurrent(NULL,NULL)
ret = ReleaseDC(hDC, hWnd)
hret = fwglDeleteContext(hRC)
bret = DestroyWindow(hWnd)
if (hPalette /= NULL) bret = DeleteObject(hPalette)

return

end function WinMain

 