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
!#      Rings sample program for Intel Visual Fortran
!#******************************************************************************
!

module RingsMod
use ifwin, only: NULL, HANDLE

integer(4), parameter, public ::  XSIZE   = 100
integer(4), parameter, public ::  YSIZE   = 75

integer(4), parameter, public ::  RINGS      = 5
integer(4), parameter, public ::  BLUERING   = 1
integer(4), parameter, public ::  BLACKRING  = 2
integer(4), parameter, public ::  REDRING    = 3
integer(4), parameter, public ::  YELLOWRING = 4
integer(4), parameter, public ::  GREENRING  = 5

real(4), parameter, public ::  BACKGROUND = 8.0

integer(4), parameter, public ::  BLACK   = 0
integer(4), parameter, public ::  RED     = 1
integer(4), parameter, public ::  GREEN   = 2
integer(4), parameter, public ::  YELLOW  = 3
integer(4), parameter, public ::  BLUE    = 4
integer(4), parameter, public ::  MAGENTA = 5
integer(4), parameter, public ::  CYAN    = 6
integer(4), parameter, public ::  WHITE   = 7


integer(2) Point(2)
logical(4) :: rgbmode = .true.

integer(1)  rgb_colors(RINGS, 3)
integer(4)  mapped_colors(RINGS)
real(4)     dests(RINGS,3)
real(4)     offsets(RINGS,3)
real(4)     angs(RINGS)
real(4)     rotAxis(RINGS,3)
integer(4)  iters(RINGS)
integer(4)  theTorus
integer(HANDLE) :: hDC  ! Device Context
logical :: run = .TRUE. ! Keep running while true


contains

  ! WindowProc that is called by Windows to handle messages for our OpenGL Window
  !
  ! The interface to this is standardized and it must use the STDCALL calling mechanism
  ! on IA-32 architecture systems. (On other platforms, the STDCALL attribute serves to
  ! change the argument passing default to be by-value.)
  !
  integer(LONG) function WindowProc (hWnd, uMsg, wParam, lParam)

  use ifwina
  use ifopngl
  use GLUtilsMod

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
    call DrawScene
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
        call ReInit
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

  subroutine DrawScene()

  use ifopngl
  use ifwina

! DrawScene is called in the idle loop.  It gradually moves and 
! rotates the rings towards the desired end position and orientation.
! The offset for each iteration is randomized in Init.
!

   integer(SINT) :: i, j
   integer(BOOL) :: bret

   do i = 1, RINGS
      if (iters(i) > 0) then
         do j = 1, 3
            offsets(i,j) = Clamp(iters(i), offsets(i,j))
         end do
         angs(i) = Clamp(iters(i), angs(i))
         iters(i) = iters(i)-1
      end if
   end do

   call fglPushMatrix()

   call fglClear(IOR(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
   call fgluLookAt(DBLE(0), DBLE(0), DBLE(10), DBLE(0), DBLE(0), DBLE(0), DBLE(0), DBLE(1), DBLE(0))

   do i = 1, RINGS
      if (rgbmode) then
         call fglColor3ubv(LOC(rgb_colors(i,1)))
      else
         call fglIndexi(mapped_colors(i))
      end if
      call fglPushMatrix()
      call fglTranslatef(dests(i,1)+offsets(i,1), dests(i,2)+offsets(i,2),   &
               dests(i,3)+offsets(i,3))
      call fglRotatef(angs(i), rotAxis(i,1), rotAxis(i,2), rotAxis(i,3))
      call fglCallList(theTorus)
      call fglPopMatrix()
   end do

   call fglPopMatrix()

   call fglFlush()

   bret = SwapBuffers(hDC)	! Ignored if single-buffered
   
end subroutine DrawScene



real(4) function MyRand()
    ! Return a random number between -5.0 and +5.0
    real(4) r
    call random_number(r)
    MyRand = 10.0 * ( r  - 0.5)
    return
end function MyRand

subroutine FillTorus (rc, numc, rt, numt)
use ifopngl
real(4)  rc, rt
integer(4)  numc, numt
integer(4)  i, j, k
   real(8) s, t
   real(8) x, y, z
   real(8), parameter ::  twopi = 2.0_8 * 3.14159265358979323846_8

   do i = 0, numc-1
      call fglBegin(GL_QUAD_STRIP)
      do j = 0, numt
         do k = 1, 0, -1
            s = MOD((i + k) , numc) + 0.5
            t = MOD(j , numt)

            x = cos(t*twopi/numt) * cos(s*twopi/numc)
            y = sin(t*twopi/numt) * cos(s*twopi/numc)
            z = sin(s*twopi/numc)
            call fglNormal3f(REAL(x), REAL(y), REAL(z))

            x = (rt + rc * cos(s*twopi/numc)) * cos(t*twopi/numt)
            y = (rt + rc * cos(s*twopi/numc)) * sin(t*twopi/numt)
            z = rc * sin(s*twopi/numc)
            call fglVertex3f(REAL(x), REAL(y), REAL(z))
         end do
      end do
      call fglEnd()
   end do
end subroutine FillTorus

real(4) function Clamp (iters_left, t)
integer(4)  iters_left
real(4) t
   if (iters_left < 3) then
      Clamp = 0.0
      return
   end if
   Clamp = (iters_left-2)*t/iters_left
   return
end function Clamp

subroutine ReInit()

! Re-initialize rings in new positions

   integer(4)   i
   real(4) deviation

   deviation = MyRand() / 2
   deviation = deviation * deviation
   do i = 1, RINGS
      offsets(i,1) = MyRand()
      offsets(i,2) = MyRand()
      offsets(i,3) = MyRand()
      angs(i) = 260.0 * MyRand()
      rotAxis(i,1) = MyRand()
      rotAxis(i,2) = MyRand()
      rotAxis(i,3) = MyRand()
      iters(i) = (deviation * MyRand() + 200.0)
   end do
end subroutine ReInit

subroutine Init()

! Initialize various variables and arrays

use ifopngl

   real(4) base, height
   real(4) aspect, x, y
   integer i
   real(4) top_y
   real(4) bottom_y
   real(4) top_z
   real(4) bottom_z
   real(4) spacing
   real(4) lmodel_ambient(4)
   real(4) lmodel_twoside(1)
   real(4) lmodel_local(1)
   real(4) light0_ambient(4)
   real(4) light0_diffuse(4)
   real(4) light0_position(4)
   real(4) light0_specular(4)
   real(4) bevel_mat_ambient(4)
   real(4) bevel_mat_shininess(1)
   real(4) bevel_mat_specular(4)
   real(4) bevel_mat_diffuse(4)

   DATA top_y /1.0/, bottom_y /0.0/
   DATA top_z /0.15/, bottom_z /0.69/, spacing /2.5/
   DATA lmodel_ambient        /0.0, 0.0, 0.0, 0.0/
   DATA lmodel_twoside        /GL_FALSE/
   DATA lmodel_local          /GL_FALSE/
   DATA light0_ambient        /0.1, 0.1, 0.1, 1.0/
   DATA light0_diffuse        /1.0, 1.0, 1.0, 0.0/
   DATA light0_position       /0.8660254, 0.5, 1, 0/
   DATA light0_specular       /1.0, 1.0, 1.0, 0.0/
   DATA bevel_mat_ambient     /0.0, 0.0, 0.0, 1.0/
   DATA bevel_mat_shininess   /40.0/
   DATA bevel_mat_specular    /1.0, 1.0, 1.0, 0.0/
   DATA bevel_mat_diffuse     /1.0, 0.0, 0.0, 0.0/

   call random_seed
   call ReInit()
   do i = 1, RINGS
      rgb_colors(i,1) = 0
      rgb_colors(i,2) = 0
      rgb_colors(i,3) = 0
   end do

   rgb_colors(BLUERING,3)     = 255
   rgb_colors(REDRING,1)      = 255
   rgb_colors(GREENRING,2)    = 255
   rgb_colors(YELLOWRING,1)   = 255
   rgb_colors(YELLOWRING,2)   = 255
   mapped_colors(BLUERING)    = BLUE
   mapped_colors(REDRING)     = RED
   mapped_colors(GREENRING)   = GREEN
   mapped_colors(YELLOWRING)  = YELLOW
   mapped_colors(BLACKRING)   = BLACK

   dests(BLUERING,1) = -spacing
   dests(BLUERING,2) = top_y
   dests(BLUERING,3) = top_z

   dests(BLACKRING,1) = 0.0
   dests(BLACKRING,2) = top_y
   dests(BLACKRING,3) = top_z

   dests(REDRING,1) = spacing
   dests(REDRING,2) = top_y
   dests(REDRING,3) = top_z

   dests(YELLOWRING,1) = -spacing / 2.0
   dests(YELLOWRING,2) = bottom_y
   dests(YELLOWRING,3) = bottom_z

   dests(GREENRING,1) = spacing / 2.0
   dests(GREENRING,2) = bottom_y
   dests(GREENRING,3) = bottom_z

   base = 2.0
   height = 2.0
   theTorus = fglGenLists(1)
   call fglNewList(theTorus, GL_COMPILE)
   call FillTorus(0.1, 8, 1.0, 25)
   call fglEndList()

   x = real(XSIZE)
   y = real(YSIZE)
   aspect = x / y
   call fglEnable(GL_CULL_FACE)
   call fglCullFace(GL_BACK)
   call fglEnable(GL_DEPTH_TEST)
   call fglClearDepth(DBLE(1.0))

   if (rgbmode) then
      call fglClearColor(0.5, 0.5, 0.5, 0.0)
      call fglLightfv(GL_LIGHT0, GL_AMBIENT, loc(light0_ambient))
      call fglLightfv(GL_LIGHT0, GL_DIFFUSE, loc(light0_diffuse))
      call fglLightfv(GL_LIGHT0, GL_SPECULAR, loc(light0_specular))
      call fglLightfv(GL_LIGHT0, GL_POSITION, loc(light0_position))
      call fglEnable(GL_LIGHT0)

      call fglLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, loc(lmodel_local))
      call fglLightModelfv(GL_LIGHT_MODEL_TWO_SIDE, loc(lmodel_twoside))
      call fglLightModelfv(GL_LIGHT_MODEL_AMBIENT, loc(lmodel_ambient))
      call fglEnable(GL_LIGHTING)

      call fglMaterialfv(GL_FRONT, GL_AMBIENT, loc(bevel_mat_ambient))
      call fglMaterialfv(GL_FRONT, GL_SHININESS, loc(bevel_mat_shininess))
      call fglMaterialfv(GL_FRONT, GL_SPECULAR, loc(bevel_mat_specular))
      call fglMaterialfv(GL_FRONT, GL_DIFFUSE, loc(bevel_mat_diffuse))

      call fglColorMaterial(GL_FRONT_AND_BACK, GL_DIFFUSE)
      call fglEnable(GL_COLOR_MATERIAL)
      call fglShadeModel(GL_SMOOTH)

   else
      call fglClearIndex(BACKGROUND)
      call fglShadeModel(GL_FLAT)
   end if

   call fglMatrixMode(GL_PROJECTION)
   call fgluPerspective(DBLE(45), DBLE(1.33), DBLE(0.1), DBLE(100.0))
   call fglMatrixMode(GL_MODELVIEW)
end subroutine Init


end module RingsMod