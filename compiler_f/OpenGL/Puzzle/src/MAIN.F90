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
!#      Puzzle - Cube sample program for Intel Visual Fortran
!#******************************************************************************
!

integer function WinMain( hInstance, hPrevInstance, lpszCmdParam, nCmdShow )
!DEC$ attributes stdcall, decorate, alias: 'WinMain' :: WinMain
    use ifwin
    use sharedata
	use ifport, only:sleepqq
    implicit none
    integer(HANDLE) hInstance
    integer(HANDLE) hPrevInstance
    integer(LPVOID) lpszCmdParam
    integer(DWORD) nCmdShow
    integer WndProc
    external WndProc
!DEC$ attributes stdcall :: WndProc
    integer(HANDLE) hwnd
    integer(HANDLE) hAccel
    integer ignor
    integer wsflag

    type (t_MSG) msg
    type (t_WNDCLASS) wndclass


    character szApp*9
    character title*40
    character menu*12, accel*12, icon*10

    szApp = 'CubeWin'C
    title     = 'The CUBE -- In Intel Visual Fortran'C
    menu      = 'cubeMenu'C
    accel     = 'cubeAccel'C
    icon      = 'cubeIcon'C
    call initShareData()

    if (hPrevInstance .eq. 0) then
	wndclass%style          = ior(CS_HREDRAW, CS_VREDRAW)
	wndclass%lpfnWndProc    = loc(WndProc)
	wndclass%cbClsExtra     = 0
	wndclass%cbWndExtra     = 0
	wndclass%hInstance      = hInstance
	wndclass%hIcon          = LoadIcon(hInstance, loc(icon))
!	wndclass%hIcon          = LoadIcon(NULL, IDI_APPLICATION())
	wndclass%hCursor        = LoadCursor(NULL, IDC_ARROW)
	wndclass%hbrBackground  = GetStockObject(GRAY_BRUSH)
	wndclass%lpszMenuName   = loc(menu)
	wndclass%lpszClassName  = loc(szApp)
	ignor = RegisterClass(wndclass)  ! register window class first
    end if
    wsflag = or(or(WS_OVERLAPPEDWINDOW, WS_CLIPCHILDREN),WS_CLIPSIBLINGS)
    hwnd = CreateWindowEx( &
                    0, & ! Style
                    szApp,       &  !
                    TITLE,           &
                    wsflag,               &
                    CW_USEDEFAULT,        &
                    CW_USEDEFAULT,        &
                    500,        &
                    500,        &
                    NULL,                 &
                    NULL,                 &
                    hInstance,            &
                    NULL)

    ignor = ShowWindow(hwnd, nCmdShow)           ! show windows
    ignor = UpdateWindow(hwnd)
    hAccel = LoadAccelerators(hInstance, loc(accel))

    do while (GetMessage(msg, NULL, 0, 0))
	if (TranslateAccelerator(hwnd, hAccel, msg) .eq. 0) then
		ignor = TranslateMessage(msg)
		ignor = DispatchMessage(msg)
	endif
    end do
    WinMain = msg.wParam
    return
end function WinMain


! -----------------------------------------------------------
integer function WndProc( hwnd, message, wParam, lParam )
!DEC$ attributes stdcall :: WndProc
    use ifwina
    use ifopngl
    use sharedata
    implicit none

    integer(HANDLE) hwnd
    integer message
    integer(fWPARAM) wParam
    integer(fLPARAM) lParam

    integer(HANDLE), save:: hDC, hRC
    integer ignor, nColors
    integer glnWidth, glnLength

    type (t_PAINTSTRUCT) ps

    select case (message)
    case (WM_CREATE)
    hDC = GetDC(hwnd)
	call SetDCPixelFormat(hDC)
	hRC = fwglCreateContext(hDC)
    ignor = fwglMakeCurrent(hDC, hRC)
	call cubeInit()
	call newCubes(hDC)
	WndProc = 0
	return
    case (WM_SIZE)
	if (lparam == 0) then
	  WndProc = 0
	  return
	  end if
	glnWidth = and(lParam, #0000FFFF)  !  low word
	glnLength = ishft(lParam, -16)     !  high word
	call reshape(glnWidth, glnLength)
    hDC = GetDC(hwnd)
	call reCube(hDC)
	WndProc = 0
	return
    case (WM_PAINT)
	hDC = BeginPaint(hwnd, ps)
	call reCube(hDC)
	ignor = EndPaint(hwnd, ps)
	WndProc = 0
	return
    case (WM_QUERYNEWPALETTE)
	if (g_hPalette .ne. 0) then
	    hDC = GetDC(hwnd)
	    ignor = SelectPalette(hDC, g_hPalette, FALSE)
	    nColors = RealizePalette(hDC)
	    if (nColors) then
		ignor = InvalidateRect(hwnd, NULL, FALSE)
	    endif
	    ignor = ReleaseDC(hwnd, hDC)
	    WndProc = nColors
	    return
	endif
    case (WM_PALETTECHANGED)
	if (g_hPalette .ne. 0 .and. wParam .ne. hwnd) then
	    hDC = GetDC(hwnd)
	    ignor = SelectPalette(hDC, g_hPalette, FALSE)
	    nColors = RealizePalette(hDC)
	    if (nColors) then
		ignor = UpdateColors(hDC)
	    endif
	    ignor = ReleaseDC(hwnd, hDC)
	    WndProc = 0
	    return
	endif
    case(WM_TIMER)
	hDC = GetDC(hwnd)
	if (is_reset) then
	  call randomRotate(hDC)
	else
	  call undoCmdStack(hDC)
	end if
	is_reset = .not. is_reset
	ignor = SetTimer(hwnd,1,2000,NULL)
	WndProc = 0
	return
    case (WM_COMMAND)
	call Commands(hwnd, hRc, wParam, lParam)
	WndProc = 0
	return
    case (WM_DESTROY)
        ignor = fwglMakeCurrent(0, 0);
    !	ignor = wglDeleteContext(hRC)
	ignor = ReleaseDC(hwnd, hdc)
	call PostQuitMessage(0)
	WndProc = 0
	return
    end select

    WndProc = DefWindowProc(hwnd, message, wParam, lParam)
    return
end function


! ----------  Menu Command Handle routine ---------------
subroutine Commands(hwnd, hRC, wParam, lParam)
    use ifopngl
    use ifwin
    use sharedata
    integer(HANDLE) hwnd, hRC
    integer(fWPARAM) wParam
    integer(fLPARAM) lParam
    integer cmd
    integer(HANDLE), save:: hDC
	integer ignor
	logical,save :: is_continuous = .false.
    type(t_RECT) rt
!    external cube

    wParam = and(wParam, #0000ffff)
    select case (wParam)
    case (10)    ! New cube
	hDC = GetDC(hwnd)
	call newCubes(hDC)
	return
    case (11)    ! Random cube
	hDC = GetDC(hwnd)
	call randomRotate(hDC)
	return
    case (12)    ! Recover
	hDC = GetDC(hwnd)
	call undoCmdStack(hDC)
	return
    case (13)    ! Exit
	call PostQuitMessage(0)
	return
	case(14)	 ! Continuous
	hDC = GetDC(hwnd)
	call newCubes(hDC)
	is_reset = .true.
	if (is_continuous) then
	  ! Kill timer
	  ignor = KillTimer(hwnd,1)
	else
	  ! Set timer
	  ignor = SetTimer(hwnd,1,500,NULL)
	end if
	is_continuous = .not. is_continuous
	return
    case (200)    ! Speed - slow
	g_vSpeed = 5.0
	return
    case (201)    ! Speed - medium
	g_vSpeed = 10.0
	return
    case (202)    ! Speed - fast
	g_vSpeed = 15.0
	return
    case (21)    ! X. Rotate
	hDC = GetDC(hwnd)
	call RotateCont(hDC, 1.0, 0.0, 0.0, 180.0, g_vSpeed)
	return
    case (22)    ! Y. Rotate
	hDC = GetDC(hwnd)
	call RotateCont(hDC, 0.0, 1.0, 0.0, 180.0, g_vSpeed)
	return
    case (23)    ! Z. Rotate
	hDC = GetDC(hwnd)
	call RotateCont(hDC, 0.0, 0.0, 1.0, 180.0, g_vSpeed)
	return
    case (240)    ! Step - small
	g_vStep = 10.0
	return
    case (241)    ! Step - medium
	g_vStep = 20.0
	return
    case (242)    ! Step - fast
	g_vStep = 30.0
	return
    case (25)    ! X. Rotate/Step
	hDC = GetDC(hwnd)
	call RotateStep(hDC, 1.0, 0.0, 0.0, g_vStep)
	return
    case (26)    ! Y. Rotate/Step
	hDC = GetDC(hwnd)
	call RotateStep(hDC, 0.0, 1.0, 0.0, g_vStep)
	return
    case (27)    ! Z. Rotate/Step
	hDC = GetDC(hwnd)
	call RotateStep(hDC, 0.0, 0.0, 1.0, g_vStep)
	return
    case (30)    ! Size -- Smaller
	hDC = GetDC(hwnd)
	g_vAngle = g_vAngle + 2
	if (g_vAngle .ge. 180) then
	    g_vAngle = 90
	endif
	ignor = GetClientRect(hwnd, rt)
	call reshape(rt%right - rt%left, rt%bottom - rt%top)
	call reCube(hDC)
	return
    case (31)    ! Size -- Larger
	hDC = GetDC(hwnd)
	if (g_vAngle .le. 5) then
	    g_vAngle = 5
	endif
	g_vAngle = g_vAngle - 2
	ignor = GetClientRect(hwnd, rt)
	call reshape(rt%right - rt%left, rt%bottom - rt%top)
	call reCube(hDC)
	return
    case (32)    ! Cube Size -- Smaller
	hDC = GetDC(hwnd)
	g_cSize = g_cSize - 0.04
	if (g_cSize .le. 0.08) then
	    g_cSize = 0.08
	endif
	call CreateCubeDrawList()
	call reCube(hDC)
	return
    case (33)    ! Cube Size -- Larger
	hDC = GetDC(hwnd)
	g_cSize = g_cSize + 0.04
	if (g_cSize .ge. 1.48) then
	    g_cSize = 1.48
	endif
	call CreateCubeDrawList()
	call reCube(hDC)
	return
    case (34)    ! Cube Size -- Smallest
	hDC = GetDC(hwnd)
	g_cSize = 0.08
	call CreateCubeDrawList()
	call reCube(hDC)
	return
    case (35)    ! Cube Size -- Largest
	hDC = GetDC(hwnd)
	g_cSize = 1.48
	call CreateCubeDrawList()
	call reCube(hDC)
	return
    case (40)    ! Axis Flag
	hDC = GetDC(hwnd)
	g_axisOn = mod(g_axisOn+1, 2)
	call reCube(hDC)
	return
    case (51)   ! Speed slower
	g_rSpeed = g_rSpeed -1
	if (g_rSpeed .le. 0) then
	    g_rSpeed = 1
	endif
    case (52)   ! Speed faster
	g_rSpeed = g_rSpeed + 1
	if (g_rSpeed .ge. 6) then
	    g_rSpeed = 6
	endif
	return
    case (53)   ! Speed slowest
	g_rSpeed = 1
	return
    case (54)   ! Speed fastest
	g_rSpeed = 6
	return
    case (500)  ! Undo rotate
	hDC = GetDC(hwnd)
	call popCmd(cmd)
	cmd = ieor(cmd, G_ROTATE_CW)
	call rotateCommander(hDC, cmd)
	return
    case (512:590)   ! Rotate one plane
	hDC = GetDC(hwnd)
	call rotateAndPush(hDC, INT(wParam,KIND(0)))
	return
    end select
	return
end


! ---------- Setup Pixel Format for a Display Context ----------
subroutine SetDCPixelFormat(hDC)
    use ifwina
    use sharedata
    implicit none
    integer(HANDLE) hDC
    integer ignor, ipfd
    parameter (ipfd=or(or(PFD_DRAW_TO_WINDOW,PFD_SUPPORT_OPENGL), &
			  PFD_DOUBLEBUFFER))
    type (t_PIXELFORMATDESCRIPTOR)::pfd = t_PIXELFORMATDESCRIPTOR( &
	40,			&	! size of this pfd
	1,			&	! version number
	ipfd,                   &
	PFD_TYPE_RGBA,		&	! RGBA type
	24,			&	! 24-bit color depth
	0, 0, 0, 0, 0, 0,	&	! color bits ignord
	0,			&	! no alpha buffer
	0,			&	! shift bit ignord
	0,			&	! no accumulation buffer
	0, 0, 0, 0, 		&	! accum bits ignord
	32,			&	! 32-bit z-buffer	
	0,			&	! no stencil buffer
	0,			&	! no auxiliary buffer
	PFD_MAIN_PLANE,		&	! main layer
	0,			&	! reserved
	0, 0, 0 )			! layer masks ignord

    integer nPixelFormat, bStatus;
    character*28  str1, str2
    character*7 err
    type(t_LOGPALETTE) :: palette
	pointer( palette_ptr, palette )
    integer  redMask, greenMask, blueMask, nColors, i

    str1 = 'ChoosePixelFormat failed'C
    str2 = 'SetPixelFormat failed'C
    err = 'Error'C

    nPixelFormat = ChoosePixelFormat(hDC, pfd)
    if (nPixelFormat .eq. 0) then
        ignor = MessageBox(0, str1, err, 0);
        return
    endif

    bStatus = SetPixelFormat(hDC, nPixelFormat, pfd)
    if (bStatus .eq. 0) then
        ignor = MessageBox(0, str2, err, 0);
        return
    endif

    ignor = DescribePixelFormat(hDC, nPixelFormat, 40, pfd)
    if (and(pfd%dwFlags, PFD_NEED_PALETTE) .ne. 0) then
	nColors = ishft(1_4, pfd%cColorBits)
	palette_ptr = malloc(4+nColors*4)
	palette%palVersion = #300
	palette%palNumEntries = nColors
	
	redMask = ishft(1_4, pfd%cRedBits) -1
	greenMask = ishft(1_4, pfd%cGreenBits) -1
	blueMask = ishft(1_4, pfd%cBlueBits) -1
	do i=0, nColors-1
	    palette%palPalEntry(i+1)%peRed = &
		 (and(ishft(i, -pfd%cRedShift), redMask) * 255) / redMask
	    palette%palPalEntry(i+1)%peGreen = &
		 (and(ishft(i, -pfd%cGreenShift),greenMask) * 255) / greenMask
	    palette%palPalEntry(i+1)%peBlue = &
		 (and(ishft(i, -pfd%cBlueShift), blueMask)*255) / blueMask
	    palette%palPalEntry(i+1)%peFlags = 0
	enddo
	g_hPalette = CreatePalette(palette)
	call free(palette_ptr)

	if (g_hPalette .ne. 0) Then
	    ignor = SelectPalette(hDC, g_hPalette, FALSE)
	    nColors = RealizePalette(hDC)
	endif
    endif
    return
end

! ---------------------------------------
subroutine initShareData()
    use sharedata
    g_vAngle  = 25.0_8
    g_rSpeed  = 4
    g_vSpeed  = 10.0
    g_vStep  = 10.0
    g_cSize   = 1.48
    g_hPalette = 0
end
