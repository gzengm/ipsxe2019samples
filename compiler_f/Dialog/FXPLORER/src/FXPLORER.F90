!==============================================================
!
! SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
! http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!
! Copyright 2007-2016 Intel Corporation
!
! THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
! NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
!
! =============================================================

! This software FXPLORER.F90 is part of the Intel(R) Visual Fortran Compiler.
!
! This software is provided as an example that demonstrates a particular
! feature/function of the licensed product. The software is not intended to
! provide a complete solution to any application problem but rather is provided
! as a teaching mechanism.
! 
! Users may incorporate any part of this sample into their own source after 
! making appropriate changes to make it suitable for the intended application.
!
! This sample program demonstrates an SDI Fortran Windows application using
! the Visual Fortran Dialog Procedures.  It uses an Internet Explorer ActiveX
! control in the main window of the application. The Internet Explorer control
! is installed with Internet Explorer.  If Internet Explorer is not installed,
! this sample will not run. It is also required that the Intel Visual Fortran DLL
! IFDLG100.DLL be present in PATH and be registered.  This is done when the compiler
! is installed.
!
! The basic structure of this application was generated using the Fortran
! Windows Application project wizard.
!
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!
!  Module FXCALLBACKS has functions called while the program is running

!

!****************************************************************************
!
!  FUNCTION: WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!
!  PURPOSE:  Entry point for the application
!
!  COMMENTS: Displays the main window and processes the message loop
!
!****************************************************************************

integer function WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'WinMain' :: WinMain

    use ifwin
    use ifcom
    use ifauto
    use fxplorerGlobals
    use fxcallbacks

    implicit none

    integer(HANDLE) hInstance
    integer(HANDLE) hPrevInstance
    integer(LPVOID) lpszCmdLine
    integer(UINT) nCmdShow


    ! Variables
    type (T_WNDCLASS)       wc
    type (T_MSG)            mesg
    integer(DWORD)          ret
    integer(BOOL)           lret
    integer(HANDLE)         haccel

    character(SIZEOFAPPNAME) lpszClassName
    character(SIZEOFAPPNAME) lpszIconName
    character(SIZEOFAPPNAME) lpszAppName
    character(SIZEOFAPPNAME) lpszMenuName
    character(SIZEOFAPPNAME) lpszAccelName

    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndFocus = NULL
    call COMINITIALIZE(ret)

    lpszClassName ="fxplorer"C
    lpszAppName ="Fortran Explorer"C
    lpszIconName ="fxplorer"C  
    lpszMenuName ="fxplorer"C
    lpszAccelName ="fxplorer"C

    !  If this is the first instance of the application, register the
    !  window class(es)
    if (hPrevInstance .eq. 0) then
        !  Main window
         wc%lpszClassName = LOC(lpszClassName)
         wc%lpfnWndProc = LOC(MainWndProc)
         wc%style = IOR(CS_VREDRAW , CS_HREDRAW)
         wc%hInstance = hInstance
         wc%hIcon = LoadIcon( hInstance, LOC(lpszIconName))
         wc%hCursor = LoadCursor( NULL, IDC_ARROW )
         wc%hbrBackground = ( COLOR_WINDOW+1 )
         wc%lpszMenuName = NULL
         wc%cbClsExtra = 0
         wc%cbWndExtra = 0
         if (RegisterClass(wc) == 0) goto 99999
    end if

    ! Load the window's menu and accelerators and create the window
    !
    ghMenu = LoadMenu(hInstance, LOC(lpszMenuName))
    if (ghMenu == 0) goto 99999
    ghFileSubMenu = GetSubMenu(ghMenu, 0)
    ghNavigateSubMenu = GetSubMenu(ghMenu, 1)
    ghHelpSubMenu = GetSubMenu(ghMenu, 2)
    haccel = LoadAccelerators(hInstance, LOC(lpszAccelName))
    if (haccel == 0) goto 99999

    ghwndMain = CreateWindowEx(  0, lpszClassName,                 &
                                 lpszAppName,                      &
                                 INT(WS_OVERLAPPEDWINDOW),         &
                                 CW_USEDEFAULT,                    &
                                 0,                                &
                                 CW_USEDEFAULT,                    &
                                 0,                                &
                                 NULL,                             &
                                 ghMenu,                           &
                                 hInstance,                        &
                                 NULL                              &
                              )
    if (ghwndMain == 0) goto 99999

    lret = ShowWindow( ghwndMain, nCmdShow )

    ! Read and process messsages
    do while( GetMessage (mesg, NULL, 0, 0) /= 0) 
       if ( TranslateAccelerator (ghwndMain, haccel, mesg) == 0) then
	       if ( DlgIsDlgMessage(mesg, gdlg) .eqv. .FALSE. ) then
               lret = TranslateMessage( mesg )
               ret  = DispatchMessage( mesg )
           end if
       end if
    end do

    call DlgUninit(gdlg)
    call COMUNINITIALIZE()

    WinMain = mesg.wParam
    return

99999 &

    ret = MessageBox(ghwndMain, "Error initializing application fxplorer"C, &
                     "Error"C, MB_OK)
    call COMUNINITIALIZE()
    WinMain = 0

end function WinMain