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

!  Generic.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   MainWndProc()  - processes messages for the main window
!   CenterWindow() - centers one window over another
!   AboutDlgProc() - processes messages for the about box
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

integer*4 function WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'WinMain' :: WinMain

    use user32
    use kernel32
    use GenericGlobals

    implicit none

    interface
        integer function MainWndProc ( hWnd, mesg, wParam, lParam )
        !DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'MainWndProc' :: MainWndProc

        use user32
        integer(HANDLE) hWnd
        integer(UINT) mesg
        integer(UINT_PTR) wParam
        integer(ULONG_PTR) lParam
        end function MainWndProc

    end interface

    integer(HANDLE) hInstance
    integer(HANDLE) hPrevInstance
    integer(LPVOID) lpszCmdLine
    integer(DWORD) nCmdShow

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
    ghwndMain = NULL
    lpszClassName ="Generic"C
    lpszAppName ="Generic"C
    lpszIconName ="Generic"C  
    lpszMenuName ="Generic"C
    lpszAccelName ="Generic"C

    !  If this is the first instance of the application, register the
    !  window class(es)
    if (hPrevInstance .eq. 0) then
        !  Main window
         wc%style = IOR(CS_VREDRAW , CS_HREDRAW)
         wc%lpfnWndProc = LOC(MainWndProc)
         wc%cbClsExtra = 0
         wc%cbWndExtra = 0
         wc%hInstance = hInstance
         wc%hCursor = LoadCursor( NULL, IDC_ARROW )
         wc%hbrBackground = ( COLOR_WINDOW+1 )
         wc%lpszMenuName = NULL
         wc%lpszClassName = LOC(lpszClassName)
         wc%hIcon = LoadIcon( hInstance, LOC(lpszIconName))
         if (RegisterClass(wc) == 0) goto 99999
    end if

    ! Load the window's menu and accelerators and create the window
    !
    ghMenu = LoadMenu(hInstance, LOC(lpszMenuName))
    if (ghMenu == 0) goto 99999
    haccel = LoadAccelerators(hInstance, LOC(lpszAccelName))
    if (haccel == 0) goto 99999

    ghwndMain = CreateWindowEx( 0,   lpszClassName,                &
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
       if ( TranslateAccelerator (mesg%hwnd, haccel, mesg) == 0) then
           lret = TranslateMessage( mesg )
           ret  = DispatchMessage( mesg )
       end if
    end do

    WinMain = mesg.wParam
    return

99999 &

    ret = MessageBox(ghwndMain, "Error initializing application Generic"C, &
                     "Error"C, MB_OK)
    WinMain = 0

    end function WinMain

!****************************************************************************
!
!  FUNCTION: MainWndProc ( hWnd, mesg, wParam, lParam )
!
!  PURPOSE:  Processes messages for the main window
!
!  COMMENTS:
!
!****************************************************************************

integer function MainWndProc ( hWnd, mesg, wParam, lParam )
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'MainWndProc' :: MainWndProc

    use user32
    use GenericGlobals

    implicit none

    interface
        integer*4 function AboutDlgProc( hDlg, message, uParam, lParam )
        !DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'AboutDlgProc' :: AboutDlgProc
        use user32
        integer(HANDLE)     hDlg        ! window handle of the dialog box
        integer(UINT)     message     ! type of message
        integer(UINT)     uParam      ! message-specific information
        integer(UINT)     lParam
        end function aboutdlgproc
    end interface


    integer(HANDLE) hWnd
    integer(UINT) mesg
    integer(UINT_PTR) wParam
    integer(ULONG_PTR) lParam

    include 'resource.fd'

    ! Variables
    integer(DWORD)           ret
    character(SIZEOFAPPNAME)  lpszName, lpszHelpFileName, lpszContents, lpszMessage
    character(SIZEOFAPPNAME)  lpszHeader

    select case ( mesg )

    ! WM_DESTROY: PostQuitMessage() is called 
      case (WM_DESTROY)
          call PostQuitMessage( 0 )
          MainWndProc = 0
          return

    ! WM_COMMAND: user command
      case (WM_COMMAND)
        select case ( IAND(wParam, 16#ffff ) )
 
            case (IDM_EXIT)
                ret = SendMessage( hWnd, WM_CLOSE, 0_UINT_PTR, 0_LONG_PTR )
                MainWndProc = 0
                return
  
            case (IDM_ABOUT)
                lpszName = "AboutDlg"C
                ret = DialogBoxParam(ghInstance,LOC(lpszName),hWnd,& 
                  LOC(AboutDlgProc), 0_ULONG_PTR)
                MainWndProc = 0
                return

            ! All of the other possible menu options are currently disabled

            case DEFAULT
                MainWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )
                return
        end select

    ! Let the default window proc handle all other messages
      case default
          MainWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )

    end select

end function MainWndProc

 
!/****************************************************************************
!
!  FUNCTION: AboutDlgProc(HWND, UINT, WPARAM, LPARAM)
!
!  PURPOSE:  Processes messages for "About" dialog box
!
!  COMMENTS: Display version information from the version section of the
!            application resource.  Wait for user to click on "Ok" button,
!            then close the dialog box.
!
!****************************************************************************/

integer*4 function AboutDlgProc( hDlg, message, uParam, lParam )
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'AboutDlgProc' :: AboutDlgProc

    use kernel32
    use user32
    use gdi32
    use version
    use GenericGlobals

    implicit none

    integer(HANDLE)     hDlg        ! window handle of the dialog box
    integer(UINT)     message     ! type of message
    integer(UINT)     uParam      ! message-specific information
    integer(UINT)     lParam

    include 'resource.fd'

    ! Variables
    integer(HANDLE) :: hfontDlg = 0
    save        hfontDlg

    integer(DWORD)    dwVerHnd
    integer(DWORD)    dwVerInfoSize
    integer(UINT)     uVersionLen
    integer(BOOL)     bRetCode
    character(MAX_PATH)   szFullPath
    character(256)   szResult
    character(256)   szGetName
    character(256)   versioninfo
    pointer (p_versioninfo, versioninfo)

    integer(LPVOID)  lpstrVffInfo
    integer(DWORD)   ret
    integer i

    select case (message)
      case (WM_INITDIALOG)   ! message: initialize dialog box
         ! Create a font to use
         if (hfontDlg == 0) &
           hfontDlg = CreateFont(14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,& 
                        IOR(INT(VARIABLE_PITCH) , INT(FF_SWISS)), ""C)

         ! Center the dialog over the application window
         call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

         ! Get version information from the application
         ret = GetModuleFileName (ghInstance, szFullPath,     &
                               len(szFullPath))
         dwVerInfoSize = GetFileVersionInfoSize(szFullPath,   &
                                       LOC(dwVerHnd))

         if (dwVerInfoSize .NE. 0) then
            ! If we were able to get the information, process it:
            ! Allocate memory for the version information
            lpstrVffInfo = malloc(dwVerInfoSize)
            ret = GetFileVersionInfo (szFullPath, dwVerHnd, &
            int(dwVerInfoSize,DWORD), lpstrVffInfo)

            ! Walk through the dialog items that we want to replace:
            do i = IDC_VER1, IDC_VER5
               ret = GetDlgItemText(hDlg, i, szResult,      &     
                             len(szResult))
                
               ! Construct the value name.  Since szResult has a NUL
               ! on the end as coming from GetDlgItemText, szGetName
               ! will too.
               szGetName = "\\StringFileInfo\040904E4\\" // szResult

               bRetCode  =  VerQueryValue(lpstrVffInfo,       &
                                            szGetName,       &
                                            LOC(p_versioninfo),       &
                                            LOC(uVersionLen))

               if ( bRetCode .NE. 0 ) then
                  ! Replace dialog item text with version info
                  ret = lstrcpy(szResult,versioninfo)
                  ret = SetDlgItemText(hDlg, i,szResult)
                  ret = SendMessage (GetDlgItem (hDlg, i),   &
                                   WM_SETFONT, hfontDlg, int(TRUE,LONG_PTR))
               end if
            end do 

            call free(lpstrVffInfo)
         end if 
         AboutDlgProc = 1
         return
      case (WM_COMMAND)                      ! message: received a command
         if ((IAND(uParam,16#ffff) .EQ. IDOK) & !OK Selected?
            .OR. (IAND(uParam,16#ffff) .EQ. IDCANCEL)) then ! Close command?
            ret = EndDialog(hDlg, int(TRUE,INT_PTR))      ! Exit the dialog
            ret = DeleteObject (hfontDlg)
            AboutDlgProc = 1
            return
         end if
    end select  
    AboutDlgProc = 0 ! Didn't process the message
    return

    contains

!****************************************************************************
!
!  FUNCTION: CenterWindow (HWND, HWND)
!
!  PURPOSE:  Center one window over another
!
!  COMMENTS: Dialog boxes take on the screen position that they were designed
!            at, which is not always appropriate. Centering the dialog over a
!            particular window usually results in a better position.
!
!****************************************************************************

subroutine CenterWindow (hwndChild, hwndParent)

    use user32
    use gdi32 
    use GenericGlobals

    implicit none

    integer(HANDLE)         hwndChild, hwndParent

    include 'resource.fd'

    ! Variables
    type (T_RECT)   rChild, rParent
    integer         wChild, hChild, wParent, hParent
    integer         wScreen, hScreen, xNew, yNew
    integer(HANDLE)         hdc
    integer(BOOL)       retval

    ! Get the Height and Width of the child window
       retval = GetWindowRect (hwndChild, rChild)
       wChild = rChild.right - rChild.left
       hChild = rChild.bottom - rChild.top

    ! Get the Height and Width of the parent window
       retval = GetWindowRect (hwndParent, rParent)
       wParent = rParent.right - rParent.left
       hParent = rParent.bottom - rParent.top

    ! Get the display limits
       hdc = GetDC (hwndChild)
       wScreen = GetDeviceCaps (hdc, HORZRES)
       hScreen = GetDeviceCaps (hdc, VERTRES)
       retval = ReleaseDC (hwndChild, hdc)

    ! Calculate new X position, then adjust for screen
       xNew = rParent.left + ((wParent - wChild) /2)
       if (xNew .LT. 0) then
          xNew = 0
       else if ((xNew+wChild) .GT. wScreen) then
          xNew = wScreen - wChild
       end if

    ! Calculate new Y position, then adjust for screen
       yNew = rParent.top  + ((hParent - hChild) /2)
       if (yNew .LT. 0) then
          yNew = 0
       else if ((yNew+hChild) .GT. hScreen) then
          yNew = hScreen - hChild
       end if

    ! Set it, and return
       retval = SetWindowPos (hwndChild, NULL, xNew, yNew, 0, 0,      &
                      IOR(SWP_NOSIZE , SWP_NOZORDER))
end subroutine CenterWindow

end function AboutDlgProc
