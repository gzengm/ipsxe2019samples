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


integer*4 function  WinMain(hInstance, hPrevInstance, lpCmdLine, nCmdShow)
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'WinMain' :: WinMain
use user32
use gdi32
use kernel32
use anginc
implicit none

integer(HANDLE) hInstance
integer(HANDLE) hPrevInstance
integer(LPVOID) lpCmdLine
integer(DWORD) nCmdShow

type (T_MSG)        mesg
type (T_WNDCLASS)   wc
integer(DWORD)           ret
integer(BOOL)           lret
character*100     lpszClassName, lpszMenuName, lpszIconName
integer ierr


! Check for previous instance.  If none, then register class.
    if (hPrevInstance == 0) then
        lpszClassName       = "Angle"C
        lpszIconName        = "AngleIcon"C
        wc%style            = 0
        wc%lpfnWndProc      = LOC(MainWndProc)
        wc%cbClsExtra       = 0
        wc%cbWndExtra       = 0
        wc%hInstance        = hInstance
        wc%hIcon            = LoadIcon(hInstance, LOC(lpszIconName))
        wc%hCursor          = LoadCursor(NULL, IDC_ARROW)
        wc%hbrBackground    = GetStockObject(LTGRAY_BRUSH)
        wc%lpszMenuName     = NULL
        wc%lpszClassName    = LOC(lpszClassName)

        if (RegisterClass(wc) == 0) then
            WinMain = 0
            return
        end if
  end if   !/* class registered o.k. */
! /*  Create the main window.  Return FALSE if CreateWindow() fails */

   hInst = hInstance
   hwndMain =  CreateWindowEx(0,                                 &
                      lpszClassName,                             &
                      'AngleArc'C   ,                            &
                      IOR(WS_OVERLAPPEDWINDOW,WS_CLIPCHILDREN),  &
                      CW_USEDEFAULT,                             &
                      CW_USEDEFAULT,                             &
                      CW_USEDEFAULT,                             &
                      CW_USEDEFAULT,                             &
                      NULL,                                      &
                      NULL,                                      &
                      hInstance,                                 &
                      NULL)

   if (hwndMain == 0) then
      ierr = GetLastError()
      WinMain = 0
      return
   end if

   lret = ShowWindow(hwndMain, nCmdShow)
   lret = UpdateWindow(hwndMain)

   ! /* Loop getting messages and dispatching them. */
   do while (GetMessage(mesg, NULL, 0, 0) .NEQV. .FALSE.)
      if (IsDialogMessage(hwndDlg, mesg) .EQV. .FALSE.) then
         ret = TranslateMessage(mesg)
         ret = DispatchMessage(mesg)
      end if
   end do
   WinMain = mesg%wParam
   return
   
contains

!
!MainWndProc
!
integer*4 function MainWndProc(hwnd, message, wParam, lParam)
!DEC$ ATTRIBUTES STDCALL :: MainWndProc
use user32
use gdi32, LineTo => MSFWIN$LineTo
use ifwbase
use anginc
implicit none

integer(HANDLE)   hwnd, wParam, lParam
integer(UINT) message

type (T_PAINTSTRUCT)  ps
type (T_RECT)         rectp
character*(MAXCHARS)   buffer
integer(HANDLE)   hPenGrid, hPenArc
SAVE              hPenGrid, hPenArc
integer(HANDLE)   hdc, hDlg
SAVE              hDlg

integer           i
integer           x, y, radius
real              start, sweep
integer(BOOL)         result
integer(DWORD)         ret
integer(BOOL)        bret
character*100     lpszEditText
integer(LPVOID) iaddr

select case (message)

!/**********************************************************************\
!*  WM_CREATE
!*
!*  Then create three pens for drawing with later.
!\**********************************************************************/
case (WM_CREATE)
      hPenGrid = CreatePen(PS_SOLID, 1, INT4(GRIDCOLOR))
      hPenArc = CreatePen(PS_SOLID, 2, 16#01000005)
      iaddr = LOC(DlgProc)
      hwndDlg = CreateDialogParam(hInst, INT(100,LPVOID), hwnd, LOC(DlgProc), 0_UINT_PTR)
      hDlg = hwndDlg
      bret = ShowWindow(hwndDlg, SW_SHOW)

!/**********************************************************************\
!*  WM_DESTROY
!*
!* Complement of the WM_CREATE message.  Delete the pens that were
!*  created and then call postquitmessage.
!\**********************************************************************/
  case (WM_DESTROY)
      bret = DeleteObject(hPenGrid)
      bret = DeleteObject(hPenArc)
      call PostQuitMessage(0)


!/**********************************************************************\
!*  WM_SIZE
!*
!* Stretch the top dialog to fill the width of the main window.
!\**********************************************************************/
  case (WM_SIZE)
      bret = SetWindowPos(hDlg, int(NULL,HANDLE), 0, 0, INT(lParam,SINT),   &
           DIALOGHEIGHT, 0)


!/**********************************************************************\
!*  WM_PAINT
!*
!* First shift the viewport origin down so that 0,0 is the top left
!*  most visible point (out from underneath the top dialog).  Second,
!*  draw the grid with wider lines on the axes.  Finally, read the
!*  values out of the top dialog, do elementary validation, and then
!*  try to call AngleArc() with the values.  If a value fails validation,
!*  then write a small error message, and don't draw the arc.
!\**********************************************************************/
    case (WM_PAINT)

        hdc = BeginPaint(hwnd, ps)

        bret = SetViewportOrgEx(hdc, 0, DIALOGHEIGHT, NULL)

        bret = GetClientRect(hwnd, rectp)

        ret = SelectObject(hdc, hPenGrid)
        !/* Draw vertical lines.  */
        do i = 0, rectp%right, TICKSPACE
            bret = MoveToEx(hdc, i, rectp%top, NULL)
            bret = LineTo(hdc, i, rectp%bottom)
        end do

        bret = MoveToEx(hdc, 1, 0, NULL)
        bret = LineTo(hdc, 1, rectp%bottom)

        !/* Draw horizontal lines.  */
        do i = 0, rectp%bottom, TICKSPACE
            bret = MoveToEx(hdc, rectp%left, i, NULL)
            bret = LineTo(hdc, rectp%right, i)
        end do

        bret = MoveToEx(hdc, 0, 1, NULL)
        bret = LineTo(hdc, rectp%right, 1)

        !/* new color pen for the actual arc. */
        ret = SelectObject(hdc, hPenArc)

        !/*
        ! * Query the top dialog parameters, if a value is bad, report that
        ! * and break out of conditional. if all values are good, then set
        ! * the current point and call AngleArc().
        ! */
        if (IsWindow(hDlg) .NEQV. .FALSE.) then
            x = GetDlgItemInt(hDlg, DID_X, LOC(result) ,TRUE)
            if (result .EQV. .FALSE.) then
                lpszEditText = "Bad X"C
                bret = TextOut(hdc, 10, rectp%bottom - 2*DIALOGHEIGHT, lpszEditText, 5)
                goto 1001
            end if
            y = GetDlgItemInt(hDlg, DID_Y, LOC(result), TRUE)
            if (result .EQV. .FALSE.) then
                lpszEditText = "Bad Y"C
                bret = TextOut(hdc, 30, rectp%bottom - 2 * DIALOGHEIGHT, lpszEditText, 5)
                goto 1001
            end if
            radius = GetDlgItemInt(hDlg, DID_RADIUS, LOC(result), TRUE)
            if (result .EQV. .FALSE.) then
                lpszEditText = "Bad Radius"C
                bret = TextOut(hdc, 50, rectp%bottom - 2*DIALOGHEIGHT, lpszEditText, 10)
                goto 1001
            end if
           !/*
           !* Hard to validate these floating point numbers. Good chance
           !* that invalid values will just map to 0.0
           !*/
         if (GetDlgItemText(hDlg, DID_START, buffer, MAXCHARS) == 0) then
               lpszEditText = "Bad Start"C
               bret = TextOut(hdc, 70, rectp%bottom -  2*DIALOGHEIGHT,lpszEditText, 9)
               goto 1001
           end if
           !** chartoreal needs to be done as FORTRAN does a strict
           !** format checking
           start = chartoreal(LOC(buffer))
           if (GetDlgItemText(hDlg, DID_SWEEP, buffer, MAXCHARS) == 0) then
               lpszEditText = "Bad Sweep"C
               bret = TextOut(hdc, 90, INT(rectp%bottom - 2*DIALOGHEIGHT),lpszEditText, 9)
               goto 1001
           end if
           sweep = chartoreal(LOC(buffer))

           bret = MoveToEx(hdc, x, y, NULL)
           bret = AngleArc(hdc, x, y, radius, start, sweep)
        end if
        bret = EndPaint(hwnd, ps)
        MainWndProc = 0
        return
    end select   !/* end switch */

1001  MainWndProc = DefWindowProc(hwnd, message, wParam, lParam)

return
end function MainWndProc

!/**************************************************************************\
!*
!*  function:  DlgProc()
!*
!*  input parameters:  normal window procedure parameters.
!*
!*
!\**************************************************************************/
integer*4 function DlgProc(hwnd, message, wParam, lParam)
!DEC$ ATTRIBUTES STDCALL :: DlgProc
use ifwina
use anginc
integer(HANDLE) hwnd, wParam, lParam
integer(UINT) message
integer(DWORD) ret
integer(BOOL)  bret
integer(HANDLE) hwndParent
type (T_RECT) rectp
character(100)  lpszEditText


    select case (message)

        !/**********************************************************************\
        !*  WM_INITDIALOG
        !*
        !* Fill the entry fields with sensible original values.
        !\**********************************************************************/
    case (WM_INITDIALOG)
        hWndParent = GetParent (hwnd)
        bret = GetClientRect(hwndParent, rectp)
        ret = SendMessage(hwndParent, WM_SIZE, 0_UINT_PTR, int(rectp%right - rectp%left,UINT_PTR))
        bret = InvalidateRect(hwndParent, NULL, FALSE)
        lpszEditText = "100"C
        bret = SetDlgItemText(hwnd, DID_X, lpszEditText)
        lpszEditText = "100"C
        bret = SetDlgItemText(hwnd, DID_Y, lpszEditText)
        lpszEditText = "50"C
        bret = SetDlgItemText(hwnd, DID_RADIUS, lpszEditText)
        lpszEditText = "0.0"C
        bret = SetDlgItemText(hwnd, DID_START, lpszEditText)
        lpszEditText = "270.0"C
        bret = SetDlgItemText(hwnd, DID_SWEEP, lpszEditText)

        DlgProc = 1
        return



        !/**********************************************************************\
        !*  WM_COMMAND, DID_DRAW
        !*
        !* Invalidate the main window so that we force a repaint.
        !\**********************************************************************/
    case (WM_COMMAND)
         if (LOWORD(int(wParam,DWORD)) == DID_DRAW) then
             hWndParent = GetParent (hwnd)
             bret = InvalidateRect(hwndParent, NULL, TRUE)
         end if
         DlgProc = 0
         return
    end select   !/* end switch */
    DlgProc = 0
    return

end function DlgProc


end function WinMain