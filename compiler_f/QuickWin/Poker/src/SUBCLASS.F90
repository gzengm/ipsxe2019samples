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
!
! POKER -- Video Poker in Fortran with QuickWin
!
!
!==============================================================
!
! Module Subclass		(Windows tricks with the Poker window)
!

module subclass

use ifqwin
use user32
implicit none

private
public SubClassInit

integer(PVOID) FrameProc, ChildProc

contains

subroutine SubclassInit()
integer i
integer(int_ptr_kind()) j
logical l

FrameProc = SetWindowLongPtr( GetHWndQQ(QWIN$FRAMEWINDOW), &
			   GWL_WNDPROC, loc(SubclassFrame) )
j = GetWindowLongPtr( GetHWndQQ(QWIN$FRAMEWINDOW), GWL_STYLE )
j = ior( iand( j, not(WS_THICKFRAME) ), WS_BORDER )
j = iand( j, not(WS_MAXIMIZEBOX) )
i = SetWindowLongPtr( GetHWndQQ(QWIN$FRAMEWINDOW), GWL_STYLE, j )	

l = SetWindowText( GetHwndQQ(QWIN$FRAMEWINDOW), &
		    "Intel(R) Visual Fortran Video Poker"C )

ChildProc = SetWindowLongPtr( GetHWndQQ(0), &
			   GWL_WNDPROC, loc(SubclassChild) )
j = GetWindowLongPtr( GetHWndQQ(0), GWL_STYLE )
j = ior( iand( j, not(WS_CAPTION.or.WS_THICKFRAME.or.WS_SYSMENU) ), WS_BORDER )
i = SetWindowLongPtr( GetHWndQQ(0), GWL_STYLE, j )	

l = MoveWindow( GetHWndQQ(0), -1, -1, 500, 400, TRUE )

end subroutine

recursive integer(4) function SubclassFrame( Hwnd, Msg, wParam, lParam )
!DEC$ attributes stdcall :: SubclassFrame
use pokerregistry
use ifwinty
integer(HANDLE) Hwnd
integer(UINT) Msg
integer(fWPARAM) wParam
integer(fLPARAM) lParam

if( Msg == WM_CLOSE ) then
	call SaveRegistry()	
end if
SubclassFrame = CallWindowProc( FrameProc, Hwnd, Msg, wParam, lParam )
end function

recursive integer(4) function SubclassChild( Hwnd, Msg, wParam, lParam )
!DEC$ attributes stdcall :: SubclassChild
use layout
use ifwinty
integer(HANDLE) Hwnd
integer(UINT) Msg
integer(fWPARAM) wParam
integer(fLPARAM) lParam
integer(LRESULT) i
integer(UINT) newmsg

if( Msg == WM_CHAR ) then
	SubclassChild = 0
	return
else if( Msg == WM_KEYDOWN .or. Msg == WM_KEYUP ) then
	select case( Msg )
		case( WM_KEYDOWN ); newmsg = WM_LBUTTONDOWN
		case( WM_KEYUP );   newmsg = WM_LBUTTONUP
	end select
	if( wParam >= ichar('1') .and. wParam <= ichar('5') ) then
		i = CallWindowProc( ChildProc, Hwnd, newmsg, 0, &
           int(ior(ishl(cardy(int(wParam,UINT)-ichar('0'))+1,16) , &
		     cardx(int(wParam,UINT)-ichar('0'))+1),fLPARAM))
	   	SubclassChild = 0
		return
	else if( wParam == VK_RETURN ) then
	    if( Msg /= WM_CHAR ) &
		i = CallWindowProc( ChildProc, Hwnd, newmsg, 0, &
				    int(ior(ishl(DealButtonY+1,16), &
				         DealButtonX+1 ),fLPARAM))
	    	SubclassChild = 0
	    	return
	end if
end if
SubclassChild = CallWindowProc( ChildProc, Hwnd, Msg, wParam, lParam )
end function

end module

	
