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

module poker

use layout
use cards
use scoring
use buttonmod
use kernel32
use pokerregistry
use subclass
use ifqwin
use ifport, only: sleepqq
!DEC$ if defined( TESTHAND )
use testhand
!DEC$ endif

logical :: hold(5)
logical :: holdsallowed = .false.
integer(HANDLE) :: ButtonSem
character*80 currentmsg

type( button ) DealButton

contains

subroutine PokerStart()
implicit none
type(deck) PokerDeck
type(hand) PokerHand

record /windowconfig/ wc

integer(2) Win1, Win2
integer i, ii
logical firsthand, firsthand2

i = clickqq( QWIN$STATUS )

wc.numxpixels = WindowSizeX
wc.numypixels = WindowSizeY
wc.numtextcols = -1
wc.numtextrows = -1
wc.numcolors = -1
wc.fontsize = -1
wc.title = '5 Card Draw'C
if( .not. setwindowconfig( wc ) ) then
	stop
endif

call SubclassInit()

i = SetBkColor( 2 )
call ClearScreen( $GCLEARSCREEN )

i = setexitqq( QWIN$EXITNOPERSIST )

i = initializefonts()

ButtonSem = CreateSemaphore( NULL, 0, 1, NULL )

call drawlockinit()
call random_seed()
call cardinit()
call labelredraw()
call displaybet()
call betchange(bet,.false.)
call displaycredits()
call ButtonInit(DealButton,DealButtonX,DealButtonY, &
		DealButtonXSize,DealButtonYSize,"Deal Hand")

do ii = 1, 5
	call CardDraw( CardBlank, cardx(ii), cardy(ii) )
end do

i = registermouseevent( 0, MOUSE$LBUTTONDOWN.or.MOUSE$LBUTTONUP &
			   .or.MOUSE$RBUTTONDOWN, MouseEvent )

Win2 = -1
firsthand = .true.; firsthand2 = .true.
do
	! Wait for Deal Hand
	currentmsg = "Press 'Deal Hand' to place your bet and draw 5 cards"
	if( RegMessage .or. firsthand ) then
            call message(currentmsg)
	endif
	call DrawLock()
	call ButtonWord( DealButton, "Deal Hand" )
	call DrawUnlock()
	HoldsAllowed = .false.

	! Re-enable bet and game changes after hand is over
	ii = ModifyMenuFlagsQQ( 2, 1, $MENUENABLED )
	ii = ModifyMenuFlagsQQ( 2, 2, $MENUENABLED )
	ii = ModifyMenuFlagsQQ( 2, 3, $MENUENABLED )
	ii = ModifyMenuFlagsQQ( 2, 4, $MENUENABLED )
	ii = ModifyMenuFlagsQQ( 2, 5, $MENUENABLED )
	ii = ModifyMenuFlagsQQ( 1, 1, $MENUENABLED )

	! Check to see if any credits left to bet, loop until there is
	do
	ii = WaitForSingleObject( ButtonSem, -1 )
		if( Credits < Bet ) then
			currentmsg = "You need more Credits; select Credits/Deposit"
			call message( currentmsg )
		else
			exit
		end if
	end do
	call changecredits( int2(-bet) )
	call message(" ")
	call WinnerSign( .false., .false. )

	! Disable bet and game changes during hand
	ii = ModifyMenuFlagsQQ( 2, 1, $MENUGRAYED )
	ii = ModifyMenuFlagsQQ( 2, 2, $MENUGRAYED )
	ii = ModifyMenuFlagsQQ( 2, 3, $MENUGRAYED )
	ii = ModifyMenuFlagsQQ( 2, 4, $MENUGRAYED )
	ii = ModifyMenuFlagsQQ( 2, 5, $MENUGRAYED )
	ii = ModifyMenuFlagsQQ( 1, 1, $MENUGRAYED )
	if( Win2 /= -1 ) then
		call DrawLabel( Win2, 15_2 )
	end if
	hold = .false.	
	HoldsAllowed = .true.
	
	! Deal Hand
	call DrawLock()
	ii = setcolor(2)
	ii = rectangle( $GFILLINTERIOR, &
			cardx(1),cardy(1)+CardSizeY+1_2, &
			cardx(6),cardy(1)+CardSizeY+40_2 )
	do ii = 1, 5
		call CardDraw( CardBlank, cardx(ii), cardy(ii) )
	end do
	call DrawUnlock()
	call sleepqq(150)
	call CardDeckShuffle( PokerDeck )
	do ii = 1, 5
!DEC$ if defined(TESTHAND)
		PokerHand%Cards(ii) = testcards(testtop)
		testtop = testtop + 1
		if( testtop > ubound(testcards,1) ) testtop = 1
!DEC$ else
		PokerHand%Cards(ii) = CardDeckDraw( PokerDeck )
!DEC$ endif
	    call DrawLock()
	    call CardDraw( PokerHand%Cards(ii), cardx(ii), cardy(ii) )
	    call DrawUnlock()
	    call sleepqq(150)
	end do
	Win1 = FindHighestHand( PokerHand, .false. )

	! Allow Holds, and wait for Draw Cards
	call DrawLock()
	call ButtonWord( DealButton, "Draw Cards")
	call DrawUnlock()
        currentmsg = "Select cards to hold; press 'Draw Cards' when finished"
	if( RegMessage .or. firsthand ) then
		call message(currentmsg)
	endif
	ii = WaitForSingleObject( ButtonSem, -1 )
	call message(" ")
	HoldsAllowed = .false.
	
	! Draw more cards
	do ii = 1, 5
	    if( .not. hold(ii) ) then
	        call DrawLock()
		call CardDraw( CardBlank, cardx(ii), cardy(ii) )
	        call DrawUnlock()
	    endif
	end do
	do ii = 1, 5
	    if( .not. hold(ii) ) then
		PokerHand%Cards(ii) = CardDeckDraw( PokerDeck )
	        call DrawLock()
		call CardDraw( PokerHand%Cards(ii), cardx(ii), cardy(ii) )
	        call DrawUnlock()
		call sleepqq(150)
	     end if
	end do

	! Score, and update credits
	Win2 = FindHighestHand( PokerHand, .true. )
	if( (Win2 /= Win1) .and. (Win1 /= -1) ) then
		call DrawLabel( Win1, 15_2 )
	end if
	call WinnerSign( .true., Win2 /= -1 )

	firsthand = .false.; firsthand2 = .false.
end do
end subroutine

subroutine Message( str )
character*(*) str
type(xycoord) s
integer j

call DrawLock()

j = setcolor(2)
j = rectangle($GFILLINTERIOR,1,MessageY,WindowSizeX,MessageY+MessageYSize)
j = setcolor( 10 )
j = setfont("t'Times New Roman'h20i" )
call moveto( (WindowSizeX-getgtextextent(str(1:len_trim(str))))/2, MessageY, s )
call outgtext( str(1:len_trim(str)) )

call DrawUnlock()

end subroutine

subroutine WinnerSign( show, type )
logical show, type
type(xycoord) s
integer j
character*20 str

call DrawLock()

j = setcolor(2)
j = rectangle($GFILLINTERIOR,WinnerX,WinnerY,&
		WinnerX+WinnerXSize,WinnerY+WinnerYSize)
if( show ) then
	if( type ) then
		str = "Winner!"
		j = setcolor( 14 )
		j = setfont("t'Arial'h29e" )
	call moveto( WinnerX+ &
			(WinnerXSize-getgtextextent(str(1:len_trim(str))))/2, &
			WinnerY, s )
	else
		str = "Bet Lost"
		j = setcolor( 10 )
		j = setfont("t'Arial'h24i" )
	call moveto( WinnerX+ &
			(WinnerXSize-getgtextextent(str(1:len_trim(str))))/2, &
			WinnerY+3, s )
	end if

	call outgtext( str(1:len_trim(str)) )
endif

call DrawUnlock()

end subroutine

subroutine MouseEvent( unit, me, ks, mX, mY )
use cards
integer*4 unit, me, ks, mx, my
type(xycoord) s
logical, save :: ButtonDown = .false.

integer cardseen, i

if( me == MOUSE$RBUTTONDOWN ) then
	i = ReleaseSemaphore( ButtonSem, 1, 0 )
	return
else if( me == MOUSE$LBUTTONDOWN .and. ButtonHit(DealButton,mX,mY) ) then
	ButtonDown = .true.	
	call DrawLock()
	call ButtonPress( DealButton,.true. )
	call DrawUnlock()
	i = ReleaseSemaphore( ButtonSem, 1, 0 )
	return
else if( me == MOUSE$LBUTTONUP ) then
	if( ButtonDown ) then
		call DrawLock()
		call ButtonPress( DealButton,.false. )
		call DrawUnlock()
		ButtonDown = .false.
	endif
	return
else if( .not. HoldsAllowed ) then
	return
endif

cardseen = 0
do i = 1, 5
	if( mx >= cardx(i) .and. mx <= cardx(i)+CardSizeX .and. &
	    my >= cardy(i) .and. my <= cardy(i)+CardSizeY ) then
		cardseen = i
		exit
	end if
end do

if( cardseen > 0 ) then
	call DrawLock()
	hold(cardseen) = .not. hold(cardseen)
	if( hold(cardseen) ) then
		i = setcolor(15)
		i = setfont("t'Arial'h28e")
		call moveto( cardx(cardseen)+4, &
			     cardy(cardseen)+CardSizeY+12,s )
		call outgtext( 'HOLD' )
		call moveto( cardx(cardseen)+3, &
			     cardy(cardseen)+CardSizeY+11,s )
		i = setcolor(9)
		call outgtext( 'HOLD' )
	else
		i = setcolor(2)
		i = rectangle( $GFILLINTERIOR, cardx(cardseen)+3_2, &
	 	       cardy(cardseen)+11_2+CardSizeY, &
		       cardx(cardseen)+3_2+CardSizeX, &
		       cardy(cardseen)+11_2+CardSizeY+30_2 )
	endif
	call DrawUnlock()
end if
return; call unusedqq( unit, ks )
end subroutine

subroutine MsgChange( l )
logical l
RegMessage = .not. RegMessage
if( RegMessage ) then
	j = ModifyMenuFlagsQQ( 4, 3, $MENUCHECKED.or.$MENUENABLED )
	call message(currentmsg)
else
	j = ModifyMenuFlagsQQ( 4, 3, $MENUUNCHECKED.or.$MENUENABLED )
	call message(" ")
endif
firsthand = .false.
return; call unusedqq( l )
end subroutine

subroutine PokerEnd( l )
logical l
call SaveRegistry()
call exit(0)
return; call unusedqq( l )
end subroutine

subroutine AboutPoker( ll )
logical ll, l
integer i
type(dialog) AboutDlg
include 'resource.fd'
l = DlgInit( IDD_ABOUT, AboutDlg )
i = DlgModal( AboutDlg )
call DlgUninit( AboutDlg )
return; call unusedqq( ll )
end subroutine

subroutine HelpContents( ll )
logical ll, l
integer i
type(dialog) AboutDlg
include 'resource.fd'
l = DlgInit( IDD_HELPCONTENTS, AboutDlg )
l = DlgSetSub( AboutDlg, IDC_HELPSCORING, HelpSecond )
l = DlgSetSub( AboutDlg, IDC_HELPPAYTABLE, HelpPaytable )
l = DlgSetSub( AboutDlg, IDC_HELPKEYBOARD, HelpSecond )
i = DlgModal( AboutDlg )
call DlgUninit( AboutDlg )
return; call unusedqq( ll )
end subroutine

subroutine HelpSecond( Dlg, Id, Callbacktype )
type(dialog) Dlg
integer Id, Callbacktype
type(dialog) ScoreDlg
integer i
logical l
include 'resource.fd'
select case( Id )
	case( IDC_HELPSCORING ); l = DlgInit( IDD_HELPSCORING, ScoreDlg )
	case( IDC_HELPKEYBOARD ); l = DlgInit( IDD_HELPKEYBOARD, ScoreDlg )
end select
i = DlgModal( ScoreDlg )
call DlgUninit( ScoreDlg )
return; call unusedqq(dlg, id, callbacktype)
end subroutine
	
end module poker

!
! Bootstrap into the Poker module
!
program video_poker
use poker
call PokerStart()
end

!----------------------------------------------------------------------
!
! function InitialSettings		(QuickWin initial menu layout)
!
! This routine is called before the main program starts, to setup the menus.
! In order to do this, the old window location must be obtained, and so the
! registry is read here as well.
!
logical function initialsettings()
use poker
use scoring
use pokerregistry
logical result
record /qwinfo/ qwi

call ReadRegistry()

qwi.x = RegXPos
qwi.y = RegYPos
qwi.w = WindowSizeX
qwi.h = WindowSizeY
qwi.type = QWIN$SET
i = SetWSizeQQ( QWIN$FRAMEWINDOW, qwi )

result = APPENDMENUQQ(1, $MENUENABLED, '&Game'C, NUL )
result = APPENDMENUQQ(1, $MENUENABLED, '&Edit Paytable...'C, EditPaytable )
result = APPENDMENUQQ(1, $MENUSEPARATOR, ''C, NUL )
result = APPENDMENUQQ(1, $MENUENABLED, 'E&xit'C, PokerEnd )

result = APPENDMENUQQ(2, $MENUENABLED, '&Bet'C, NUL )
result = APPENDMENUQQ(2, $MENUENABLED, '&1 Credit'C, Bet1 )
result = APPENDMENUQQ(2, $MENUENABLED, '&2 Credits'C, Bet2 )
result = APPENDMENUQQ(2, $MENUENABLED, '&3 Credits'C, Bet3 )
result = APPENDMENUQQ(2, $MENUENABLED, '&4 Credits'C, Bet4 )
result = APPENDMENUQQ(2, $MENUENABLED, '&5 Credits'C, Bet5 )

result = APPENDMENUQQ(3, $MENUENABLED, '&Credits'C, NUL )
result = APPENDMENUQQ(3, $MENUENABLED, '&Deposit $100'C, Add100 )
result = APPENDMENUQQ(3, $MENUENABLED, '&Withdrawl $100'C, With100 )
result = APPENDMENUQQ(3, $MENUSEPARATOR, ''C, NUL )
result = APPENDMENUQQ(3, $MENUENABLED, '&Player Statistics...'C, Stats )

result = APPENDMENUQQ(4, $MENUENABLED, '&Help'C, NUL )
result = APPENDMENUQQ(4, $MENUENABLED, '&Contents...'C, HelpContents )
if( RegMessage ) then
	i = $MENUCHECKED
else
	i = $MENUUNCHECKED
endif
result = APPENDMENUQQ(4, $MENUSEPARATOR, ''C, NUL )
result = APPENDMENUQQ(4, $MENUENABLED.or.i, '&Messages'C,MsgChange)
result = APPENDMENUQQ(4, $MENUSEPARATOR, ''C, NUL )
result = APPENDMENUQQ(4, $MENUENABLED, '&About Poker...'C, AboutPoker )

initialsettings = .true.
end


