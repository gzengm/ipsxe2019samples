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

module scoring

use cardhand
use ifqwin
use ifwin
use iflogm
use dlock
use layout
implicit none

include 'resource.fd'

public DisplayBet, DisplayCredits
public ChangeCredits, FindHighestHand, DrawLabel

integer bet, credits
integer handplayed, handscoring
integer handpayoff, handbet
integer creditsin, creditsout

type winner
	type(hand) winninghand
	integer(2) labelx, labely
	character*20 label
	integer(2) numx, numy
	integer payoff, payoffdefault
	integer editcontrol
	integer payoff5, payoff5default
	integer editcontrol5
end type

type(winner) :: winners(10) = &
        (/ WINNER( HandRoyalFlush, LabelCol1X, LabelY, &
  	           'Royal Flush', NumCol1X, LabelY, &
		   250, 250, IDC_PAYROYALFLUSH, 4000, 4000, &
		   IDC_PAYROYALFLUSH5 ), &
	   WINNER( HandStraightFlush, LabelCol1X, LabelY+YOff, &
		   'Straight Flush', NumCol1X, LabelY+YOff, &
		   50, 50, IDC_PAYSTRAIGHTFLUSH, -1, -1, -1 ), &
	   WINNER( HandFourOfAKind, LabelCol1X, LabelY+2*YOff, &
		   'Four of a Kind', NumCol1X, LabelY+2*YOff, &
		   25, 25, IDC_PAYFOUROFAKIND, -1, -1, -1 ), &
	   WINNER( HandFullHouse, LabelCol1X, LabelY+3*YOff, &
		   'Full House', NumCol1X, LabelY+3*YOff, &
		   8, 8, IDC_PAYFULLHOUSE, -1, -1, -1 ), &
	   WINNER( HandFlush, LabelCol1X, LabelY+4*YOff, &
		   'Flush', NumCol1X, LabelY+4*YOff, &
		   5, 5, IDC_PAYFLUSH, -1, -1, -1 ), &
	   WINNER( HandStraight, LabelCol2X, LabelY, &
		   'Straight', NumCol2X, LabelY, &
		   4, 4, IDC_PAYSTRAIGHT, -1, -1, -1 ), &
	   WINNER( HandThreeOfAKind, LabelCol2X, LabelY+YOff, &
		   'Three of a Kind', NumCol2X, LabelY+YOff, &
		   3, 3, IDC_PAYTHREEOFAKIND, -1, -1, -1 ), &
	   WINNER( HandTwoPair, LabelCol2X, LabelY+2*YOff, &
		   'Two Pair', NumCol2X, LabelY+2*YOff, &
		   2, 2, IDC_PAYTWOPAIR, -1, -1, -1 ), &
	   WINNER( HandJacksOrBetter, LabelCol2X, LabelY+3*YOff, &
		   'Jacks or Better', NumCol2X, LabelY+3*YOff, &
		   1, 1, IDC_PAYJACKSORBETTER, -1, -1, -1 ), &
	   WINNER( HandTensOrBetter, LabelCol2X, LabelY+4*YOff, &
		   "10's or Better", NumCol2X, LabelY+4*YOff, &
		   0, 0, IDC_PAYTENSORBETTER, -1, -1, -1 ) /)

integer, parameter :: MAXPAY1 = 150000
integer, parameter :: MAXPAY5 = 10000000

contains

integer function FindHighestHand( PlayerHand, AddToCredits )
type(hand) PlayerHand
logical AddToCredits
integer(2) i, Win, payoff
Win = -1
do i = 1, ubound(winners,1)
	if( winners(i)%Payoff > 0 .and. &
	    CardHandSame( PlayerHand, winners(i)%winninghand ) ) then
		if( Win /= -1 ) then
			if( winners(Win)%payoff < winners(i)%payoff ) then
				Win = i
			end if
		else
			Win = i
		endif
	end if
end do
payoff = 0
if( Win /= -1 ) then
	call DrawLabel( Win, 14_2 )
	if( AddToCredits ) then
		payoff = CalcPayoff( winners(Win) )
		call ChangeCredits( payoff )
	endif
end if
if( AddToCredits ) then
		if( payoff /= 0 ) then
			HandScoring = HandScoring + 1
		end if
		HandPayoff = HandPayoff + payoff
		HandBet = HandBet + Bet
		HandPlayed = HandPlayed + 1
end if
FindHighestHand = Win
end function

subroutine LabelRedraw()
integer(2) i, j
type(xycoord) s

call DrawLock()

! Draw Lines
i = setcolor(15)
do i = 0, 5
	call moveto(labelcol1x-linexoff,labely-lineyoff+(i*yoff),s)
	j = lineto(numcol2x+linexoff,labely-lineyoff+(i*yoff))
end do
call moveto(labelcol1x-linexoff,labely-lineyoff,s)
j = lineto(labelcol1x-linexoff,labely-lineyoff+(5_2*yoff))
call moveto(numcol2x+linexoff,labely-lineyoff,s)
j = lineto(numcol2x+linexoff,labely-lineyoff+(5_2*yoff))
call moveto(labelcol2x-linexoff,labely-lineyoff,s)
j = lineto(labelcol2x-linexoff,labely-lineyoff+(5_2*yoff))

! Draw Labels
do i = 1, ubound(Winners,1)
	if( Winners(i)%payoff > 0 ) then
		call DrawLabel( i, 15_2, .true. )
	else
		call DrawLabel( i, 2_2, .true. )
	endif
end do

call DrawUnlock()

end subroutine

subroutine DrawLabel( i, color, ininit )
integer(2) i, j, color
logical, optional :: ininit
logical init
integer payoff
type(xycoord) s
character*30 str, stre

if( .not. present( ininit ) ) then
	init = .false.
else
	init = ininit
endif

call DrawLock()

j = setcolor(color)
j = setfont("t'Times New Roman'h16")
call moveto(winners(i)%labelx,winners(i)%labely,s)
call outgtext(winners(i)%label)
payoff = CalcPayoff( winners(i) )
call FormatThousands( payoff, str )
if( init ) then
	j = setcolor(2)
	if( winners(i)%payoff5 >= 0 ) then
		call FormatThousands( MAXPAY5, stre )
	else
		call FormatThousands( MAXPAY1*5, stre )
	endif
	do j = 1, len_trim(stre)
		! use 8's for all digits, to make sure they all fit
		if( stre(j:j) >= '0' .and. stre(j:j) <= '9' ) then
			stre(j:j) = '8'
		end if
	end do
	j = rectangle( $GFILLINTERIOR, &
		       winners(i)%numx-getgtextextent(stre(1:len_trim(stre)))-1_2, &
		       winners(i)%numy+1_2, &
		       winners(i)%numx, winners(i)%numy+yoff-lineyoff-1_2 )
	j = setcolor(color)
end if
call moveto(winners(i)%numx-getgtextextent(str(1:len_trim(str))), &
	    winners(i)%numy, s )
call outgtext(str)

call DrawUnlock()

end subroutine

integer function CalcPayoff( winhand )
type(winner) winhand
if( bet == 5 .and. winhand%payoff5 >= 0 ) then
	CalcPayoff = winhand%payoff5
else
	CalcPayoff = winhand%payoff * bet
end if
end function

subroutine DisplayBet()
type(xycoord) s
character*20 str
integer i

call DrawLock()

i = setfont("t'Times New Roman'h20")
i = setcolor(15)
call moveto( TextX, BetY, s )
call outgtext("Bet:")
write( str,'(I1)' )Bet
str = '$ '//adjustl(str)
i = setcolor(2)
i = rectangle( $GFILLINTERIOR, &
	       NumX-getgtextextent('$ 88'), &
	       BetY,NumX,BetY+21_2 )
i = setcolor(15)
call moveto( NumX-getgtextextent(str(1:len_trim(str))), BetY, s )
call outgtext(str)

call DrawUnlock()

end subroutine

subroutine ChangeCredits(num)
integer(2) num
logical j
Credits = Credits + num
if( Credits > 2000000000 ) Credits = 2000000000	   ! Keep it from going negative
call DisplayCredits()
if( Credits >= 100 ) then
	j = ModifyMenuFlagsQQ( 3, 2, $MENUENABLED )
else
	j = ModifyMenuFlagsQQ( 3, 2, $MENUGRAYED )
endif
end subroutine

subroutine DisplayCredits()
type(xycoord) s
character*30 str
integer(2) i, tx, nx
logical, save :: lastsmall = .false.

call DrawLock()

i = setfont("t'Times New Roman'h20")
tx = getgtextextent("Credits:")

call FormatThousands( Credits, Str )
str = '$ '//str
nx = getgtextextent(str(1:len_trim(str)))

i = setcolor(2)
if( (NumX - nx) < TextX+tx+1 ) then
	i = rectangle( $GFILLINTERIOR, TextX, CreditY, &
				       NumX, CreditY+21_2 )
	lastsmall = .false.
else
	if( .not. lastsmall ) then
		i = rectangle( $GFILLINTERIOR, TextX, CreditY, &
					       NumX, CreditY+21_2 )
		i = setcolor(15)
		call moveto( TextX, CreditY, s )
		call outgtext("Credits:")
		lastsmall = .true.
	else
		i = rectangle( $GFILLINTERIOR, TextX+tx+1_2, CreditY, &
				               NumX,CreditY+21_2 )
	end if
end if
i = setcolor(15)
call moveto( NumX-getgtextextent(str(1:len_trim(str))), CreditY, s )
call outgtext(str)

call DrawUnlock()

end subroutine

subroutine bet1( l ); logical l; call betchange( 1, l ); end subroutine
subroutine bet2( l ); logical l; call betchange( 2, l ); end subroutine
subroutine bet3( l ); logical l; call betchange( 3, l ); end subroutine
subroutine bet4( l ); logical l; call betchange( 4, l ); end subroutine
subroutine bet5( l ); logical l; call betchange( 5, l ); end subroutine

subroutine betchange( newbet, instate )
integer newbet
logical instate
integer i
integer result

    do i = 1, 5
	if( i .eq. newbet ) then
	    result = modifymenuflagsqq( 2, i, $MENUCHECKED.or.$MENUENABLED )
	else
	    result = modifymenuflagsqq( 2, i, $MENUUNCHECKED.or.$MENUENABLED )
	endif
    end do
if( newbet .ne. bet ) then
    bet = newbet
    call displaybet()
    call labelredraw()
end if
return; call unusedqq( instate )
end subroutine

subroutine Add100( l )
logical l
call ChangeCredits( 100 )
CreditsIn = CreditsIn + 100
return; call unusedqq(l)
end subroutine

subroutine With100( l )
logical l
call ChangeCredits( -100 )
CreditsOut = CreditsOut + 100
return; call unusedqq(l)
end subroutine

subroutine editpaytable( lll )
logical lll
type( dialog ) PayDlg
logical l
integer i

l = DlgInit( IDD_POKERPAY, PayDlg )
call LoadPaytable( PayDlg )
i = DlgSetSub( PayDlg, IDCHECKOK, EditPaytableOK )
i = DlgSetSub( PayDlg, IDPAY56, CasinoPay )
i = DlgSetSub( PayDlg, IDPAY57, CasinoPay )
i = DlgSetSub( PayDlg, IDPAY58, CasinoPay )
i = DlgSetSub( PayDlg, IDPAY69, CasinoPay )
i = DlgSetSub( PayDlg, IDPAYTENSORBETTER, SetTensOrBetter )
i = DlgSetSub( PayDlg, IDPAYHELP, HelpPaytable )
i = DlgModal( PayDlg )
call DlgUninit( PayDlg )
return; call unusedqq( lll )
end subroutine

subroutine HelpPaytable( PayDlg, Id, Callbacktype )
type(dialog) PayDlg
type(dialog) Dlg
integer Id, Callbacktype
logical l
l = DlgInit( IDD_HELPPAYTABLE, Dlg )
l = DlgModal( Dlg )
call DlgUninit( Dlg )
return; call unusedqq( PayDlg, Id, Callbacktype )
end subroutine

subroutine LoadPaytable( dlg )
type( dialog ) dlg
logical l
integer j
character*60 Str
do j = 1, ubound(winners,1)
	call FormatThousands( winners(j)%payoff, Str )
	l = DlgSet( Dlg, winners(j)%editcontrol, Str(1:len_trim(str)) )
	if( winners(j)%editcontrol5 >= 0 ) then
		call FormatThousands( winners(j)%payoff5, Str )
		l = DlgSet( Dlg, winners(j)%editcontrol5, Str(1:len_trim(str)) )
	end if
end do
end subroutine	

subroutine SetTensOrBetter( Dlg, Id, Callbacktype )
type(dialog) Dlg
integer Id, Callbacktype
integer j
logical l
do j = 1, ubound(winners,1)
	if( winners(j)%editcontrol == IDC_PAYTENSORBETTER ) then
		l = DlgSet( Dlg, winners(j)%editcontrol, "1" )
	end if
end do
return; call unusedqq( id, callbacktype )
end subroutine

subroutine CasinoPay( Dlg, Id, Callbacktype )
type(dialog) Dlg
integer Id, Callbacktype
integer fullhouse, flush
integer j, i
logical l
character*20 str

select case( Id )
	case( IDPAY56 ); flush = 5; fullhouse = 6
	case( IDPAY57 ); flush = 5; fullhouse = 7
	case( IDPAY58 ); flush = 5; fullhouse = 8
	case( IDPAY69 ); flush = 6; fullhouse = 9
end select
do j = 1, ubound(winners,1)
	if( winners(j)%editcontrol == IDC_PAYFULLHOUSE ) then
		i = fullhouse
	else if( winners(j)%editcontrol == IDC_PAYFLUSH ) then
		i = flush
	else
		i = winners(j)%payoffdefault
	endif
	call FormatThousands( i, Str )
	l = DlgSet( Dlg, winners(j)%editcontrol, Str(1:len_trim(str)) )
	if( winners(j)%editcontrol5 >= 0 ) then
		call FormatThousands( winners(j)%payoff5default, Str )
		l = DlgSet( Dlg, winners(j)%editcontrol5, Str(1:len_trim(str)) )
	end if
end do
return; call unusedqq( callbacktype )
end subroutine

subroutine EditPaytableOK( Dlg, Id, Callbacktype )
type(dialog) Dlg
integer Id, Callbacktype
integer i
character*20 str1
if( .not. UnLoadPaytable() ) then
	call FormatThousands( MAXPAY1, str1 )
	i =  MessageBox( Dlg%Hwnd, &
			 'All Paytable entries must be between 0 and '&
			 //str1(1:len_trim(str1))//&
			 ' ($5 Royal Flush payoff may higher).&
		         &  Press "Retry" to edit the table,&
			 & or press "Cancel" to discard your changes.'C, &
			 "Illegal Paytable Entries"C, &
			 MB_RETRYCANCEL .or. MB_ICONSTOP )
	if( i == IDCANCEL ) then
		call DlgExit( Dlg )
	end if
else
	call LabelRedraw()
	call DlgExit( Dlg )
end if
return; call unusedqq( Callbacktype, Id )
contains
logical function UnLoadPaytable()
logical l
integer j
character*60 Str
integer pay1(ubound(winners,1)), pay5(ubound(winners,1))
! Scan for illegal values
do j = 1, ubound(winners,1)
	l = DlgGet( Dlg, winners(j)%editcontrol, Str )
	pay1(j) = atoi( str )
	if( pay1(j) > MAXPAY1 .or. pay1(j) < 0 ) then
		UnLoadPaytable = .false.
		return
	end if		
	if( winners(j)%editcontrol5 >= 0 ) then
		l = DlgGet( Dlg, winners(j)%editcontrol5, Str )
		pay5(j) = atoi( str )
		if( pay5(j) > MAXPAY5 .or. pay5(j) < 0 ) then
			UnLoadPaytable = .false.
			return
		end if
	end if
end do

! All OK
do j = 1, ubound(winners,1)
	winners(j)%payoff = pay1(j)
	if( winners(j)%editcontrol5 >= 0 ) then
		winners(j)%payoff5 = pay5(j)
	end if
end do
UnLoadPaytable = .true.
end function unloadpaytable

integer function atoi( str )
character*(*) str
integer i, j
atoi = 0
do i = 1, len_trim(str)
	j = ichar(str(i:i)) - ichar('0')
	if( j >= 0 .and. j <= 9 ) then
		atoi = (atoi * 10) + j
	else if( .not. (str(i:i) == ',' .or. str(i:i) == ' ') ) then
		atoi = -1
		return
	end if
end do
end function atoi
end subroutine editpaytableOK

subroutine stats( lll )
logical lll

type(dialog) StatDlg
logical l
integer i

l = DlgInit( IDD_STATS, StatDlg )
call LoadStats( StatDlg)
l = DlgSetSub( StatDlg, IDC_RESET, ResetStats )
i = DlgModal( StatDlg )
call DlgUninit( StatDlg )
return; call unusedqq(lll)
end subroutine

subroutine LoadStats( Dlg )
type(dialog) Dlg
character*60 Str
logical l
write(str,*) HandPlayed
call FormatThousands( HandPlayed, Str )
l = DlgSet( Dlg, HAND_PLAYED, Str )
call FormatThousands( HandScoring, Str )
l = DlgSet( Dlg, HAND_SCORED, Str )
if( HandPlayed == 0 ) then
	write(str,*) '----'
else if( HandPayoff == 0 ) then
	str = '0'
else
	write(str,'(F20.3)') real(HandPayoff)/real(HandPlayed)
endif
l = DlgSet( Dlg, HAND_AVERAGE, adjustl(Str) )
if( HandPlayed == 0 ) then
	write(str,*) '----'
else if( HandBet == 0 ) then
	str = '0'
else
	write(str,'(F20.3)') real(HandBet)/real(HandPlayed)
endif
l = DlgSet( Dlg, HAND_BET, adjustl(Str) )

call FormatThousands( Credits, Str )
l = DlgSet( Dlg, CREDITS_CURRENT, Str )
call FormatThousands( CreditsIn, Str )
l = DlgSet( Dlg, CREDITS_DEPOSITED, Str )
call FormatThousands( CreditsOut, Str )
l = DlgSet( Dlg, CREDITS_WITHDRAWN, Str )
call FormatThousands( CreditsOut+Credits-CreditsIn, Str )
l = DlgSet( Dlg, CREDITS_TOTAL, Str )
end subroutine

subroutine FormatThousands( Num, Str )
integer Num
character*(*) Str
if( Num >= 1000000000 ) then
	write(str,'(I1,A1,I3.3,A1,I3.3,A1,I3.3)') &
		Num/1000000000, ',', mod(Num,1000000000)/1000000, &
			',',mod(Num,1000000)/1000,',',mod(Num,1000)
else if( Num >= 1000000 ) then
	write(str,'(I4,A1,I3.3,A1,I3.3)') &
			Num/1000000,',',mod(Num,1000000)/1000,',',mod(Num,1000)
else if( Num >= 1000 ) then
	write(str,'(I7,A1,I3.3)') Num/1000,',',mod(Num,1000)
else
	write(str,'(I10)') Num
endif	
str = adjustl( str )
end subroutine

subroutine ResetStats( Dlg, Id, Callbacktype )
type(dialog) Dlg
integer Id, Callbacktype
HandPlayed = 0
HandScoring = 0
HandPayoff = 0
HandBet = 0
Credits = 100
CreditsIn = 100
CreditsOut = 0
call LoadStats( Dlg )
call DisplayCredits()
return; call unusedqq(Callbacktype, Id )
end subroutine

end module scoring





