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


program Calendar
USE CALNMOD   !Data Module
USE IFQWIN
USE IFNLS
implicit none

INTEGER(2) fontnum, numfonts
INTEGER(4) cp
type (xycoord) pos

character*50 lang, country


INTEGER(4) i4

TYPE (qwinfo)  qw


OPEN(3, file='user', title='Calendar QuickWin Fortran example')

! Clear the screen to the background color.
i4 = setbkcolor(BackColor)
call clearscreen($GCLEARSCREEN)
! Get the application's child window.
i4 = getwsizeqq(3,QWIN$SIZECURR, qw)
! maximize it.
qw.type = QWIN$MAX
i4 = setwsizeqq(3, qw)


! Get the local information.  This could be used to customize
! the fonts and text.  The program does use locale-specific
! text for month and day numbers and regional settings for
! date formats.
!
call NLSGetLocale(lang, country, cp)   ! get Locale information

!+++++++ Initialize Font
numfonts = INITIALIZEFONTS ( )
fontnum = SETFONT ('t''Arial''h18b')


! Display local language and country information
!
CALL MOVETO (INT4(10), INT4(30), pos)
CALL OUTGTEXT('Language:')
CALL MOVETO (INT2(100), INT2(30), pos)
CALL OUTGTEXT(lang)
CALL MOVETO(INT2(210), INT2(30), pos)
CALL OUTGTEXT('Country :')
CALL MOVETO (INT2(300), INT2(30), pos)
CALL OUTGTEXT(country)


CALL PRINTTHISMONTH()


! Loop forever
do while (.TRUE.)
end do

END


!+++++++++++++++++++++++++++++++++++++++++++++++++
!  InitialSettings ( )
!
LOGICAL(4) FUNCTION InitialSettings()

USE DFLIB
USE DFNLS
implicit none

LOGICAL(4)  l4
integer(4) cp

call NLSGetLocale(CODEPAGE = cp)   
! This routine is called automatically when the program begins.  It sets
! up the menu structure for the program, and connects "callback" routines
! with each menu item.

   l4 = appendmenuqq(1, $MENUENABLED,   'File'C,           NUL)
   l4 = appendmenuqq(1, $MENUENABLED,   'Print...'C,       WINPRINT)
   l4 = appendmenuqq(1, $MENUENABLED,   'Save...'C,        WINSAVE)
   l4 = appendmenuqq(1, $MENUENABLED,   'Exit'C,           WINEXIT)

   l4 = appendmenuqq(2, $MENUENABLED,   'Edit'C,	   NUL)
   l4 = appendmenuqq(2, $MENUENABLED,   'Select Text'C,	   WINSELECTTEXT)
   l4 = appendmenuqq(2, $MENUENABLED,   'Select Graphics'C,WINSELECTGRAPHICS)
   l4 = appendmenuqq(2, $MENUENABLED,   'Select All'C,	   WINSELECTALL)
   l4 = appendmenuqq(2, $MENUENABLED,   'Copy'C,	   WINCOPY)
   l4 = appendmenuqq(2, $MENUENABLED,   'Paste'C,	   WINPASTE)

   l4 = appendmenuqq(3, $MENUENABLED,   'View'C,           NUL)
   l4 = appendmenuqq(3, $MENUENABLED,   'Size to Fit'C,    WINSIZETOFIT)
   l4 = appendmenuqq(3, $MENUENABLED,   'Full Screen'C,    WINFULLSCREEN)

   l4 = appendmenuqq(4, $MENUENABLED,   'State'C,          NUL)
   l4 = appendmenuqq(4, $MENUENABLED,   'Pause'C,          WINSTATE)

   l4 = appendmenuqq(5, $MENUENABLED,   'Window'C,         NUL)
   l4 = appendmenuqq(5, $MENUENABLED,   'Cascade'C,        WINCASCADE)
   l4 = appendmenuqq(5, $MENUENABLED,   'Tile'C,           WINTILE)
   l4 = appendmenuqq(5, $MENUENABLED,   'Arrange Icons'C,  WINARRANGE)
   l4 = appendmenuqq(5, $MENUENABLED,   'Clear Paste'C,    WINCLEARPASTE)
   l4 = appendmenuqq(5, $MENUENABLED,   'Status Bar'C,     WINSTATUS)

   l4 = appendmenuqq(6, $MENUENABLED,   'Help'C,           NUL)
   l4 = appendmenuqq(6, $MENUENABLED,   'Using Help'C,	   WINUSING)
   l4 = appendmenuqq(6, $MENUENABLED,   'About'C,	   WINABOUT)


InitialSettings = l4

return
end



!+++++++++++++++++++++++++++++++++++++++++++++++++
!  PrintBox ( )
!  box : 0,1,2, ...
!
subroutine PrintBox(box, day)  
  USE CALNMOD			!Data Module
  USE IFQWIN
  implicit none
  
  INTEGER(4), INTENT(IN) :: box, day
  INTEGER(2) col, row
  INTEGER(4) color, i4
  type (xycoord) pos
  

  row = box / 7
  col = mod(box, 7)

  i4 = setcolorrgb(GridColor)    
  i4 = rectangle( $GBORDER, Calendar(col+1)%numx,   &
                  ColYOff*row+Calendar(col+1)%numy, &
		  Calendar(col+1)%numx+ColXOff,     &
                  ColYOff*row+Calendar(col+1)%numy+ColYOff )

  if (day > 0) then
    SELECT CASE  (mod(box,7))
	  CASE (0) 
	     color = WeekendColor
	  CASE (1) 
	     color = WeekdayColor
	  CASE (2) 
	     color = WeekdayColor
	  CASE (3) 
	     color = WeekdayColor
	  CASE (4) 
	     color = WeekdayColor
	  CASE (5) 
	     color = WeekdayColor
	  CASE (6) 
	     color = WeekendColor
      CASE DEFAULT
	     color = WeekendColor
    END SELECT
    i4 = setcolorrgb(color)    
    CALL MOVETO(Calendar(col+1)%numx+3, ColYOff*row+Calendar(col+1)%numy+3,pos)
    CALL OUTGTEXT(DayString(day))
  end if
	
end subroutine



!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
!  PRINTTHISMONTH( )
!
!

subroutine PRINTTHISMONTH()
USE CALNMOD   !Data Module
USE IFQWIN 
USE IFPORT, only: PACKTIMEQQ
USE DFNLS
implicit none


character*8 date
character*10 time
character*5 zone
character*50 str
INTEGER(4) DateTime(8), tm, cp
INTEGER(4) DaysInMonth
INTEGER(4) color, day, day1st, box, i4
INTEGER(2) fontnum
type (xycoord) pos

! Get locale, could change output based on codepage returned
!
call NLSGetLocale(CODEPAGE = cp)   

! Get today's date and time using Fortran 90 intrinsic.  Convert to
! format of date as specified by user's regional settings
!
i4 = setcolorrgb(TextColor)    
call date_and_time(date, time, zone, DateTime)
call PackTimeQQ(tm, INT2(DateTime(1)), INT2(DateTime(2)),&
        INT2(DateTime(3)), INT2(DateTime(5)), INT2(DateTime(6)),&
        INT2(DateTime(7)) )
i4 = NLSFormatDate(str, tm, NLS$LongDate)
CALL MOVETO (INT2(10), INT2(80), pos)
CALL OUTGTEXT('Long Format  : ')
CALL MOVETO (INT2(140), INT2(80), pos)
CALL OUTGTEXT(str)

i4 = NLSFormatDate(str, tm, NLS$UseAltCalendar)
CALL MOVETO (INT2(10), INT2(100), pos)
CALL OUTGTEXT('Short Format :')
CALL MOVETO (INT2(140), INT2(100), pos)
CALL OUTGTEXT(str)

i4 = NLSGetLocaleInfo(NLS$LI_SDAYNAME1, str)

CALL SetNameOfMonth(DateTime(2), Str)
CALL MOVETO(INT2(40), INT2(ColY-60), pos)
CALL OUTGTEXT('Calendar of')
CALL MOVETO(INT2(160), INT2(ColY-60), pos)
CALL OUTGTEXT(str)


CALL Get1stDayWeek(day1st)  !0:SUN, 1:MON,...
CALL SetNameOfDay(day1st, Str)
CALL MOVETO(INT2(40), INT2(ColY-40), pos)
CALL OUTGTEXT('Starting on ')
CALL MOVETO(INT2(160), INT2(ColY-40), pos)
CALL OUTGTEXT(str)

!Here we have 
!   day1st : indicates a day of week of 1st of this month
!            0:SUN, 1:MON, ..., 6:SAT
!   


! Print header with names of days, different colors for weekends
fontnum = SETFONT ('t''Arial''h14b')
DO day=0,6
  SELECT CASE  (day)
	  CASE (0) 
	     color = WeekendColor
	  CASE (1) 
	     color = WeekdayColor
	  CASE (2) 
	     color = WeekdayColor
	  CASE (3) 
	     color = WeekdayColor
	  CASE (4) 
	     color = WeekdayColor
	  CASE (5) 
	     color = WeekdayColor
	  CASE (6) 
	     color = WeekendColor
      CASE DEFAULT
	     color = WeekendColor
   END SELECT
   i4 = setcolorrgb(color)    
   CALL SetNameOfDayShort(day, str)
   CALL MOVETO(Calendar(day+1)%numx, Calendar(day+1)%numy-20,pos)
   CALL OUTGTEXT(str)
END DO    
   
box = 0

! Print initial blank boxes for days of first week that are not
! in the current month
!
Do WHILE(box < day1st)
  CALL PrintBox(box, 0)
  box = box + 1
END DO


fontnum = SETFONT ('t''Arial''h18b')

! Get number of days in month, adjusting for leap years if necessary
!
DaysInMonth = EndOfMonth(DateTime(2))
IF (DateTime(2) == 2) THEN	! Check for leap year if February
  IF ((MOD(DateTime(1),4) == 0) .AND.       &
      ((MOD(DateTime(1),100) /= 100) .OR.   &
	   (MOD(DateTime(1),400) == 0))) DaysInMonth = 29
  END IF
       

! Print individual day boxes
!
DO day=1, DaysInMonth
   CALL PrintBox(box, day)
   box = box + 1
   
END DO

END subroutine



!+++++++++++++++++++++++++++++++++++++++++++++++++
! Return a day of week for specified day
!
!
! Parameter : 0:SUN, 1:MON, 2:Tue, ...
!
subroutine Get1stDayWeek(day)
USE KERNEL32
implicit none

INTEGER(4) day

type (T_SYSTEMTIME) systime


CALL GetLocalTime(systime)  !0:Sun, 1:Mon...

day = mod(systime.wDayOfWeek+7 - mod(systime.wDay, 7) + 1, 7)

END subroutine


!+++++++++++++++++++++++++++++++++++++++++++++++++

subroutine SetNameOfDay(day, name)
USE IFNLS
implicit none

INTEGER(4), INTENT(IN) :: day
CHARACTER(*), INTENT(OUT) :: name
INTEGER(4) NameOfDay, i4

! Get the long name of a day based on the locale setting
!
Select Case (day)
   case (0)
      NameOfDay = NLS$LI_SDAYNAME7   !SUN
   case (1)
      NameOfDay = NLS$LI_SDAYNAME1   !MON
   case (2) 
      NameOfDay = NLS$LI_SDAYNAME2   !TUE
   case (3) 
      NameOfDay = NLS$LI_SDAYNAME3   !WED
   case (4) 
      NameOfDay = NLS$LI_SDAYNAME4   !THU
   case (5) 
      NameOfDay = NLS$LI_SDAYNAME5   !FRI
   case (6) 
      NameOfDay = NLS$LI_SDAYNAME6   !SAT
   case DEFAULT
      NameOfDay = NLS$LI_SDAYNAME7 
end select

i4 = NLSGetLocaleInfo(NameOfDay, name)


end subroutine

!+++++++++++++++++++++++++++++++++++++++++++++++++

subroutine SetNameOfDayShort(day, name)
USE IFNLS
implicit none

INTEGER(4), INTENT(IN) :: day
CHARACTER(*), INTENT(OUT) :: name
INTEGER(4) NameOfDay, i4

! Get the short name of a day based on the locale setting
!
Select Case (day)
   case (0)
      NameOfDay = NLS$LI_SABBREVDAYNAME7   !SUN
   case (1)
      NameOfDay = NLS$LI_SABBREVDAYNAME1   !MON
   case (2) 
      NameOfDay = NLS$LI_SABBREVDAYNAME2   !TUE
   case (3) 
      NameOfDay = NLS$LI_SABBREVDAYNAME3   !WED
   case (4) 
      NameOfDay = NLS$LI_SABBREVDAYNAME4   !THU
   case (5) 
      NameOfDay = NLS$LI_SABBREVDAYNAME5   !FRI
   case (6) 
      NameOfDay = NLS$LI_SABBREVDAYNAME6   !SAT
   case DEFAULT
      NameOfDay = NLS$LI_SABBREVDAYNAME7 
end select

i4 = NLSGetLocaleInfo(NameOfDay, name)


end subroutine


!+++++++++++++++++++++++++++++++++++++++++++++++++

subroutine SetNameOfMonth(month, name)
USE IFNLS
implicit none

INTEGER(4) month, NameOfMonth, i4
CHARACTER*(*) name

! Get the name of the month based on the locale setting
!

Select Case (month)
   case (1) 
      NameOfMonth = NLS$LI_SMONTHNAME1
   case (2) 
      NameOfMonth = NLS$LI_SMONTHNAME2   
   case (3) 
      NameOfMonth = NLS$LI_SMONTHNAME3
   case (4) 
      NameOfMonth = NLS$LI_SMONTHNAME4
   case (5) 
      NameOfMonth = NLS$LI_SMONTHNAME5
   case (6) 
      NameOfMonth = NLS$LI_SMONTHNAME6
   case (7) 
      NameOfMonth = NLS$LI_SMONTHNAME7
   case (8) 
      NameOfMonth = NLS$LI_SMONTHNAME8
   case (9) 
      NameOfMonth = NLS$LI_SMONTHNAME9
   case (10) 
      NameOfMonth = NLS$LI_SMONTHNAME10
   case (11) 
      NameOfMonth = NLS$LI_SMONTHNAME11
   case (12) 
      NameOfMonth = NLS$LI_SMONTHNAME12
   case DEFAULT
      NameOfMonth = NLS$LI_SMONTHNAME1
end select

i4 = NLSGetLocaleInfo(NameOfMonth, name)

end subroutine
