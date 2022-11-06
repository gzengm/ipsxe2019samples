!==============================================================
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
module Fortran_WinPrint

! This is an example of performing Windows printing from Fortran
! using Intel Visual Fortran.  
! 
! The module contains one function, Print_Unit, which spools to
! a printer the text file currently open on that unit.  Optional
! arguments let you specify the font, point size, margins, line spacing,
! tab stops, page orientation and whether or not Fortran carriage 
! control is to be interpreted.  See the function and declaration 
! of its arguments for more details.
!
! To use:  add to your program:
!
!	USE Fortran_WinPrint
!
! and then add a call to Print_Unit where appropriate.
!
! Future enhancements could include a modeless dialog box to abort
! spooling and page headers with numbering.
!
! Author: Steve Lionel, Intel Fortran Engineering
! with contributions by Bill Conrad, Intel Fortran Engineering and
! John Termine, Swiss Re Investors.
!
! Revision history
! 0.1 - 26-Jan-2000 - Original
! 0.2 - 31-Jan-2000 - Selecting font size based on Chars_per_Line replaced
!                     with Point_Size argument.
! 0.3 - 23-Feb-2000 - Add Orientation argument.  Fix error in margin calculation.
! 0.4 - 12-Apr-2000 - Add Duplex argument.  Rework Orientation/Duplex logic
!                     to be more reliable.  Select the font after each StartPage,
!                     required on Windows 95.  Add transparency code for overprinting.
! 1.0 - 13-Jun-2000 - Add missing test for Default_Printer argument.
! 1.1 - 21-Mar-2001 - Change PRINTER_ALL_ACCESS to PRINTER_ACCESS_USE, to allow
!                     nonprivileged users to use this
! 1.2 - 25-May-2001 - Use GetForegroundWindow to get the owner for PRINTDLG.  This allows
!                     the code to be used unchanged in QuickWin applications.
! 1.3 - 06-May-2005 - Convert for Intel Visual Fortran
! 1.4 - 02-Apr-2007 - Adapt for x64.
! 1.5 - 10-Jul-2012 - Replace use of GetForegroundWindow for hwndOwner with NULL. The
!                     earlier change is incorrect.
!

use IFWINTY, only: DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE, &
                   DMDUP_SIMPLEX, DMDUP_HORIZONTAL, DMDUP_VERTICAL
implicit none

private
public Print_Unit
integer(4), parameter, public :: &
    FWP_ERR_BADUNIT = -1 ,&          ! Unit was not open for formatted, sequential access
    FWP_ERR_PRINTDLGFAILED = -2, &   ! Call to PrintDlg failed
    FWP_ERR_GLOBALLOCKFAILED = -3, & ! Call to GlobalLock failed
    FWP_ERR_RESETDCFAILED = -4, &	 ! Call to ResetDC failed
    FWP_ERR_GLOBALUNLOCKFAILED = -5, &  ! Call to GlobalUnlock failed
	FWP_ERR_OPENPRINTERFAILED = -6   ! Call to OpenPrinter failed

! Constants for Orientation argument
!
integer(4), parameter, public :: &
    FWP_ORIENTATION_DEFAULT = -1, &   ! Use default orientation as returned
                                     ! From PrintDlg
    FWP_ORIENTATION_PORTRAIT = DMORIENT_PORTRAIT, &  ! Force portrait orientation
    FWP_ORIENTATION_LANDSCAPE = DMORIENT_LANDSCAPE    ! Force landscape orientation

! Constants for DuplexMode argument
!
integer(4), parameter, public :: &
    FWP_DUPLEXMODE_DEFAULT = -1, &      ! Use default duplex mode
	FWP_DUPLEXMODE_HORIZONTAL = DMDUP_HORIZONTAL, &  ! Long edge horizontal
	FWP_DUPLEXMODE_VERTICAL = DMDUP_VERTICAL         ! Long edge vertical


integer(4), public :: last_error     ! Holds last return status from API routine

contains

! Begin function Print_Unit
!
integer(4) function Print_Unit (Unit, Default_Printer, Fortran_CC, Horiz_Margin, Vert_Margin, &
                            Line_Spacing, Font, Point_Size, Tab_Stop, Orientation, &
							DuplexMode)

use comdlg32, only: PrintDlg, CommDlgExtendedError, T_PRINTDLG, PD_ALLPAGES, PD_RETURNDC, &
                    PD_NOPAGENUMS, PD_RETURNDEFAULT, PD_NOSELECTION
use gdi32, only: DeleteDC, StartDoc, StartPage, EndDoc, EndPage, TextOut, &
		         GetDeviceCaps, GetTextExtentPoint32, GetTextExtentExPoint, &
				 CreateFont, SelectObject, DeleteObject, ResetDC,  SetBkMode,&
				 DM_ORIENTATION, DM_DUPLEX, DM_IN_BUFFER, DM_OUT_BUFFER, &
				 PHYSICALOFFSETX, PHYSICALOFFSETY, HORZRES, VERTRES, &
				 LOGPIXELSX, LOGPIXELSY, T_DOCINFO, T_SIZE, T_DEVMODE, &
				 DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE, TRANSPARENT, &
				 FW_NORMAL, ANSI_CHARSET, OUT_DEFAULT_PRECIS, &
				 CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, &
				 DEFAULT_PITCH, FF_DONTCARE, PRINTER_ACCESS_USE, T_PRINTER_DEFAULTS
use kernel32, only: GetLastError, MulDiv, GlobalLock, GlobalUnlock, GlobalFree, &
                    GetProfileString
use winspool, only: OpenPrinter, ClosePrinter, DocumentProperties
use ifwinty, only: NULL, TRUE, FALSE, MAX_PATH, ERROR_NO_ERROR, HANDLE, DWORD

implicit none

! Routine arguments.  All but Unit are optional
!
integer(4), intent(in) :: Unit         
  ! Unit to print.  Must be open for formatted access,
  ! and NOT with CARRIAGECONTROL='FORTRAN'.  No record
  ! in the file may contain more than 512 characters
  ! (longer records are truncated to 512).

logical(4), intent(in), optional :: Default_Printer
  ! If .TRUE., system default printer is used and no
  ! print dialog box is displayed.  If .FALSE., a print
  ! dialog box is displayed to allow the user to select
  ! a printer and number of copies.  Default .TRUE.

logical(4), intent(in), optional :: Fortran_CC   
  ! .TRUE. if Fortran carriage control is 
  ! to be recognized, .FALSE. if not.  Default .FALSE.

real(4), intent(in), optional :: Horiz_Margin 
  ! Margins on left and right side of page, in inches.
  ! Ignored if outside printable area.  Default 0.5

real(4), intent(in), optional :: Vert_Margin  
  ! Margins on top and bottom of page, in inches.
  ! Ignored if outside printable area.  Default 0.5

real(4), intent(in), optional :: Line_Spacing
  ! Ratio to line height (dependent on font) of distance
  ! between top of one line and top of the next. Ignored
  ! if zero or negative or if greater than 10.0.  Default
  ! is 1.0 (100% of line height)

character*(*), intent(in), optional :: Font
  ! Name of a Windows font to use.  Default is "Courier New".
  ! Non-proportional fonts (such as Courier New) are recommended
  ! for tabular data.  Maximum of 32 characters.

integer(4), intent(in), optional :: Point_Size
  ! Size in points of desired font.  If omitted or less than
  ! zero, Windows picks a default size. 

integer(4), intent(in), optional :: Tab_Stop
  ! Number of columns between tab stops - used if line to
  ! print contains tab characters. Column 1 is the "zeroth"
  ! tab stop.  Default is 4, meaning that tab stops are columns
  ! 5, 9, 13, etc.  Ignored if less than 1.  The tab-expanded
  ! line may not exceed 1024 characters.

integer(4), intent(in), optional :: Orientation
  ! Specifies what page orientation is desired.  The value
  ! may be one of the following:
  !    FWP_ORIENTATION_DEFAULT  - Uses printer default
  !    FWP_ORIENTATION_PORTRAIT - Forces portrait orientation
  !    FWP_ORIENTATION_LANDSCAPE - Forces landscape orientation
  ! If omitted, DEFAULT is used.  Not all printers may support
  ! this option. Ignored if Default_Printer is .FALSE.

integer(4), intent(in), optional :: DuplexMode
  ! Specifies what printer duplex mode is desired if the default
  ! printer is to be used. The value may be one of the following:
  !		FWP_DUPLEXMODE_DEFAULT - Uses printer default
  !	    FWP_DUPLEXMODE_HORIZONTAL - Flip so that long edge is horizontal
  !	    FWP_DUPLEXMODE_VERTICAL - Flip so that long edge is vertical
  ! If omitted, DEFAULT is used.  Not all printers may support
  ! this option. Ignored if Default_Printer is .FALSE.

! Return value:
!
!	If successful, a positive integer containing the print
!	job number
!   If the print dialog box was displayed and the user clicked
!	cancel, zero is returned,
!   If unsuccessful, a negative integer that is one of the
!	FWP_ERR_xxx codes defined in this module.
!

type(T_PRINTDLG) :: PRINTDLG_Struct
type(T_DOCINFO) :: DOCINFO_Struct
type(T_SIZE) :: SIZE_Struct
type(T_DEVMODE) :: DEVMODE_Struct
type(T_PRINTER_DEFAULTS) :: PrtrDef

integer(HANDLE) hFont,hOldFont, hPrinter, hOldDC, hDC
integer xoffset,yoffset,page_width,page_height,pixels_per_inch_x,pixels_per_inch_y
integer line_height, current_x, current_y, initial_x, initial_y
integer max_y, line_width, font_height
pointer (p_DEVMODE_Struct, DEVMODE_Struct)

! Declare default values and local variables for optional arguments
logical(4), parameter :: DDefault_Printer = .TRUE.  ! Use default printer?
logical(4) :: WDefault_Printer
real(4), parameter :: Dline_spacing = 1.0  ! Multiplier by line height of line spacing
real(4) :: WLine_Spacing  ! Local variable
real(4), parameter :: Dhoriz_margin = 0.5   ! Left and right margins in inches
real(4) :: WHoriz_margin
real(4), parameter :: Dvert_margin = 0.5    ! Top and bottom margins in inches
real(4) :: WVert_Margin
integer(4), parameter :: DTab_stop = 4      ! Tab column width
integer(4) :: WTab_stop
integer(4), parameter :: DPoint_Size = 0    ! Point size (0=Default)
integer(4) :: WPoint_Size
character*33, parameter :: Dfont = "Courier New"C ! Font
character*33 :: Wfont
logical(4), parameter :: DFortran_CC = .FALSE.  ! Fortran carriage control?
logical(4) :: WFortran_CC

character*516 line
character*1024 buffer
character*128 DefaultPrinterName
character*(MAX_PATH+1) document_name
integer(4) line_len,substr_pos,substr_width,nLen
integer(4) column, i,j,ret
logical(4) top_of_page,Opened
character*10 formatted,sequential
integer(4) IOS

! Establish defaults for optional arguments
!
WDefault_Printer = DDefault_Printer
WLine_Spacing = DLine_Spacing
WHoriz_Margin = DHoriz_Margin
WVert_Margin = DVert_Margin
WTab_Stop = DTab_Stop
WPoint_Size = DPoint_Size
Wfont = Dfont
WFortran_CC = DFortran_CC

! Process optional arguments which don't have dependencies.
!
if (present(Default_Printer)) then
  WDefault_Printer = Default_Printer
  end if
if (present(Fortran_CC)) then
  WFortran_CC = Fortran_CC
  end if
if (present(Font)) then
  if (len(Font) <= 32) then
    WFont = TRIM(Font) // CHAR(0)
	end if
  end if
if (present(Point_Size)) then
  if (Point_Size > 0) WPoint_Size = Point_Size
  end if
if (present(Tab_Stop)) then
  if (Tab_Stop > 0) WTab_Stop = Tab_Stop
  end if
if (present(Line_Spacing)) then
  if ((Line_Spacing > 0.0) .and. (Line_Spacing < 10.0)) &
     WLine_Spacing = Line_Spacing
  end if

! Make sure unit is open.
!
INQUIRE (UNIT=Unit, OPENED=Opened, FORMATTED=Formatted, &
         SEQUENTIAL=Sequential,NAME=document_name,IOSTAT=ios)
if ((.not. Opened) .or. (Formatted /= "YES") .or. &
    (Sequential /= "YES") .or. (ios /= 0)) then
	Print_Unit = FWP_ERR_BADUNIT
	return
	end if


! Initialize PRINTDLG_Struct
!
PRINTDLG_Struct%lStructSize = SIZEOF(PRINTDLG_Struct)
PRINTDLG_Struct%hwndOwner = NULL
PRINTDLG_Struct%hDevMode = NULL
PRINTDLG_Struct%hDevNames = NULL
PRINTDLG_Struct%hDc = NULL
PRINTDLG_Struct%Flags = PD_ALLPAGES .OR. PD_RETURNDC .OR. PD_NOPAGENUMS &
   .OR. PD_NOSELECTION

if (WDefault_Printer) then
    PRINTDLG_Struct%Flags = IOR(PRINTDLG_Struct%Flags,PD_RETURNDEFAULT)
	end if

PRINTDLG_Struct%nFromPage = 1
PRINTDLG_Struct%nToPage = 1
PRINTDLG_Struct%nMinPage = 1
PRINTDLG_Struct%nMaxPage = 1
PRINTDLG_Struct%nCopies = 1
PRINTDLG_Struct%hInstance = NULL
PRINTDLG_Struct%lCustData = NULL
PRINTDLG_Struct%lpfnPrintHook = NULL
PRINTDLG_Struct%lpfnSetupHook = NULL
PRINTDLG_Struct%lpPrintTemplateName = NULL
PRINTDLG_Struct%lpSetupTemplateName = NULL
PRINTDLG_Struct%hPrintTemplate = NULL
PRINTDLG_Struct%hSetupTemplate = NULL

Last_Error = PrintDlg (PRINTDLG_Struct)
if (Last_Error == 0) then
  Last_Error = CommDlgExtendedError ()
  if (Last_Error == 0) then
	Print_Unit = 0
  else
    Print_Unit = FWP_ERR_PRINTDLGFAILED
	end if
  return
  end if

! See if a change in orientation or duplex mode was requested
!

if (wDefault_Printer .and. (present(Orientation) .or. present(DuplexMode))) then

    ! Get Name Of Default Printer see MSKB Q135387
    nLen = GetProfileString("windows"C, "device"C, ",,,"C, DefaultPrinterName, &
	    int(sizeof(DefaultPrinterName),DWORD))
    nLen = index(DefaultPrinterName(1:nLen), ",")
    DefaultPrinterName(nLen:nLen) = char(0)

    PrtrDef = T_PRINTER_DEFAULTS(NULL, NULL, PRINTER_ACCESS_USE)
    Last_Error = OpenPrinter(DefaultPrinterName, %LOC(hPrinter), PrtrDef)
    if (Last_Error == 0) then
        Last_Error = GetLastError()
		Print_Unit = FWP_ERR_OPENPRINTERFAILED
		return
        end if

    ! get access to DevMode
    p_DEVMODE_struct = GlobalLock(PRINTDLG_Struct%hDevMode)
    ! modify the DevMode struct
	if (present(Orientation)) then
	  if (Orientation /= FWP_ORIENTATION_DEFAULT)	then   
        if (IAND(DEVMODE_struct%dmFields, DM_ORIENTATION) == DM_ORIENTATION) &
             DEVMODE_struct%field1%dmOrientation = Orientation
        end if
      end if

	if (present(DuplexMode)) then	   
	  if (DuplexMode /= FWP_DUPLEXMODE_DEFAULT) then
		if (IAND(DEVMODE_struct%dmFields, DM_DUPLEX) == DM_DUPLEX) &
           DEVMODE_struct%dmDuplex = DuplexMode
        end if
	  end if

    ! set the devmode in case print driver stores info related to 
	! these modifications in the device dependant private area of 
	! the DevMode - MSKB Q167345
    Last_Error = DocumentProperties(NULL, hPrinter, DefaultPrinterName, &
		DEVMODE_struct, DEVMODE_struct,    &
        (IOR(DM_IN_BUFFER,DM_OUT_BUFFER))) ! IDOK = succcess
    
    ! update the hDC with the changes
    hOldDC = ResetDC(PRINTDLG_Struct%hDC, DEVMODE_struct)
    Last_Error = GlobalUnlock(PRINTDLG_Struct%hDevMode)
    if (Last_Error == FALSE) then
        Last_Error = GetLastError()
    end if

    Last_Error = ClosePrinter(hPrinter)
end if


! Bring up the Print dialog box and allow user to select printer
!
hDC = PRINTDLG_Struct%hDC


! Get various metrics for this printer.  Width and
! height are of the printable area.
!
xoffset = GetDeviceCaps (hDC, PHYSICALOFFSETX)
yoffset = GetDeviceCaps (hDC, PHYSICALOFFSETY)
page_width = GetDeviceCaps (hDC, HORZRES)
page_height = GetDeviceCaps (hDC, VERTRES)
pixels_per_inch_x = GetDeviceCaps (hDC, LOGPIXELSX)
pixels_per_inch_y = GetDeviceCaps (hDC, LOGPIXELSY)


! Compute initial X and Y offset to account for the 
! requested margins.  If the requested margin
! is outside the printable area, the whole printable
! area is used.  Offsets are relative to the printable
! area.
!
if (present(Horiz_Margin)) then
  if (Horiz_Margin >= 0.0) WHoriz_Margin = Horiz_Margin
  end if
if (present(Vert_Margin)) then
  if (Vert_Margin >= 0.0) WVert_Margin = Vert_Margin
  end if
initial_x = NINT(REAL(pixels_per_inch_x)*Whoriz_margin) - xoffset
if (initial_x > page_width) initial_x = 0 
initial_y = NINT(REAL(pixels_per_inch_y)*Wvert_margin) - yoffset
if (initial_y > page_height) initial_y = 0 

! Create font with requested point size
! 

font_height = 0
if (WPoint_Size > 0) then
  font_height = -MulDiv (WPoint_Size, pixels_per_inch_y, 72)
  end if

hFont = CreateFont (font_height, & !
				    0, &
					0,0, &
					FW_NORMAL, FALSE,FALSE,FALSE, &
					ANSI_CHARSET, OUT_DEFAULT_PRECIS, &
					CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, &
					DEFAULT_PITCH .OR. FF_DONTCARE, &
					Wfont)
if (hfont /= 0) then
    hOldFont = SelectObject (hDC, hFont)
	end if

! Get height of characters
Last_Error = GetTextExtentPoint32 (hDC, "Ay", 2, SIZE_struct) 
                        
! Compute printed line height (vertical distance 
! between top of one line and top of the next)
! by multiplying the character (font) height by 
! the line_spacing multiplier.  This allows users
! to adjust inter-line spacing.  A multiplier of 
! 1.00 means use the character height as the 
! line height.
!
line_height = NINT (REAL(SIZE_Struct%cy)*Wline_spacing) 

! Compute maximum Y value which will still print on 
! the page (inside margins)
!
max_y = page_height - (initial_y + line_height)

! Compute width of actual line we are printing (used for 
! wrap computations)
!
line_width = page_width - (2*initial_x)                   

! Initialize DOCINFO_Struct
!
DOCINFO_Struct%cbSize = SIZEOF(DOCINFO_Struct)
! Set document name to the part of the filename to the right
! of the rightmost slash or colon
!
i = INDEX(Document_Name, "\", .TRUE.)
j = INDEX(Document_Name, ":", .TRUE.)
if (i < j) i = j
j = LEN_TRIM(Document_Name)+1
Document_Name(j:j) = CHAR(0)
DOCINFO_Struct%lpszDocName = LOC(Document_Name(i+1:))
DOCINFO_Struct%lpszOutput = NULL
DOCINFO_Struct%lpszDatatype = NULL
DOCINFO_Struct%fwType = 0

! Start the print job
!
Print_Unit = StartDoc (hDC, DOCINFO_Struct)
if (Last_Error <= 0) then
  Last_Error = GetLastError()
  write (*,*) "StartDoc failed, code=",Last_Error
  goto 90000
  end if

! Start the first page
Last_Error = StartPage (hDC)
if (hFont /= 0) ret = SelectObject (hDC, hFont) ! Needed for Windows 95
top_of_page = .TRUE.

current_x = initial_x
current_y = initial_y

REWIND (unit)

! As long as there are lines, print them
!
do

  read (unit,'(Q,A)',end=80000) line_len, line

  line_len = MIN(LINE_LEN,512)

  ! Move line to buffer, adjusting for tab stops.
  column = 1
  do substr_pos = 1,line_len
    if (line(substr_pos:substr_pos) == CHAR(9)) then
	  do i=1,(Wtab_stop-(mod(column-1,Wtab_stop)))
	    buffer(column:column) = " "
		column = column + 1
		end do
	else
	  buffer(column:column) = line(substr_pos:substr_pos)
	  column=column+1
	  end if
	end do
  line_len = column - 1
  !buffer (column:column) = CHAR(0)

  if (line_len <= 0) then
    buffer(1:1) = " "
	line_len = 1
	end if

  substr_pos = 1

  ! Apply Fortran carriage control if necessary
  !
  if (Wfortran_cc) then
    if (buffer(1:1) == " ") then
	   continue  ! do nothing
    else if (buffer(1:1) == "1") then
	  ! Force new page
	  current_y = max_y + 1
	else if (buffer(1:1) == "0") then
	  current_y = current_y + line_height  ! Double space
    else if (buffer(1:1) == "-") then
	  current_y = current_y + (2*line_height) ! Triple space
	else if (buffer(1:1) == "+") then
	  current_y = MAX(current_y-line_height, initial_y)
	  end if
	substr_pos = 2
	line_len = line_len - 1
	if (line_len <= 0) then
	  buffer(2:2) = " "
	  line_len = 1
	  end if
	end if

    

	do

	! Do we need to start a new page?
	!
	if (current_y > max_y) then
		! End this page and start a new one
		!
		Last_Error = EndPage (hDC)
		Last_Error = StartPage (hDC)
		if (hFont /= 0) ret = SelectObject (hDC, hFont) ! Needed for Windows 95
		current_y = initial_y
		top_of_page = .true.
		end if

	! Compute number of characters that will fit on a line
	!
	Last_Error = GetTextExtentExPoint (hDC, buffer(substr_pos:), line_len, &
		    line_width, loc(substr_width), NULL, SIZE_Struct)

	! Set the background mode to TRANSPARENT so that overprinting does
	! overprint, rather than replacing the previous characters.  This has to
	! be set before every TextOut.
	!
	Last_Error = SetBKMode(hDC, TRANSPARENT)

	! Put out the line
	!
	Last_Error = TextOut (hDC, current_x, current_y, buffer(substr_pos:),&
	                substr_width)
	current_y = current_y + line_height

	top_of_page = .FALSE.
	line_len = line_len - substr_width
	substr_pos = substr_pos + substr_width
	if (line_len <= 0) exit  ! We're done with this line
	end do
  end do

80000 continue
! We're done with the job

! End the page
Last_Error = EndPage (hDC)

! End the document
Last_Error = EndDoc (hDC)

90000 continue
! Delete the device context
!
if (hFont /= 0) then
    Last_Error = SelectObject (hDC, hOldFont)
	Last_Error = DeleteObject (hFont)
	end if

Last_Error = DeleteDC (hDC)

! Free the devmode and devname fields of the PRINTDLG structure
! necessary.
!
if (PRINTDLG_struct%hDevMode /= NULL) then
    Last_Error = GlobalFree (PRINTDLG_struct%hDevMode)
	end if
if (PRINTDLG_struct%hDevNames /= NULL) then
	Last_Error = GlobalFree (PRINTDLG_struct%hDevNames)
	end if


99999 continue
return
end function Print_Unit

end module Fortran_WinPrint
