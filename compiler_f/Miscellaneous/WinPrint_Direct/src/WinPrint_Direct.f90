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
module WinPrint_Direct

! This is an example of performing Windows printing from Fortran
! using Intel Visual Fortran.  It is similar to the WinPrint sample,
! but instead of rendering the text to be printed as a bitmap font, 
! the routine writes it directly to the printer.  This is useful 
! for printing PostScript* and HP-GL* or other graphic languages.
!
! The module contains one function, Print_Direct, which spools to
! a printer the text file currently open on that unit.  Optional
! arguments let you specify the page orientation and duplex mode.
! See the function and declaration of its arguments for more details.
!
! To use:  add to your program:
!
!	USE WinPrint_Direct
!
! and then add a call to Print_Direct where appropriate.
!
! Future enhancements could include a modeless dialog box to abort
! spooling and page headers with numbering.
!
! Author: Steve Lionel, Intel Fortran Engineering
! Derived from Fortran_WinPrint - see that module for other credits
!
! Revision history
! 0.9 - 22-Apr-2002 - Original
! 1.0 - 17-Mar-2006 - Updated for Intel Fortran
! 1.1 -  2-Apr-2007 - Update for x64
! 1.2 - 10-Jul-2012 - Replace use of GetForegroundWindow for hwndOwner with NULL


use IFWINTY, only: DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE, &
                   DMDUP_SIMPLEX, DMDUP_HORIZONTAL, DMDUP_VERTICAL
implicit none

private
public Print_Direct
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

! Begin function Print_Direct
!
integer(4) function Print_Direct (Unit, Default_Printer, Orientation, DuplexMode)


use ifwin

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
type(T_DOC_INFO_1) :: DOC_INFO_1_Struct
type(T_DEVMODE) :: DEVMODE_Struct
type(T_DEVNAMES) :: DEVNAMES_STRUCT
type(T_PRINTER_DEFAULTS) :: PrtrDef

integer(HANDLE) hPrinter, hDC, hOldDC
pointer (p_DEVMODE_Struct, DEVMODE_Struct)
pointer (p_DEVNAMES_Struct, DEVNAMES_Struct)
integer(HANDLE) hDEVNAMES
pointer (p_DEVNAMES_Handle, hDEVNAMES)


! Declare default values and local variables for optional arguments
logical(4), parameter :: DDefault_Printer = .TRUE.  ! Use default printer?
logical(4) :: WDefault_Printer

character*516 line
character*1024 buffer
character*128 DefaultPrinterName
character*(128) UserPrinterName
pointer (p_PrinterName, UserPrinterName)
character*(MAX_PATH+1) document_name
integer(4) line_len,substr_pos,substr_width,nLen
integer(4) column, i,j
logical(4) Opened
character*10 formatted,sequential
integer(4) IOS

! Establish defaults for optional arguments
!
WDefault_Printer = DDefault_Printer

! Process optional arguments which don't have dependencies.
!
if (present(Default_Printer)) then
  WDefault_Printer = Default_Printer
  end if

! Make sure unit is open.
!
INQUIRE (UNIT=Unit, OPENED=Opened, FORMATTED=Formatted, &
         SEQUENTIAL=Sequential,NAME=document_name,IOSTAT=ios)
if ((.not. Opened) .or. (Formatted /= "YES") .or. &
    (Sequential /= "YES") .or. (ios /= 0)) then
	Print_Direct = FWP_ERR_BADUNIT
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
	Print_Direct = 0
  else
    Print_Direct = FWP_ERR_PRINTDLGFAILED
	end if
  return
  end if

! Get the printer name and open the printer
! See if a change in orientation or duplex mode was requested
!

if (wDefault_Printer) then
    ! Get Name Of Default Printer see MSKB Q135387
	p_PrinterName = loc(DefaultPrinterName)
    nLen = GetProfileString("windows"C, "device"C, ",,,"C, UserPrinterName, &
	    int(sizeof(DefaultPrinterName),DWORD))
    nLen = index(DefaultPrinterName(1:nLen), ",")
    UserPrinterName(nLen:nLen) = char(0)
else
	p_DEVNAMES_Handle = PRINTDLG_Struct%hDevNames
	p_DEVNAMES_Struct = hDEVNAMES
	p_PrinterName = loc(DEVNAMES_Struct) + DEVNAMES_Struct%wDeviceOffset
	end if

    PrtrDef = T_PRINTER_DEFAULTS(NULL, NULL, PRINTER_ACCESS_USE)
    Last_Error = OpenPrinter(UserPrinterName, %LOC(hPrinter), PrtrDef)
    if (Last_Error == 0) then
        Last_Error = GetLastError()
		Print_Direct = FWP_ERR_OPENPRINTERFAILED
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
	! these modifications in the device dependent private area of 
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

!end if


! Bring up the Print dialog box and allow user to select printer
!
hDC = PRINTDLG_Struct%hDC


! Initialize DOC_INFO_1_Struct
!
! Set document name to the part of the filename to the right
! of the rightmost slash or colon
!
i = INDEX(Document_Name, "\", .TRUE.)
j = INDEX(Document_Name, ":", .TRUE.)
if (i < j) i = j
j = LEN_TRIM(Document_Name)+1
Document_Name(j:j) = CHAR(0)
DOC_INFO_1_Struct%pDocName = LOC(Document_Name(i+1:))
DOC_INFO_1_Struct%pOutputFile = NULL
DOC_INFO_1_Struct%pDatatype = NULL

! Start the print job
!
Last_Error = StartDocPrinter (hPrinter, 1, loc(DOC_INFO_1_Struct))
if (Last_Error <= 0) then
  Last_Error = GetLastError()
  write (*,*) "StartDoc failed, code=",Last_Error
  goto 90000
  end if



REWIND (unit)

! As long as there are lines, print them
!
do

  read (unit,'(Q,A)',end=80000) line_len, line

  line_len = MIN(LINE_LEN,512)

  ! Move line to buffer
  column = 1
  do substr_pos = 1,line_len

	  buffer(column:column) = line(substr_pos:substr_pos)
	  column=column+1
	end do
  buffer(column:column) = CHAR(13)
  buffer(column+1:column+1) = CHAR(10)
  line_len = column + 1

  if (line_len <= 0) then
    buffer(1:1) = " "
	line_len = 1
	end if

  substr_pos = 1


	do
!

	Last_Error = WritePrinter (hPrinter, loc(buffer(substr_pos:)), line_len, loc(substr_width))
	if (Last_Error == 0) then
		Last_Error = GetLastError ()
		write (*,*) "WritePrinter failed, err=",Last_Error
		goto 90000
		end if

	line_len = line_len - substr_width
	substr_pos = substr_pos + substr_width
	if (line_len <= 0) exit  ! We're done with this line
	end do
  end do

80000 continue
! We're done with the job



! End the document
Last_Error = EndDocPrinter (hprinter)

90000 continue

! Close the printer
!
	Last_Error = ClosePrinter (hPrinter)


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
end function Print_Direct

end module WinPrint_Direct
