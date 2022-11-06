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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! whizzy.f90
!!
!! This sample tests many of the controls available with the
!! dialog manager module.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer function WinMain( hInstance, hPrevInstance, lpszCmdParam, nCmdShow )
!DEC$ attributes stdcall, decorate, alias: 'WinMain' :: WinMain

  use iflogm
  use ifwinty
  implicit none
  include 'whizzy.fd'
  
  integer(HANDLE) hInstance
  integer(HANDLE) hPrevInstance
  integer(LPVOID) lpszCmdParam
  integer(UINT) nCmdShow

  type (dialog) dlgDummy

  call DoDialog( dlgDummy, 0, 0 )

  WinMain = 0
  end function WinMain

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DoDialog
!!
!! Initializes and displays the sample dialog
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DoDialog( dlgParent, id, callbacktype )
  !DEC$ ATTRIBUTES DEFAULT :: DoDialog
  use iflogm
  implicit none
  type (dialog) dlgParent
  integer id
  integer callbacktype

    include 'whizzy.fd'

    integer retint
	logical retlog

  type (dialog) dlg

    external EnableClick, UpdateTemp, UpdateLang, UpdateCreature
    external ClickRightLeft, UpdateCombo

	integer local_id, local_callbacktype
	type (dialog) local_dlg
	local_id = id
	local_callbacktype = callbacktype
	local_dlg = dlgParent

  if ( .not. DlgInit( idd_whizzy, dlg ) ) then
	write (*,*) "error: resource not found"
  else
	! set up enable stuff
	retlog = DlgSet( dlg, IDC_BUTTON_ENABLE, "Disable" )
	retlog = DlgSetSub( dlg, IDC_BUTTON_ENABLE, EnableClick )

	! set up temperature stuff
	retlog = DlgSet( dlg, IDC_SCROLLBAR_TEMPERATURE, 200, dlg_range)
	retlog = DlgSet( dlg, IDC_EDIT_CELSIUS, "100" )
	call UpdateTemp( dlg, IDC_EDIT_CELSIUS, dlg_change)
	retlog = DlgSetSub( dlg, IDC_EDIT_CELSIUS, UpdateTemp )
	retlog = DlgSetSub( dlg, IDC_EDIT_FAHRENHEIT, UpdateTemp )
	retlog = DlgSetSub( dlg, IDC_SCROLLBAR_TEMPERATURE, UpdateTemp )

	! set up the language stuff
	retlog = DlgSetSub( dlg, IDC_RADIO_ENGLISH, UpdateLang )
	retlog = DlgSetSub( dlg, IDC_RADIO_FRENCH, UpdateLang )
	retlog = DlgSetSub( dlg, IDC_RADIO_SPANISH, UpdateLang )

	! set up the creature stuff
	retlog = DlgSet( dlg, IDC_CHECK_BIG, .true. )
	retlog = DlgSet( dlg, IDC_CHECK_GREEN, .true. )
	retlog = DlgSetSub( dlg, IDC_CHECK_BIG, UpdateCreature )
	retlog = DlgSetSub( dlg, IDC_CHECK_GREEN, UpdateCreature )
	call UpdateCreature( dlg, IDC_CHECK_BIG, dlg_clicked )

	! set up Employee stuff
	retlog = DlgSet( dlg, IDC_LIST1, 7 )
	retlog = DlgSet( dlg, IDC_LIST1, "Steve", 1 )
	retlog = DlgSet( dlg, IDC_LIST1, "José", 2 )
	retlog = DlgSet( dlg, IDC_LIST1, "Joe", 3 )
	retlog = DlgSet( dlg, IDC_LIST1, "Paul", 4 )
	retlog = DlgSet( dlg, IDC_LIST1, "Robert", 5 )
	retlog = DlgSet( dlg, IDC_LIST1, "James", 6 )
	retlog = DlgSet( dlg, IDC_LIST1, "Allen", 7 )
	retlog = DlgSet( dlg, IDC_LIST2, 6 )
	retlog = DlgSet( dlg, IDC_LIST2, "Greg", 1 )
	retlog = DlgSet( dlg, IDC_LIST2, "Linda", 2 )
	retlog = DlgSet( dlg, IDC_LIST2, "Jean", 3 )
	retlog = DlgSet( dlg, IDC_LIST2, "Ambrosio", 4 )
	retlog = DlgSet( dlg, IDC_LIST2, "Jeff", 5 )
	retlog = DlgSet( dlg, IDC_LIST2, "Brian", 6 )
	retlog = DlgSetSub( dlg, IDC_BUTTONRIGHT, ClickRightLeft )
	retlog = DlgSetSub( dlg, IDC_BUTTONLEFT, ClickRightLeft )

    ! set up Survey stuff
	retlog = DlgSet( dlg, IDC_COMBO1, 3 )
	retlog = DlgSet( dlg, IDC_COMBO1, "useless", 1 )
	retlog = DlgSet( dlg, IDC_COMBO1, "painful", 2 )
	retlog = DlgSet( dlg, IDC_COMBO1, "impacted", 3 )
	retlog = DlgSetSub( dlg, IDC_COMBO1, UpdateCombo, dlg_selchange )
	retlog = DlgSetSub( dlg, IDC_COMBO1, UpdateCombo, dlg_update )
    call UpdateCombo( dlg, IDC_COMBO1, dlg_update )

    ! set up déjà vu stuff
	retlog = DlgSetSub( dlg, IDC_BUTTON_DEJAVU, DoDialog )

	retint = DlgModal( dlg )

	call DlgUninit( dlg )

    end if

  end subroutine DoDialog

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! EnableClick
!!
!! This routine is called when the user clicks on the
!! enable/disable button.  All controls are then enabled
!! or disabled.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine EnableClick( dlg, id, callbacktype )
  !DEC$ ATTRIBUTES DEFAULT :: EnableClick
  use iflogm
  implicit none
  type (dialog) dlg
  integer id
  integer callbacktype

    include 'whizzy.fd'

	logical retlog, enabled

	integer local_id, local_callbacktype
	local_id = id
	local_callbacktype = callbacktype

    ! get any control to see what our current enable/disable state is
    retlog = DlgGet( dlg, IDC_BOX_TEMPERATURE, enabled )

	! change it to the other state
	enabled = .not. enabled

	if ( enabled ) then
	  retlog = DlgSet( dlg, IDC_BUTTON_ENABLE, "Disable" )
	else
	  retlog = DlgSet( dlg, IDC_BUTTON_ENABLE, "Enable" )
	end if

	retlog = DlgSet( dlg, IDC_BOX_TEMPERATURE, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_TEXT_CELSIUS, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_EDIT_CELSIUS, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_TEXT_FAHRENHEIT, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_EDIT_FAHRENHEIT, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_SCROLLBAR_TEMPERATURE, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_BOX_LANGUAGE, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_RADIO_ENGLISH, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_RADIO_FRENCH, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_RADIO_SPANISH, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_BOX_CREATURE, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_CHECK_BIG, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_CHECK_GREEN, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_STATIC_CREATURE, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_LIST1, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_LIST2, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_BUTTONLEFT, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_BUTTONRIGHT, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_COMBO1, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_STATIC_WISDOM, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_BOX_EMPLOYEES, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_BOX_SURVEY, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_TEXT_AVAILABLE, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_TEXT_UNAVAILABLE, enabled, dlg_enable )
	retlog = DlgSet( dlg, IDC_BUTTON_DEJAVU, enabled, dlg_enable )

  end subroutine EnableClick

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! UpdateTemp
!!
!! This routine is called whenever the temperature edit controls
!! or the associated scrollbar's  state is modified.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine UpdateTemp( dlg, id, callbacktype )
  !DEC$ ATTRIBUTES DEFAULT :: UpdateTemp
  use iflogm
  implicit none
  type (dialog) dlg
  integer id
  integer callbacktype

    include 'whizzy.fd'

	character(1024) text, buff1, buff2
	integer cel, far, retint
	logical retlog

    ! supress compiler warnings for unreferenced arguments
	integer local_callbacktype
	local_callbacktype = callbacktype

    select case (id)
	  case (IDC_EDIT_CELSIUS)
	    ! celsius value was modified by the user so
		! update both fahrenheit and scrollbar values
	    retlog = DlgGet( dlg, IDC_EDIT_CELSIUS, text )
	    read (text, *, iostat=retint) cel
		if ( retint .eq. 0 ) then
		  far = (cel-0.0)*((212.0-32.0)/100.0)+32.0
		  write (text,*) far
		  buff1 = adjustl(text)
		  buff2 = trim(buff1)
		  retlog = DlgSet( dlg, IDC_EDIT_FAHRENHEIT, buff2 )
		  retlog = DlgSet( dlg, IDC_SCROLLBAR_TEMPERATURE, cel, dlg_position )
		endif
	  case (IDC_EDIT_FAHRENHEIT)
	    ! fahrenheit value was modified by the user so
		! update both celsius and scrollbar values
	    retlog = DlgGet( dlg, IDC_EDIT_FAHRENHEIT, text )
	    read (text, *, iostat=retint) far
		if ( retint .eq. 0 ) then
		  cel = (far-32.0)*(100.0/(212.0-32.0))+0.0
		  write (text,*) cel
		  buff1 = adjustl(text)
		  buff2 = trim(buff1)
		  retlog = DlgSet( dlg, IDC_EDIT_CELSIUS, buff2 )
		  retlog = DlgSet( dlg, IDC_SCROLLBAR_TEMPERATURE, cel, dlg_position )
		endif
	  case (IDC_SCROLLBAR_TEMPERATURE)
	    ! scrollbar value was modified by the user so
		! update both celsius and fahrenheit values
	    retlog = DlgGet( dlg, IDC_SCROLLBAR_TEMPERATURE, cel, dlg_position )
		far = (cel-0.0)*((212.0-32.0)/100.0)+32.0
		write (text,*) far
	    buff1 = adjustl(text)
		buff2 = trim(buff1)
		retlog = DlgSet( dlg, IDC_EDIT_FAHRENHEIT, buff2 )
		write (text,*) cel
		buff1 = adjustl(text)
		buff2 = trim(buff1)
		retlog = DlgSet( dlg, IDC_EDIT_CELSIUS, buff2 )
	end select

  end subroutine UpdateTemp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! UpdateLang
!!
!! This routine is called when a language radio button
!! is selected.  Control text is then shown in the selected
!! language.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine UpdateLang( dlg, id, callbacktype )
  !DEC$ ATTRIBUTES DEFAULT :: UpdateLang
  use iflogm
  implicit none
  type (dialog) dlg
  integer id
  integer callbacktype

    include 'whizzy.fd'

	logical retlog

    ! supress compiler warnings for unreferenced arguments
	integer local_id, local_callbacktype
	local_id = id
	local_callbacktype = callbacktype

	select case (id)
	  case (IDC_RADIO_ENGLISH)
	    retlog = DlgSet( dlg, IDC_BOX_LANGUAGE, "Language" )
	    retlog = DlgSet( dlg, IDC_RADIO_ENGLISH, "English" )
	    retlog = DlgSet( dlg, IDC_RADIO_FRENCH, "French" )
	    retlog = DlgSet( dlg, IDC_RADIO_SPANISH, "Spanish" )
	    retlog = DlgSet( dlg, IDC_BUTTON_CANCEL, "Cancel" )
	  case (IDC_RADIO_FRENCH)
	    retlog = DlgSet( dlg, IDC_BOX_LANGUAGE, "Langue" )
	    retlog = DlgSet( dlg, IDC_RADIO_ENGLISH, "Anglais" )
	    retlog = DlgSet( dlg, IDC_RADIO_FRENCH, "Français" )
	    retlog = DlgSet( dlg, IDC_RADIO_SPANISH, "Espagnol" )
	    retlog = DlgSet( dlg, IDC_BUTTON_CANCEL, "Annuler" )
	  case (IDC_RADIO_SPANISH)
	    retlog = DlgSet( dlg, IDC_BOX_LANGUAGE, "Lengua" )
	    retlog = DlgSet( dlg, IDC_RADIO_ENGLISH, "Inglés" )
	    retlog = DlgSet( dlg, IDC_RADIO_FRENCH, "Francés" )
	    retlog = DlgSet( dlg, IDC_RADIO_SPANISH, "Español" )
	    retlog = DlgSet( dlg, IDC_BUTTON_CANCEL, "Cancelar" )
	end select

  end subroutine UpdateLang

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! UpdateCreature
!!
!! This routine is called when a checkbox state changes.
!! The static text below the checkboxes is then updated
!! appropriately.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine UpdateCreature( dlg, id, callbacktype )
  !DEC$ ATTRIBUTES DEFAULT :: UpdateCreature
  use iflogm
  implicit none
  type (dialog) dlg
  integer id
  integer callbacktype

    include 'whizzy.fd'

	logical retlog, big, green

    ! supress compiler warnings for unreferenced arguments
	integer local_id, local_callbacktype
	local_id = id
	local_callbacktype = callbacktype

    retlog = DlgGet( dlg, IDC_CHECK_BIG, big )
    retlog = DlgGet( dlg, IDC_CHECK_GREEN, green )

	if ( big ) then
	  if ( green ) then
	    retlog =DlgSet( dlg, IDC_STATIC_CREATURE, "Tyrannosaur" )
	  else
	    retlog =DlgSet( dlg, IDC_STATIC_CREATURE, "Elephant" )
	  end if
	else
	  if ( green ) then
	    retlog =DlgSet( dlg, IDC_STATIC_CREATURE, "Grasshopper" )
	  else
	    retlog =DlgSet( dlg, IDC_STATIC_CREATURE, "Slug" )
	  end if
	end if

  end subroutine UpdateCreature

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MoveListboxSelections
!!
!! This routine copies selected items from one listbox to
!! another and then deletes the selection from the source
!! listbox.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine MoveListboxSelections( dlg, idSrc, idDst )
  use iflogm
  implicit none
  type (dialog) dlg
  integer idSrc, idDst

	integer NumSrc, NumDst, NumSel
    integer idxSrc, idxDst, idxSel
	logical retlog
	character(1024) temp

    !!! first copy the items from src to dst

    retlog = DlgGet( dlg, idSrc, NumSrc )
    retlog = DlgGet( dlg, idDst, NumDst )

	! how many items are selected in the Src?
	NumSel = 1
    do
      retlog = DlgGet( dlg, idSrc, idxSrc, NumSel )
	  if (idxSrc .eq. 0) then
	    NumSel = NumSel - 1
	    exit
	  endif
	  NumSel = NumSel + 1
	end do

    ! increase the dst by that amount
    retlog = DlgSet( dlg, idDst, NumDst+NumSel )

	! and copy the selected items from Src to Dst
	idxSel = 1
    idxDst = NumDst+1
    do
	  retlog = DlgGet( dlg, idSrc, idxSrc, idxSel )
	  if (idxSrc .eq. 0 ) exit
	  retlog = DlgGet( dlg, idSrc, temp, idxSrc )
	  retlog = DlgSet( dlg, idDst, temp, idxDst )
	  idxSel = idxSel + 1
	  idxDst = idxDst + 1
	end do

	!!! then remove the items from the src

    ! start the target index with the last listbox item
    idxSrc = NumSrc
    do idxSel = NumSel, 1, -1
      retlog = DlgGet( dlg, idSrc, idxDst, idxSel )
	  retlog = DlgGet( dlg, idSrc, temp, idxSrc )
	  retlog = DlgSet( dlg, idSrc, temp, idxDst )
      idxSrc = idxSrc - 1
	end do

    ! finally decrease the source size
	retlog = DlgSet( dlg, idSrc, NumSrc-NumSel )

  end subroutine MoveListboxSelections

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! ClickRightLeft
!!
!! This routine is called whenever one of the directional
!! buttons is clicked.  Selected items from one listbox are
!! then moved to the other.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ClickRightLeft( dlg, id, callbacktype )
  !DEC$ ATTRIBUTES DEFAULT :: ClickRightLeft
  use iflogm
  implicit none
  type (dialog) dlg
  integer id
  integer callbacktype

    integer count
    logical retlog

    include 'whizzy.fd'

    ! supress compiler warnings for unreferenced arguments
	integer local_callbacktype
	local_callbacktype = callbacktype

    if ( id .eq. IDC_BUTTONRIGHT ) then
      call MoveListboxSelections( dlg, IDC_LIST1, IDC_LIST2 )
	else
      call MoveListboxSelections( dlg, IDC_LIST2, IDC_LIST1 )
	endif

    ! disable the right button if list1 is empty
    retlog = DlgGet( dlg, IDC_LIST1, count )
	retlog = DlgSet( dlg, IDC_BUTTONRIGHT, count .ne. 0 )

    ! disable the left button if list2 is empty
    retlog = DlgGet( dlg, IDC_LIST2, count )
	retlog = DlgSet( dlg, IDC_BUTTONLEFT, count .ne. 0 )

  end subroutine ClickRightLeft

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! UpdateCombo
!!
!! This routine is called when the combobox selection is
!! modified or when a new value is selected.  The static
!! text field is then updated.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine UpdateCombo( dlg, id, callbacktype )
  !DEC$ ATTRIBUTES DEFAULT :: UpdateCombo
  use iflogm
  implicit none
  type (dialog) dlg
  integer id
  integer callbacktype

	character(256) temp
	logical retlog

    include 'whizzy.fd'

    ! supress compiler warnings for unreferenced arguments
	integer local_id, local_callbacktype
	local_id = id
	local_callbacktype = callbacktype

    retlog = DlgGet( dlg, IDC_COMBO1, temp )
	temp = "Wisdom teeth are " // trim(temp)
    retlog = DlgSet( dlg, IDC_STATIC_WISDOM, temp )

  end subroutine UpdateCombo

