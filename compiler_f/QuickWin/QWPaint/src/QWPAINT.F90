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
!  QWPAINT.F90 - Sample QuickWin paintbrush program
!
!  This program demonstrates using mouse callbacks and graphics
!  routines in a QuickWin application.  It opens two windows.  The
!  Control Grid window allows you to select different graphical
!  characteristics.  The Canvas window allows you to draw graphics
!  in it based on those characteristics.
!
!  QuickWin's built-in File.Save option can be used to save the
!  contents of either graphics window as a bitmap (.BMP) file.
!
!  Routines in this source file:
!    SUBROUTINE BrushControl(iunit, ievent, ikeystate, ixpos, iypos)
!    SUBROUTINE CanvasControl(iunit, ievent, ikeystate, ixpos, iypos)
!    SUBROUTINE DrawControlGrid()
!    SUBROUTINE UpdateCursor()
!
!  Other source files:
!    PAINTMOD.F90 = Module file for global data and utility routines
!
!-----------------------------------------------------------------------

USE IFQWIN
USE QWPAINT

INTEGER(2)  i2
INTEGER(4)  i4, event, ix, iy
EXTERNAL    BrushControl, CanvasControl
RECORD      /qwinfo/ qw

! Initialize global variables.
BrushUnit   = 2
CanvasUnit  = 3
drawcolor   = 12
fillcolor   = 11
berm        = 5
scale       = 50
scalestep   = 10
shape       = 0
size        = scale / 4
fill        = $GBORDER

! Redefine the default About box.
i4 = aboutboxqq('QuickWin Paintbrush\rVersion 1.0'C)

! Prepare the Brush Control window.
OPEN(BrushUnit, file='user', title='Control Grid')
i2 = initializefonts()
call DrawControlGrid()

! Register the mouse events for the BrushUnit window.  Each time a mouse
! button is clicked in this window, the routine BrushControl will be called.
event = MOUSE$LBUTTONDOWN
event = IOR (event, MOUSE$RBUTTONDOWN)
event = IOR (event, MOUSE$LBUTTONDBLCLK)
event = IOR (event, MOUSE$RBUTTONDBLCLK)
i4 = registermouseevent(BrushUnit, event, BrushControl)

! Prepare the Canvas window.
! The clearscreen is needed in order to make the window appear at this point,
! so it can be tiled with the other window during this setup phase.
OPEN(CanvasUnit, file='user', title='Canvas')
call clearscreen($GCLEARSCREEN)
i4 = focusqq(CanvasUnit)

! Register the mouse events for the CanvasUnit window.  Each time the mouse
! is moved or a mouse button is clicked in this window, the routine
! CanvasControl will be called.
event = MOUSE$MOVE
event = IOR (event, MOUSE$LBUTTONDOWN)
event = IOR (event, MOUSE$RBUTTONDOWN)
event = IOR (event, MOUSE$RBUTTONDBLCLK)
i4 = registermouseevent(CanvasUnit, event, CanvasControl)

! *** The following note applies to both child windows. ***
! Note that if the other window has focus, mouse events other than left
! button clicks will not be perceived by this window unless focus is given
! to it.  The left button is the exception since it always gives focus to
! the window on which it is clicked.

! Maximize the application's frame window; tile the Brush Control and
! Canvas windows; turn off the Status Bar.
qw.type = QWIN$MAX
i4 = setwsizeqq(QWIN$FRAMEWINDOW, qw)
i4 = clickmenuqq(QWIN$STATUS)
i4 = clickmenuqq(QWIN$TILE)

! Wait forever to allow event-driven action.
do while (.TRUE.)
  i4 = waitonmouseevent(MOUSE$RBUTTONDOWN, i4, ix, iy)
end do
end

!-----------------------------------------------------------------------

SUBROUTINE BrushControl(iunit, ievent, ikeystate, ixpos, iypos)

USE IFQWIN
USE QWPAINT

INTEGER(4)  iunit, ievent, ikeystate, ixpos, iypos

INTEGER(2)  i2
INTEGER(4)  i4, ControlBox

! This routine responds to selections made in the Control Grid.

! Suppress "unused dummy argument" warnings.  Callback routines such as
! this one must all have the same argument list.  This means you will get
! warnings for any of the arguments that you don't use.  This statement
! eliminates such warnings by "using" any leftover arguments.
i4 = iunit + ievent

! Determine which control box was clicked upon.
ControlBox = ControlNumber(ixpos, iypos)

! If click is outside control grid, produce a warning beep, but
! do nothing else.
if (ControlBox == -1) then
  call beepqq(440, 200)
  return
end if

! If click is inside color grid, change draw or fill-to color depending on
! whether the left or right button was clicked.
if (ControlBox == 0) then
  i4 = setactiveqq(BrushUnit)
  i2 = getpixel(INT2(ixpos), INT2(iypos))
  if (set(MOUSE$KS_LBUTTON, ikeystate)) then
    drawcolor = i2
    i2 = setcolor(drawcolor)
  end if
  if (set(MOUSE$KS_RBUTTON, ikeystate)) then
    fillcolor = i2
    i2 = setcolor(fillcolor)
  end if

! If click is inside a control box, change cursor or fill flag or
! control grid size depending on which grid box was clicked.
else
  select case (ControlBox)

  case (1) ! Resize control grid
    if (set(MOUSE$KS_RBUTTON, ikeystate)) then
      scale = scale + scalestep
      if (scale.GT.100) then
        scale = 100
        call beepqq(440,200)
      else
        size = scale / 4
        call DrawControlGrid()
      end if
    else
      scale = scale - scalestep
      if (scale.LT.scalestep) then
        scale = scalestep
        call beepqq(440,200)
      else
        size = scale / 4
        call DrawControlGrid()
      end if
    end if

  case (2) ! Resize cursor
    if (set(MOUSE$KS_LBUTTON, ikeystate)) then
      size = size - 1
      if (size.LT.0) size = (scale-3)/2
    else
      size = size + 1
      if (size.GT.(scale-3)/2) size = 0
    end if

  case (3) ! Change cursor shape
    if (set(MOUSE$KS_LBUTTON, ikeystate)) then
      shape = shape + 1
      if (shape.GT.5) shape = 0
    else
      shape = shape - 1
      if (shape.LT.0) shape = 5
    end if

  case (4) ! Toggle fill flag
    if (fill == $GBORDER) then
      fill = $GFILLINTERIOR
    else
      fill = $GBORDER
    end if

  end select
end if

! Redraw the icons in the top row of the control grid to account for
! any color or size changes made above.
call UpdateCursor()

return
end

!-----------------------------------------------------------------------

SUBROUTINE CanvasControl(iunit, ievent, ikeystate, ixpos, iypos)

USE IFQWIN
USE QWPAINT

INTEGER(4)  iunit, ievent, ikeystate, ixpos, iypos

INTEGER(2)  i2, localfill
INTEGER(4)  i4

! This routine responds to mouse activity in the Canvas window.

! Direct all graphics output to this window.
i4 = setactiveqq(CanvasUnit)

! Suppress "unused dummy argument" warnings.
i4 = iunit

! For right button doubleclicks, we either invoke a floodfill at the
! present mouse position with the present cursor and fill-to colors, or
! if the Ctrl key was held down at the time of the click we clear the
! screen.
if (set(MOUSE$RBUTTONDBLCLK, ievent)) then
  if (set(MOUSE$KS_CONTROL, ikeystate)) then
    call clearscreen($GCLEARSCREEN)
  else
    i2 = floodfill(INT2(ixpos), INT2(iypos), fillcolor)
  end if
  return
end if

! If the Ctrl key is held while drawing, reverse the current fill state.
localfill = fill
if (set(MOUSE$KS_CONTROL, ikeystate)) then
  if (localfill == $GBORDER) then
    localfill = $GFILLINTERIOR
  else
    localfill = $GBORDER
  end if
end if

! If the Shift key is held while drawing, draw in black.  Otherwise use
! the current cursor color.
i2 = setcolor(drawcolor)
if (set(MOUSE$KS_SHIFT, ikeystate)) i2 = setcolor(0_2)

! If the left button is down (whether we entered this callback routine
! due to a button click or a mouse movement), draw the current cursor
! centered at the present mouse location.
if (set(MOUSE$KS_LBUTTON, ikeystate)) then
  select case (shape)
    case (0)
      i4 =   ellipse(localfill, INT2(ixpos-size), INT2(iypos-size), &
                                INT2(ixpos+size), INT2(iypos+size))
    case (1)
      i4 = rectangle(localfill, INT2(ixpos-size), INT2(iypos-size), &
                                INT2(ixpos+size), INT2(iypos+size))
    ! The circles and rectangles can be filled in when drawn by the
    ! graphics routines, but the triangles are different.  Since they
    ! are not primitives but are instead constructed by this program,
    ! they must be floocfilled separately.  This of course makes them
    ! take longer to appear.
    case (2:5)
      call DrawTriangle(int2(ixpos-size), int2(iypos-size), &
      int2(ixpos+size), int2(iypos+size))
      if (localfill == $GFILLINTERIOR) &
      i2 = floodfill(INT2(ixpos), INT2(iypos), drawcolor)
  end select
end if

return
end

!-----------------------------------------------------------------------

SUBROUTINE DrawControlGrid()

USE IFQWIN
USE QWPAINT

INTEGER(2)  i2
INTEGER(4)  i4, i, j, fontnum
RECORD      /xycoord/ xy

! This routine draws the grid of controls which correspond to various
! paintbrush actions.  The grid is 4x5, with a border (berm) around it.

! Erase the Control Grid window.
call setvieworg(0, 0, xy)
call clearscreen($GCLEARSCREEN)

! Draw the grid border.
i2 = setcolor(9_2)
i4 = rectangle($GFILLINTERIOR, 0_2, 0_2, INT2(scale*4+berm*2), INT2(scale*5+berm*2))

! By setting the view origin to (berm,berm), we can now draw controls
! to the window without having to consider the offset of the berm all
! the time.
call setvieworg(berm, berm, xy)

! Black out the area for the control grid.
i2 = setcolor(0_2)
i4 = rectangle($GFILLINTERIOR, 0_2, 0_2, INT2(scale*4), INT2(scale*5))

! Draw the color grid.
do i=0,3
  do j=0,3
    i2 = setcolor(INT2(i*4+j))
    i4 = rectangle($GFILLINTERIOR, INT2(j*scale), INT2((i+1)*scale), &
                               INT2((j+1)*scale), INT2((i+2)*scale))
  end do
end do

! Draw the boxes for the control icons along the top row.
i2 = setcolor(9_2)
i4 = rectangle($GBORDER, INT2(0*scale), 0_2, INT2(1*scale), INT2(scale))
i4 = rectangle($GBORDER, INT2(1*scale), 0_2, INT2(2*scale), INT2(scale))
i4 = rectangle($GBORDER, INT2(2*scale), 0_2, INT2(3*scale), INT2(scale))
i4 = rectangle($GBORDER, INT2(3*scale), 0_2, INT2(4*scale), INT2(scale))

! Draw the grid size control box icon.
do i=1,4
  i4 = rectangle($GBORDER, int2(scale/6.0*i),     int2(scale/6.0*0.5), &
                           int2(scale/6.0*(i+1)), int2(scale/6.0*5.5))
end do
do i=1,5
  i4 = rectangle($GBORDER, &
    int2(scale/6.0*1.0),  int2((scale/6.0*i)-(scale/12.0)), &
    int2(scale/6.0*5.0),  int2((scale/6.0*(i+1))-(scale/12.0)) )
end do

! Write out the instructions below the control grid.
i2 = setcolor(15_2)
fontnum = setfont('t''Arial''h16')
i = scale*5+10

call moveto(2_2, INT2(i), xy)
call outgtext('Control grid and cursor sizing (boxes 1 and 2):')
i = i + 20
call moveto(12_2, INT2(i), xy)
call outgtext('Left button = smaller; right button = bigger')
i = i + 20

call moveto(2_2, INT2(i), xy)
call outgtext('Cursor shape selection (box 3):')
i = i + 20
call moveto(12_2, INT2(i), xy)
call outgtext('Either button cycles through shapes')
i = i + 20

call moveto(2_2, INT2(i), xy)
call outgtext('Fill state (box 4):')
i = i + 20
call moveto(12_2, INT2(i), xy)
call outgtext('Either button toggles fill state')
i = i + 20
call moveto(12_2, INT2(i), xy)
call outgtext('(Ctrl key reverses fill state while drawing)')
i = i + 20

call moveto(2_2, INT2(i), xy)
call outgtext('Color selection (boxes 5 through 20):')
i = i + 20
call moveto(12_2, INT2(i), xy)
call outgtext('Left button = cursor color; right button = fill-to color')
i = i + 20

call moveto(2_2, INT2(i), xy)
call outgtext('Drawing (on Canvas window):')
i = i + 20
call moveto(12_2, INT2(i), xy)
call outgtext('Left button draws current cursor (shape, size, & color)')
i = i + 20
call moveto(12_2, INT2(i), xy)
call outgtext('(with Shift key draws black instead of current color)')
i = i + 20
call moveto(12_2, INT2(i), xy)
call outgtext('Right button doubleclick = floodfill')
i = i + 20
call moveto(12_2, INT2(i), xy)
call outgtext('(with Ctrl key clears screen instead of floodfill)')

i = i + 20
call moveto(2_2, INT2(i), xy)
call outgtext('File.Save can be used to save bitmaps')

! The other 3 icons (cursor size, cursor shape, and fill state) are
! drawn via the UpdateCursor routine.
call UpdateCursor()

return
end

!-----------------------------------------------------------------------

SUBROUTINE UpdateCursor()

USE IFQWIN
USE QWPAINT

IMPLICIT NONE
INTEGER(2)  ulx, uly, lrx, lry
INTEGER(4) i4, i2
! This routine draws the cursor control icons in Control Grid boxes 2-4.

! Direct all graphics output to this window.
i4 = setactiveqq(BrushUnit)

! Blank out the previous cursor (box 3).
i2 = setcolor(0_2)
i4 = rectangle($GFILLINTERIOR, INT2(2*scale+1), 1_2, INT2(3*scale-1), INT2(scale-1))

! Draw the cursor size control box icon (box 2).
! For this control grid box, we don't need to blank out the old icon.
! Because its shape is always the same, we just let the new one overwrite
! the old one.
i2 = setcolor(drawcolor)
i4 = rectangle($GBORDER, int2(1.1*scale), int2(0.55*scale), &
                         int2(1.3*scale), int2(0.75*scale))
i4 = rectangle($GBORDER, int2(1.4*scale), int2(0.25*scale), &
                         int2(1.9*scale), int2(0.75*scale))

! Draw the new cursor (box 3).
ulx = 2.5*scale-size
uly = 0.5*scale-size
lrx = 2.5*scale+size
lry = 0.5*scale+size

select case (shape)
  case (0)
    i4 =   ellipse($GBORDER, ulx, uly, lrx, lry)
  case (1)
    i4 = rectangle($GBORDER, ulx, uly, lrx, lry)
  case (2:5)
    call DrawTriangle       (ulx, uly, lrx, lry)
end select

! Draw the fill state control box icon (box 4).
! For this control grid box, we don't need to blank out the old icon.
! Because its shape is always the same, we just let the new one overwrite
! the old one.
i2 = setcolor(fillcolor)
i4 = rectangle($GFILLINTERIOR, int2(3.25*scale), int2(0.25*scale), &
                               int2(3.75*scale), int2(0.75*scale))
if (fill == $GBORDER) then
  i2 = setcolor(0_2)
  i4 = rectangle($GFILLINTERIOR, int2(3.35*scale), int2(0.35*scale), &
                                 int2(3.65*scale), int2(0.65*scale))
end if

return
end

!-----------------------------------------------------------------------
