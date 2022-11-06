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
!======================================================================
!
! Module BitmapDraw
!
! QuickWin lacks support for drawing an image into a buffer in memory and
! then very quickly drawing this image to the screen.  However, it does
! have the ability to grab an image from a window, and then to paste that
! image elsewhere in that window or in another window.  As it turns out,
! the image format that it gets and puts is in the form of a bitmap file
! (.BMP) and we can use this fact to create our own .BMP image in memory
! for use by QuickWin's PutImage routine.
!

!----------------------------------------------------------------------
!
! Module BitmapTypes
!
! Type definitions for the Win32 BMP format, and for our bitmap structure.
!
! Since interfaces do not inherit data types from outside the interface,
! we need to break these types out into their own module.
!
module BitmapTypes

    !
    ! Standard .BMP structures.  See the Win32 Programmer's Reference for
    ! more details (available through MS Press, or on MSDN).
    !
    !DEC$ pack:1
!DEC$ OPTIONS /WARN=NOALIGN
    type BITMAPFILEHEADER  	! Size = 14
	character(2) bfType
	integer(4) bfSize
	integer(2) bfReserved1
	integer(2) bfReserved2
	integer(4) bfOffBits
    end type BITMAPFILEHEADER
    type BITMAPINFOHEADER  	! Size = 40
	integer(4) biSize
	integer(4) biWidth
	integer(4) biHeight
	integer(2) biPlanes
	integer(2) biBitCount
	integer(4) biCompression
	integer(4) biSizeImage
	integer(4) biXPelsPerMeter
	integer(4) biYPelsPerMeter
	integer(4) biClrUsed
	integer(4) biClrImportant
    end type BITMAPINFOHEADER
    type RGBQUAD  		! Size = 4	
	integer(1) rgbBlue
	integer(1) rgbGreen
	integer(1) rgbRed
	integer(1) rgbReserved
    end type RGBQUAD

    !
    ! This is the user visible bitmap structure, passed to and returned
    ! by the routines in the BitmapDraw module.
    ! The first element, Pixel, is the only element actually used by the
    ! user, to set the pixel index value for a given pixel.  The rest
    ! are used internally.
    !
    ! When in memory, the structure looks something like:
    !
    !    ----------------------------------------------------------------
    !    |    |   | BMP Header | BMP Palette | BMP Pixels               |
    !    ----------------------------------------------------------------
    !      ^--- Pixel pointer, points to BMP pixel memory ---^
    !           ^--- private variables
    !                ^--- BMP Header, address passed to PutImage
    !
    type bitmap
	integer(1), pointer :: Pixel(:,:)
	integer Bptr, Blank
	type(BITMAPFILEHEADER) Bheader
	type(BITMAPINFOHEADER) Binfo
	type(RGBQUAD) Palette(0:255)
	integer(1) EndBitmap		     ! Last element, used to get sizeof
    end type bitmap
    !DEC$ END OPTIONS
    !DEC$ pack
end module BitmapTypes

!----------------------------------------------------------------------
!
! Module BitmapDraw
!
module BitmapDraw

use BitmapTypes
use Descript
implicit none

private
public Bitmap, BitmapRGB
public BitmapAllocate, BitmapDeallocate, BitmapLine
public BitmapSetPalette, BitmapGetPalette, BitmapPut, BitmapGet

! Our own RGB structure for pasing to/from
! BitmapSetPalette and BitmapGetPalette
!
type BitmapRGB
    integer*1 Red
    integer*1 Green
    integer*1 Blue
end type

! Possible values for BitmapPut
!
integer, parameter, public :: BitmapPutSet = 3,   &
			      BitmapPutReset = 2, &
			      BitmapPutAnd = 1,   &
			      BitmapPutOr = 0,    &
			      BitmapPutXor = 4

interface descriptorloc
	integer(int_ptr_kind()) function bapointerloc( ba )
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: bapointerloc
		use bitmaptypes
		type(bitmap), pointer :: ba
	end function
end interface

contains

!----------------------------------------------------------------------
!
! function BitmapAllocate		(Create a bitmap)
!
! Creates a Bitmap object, given the size of the bitmap specified.
! Initializes all internal structures, and initializes the palette and
! all pixels to 0.
!
function BitmapAllocate( SizeX, SizeY )
type(bitmap), pointer :: BitmapAllocate
integer(2) SizeX, SizeY
integer(K_PTR) i
pointer (pd,i)

! We use an integer pointer, since we need to allocate more memory than
! for just the bitmap structure (we need the pixels themselves too).
!
integer(int_ptr_kind()) bptr
type(bitmap), automatic, target :: sizeof
type(DAssign) :: zerosize(1:0)

! Each row in the BMP is padded to next higher 4 byte boundary.
!
integer Pad, sx, tmp
Pad(sx) = ISHFT(ISHFT((sx)+3,-2),2)

BitmapAllocate => sizeof		! to get rid of warning

tmp = (loc(sizeof%EndBitmap)-loc(sizeof)) + SizeY*Pad(SizeX) 
bptr = malloc( (loc(sizeof%EndBitmap)-loc(sizeof)) + SizeY*Pad(SizeX) )
! Set the address of the memory for BitmapAllocate (scalar ptr == no descriptor)
pd = DescriptorLoc(BitmapAllocate)
i = bptr

call DescriptorAssign( DescriptorLoc(BitmapAllocate%Pixel), &
		       loc(BitmapAllocate%EndBitmap), &
		       1, &
		       (/ DAssign( 1, SizeX,  1, Pad(SizeX) ), &
		          DAssign( SizeY, 1, -1, SizeY      ) /) )

BitmapAllocate%Bptr = Bptr			

! Initialize BMP headers
!
BitmapAllocate%Bheader%bfType = 'BM'
BitmapAllocate%Bheader%bfSize = 14 + 40 + (256*4) + (SizeY*Pad(SizeX))
BitmapAllocate%Bheader%bfReserved1 = 0
BitmapAllocate%Bheader%bfReserved2 = 0
BitmapAllocate%Bheader%bfOffBits = 14 + 40 + (256*4)

BitmapAllocate%Binfo%biSize = 40
BitmapAllocate%Binfo%biWidth = SizeX
BitmapAllocate%Binfo%biHeight = SizeY
BitmapAllocate%Binfo%biPlanes = 1
BitmapAllocate%Binfo%biBitCount = 8
BitmapAllocate%Binfo%biCompression = 0
BitmapAllocate%Binfo%biSizeImage = 0
BitmapAllocate%Binfo%biXPelsPerMeter = 0
BitmapAllocate%Binfo%biYPelsPerMeter = 0
BitmapAllocate%Binfo%biClrUsed = 0
BitmapAllocate%Binfo%biClrImportant = 0

! Intiailize pixels to 0, and initialize palette to black.
!
BitmapAllocate%Pixel = 0
BitmapAllocate%Palette = RGBQUAD( 0, 0, 0, 0 )

end function BitmapAllocate

!----------------------------------------------------------------------
!
! Subroutine BitmapDeallocate		(Destroy a bitmap object)
!
subroutine BitmapDeallocate( bmap )
type(bitmap) bmap
integer(int_ptr_kind()) t
if( bmap%bptr /= 0 ) then
	t = bmap%bptr
	bmap%bptr = 0
	call free(t)
end if
end subroutine BitmapDeallocate

!----------------------------------------------------------------------
!
! Subroutine BitmapSetPalette	(Set palette entries in a bitmap object)
!
subroutine BitmapSetPalette( bmap, rgbs, startindex )
type(bitmap), pointer :: bmap
type(bitmaprgb) rgbs(:)
integer starti, i
integer, optional :: startindex

if( present( startindex ) ) then
	starti = startindex-lbound(rgbs,1)
else
	starti = -lbound(rgbs,1)		! assumed start at 0
end if
!print *,starti,rgbs,pointerloc(bmap)
do i = lbound(rgbs,1), ubound(rgbs,1)
	bmap%Palette(i+starti)%rgbBlue  = rgbs(i)%blue
	bmap%Palette(i+starti)%rgbRed   = rgbs(i)%red
	bmap%Palette(i+starti)%rgbGreen = rgbs(i)%green
end do

!pause

end subroutine

!----------------------------------------------------------------------
!
! Subroutine BitmapGetPalette	(Retrieve palette entries from a bitmap)
!
subroutine BitmapGetPalette( bmap, startindex, rgbs )
type(bitmap) bmap
integer startindex, i
type(bitmaprgb) rgbs(:)
integer starti

starti = startindex-lbound(rgbs,1)

do i = lbound(rgbs,1), ubound(rgbs,1)
	rgbs(i)%blue  = bmap%Palette(i+starti)%rgbBlue
	rgbs(i)%red   = bmap%Palette(i+starti)%rgbRed
	rgbs(i)%green = bmap%Palette(i+starti)%rgbGreen
end do
end subroutine

!---------------------------------------------------------------------------
!
! Subroutine BitmapLine			(Draw a line on a bitmap object)
!
subroutine BitmapLine( bmap, index, sx_in, sy_in, ex, ey )
type(bitmap) bmap
integer index
integer sx_in, sy_in, ex, ey
integer sx, sy

integer dx, dy
integer r, dxdy2, dy2		
integer :: xinc, yinc

xinc = 1
yinc = 1

sx = sx_in;  sy = sy_in		! to avoid changing arguments
dx = ex-sx;  dy = ey-sy		! displacements calculated

! We take the absolute value of dx and dy, to determine the slope later.
! Also, we determine in which direction the line should move (increase
! or decrease x/y) with the xinc and yinc variables.
if( dx < 0 ) then
	dx = -dx
	xinc = -1
end if
if( dy < 0 ) then
	dy = -dy
	yinc = -1
end if
	
! Depending on the slope, we want x or y to be the independent variable,
! and the other to be dependent.  The first part of the 'if' statement
! is directly like the algorithm, with xinc and yinc for negative dx/dy.
! The scond part is the same as the first, except for 'dx' and 'dy'
! being exchanged everywhere, with sy as the independent variable.
!
! The code did not have to be duplicated, since 'dx' and 'dy' could have
! been swapped and a flag used to increment sx or sy at the two points
! where this is done.  I felt that to keep the code as speedy as
! possible that this would be the best way to handle it, since the loop
! is very small.
!
if( dx >= dy ) then
	dxdy2 = 2*(dx-dy)		
	dy2 = 2*dy
	r = 2*dy - dx

	dx = dx+1
	do while( dx /= 0 )	
		dx = dx-1	
		bmap%Pixel(sx,sy) = index
		sx = sx + xinc
		if( r >= 0 ) then
			sy = sy + yinc		
			r = r - dxdy2
		else
			r = r + dy2
		end if
	end do
else
	dxdy2 = 2*(dy-dx);		
	dy2 = 2*dx;
	r = 2*dx - dy;
	
	dy = dy+1	
	do while( dy /= 0 )
		dy = dy-1
		bmap%Pixel(sx,sy) = index
		sy = sy + yinc

		if( r >= 0 ) then
			sx = sx + xinc
			r = r - dxdy2
		else
			r = r + dy2
		end if
	end do
end if
end subroutine BitmapLine

!----------------------------------------------------------------------
!
! Subroutine BitmapPut			(Draw a bitmap object on a window)
!
subroutine BitmapPut( bmap, x, y, interaction )
type(bitmap) bmap
integer(2) x, y
integer(2) real_interaction
integer(2), optional :: interaction

interface
  subroutine PutImageBitmap( x, y, image, action )
    !DEC$ attributes C, decorate, alias:"_putimage" :: PutImageBitmap
  integer(2) x, y, action
  integer(int_ptr_kind()) :: image
  end subroutine
end interface

if( present( interaction ) ) then
	real_interaction = interaction
else
	real_interaction = BitmapPutSet
endif

call PutImageBitmap( x, y, loc(bmap%Bheader), real_interaction )
end subroutine BitmapPut

!----------------------------------------------------------------------
!
! Subroutine BitmapGet		(Get a bitmap object from a window)
!
subroutine BitmapGet( bmap, x, y )
type(bitmap) bmap
integer(2) x, y
interface
  subroutine GetImageBitmap( x, y, ex, ey, bmap )
    !DEC$ attributes C, decorate, alias:"_getimage" :: GetImageBitmap
  integer(2) x, y, ex, ey
  integer(int_ptr_kind())bmap
  end subroutine
end interface
call GetImageBitmap( x, y, &
		     INT2(x+bmap%Binfo%biWidth), INT2(y+bmap%Binfo%biHeight), &
		     loc(bmap%Bheader) )
end subroutine BitmapGet

end module BitmapDraw

!DEC$ if defined(BitmapTest)
!----------------------------------------------------------------------
!
! Program BitmapTest		(Tests bitmap routines)
!
! This is a simple program which uses the bitmap routines as a test.
! To compile the test, define "BitmapTest" when compiling (either at
! the command line with "/DBitmapTest" or in the IDE).
!
program go
use BitmapDraw
use ifport, only: sleepqq
interface
subroutine cfunc(i)
!DEC$ ATTRIBUTES c :: cfunc
integer(1), pointer :: i(:,:)
!DEC$ attributes reference :: i
end subroutine
end interface
type(Bitmap), pointer :: Bmap
open(1,file='user')
Bmap => BitmapAllocate( 150, 300 )
open(2,file='user')
call BitmapPut( Bmap, 1, 1 )
call sleepqq(1000)
call BitmapSetPalette( Bmap, 0, (/ BitmapRGB( #ff, 0, 0 ) /) )
call BitmapPut( Bmap, 1, 1 )
call sleepqq(1000)
call BitmapSetPalette( Bmap, 0, (/ BitmapRGB( 0, #ff, 0 ) /) )
call BitmapPut( Bmap, 1, 1 )
call sleepqq(1000)
call BitmapSetPalette( Bmap, 1, (/ BitmapRGB( 0, 0, #ff ) /) )
call BitmapPut( Bmap, 1, 1 )
call sleepqq(1000)
Bmap%Pixel = 1
call BitmapLine( Bmap, 0, 1, 1, 30, 100 )

Bmap%Pixel(1,1) = 0
Bmap%Pixel(10,1) = 0
Bmap%Pixel(20,1) = 0
Bmap%Pixel(30,1) = 0
Bmap%Pixel(40,1) = 0
Bmap%Pixel(50,1) = 0
call BitmapPut( Bmap, 1, 1 )

Bmap%Pixel(1:150,1) = 0
call BitmapPut( Bmap, 1, 1 )

end program
!DEC$ endif
