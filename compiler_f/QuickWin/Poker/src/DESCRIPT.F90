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
!*======================================================================
!*
!* DESCRIPT.F90 -- Intel Visual Fortran Descriptor definitions
!*
!*

module descript

integer, parameter :: DescriptorMaxRank = 7
integer, parameter :: K_PTR = INT_PTR_KIND()
integer(K_PTR), parameter :: M_DEFINED = Z'01'
integer(K_PTR), parameter :: M_NODEALLOC = Z'02'
integer(K_PTR), parameter :: M_CONTIG = Z'03'

!----------------------------------------------------------------------
!
! Type DescriptorTriplet and Type Descriptor
!
! A descriptor is a pointer to a piece of memory and also defines how that
! piece of memory should be accessed when given array indices.  A Fortran 90
! pointer is a form of a descriptor. Type Descriptor is the data structure
! of a descriptor.  Which in turn contains a Type DescriptorTriplet for each
! dimension of the array.
!
! Base and Offset values are added together to obtain the address
! of the first memory location which contains the array.  See the routines
! DescriptorElementAddress and DescriptorAssign for more information
! on how these fields are used and filled in.
!
! Len is used to store the length of Fortran character strings.
!
! Intel descriptors also contain the fields Reserved and Rank.
! There are cases in which these fields are filled in by generated code
! or by the runtimes, but they cannot be relied upon.  It is good practice
! to fill in Rank on any new pointers, and to zero the reserved word.
!
type DescriptorTriplet
    integer(K_PTR) Extent		! Number of Elements in this dimension
    integer(K_PTR) Mult		! Multiplier for this dimension
    integer(K_PTR) LowerBound	! LowerBound of this dimension
end type

type Descriptor
    integer(K_PTR) Base		! Base address
    integer(K_PTR) Len		! Length of data type, used only for
				! character strings
    integer(K_PTR) Offset		
    integer(K_PTR) Flags
    integer(K_PTR) Rank		! Rank of pointer
    integer(K_PTR) Reserved		! Reserved for future use
    type(DescriptorTriplet) Dim(DescriptorMaxRank)
end type

! Structure used to pass array dimension information to DescriptorAssign
!
type DAssign
    integer(K_PTR) LowerBound
    integer(K_PTR) UpperBound
    integer(K_PTR) Stride
    integer(K_PTR) RealSize
end type

! Values for OrderArg to DescriptorAssign
!
logical, parameter :: DescriptColumnOrder = .true.
logical, parameter :: DescriptRowOrder = .false.

! These interfaces are provided to DescriptorLoc for the basic data types.
! See the description for DescriptorLoc at the end of this file for more
! information on adding support for other data types.
!
interface DescriptorLoc
! INTEGER(1) from Rank=0 to Rank=3
    integer(K_PTR) function DescriptorLoc$I10(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$I10
    integer(1), pointer :: p; 		end function
    integer(K_PTR) function DescriptorLoc$I11(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$I11
    integer(1), pointer :: p(:); 	end function
    integer(K_PTR) function DescriptorLoc$I12(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$I12
    integer(1), pointer :: p(:,:); 	end function
    integer(K_PTR) function DescriptorLoc$I13(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$I13
    integer(1), pointer :: p(:,:,:); 	end function
! INTEGER(4) from Rank=0 to Rank=3
    integer(K_PTR) function DescriptorLoc$I40(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$I40
    integer(4), pointer :: p; 		end function
    integer(K_PTR) function DescriptorLoc$I41(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$I41
    integer(4), pointer :: p(:); 	end function
    integer(K_PTR) function DescriptorLoc$I42(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$I42
    integer(4), pointer :: p(:,:); 	end function
    integer(K_PTR) function DescriptorLoc$I43(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$I43
    integer(4), pointer :: p(:,:,:); 	end function
! REAL(4) from Rank=0 to Rank=3
    integer(K_PTR) function DescriptorLoc$R40(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$R40
    real(4), pointer :: p; 		end function
    integer(K_PTR) function DescriptorLoc$R41(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$R41
    real(4), pointer :: p(:); 		end function
    integer(K_PTR) function DescriptorLoc$R42(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$R42
    real(4), pointer :: p(:,:); 	end function
    integer(K_PTR) function DescriptorLoc$R43(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$R43
    real(4), pointer :: p(:,:,:); 	end function
! REAL(8) from Rank=0 to Rank=3
    integer(K_PTR) function DescriptorLoc$R80(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$R80
    real(8), pointer :: p; 		end function
    integer(K_PTR) function DescriptorLoc$R81(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$R81
    real(8), pointer :: p(:); 		end function
    integer(K_PTR) function DescriptorLoc$R82(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$R82
    real(8), pointer :: p(:,:); 	end function
    integer(K_PTR) function DescriptorLoc$R83(p)
    import
    !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$R83
    real(8), pointer :: p(:,:,:); 	end function
end interface

contains

!----------------------------------------------------------------------
!
! Function DescriptorElementAddress
!
! This routine will return the address of an element of a descriptor.
!
! This routine is here for reference only.  Fortran 90 can dereference
! a descriptor on its own.
!
integer(K_PTR) function DescriptorElementAddress( dparg, dims )
    integer(K_PTR) dp, dparg
    integer(K_PTR) dims(:)			! array of dimension values
    type( Descriptor ) d
    pointer( dp, d )
    integer(K_PTR) p			! result pointer
    integer(K_PTR) r			! rank counter
    dp = dparg

    if( lbound(dims,1) /= 1 .or. ubound(dims,1) > DescriptorMaxRank ) then
         DescriptorElementAddress = -1
         return
    end if

    p = d%Base + d%Offset
    do r = 1, ubound(dims,1)
        p = p + (dims(r) * d%dim(r)%Mult)
    end do
    DescriptorElementAddress = p
end function

!----------------------------------------------------------------------
!
! Function DescriptorAssign
!
! This routine will assign a descriptor to a piece of memory.  It is used
! as the equivalent of a Fortran 90 pointer assignment, but gives one
! more control over the assignment, allowing for example assignment to any
! piece of memory.  Another use is to create an array which can be used
! from both Fortran or C.
!
! Arguments are:
!    dparg -- Address of Fortran 90 pointer
!    base  -- Base address of memory to assign to pointer
!    size  -- Datatype size (for example, 4 for Integer(4))
!    dims  -- Information about each dimension (Array of information)
!        LowerBound -- Lower bound for this dimension
!        UpperBound -- Upper bound for this dimension
!        Stride     -- Stride for this dimension
!        RealSize   -- Actual extent of this dimension
!    orderarg (optional) -- Either column order or row order
!
! For example, with the following declaration:
!
!    integer(4), target :: arr(10,10)
!    integer(4), pointer :: dp(:,:)
!
! The following use of DescriptorAssign:
!
!    call DescriptorAssign( &
!	DescriptorLoc(dp), loc(arr(1,1)), 4, &
!	(/ DAssign( l1, u1, s1, 10 ), DAssign( l2, u2, s2, 10 ) /) )
!
! is equivalent to the following Fortran 90 pointer assignment:
!
!    dp => arr(l1:u1:s1,l2:u2:s2)
!
subroutine DescriptorAssign( dparg, base, size, dims, orderarg )
    integer(K_PTR) dparg, dp
    logical, optional :: orderarg
    logical columnorder
    type( DAssign ) dims(:)

    type( Descriptor ) d
    pointer( dp, d )

    integer(K_PTR) r, mult, size, base
    integer todo, dir

    dp = dparg

    if( present( orderarg ) ) then;    columnorder = orderarg
    else; 			       columnorder = .true.
    end if

    if( columnorder ) then;	       dir = 1;  r = 1
    else;			       dir = -1; r = ubound(dims,1)
    end if

    mult = size
    d%Base = base
    d%Offset = 0
    d%Len = size
    d%Rank = ubound(dims,1)
    d%Reserved = 0
    d%Flags = M_NODEALLOC .OR. M_CONTIG .OR. M_DEFINED

    do todo = 1, ubound(dims,1)
	d%Base = d%Base + (dims(r)%LowerBound-1) * mult
	d%dim(r)%extent = (dims(r)%UpperBound - dims(r)%LowerBound +  &
				dims(r)%Stride)  / dims(r)%Stride
	if( d%dim(r)%extent < 0 ) d%dim(r)%extent = 0
	d%dim(r)%mult = dims(r)%Stride * mult
	d%dim(r)%lowerbound = 1
	d%offset = d%offset - d%dim(r)%mult
	mult = mult * dims(r)%RealSize
	r = r + dir
    end do

end subroutine DescriptorAssign

!----------------------------------------------------------------------
!
! subroutine DescriptorPrint
!
! Prints the contents of a Descriptor.  Primarily used for debugging
! purposes, or to investigate what information a descriptor holds.
!
subroutine DescriptorPrint( dparg, rank )
    integer(K_PTR) dparg, dp
    type( Descriptor ) d
    pointer( dp, d )
    integer r, rank
    dp = dparg
    print *,'Descriptor at Address:  ',dparg
    print *,'  Base Address:         ',d%Base
    print *,'  Length:               ',d%Len
    print *,'  Offset:               ',d%offset
    print *,'  Rank:                 ',d%rank
    print *,'  Flags:                ',d%flags
    do r = 1, rank
        print '(X,"  Dimension ",i1," Extent:  ",i12)',r,d%dim(r)%extent
        print *,'              Mult:     ',d%dim(r)%Mult
        print *,'          LowerBound:   ',d%dim(r)%LowerBound
    end do
end subroutine DescriptorPrint

end module descript

!----------------------------------------------------------------------
!
! Function DescriptorLoc	(Returns the address of a descriptor)
!
! This simple routine will return the address of a descriptor.  Any
! time you call a descriptor routine, you use this function to pass
! the address of the descriptor, instead of the descriptor itself.
!
! To use it, you must have an interface for the type of data type
! that the descriptor is pointing to.  To do this, add a specific interface
! to the generic interface for DescriptorLoc, such as:
!
!   interface DescriptorLoc
!      integer function DescriptorLoc$typefoo(p)
!      !DEC$ attributes decorate, alias:'DESCRIPTORLOC' :: DescriptorLoc$typefoo
!      type(foo), pointer :: p
!      end function
!   end interface
!
! This has been done for you for rank 0 to 3 pointer arrays of type
! integer(1), integer(4), real(4), real(8),  at the top of the descript
! module.  To properly do this, you need not change this module, just add
! lines similar to those above in your module where you use the descript
! module.  For an example, see bitmap.f90 in the POKER example.
!
integer(INT_PTR_KIND()) function DescriptorLoc( in )
integer(INT_PTR_KIND()) in
!DEC$ attributes value :: in
DescriptorLoc = in
end function

!* End file descript.f90
