!
! SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
! http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!
! Copyright 2018 Intel Corporation
!
! THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
! NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
!
!------------------------------------------------------------------------------
!
!  ./Poisson/solutions/Icomm/pardat.f90
!

MODULE parallel_data

USE mpi
USE types

USE errhnd  ! for setting (init_errh) and freeing (end_errh) error handlers
            ! see Courses/MPI-1/ErrHnd directory

IMPLICIT NONE

INTEGER:: myid, numprocs            ! Rank, #procs

INTEGER:: p1, p2                    ! Processor grid
INTEGER:: myid_i, myid_j            ! Own coordinates in this grid

INTEGER:: neigh_w=MPI_PROC_NULL,                                           &
   &      neigh_e=MPI_PROC_NULL,                                           &
   &      neigh_s=MPI_PROC_NULL,                                           &
   &      neigh_n=MPI_PROC_NULL
                                    ! west, east, south, north neighbor

! All id's related to MPI_COMM_WORLD


INTEGER:: len_cb                    ! length of combuf
REAL(KIND=REAL8), DIMENSION(:), ALLOCATABLE:: combuf   
                                    ! aux. buffer for communication

CONTAINS

! Module functions:
!    init_par                        initialize grid partitioning data
!    exchange                        boundary exchange of red or black points
!
!

SUBROUTINE init_par(n,imin,imax,jmin,jmax,ierr)

!
! Initialize the above module data, given grid size n
! myid, p1, p2, numprocs are assumed to be initialized before, 
! p1*p2==numprocs mandatory
!
! Input:  n   (global grid size)
! Output: imin, imax, jmin, jmax: local grid is imin..imax x jmin..jmax
!         ierr: system error code (buffer allocation failure)
!


IMPLICIT NONE

INTEGER, INTENT(IN ):: n           ! Grid size
INTEGER, INTENT(OUT):: ierr,imin,imax,jmin,jmax

                                   ! error code, local grid size
INTEGER:: portion

IF( myid < p1*p2 ) THEN

!! Coordinates in p1 x p2 processor grid

  myid_i = mod(myid,p1)
  myid_j = myid/p1


  portion = n/p1

  imin = myid_i*portion + min( myid_i, mod(n,p1) ) + 1

  IF( myid_i < mod(n,p1) ) portion=portion+1
  imax = imin+portion-1

  portion = n/p2

  jmin = myid_j*portion + min( myid_j, mod(n,p2) ) + 1

  IF( myid_j < mod(n,p2) ) portion=portion+1
  jmax = jmin+portion-1

  IF( myid_i>0    )  neigh_w=myid-1
  IF( myid_i<p1-1 )  neigh_e=myid+1
  IF( myid_j>0    )  neigh_s=myid-p1
  IF( myid_j<p2-1 )  neigh_n=myid+p1

  len_cb = 2*(2+ imax-imin+1+jmax-jmin+1)    !Length of Boundary x 2
  ALLOCATE(combuf(len_cb), stat=ierr)
  CALL errfunc(MPI_COMM_WORLD,MPI_SUCCESS-abs(ierr))

  combuf = 0.

ENDIF

END SUBROUTINE init_par


SUBROUTINE exchange(COLOR, x, imin, imax, jmin, jmax)
!
! INPUT: imin, imax, jmin, jmax           local grid bounds
!        COLOR                            CHARACTER(LEN=*)
!                                         'RED' for points (i,j) with i+j even
!                                         'BLACK' for points (i,j) with i+j odd
!
!
! INOUT: x(imin-1:imax+1,jmin-1:jmax+1)    input: any gridfunction 
!                                          output: same gridfunction, 
!                                          updated 'COLOR' boundary points
!                                          x(imin-1,:), x(imax+1,:),
!                                          x(:,jmin-1), x(:,jmax+1)
!
! Implementation: MPI_ISEND to 4 neighbors in a fixed order
!                 MPI_RECV from all neighbors (fixed order)
!                 MPI_WAITALL 
!                 MPI_COMM_WORLD is used as underlying communicator
!
!


USE types

IMPLICIT NONE

CHARACTER*(*), INTENT(IN):: COLOR
INTEGER, INTENT(IN):: imin, imax, jmin, jmax

REAL(KIND=REAL8), DIMENSION(imin-1:imax+1,jmin-1:jmax+1):: x

INTEGER:: modulus, ifrst_n, ifrst_s, i, jfrst_w, jfrst_e, j
INTEGER:: ierr, ic, ic0, len_r, ptr_r

INTEGER:: request(4), status(MPI_STATUS_SIZE,4)

IF( COLOR == 'RED' ) THEN
  modulus=0
ELSE IF (COLOR == 'BLACK') THEN
  modulus=1
ELSE
  WRITE(*,*) 'exchange: wrong color'
ENDIF

request = MPI_REQUEST_NULL

ic=0
! Northern exchange, line j==jmax

IF( neigh_n /= MPI_PROC_NULL ) THEN

ifrst_n = imin
IF( mod(ifrst_n+jmax,2) /= modulus ) THEN
  ifrst_n=ifrst_n+1
ENDIF

ic0=ic+1
DO i=ifrst_n,imax,2
  ic=ic+1
  combuf(ic) = x(i,jmax)
ENDDO
 
CALL MPI_ISEND(combuf(ic0), ic-ic0+1, MPI_DOUBLE_PRECISION, neigh_n, 1000,    &
               MPI_COMM_WORLD, request(1), ierr)

ENDIF


! Southern exchange, line j==jmin

IF( neigh_s /= MPI_PROC_NULL ) THEN

ifrst_s = imin
IF( mod(ifrst_s+jmin,2) /= modulus ) THEN
  ifrst_s=ifrst_s+1
ENDIF

ic0=ic+1
DO i=ifrst_s,imax,2
  ic=ic+1
  combuf(ic) = x(i,jmin)
ENDDO
 
CALL MPI_ISEND(combuf(ic0), ic-ic0+1, MPI_DOUBLE_PRECISION, neigh_s, 1000,    &
               MPI_COMM_WORLD, request(2), ierr)

ENDIF


! Western exchange, line i==imin

IF( neigh_w /= MPI_PROC_NULL ) THEN

jfrst_w = jmin
IF( mod(imin+jfrst_w,2) /= modulus ) THEN
  jfrst_w=jfrst_w+1
ENDIF

ic0=ic+1
DO j=jfrst_w,jmax,2
  ic=ic+1
  combuf(ic) = x(imin,j)
ENDDO
 
CALL MPI_ISEND(combuf(ic0), ic-ic0+1, MPI_DOUBLE_PRECISION, neigh_w, 1000,    &
               MPI_COMM_WORLD, request(3), ierr)

ENDIF


! Eastern exchange, line i==imax

IF( neigh_e /= MPI_PROC_NULL ) THEN

jfrst_e = jmin
IF( mod(imax+jfrst_e,2) /= modulus ) THEN
  jfrst_e=jfrst_e+1
ENDIF

ic0=ic+1
DO j=jfrst_e,jmax,2
  ic=ic+1
  combuf(ic) = x(imax,j)
ENDDO
 
CALL MPI_ISEND(combuf(ic0), ic-ic0+1, MPI_DOUBLE_PRECISION, neigh_e, 1000,    &
               MPI_COMM_WORLD, request(4), ierr)

ENDIF

len_r = len_cb-ic
ptr_r = ic+1

IF( neigh_n /= MPI_PROC_NULL ) THEN

CALL MPI_RECV(combuf(ptr_r), len_r, MPI_DOUBLE_PRECISION, neigh_n, 1000,       &
              MPI_COMM_WORLD, status, ierr)

ifrst_n = ifrst_n-1
IF( ifrst_n < imin ) THEN
  ifrst_n = ifrst_n+2
ENDIF

ic=ptr_r-1
DO i=ifrst_n,imax,2
  ic=ic+1
  x(i,jmax+1) = combuf(ic)
ENDDO

ENDIF


IF( neigh_s /= MPI_PROC_NULL ) THEN

CALL MPI_RECV(combuf(ptr_r), len_r, MPI_DOUBLE_PRECISION, neigh_s, 1000,       &
              MPI_COMM_WORLD, status, ierr)

ifrst_s = ifrst_s-1
IF( ifrst_s < imin ) THEN
  ifrst_s = ifrst_s+2
ENDIF

ic=ptr_r-1
DO i=ifrst_s,imax,2
  ic=ic+1
  x(i,jmin-1) = combuf(ic)
ENDDO

ENDIF

IF( neigh_w /= MPI_PROC_NULL ) THEN

CALL MPI_RECV(combuf(ptr_r), len_r, MPI_DOUBLE_PRECISION, neigh_w, 1000,      &
               MPI_COMM_WORLD, status, ierr)

jfrst_w = jfrst_w-1
IF( jfrst_w < jmin ) THEN
  jfrst_w = jfrst_w+2
ENDIF

ic=ptr_r-1
DO j=jfrst_w,jmax,2
  ic=ic+1
  x(imin-1,j) = combuf(ic)
ENDDO

ENDIF


IF( neigh_e /= MPI_PROC_NULL ) THEN

CALL MPI_RECV(combuf(ptr_r), len_r, MPI_DOUBLE_PRECISION, neigh_e, 1000,      &
               MPI_COMM_WORLD, status, ierr)

jfrst_e = jfrst_e-1
IF( jfrst_e < jmin ) THEN
  jfrst_e = jfrst_e+2
ENDIF

ic=ptr_r-1
DO j=jfrst_e,jmax,2
  ic=ic+1
  x(imax+1,j) = combuf(ic)
ENDDO

ENDIF

CALL MPI_WAITALL(4, request, status, ierr)

END SUBROUTINE exchange

END MODULE

