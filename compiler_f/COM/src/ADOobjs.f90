!#==============================================================================
!#
!#  SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
!#  http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!#
!#  Copyright 2007-2016 Intel Corporation
!#
!#  THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED,
!#  INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY,
!#  FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL
!#  PROPERTY RIGHTS.
!#
!#==============================================================================
!#
!#
!#******************************************************************************
!# Content:
!#      MODULE ADOBJECTS
!#******************************************************************************


	MODULE ADOBJECTS
	IMPLICIT NONE

    INTEGER, PARAMETER :: K_OBJ = INT_PTR_KIND()
  ! Object Pointers

	  INTEGER(K_OBJ) excelapp
	  INTEGER(K_OBJ) workbooks
	  INTEGER(K_OBJ) workbook
	  INTEGER(K_OBJ) worksheet
	  INTEGER(K_OBJ) range
	  INTEGER(K_OBJ) charts
	  INTEGER(K_OBJ) chart
	  INTEGER(K_OBJ), DIMENSION(1:12) :: cells
	  INTEGER(K_OBJ) categoryAxis
	  INTEGER(K_OBJ) valueAxis

  ! BSTRs
	  INTEGER(K_OBJ) bstr1
	  INTEGER(K_OBJ) bstr2
	  INTEGER(K_OBJ) bstr3

	CONTAINS

	  SUBROUTINE INITOBJECTS()
	
		  USE IFCOM
	
		  INTEGER i;

		  excelapp = 0
		  workbooks = 0
		  workbook = 0
		  worksheet = 0
		  range = 0
		  charts = 0
		  chart = 0
		  DO i=1,12
			cells(i) = 0
		  END DO
		  categoryAxis = 0
		  valueAxis = 0

		  bstr1 = 0
		  bstr2 = 0
		  bstr3 = 0
	
	  END SUBROUTINE
	  
	  SUBROUTINE RELEASEOBJECTS()
	
		  USE IFCOM
	
		  INTEGER status
		  INTEGER i

		  IF (range /= 0) status = COMRELEASEOBJECT ( range )
		  IF (chart /= 0) status = COMRELEASEOBJECT ( chart )
		  IF (charts /= 0) status = COMRELEASEOBJECT ( charts )
		  IF (worksheet /= 0) status = COMRELEASEOBJECT ( worksheet )
		  IF (workbook /= 0) status = COMRELEASEOBJECT ( workbook )
		  IF (workbooks /= 0) status = COMRELEASEOBJECT ( workbooks )
		  DO i=1,12
			  IF (cells(i) /= 0) status = COMRELEASEOBJECT ( cells(i) )
		  END DO
		  IF (categoryAxis /= 0) status = COMRELEASEOBJECT ( categoryAxis )
		  IF (valueAxis /= 0) status = COMRELEASEOBJECT ( valueAxis )
		  IF (excelapp /= 0) status = COMRELEASEOBJECT ( excelapp )

		  IF (bstr1 /= 0) CALL SysFreeString(bstr1)
		  IF (bstr2 /= 0) CALL SysFreeString(bstr2)
		  IF (bstr3 /= 0) CALL SysFreeString(bstr3)
	
	  END SUBROUTINE


	END MODULE