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
! Module file for Calendar QuickWin sample program
!

module CALNMOD

INTEGER(4), Parameter :: BackColor=2
INTEGER(4), Parameter :: TextColor=#FFFFFF   !RGB Color in (B G R order)
INTEGER(4), Parameter :: GridColor=#FFFFFF   !RGB Color in (B G R order)
INTEGER(4), Parameter :: WeekendColor=#0000FF   !RGB Color in (B G R order)
INTEGER(4), Parameter :: WeekdayColor=#00FF00   !RGB Color in (B G R order)


INTEGER(2) ,Parameter :: ColX=100
INTEGER(2) ,Parameter :: ColY=220
INTEGER(2) ,Parameter :: ColXOff = 30
INTEGER(2) ,Parameter :: ColYOff = 30




type table 
	integer(2) numx, numy
end type

! Calendar(i) points the upper left corner 
Type(table) :: Calendar(7) = &
		(/ table(30, ColY), table(30+ColXOff,ColY), &
		   table(30+ColXOff*2, ColY), table(30+ColXOff*3,ColY), &
		   table(30+ColXOff*4, ColY), table(30+ColXOff*5,ColY), &
		   table(30+ColXOff*6, ColY)   /)


CHARACTER(2) :: DayString(31) = &
				(/ ' 1',' 2',' 3',' 4',' 5', &
				   ' 6',' 7',' 8',' 9','10', &
				   '11','12','13','14','15', &
				   '16','17','18','19','20', &
				   '21','22','23','24','25', &
				   '26','27','28','29','30','31' /)

INTEGER(2) :: EndOfMonth(12) = &
				(/ 31, 28, 31, 30, 31, &
				   30, 31, 31, 30, 31, &
				   30, 31 /)

end module
