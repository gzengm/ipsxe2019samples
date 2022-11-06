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
!#      Puzzle - Cube sample program for Intel Visual Fortran
!#******************************************************************************
!

subroutine cubeInit()
    use ifopngl
    use sharedata
    implicit none
    integer i, k, seed(8)
    real(8) r8

    real(4):: glfLightAmbient(4) = (/ 0.3, 0.3, 0.3, 1.0 /)
    real(4):: glfLightDiffuse(4) = (/ 0.6, 0.6, 0.6, 1.0 /)
    real(4):: glfLightSpecular(4) = (/ 0.6, 0.6, 0.6, 1.0 /)

    ! ---- Set random seed ------
    call random_seed(size=k)
    call date_and_time(values=seed)
    seed(1) = seed(7)+(seed(6)+(seed(5)+seed(3)*24)*60)*60
    call random_seed(put=seed(1:k))

    ! ---- Initialize command stack ------
    allocate(cmdStack%buf(1:250))
    cmdStack%size = 250
    cmdStack%top = 0
    cmdStack%count = 0

    call fglEnable(GL_DEPTH_TEST)
    call fglEnable(GL_CULL_FACE)
    call fglClearColor(0.9, 0.8, 0.6, 0.2)

    ! ---- Lighting and shading ---
    !call fglLightfv(GL_LIGHT0, GL_AMBIENT, loc(glfLightAmbient))
    !call fglLightfv(GL_LIGHT0, GL_DIFFUSE, loc(glfLightDiffuse))
    !call fglLightfv(GL_LIGHT0, GL_SPECULAR, loc(glfLightSpecular))
    !call fglEnable(GL_LIGHTING)
    !call fglEnable(GL_LIGHT0)

    ! ---- Initialize sin/cos table ---
    do i=1, 89
        r8 = real(i,8)
        g_cos_tab(i) = dcosd(r8)
        g_sin_tab(i) = dsind(r8)
    enddo
    g_cos_tab(0) = 1.0
    g_cos_tab(90) = 0.0
    g_sin_tab(0) = 0.0
    g_sin_tab(90) = 1.0

    g_identity = 0
    do i=1, 4
	g_identity(i,i) = 1.0
    enddo

    ! ---- create draw list for stand cube ----
    call CreateCubeDrawList()
end

! ---- Initialize cube info --------------------------
subroutine initCubeInfo()
    use sharedata
    implicit none
    integer i, j, k
    do i=1, 3
        do j=1, 3
            do k=1, 3
               g_cube(i, j, k)%ort = g_identity
            enddo
        enddo
    enddo
    do i=1, 3
        do j=1, 3
            do k=1, 3
               g_slot(i, j, k)%cId = (/ i, j, k/)
            enddo
        enddo
    enddo
    cmdStack%top = 0
    cmdStack%count = 0
end

! ---------------------------------------------------------
subroutine newCubes(hDc)
    use ifopngl
    use ifwinty, only: HANDLE
    implicit none
    integer(HANDLE) hDC

    call fglClear(or(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT))
    call initCubeInfo()  ! clear cube info

    ! ---- load matrix
    call fglMatrixMode(GL_MODELVIEW)
    call fglLoadIdentity()

    ! ---- Set view point ----
    call fglTranslatef(0.0, 0.0, -42.0)
    call fglRotatef(30.0, 1.0, 0.0, 0.0)
    call fglRotatef(-30.0, 0.0, 1.0, 0.0)
    call renderCubes(hDC)
end


! ---------------------------------------------------------
subroutine randomRotate(hDc)
    use ifopngl
    use sharedata
    use ifwinty, only: HANDLE
    implicit none
    integer(HANDLE) hDC
    integer nn, mm
    real(4) rn, deg
    integer cmd, i, times

    call random_number(rn)
    times = mod(int(rn*10000),11)+18
    call fglMatrixMode(GL_MODELVIEW)
    do i=1, times
        call random_number(rn)
	nn = mod(int(rn*10000), 18)  !  0 --> 17
        mm = (nn / 6)
	select case (mm)
	case (0)
	    cmd = G_ROTATE_X
	case (1)
	    cmd = G_ROTATE_Y
	case (2)
	    cmd = G_ROTATE_Z
	case default
	    cmd = 0 ! Should never happen
	end select
	mm = mod(nn, 6)+1  !  now 1 --> 6
	if (mm .ge. 4) then
	    mm = mm + 1
	endif
	cmd = or(cmd, mm)
	call rotateAndPush(hDC, cmd)
    call fglClear(ior(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT))
	!  --- Rotate view point ---
	! if (mod(i, 5) .eq. 0) then
  	!     call glRotatef(10.0, 0.0, 1.0, 0.0)
    ! endif
    	call renderCubes(hDC)
    enddo
end

! ---------------------------------------------------------
subroutine undoCmdStack(hDC)
    use sharedata
    use ifwinty
    integer(HANDLE) hDC
    integer cmd
    call popCmd(cmd)
    do while (cmd .ne. 0)
	cmd = ieor(cmd, G_ROTATE_CW)
	call rotateCommander(hDC, cmd)
	call popCmd(cmd)
    end do
end

! ---------------------------------------------------------
subroutine rotateAndPush(hDC, cmd)
    use ifwinty, only: HANDLE
    integer(HANDLE) hDC
    integer cmd
    call pushCmd(cmd)
    call rotateCommander(hDC, cmd)
end

! ---------------------------------------------------------
subroutine rotateCommander(hDC, cmd)
    use sharedata
    use ifopngl
    use ifwinty, only: HANDLE
    implicit none
    integer(HANDLE) hDC
    integer cmd, x, y, z, cw, i, steps
    real angle
    real, save:: gear(6)=(/5.0, 10.0, 15.0, 30.0, 45.0, 90.0 /)

    x = 0
    y = 0
    z = 0
    if (and(cmd, G_ROTATE_X) .eq. G_ROTATE_X) then
	x = and(cmd, G_ROTATE_LEV)
    endif
    if (and(cmd, G_ROTATE_Y) .eq. G_ROTATE_Y) then
	y = and(cmd, G_ROTATE_LEV)
    endif
    if (and(cmd, G_ROTATE_Z) .eq. G_ROTATE_Z) then
	z = and(cmd, G_ROTATE_LEV)
    endif
    cw = and(cmd, G_ROTATE_CW)
    if (cw .eq. 0) then
	angle = gear(g_rSpeed)
    else
	angle = -gear(g_rSpeed)
    endif
    steps = 90.0 / gear(g_rSpeed)

    call fglMatrixMode(GL_MODELVIEW)
    do i=1, steps
    	call rotateOneLevel(x, y, z, angle)
    	call fglClear(ior(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT))
    	call renderCubes(hDC)
    enddo
    call updateSlot(x, y, z, cw)
end

! -------------------------------------------------
! --- Assert: only one of x, y, or z is nozero ---
! --- Assert: -90 <= degree <= 90 -------------------
subroutine rotateOneLevel(x, y, z, angle)
    use sharedata
    implicit none
    integer x, y, z, ccw, angx, idx(3), j, k
    real angle
    real matrx(4,4)

    if (angle < 0) then
	ccw = -1
	angx = int(-angle)
    else
	ccw = 1
	angx = int(angle)
    endif
    matrx = g_identity
    if (x .gt. 0) then
	matrx(2,2) = g_cos_tab(angx)
	matrx(3,3) = g_cos_tab(angx)
	matrx(2,3) = -g_sin_tab(angx)*ccw
	matrx(3,2) = g_sin_tab(angx)*ccw
    	do j=1, 3
            do k=1, 3
		idx = g_slot(x,j,k)%cId
	    	g_cube(idx(1), idx(2), idx(3))%ort =  &
			matmul(matrx, g_cube(idx(1),idx(2),idx(3))%ort)
            enddo
    	enddo
    else if (y .gt. 0) then
	matrx(1,1) = g_cos_tab(angx)
	matrx(3,3) = g_cos_tab(angx)
	matrx(1,3) = g_sin_tab(angx)*ccw
	matrx(3,1) = -g_sin_tab(angx)*ccw
    	do j=1, 3
            do k=1, 3
		idx = g_slot(j,y,k)%cId
	    	g_cube(idx(1), idx(2), idx(3))%ort =  &
			matmul(matrx, g_cube(idx(1),idx(2),idx(3))%ort)
            enddo
    	enddo
    else if (z .gt. 0) then
	matrx(1,1) = g_cos_tab(angx)
	matrx(2,2) = g_cos_tab(angx)
	matrx(1,2) = -g_sin_tab(angx)*ccw
	matrx(2,1) = g_sin_tab(angx)*ccw
    	do j=1, 3
            do k=1, 3
		idx = g_slot(j,k,z)%cId
	    	g_cube(idx(1), idx(2), idx(3))%ort =  &
			matmul(matrx, g_cube(idx(1),idx(2),idx(3))%ort)
            enddo
	enddo
    endif
end


! -------------------------------------------------
subroutine updateSlot(x, y, z, cw)
    use sharedata
    integer x, y, z, cw
    integer buf(3)

    if (x .gt. 0) then
	if (cw .ne. 0) then
	    buf = g_slot(x, 1, 1)%cId
	    g_slot(x, 1, 1)%cId = g_slot(x, 3, 1)%cId
	    g_slot(x, 3, 1)%cId = g_slot(x, 3, 3)%cId
	    g_slot(x, 3, 3)%cId = g_slot(x, 1, 3)%cId
	    g_slot(x, 1, 3)%cId = buf
	    buf = g_slot(x, 2, 1)%cId
	    g_slot(x, 2, 1)%cId = g_slot(x, 3, 2)%cId
	    g_slot(x, 3, 2)%cId = g_slot(x, 2, 3)%cId
	    g_slot(x, 2, 3)%cId = g_slot(x, 1, 2)%cId
	    g_slot(x, 1, 2)%cId = buf
	else
	    buf = g_slot(x, 1, 1)%cId
	    g_slot(x, 1, 1)%cId = g_slot(x, 1, 3)%cId
	    g_slot(x, 1, 3)%cId = g_slot(x, 3, 3)%cId
	    g_slot(x, 3, 3)%cId = g_slot(x, 3, 1)%cId
	    g_slot(x, 3, 1)%cId = buf
	    buf = g_slot(x, 2, 1)%cId
	    g_slot(x, 2, 1)%cId = g_slot(x, 1, 2)%cId
	    g_slot(x, 1, 2)%cId = g_slot(x, 2, 3)%cId
	    g_slot(x, 2, 3)%cId = g_slot(x, 3, 2)%cId
	    g_slot(x, 3, 2)%cId = buf
	endif
    else if (y .gt. 0) then
	if (cw .eq. 0) then
	    buf = g_slot(1, y, 1)%cId
	    g_slot(1, y, 1)%cId = g_slot(3, y, 1)%cId
	    g_slot(3, y, 1)%cId = g_slot(3, y, 3)%cId
	    g_slot(3, y, 3)%cId = g_slot(1, y, 3)%cId
	    g_slot(1, y, 3)%cId = buf
	    buf = g_slot(2, y, 1)%cId
	    g_slot(2, y, 1)%cId = g_slot(3, y, 2)%cId
	    g_slot(3, y, 2)%cId = g_slot(2, y, 3)%cId
	    g_slot(2, y, 3)%cId = g_slot(1, y, 2)%cId
	    g_slot(1, y, 2)%cId = buf
	else
	    buf = g_slot(1, y, 1)%cId
	    g_slot(1, y, 1)%cId = g_slot(1, y, 3)%cId
	    g_slot(1, y, 3)%cId = g_slot(3, y, 3)%cId
	    g_slot(3, y, 3)%cId = g_slot(3, y, 1)%cId
	    g_slot(3, y, 1)%cId = buf
	    buf = g_slot(2, y, 1)%cId
	    g_slot(2, y, 1)%cId = g_slot(1, y, 2)%cId
	    g_slot(1, y, 2)%cId = g_slot(2, y, 3)%cId
	    g_slot(2, y, 3)%cId = g_slot(3, y, 2)%cId
	    g_slot(3, y, 2)%cId = buf
	endif
    else if (z .gt. 0) then
	if (cw .ne. 0) then
	    buf = g_slot(1, 1, z)%cId
	    g_slot(1, 1, z)%cId = g_slot(3, 1, z)%cId
	    g_slot(3, 1, z)%cId = g_slot(3, 3, z)%cId
	    g_slot(3, 3, z)%cId = g_slot(1, 3, z)%cId
	    g_slot(1, 3, z)%cId = buf
	    buf = g_slot(2, 1, z)%cId
	    g_slot(2, 1, z)%cId = g_slot(3, 2, z)%cId
	    g_slot(3, 2, z)%cId = g_slot(2, 3, z)%cId
	    g_slot(2, 3, z)%cId = g_slot(1, 2, z)%cId
	    g_slot(1, 2, z)%cId = buf
	else
	    buf = g_slot(1, 1, z)%cId
	    g_slot(1, 1, z)%cId = g_slot(1, 3, z)%cId
	    g_slot(1, 3, z)%cId = g_slot(3, 3, z)%cId
	    g_slot(3, 3, z)%cId = g_slot(3, 1, z)%cId
	    g_slot(3, 1, z)%cId = buf
	    buf = g_slot(2, 1, z)%cId
	    g_slot(2, 1, z)%cId = g_slot(1, 2, z)%cId
	    g_slot(1, 2, z)%cId = g_slot(2, 3, z)%cId
	    g_slot(2, 3, z)%cId = g_slot(3, 2, z)%cId
	    g_slot(3, 2, z)%cId = buf
	endif
    endif
end

! ------------------------------------------------------
subroutine reCube(hDC)
    use ifopngl
    use ifwinty, only: HANDLE
    implicit none
    integer(HANDLE) hDC

    call fglClear(ior(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT))
    call renderCubes(hDC)
end

! -------------------------------------------------
subroutine reshape(width, length)
    use ifopngl
    use sharedata
    integer width, length
    real(8) aspect

    aspect = width
    aspect = aspect / length
    call fglMatrixMode(GL_PROJECTION)
    call fglLoadIdentity()
    call fgluPerspective(g_vAngle, aspect, 1.0_8, 70.0_8)
    call fglViewPort(0, 0, width, length)
    call fglMatrixMode(GL_MODELVIEW)
end

! --------------------------------------------------
subroutine RotateStep(hDC, x, y, z, degree)
    use ifopngl
    use ifwinty, only: HANDLE
    implicit none
    integer(HANDLE) hDC
    real(4) x, y, z, degree

    call fglMatrixMode(GL_MODELVIEW)
    call fglClear(or(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT))
    call fglRotatef(degree, x, y, z)
    call renderCubes(hDC)
end


! --------------------------------------------------
subroutine RotateCont(hDC, x, y, z, degree, speed)
    use ifopngl
    use ifwinty, only: HANDLE
    implicit none
    integer(HANDLE) hDC
    integer i
    real(4)  x, y, z, degree, speed

    call fglMatrixMode(GL_MODELVIEW)
    do i=1, int(degree), int(speed)
	call fglClear(or(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT))
	call fglRotatef(speed, x, y, z)
	call renderCubes(hDC)
    enddo
end

! ---------------------------------------------------------
subroutine renderCubes(hDC)
    use ifopngl
    use ifwin
    use sharedata
    implicit none
    integer(HANDLE) hDC
    real(4):: map(3)=(/ -3.0, 0.0, 3.0 /)
    integer i, j, k, ignor

    real(4):: glfMaterialColor(4) = (/ 1.0, 0.0, 0.0, 1.0 /)
   !call fglMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, loc(glfMaterialColor))

    if (g_axisOn .eq. 1) then
	 call drawAxis()
    endif

    do i=1, 3
   	do j=1, 3
            do k=1, 3
		call fglPushMatrix()	
		call fglMultMatrixf(loc(g_cube(i,j,k)%ort))
		call fglTranslatef(map(i), map(j), map(k))
		call fglCallList(1)
		call fglPopMatrix()
      	    enddo
    	enddo
    enddo
    ignor = SwapBuffers(hDC)
end

! ---------------------------------------------
subroutine CreateCubeDrawList()
    use ifopngl
    use sharedata
    real(4) one, mone, tmp(3)

    one = g_cSize
    mone = -g_cSize
    call fglNewList(1, GL_COMPILE)
    call fglColor3f(0.0, 0.0, 1.0)  ! blue
    call fglBegin(GL_POLYGON) ! Front face (CCW)
    	call fglNormal3fv(loc((/ 0.0, 0.0, one/)))
    	call fglVertex3f(one, one, one)
    	call fglVertex3f(mone, one, one)
    	call fglVertex3f(mone, mone, one)
    	call fglVertex3f(one, mone, one)
    call fglEnd()

    call fglColor3f(1.0, 1.0, 0.0)  ! yellow
    call fglBegin(GL_POLYGON)  ! Back-face (CW)
   	call fglNormal3fv(loc((/ 0.0, 0.0, mone /)))
   	call fglVertex3f(one, one, mone)
   	call fglVertex3f(one, mone, mone)
   	call fglVertex3f(mone, mone, mone)
   	call fglVertex3f(mone, one, mone)
    call fglEnd()

    call fglColor3f(1.2, 0.55, 0.06)  ! pink
    call fglBegin(GL_POLYGON)  ! Back-face (CW)
    	call fglNormal3fv(loc((/ mone, 0.0, 0.0 /)))
   	call fglVertex3f(mone, one, one)
   	call fglVertex3f(mone, one, mone)
   	call fglVertex3f(mone, mone, mone)
   	call fglVertex3f(mone, mone, one)
    call fglEnd()

    call fglColor3f(25.3, 0.07, 0.00)  ! red
    call fglBegin(GL_POLYGON)
   	call fglNormal3fv(loc((/ one, 0.0, 0.0 /)))
   	call fglVertex3f(one, one, one)
   	call fglVertex3f(one, mone, one)
   	call fglVertex3f(one, mone, mone)
   	call fglVertex3f(one, one, mone)
    call fglEnd()

    call fglColor3f(0.0, 1.0, 0.0)  ! green
    call fglBegin(GL_POLYGON)
   	call fglNormal3fv(loc((/ 0.0, one, 0.0 /)))
   	call fglVertex3f(mone, one, mone)
   	call fglVertex3f(mone, one, one)
   	call fglVertex3f(one, one, one)
   	call fglVertex3f(one, one, mone)
    call fglEnd()

    call fglColor3f(1.0, 1.0, 1.0)  !  white
    call fglBegin(GL_POLYGON)
   	call fglNormal3fv(loc((/0.0, mone, 0.0 /)))
   	call fglVertex3f(mone, mone, mone)
   	call fglVertex3f(one, mone, mone)
   	call fglVertex3f(one, mone, one)
   	call fglVertex3f(mone, mone, one)
    call fglEnd()
    call fglEndList()
end

! --------------------------------------
subroutine drawAxis()
    use ifopngl
    real w
    w = 3.0
    call fglLineWidth(w)
    call fglBegin(GL_LINES)
    	call fglColor3f(1.0, 0.05, 0.05)
    	call fglVertex3f(0.0, 0.0, 0.0)
    	call fglVertex3f(25.0, 0.0, 0.0)
    call fglEnd()
    call fglBegin(GL_LINES)
	call fglColor3f(0.0, 1.0, 0.0)
	call fglVertex3f(0.0, 0.0, 0.0)
	call fglVertex3f(0.0, 25.0, 0.0)
    call fglEnd()
    call fglBegin(GL_LINES)
	call fglColor3f(0.0, 0.0, 1.0)
	call fglVertex3f(0.0, 0.0, 0.0)
	call fglVertex3f(0.0, 0.0, 25.0)
    call fglEnd()
    w = 1.0
    call fglLineWidth(w)
end

! ---------------------------------------
subroutine pushCmd(cmd)
    use sharedata
    integer cmd

    cmdStack%top = cmdStack%top + 1
    if (cmdStack%top .gt. cmdStack%size) then
	cmdStack%top = 1
    endif
    cmdStack%buf(cmdStack%top) = cmd
    cmdStack%count = cmdStack%count + 1
    if (cmdStack%count .gt. cmdStack%size) then
	! --- The stack is full ----
	cmdStack%count = cmdStack%size
    endif
end

! ---------------------------------------
subroutine popCmd(cmd)
    use sharedata
    integer cmd

    if (cmdStack%count .le. 0) then
	! --- if stack is empty, nothing to pop ---
	cmd = 0
	return
    else
	cmdStack%count = cmdStack%count - 1
	cmd = cmdStack%buf(cmdStack%top)
	cmdStack%top = cmdStack%top - 1
	if (cmdStack%top .le. 0  .and. cmdStack%count .gt. 0) then
	    cmdStack%top = cmdStack%size
	endif
	return
    endif
end

