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

! ProcessorInfo.f90
!
! Demonstrates the Windows API GetLogicalProcessorInformation, adapted from a Microsoft Developer
! Network example program.  This program also demonstrates the following Fortran 2003/2008 features:
! - Procedure pointers (F2003)
! - C_F_PROCPOINTER (F2003)
! - C_SIZEOF (F2008)
! - POPCNT intrinsic (F2008)
! - Unlimited format repeat count (F2008)
! - ERROR STOP (F2008)
!
! February 7, 2013 - Windows XP 64-bit and Windows Server 2003 don't return information
!                    on Hyperthreading. Add code to warn about that.
!**************************************************************************** 
program ProcessorInfo
    use, intrinsic :: ISO_C_BINDING
    use kernel32

    implicit none
    
    ! Variables
    procedure(GetLogicalProcessorInformation), pointer :: glpi
    type(T_SYSTEM_LOGICAL_PROCESSOR_INFORMATION), allocatable, dimension(:) :: buffer
    integer(DWORD) :: returnLength = 0
    integer :: logicalProcessorCount = 0
    integer :: numaNodeCount = 0
    integer :: processorCoreCount = 0
    integer :: processorCacheCount(3) = [0,0,0]
    integer :: processorPackageCount = 0
    integer(DWORD) :: ret
    integer :: nlpi, lpi_element_length, i
    integer(1), pointer, dimension(:) :: dumpbuf
    
    ! MSDN says that because GetLogicalProcessorInformation is not supported on all versions
    ! of Windows, it suggests getting the address dynamically.  We'll do that here, though
    ! in reality it should not be necessary. The following statement uses only Fortran standard
    ! syntax - it would be a bit simpler to use the integer pointer extension, but this makes a 
    ! good example.
    !
    ! The steps here are:
    ! 1. Call GetModuleHandle to get a handle to the kernel32 DLL which will already be loaded in this image.
    !    Note that this is not the same as LoadLibrary, which assumes that a DLL is not already loaded.
    ! 2. Call GetProcAddress to get the address of GetLogicalProcessorInformation
    ! 3. Use TRANSFER to convert that address to a C_FUNPTR
    ! 4. Use C_F_PROCPOINTER to convert the C_FUNPTR to a Fortran procedure pointer
    
    call c_f_procpointer( &
        transfer( &
            GetProcAddress( &
                GetModuleHandle("kernel32"//C_NULL_CHAR), &
                "GetLogicalProcessorInformation"//C_NULL_CHAR &
                ), &
            C_NULL_FUNPTR &
            ), &
        glpi)
    
    if (.not. associated(glpi)) then
        print *, "GetLogicalProcessorInformation not supported"
        error stop
    end if
     
    ! We don't know in advance the size of the buffer we need. We'll pick a number, allocate it,
    ! and see if that's sufficient.  If not, we'll use the returned size information and reallocate
    ! the buffer to the required size.
    
    allocate (buffer(20))
    lpi_element_length = C_SIZEOF(buffer(1))
    returnLength = C_SIZEOF(buffer)
    print *, "Trying buffer length of ", returnLength
    ret = glpi(buffer, returnLength)
    print *, "Required buffer length is ", returnLength
    if (ret == FALSE) then ! Failed
        if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) then
            deallocate (buffer)
            allocate (buffer(returnLength/lpi_element_length))
            ret = glpi(buffer, returnLength)
            print *, "Reallocated buffer length to ", returnLength
            if (ret == FALSE) then
                print *, "GetLogicalProcessorInformation call failed with error code ", GetLastError()
                error stop
            end if
        else
            print *, "GetLogicalProcessorInformation call failed with error code ", GetLastError()
            error stop
        end if
    end if
    

    ! Now we can iterate through the elements of buffer and see what we can see
    
    do i=1, returnLength / lpi_element_length ! Number of elements in buffer
        select case (buffer(i)%Relationship)
        case(RelationNumaNode)
            ! NUMA nodes return one record of this type
            numaNodeCount = numaNodeCount + 1
        
        case(RelationProcessorCore)
            if (buffer(i)%Flags == 1) then
                processorCoreCount = processorCoreCount + 1
            else
                ! Pre Windows Vista
                processorCoreCount = processorCoreCount + popcnt(buffer(i)%processorMask)
            end if
                            
            ! A Hyperthreaded core supplies more than one logical processor
            logicalProcessorCount = logicalProcessorCount + popcnt(buffer(i)%processorMask)
            
        case(RelationCache)
            ! One cache descriptor for each cache
            if (buffer(i)%Cache%Level > 0 .and. buffer(i)%Cache%Level <= 3) then
                processorCacheCount(buffer(i)%Cache%Level) = processorCacheCount(buffer(i)%Cache%Level) + 1
            else
                print *, "Invalid processor cache level ", buffer(i)%Cache%Level
            end if
            
        case(RelationProcessorPackage)
            !Logical processors share a physical package (socket)
            processorPackageCount = processorPackageCount + 1
            
        case default
            print *, "Unrecognized relationship code ", buffer(i)%Relationship
            
        end select
    end do
    
        
    ! Display the information we collected
        
    print '(A)', "GetLogicalProcessorInformation results:"
    if (processorPackageCount == 0) then
      print '(A)', "  This operating system does not return processor package count nor", &
        & "    information about Hyperthreading"
      processorCoreCount = logicalProcessorCount
    else
      print '(A,I0)',"  Number of physical processor packages: ", processorPackageCount
    end if
    print '(A,I0)',"  Number of NUMA nodes: ", numaNodeCount
    print '(A,I0)',"  Number of processor cores: ", processorCoreCount
    print '(A,I0)',"  Number of logical processors: ", logicalProcessorCount
    print '(A,*(I0,:,"/"))',"  Number of processor L1/L2/L3 caches: ",processorCacheCount

    end program ProcessorInfo