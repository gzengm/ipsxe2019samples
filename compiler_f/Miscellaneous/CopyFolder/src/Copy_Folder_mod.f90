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
! A routine to copy the contents of one file folder to another,
! with any depth of folders. If any destination folder does not
! exist, it is created. If a target file exists, it is overwritten.
!
! Demonstrates the use of Win32 APIs for file manipulation and
! Fortran recursion
!
!
module Copy_Folder_Mod

contains

! Routine Copy_Folder copies an entire folder and all its contents
! to another folder
!
! Calling sequence:
!
!  ret = Copy_Folder (source_path, dest_path)
!
! Arguments:
!
!  source_path - CHARACTER(*), INTENT(IN) : complete file path
!     of folder to be copied.  The path must end in a folder name
!
!  dest_path - CHARACTER(*), INTENT(IN) : complete file path
!     of destination folder.  If it does not exist, it will be
!     created
!
! Return value:
!
!  Default integer with status of the operation.  The value is 0 if 
!    successful, otherwise it may be a Win32 error code
!
recursive integer function Copy_Folder (source_path, dest_path)
use kernel32
implicit none
character*(*), intent(in) :: source_path, dest_path
character*(MAX_PATH) :: old_file, new_file

type (T_WIN32_FIND_DATA) :: find_data
integer(HANDLE) fhandle
integer ret, spos, npos
logical lret

! Find the source_path and verify that it is a directory (folder)
!
fhandle = FindFirstFile (source_path//""C, find_data)
if (fhandle == INVALID_HANDLE_VALUE) then
  Copy_Folder = GetLastError ()
  return
  end if
ret = FindClose (fhandle) ! Don't need this one anymore
if (iand(find_data%dwFileAttributes, FILE_ATTRIBUTE_DIRECTORY) == 0) then
  Copy_Folder = ERROR_PATH_NOT_FOUND  ! Indicates not a directory
  return
  end if

! Create the target path directory.  We don't validate
! the output directory name.
!
new_file = dest_path // "\\"C
ret = CreateDirectory (new_file, NULL)
if (ret == 0) then
  Copy_Folder = GetLastError ()
  if (Copy_Folder /= ERROR_ALREADY_EXISTS) return
  end if

! Find all of the files in this folder
!
fhandle = FindFirstFile (source_path//"\\*"C, find_data)
if (fhandle == INVALID_HANDLE_VALUE) then
  ret = GetLastError ()
  if (ret == ERROR_FILE_NOT_FOUND) then
    ! No files in this folder - return
    Copy_Folder = 0
    return
    end if
  Copy_Folder = ret  ! Error
  return
  end if

! Loop processing files
!
DO
  npos = index (find_data%cFileName, CHAR(0)) - 1 ! Length
  old_file = source_path // "\" // find_data%cFileName(1:npos)
  new_file = dest_path // "\" // find_data%cFileName(1:npos)
  if (find_data%cFileName(1:1) /= ".") then
      ! Is this a directory?  If so, recurse
      !
      if (iand(find_data%dwFileAttributes, FILE_ATTRIBUTE_DIRECTORY) /= 0) then

        ret = Copy_Folder (trim(old_file), trim(new_file))
        if (ret /= 0) then
          Copy_Folder = ret
          ret = FindClose (fhandle)
          return
          end if
      else

        ! Copy this file
        !
        ! Set the attributes of the target file to normal. It may not exist
        ! in which case we'll ignore the error.
        ret = SetFileAttributes (trim(new_file)//""C, FILE_ATTRIBUTE_NORMAL)
        ! Do the file copy.  The third argument being FALSE means that we want to
        ! overwrite any existing file.
        ret = CopyFile (trim(old_file)//""C, trim(new_file)//""C, FALSE)
        if (ret == 0) then
          Copy_Folder = GetLastError ()
          ret = FindClose (fhandle)
          return
          end if
        ret = SetFileAttributes (trim(new_file)//""C, FILE_ATTRIBUTE_NORMAL)
        end if
    end if
  ! Find next file
  !
  ret = FindNextFile (fhandle, find_data)
  if (ret == 0) then
    ret = GetLastError ()
    if (ret == ERROR_NO_MORE_FILES) exit  ! Exit loop
    Copy_Folder = ret
    ret = FindClose (fhandle)
    return
    end if
  end do

! Done with this folder
!
ret = FindClose (fhandle)
Copy_Folder = 0
return
end function Copy_Folder

end module Copy_Folder_Mod