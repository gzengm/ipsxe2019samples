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
program Decrypt
use advapi32
use kernel32
use ifport, only: GETFILEINFOQQ, FILE$INFO, FILE$FIRST, FILE$LAST
implicit none

integer(DWORD), parameter :: MY_ENCODING_TYPE = IOR(PKCS_7_ASN_ENCODING,X509_ASN_ENCODING)
integer(DWORD), parameter :: ENCRYPT_ALGORITHM = CALG_RC2
integer(DWORD), parameter :: ENCRYPT_BLOCK_SIZE = 8

character*(MAX_PATH) :: source, dest, password

write (*,*) "Decrypt a file"
write (*,101,advance='NO') "Enter the name of the file to be decrypted: "
101 format(1x,A)
read (*,102,END=999,ERR=999) source
102 format (A)
write (*,101,advance='NO') "Enter the name of the output file: "
read (*,102,END=999,ERR=999) dest
write (*,101,advance='NO') "Enter password (ENTER for none): "
read (*,102,END=999,ERR=999) password

if (password == "") &
  write (*,*) "The key will be generated without a password."

write (*,*)

if (Decrypt_File (trim(source), trim(dest), trim(password))) then
  write (*,*) "Decryption successful!"
else
  write (*,*) "Decryption failed!"
  end if

999 continue

contains

logical function Decrypt_File (Source, Destination, Password)
implicit none
character*(*), intent(in) :: Source, Destination, Password

integer(HANDLE) :: hCryptProv
integer(HANDLE) :: hKey
integer(HANDLE) :: hXchgKey
integer(HANDLE) :: hHash

pointer (pbKeyBlob, KeyBlob)
integer(BYTE),DIMENSION(*) :: KeyBlob
integer(DWORD) :: dwKeyBlobLen

pointer (pbBuffer, Buffer)
integer(BYTE), dimension(*) :: Buffer
integer(DWORD) :: dwBufferLen

integer(DWORD) :: dwBlockLen
integer(DWORD) :: dwCount
integer(BOOL) :: Final

type(FILE$INFO) :: FileInfo
integer(ULONG_PTR) :: hFileInfo
integer(DWORD) :: ret
integer(DWORD) :: BytesRemaining

Decrypt_File = .false.
hCryptProv = NULL
hKey = NULL
hXchgKey = NULL
hHash = NULL

! Get the size of the source file in bytes
!
hFileInfo = FILE$FIRST
ret = GETFILEINFOQQ (Source, FileInfo, hFileInfo)
if (ret == 0) then
  write (*,*) "File not found"
  return 
  end if
BytesRemaining = FileInfo%length
! Keep calling GETFILEINFOQQ until no more files
! otherwise resources may be lost
!
do while (hFileInfo /= FILE$LAST)
  ret = GETFILEINFOQQ (Source, FileInfo, hFileInfo)
  end do

! Open Source
!
open (unit=1,file=Source,form='BINARY',status='OLD',iostat=ret)
if (ret /= 0) then
  write (*,*) "Error",ret," opening ",trim(Source)
  return 
  end if

! Open Destination
!
open (unit=2,file=Destination,form='BINARY',status='REPLACE',iostat=ret)
if (ret /= 0) then
  write (*,*) "Error",ret," creating ",trim(Destination)
  return 
  end if

! Get handle to the default provider
!

ret = CryptAcquireContext( &
      hCryptProv, &
      NULL, &
      NULL, &
      PROV_RSA_FULL, &
      0)
if (ret == 0) then
  ! See if the problem is that there is no key container
  ! If not, create a new one
  !
  ret = GetLastError()
  if (ret == NTE_BAD_KEYSET) then
    ret = CryptAcquireContext( &
      hCryptProv, &
      NULL, &
      NULL, &
      PROV_RSA_FULL, &
      CRYPT_NEWKEYSET)
	end if
  if (ret == 0) then
    call handle_error ("Error during CryptAcquireContext!")
    return
    end if
  end if

! Create the session key
!

if (Password == "") then

  ! Read key blob length from source file and allocate memory.
  !
  read (1,iostat=ret) dwKeyBlobLen
  if (ret /= 0) then
    write (*,*) "Error ",ret," reading file header!"
	return
	end if

  pbKeyBlob = malloc(dwKeyBlobLen)
  if (pbKeyBlob == NULL) then
    write (*,*) "Memory allocation error."
	return
	end if

  ! Read key blob from source file
  !
  read (1,iostat=ret) KeyBlob(1:dwKeyBlobLen)
  if (ret /= 0) then
    write (*,*) "Error ",ret," reading key blob!"
	return
	end if

  ! Import key blob into CSP
  !
  ret = CryptImportKey ( &
      hCryptProv, &
      KeyBlob, &
      dwKeyBlobLen, &
      0_ULONG_PTR, &
      0, &
      hKey)
  if (ret == FALSE) then
    call handle_error ("Error during CryptImportKey!")
	return
	end if

  ! Free the blob!  (Where's Steve McQueen when we need him?)
  !
  call free (pbKeyBlob)

else
  ! Decrypt the file with a session key derived from a password
  !

  ! Create a hash object
  !
  ret = CryptCreateHash ( &
       hCryptProv, &
       CALG_MD5, &
       0_ULONG_PTR, &
       0, &
       hHash)
  if (ret == FALSE) then
    call handle_error ("Error during CryptCreateHash!")
	return
	end if
 
  ! Hash the password
  !
  ret = CryptHashData ( &
       hHash, &
       %ref(Password), &
       len(Password), &
       0)
  if (ret == FALSE) then
    call handle_error ("Error during CryptHashData!")
	return
	end if

  ! Derive a session key from the hash object
  !
  ret = CryptDeriveKey ( &
       hCryptProv, &
       ENCRYPT_ALGORITHM, &
       hHash, &
       0, &
       hKey)

  if (ret == FALSE) then
    call handle_error ("Error during CryptDeriveKey!")
	return
	end if
 
  ! Destroy the hash object
  !

  ret = CryptDestroyHash(hHash); 
  hHash = NULL; 

  end if  

! The decryption key is now available, either having been imported
! from a blob read in from the source file or having been created 
! using the password. This point in the program is not reached if 
! the decryption key is not available.
!

! Determine number of bytes to decrypt at a time.  This must be a
! multiple of ENCRYPT_BLOCK_SIZE.  ENCRYPT_BLOCK_SIZE is set
! as a PARAMETER constant.
!
dwBlockLen = 1000 - MOD(1000,ENCRYPT_BLOCK_SIZE)
dwBufferLen = dwBlockLen

! Allocate memory
!
pbBuffer = malloc(dwBufferLen)
if (pbBuffer == 0) then
  write (*,*) "Out of memory!"
  return
  end if

! In a DO loop, read from the source file, decrypt the
! data, and write to the destination file.
!
Final = FALSE
do while (BytesRemaining > 0)

  ! Read up to dwBlockLen bytes from the source file.
  !
  dwCount = min(BytesRemaining, dwBlockLen)
  BytesRemaining = BytesRemaining - dwCount
  if (BytesRemaining == 0) Final = TRUE
  read (1,iostat=ret) buffer(1:dwCount)
  if (ret /= 0) then
    write (*,*) "Error ",ret," reading ciphertext!"
	return
	end if

  ! Decrypt data
  !
  ret = CryptDecrypt (&
     hKey, &
     0_ULONG_PTR, &
     Final, &
     0, &
     Buffer, &
     dwCount)
  if (ret == FALSE) then
    call handle_error ("Error during CryptDecrypt!")
	return
	end if

  ! Write decrypted data to the destination file
  ! Note that dwCount is modified by CryptDecrypt
  ! and may be smaller than the amount of data decrypted.
  !
  write (2,iostat=ret) buffer(1:dwCount)
  if (ret /= 0) then
    write (*,*) "Error ",ret," writing plaintext!"
	return
	end if

  end do

! Close files
!
close (1)
close (2)

! Free memory
!
call free (pbBuffer)

! Destroy session key
!
if(hKey /= NULL) ret = CryptDestroyKey(hKey)

! Release key exchange key handle
!
if (hXchgKey /= NULL) ret = CryptDestroyKey(hXchgKey)

! Destroy hash object
!
if (hHash /= NULL) ret = CryptDestroyHash(hHash)

! Release provider handle
!
if (hCryptProv /= NULL) ret = CryptReleaseContext (hCryptProv, 0)
 
Decrypt_File = .true.
return
end function Decrypt_File

subroutine handle_error (msg)
implicit none
character*(*) msg
character*8 cerr
integer ierr

ierr = GetLastError ()
write (cerr,'(Z8.8)') ierr
write (*,*) msg
write (*,*) "Error code is ",cerr
return 
end subroutine handle_error

end program Decrypt
