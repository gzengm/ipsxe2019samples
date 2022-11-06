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

program Encrypt
use advapi32
use kernel32
use ifport, only: GETFILEINFOQQ, FILE$INFO, FILE$FIRST, FILE$LAST
implicit none

integer(DWORD), parameter :: MY_ENCODING_TYPE = IOR(PKCS_7_ASN_ENCODING,X509_ASN_ENCODING)
integer(DWORD), parameter :: ENCRYPT_ALGORITHM = CALG_RC2
integer(DWORD), parameter :: ENCRYPT_BLOCK_SIZE = 8

character(MAX_PATH) :: source, dest, password

write (*,*) "Encrypt a file"
write (*,101,advance='NO') "Enter the name of the file to be encrypted: "
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

if (Encrypt_File (trim(source), trim(dest), trim(password))) then
  write (*,*) "Encryption successful!"
else
  write (*,*) "Encryption failed!"
  end if

999 continue

contains

logical function Encrypt_File (Source, Destination, Password)
implicit none
character(*), intent(in) :: Source, Destination, Password

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

Encrypt_File = .false.
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
! Keep calling GETFILEINFOQQ until no more files,
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
write (*,*) "A cryptographic provider has been acquired."

! Create the session key
!
if (Password == "") then

  ! No password was provided.  Encrypt the file with a random
  ! session key and write the key to the file.
 
  ! Create a random session key
  !
  ret = CryptGenKey ( &
          hCryptProv, &
          ENCRYPT_ALGORITHM, &
          CRYPT_EXPORTABLE, &
          hKey)
  if (ret == 0) then
    call handle_error ("Error during CryptGenKey!");
	return
	end if
  write (*,*) "A session key has been created."
 
  ! Get handle to the encrypter's exchange public key.
  !
  ret = CryptGetUserKey ( &
           hCryptProv, &
           AT_KEYEXCHANGE, &
           hXchgKey)
  if (ret == 0) then
    call handle_error ("User public key is not available and may not exist.")
	return
	end if
  write (*,*) "The user public key has been retrieved."
 
  ! Determine size of the key blob, and allocate memory.
  !
  dwKeyBlobLen = 0
  ret = CryptExportKey ( &
           hKey, &
           hXchgKey, &
           SIMPLEBLOB, &
           0, &
           %val(0), &
           dwKeyBlobLen)
  if (ret == 0) then
    call handle_error ("Error retrieving blob length!")
	return
	end if
  write (*,*) "The key blob is",dwKeyBlobLen," bytes long"

  pbKeyBlob = malloc(dwKeyBlobLen)
  if (pbKeyBlob == 0) then
    write (*,*) "Unable to allocate memory for key blob!"
	return
	end if
  write (*,*) "Memory is allocated for the key blob."
 
  ! Encrypt and export session key into a simple key blob
  !
  ret = CryptExportKey ( &
          hKey, &
          hXchgKey, &
          SIMPLEBLOB, &
          0, &
          %ref(KeyBlob), & ! %ref overrides type checking
          dwKeyBlobLen)
  if (ret == 0) then
    call handle_error ("Error during CryptExportKey!")
	return
	end if
  write (*,*) "The key has been exported."

  ! Release key exchange key handle
  !
  ret = CryptDestroyKey(hXchgKey)
  hXchgKey = NULL;

  ! Write size of key blob to destination file
  !
  write (2,iostat=ret) dwKeyBlobLen
  if (ret /= 0) then
    write (*,*) "Error ",ret," writing header."
	return
	end if
  write (*,*) "A file header has been written."
 
  ! Write key blob to destination file
  !
  write (2,iostat=ret) KeyBlob(1:dwKeyBlobLen)
  if (ret /= 0) then
    write (*,*) "Error ",ret," writing blob."
	return
	end if
  write (*,*) "The key blob has been written to the file."

  ! Free the blob!  (Where's Steve McQueen when we need him?)
  !
  call free (pbKeyBlob)
 
else

  ! The file will be encrypted with a session key derived from a
  ! password.  The session key will be recreated when the file
  ! is decrypted only if the password used to create the key
  ! is available
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
  write (*,*) "A hash object has been created."
 
  ! Hash the password
  !
  ret = CryptHashData ( &
       hHash, &
       %ref(Password), & ! %ref overrides type checking
       len(Password), &
       0)
  if (ret == FALSE) then
    call handle_error ("Error during CryptHashData!")
	return
	end if
  write (*,*) "The password has been added to the hash."

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
  write (*,*) "An encryption key is derived from the password hash."
 
  ! Destroy the hash object
  !
  ret = CryptDestroyHash(hHash); 
  hHash = NULL; 

  end if  

! The session key is now ready.  If it is not a key derived from a
! password, the session key encrypted with the encrypter's private
! key has been written to the destination file.


! Determine number of bytes to encrypt at a time.  This must be a
! multiple of ENCRYPT_BLOCK_SIZE.  ENCRYPT_BLOCK_SIZE is set
! as a PARAMETER constant.
!
dwBlockLen = 1000 - MOD(1000,ENCRYPT_BLOCK_SIZE)

! Determine the block size.  If a block cipher is used,
! it must have room for an extra block.
!
if (ENCRYPT_BLOCK_SIZE > 1) then
  dwBufferLen = dwBlockLen + ENCRYPT_BLOCK_SIZE
else 
  dwBufferLen = dwBlockLen
  end if

! Allocate memory
!
pbBuffer = malloc(dwBufferLen)
if (pbBuffer == 0) then
  write (*,*) "Out of memory!"
  return
  end if
write (*,*) "Memory has been allocated for the buffer."

! In a DO loop, read from the source file, encrypt the
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
    write (*,*) "Error ",ret," reading plaintext!"
	return
	end if

  ! Encrypt data
  !
  ret = CryptEncrypt (&
     hKey, &
     0_ULONG_PTR, &
     Final, &
     0, &
     Buffer, &
     dwCount, &
     dwBufferLen)
  if (ret == FALSE) then
    call handle_error ("Error during CryptEncrypt!")
	return
	end if

  ! Write encrypted data to the destination file
  ! Note that dwCount is modified by CryptEncrypt
  ! and may be larger than the amount of data encrypted.
  !
  write (2,iostat=ret) buffer(1:dwCount)
  if (ret /= 0) then
    write (*,*) "Error ",ret," writing ciphertext!"
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
 
Encrypt_File = .true.
return
end function Encrypt_File

subroutine handle_error (msg)
implicit none
character(*) msg
character(8) cerr
integer ierr

ierr = GetLastError ()
write (cerr,'(Z8.8)') ierr
write (*,*) msg
write (*,*) "Error code is ",cerr
return 
end subroutine handle_error

end program Encrypt
