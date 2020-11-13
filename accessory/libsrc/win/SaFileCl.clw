!============================================================
! SaFileCl.clw -- File class Implementation
!
!  Copyright © 2000-2006 Sand & Associates, Larry@sand-associates.com
!
!  This software is provided 'as-is', without any express or implied
!  warranty.  In no event will the authors be held liable for any damages
!  arising from the use of this software.
!
!  Permission is granted to anyone to use this software for any purpose,
!  including commercial applications, and to alter it and redistribute it
!  freely, subject to the following restrictions:
!
!  1. The origin of this software must not be misrepresented; you must not
!     claim that you wrote the original software. If you use this software
!     in a product, an acknowledgment in the product documentation would be
!     appreciated but is not required.
!
!  2. Altered source versions must be plainly marked as such, and must not be
!     misrepresented as being the original software.
!
!  3. This notice may not be removed or altered from any source distribution.
!
!============================================================
! --- 2006.12 ---
! Added    Flush method
! Removed  call to SA_FlushFileBuffers from Close method   (Arnor Baldvinsson)
! Added    new version of Crc32 calculation.  Compiled automatically in C6 only 
!          and available by setting _USECRC32C_ => 1 in proj settings
!          In < C6 user must add crc32.c to the project 
!          
!--------------------------------------------------------------
  MEMBER()

  INCLUDE('SaFileCl.inc'),Once

  MAP
    INCLUDE('SaWApi.inc','Prototypes'),ONCE

    MODULE('ClarionRTL')
      CRC32(UNSIGNED pBuffer, LONG cbBuffer, LONG InitalCRC=0),LONG,NAME('CLA$CRC32')
    END

  END    

 Compile('ExtCmod', _USECRC32C_) 
  Compile('C6', _C60_ )
    PRAGMA('compile(CRC32C.C)') 
  C6                              

  Map
    !http://www.csbruce.com/~csbruce/software/crc32.c
    Module('CRC32C.C')  
      Crc32c(Ulong initalCrc32 =0, UNSIGNED pbuf, ULong BufLen),ULong, Name('_Crc32Buf')
    End
  End
  !ExtCmod
                
                
!=================================================================================================
! CreateFile
!
! Create a file with write access and default security descriptor.  If the file esists it's
! overwritten (CREATE_ALWAYS)
!
! Returns hFile or -1 for invalid handle
!=================================================================================================
SaFileClass.CreateFile   PROCEDURE(STRING FileName, ULONG CreateMode)
  CODE
  Self.szFileName = CLIP(FileName)

  CASE CreateMode
  OF SAFC_WRITE_ONLY
    Self.hFile = Self.CreateFile(Self.szFileName, SA_GENERIC_WRITE, SAFC_FILE_SHARE_NULL, SA_CREATE_ALWAYS, SA_FILE_ATTRIBUTE_NORMAL)
  Else
    Self.hFile = 0
  END

  IF Self.hFile = SA_INVALID_HANDLE_VALUE
    Self.LastError = SA_GetLastError()
  END

  RETURN Self.hFile

!=================================================================================================
! CreateFile            api wrapper
!
! The parameters for this method match their respective parameters for the API call.
! This method always passes null for the security attributes and null for the file template.
! Override this method to create one that allows security attributes.
!
! Returns the hFile or -1 for invalid handle
!=================================================================================================
SaFileClass.CreateFile   PROCEDURE(*CString szFileName, ULong DesiredAccess, ULong ShareMode, |
                                                        ULong CreationDisposition, ULong Flags )
  Code
  Return Self.CreateFile(Self.szFileName, DesiredAccess, ShareMode, , CreationDisposition, Flags)

!=================================================================================================
! CreateFile            api wrapper
!
! The parameters for this method match their respective parameters for the API call.
! This method always passes null for the file template.
!
! Returns the hFile or -1 for invalid handle
!=================================================================================================
SaFileClass.CreateFile   PROCEDURE(*CString szFileName, ULong DesiredAccess, ULong ShareMode, |
                                                        <*SA_SECURITY_ATTRIBUTES SecurityAttributes>,|
                                                        ULong CreationDisposition, ULong Flags )
  Code
  Return SA_CreateFile(Self.szFileName, DesiredAccess, ShareMode, SecurityAttributes, CreationDisposition, Flags, 0)



!=================================================================================================
! OpenFile
!
! Open a file with a default security descriptor.
!
! Returns hFile or -1 for invalid handle
!=================================================================================================
SaFileClass.Open    PROCEDURE(STRING FileName, ULONG OpenMode=SAFC_READ_ONLY)
  CODE
  Self.szFileName = CLIP(FileName)
  CASE OpenMode
  OF SAFC_READ_ONLY
    Self.hFile = Self.CreateFile(Self.szFileName, SA_GENERIC_READ, SAFC_FILE_SHARE_NULL, SA_OPEN_EXISTING, SA_FILE_ATTRIBUTE_NORMAL)
  OF SAFC_READ_WRITE
    Self.hFile = Self.CreateFile(Self.szFileName, SA_GENERIC_READ+SA_GENERIC_WRITE, SAFC_FILE_SHARE_NULL, SA_OPEN_EXISTING, SA_FILE_ATTRIBUTE_NORMAL)
  Else
    Self.hFile = 0
  END

  IF Self.hFile = SA_INVALID_HANDLE_VALUE
    Self.LastError = SA_GetLastError()
  END

  RETURN Self.hFile


!=================================================================================================
! Write
! pFileBuffer = Address(FileBuffer)
! nBytesToWrite = number of bytes to write to the FileBuffer
! Returns number of bytes written.
!=================================================================================================
SaFileClass.Write  PROCEDURE(UNSIGNED pFileBuffer, | Pointer to the file buffer to write
                             ULONG BytesToWrite)   ! Number of bytes to write from the buffer
BytesWritten    ULONG,AUTO
  CODE
  BytesWritten = 0
  IF Self.hFile
    IF SA_WriteFile(Self.hFile, pFileBuffer, BytesToWrite, BytesWritten, ) = 0
      !Error writing to the file
      Self.LastError = SA_GetLastError()
    END
  END
  RETURN BytesWritten

!=================================================================================================
! Close the file handle
!=================================================================================================
SaFileClass.Close    PROCEDURE()
  CODE
  IF Self.hFile <> SA_INVALID_HANDLE_VALUE AND Self.hFile <> 0
    SA_CloseHandle(Self.hFile)
  END
  Self.hFile = 0
  RETURN


!=================================================================================================
! Flush the buffer
!=================================================================================================
SaFileClass.Flush    PROCEDURE()
  CODE
  IF Self.hFile <> SA_INVALID_HANDLE_VALUE AND Self.hFile <> 0
    SA_FlushFileBuffers(Self.hFile)
  END
  RETURN



!=================================================================================================
! GetSize
!
! Find the length, in bytes, of the file idenified by Self.hFile.  Assumes a file was previously
! opened or created by the object.
!
! Returns file size in bytes, Maximum file size is 4GB
!=================================================================================================
SaFileClass.GetSize       PROCEDURE()  !Returns number of bytes in disk file
FileSize ULONG,AUTO
 CODE

 FileSize = SA_GetFileSize(Self.hFile, )  !< 4GB file size
 IF FileSize = SA_INVALID_FILE_SIZE
   Self.LastError = SA_GetLastError()
   FileSize = 0
 END
 RETURN FileSize

!=================================================================================================
! StringToFile
!
! Write a string to a disk file, default bytes to write to the clipped length of the string.
!=================================================================================================
SaFileClass.StringToFile      Procedure(*String theString, String Filename, ULong pBytesToWrite=0)!,ULong
nBytesToWrite   LONG,AUTO
nBytesWritten   LONG,AUTO
  CODE
  nBytesWritten = 0
  If Self.CreateFile(Filename, SAFC_WRITE_ONLY) <> SA_INVALID_HANDLE_VALUE
    If pBytesToWrite > 0
      nBytesToWrite = pBytesToWrite
    Else
      nBytesToWrite = Len(Clip(theString))
    End
    nBytesWritten = Self.Write(Address(theString), nBytesToWrite)
  End
  Return nBytesWritten

!=================================================================================================
! FileToString
!
! Read file up size of string from the current filepointer position.
! Returns number of bytes acutally read.
!=================================================================================================
SaFileClass.FileToString   PROCEDURE(String Filename, *String pFileBuffer)!,ULONG
nBytesToRead    ULONG,AUTO
nBytesRead      ULONG,AUTO
  CODE
  nBytesRead = 0
  If Self.Open(Filename) <> SA_INVALID_HANDLE_VALUE
    nBytesToRead = Self.GetSize()
    If nBytesToRead <= Size(pFileBuffer)
      nBytesRead = Self.Read(Address(pFileBuffer), nBytesToRead)
    End
  End
  RETURN nBytesRead

!=================================================================================================
! Read
!
! Read specified number of bytes from the current filepointer position.
! pFileBuffer = Address(FileBuffer)
! nBytesToRead = number of bytes to read from FileBuffer
! Returns number of bytes acutally read.
!=================================================================================================
SaFileClass.Read   PROCEDURE(UNSIGNED puFileBuffer, ULONG nBytesToRead)!,ULONG
nBytesRead   ULONG,AUTO
  CODE
  IF SA_ReadFile(Self.hFile,                     |
                 puFileBuffer,                    | pointer to data buffer
                 nBytesToRead,                   |
                 nBytesRead,   )
  ELSE
    nBytesRead = 0
  END

  RETURN nBytesRead


!=================================================================================================
! SetFilePointer
!
!
!=================================================================================================
SaFileClass.SetFilePointer PROCEDURE(LONG nBytesToMove, LONG MoveMethod=SA_FILE_BEGIN)
  CODE

  IF SA_SetFilePointer(Self.hFile,         | handle to file
                       nBytesToMove,       | bytes to move pointer
                       ,                   | bytes to move pointer !Omit for null
                       MoveMethod          | starting point SA_FILE_BEGIN, SA_FILE_CURRENT, SA_FILE_END
                      ) = SA_INVALID_SET_FILE_POINTER
    RETURN False
  ELSE
    RETURN True
  END

!=================================================================================================
! CRC32
!
! Class wrapper for CRC32 
!=================================================================================================
SaFileClass.CRC32      PROCEDURE(UNSIGNED pBuffer, UNSIGNED cbBuffer, ULONG InitalCRC=0)
  CODE
  Compile('USECRC32C', _USECRC32C_)
    If Self.buseCrc32c
      Return CRC32c(InitalCRC, pBuffer, cbBuffer)
    Else
      Return CRC32(pBuffer, cbBuffer, InitalCRC)
    End
  !USECRC32C

  Omit('notCRC32C', _USECRC32C_)
    Return CRC32(pBuffer, cbBuffer, InitalCRC)
  !notCRC32C


!=================================================================================================
! Construct
!
! Initialize the properties
!=================================================================================================
SaFileClass.Construct   PROCEDURE()
  CODE
  Self.hFile = 0
  Self.LastError = 0
  Self.buseCrc32c = True
  RETURN


!=================================================================================================
! Destruct
!
!
!=================================================================================================
SaFileClass.Destruct   PROCEDURE()
  CODE
  Self.Close()
  RETURN
