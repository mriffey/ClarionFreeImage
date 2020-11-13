!ABCIncludeFile(SAAPI)
!============================================================
! SaFileCl.inc -- File class header file
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
! 2008.07.03  Added FileToString and StringToFile methods.
!============================================================


 OMIT('_EndOfFileClassInclude_',_SaApiFileClassIncluded_)
_SaApiFileClassIncluded_ EQUATE(1)

  Include('SaWapi.inc','Defines'),Once

    ITEMIZE
SAFC_WRITE_ONLY         Equate(1)
SAFC_READ_ONLY          Equate
SAFC_READ_WRITE         Equate
    END

SAFC_FILE_SHARE_NULL    Equate(0)


!=================================================================================
! SaFileClass
!
! Remember to add the correct _SALinkMode_ and _SADLLMode_ your project defines
!=================================================================================
SaFileClass    CLASS, TYPE, Thread, MODULE('SaFileCl.clw'), |
                                    LINK('SaFileCl.clw',_SALinkMode_), |
                                    DLL(_SADLLMode_)
szFileName        CSTRING(261)
hFile             Long        !Handle to the managed file
LastError         ULONG            !Last api errorcode
buseCrc32c        BOOL        !Use new crc calc, available in c6+

CreateFile        Procedure(STRING FileName, ULONG CreateMode ),Long    !Returns hFile or INVALID_HANDLE_VALUE (-1)
CreateFile        Procedure(*CString szFileName, ULong DesiredAccess, ULong ShareMode, ULong CreationDisposition, ULong Flags ),Long
CreateFile        Procedure(*CString szFileName, ULong DesiredAccess, ULong ShareMode, <*SA_SECURITY_ATTRIBUTES SecurityAttributes>, ULong CreationDisposition, ULong Flags ),Long
Open              Procedure(STRING FileName, ULONG OpenMode=SAFC_READ_ONLY ),Long      !Returns hFile or INVALID_HANDLE_VALUE (-1)
Close             Procedure()
Write             Procedure(UNSIGNED pFileBuffer, ULONG BytesToWrite),ULONG,VIRTUAL !Returns bytes written to file
Read              Procedure(UNSIGNED pFileBuffer, ULONG BytesToRead),ULONG,VIRTUAL
SetFilePointer    Procedure(LONG nBytesToMove, LONG MoveMethod = SA_FILE_BEGIN),BOOL

                  !Note: you can allocate a string of (2^31) - 891879448 bytes using stringref &= New(String)
FileToString      Procedure(String Filename, *String pFileBuffer),ULONG,VIRTUAL
StringToFile      Procedure(*String theString, String Filename, ULong pBytesToWrite=0),ULong

GetSize           Procedure(),ULONG !Return the size of the file in bytes (up to 4GB)
CRC32             Procedure(UNSIGNED pBuffer, UNSIGNED cbBuffer, ULong InitalCRC=0),ULong
Flush             Procedure()

Construct         Procedure()
Destruct          Procedure()
                END



 !_EndOfFileClassInclude_