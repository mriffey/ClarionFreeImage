!============================================================
! SaUtil.inc -- Utility class implementation
!
!  Copyright © 2005 Sand & Associates, Larry@sand-associates.com
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

  Member
  Include('SaUtilCl.inc'),Once
  Include('SaFileCl.inc'),Once

  MAP
    Include('SaWApi.inc','Prototypes'),Once
  END



!---------------------------------------------------------------------------------------
SaUtilityClass.SaFileToBlob     Procedure(String sFileName, *BLOB FileBlob)!,BOOL
!---------------------------------------------------------------------------------------
theFile     SaFileClass
FileBuffer  &String
FileSize    ULong,Auto
bResult     BOOL,Auto
  Code
  bResult = False
  If theFile.Open(sFileName, SAFC_READ_ONLY) > 0
    FileSize = theFile.GetSize()
    If FileSize > 0
      FileBuffer &= New String(FileSize)
      If Not FileBuffer &= Null
        If theFile.Read(Address(FileBuffer), FileSize) = FileSize
          FileBlob{PROP:Size} = 0                 !Clear the BLOB
          FileBlob[0 : FileSize-1] = FileBuffer
          FileBlob{PROP:Size} = FileSize          !Not strictly required with string slicing
          bResult = True
        End
      End
    End
  End
  Dispose(FileBuffer)
  Return bResult


!---------------------------------------------------------------------------------------
SaUtilityClass.SaBlobToFile     Procedure(*BLOB FileBlob, String sFileName)!,BOOL
!---------------------------------------------------------------------------------------
theFile     SaFileClass
FileSize    ULong,Auto
pFileBuffer UNSIGNED
bResult     BOOL,Auto
hMem        UNSIGNED,Auto
  Code
  bResult = False
  !----Overwrite any existing file----
  If theFile.CreateFile(sFileName, SAFC_WRITE_ONLY) > 0
    FileSize = FileBlob{Prop:Size}
    If FileSize > 0
      hMem = FileBlob{Prop:Handle}
      pFileBuffer = SA_GlobalLock(hMem)
      If pFileBuffer <> 0
        If theFile.Write(pFileBuffer, FileSize) = FileSize
          bResult = True
        End
        SA_GlobalUnlock(hMem)
      End
    End
  End
  Return bResult




