!============================================================
! SaClipCl.clw -- Clipboard Class implementation
!
!  Copyright © 2005 - 2006 Sand & Associates, Larry@sand-associates.com
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
  INCLUDE('SaClipCl.inc'),ONCE
  MAP
    INCLUDE('SaWApi.inc','Prototypes'),ONCE
  END


!--------------------------------------------------------------------------
SaClipboardClass.OpenClipboard       Procedure(WINDOW w)!,BOOL,Virtual
!--------------------------------------------------------------------------
  Code
  Return Self.OpenClipboard(w{Prop:Handle})

!--------------------------------------------------------------------------
SaClipboardClass.OpenClipboard       Procedure(Long hWnd)!,BOOL,Protected
!--------------------------------------------------------------------------
  Code
  If Self.bOpened = True
    Self.CloseClipboard()
  End

  If SA_OpenClipboard(hWnd) = False
    Self.LastErrorCde = SA_GetLastError()
    Self.bOpened = False
  Else
    Self.bOpened = True
  End
  Return Self.bOpened


!--------------------------------------------------------------------------
SaClipboardClass.SetClipboard   Procedure(UNSIGNED pPackedDIB, UNSIGNED DIBSize)!,BOOL,Virtual
!--------------------------------------------------------------------------
  Code
  Return Self.SetClipboardData(SA_CF_DIB, pPackedDIB, DIBSize)


!--------------------------------------------------------------------------
SaClipboardClass.SetClipboard   Procedure(String sText)!,BOOL,Virtual
!--------------------------------------------------------------------------
cbText      UNSIGNED
szText      &Cstring
bResult     BOOL,Auto
  Code
  bResult = False
  cbText = LEN(sText)
  szText &= New Cstring(cbText + 1)
  If Not szText &= Null
    szText = sText
    bResult = Self.SetClipboardData(SA_CF_TEXT, Address(szText), cbText)
    Dispose(szText)
  End
  Return bResult

!--------------------------------------------------------------------------
SaClipboardClass.SetClipboard   Procedure(*Cstring szText)!,BOOL,Virtual
!--------------------------------------------------------------------------
cbText      UNSIGNED
  Code
  cbText = LEN(szText)
  Return Self.SetClipboardData(SA_CF_TEXT, Address(szText), cbText)


!--------------------------------------------------------------------------
SaClipboardClass.SetClipboardData   Procedure(UNSIGNED CBFormat, UNSIGNED pData, UNSIGNED cbData)!,BOOL,Virtual,Protected
!--------------------------------------------------------------------------
hMem    SA_HANDLE
lpMem   UNSIGNED
cbMem   UNSIGNED
bResult BOOL,Auto
GmemFlags UNSIGNED,Auto
  Code
  bResult = False
  If Self.bOpened = False Then Return bResult; End
  GmemFlags = SA_GMEM_MOVEABLE

  Case CBFormat
  Of SA_CF_TEXT
    cbMem = cbData + 1              !add a byte for the terminating null
    GmemFlags += SA_GMEM_ZEROINIT   !memory is set to all nulls <0>
  Of SA_CF_DIB
    cbMem = cbData
  Else
    cbMem = cbData
  End

  If SA_EmptyClipboard()
    !------------------------------------------------------
    !Try to allocate the memory
    hMem = SA_GlobalAlloc(GmemFlags, cbMem)
    If hMem <> 0
      !------------------------------------------------------
      ! Attempt to copy the memory from the passed pointer to the new memory
      lpMem = SA_GlobalLock(hMem)
      If lpMem <> 0
        SA_MoveMemory(lpMem, pData, cbData)
        SA_GlobalUnlock(hMem)
        !------------------------------------------------------
        ! Try to attach this memory to the clipboard
        If SA_SetClipboardData(CBFormat, hMem) <> False
           bResult = True
        End
      End
    End
  End
  If bResult = False
    Self.LastErrorCde = SA_GetLastError()
    !------------------------------------------------------
    ! only free the memory if it wasn't attached to the clipboard
    If hMem <> 0 Then SA_GlobalFree(hMem).
  End
  Return bResult


!--------------------------------------------------------------------------
SaClipboardClass.GetClipboardData   Procedure(UNSIGNED CBFormat)!,UNSIGNED
!--------------------------------------------------------------------------
hMem    UNSIGNED,Auto
  Code
  hMem = 0
  If Self.bOpened = False Then Return hMem; End

  If SA_IsClipboardFormatAvailable(CBFormat) <> False
    hMem = SA_GetClipboardData(CBFormat)
    If hMem <> 0
      Self.OnGetClipboardData(CBFormat, hMem)
    Else
      Self.LastErrorCde = SA_GetLastError()
    End
  End !format is available on the clipboard

  Return hMem

!--------------------------------------------------------------------------
! hMem contains the HGLOBAL you must call GlobalLock and GlobalUnlock within
! this method to use the data.  DO NOT leave the memory locked!
!--------------------------------------------------------------------------
SaClipboardClass.OnGetClipboardData Procedure(UNSIGNED CBFormat, UNSIGNED hMem)!,Virtual
!--------------------------------------------------------------------------
  Code
  Return
          
!--------------------------------------------------------------------------
! If there's a DIB available on the clipboard then copy it into the Clarion
! BLOB.
!--------------------------------------------------------------------------
SaClipboardClass.GetClipboard   Procedure(*BLOB theBlob, Long CbFormat=SA_CF_DIB)!,Bool
!--------------------------------------------------------------------------
hMem    UNSIGNED,Auto
  Code
  hMem = Self.GetClipboardCopy(CbFormat)
  If hMem <> 0
    !---------------------------------------------
    ! Copy the image into the blob's buffer
    ! this makes another copy so we need to de-allocate
    ! the hBlobMem buffer asap
    theBlob{Prop:Size}   = 0
    theBlob{Prop:Handle} = hMem
    theBlob{Prop:Size}   = SA_GlobalSize(hMem)
    SA_GlobalFree(hMem)
  End
  Return Choose(hMem <> 0, True, False)


!--------------------------------------------------------------------------
SaClipboardClass.GetClipboard    Procedure(*Window theWindow, Long theImageControl)!,BOOL
!--------------------------------------------------------------------------
hMem    UNSIGNED,Auto
  Code
  hMem =Self.GetClipboardCopy(SA_CF_DIB)
  If hMem <> 0
    !--------------------------------------------------------
    !paste the image into an image control
    theWindow $ theImageControl{PROP:ImageBlob} = hMem
    SA_GlobalFree(hMem)
  End
  Return Choose(hMem <> 0, True, False)



!--------------------------------------------------------------------------
! Important: You must call SA_GlobalFree on the returned handle, otherwise
!            you'll create a memory leak.
!
! This code will handle most DIBs.  Some images copied to the clipboard from
! photoshop contain extra data and this metohd will not deal with them.
! Tested on 1, 4, 8, 16, 24, and 32 bit images
! Tested on images with palettes with limited number of entries.  For example
! an 8bit image with 38 palette entries.
!--------------------------------------------------------------------------
SaClipboardClass.GetClipboardCopy Procedure(UNSIGNED CBFormat)!,UNSIGNED,Protected
!--------------------------------------------------------------------------
hCbMem  UNSIGNED,Auto                  !handle to clipboard memory
lpCbMem UNSIGNED,Auto                  !pointer to clipboard memory
nCbSize UNSIGNED,Auto                  !Size of the clipboard memory in bytes

bfh             &SA_BITMAPFILEHEADER
bih             &SA_BITMAPINFOHEADER
nPaletteSize    UNSIGNED,Auto          !Size of the palette in bytes
hMem            UNSIGNED,Auto          !handle to the memory for the copy
lpMem           UNSIGNED,Auto          !pointer to the memory for the copy
nMemSize        UNSIGNED,Auto          !Size of the memory to allocate for the .bmp
nImageSize      UNSIGNED,Auto          !Size of the image bits without the headers
  Code

  hMem = 0
  If Self.bOpened = False Then Return hMem; End

  hCbMem  = Self.GetClipboardData(CbFormat)
  If hCbMem <> 0

    Case CbFormat
    Of SA_CF_DIB
      !---------------------------------------------
      ! Try to lock the memory and obtain a pointer
      lpCbMem = SA_GlobalLock(hCbMem)
      If lpCbMem <> 0
        !---------------------------------------------
        ! Get bitmap info header data to find the size
        ! of the palette, and how big the image is
        ! then use this data to allocate a block of
        ! memory large enough to hold the
        ! BITMAPFILEHEADER + BITMAPINFOHEADER + PALETTE + IMAGEBITS
        bih &= (lpCbmem) !Get info from the clipboard's BitmapInfoHeader
        If bih.biBitCount <= 8 !palette is manditory for 1, 4, and 8 bit images
          if bih.biClrUsed < 1
            nPaletteSize = bih.biBitCount^2
          Else
            nPaletteSize = bih.biClrUsed
          End
        Elsif bih.biCompression = SA_BI_BITFIELDS
          nPaletteSize = 3
        Else
          If bih.biClrUsed > 0   
            nPaletteSize = bih.biClrUsed
          Else
            nPaletteSize = 0
          End
        End
        nPaletteSize *= 4  !convert to bytes

        ! Calculate the size of the image data(accounts for 32 bit alignment of scan lines) {0FFFFFFE0h = "~31"}
        nImageSize = ABS(bih.biHeight) * BShift( Band( ((bih.biWidth * bih.biBitCount) + 31 ) , 0FFFFFFE0h), -3)

        nMemSize = Size(SA_BITMAPFILEHEADER) + bih.biSize + nPaletteSize + nImageSize
        !---------------------------------------------
        ! Try to allocate the memory block that the .bmp
        ! will be created in.
        bih &= Null
        hMem = SA_GlobalAlloc(SA_GMEM_MOVEABLE, nMemSize)
        If hMem <> 0
          lpMem = SA_GlobalLock(hMem)
          If lpMem <> 0
           !---------------------------------------------
           ! Assign the correct info in the file header and
           ! copy the image from the clipboard
            bfh &= (lpMem)                                  !File header is first item in memory
            bih &= (lpMem + Size(SA_BITMAPFILEHEADER) )     !new image info header is next
            SA_MoveMemory(lpMem + Size(SA_BITMAPFILEHEADER), lpCbMem, nMemSize - Size(SA_BITMAPFILEHEADER) )
            bfh.bfType = 4D42h !'BM' in little endian order
            bfh.bfSize = nMemSize
            bfh.bfReserved1 = 0
            bfh.bfReserved2 = 0
            bfh.bfOffBits =  Size(SA_BITMAPFILEHEADER) + bih.biSize + nPaletteSize
            
            bih &= Null
            bfh &= Null
            !---------------------------------------------
            ! Done writing to this memory so unlock it now
            SA_GlobalUnlock(hMem)
          End
        End  !if locked
        !-------------------------------------------
        ! Done with the clipboard memory so unlock it now
        SA_GlobalUnlock(hCbMem)
      End
    !ToDo implement other clipboard formats
    End !of

  End !If hcbMem
  Return hMem


!--------------------------------------------------------------------------
SaClipboardClass.GetClipboard   Procedure()!,String
!--------------------------------------------------------------------------
s       &CString
cbMem   UNSIGNED,Auto
  Code
  Self.hMem = Self.GetClipboardData(SA_CF_TEXT)
  If Self.hMem <> 0
    cbMem = SA_GlobalSize(Self.hMem)
    Self.lpMem = SA_GlobalLock(Self.hMem)
    If Self.lpMem <> 0 And cbMem > 0
      s &= (Self.lpMem)&':'&cbMem
      Return s    !remember to call closeclipboard to unlock memory
    End
  End
  Return ''

!--------------------------------------------------------------------------
SaClipboardClass.CloseClipboard      Procedure()!,Protected
!--------------------------------------------------------------------------
  Code
  If Self.bOpened = True
    If Self.lpMem
      SA_GlobalUnlock(Self.hMem)
      Self.lpMem = 0
      Self.hMem = 0
    End

    If SA_CloseClipboard() <> False
      Self.bOpened = False
    Else
      Self.LastErrorCde = SA_GetLastError()
    End
  End
  Return


!--------------------------------------------------------------------------
SaClipboardClass.Construct     Procedure()!,Protected
!--------------------------------------------------------------------------
  Code
  Self.bOpened = False
  Self.lpMem = 0
  Self.hMem = 0
  Return

!--------------------------------------------------------------------------
SaClipboardClass.Destruct      Procedure()!,Protected
!--------------------------------------------------------------------------
  Code
  Self.CloseClipboard()
  Return
