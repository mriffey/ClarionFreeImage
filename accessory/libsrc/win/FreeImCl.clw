!============================================================
! freeImCl -- Clarion FreeImage Class implementation
!
!  Copyright © 2005-2013 Sand & Associates, Larry@sand-associates.com
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
! 2007.05.02  Added Capture Screen Method
! 2007.05.04  Fixed error in .Save() method where fif was not 
!             set to file format selected in the file dialog.
!             This only occured when the image had no file name
!             as is the case when it's created with NewImage()
! 2010.01.21  Added Blank() method    
! 2013.07.06  Change Load to use FileDialog().
!             Add JPEGCrop(), JPEGTransform methods
!             Remove protected attribute from GetDialogFilter()    
!             Added ResizeCanvas methods to resize the image filling the background 
!             with the specified color.  This is used to add a border to an image.  
! 2018.12.20  Added RotateImageOnEXIFOrientation() method
!============================================================    
! 2020.4.13   Modified by Charles Edmonds
!             Replaced deprecated call to FreeImage_RotateClassic with a call to FreeImage_Rotate
!             Created new .LIB file from FreeImage 3.18.0 DLL
!============================================================    

 Member
  Include('FreeImCl.inc'),Once

  MAP
    Include('FreeImg.inc','Prototypes'),Once
    Include('SaWApi.inc','Prototypes'),Once
    module('winapi')
     outputdebugstring(*Cstring szText),Pascal,Raw,Name('OutputDebugStringA')
    end

fods         Procedure(String sText)
errorproc   Procedure(FREE_IMAGE_FORMAT fif, UNSIGNED lpszMessage), C
  END

fods                  PROCEDURE  (String sText)             
szString    &Cstring
  Code
  szString &= new Cstring(Len(sText)+1)
  if Not szString &= Null
     szString = sText
     OutputDebugString(szString)
  end
  Dispose(szString)

!--------------------------------------------------------------------------
! Called by the FreeImage library when error occurs.
!
!--------------------------------------------------------------------------
errorproc   Procedure(FREE_IMAGE_FORMAT fif, UNSIGNED lpszMessage)
!--------------------------------------------------------------------------
szFIErrorMessage &Cstring

  Code
  szFIErrorMessage &= (lpszMessage &':'& SA_lstrlen(lpszMessage))
  Message(szFIErrorMessage, 'FreeImage Notification', Icon:Exclamation)

  Return

!*************************************************************************




!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.GetFileDialogfilter     Procedure(BOOL bReadonly=False )!,String
!--------------------------------------------------------------------------
szFilter Cstring(2048)
  Code
  Self.GetFileDialogFilter(szFilter, bReadOnly)
  Return szFilter


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.GetFileDialogfilter     Procedure(*Cstring szFilter, BOOL bReadonly=False, BOOL bNullSep=False )
!--------------------------------------------------------------------------
i   UNSIGNED,Auto
j   UNSIGNED,Auto
npos   UNSIGNED,Auto
szExt    &Cstring


szExtList                                 Cstring(256)
szDesc                                    &Cstring   

pExtList                                    UNSIGNED,Auto   
nExtLen Long,Auto
  Code
  szFilter = szFilter & 'Common Image formats|*.jpg;*.jpeg;*.tif;*.gif;*.jfif;*.tiff;*.bmp|'
  Loop i = 0 To FreeImage_GetFifCount()-1
    If Choose(bReadOnly=True, FreeImage_FIFSupportsReading(i), FreeImage_FIFSupportsWriting(i))
        pExtList = FreeImage_GetFIFExtensionList(i) 
        nExtLen = SA_lstrlen(pExtList) +1 
        szExt &= pExtList &':'&  nExtLen
        szExtList = ''
      Loop j = 1 To Len(szExt)
        nPos = Instring(',', szExt, 1, j)
        If nPos = 0
          nPos = Len(szExt) +1
        end
        szExtList = szExtList & '*.'& szExt[j : nPos-1] &';'
        j = nPos
      End
      szExtList = szExtList[1 : Len(szExtList)-1 ]
      szDesc &= FreeImage_GetFIFDescription(i)
      szFilter = szFilter & szDesc &'|' & szExtList & '|'
    End
  End 
  szFilter = szFilter & 'All Files|*.*'      
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetFileName   Procedure(String sImageFilename)!,BOOL,Proc
!--------------------------------------------------------------------------
  Code
  Dispose(Self.szImageFileName)     
  Self.szImageFileName &= NEW(Cstring(Len(Clip(sImageFilename))+1) )

  If Not (Self.szImageFileName &= Null)
    Self.szImageFileName = Clip(sImageFilename)
    Return True
  End
  Return False


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetFileName   Procedure(*CString szImageFilename)!,BOOL,Proc
!--------------------------------------------------------------------------
szTempFilename &Cstring
bResult BOOL,Auto
FilenameLen     Long,Auto
  Code
  bResult = False
  FilenameLen = Len(Clip(szImageFilename)) +1
  szTempFilename &= New(Cstring(FilenameLen) )
  If szTempFilename &= Null
    Return bResult
  End
  szTempFilename = szImageFilename

  Dispose(Self.szImageFileName)
  Self.szImageFileName &= NEW(Cstring(FilenameLen) )

  If Not (Self.szImageFileName &= Null)
    Self.szImageFileName = szTempFilename
    bResult = True
  End

  Dispose(szTempFilename)
  Return bResult


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetFileName   Procedure(*CString szImageFilename)
!--------------------------------------------------------------------------
  Code
  If Self.szImageFilename &= Null
    szImageFilename = ''
  Else
    szImageFilename = Self.szImageFilename
  End

  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetFileName   Procedure()!,String
!--------------------------------------------------------------------------
  Code
  If Self.szImageFilename &= Null
    Return ''
  End
  Return Clip(Self.szImageFilename)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Load                Procedure(String ImageFilename)!,BOOL,Proc
!--------------------------------------------------------------------------
sFilename String(512)
  Code
  If ImageFilename <> ''
    sFileName = ImageFilename
    If ImageFilename[1] ='~'
      Return Self.iImage.LoadResource(ImageFilename)
    ElsIf sFileName <> '' And Self.iImage.SetFilename(sFilename)
      Return Self.iImage.Load(Self.szImageFileName)
    End
  Else
    sFilename = ''
    if FileDialog( Clip(Self.szOpenDialogTitle), sFilename, Self.GetFileDialogFilter(FIFF_SUPPORTSread), FILE:KeepDir + FILE:LongName)
      If Self.iImage.SetFilename(Clip(sFilename))
        Return Self.iImage.Load(Clip(sFilename))
      end  
    end
  End
  Return False


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Load                Procedure(*Cstring szImageFilename)!,BOOL,Proc
!--------------------------------------------------------------------------
  Code
  If Self.pImage
    FreeImage_Unload(Self.pImage)
  End

  Self.pImage = 0
  Self.bRetainImage = False

  Self.fif = FreeImage_GetFileType(szImageFileName, 0)
  If Self.fif = FIF_UNKNOWN  !Couldn't read it from the file so try using the filename
    Self.fif = FreeImage_GetFIFFromFilename(szImageFilename)
  End

  If Self.fif <> FIF_UNKNOWN And FreeImage_FIFSupportsReading(Self.fif)
    Self.iImage.Onload(Self.fif)
    Self.pImage = FreeImage_Load(Self.fif , szImageFileName, Self.LoadOption )
    If Self.pImage <> 0
      Self.iImage.SetFilename(szImageFilename) 
      Self.iImage.GetSize(Self.imageSize)
    Else
      Self.OnLoadFailure()
    End
  End
  If FreeImage_IsTransparent(Self.pImage) !And
    FreeImage_SetTransparent(Self.pImage, True)
  End
  Return Choose(Self.pImage <> 0, True, False)


!--------------------------------------------------------------------------
! Load an image from a Clarion Blob,Binary
!--------------------------------------------------------------------------
FreeImageClass.iImage.Load                Procedure(*BLOB ImageBlob)!,BOOL
loaded  BOOL
  Code
  if ImageBlob{Prop:Size} > 0
    loaded = Self.iImage.Load(ImageBlob{PROP:Handle}, ImageBlob{PROP:Size})
  else
    loaded = False
  end
  Return loaded

!--------------------------------------------------------------------------
! Load an image from a global memory handle
!--------------------------------------------------------------------------
FreeImageClass.iImage.Load          Procedure(SA_HGLOBAL hMem, Long cbImage)!,BOOL
fihMemory   FIMEMORY
lpBuffer    UNSIGNED
  Code
  If Self.pImage
    FreeImage_Unload(Self.pImage)
  End
  Self.pImage = 0

  If cbImage > 0

    Self.bRetainImage = False

    lpBuffer = SA_GlobalLock(hMem)
    If lpBuffer <> 0
      fihMemory = FreeImage_OpenMemory(lpBuffer, cbImage)
      IF fihMemory <> 0
        Self.fif = FreeImage_GetFileTypeFromMemory(fihMemory, 0)
        If Self.fif <> FIF_UNKNOWN And FreeImage_FIFSupportsReading(Self.fif)
          Self.iImage.Onload(Self.fif)
          Self.pImage = FreeImage_LoadFromMemory(Self.fif, fihMemory, Self.LoadOption)
        End
        FreeImage_CloseMemory(fihMemory)
      End
      SA_GlobalUnlock(hMem)
    End

  End

  If Self.pImage = 0
    Self.OnLoadFailure()
  Else
    Self.iImage.GetSize(Self.imageSize)
  End

  Return Choose(Self.pImage <> 0, True, False)

!--------------------------------------------------------------------------
! Load an image from a linked resource
!--------------------------------------------------------------------------
FreeImageClass.iImage.LoadResource   Procedure(String sImageName)!,BOOL

szResType   Cstring('IMAGE')
szImgName   &Cstring
hResInfo    Long
hRes        Long

fihMemory   FIMEMORY
lpBuffer    UNSIGNED
cbBuffer    UNSIGNED
dotPos      UNSIGNED
  Code
  szImgName &=New Cstring(Len(sImageName)+1)
  If sImageName[1] ='~'
    szImgName=Upper(sImageName[2 : Len(Clip(sImageName)) ])
  Else
    szImgName=Upper(sImageName)
  End

  Loop dotPos = 1 to Len(szImgName)
    If szImgName[dotPos]='.'
      szImgName[dotPos]='_'
    End
  End

  hResInfo = SA_FindResource(System{Prop:AppInstance}, szImgName, szResType)
  hRes = SA_LoadResource(System{Prop:AppInstance}, hResInfo)

  If Self.pImage
    FreeImage_Unload(Self.pImage)
  End

  Self.pImage = 0
  Self.bRetainImage = False
  cbBuffer = SA_SizeofResource(System{Prop:AppInstance}, hResInfo)
  lpBuffer = SA_LockResource(hRes)
  If lpBuffer <> 0 And cbBuffer <> 0
    fihMemory = FreeImage_OpenMemory(lpBuffer, cbBuffer)
    IF fihMemory <> 0
      Self.fif = FreeImage_GetFileTypeFromMemory(fihMemory, 0)
      If Self.fif <> FIF_UNKNOWN And FreeImage_FIFSupportsReading(Self.fif)
        Self.iImage.Onload(Self.fif)
        Self.pImage = FreeImage_LoadFromMemory(Self.fif, fihMemory, Self.LoadOption)
      End
      FreeImage_CloseMemory(fihMemory)
    End
  End
  If Self.pImage = 0
    Self.OnLoadFailure()   
  Else
    Self.iImage.GetSize(Self.imageSize)
  End

  Dispose(szImgName)
  Return Choose(Self.pImage <> 0, True, False)


!--------------------------------------------------------------------------
! OnLoad()
!
! This method is called immediately before the library call to load the image
!
!--------------------------------------------------------------------------
FreeImageClass.iImage.OnLoad              Procedure(FREE_IMAGE_FORMAT fif)
!--------------------------------------------------------------------------
  Code
  Self.OnLoad(fif)
  Return


!--------------------------------------------------------------------------
! OnLoad()
!
!--------------------------------------------------------------------------
FreeImageClass.OnLoad              Procedure(FREE_IMAGE_FORMAT fif)!,Virtual
!--------------------------------------------------------------------------
  Code
  Return


!--------------------------------------------------------------------------
!
!--------------------------------------------------------------------------
FreeImageClass.iImage.OnLoadFailure       Procedure()
  Code
  Self.OnLoadFailure()
  Return

!--------------------------------------------------------------------------
!
!--------------------------------------------------------------------------
FreeImageClass.OnLoadFailure       Procedure()
  Code
  Return

!-------------------------------------------------------------------------------
!Type    Flag               Description
!-------------------------------------------------------------------------------
!
!GIF     GIF_DEFAULT
!        GIF_LOAD256         Load the image as a 256 color image with unused palette
!                            entries, if it's 16 or 2 color.
!        GIF_PLAYBACK        'Play' the GIF to generate each frame (as 32bpp) instead
!                            of returning raw frame data when loading
!
!ICO     ICO_MAKEALPHA       Convert to 32-bit and create an alpha channel from the AND-mask when
!                            loading
!
!JPEG    JPEG_DEFAULT        Loads the file as fast as possible, sacrificing some quality
!        JPEG_FAST           Loads the file as fast as possible, sacrificing some quality
!        JPEG_ACCURATE       Loads the file with the best quality, sacrificing some speed
!        JPEG_CMYK           This flag will load CMYK bitmaps as 32-bit separated CMYK
!
!PCD     PCD_DEFAULT         A PhotoCD picture comes in many sizes. This flag will load
!                            the one sized 768 x 512 PCD_BASE This flag will load the one
!                            sized 768 x 512
!
!        PCD_BASEDIV4        This flag will load the bitmap sized 384 x 256
!        PCD_BASEDIV16       This flag will load the bitmap sized 192 x 128
!
!PNG     PNG_IGNOREGAMMA     Avoid gamma correction
!
!TARGA   TARGA_LOAD_RGB888   If set the loader converts RGB555 and ARGB8888 -> RGB888
!
!TIFF    TIFF_CMYK           This flag will load CMYK bitmaps as 32-bit separated CMYK
!-------------------------------------------------------------------------------
FreeImageClass.iImage.SetLoadOption       Procedure(Long LoadOption)
!--------------------------------------------------------------------------
  Code
  Self.LoadOption = LoadOption
  Return



!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetLoadOption       Procedure()!,Long
!--------------------------------------------------------------------------
  Code
  Return Self.LoadOption



!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Save                Procedure()!,BOOL,Proc
!--------------------------------------------------------------------------
SaveFileDialog  FileDialogClass
  Code
  If Self.szImageFilename &= Null
    If Self.iImage.SetFilename(' ') = False
      Return False
    End
  End

  If Self.szImageFilename = ''
    SaveFileDialog.SetDialogFilter(Self.GetFileDialogFilter(FIFF_SUPPORTSwrite))
    If SaveFileDialog.Show(, FILE:Save+FILE:KeepDir, ,SA_OFN_HIDEREADONLY) 
      If Self.iImage.SetFilename(SaveFileDialog.szFullFilename) = False
        Return False
      End 
      Self.fif = FreeImage_GetFIFFromFilename(SaveFileDialog.szFullFilename)
    Else
      Return False
    End
  End
  Return Self.iImage.Save(Self.szImageFilename, Self.fif)

            
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Save                Procedure(*Cstring szImageFilename, FREE_IMAGE_FORMAT fif=FIF_UNKNOWN)!,BOOL,Proc,Protected
!--------------------------------------------------------------------------
  Code
  If fif = FIF_UNKNOWN
    !Attempt to get image format from the filename
    fif = FreeImage_GetFIFFromFilename(szImageFilename)
    If fif = FIF_UNKNOWN
      Self.iImage.TakeError(UNKNOWN_IMAGE_FORMAT)
    End
  End
  If FreeImage_FIFSupportsWriting(fif) And FreeImage_FIFSupportsExportBPP(fif, FreeImage_GetBPP(Self.pImage))
    Self.fif = fif
    Self.iImage.OnSave(fif)
    Return FreeImage_Save(fif, Self.pImage, szImageFilename, Self.SaveOption)
  End
  Self.OnSaveFailure()
  Return False


!--------------------------------------------------------------------------
! Save an image to a blob
!--------------------------------------------------------------------------
FreeImageClass.iImage.Save Procedure(*Blob theBlob, FREE_IMAGE_FORMAT fif=FIF_UNKNOWN)!,BOOL
!--------------------------------------------------------------------------
fihMemory       FIMEMORY
lp_buffer       Long
size_in_bytes   UNSIGNED
bResult         BOOL,Auto
hMemBlob        Long
sBuff           &String
  Code
  bResult = False
  fihMemory = FreeImage_OpenMemory()
  IF fihMemory <> 0

    ! if the format isn't passed then use this object's file format.
    ! And if that's unknown then default to PNG 
    If fif = FIF_UNKNOWN
      If Self.fif <> FIF_UNKNOWN
        fif = Self.fif
      Else
        fif = FIF_PNG
      End
    End

    If FreeImage_FIFSupportsWriting(fif) And FreeImage_FIFSupportsExportBPP(fif, FreeImage_GetBPP(Self.pImage))
      Self.iImage.OnSave(fif)
      If FreeImage_SaveToMemory(fif, Self.pImage, fihMemory, Self.SaveOption) = True
        If fihMemory <> 0
          lp_buffer = 0
          If FreeImage_AcquireMemory(fihMemory, lp_buffer, size_in_bytes) = True And lp_Buffer <> 0 And size_in_bytes <> 0
               sBuff &= (lp_buffer&':'&size_in_bytes)
               theBlob{PROP:Size} = 0
               theBlob[0 : size_in_bytes-1] = sBuff
               sBuff &= Null
               bResult = True
            !End
          End
        End
      End
    End
    FreeImage_CloseMemory(fihMemory)
  End

  If bResult = False
    Self.OnSaveFailure()
  End

  Return bResult


!--------------------------------------------------------------------------
! OnSaveFailure()
!
! This method is called immediately after the library call to save the image
!
!--------------------------------------------------------------------------
FreeImageClass.iImage.OnSaveFailure      Procedure()
!--------------------------------------------------------------------------
  Code
  Self.OnSaveFailure()
  Return

!--------------------------------------------------------------------------
! OnSaveFailure()
!
!--------------------------------------------------------------------------
FreeImageClass.OnSaveFailure          Procedure()!,Virtual
!--------------------------------------------------------------------------
  Code
  Return


!--------------------------------------------------------------------------
! An image control (CLARION) expects PROP:ImageBlob to be a global memory handle that
! contains an image in the form of a disk file. This can be any image
! format that the image control supports.  BMP, JPEG, GIF, or ICO.
! This method uses BMP becuase it won't suffer more JPEG loss and the image
! control will not have to decompress the image again.  The BMP format is a
! DIB with a BitmapFileHeader.
!--------------------------------------------------------------------------
FreeImageClass.iImage.CopyToImageControl  Procedure(Window theWindow, Long feqImageControl)
!--------------------------------------------------------------------------
fihMemory       FIMEMORY
mem_buffer      Long
size_in_bytes   UNSIGNED
hMem            SA_HGLOBAL
lpGmem          UNSIGNED

  Code
  fihMemory = FreeImage_OpenMemory()
  IF fihMemory <> 0
    If FreeImage_SaveToMemory(FIF_BMP, Self.pImage, fihMemory, 0) = True
        If FreeImage_AcquireMemory(fihMemory, mem_buffer, size_in_bytes) = True
            hmem = SA_GlobalAlloc(SA_GMEM_MOVEABLE, size_in_bytes)
            if hmem <> 0 
                lpGmem = SA_GlobalLock(hMem)
                if lpGmem <> 0
                  SA_MoveMemory(lpGMem, mem_buffer, size_in_bytes)
                  SA_GlobalUnlock(hMem)
                  theWindow $ feqImageControl{PROP:ImageBlob} = hMem !requires hGlobal
                end
                SA_globalfree(hMem)
            else
                Self.iImage.TakeError(OUT_OF_MEMORY)
            end
        End
    End
    FreeImage_CloseMemory(fihMemory)

  else
     Self.iImage.TakeError(OUT_OF_MEMORY)
  End
  Return



!--------------------------------------------------------------------------
! OnSave()
!
! This method is called immediately before the library call to save the image
!
!--------------------------------------------------------------------------
FreeImageClass.iImage.OnSave              Procedure(FREE_IMAGE_FORMAT fif)     
!--------------------------------------------------------------------------
  Code
  ! Derive this method and add your custom code.
  !
  ! For example you could use this virtual embed point to set
  ! options for this image format (fif), if the fif was jpg
  ! you could call a dialog to set the compression
  Self.OnSave(fif)
  Return

!--------------------------------------------------------------------------
! OnSave()
!
! This method is called immediately after the library call to save the image
!
!--------------------------------------------------------------------------
FreeImageClass.OnSave              Procedure(FREE_IMAGE_FORMAT fif)!,Virtual
  Code
  Return

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.SaveAs               Procedure(String sImageFilename, FREE_IMAGE_FORMAT fif=FIF_UNKNOWN)!,BOOL,Proc
!--------------------------------------------------------------------------
bResult         BOOL,Auto
SaveFileDialog  FileDialogClass
   Code
   bResult = False
   If sImageFilename = ''
     SaveFileDialog.SetDialogFilter(Self.GetFileDialogFilter(FIFF_SUPPORTSwrite))
     If SaveFileDialog.Show(, FILE:Save+FILE:KeepDir, ,SA_OFN_HIDEREADONLY)
       If Self.iImage.SetFilename(SaveFileDialog.szFullFilename)
         bResult = Self.iImage.Save(Self.szImageFilename, fif)
       End
     End
   Else
     If Self.iImage.SetFilename(sImageFilename)
       bResult = Self.iImage.Save(Self.szImageFilename, fif)
     End
   End

   Return bResult




!--------------------------------------------------------------------------
!Bitmap type Flag                Description
!--------------------------------------------------------------------------
!BMP         BMP_DEFAULT         Save without any compression
!            BMP_SAVE_RLE        Compress the bitmap using RLE when saving
!
!JPEG        JPEG_DEFAULT        Saves with good quality (75:1)
!            JPEG_QUALITYSUPERB  Saves with superb quality (100:1)
!            JPEG_QUALITYGOOD    Saves with good quality (75:1)
!            JPEG_QUALITYNORMAL  Saves with normal quality (50:1)
!            JPEG_QUALITYAVERAGE Saves with average quality (25:1)
!            JPEG_QUALITYBAD     Saves with bad quality (10:1)
!            Integer x in [0..100] Save with quality x:100
!
!PBM, PGM, PPM
!            PNM_DEFAULT     Saves the bitmap as a binary file
!            PNM_SAVE_RAW        Saves the bitmap as a binary file
!            PNM_SAVE_ASCII      Saves the bitmap as an ASCII file
!
!TIFF        TIFF_DEFAULT        Save using CCITTFAX4 compression for
!                                1-bit bitmaps and LZW compression for any other bitmaps
!            TIFF_CMYK           Stores tags for separated CMYK (use BOR to combine with
!                                TIFF compression flags)
!            TIFF_PACKBITS       Save using PACKBITS compression.
!            TIFF_DEFLATE        Save using DEFLATE compression (also known as ZLIB compression)
!            TIFF_ADOBE_DEFLATE  Save using ADOBE DEFLATE compression
!            TIFF_NONE           Save without any compression
!            TIFF_CCITTFAX3      Save using CCITT Group 3 fax encoding
!            TIFF_CCITTFAX4      Save using CCITT Group 4 fax encoding
!            TIFF_LZW            Save using LZW compression
!            TIFF_JPEG           Save using JPEG compression
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetSaveOption       Procedure(Long SaveOption)
!--------------------------------------------------------------------------
  Code
  Self.SaveOption = SaveOption
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetSaveOption       Procedure()!,Long
!--------------------------------------------------------------------------
  Code
  Return Self.SaveOption


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.NewImage Procedure(Long nWidth, Long nHeight, Long nBPP,|
                                         FREE_IMAGE_FORMAT fif=FIF_UNKNOWN )!,BOOL
!--------------------------------------------------------------------------
  Code

  If Self.pImage
    FreeImage_Unload(Self.pImage)
  End

  Self.pImage = 0
  Self.bRetainImage = False

  If Self.fif = FIF_UNKNOWN  
    Self.fif = FIF_PNG
  End
  !Create the bitmap, this image has all bits set to zero
  !this usually creates a black bitmap.
  Self.pImage = FreeImage_Allocate(nWidth, nHeight, nBPP)  
  Self.iImage.GetSize(Self.imageSize)
  
  If Self.pImage <> 0
    Self.iImage.SetFilename('')
  End
  Return Choose(Self.pImage <> 0, True, False)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Blank Procedure()
!--------------------------------------------------------------------------
  Code

  If Self.pImage
    FreeImage_Unload(Self.pImage)
    Self.pImage = 0
  End

  Return


!--------------------------------------------------------------------------
! Percent change range -100 to +100, where zero is no change and -percent
! is darker and +percent is brighter.  Valid for 8, 24, and 32 bit images
!--------------------------------------------------------------------------
FreeImageClass.iImage.Brightness       Procedure(Real PercentChange)!,BOOL,Proc
!--------------------------------------------------------------------------
  Code
  If FreeImage_GetBPP(Self.pImage) < 8 And Self.bAutoColorDepth = True
    Self.iImage.ConvertColorDepth(FI_24BIT, , )
  End
  Return FreeImage_AdjustBrightness(Self.pImage, PercentChange)


!--------------------------------------------------------------------------
! Percent change range -100 to +100, where zero is no change and -percent
! is less contrast and +percent more contrast.  Valid for 8, 24, and 32 bit images
!--------------------------------------------------------------------------
FreeImageClass.iImage.Contrast       Procedure(Real PercentChange)!,BOOL,Proc
!--------------------------------------------------------------------------
  Code
  If FreeImage_GetBPP(Self.pImage) < 8 And Self.bAutoColorDepth = True
    Self.iImage.ConvertColorDepth(FI_8BIT, , )
  End
  Return FreeImage_AdjustContrast(Self.pImage, PercentChange)


!--------------------------------------------------------------------------
! ChangeGamma is a value between >0 and < 1.0 to darken 1.0 = no change and
! > 1.0 lightens the image.
!--------------------------------------------------------------------------
FreeImageClass.iImage.Gamma               Procedure(Real changeGamma)!,BOOL,Proc
  Code
  If FreeImage_GetBPP(Self.pImage) < 8 And Self.bAutoColorDepth = True
    Self.iImage.ConvertColorDepth(FI_24BIT, , )
  End
  Return FreeImage_AdjustGamma(Self.pImage, Choose(changeGamma > 0, changeGamma, 1.0) )


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Invert              Procedure()!,BOOL,Proc
  Code
  Return FreeImage_Invert(Self.pImage)


!--------------------------------------------------------------------------
FreeImageClass.iImage.SetAutoColorDepth   Procedure(BOOL bAutoColor=True)
!--------------------------------------------------------------------------
  Code
  !Anything other than false will turn on
  !automatic color conversions.  When this property is true
  !images are upconverted to a color depth to successfully
  !perform an operation.  Usually to 24 bit.
  Self.bAutoColorDepth = Choose(bAutoColor <> False) 
  Return
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetAutoColorDepth   Procedure()!,BOOL
!--------------------------------------------------------------------------
  Code
  Return Self.bAutoColorDepth

!--------------------------------------------------------------------------
! Replace the current image with the new image
!--------------------------------------------------------------------------
FreeImageClass.iImage.ConvertColorDepth   Procedure(COLORDEPTH cvtToColorDepth,  |
                                             UNSIGNED dither=FID_NONE,           |
                                             UNSIGNED oneBitThreshold=128 |
                                             )!,BOOL,Proc
!--------------------------------------------------------------------------
dstImage    FreeImageClass
  Code
  If Self.iImage.ConvertColorDepth(dstImage.iImage, cvtToColorDepth, dither, oneBitThreshold)
    dstImage.bRetainImage = True !ensure temp image is not unloaded
    Return Self.iImage.ReplaceImage(dstImage.pImage)
  End
  Return False


!--------------------------------------------------------------------------
! Return converted image in the passed FreeImageClass
!--------------------------------------------------------------------------
FreeImageClass.iImage.ConvertColorDepth   Procedure(*iImage dstImage,     |
                                             COLORDEPTH cvtToColorDepth,  |
                                             Long dither=FID_NONE,        |
                                             UNSIGNED oneBitThreshold=128 |
                                             )!,BOOL,Proc
!--------------------------------------------------------------------------
pIntImage   LPFIBITMAP,Auto
QuantMethod FREE_IMAGE_QUANTIZE,Auto
pDstImage   LPFIBITMAP,Auto
  Code

  Case cvtToColorDepth
  Of FI_1BIT
    If dither = FID_NONE
      pDstImage = FreeImage_Threshold(Self.pImage, oneBitThreshold)
    Else
      pDstImage = FreeImage_Dither(Self.pImage, dither)
    End

  Of FI_4BIT
    pDstImage = FreeImage_ConvertTo4Bits(Self.pImage)

  Of FI_8BIT OrOf FI_8BITCOLOR
    If Not (dither = FIQ_WUQUANT Or dither = FIQ_NNQUANT)
      QuantMethod = FIQ_WUQUANT
    Else
      QuantMethod = dither
    End

    If FreeImage_GetBPP(Self.pImage) <> 24
      !First Create an intermediate 24bpp image
      pIntImage = FreeImage_ConvertTo24Bits(Self.pImage)           
      If pIntImage <> 0
        pDstImage = FreeImage_ColorQuantize(pIntImage, QuantMethod)
        FreeImage_Unload(pIntImage)
      Else
        pDstImage = 0
      End
    Else
      pDstImage = FreeImage_ColorQuantize(Self.pImage, QuantMethod)
    End

  Of FI_8BITGRAY
    pDstImage = FreeImage_ConvertTo8Bits(Self.pImage)
  Of FI_16BIT OrOf FI_16_555BIT
    pDstImage = FreeImage_ConvertTo16Bits555(Self.pImage)
  Of FI_16_565BIT
    pDstImage = FreeImage_ConvertTo16Bits565(Self.pImage)
  Of FI_24BIT
    pDstImage = FreeImage_ConvertTo24Bits(Self.pImage)
  Of FI_32BIT
    pDstImage = FreeImage_ConvertTo32Bits(Self.pImage)
  End
  Return dstImage.ReplaceImage(pDstImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.ConvertToGrayscale  Procedure()!,BOOL,Proc
!--------------------------------------------------------------------------
tempImage   FreeImageClass
  Code
  If Self.iImage.ConvertToGrayscale(tempImage.iImage) = True
    tempImage.bRetainImage = True
    Return Self.iImage.ReplaceImage(tempImage.pImage)
  End

  Return False


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.ConvertToGrayscale  Procedure(*iImage dst)!,BOOL,Proc
!--------------------------------------------------------------------------
pDstImage LPFIBITMAP
  Code
  pDstImage = FreeImage_ConvertToGreyScale(Self.pImage)
  Return dst.ReplaceImage(pDstImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Crop          Procedure(Long X, Long Y, |
                                              Long nWidth, Long nHeight)!,BOOL,Proc
!--------------------------------------------------------------------------
rcSrc   Like(SA_RECT)
   Code
   rcSrc.left = X
   rcSrc.top = Y
   rcSrc.right = X+nWidth
   rcSrc.bottom = Y+nHeight
   Return Self.iImage.Crop(rcSrc)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Crop                 Procedure(*SA_RECT rcSrc)!,BOOL,Proc
!--------------------------------------------------------------------------
pNewImage   UNSIGNED,Auto
  Code
   pNewImage = FreeImage_Copy(Self.pImage, rcSrc.left, rcSrc.top, rcSrc.right, rcSrc.bottom)
   Return Self.iImage.ReplaceImage(pNewImage)


!--------------------------------------------------------------------------
! Lossles JPEG Crop, do not load image first, cropped area rounded to
! nearest 8 or 16 pixels so requested image may be larger
!--------------------------------------------------------------------------
FreeImageClass.iImage.JpegCrop            Procedure(String srcFile, String dstFile, Long X, Long Y, Long nWidth, Long nHeight)!,BOOL,Proc
!--------------------------------------------------------------------------
rcSrc   Like(SA_RECT)
szSrcFile   CString(512)
szDstFile   Cstring(512) 
   Code
   rcSrc.left     = X
   rcSrc.top      = Y
   rcSrc.right    = X+nWidth
   rcSrc.bottom   = Y+nHeight

   szSrcFile      = Clip(srcFile) 
   szDstFile      = Clip(dstFile )            
   
   Return Self.iImage.JpegCrop(szSrcFile, szDstFile, rcSrc)
   
!--------------------------------------------------------------------------  
! Lossles JPEG Crop, do not load image first, cropped area rounded to
! nearest 8 or 16 pixels so requested image may be larger
!--------------------------------------------------------------------------
FreeImageClass.iImage.JpegCrop            Procedure(*Cstring srcFile, *Cstring dstFile, *SA_RECT rcSrc)!,BOOL,Proc
!--------------------------------------------------------------------------
   Code
   Return FreeImage_JpegCrop(srcFile, dstFile, rcSrc.left, rcSrc.top, rcSrc.right, rcSrc.bottom)

 
!--------------------------------------------------------------------------
! Lossles JPEG Transforms, do not load image first, transformed image is 
! rounded to nearest 8 or 16 pixels if Perfect=False (omitted default)
!--------------------------------------------------------------------------
FreeImageClass.iImage.JpegTransform       Procedure(String srcFile, String dstFile, Long Operation, BOOL Perfect=0)!,BOOL,Proc
szSrcFile   CString(512)
szDstFile   Cstring(512) 
   Code  
   if Not InRange(Operation, FIJPEG_OP_FLIP_H, FIJPEG_OP_ROTATE_270)
     Operation = FIJPEG_OP_NONE
   end   
   szSrcFile = Clip(srcFile)
   szDstFile = Clip(dstFile)

   Return FreeImage_JpegTransform(szSrcFile, szDstFile, Operation, Perfect) 
 
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.CopyImage            Procedure(*iImage dstImage, |
                                              Long X, Long Y, |
                                              Long nWidth, Long nHeight)!,BOOL,Proc
!--------------------------------------------------------------------------
rcSrc   Like(SA_RECT)
   Code
   rcSrc.left = X
   rcSrc.top = Y
   rcSrc.right = X+nWidth
   rcSrc.bottom = Y+nHeight
   Return Self.iImage.CopyImage(dstImage, rcSrc)

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.CopyImage            Procedure(*iImage dstImage, *SA_RECT rcSrc)!,BOOL,Proc
!--------------------------------------------------------------------------
pNewImage   LPFIBITMAP
  Code
  pNewImage = FreeImage_Copy(Self.pImage, rcSrc.left, rcSrc.top, rcSrc.right, rcSrc.bottom)
  Return dstImage.ReplaceImage(pNewImage)

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.CopyImage             Procedure(*iImage dstImage)!,BOOL,Proc
!--------------------------------------------------------------------------
pNewImage   LPFIBITMAP
  Code
  pNewImage = FreeImage_Clone(Self.pImage)
  Return dstImage.ReplaceImage(pNewImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetBpp  Procedure()!,Long
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetBPP(Self.pImage)


!--------------------------------------------------------------------------
! Returns one of the following values:
!
! Value             Description
! FIC_MINISBLACK    Monochrome bitmap (1-bit) : first palette entry is black.
!                   Palletised bitmap (4 or 8-bit) : the bitmap has a greyscale palette
! FIC_MINISWHITE    Monochrome bitmap (1-bit) : first palette entry is white.
!                   Palletised bitmap (4 or 8-bit) : the bitmap has an inverted greyscale palette
! FIC_PALETTE       Palettized bitmap (1, 4 or 8 bit) FIC_RGB High-color bitmap (16, 24 or 32 bit)
! FIC_RGBALPHA      High-color bitmap with an alpha channel (32 bit only)
! FIC_CMYK          CMYK bitmap (32 bit only)
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetColorType        Procedure()!,FREE_IMAGE_COLOR_TYPE
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetColorType(Self.pImage)


!--------------------------------------------------------------------------
! Returns width of line in bytes rounded to next 32 bit boundry
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetPitch            Procedure()!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetPitch(Self.pImage)


!--------------------------------------------------------------------------
! Returns the size of the DIB-element of a FIBITMAP in memory,
! i.e. the BITMAPINFOHEADER + palette + data bits
! (note that this is not the real size of a FIBITMAP,
! only the size of its DIB-element).
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetDIBSize           Procedure()!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetDIBSize(Self.pImage)



!--------------------------------------------------------------------------
! Returns the horizontal resolution, in pixels-per-meter
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetDotsPerMeterX    Procedure()!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetDotsPerMeterX(Self.pImage)


!--------------------------------------------------------------------------
! Sets the horizontal resolution, in pixels-per-meter
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetDotsPerMeterX    Procedure(UNSIGNED resolution)
!--------------------------------------------------------------------------
  Code
  FreeImage_SetDotsPerMeterX(Self.pImage, resolution)
  Return


!--------------------------------------------------------------------------
! Returns the vertical resolution, in pixels-per-meter
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetDotsPerMeterY    Procedure()!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  Return  FreeImage_GetDotsPerMeterY(Self.pImage)


!--------------------------------------------------------------------------
! Sets the vertical resolution, in pixels-per-meter
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetDotsPerMeterY    Procedure(UNSIGNED resolution)
!--------------------------------------------------------------------------
  Code
  FreeImage_SetDotsPerMeterY(Self.pImage, resolution)
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetRedMask          Procedure()!,UNSIGNED
  Code
  Return FreeImage_GetRedMask(Self.pImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetGreenMask        Procedure()!,UNSIGNED
  Code
  Return FreeImage_GetGreenMask(Self.pImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetBlueMask         Procedure()!,UNSIGNED
  Code
  Return FreeImage_GetBlueMask(Self.pImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetTransparencyCount Procedure()!,UNSIGNED
  Code
  Return FreeImage_GetTransparencyCount(Self.pImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetTransparencyTable Procedure()!,UNSIGNED
  Code
  Return FreeImage_GetTransparencyTable(Self.pImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetTransparencyTable Procedure(UNSIGNED pTranTable, Long cbTranTable)
  Code
  FreeImage_SetTransparencyTable(Self.pImage, pTranTable, cbTranTable)
  Return


!--------------------------------------------------------------------------
! Only valid for 8 and 32 bit images
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetTransparent      Procedure(BOOL bTransparent=True)
  Code
  FreeImage_SetTransparent(Self.pImage, bTransparent)
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.IsTransparent       Procedure()!,BOOL
  Code
  Return FreeImage_IsTransparent(Self.pImage)


!--------------------------------------------------------------------------
! 8, 24, 32 bit images
!--------------------------------------------------------------------------
FreeImageClass.iImage.HasBackgroundColor  Procedure()!,BOOL
  Code
  Return FreeImage_HasBackgroundColor(Self.pImage)



!--------------------------------------------------------------------------
! 8 bit background color returned in rgbReserved element of rgbquad
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetBackgroundColor  Procedure(*SA_RGBQUAD rgbColor)!,BOOL
  Code
  Return FreeImage_GetBackgroundColor(Self.pImage, rgbColor)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetBackgroundColor  Procedure(*SA_RGBQUAD rgbColor)!,BOOL
!--------------------------------------------------------------------------
  Code
  Return FreeImage_SetBackgroundColor(Self.pImage, rgbColor)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.FillBackground      Procedure(*SA_RGBQUAD backgroundColor, Long Options=0)!,BOOL
  Code
  Return FreeImage_FillBackground(backgroundColor, Options)         
  

!--------------------------------------------------------------------------
! Alpha blend a sub image (must be = or smaller then target image.  Target
! image (this image) Must have BPP >= sub image
!--------------------------------------------------------------------------
FreeImageClass.iImage.AlphaBlend      Procedure(*iImage srcImage, Long X, Long Y, Long nAlphaBlend)!,BOOL,Proc
!--------------------------------------------------------------------------
dstImageBPP UNSIGNED,Auto
  Code
  dstImageBPP = Self.iImage.GetBpp()
  If Self.iImage.GetBpp() < srcImage.GetBpp()
    Self.iImage.TakeError(-1, 'destination BPP must be >= source BPP')
  End
  If Self.imageSize.width < srcImage.GetWidth() Or Self.imageSize.height < srcImage.GetHeight()
    Self.iImage.TakeError(-1, 'destination width and height must be >= source width and height')
  End
  If Self.iImage.GetBpp() = 1 And Self.bAutoColorDepth = True
    Self.iImage.ConvertColorDepth(FI_24BIT, , )
  End
  Return FreeImage_Paste(Self.pImage, srcImage.GetBitmap(), X, Y, nAlphaBlend)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Composite           Procedure(UNSIGNED bkgColor=0)!,BOOL,Proc
!--------------------------------------------------------------------------
pNewImage   LPFIBITMAP,Auto
TheBkgColor   Like(SA_RGBQUAD)
BGR           Like(SA_BGRQUAD),Over(bkgColor)
  Code 
  if Band(bkgColor, 80000000H) = 80000000H             !it's a Clarion System Color Constant like COLOR:BTNFACE
    bkgColor = SA_GetSysColor(Band(bkgColor, 0FFH))
  end
  Self.iImage.BGRtoRGB(TheBkgColor, BGR)
  pNewImage = FreeImage_Composite(Self.pImage, False, address(TheBkgColor), )
  Return Self.iImage.ReplaceImage(pNewImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Composite           Procedure(*iImage bkgImage)!,BOOL,Proc
!--------------------------------------------------------------------------
pNewImage   LPFIBITMAP,Auto
  Code
  If FreeImage_GetBPP(bkgImage.GetBitmap()) <> 24
    bkgImage.ConvertColorDepth(FI_24BIT, , )
  End
  pNewImage = FreeImage_Composite(Self.pImage, 0, 0, bkgImage.GetBitmap())
  Return Self.iImage.ReplaceImage(pNewImage)

!--------------------------------------------------------------------------
! Alpha blend this image with BkgImage and return result in DstImage
!--------------------------------------------------------------------------
FreeImageClass.iImage.Composite           Procedure(*iImage DstImage, *iImage bkgImage)!,BOOL,Proc
!--------------------------------------------------------------------------
pNewImage   LPFIBITMAP,Auto
  Code
  If FreeImage_GetBPP(bkgImage.GetBitmap()) <> 24
    bkgImage.ConvertColorDepth(FI_24BIT, , )
  End
  pNewImage = FreeImage_Composite(Self.pImage, 0, 0, bkgImage.GetBitmap())
  Return DstImage.ReplaceImage(pNewImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Rotate              Procedure(Real fAngle)!,BOOL,Proc
!--------------------------------------------------------------------------
pNewImage   LPFIBITMAP,Auto
imageBPP    UNSIGNED,Auto
  Code
  If fAngle = 0.0 Then Return True; End

  imageBPP = FreeImage_GetBPP(Self.pImage)
  If imageBPP = 4 And Self.bAutoColorDepth = True
    Self.iImage.ConvertColorDepth(FI_24BIT, , )
  ElsIf imageBPP = 1
    If fAngle % 90 <> 0
      !ToDo adjust to nearest 90 degrees for 1 bit images
      Return False
    End
  End
!////////////////////////////////////////////////////////////////////////////////////  
! Modified 4-13-2020 by Charles Edmonds
! Replaced deprecated call to FreeImage_RotateClassic with a call to FreeImage_Rotate
!  pNewImage = FreeImage_RotateClassic(Self.pImage, fAngle)
!////////////////////////////////////////////////////////////////////////////////////  
  pNewImage = FreeImage_Rotate(Self.pImage, fAngle)
  Return Self.iImage.ReplaceImage(pNewImage)

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Rotate       Procedure(Real fAngle, |
                                             Real xShift, Real yShift, |
                                             Real xOrigin, Real yOrigin, |
                                             BOOL bUseMask)!,BOOL,Proc
!--------------------------------------------------------------------------
pNewImage    LPFIBITMAP,Auto
  Code
  If FreeImage_GetBPP(Self.pImage) <= 4 And Self.bAutoColorDepth = True
    Self.iImage.ConvertColorDepth(FI_24BIT, , )
  End

  pNewImage = FreeImage_RotateEx(Self.pImage, fAngle, xShift, yShift, xOrigin, yOrigin, bUseMask)
  Return Self.iImage.ReplaceImage(pNewImage)

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.RotateImageOnEXIFOrientation     Procedure(*iImage thisImage)
  Code
  Case lower(thisImage.GetMetadata(FIMD_EXIF_MAIN,'Orientation'))
  of 'left side, bottom'
     thisImage.Rotate(90)

  of 'top, left side'
     !don't need to do anything for this orientation

  of 'bottom, right side'
    thisImage.Flip(FI_FLIPVERT)

  of 'right side, top'
    thisImage.Rotate(-90)

  end

  Return
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Flip         Procedure(UNSIGNED HorzOrVert=FI_FLIPHORIZ)!,BOOL,Proc
!--------------------------------------------------------------------------
bResult     BOOL,Auto
  Code
  bResult = False
  Case HorzOrVert
  Of FI_FLIPHORIZ
    bResult = FreeImage_FlipHorizontal(Self.pImage)
  Of FI_FLIPVERT
    bResult = FreeImage_FlipVertical(Self.pImage)
  End
  Return bResult


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Rescale        Procedure(*Real fPercentX, |
                                               *Real fPercentY, |
                                               FREE_IMAGE_FILTER fiFilter)
!--------------------------------------------------------------------------
fScaleFactorX Real,Auto
fScaleFactorY Real,Auto
  Code
  fScaleFactorX = fPercentX/100
  fScaleFactorY = fPercentY/100                                     
  Return Self.iImage.Rescale(Self.imageSize.width*fScaleFactorX, Self.imageSize.height*fScaleFactorY, fiFilter)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Rescale             Procedure(*Real fPercent, |
                                                    FREE_IMAGE_FILTER fiFilter)
!--------------------------------------------------------------------------
fScaleFactor Real,Auto
  Code
  fScaleFactor = fPercent/100
  Return Self.iImage.Rescale(Self.imageSize.width*fScaleFactor, Self.imageSize.height*fScaleFactor, fiFilter)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Rescale      Procedure(*iImage dstImage, |
                                             Real fPercent,    |
                                             FREE_IMAGE_FILTER fiFilter)
!--------------------------------------------------------------------------
fScaleFactor Real,Auto
  Code
  fScaleFactor = fPercent/100
  Return Self.iImage.Rescale(dstImage, Self.imageSize.width*fScaleFactor, Self.imageSize.height*fScaleFactor, fiFilter)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Rescale      Procedure(UNSIGNED nDstWidth, |
                                             UNSIGNED nDstHeight,|
                                             FREE_IMAGE_FILTER fiFilter)
!--------------------------------------------------------------------------
pNewImage   LPFIBITMAP,Auto
  Code

 
  If FreeImage_GetBPP(Self.pImage) < 24 And Self.bAutoColorDepth = True
    Self.iImage.ConvertColorDepth(FI_24BIT, , )
  End

  pNewImage = FreeImage_Rescale(SELF.pImage, nDstWidth, nDstHeight, fiFilter)
  Return Self.iImage.ReplaceImage(pNewImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Rescale      Procedure(*iImage dstImage,    |
                                             UNSIGNED nDstWidth,  |
                                             UNSIGNED nDstHeight, |
                                             FREE_IMAGE_FILTER fiFilter)
!--------------------------------------------------------------------------
pNewImage   LPFIBITMAP,Auto
  Code
  If FreeImage_GetBPP(Self.pImage) < 24 And Self.bAutoColorDepth = True
    Self.iImage.ConvertColorDepth(FI_24BIT, , )
  End

  pNewImage = FreeImage_Rescale(Self.pImage, nDstWidth, nDstHeight, fiFilter)
  Return dstImage.ReplaceImage(pNewImage)

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.FitTo        Procedure(UNSIGNED nDstWidth,                 |
                                             UNSIGNED nDstHeight,                |
                                             UNSIGNED fiFilter=FILTER_BILINEAR,  |
                                             UNSIGNED fitMethod=CFIFIT_BOTH,     | 
                                             UNSIGNED limitLongSideTo=0,         |
                                             BOOL maintainAspectRatio=True,      |
                                             BOOL adjustPower2=False) !,BOOL
!--------------------------------------------------------------------------
aspectRatio     Real,Auto   !Aspect ratio of original image     
scaledHeight    UNSIGNED
scaledWidth     UNSIGNED  
longSide        Long

  Code
  If fitMethod = CFIFIT_NONE Or (Self.imageSize.width = nDstWidth And Self.imageSize.height = nDstHeight) And Not adjustPower2
    Return False
  End  
  longSide    = Self.iImage.GetLongSide()
  aspectRatio = Self.iImage.GetAspectRatio()

  scaledWidth   = nDstWidth
  scaledHeight  = nDstHeight
  
  if FitMethod = CFIFIT_BEST    !Contributed by Frank O'Classen 2013.09.19 for image control resizing for consistency make it an alias for both with aspect ratio.
     FitMethod           = CFIFIT_BOTH
     maintainAspectRatio = True  
     adjustPower2        = False
  end
  
  
  Case FitMethod 
     
  Of CFIFIT_BOTH 
    If maintainAspectRatio
      Case longSide
      Of CFI_WIDTH OrOf CFI_SQUARE                  
        scaledHeight = nDstWidth * aspectRatio
      Else
        scaledWidth = nDstHeight * aspectRatio
      End
    End 
    
  Of CFIFIT_WIDTH
    If maintainAspectRatio
      Case longSide
      Of CFI_WIDTH OrOf CFI_SQUARE                  
        scaledHeight = nDstWidth * aspectRatio
      Else
        scaledHeight = nDstWidth * (Self.imageSize.height / Self.imageSize.width)
      End
    End

  Of CFIFIT_HEIGHT
    If maintainAspectRatio
      Case longSide
      Of CFI_WIDTH OrOf CFI_SQUARE                  
        scaledWidth = nDstHeight * (Self.imageSize.width / Self.imageSize.height)
      Else
        scaledWidth = nDstHeight * aspectRatio
      End
    End
  End 
  
  !--------------- Make Sides a Power Of Two -----------------
  If adjustPower2   
    scaledWidth = Self.iImage._PowerOfTwo_Nearest(scaledWidth)
    scaledHeight = Self.iImage._PowerOfTwo_Nearest(scaledHeight)
  End
    
  !--------------- Limit longest side to a maximum number of pixels -------------  
  If limitLongSideTo > 0
    Self.iImage.LimitLongSide(longSide, limitLongSideTo, scaledWidth, scaledHeight)
    If adjustPower2   
      scaledWidth = Self.iImage._PowerOfTwo_Nearest(scaledWidth)
      scaledHeight = Self.iImage._PowerOfTwo_Nearest(scaledHeight)
    End
  End


  if longSide = CFI_SQUARE And maintainAspectRatio
    scaledHeight = scaledWidth
  end

  Return Self.iImage.Rescale(scaledWidth, scaledHeight, fiFilter)
                                                                        
                                                                        
                                                                        
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.limitLongSide       Procedure(Long longSide, UNSIGNED limitLongSideTo, *UNSIGNED scaledWidth, *UNSIGNED scaledHeight)
!--------------------------------------------------------------------------
  Code
  Case longSide
  Of CFI_WIDTH OrOf CFI_SQUARE                                            
     If scaledWidth > limitLongSideTo     
        scaledWidth  = limitLongSideTo      
        !This is scaled by aspect ratio
        scaledHeight = scaledWidth * (Self.imageSize.height / Self.imageSize.width)
     End
  Else
     If scaledHeight > limitLongSideTo       
        !This is scaled by aspect ratio
        scaledWidth  = scaledHeight * (Self.imageSize.width / Self.imageSize.height)
        scaledHeight = limitLongSideTo
     End
  End


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage._PowerOfTwo_Nearest Procedure(ULONG xValue) !ULONG
!--------------------------------------------------------------------------
! assumes positive values only, so changed to ULONG
! Thanks to Mark Goldberg for this method
PowerOfTwo ULONG,Auto
 Code
 PowerOfTwo = Self.iImage._PowerOfTwo_Floor(xValue)
 If PowerOfTwo And xValue - PowerOfTwo > PowerOfTwo / 2  !round it off...
    PowerOfTwo *= 2
 End
 Return PowerOfTwo     
                      
                      
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage._PowerOfTwo_Floor Procedure(ULONG xValue) !ULONG
PowerOfTwo ULONG(1)
! http://stackoverflow.com/questions/466204/rounding-off-to-nearest-power-of-2
! Thanks to Mark Goldberg for this method
 Code
 If xValue = 0
    PowerOfTwo = 0
 Else
    Loop
      Case xValue
        Of 1
          Break
        Of 3
          PowerOfTwo *= 4
          Break
      End
      xValue = BShift(xValue,-1) ! /= 2
      PowerOfTwo = BShift(PowerOfTwo ,1) ! *= 2
    End
 End
 Return PowerOfTwo 
 
 
 
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetLongSide         Procedure()!,Long
!--------------------------------------------------------------------------
longSide Long
  Code
  If Self.imageSize.width > Self.imageSize.height
    longSide = CFI_WIDTH
  Elsif Self.imageSize.width = Self.imageSize.height  
    longSide = CFI_SQUARE
  Else
    longSide = CFI_HEIGHT
  End
  Return longSide
  
  
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetAspectRatio      Procedure()!,Real
aspectRatio Real,Auto
!--------------------------------------------------------------------------
  Code
  Case Self.iImage.GetLongSide()
  of CFI_WIDTH OrOf CFI_SQUARE
    aspectRatio = Self.imageSize.height / Self.imageSize.width
  Else
    aspectRatio = Self.imageSize.width / Self.imageSize.height
  End
  Return aspectRatio


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Thumbnail    Procedure(*iImage dstImage,    |
                                             UNSIGNED nDstWidth,  |
                                             FREE_IMAGE_FILTER fiFilter)
!--------------------------------------------------------------------------
fAspectRatio    Real,Auto   !Aspect ratio of original image
  Code  
  fAspectRatio = Self.iImage.GetAspectRatio()
  If Self.imageSize.width >= Self.imageSize.height
    Return Self.iImage.Rescale(dstImage, nDstWidth, nDstWidth*fAspectRatio, fiFilter)
  Else
    Return Self.iImage.Rescale(dstImage, nDstWidth*fAspectRatio, nDstWidth, fiFilter)
  End


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Thumbnail           Procedure(UNSIGNED nDstWidth, |
                                                    FREE_IMAGE_FILTER fiFilter)
!--------------------------------------------------------------------------
dstImageCl    FreeImageClass
  Code
  dstImageCl.bRetainImage = True
  Self.iImage.Thumbnail(dstImageCl.iImage, nDstWidth, fiFilter)
  Return Self.iImage.ReplaceImage(dstImageCl.pImage)



!-------------------------------------------------------------------------- 
! Fit image into nMaxPixelSize square maintaining Aspect ratio
!--------------------------------------------------------------------------
FreeImageClass.iImage.Thumbnail           Procedure(UNSIGNED nMaxPixelSize)!,BOOL,Proc
dstImage  FreeImageClass
  Code      
  dstImage.bRetainImage = True  
  if Self.iImage.Thumbnail(dstImage.iImage, nMaxPixelSize)
    Return Self.iImage.ReplaceImage(dstImage.pImage)
  else
    Return False
  end
 

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.Thumbnail           Procedure(*iImage dstImage, |
                                                    UNSIGNED nMaxPixelSize)!,BOOL,Proc
pNewImage   LPFIBITMAP,Auto
  Code
  pNewImage = FreeImage_MakeThumbnail(Self.pImage, nMaxPixelSize, True)   
  Return dstImage.ReplaceImage(pNewImage)
 
 
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.ResizeCanvas        Procedure(Long leftPx, Long topPx, Long rightPx, Long bottomPx, Long pColor)!,BOOL,Proc

RGBA        Like(SA_RGBQUAD)
BGRA        Like(SA_RGBQUAD), Over(pColor)   
dstImageCl  FreeImageClass
  Code      

  Self.iImage.BGRtoRGB(RGBA, BGRA)
  dstImageCl.bRetainImage = True      
  if self.iImage.ResizeCanvas(dstImageCl.iImage, leftPx, topPx, rightPx, bottomPx, RGBA)
    Return Self.iImage.ReplaceImage(dstImageCl.pImage)
  else
    Return False
  end

 
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.ResizeCanvas   Procedure(*iImage dstIimage, Long leftPx, Long topPx, Long rightPx, Long bottomPx, *SA_RGBQUAD pColor, Long options=0)!,BOOL   
pNewImage   LPFIBITMAP,Auto
  Code
  pNewImage = FreeImage_EnlargeCanvas(Self.pImage, leftPx, topPx, rightPx, bottomPx, pColor, )  
  Return dstIimage.ReplaceImage(pNewImage)
  


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetPixel            Procedure(UNSIGNED x, UNSIGNED y)!,Long
!--------------------------------------------------------------------------
RGBA Like(SA_RGBQUAD)
BGRA Like(SA_BGRQUAD)
LColor Long,Over(BGRA)
pixelIndex Byte
  Code
  If Self.iImage.GetBpp() <= 8
    If FreeImage_GetPixelIndex(Self.pImage, x, y, pixelIndex)
      lColor = pixelIndex
    End
  Else
    y = Self.imageSize.height -y  !image is a bottom up dib
    If FreeImage_GetPixelColor(Self.pImage, x, y, RGBA)
      Self.iImage.RGBToBGR(BGRA, RGBA)
    Else
     lColor = 0 !black
    End
  End
  Return lColor


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetPixel            Procedure(UNSIGNED x, UNSIGNED y, Long BGRAColor)!,BOOL
!--------------------------------------------------------------------------
RGBA    Like(SA_RGBQUAD)
BGRA    Group(SA_BGRQUAD),Over(BGRAColor)
        End

bmi  &SA_BITMAPINFOHEADER

pixelIndex  Byte

bRetVal     BOOL,Auto
  Code

  bmi &= FreeImage_GetInfoHeader(Self.pImage)
  Self.iImage.BGRtoRGB(RGBA, BGRA)

  Self.iImage.GetPalette()
  pixelIndex = Self.iImage.FindNearestColorIndex(BGRAColor)


  y = Self.imageSize.height -y  !image is a bottom up dib
  If FreeImage_GetBPP(Self.pImage) > 8
    bRetVal = FreeImage_SetPixelColor(Self.pImage, x, y, RGBA)
  Else
    bRetVal = FreeImage_SetPixelIndex(Self.pImage, x, y, pixelIndex)
  End
  Return true


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetPalette           Procedure()
!--------------------------------------------------------------------------
  Code
  Self.pPalette = FreeImage_GetPalette(Self.pImage)
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetPaletteEntry           Procedure(Long nPaletteIndex)!,Long
!--------------------------------------------------------------------------
RGBA        Like(SA_RGBQUAD)
LRetVal     Long,Over(RGBA)

  Code
  If Self.pPalette
    SA_memcpyRawDst(RGBA, (Self.pPalette + (nPaletteIndex-1)*4), 4)
  End
  Return LRetVal


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.FindNearestColorIndex    Procedure(Long TheColor)!,Long
!--------------------------------------------------------------------------
i   Long,Auto
  Code

  Loop i = 1 to 256
    If Self.iImage.GetPaletteEntry(i) >= TheColor
      Break
    End
  End
  Return i


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetBitmap           Procedure()!,LPFIBITMAP
!--------------------------------------------------------------------------
  Code
  Return Self.pImage


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetBits             Procedure()!,UNSIGNED !Returns pointer to image bits 16 byte aligned
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetBits(Self.pImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetScanLine          Procedure(Long nScanLine)!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetScanLine(Self.pImage, nScanLine)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetChannel       Procedure(*iImage dstImage, FREE_IMAGE_COLOR_CHANNEL ficc=FICC_RED)!,BOOL
!--------------------------------------------------------------------------
pDstImage  LPFIBITMAP,Auto
  Code
  !must be 24 or 32 bit image
  If FreeImage_GetBPP(Self.pImage) < 24 And Self.bAutoColorDepth = True   
    Self.iImage.ConvertColorDepth(FI_24BIT, , )
  End

  pDstImage = FreeImage_GetChannel(Self.pImage, ficc)
  Return dstImage.ReplaceImage(pDstImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetChannel       Procedure(*iImage Bit8Image, FREE_IMAGE_COLOR_CHANNEL ficc=FICC_RED)!,BOOL
!--------------------------------------------------------------------------
pDstImage  LPFIBITMAP,Auto
  Code
  !must be 24 or 32 bit image
  If FreeImage_GetBPP(Self.pImage) < 24 And Self.bAutoColorDepth = True   
    Self.iImage.ConvertColorDepth(FI_24BIT, , )
  End

  Return FreeImage_SetChannel(Self.pImage, Bit8Image.GetBitmap(), ficc)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.BGRtoRGB              Procedure(*SA_RGBQUAD RGB, *SA_BGRQUAD BGR)
!--------------------------------------------------------------------------
  Code
  RGB.rgbRed    = BGR.bgrRed
  RGB.rgbBlue   = BGR.bgrBlue
  RGB.rgbGreen  = BGR.bgrGreen
  RGB.rgbReserved  = BGR.bgrReserved
  Return

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.RGBtoBGR            Procedure(*SA_BGRQUAD BGR, *SA_RGBQUAD RGB)
!--------------------------------------------------------------------------
  Code
  BGR.bgrRed    = RGB.rgbRed
  BGR.bgrBlue   = RGB.rgbBlue
  BGR.bgrGreen  = RGB.rgbGreen
  BGR.bgrReserved = RGB.rgbReserved
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetWidth               Procedure()!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetWidth(Self.pImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetHeight              Procedure()!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetHeight(Self.pImage)

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetSize             Procedure(*ImageSizeGroup imageSize)
!--------------------------------------------------------------------------
  Code 
  Clear(imageSize)
  if Self.pImage <> 0
    Self.imageSize.width = FreeImage_GetWidth(Self.pImage)
    Self.imageSize.height = FreeImage_GetHeight(Self.pImage)
  End  
  Return
  

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetBMInfoHeader     Procedure()!,UNSIGNED    !Returns address of BITMAPINFOHEADER
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetInfoHeader(Self.pImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetBMInfo           Procedure()!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetInfo(Self.pImage)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetImageBits         Procedure()!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetBits(Self.pImage)

!--------------------------------------------------------------------------
!not implemented
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetICCProfile        Procedure()!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  Return FreeImage_GetICCProfile(Self.pImage)


!--------------------------------------------------------------------------
! not implemented
!--------------------------------------------------------------------------
FreeImageClass.iImage.CreateICCProfile        Procedure()!,UNSIGNED
!--------------------------------------------------------------------------
cbData Long
someData Long
  Code
  cbData = Size(someData)
  Return FreeImage_CreateICCProfile(Self.pImage, someData, cbData)


!--------------------------------------------------------------------------
! Destroys any existing ICC profile.  Save image to complete the destruction
!--------------------------------------------------------------------------
FreeImageClass.iImage.DestroyICCProfile        Procedure()
!--------------------------------------------------------------------------
  Code
  FreeImage_DestroyICCProfile(Self.pImage)
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetCopyright        Procedure()!,String
!--------------------------------------------------------------------------
lpszInfo        UNSIGNED
szFICopyright   &Cstring
  Code

  lpszInfo = FreeImage_GetCopyrightMessage()
  szFICopyright &= lpszInfo&':'& SA_lstrlen(lpszInfo)
  !-----------------------------------------------------------
  ! Note:  This string must be displayed in the program and/or
  !        documentation of any product using FreeImage
  !-----------------------------------------------------------
  Return szFICopyright


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetVersion      Procedure()!,String
!--------------------------------------------------------------------------
lpszInfo        UNSIGNED
szFIDLLVersion   &Cstring
  Code
  lpszInfo = FreeImage_GetVersion()
  szFIDLLVersion &= (lpszInfo)&':'& SA_lstrlen(lpszInfo)
  Return szFIDLLVersion


!--------------------------------------------------------------------------
! Replace this object's current image with the fidib pointed to by pNewImage
!--------------------------------------------------------------------------
FreeImageClass.iImage.ReplaceImage        Procedure(*LPFIBITMAP pNewImage)!,BOOL,Proc
!--------------------------------------------------------------------------
  Code
  If Self.pImage <> 0
    FreeImage_Unload(Self.pImage)
  End

  If pNewImage <> 0
    Self.pImage = pNewImage
    Self.iImage.GetSize(Self.imageSize)   
  Else
    Return False
  End
  Return Choose(Self.pImage <> 0, True, False)


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.EnumMetaData  Procedure(MetaDataQueue mdQ)!,UNSIGNED
!--------------------------------------------------------------------------
!  FIMD_COMMENTS       !0 single comment or keywords
!  FIMD_EXIF_MAIN      !1 Exif-TIFF metadata
!  FIMD_EXIF_EXIF      !2 Exif-specific metadata
!  FIMD_EXIF_GPS       !3 Exif GPS metadata
!  FIMD_EXIF_MAKERNOTE !4 Exif maker note metadata
!  FIMD_EXIF_INTEROP   !5 Exif interoperability metadata
!  FIMD_IPTC           !6 IPTC/NAA metadata
!  FIMD_XMP            !7 Abobe XMP metadata
!  FIMD_GEOTIFF        !8 GeoTIFF metadata
!  FIMD_ANIMATION      !9 Animation metadata
!  FIMD_CUSTOM         !10 Used to attach other metadata types to a dib
metadataModel   UNSIGNED,Auto
nModels     UNSIGNED,Auto
TagCount    UNSIGNED,Auto
tag         &FITAG
lpTag       UNSIGNED
mHandle     UNSIGNED
szTag       &Cstring


  Code
  Free(mdQ)
  nModels = 0
  Loop metadataModel = FIMD_COMMENTS to FIMD_LAST-1
    TagCount = Self.iImage.GetMetadataCount(metadataModel)

    !This image has tags attached for this Metadata Model.
    If TagCount
      nModels += 1
      mHandle = FreeImage_FindFirstMetadata(metadataModel, Self.pImage, lpTag)

      If mHandle
        Loop
          tag &= (lpTag)
          !szTag &= FreeImage_TagToString(metadataModel, tag)
          Clear(mdQ)
          szTag &= FreeImage_GetTagKey(tag)
          mdQ.mdKey = szTag
          szTag &= FreeImage_TagToString(metadataModel, tag)
          mdQ.mdValue = szTag 
          If FreeImage_GetTagDescription(tag)
            szTag &= FreeImage_GetTagDescription(tag)
            mdQ.mdDescription = szTag
           Else
            mdQ.mdDescription = ''
          End
          mdQ.mdModel = metadataModel
          Add(mdQ)
          If Self.iImage.TakeError( ErrorCode() ) Then Break.
          tag &= Null
        Until FreeImage_FindNextMetadata(mHandle, lpTag) = False
        FreeImage_FindCloseMetadata(mHandle)
      End
    End
  End

  Return nModels


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetMetadataCount    Procedure(FREE_IMAGE_MDMODEL MetadataModel)!,UNSIGNED
!--------------------------------------------------------------------------
  Code
  If Self.pImage
    Return FreeImage_GetMetadataCount(MetadataModel, Self.pImage)
  Else
    Return 0
  End


!--------------------------------------------------------------------------
!
! Note: Metadata key names are case sensitive.
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetMetadata    Procedure(FREE_IMAGE_MDMODEL MetadataModel, String sKey)!,String
!--------------------------------------------------------------------------
szTagValue      Cstring(128),Auto
szTag           &Cstring
szKey           Cstring(128),Auto
lpTag           UNSIGNED
tag             &FITAG
  Code
  szTagValue = ''
  If Self.pImage      
    szKey = Clip(sKey)
    If FreeImage_GetMetadata(MetadataModel, Self.pImage, szKey, lpTag) And lpTag <> 0
      tag &= (lpTag)
      szTag &= FreeImage_TagToString(metadataModel, tag)
      szTagValue = szTag
    Else
      szTagValue = ''
    End
  End
  Return szTagValue


!--------------------------------------------------------------------------
!
! Note:  To make the change permanent, you must save the image.
!        Comments are allowed in JPEG, PNG, and GIF
!
!  Currently only tested on JPEG images.
!--------------------------------------------------------------------------
FreeImageClass.iImage.SetMetadataComments Procedure(String sComments)!,BOOL
!--------------------------------------------------------------------------
bResult     BOOL,Auto
tag         &FITAG
szKey       Cstring('Comment')
szComments  &Cstring
cbComments  UNSIGNED,Auto
lpTag       UNSIGNED
  Code

  !Allocate the memory for a cstring to hold the comments
  cbComments = Len(Clip(sComments)) +1
  szComments &= New Cstring(cbComments)
  If szComments &= Null
    Self.iImage.TakeError(OUT_OF_MEMORY,'Unable to allocate memory for comments.')
    Return False
  End
  szComments = Clip(sComments)

  !Now check if the Comment tag exists in this image
  ! and if it does change the existing comment
  If FreeImage_GetMetadata(FIMD_COMMENTS, Self.pImage, szKey, lpTag) And lpTag <> 0
    tag &= (lpTag)
    Self.iImage.TakeError(FreeImage_SetTagLength(tag, cbComments) -1)
    Self.iImage.TakeError(FreeImage_SetTagCount(tag, cbComments) -1)
    Self.iImage.TakeError(FreeImage_SetTagType(tag, FIDT_ASCII) -1)
    Self.iImage.TakeError(FreeImage_SetTagValue(tag, szComments) -1)

  ! Otherwise create a new comment tag for the image
  Else
    tag &= FreeImage_CreateTag()
    If Not tag &= Null
      Self.iImage.TakeError(FreeImage_SetTagKey(tag, szKey) -1)
      Self.iImage.TakeError(FreeImage_SetTagLength(tag, cbComments) -1)
      Self.iImage.TakeError(FreeImage_SetTagCount(tag, cbComments) -1)
      Self.iImage.TakeError(FreeImage_SetTagType(tag, FIDT_ASCII) -1)
      Self.iImage.TakeError(FreeImage_SetTagValue(tag, szComments) -1)

      Self.iImage.TakeError(FreeImage_SetMetadata(FIMD_COMMENTS, Self.pImage, FreeImage_GetTagKey(tag), tag)-1)

      FreeImage_DeleteTag(tag)

    End
  End
  Dispose(szComments)
  Return bResult
  
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.CaptureScreen       Procedure(Window W)
  Code  
  Self.CaptureScreen(W{Prop:Handle})
  Return
  
  
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.CaptureScreen       Procedure(UNSIGNED hwnd)
rcCap                                Like(SA_RECT)   
  Code
  !Get the coordinates of the object to capture  
  SA_GetWindowRect(hWnd, rcCap)
  SA_MapWindowPoints(0, hwnd, rcCap, 2)
  Self.CaptureScreen(hwnd, rcCap)
  Return
  
  
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.CaptureScreen       Procedure(UNSIGNED hwnd, *SA_RECT rcCap)
hDC         SA_HDC,Auto
hDCMem      SA_HDC 
hBm                                   Long                              
hBmOld     Long

bmih        &SA_BITMAPINFOHEADER
nWidth                                Long
nHeight                               Long

  Code 
    
  nWidth = rcCap.Right-rcCap.Left    
  nHeight = rcCap.Bottom-rcCap.Top
  
  !Create the compatible DC and bitmap for the captured screen
  hDC = SA_GetDC(hwnd)
  hDCmem = SA_CreateCompatibleDC(hDC)
  If hDC = 0 or hDCmem = 0   
    Do CleanUp
    Return
  End
  
  hbm = SA_CreateCompatibleBitmap(hDC, nWidth, nHeight)
  If hBm 
    !Select the bitmap into the device contexts
    hBmOld = SA_SelectObject(hdcMem, hBm)
    !Make the copy
    SA_BitBlt(hDCmem, |
              0,   |
              0,    |
              nWidth,  |
              nHeight, |
              hDC, |
              rcCap.Left, |
              rcCap.Top, |
              SA_SRCCOPY)     
      !Create a new image, using a 24 bit BMP          
      If Self.iImage.NewImage(nWidth, nHeight, 24, FIF_BMP)
       !Copy the screen capture into this new image
       SA_GetDIBits(hDCMem, hBm, 0, Self.imageSize.height, Self.iImage.GetImageBits(), Self.iImage.GetBMInfo(), SA_DIB_RGB_COLORS)
     End
     !Release the memory for the bitmap 
     SA_DeleteObject(SA_SelectObject(hdcMem, hBmOld))
  End   
  Do CleanUp
  Return

CleanUp  Routine
  !Release the DC and memory for the memory DC  
  If hDC <> 0 
    SA_ReleaseDC(hwnd, hDC)
  End
  If hDCmem <> 0
    SA_DeleteDC(hDCMem)
  End
   
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.CaptureScreen       Procedure(Window W)
!--------------------------------------------------------------------------
  Code
  Self.CaptureScreen(W{Prop:Handle})
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.CaptureScreen       Procedure(UNSIGNED hwnd) 
!--------------------------------------------------------------------------
  Code  
  Self.CaptureScreen(hwnd)
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.CaptureScreen       Procedure(UNSIGNED hwnd, *SA_RECT rcCap)
!--------------------------------------------------------------------------
  Code
  Self.CaptureScreen(hwnd, rcCap)
  Return
  
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.TakeError    Procedure(UNSIGNED ErrorCde, <String sErrorMsg>)!,UNSIGNED
!--------------------------------------------------------------------------
szErr Cstring(256)
  Code

  Self.LastError = ErrorCde

  If Self.bDebug And ErrorCde <> 0
    If Not Omitted(3)
      szErr = sErrorMsg
    
    Else
      Case ErrorCde
      Of UNKNOWN_IMAGE_FORMAT
        szErr = 'Unable to determine image file format.'
      Of CANCELED
        szErr = 'Operation was canceled.'
      Of OUT_OF_MEMORY
        szErr = 'Unable to allocate enough memory.'
      End
    End

    If szErr
      Message(szErr&'|Errorcode: '&ErrorCde, 'Clarion FreeImage', Icon:Exclamation)
    Else
      Message('Errorcode: '&ErrorCde, 'Clarion FreeImage', Icon:Exclamation)
    End
  End
  Return ErrorCde


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.iImage.GetLastError        Procedure()!,Long
!--------------------------------------------------------------------------
  Code
  Return Self.LastError


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.Construct    Procedure()
!--------------------------------------------------------------------------
  Code
  Self.bDebug = DEBUGSTATE

  Self.pImage           = 0
  Self.LoadOption       = 0
  Self.pPalette         = 0
  Self.bAutoColorDepth  = True
  Self.bRetainImage     = False

  !-----------------------------------------------------------
  ! Set the error handler callback procedure
  !-----------------------------------------------------------
  If Self.bDebug
    FreeImage_SetOutputMessage(Address(errorproc))
  End

  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FreeImageClass.Destruct    Procedure()
!--------------------------------------------------------------------------
nCountLockedPages Long
  Code
  Dispose(Self.szImageFileName)
  If Self.pImage And Not Self.bRetainImage
    FreeImage_Unload(Self.pImage)
  End

  Return
