!ABCIncludeFile(FREEIMAGE)
!============================================================
! freeimgcl.inc -- Clarion FreeImage Class header file
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
! Added new composite methods:
!Composite           Procedure(UNSIGNED bkgColor=0),BOOL,Proc
!Composite           Procedure(*iImage bkgImage),BOOL,Proc
!Composite           Procedure(*iImage DstImage, *iImage bkgImage),BOOL,Proc
! 
! 3.15.4
! Removed   Protected attribute from pImage, GetDialogFilter 4
! Added ResizeCanvas method to resize the image filling the background 
!       with the specified color.  This is used to add a border to an image.
! Added lossles JPEG opertations for crop and transforms
!============================================================

  OMIT('_EndOfFreeImageClass_',_FreeImageClassIncluded_)
_FreeImageClassIncluded_  EQUATE(1)
  Include('FreeImg.inc','Defines'),Once
  Include('SaFiDiCl.inc'),Once


DEBUGSTATE      EQUATE(False)

ClassErrors             Itemize(10000)
OUT_OF_MEMORY               Equate
UNKNOWN_IMAGE_FORMAT        Equate
CANCELED                    Equate
                        End

!FitTo method styles
         Itemize(0)
CFIFIT_NONE     Equate
CFIFIT_WIDTH    Equate
CFIFIT_HEIGHT   Equate
CFIFIT_BOTH     Equate    
CFIFIT_BEST     Equate
         End
              
         Itemize(0),Pre()
CFI_SQUARE      Equate
CFI_WIDTH       Equate
CFI_HEIGHT      Equate
         End              

MetaDataQueue   Queue,Type
mdModel           Unsigned
mdKey             Cstring(128)
mdDescription     Cstring(128)
mdValue           Cstring(128)
                End

ImageSizeGroup  Group,Type
width             Long
height            Long
                End

iImage  Interface,Type
Load                Procedure(String sImageFilename),BOOL
Load                Procedure(*Cstring szImageFilename),BOOL
Load                Procedure(*BLOB ImageBlob),BOOL
Load                Procedure(UNSIGNED hMem, Long cbImage),BOOL
LoadResource        Procedure(String sImageName),BOOL

OnLoad              Procedure(UNSIGNED fif)
OnLoadFailure       Procedure()

SetLoadOption       Procedure(Long LoadOption)
GetLoadOption       Procedure(),Long

SaveAs              Procedure(String sImageFilename, UNSIGNED fif=FIF_UNKNOWN),BOOL,Proc
Save                Procedure(*Cstring szImageFilename, UNSIGNED fif=FIF_UNKNOWN),BOOL,Proc
Save                Procedure(),BOOL,Proc
Save                Procedure(*Blob theBlob, UNSIGNED fif=FIF_UNKNOWN),BOOL
OnSave              Procedure(UNSIGNED fif)
OnSaveFailure       Procedure()
CopyToImageControl  Procedure(Window theWindow, Long feqImageControl)

NewImage            Procedure(Long nWidth, Long nHeight, Long nBPP, UNSIGNED fif=FIF_UNKNOWN),BOOL
Blank               Procedure()

SetSaveOption       Procedure(Long SaveOption),Virtual !,EXTENDS
GetSaveOption       Procedure(),Long

SetFileName         Procedure(String sImageFilename),BOOL,Proc
SetFileName         Procedure(*Cstring szImageFilename),BOOL,Proc

GetFileName         Procedure(),String
GetFileName         Procedure(*CString szImageFilename)

Brightness          Procedure(Real percentChange),BOOL,Proc
Contrast            Procedure(Real percentChange),BOOL,Proc
Gamma               Procedure(Real gammaChange),BOOL,Proc
Invert              Procedure(),BOOL,Proc

SetAutoColorDepth   Procedure(BOOL bAutoColor=True)
GetAutoColorDepth   Procedure(),BOOL

ConvertColorDepth   Procedure(UNSIGNED cvtToColorDepth, Long dither=FID_NONE, UNSIGNED oneBitThreshold=128 ),BOOL,Proc
ConvertColorDepth   Procedure(*iImage dstImage, UNSIGNED cvtColorDepth, Long dither=FID_NONE, UNSIGNED oneBitThreshold=128),BOOL,Proc !New Image is returned in dstImage

ConvertToGrayscale  Procedure(),BOOL,Proc
ConvertToGrayscale  Procedure(*iImage dstImage),BOOL,Proc                                            !New Image is returned in dstImage

Crop                Procedure(Long X, Long Y, Long nWidth, Long nHeight),BOOL,Proc
Crop                Procedure(*SA_RECT rcSrc),BOOL,Proc    

JpegCrop            Procedure(String srcFile, String dstFile, Long X, Long Y, Long nWidth, Long nHeight),BOOL,Proc
JpegCrop            Procedure(*Cstring srcFile, *Cstring dstFile, *SA_RECT rcSrc),BOOL,Proc

JpegTransform       Procedure(String srcFile, String dstFile, Long Operation, BOOL Perfect=0),BOOL,Proc

CopyImage           Procedure(*iImage dstImage, Long X, Long Y, Long nWidth, Long nHeight),BOOL,Proc  !New Image is returned in dstImage
CopyImage           Procedure(*iImage dstImage, *SA_RECT rcSrc),BOOL,Proc                             !New Image is returned in dstImage
CopyImage           Procedure(*iImage dstImage),BOOL,Proc                                             !New Image is returned in dstImage

AlphaBlend          Procedure(*iImage srcImage, Long X, Long Y, Long nAlphaBlend),BOOL,Proc           !Source image must be smaller or same size as managed imge
Composite           Procedure(UNSIGNED bkgColor=0),BOOL,Proc
Composite           Procedure(*iImage bkgImage),BOOL,Proc
Composite           Procedure(*iImage DstImage, *iImage bkgImage),BOOL,Proc

Rotate              Procedure(Real fAngle),BOOL,Proc
Rotate              Procedure(Real fAngle, Real xShift, Real yShift, Real xOrigin, Real yOrigin, BOOL bUseMask),BOOL,Proc

Flip                Procedure(UNSIGNED HorzOrVert=FI_FLIPHORIZ),BOOL,Proc

Rescale             Procedure(*Real fPercentX, *Real fPercentY, UNSIGNED fiFilter),BOOL,Proc
Rescale             Procedure(*Real fPercent, UNSIGNED fiFilter),BOOL,Proc
Rescale             Procedure(UNSIGNED nDstWidth, UNSIGNED nDstHeight, UNSIGNED fiFilter),BOOL,Proc
Rescale             Procedure(*iImage dstImage, Real fPercent, UNSIGNED fiFilter),BOOL,Proc                            !New Image is returned in dstImage
Rescale             Procedure(*iImage dstImage, UNSIGNED nDstWidth, UNSIGNED nDstHeight, UNSIGNED fiFilter),BOOL,Proc  !New Image is returned in dstImage

FitTo               Procedure(UNSIGNED nDstWidth, UNSIGNED nDstHeight, UNSIGNED fiFilter=FILTER_BILINEAR, UNSIGNED FitMethod=CFIFIT_BOTH, UNSIGNED limitLongSideTo=0, BOOL maintainAspectRatio=True, BOOL adjustPower2=False),BOOL,Proc
GetAspectRatio      Procedure(),Real    
limitLongSide       Procedure(Long longSide, UNSIGNED limitLongSideTo, *UNSIGNED scaledWidth, *UNSIGNED scaledHeight)

Thumbnail           Procedure(*iImage dstImage, UNSIGNED nDstWidth, UNSIGNED fiFilter),BOOL,Proc                       !New Image is returned in dstImage
Thumbnail           Procedure(UNSIGNED nDstWidth, UNSIGNED fiFilter),BOOL,Proc

Thumbnail           Procedure(*iImage dstImage, UNSIGNED nMaxPixelSize),BOOL,Proc                                      !New Image is returned in dstImage
Thumbnail           Procedure(UNSIGNED nMaxPixelSize),BOOL,Proc
 
ResizeCanvas        Procedure(Long leftPx, Long topPx, Long rightPx, Long bottomPx, Long pColor),BOOL,Proc   
ResizeCanvas        Procedure(*iImage dstIimage, Long leftPx, Long topPx, Long rightPx, Long bottomPx, *SA_RGBQUAD pColor, Long options=0),BOOL !New Image is returned in dstImage  
                                                  
                    !Returns the channel in passed iImage interface
GetChannel          Procedure(*iImage dstIimage, UNSIGNED ficc=FICC_RED),BOOL,Proc
SetChannel          Procedure(*iImage Bit8Image, UNSIGNED ficc=FICC_RED),BOOL,Proc

GetPixel            Procedure(UNSIGNED x, UNSIGNED y),Long
SetPixel            Procedure(UNSIGNED x, UNSIGNED y, Long BGRAColor),BOOL

GetPalette          Procedure()
GetPaletteEntry     Procedure(Long nPaletteIndex),Long
FindNearestColorIndex Procedure(Long TheColor),Long

GetWidth            Procedure(),UNSIGNED
GetHeight           Procedure(),UNSIGNED
GetSize             Procedure(*ImageSizeGroup imageSize)
GetLongSide         Procedure(),Long

GetBMInfoHeader     Procedure(),UNSIGNED    !Returns address of BITMAPINFOHEADER
GetBMInfo           Procedure(),UNSIGNED    !Returns address of BITMAPINFO
GetImageBits        Procedure(),UNSIGNED    !Returns address of the image data bits

GetBpp              Procedure(),Long
GetColorType        Procedure(),UNSIGNED
GetPitch            Procedure(),UNSIGNED
GetDIBSize          Procedure(),UNSIGNED

GetDotsPerMeterX    Procedure(),UNSIGNED
SetDotsPerMeterX    Procedure(UNSIGNED resolution)
GetDotsPerMeterY    Procedure(),UNSIGNED
SetDotsPerMeterY    Procedure(UNSIGNED resolution)
GetRedMask          Procedure(),UNSIGNED
GetGreenMask        Procedure(),UNSIGNED
GetBlueMask         Procedure(),UNSIGNED
GetTransparencyCount Procedure(),UNSIGNED
GetTransparencyTable Procedure(),UNSIGNED   !Returns address of a byte array
SetTransparencyTable Procedure(UNSIGNED pTranTable, Long cbTranTable)
SetTransparent      Procedure(BOOL bTransparent=True)
IsTransparent       Procedure(),BOOL
HasBackgroundColor  Procedure(),BOOL
GetBackgroundColor  Procedure(*SA_RGBQUAD rgbColor),BOOL !color returned in rgbColor arg
SetBackgroundColor  Procedure(*SA_RGBQUAD rgbColor),BOOL

FillBackground      Procedure(*SA_RGBQUAD backroundColor, Long Options=0),BOOL,Proc

GetBits             Procedure(),UNSIGNED !Returns pointer to image bits 16 byte aligned
GetScanLine         Procedure(Long nScanLine),UNSIGNED !Returns pointer to begining of scanline

GetICCProfile       Procedure(),UNSIGNED
CreateICCProfile    Procedure(),UNSIGNED
DestroyICCProfile   Procedure()

GetBitmap           Procedure(),UNSIGNED  !Returns pointer to FIBITMAP for use with FreeImage API
ReplaceImage        Procedure(*UNSIGNED pNewImage),BOOL

!------- Metadata methods
EnumMetadata        Procedure(MetaDataQueue mdQ),UNSIGNED
GetMetadataCount    Procedure(UNSIGNED MetadataModel),UNSIGNED
GetMetadata         Procedure(UNSIGNED MetadataModel, String sKey),String
SetMetadataComments Procedure(String sComments),BOOL

!--------
TakeError           Procedure(UNSIGNED Errorcde, <String sErrorMsg> ),UNSIGNED,Proc
GetLastError        Procedure(),Long

!------- Utility Methods ---
BGRtoRGB            Procedure(*SA_RGBQUAD RGB, *SA_BGRQUAD BGR)
RGBtoBGR            Procedure(*SA_BGRQUAD BGR, *SA_RGBQUAD RGB)

!------- 
GetCopyright        Procedure(),String
GetVersion          Procedure(),String       

!------- 
_PowerOfTwo_Nearest Procedure(ULONG xValue),ULONG
_PowerOfTwo_Floor   Procedure(ULONG xValue),ULONG          

!------- 
CaptureScreen       Procedure(Window W)
CaptureScreen       Procedure(UNSIGNED hwnd) 
CaptureScreen       Procedure(UNSIGNED hwnd, *SA_RECT rcCap)
         End

!------------------------------------------------------------------
!------------------------------------------------------------------
FreeImageClass  Class,Implements(iImage),Type,Thread,Module('FreeImCl.clw'),Link('FreeImCl.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)
!------------------------------------------------------------------

szImageFileName     &Cstring,Protected          !This image's file name
fif                 FREE_IMAGE_FORMAT,Protected !Format of the managed image
pImage              LPFIBITMAP                  !Pointer to DIB of image

bRetainImage        BOOL,Protected              !Keep image in memory when object is destroyed
LoadOption          Long,Protected              !See FreeImage docs
szOpenDialogTitle   Cstring(64),Protected
SaveOption          Long,Protected              !See FreeImage docs
szSaveDialogTitle   Cstring(64),Protected
LastError           Long,Protected
bDebug              BOOL,Protected

bAutoColorDepth     BOOL,Protected              !Automatically convert to 24 bit when required
pPalette            UNSIGNED,Protected          !Pointer to palette     

imageSize           Like(ImageSizeGroup)


!------------------------------------------------------------------
GetFileDialogFilter Procedure(BOOL bReadonly=False),String,Virtual
GetFileDialogfilter Procedure(*Cstring szFilter, BOOL bReadonly=False, BOOL bNullSep=False),Virtual !,FINAL
OnLoad              Procedure(UNSIGNED fif),Virtual
OnLoadFailure       Procedure(),Virtual
OnSave              Procedure(UNSIGNED fif),Virtual
OnSaveFailure       Procedure(),Virtual  
CaptureScreen       Procedure(Window W)
CaptureScreen       Procedure(UNSIGNED hwnd) 
CaptureScreen       Procedure(UNSIGNED hwnd, *SA_RECT rcCap)
!------------------------------------------------------------------
Construct           Procedure()
Destruct            Procedure()
                End

_EndOfFreeImageClass_

