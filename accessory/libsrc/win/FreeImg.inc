!============================================================
! FreeImg.inc -- Clarion FreeImage header file
!
!  This Clarion implementation is subject to the following:
!
!  Copyright © 2004 - 2005 Sand & Associates, Larry@sand-associates.com
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
!  4. You comply with all conditions set forth in the FreeImage public license.
!     See freeimage.sourceforge.net and or freeimage-license.txt file for the
!     contents of the license.
!
!============================================================
! ==========================================================
! FreeImage 3
!
! Design and implementation by
! - Floris van den Berg (flvdberg@wxs.nl)
! - Hervé Drolon (drolon@infonie.fr)
!
! Contributors:
! - Adam Gates (radad@xoasis.com)
! - Alex Kwak
! - Alexander Dymerets (sashad@te.net.ua)
! - Detlev Vendt (detlev.vendt@brillit.de)
! - Jan L. Nauta (jln@magentammt.com)
! - Jani Kajala (janik@remedy.fi)
! - Juergen Riecker (j.riecker@gmx.de)
! - Karl-Heinz Bussian (khbussian@moss.de)
! - Laurent Rocher (rocherl@club-internet.fr)
! - Luca Piergentili (l.pierge@terra.es)
! - Machiel ten Brinke (brinkem@uni-one.nl)
! - Markus Loibl (markus.loibl@epost.de)
! - Martin Weber (martweb@gmx.net)
! - Matthias Wandel (mwandel@rim.net)
! - Michal Novotny (michal@etc.cz)
! - Petr Pytelka (pyta@lightcomp.com)
! - Riley McNiff (rmcniff@marexgroup.com)
! - Ryan Rubley (ryan@lostreality.org)
! - Volker Gärtner (volkerg@gmx.at)
!
! This file is part of FreeImage 3
!
! COVERED CODE IS PROVIDED UNDER THIS LICENSE ON AN "AS IS" BASIS, WITHOUT WARRANTY
! OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTIES
! THAT THE COVERED CODE IS FREE OF DEFECTS, MERCHANTABLE, FIT FOR A PARTICULAR PURPOSE
! OR NON-INFRINGING. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE COVERED
! CODE IS WITH YOU. SHOULD ANY COVERED CODE PROVE DEFECTIVE IN ANY RESPECT, YOU (NOT
! THE INITIAL DEVELOPER OR ANY OTHER CONTRIBUTOR) ASSUME THE COST OF ANY NECESSARY
! SERVICING, REPAIR OR CORRECTION. THIS DISCLAIMER OF WARRANTY CONSTITUTES AN ESSENTIAL
! PART OF THIS LICENSE. NO USE OF ANY COVERED CODE IS AUTHORIZED HEREUNDER EXCEPT UNDER
! THIS DISCLAIMER.
!
! Use at your own risk!
! ==========================================================

!============================================================         
! Updated 2013.07.06  Added FreeImage Version 3.15.4 prototypes and constants
! Updated 2009.04.15: Added FreeImage Version 3.12.0 prototypes and constants
! Updated 2008.07.30: Added FreeImage Version 3.11.0 prototypes and constants
! Updated 2007.11.19: Added FreeImage version 3.10.0 prototypes and constants
! Updated 2007.04.13: Corrected FreeImage_ConvertToRawBits (thanks Sean Cameron),
!                     FreeImage_JPEGCrop, and FreeImage_LoadMultiBitmapFromMemory prototypes
! Updated 2007.03.14: Added FreeImage version 3.9.3 constant changes
! Updated 2006.11.30: Added FreeImage version 3.9.2 prototypes
! Updated 2006.07.06: Added FreeImage version 3.9.0 prototpyes
!                     and defines.
! Changed 2006.01.30: Prototype of functions returning *Cstring
!                     changed to return UNSIGNED, SetMetatdata
!                     parameter changed from *Cstring to Unsigned
!                     for use with new retun values.  These changes
!                     were made to allow the project to compile in C5
! Updated 2005-09-12: Added FreeImage version 3.8.0 prototypes
! Updated 2005-06-10: Added FreeImage version 3.7.0 prototypes
!============================================================

SECTION('Defines')

   Include('SaWapi.inc', 'Defines'),Once


FREEIMAGE_COLORORDER_RGB    equate(0)
FREEIMAGE_COLORORDER_BGR    equate(1)
FREEIMAGE_COLORORDER        equate(FREEIMAGE_COLORORDER_BGR)


!// Types used in the library (specific to FreeImage) ------------------------


!/** 48-bit RGB

FIRGB16 Group,Type
red       Ushort
green     Ushort
blue      Ushort
        End

!/** 64-bit RGBA
FIRGBA16 Group,Type
red        Ushort
green      Ushort
blue       Ushort
alpha      Ushort
         End

!/** 96-bit RGB Float
FIRGBF Group,Type
red      Sreal
green    Sreal
blue     Sreal
       End

!/** 128-bit RGBA Float

FIRGBAF Group,Type
red       Sreal
green     Sreal
blue      Sreal
alpha     Sreal
       End

!/** Data structure for COMPLEX type (complex number)

FICOMPLEX Group,Type
r           Real ! real part
i           Real ! imaginary part
       End

 !for FREEIMAGE_COLORORDER_BGR
FI_RGBA_RED             Equate(2)
FI_RGBA_GREEN           Equate(1)
FI_RGBA_BLUE            Equate(0)
FI_RGBA_ALPHA           Equate(3)
FI_RGBA_RED_MASK        Equate(000FF0000h)
FI_RGBA_GREEN_MASK      Equate(00000FF00h)
FI_RGBA_BLUE_MASK       Equate(0000000FFh)
FI_RGBA_ALPHA_MASK      Equate(0FF000000h)
FI_RGBA_RED_SHIFT       Equate(16)
FI_RGBA_GREEN_SHIFT     Equate(8)
FI_RGBA_BLUE_SHIFT      Equate(0)
FI_RGBA_ALPHA_SHIFT     Equate(24)

FI_RGBA_RGB_MASK        Equate(FI_RGBA_RED_MASK+FI_RGBA_GREEN_MASK+FI_RGBA_BLUE_MASK)

! The 16bit macros only include masks and shifts, since each color element is not byte aligned

FI16_555_RED_MASK       Equate(7C00h)
FI16_555_GREEN_MASK     Equate(03E0h)
FI16_555_BLUE_MASK      Equate(001Fh)
FI16_555_RED_SHIFT      Equate(10)
FI16_555_GREEN_SHIFT    Equate(5)
FI16_555_BLUE_SHIFT     Equate(0)
FI16_565_RED_MASK       Equate(0F800h)
FI16_565_GREEN_MASK     Equate(07E0h)
FI16_565_BLUE_MASK      Equate(001Fh)
FI16_565_RED_SHIFT      Equate(11)
FI16_565_GREEN_SHIFT    Equate(5)
FI16_565_BLUE_SHIFT     Equate(0)

! ICC profile support ------------------------------------------------------

FIICC_DEFAULT           Equate(00h)
FIICC_COLOR_IS_CMYK     Equate(01h)

FIICCPROFILE Group,Type
flags  USHORT       ! info flag
size   UNSIGNED     ! profile's size measured in bytes
lpdata UNSIGNED     ! points to a block of contiguous memory containing the profile
             End

! Important enums ----------------------------------------------------------
!
!/** I/O image format identifiers.
!*/
FREE_IMAGE_FORMAT Equate(Long)
FIF_UNKNOWN Equate(-1)
FIF_BMP    Equate(0)
FIF_ICO    Equate(1)
FIF_JPEG   Equate(2)
FIF_JNG    Equate(3)
FIF_KOALA  Equate(4)
FIF_LBM     Equate(5)
FIF_IFF     Equate(FIF_LBM)
FIF_MNG     Equate(6)
FIF_PBM     Equate(7)
FIF_PBMRAW  Equate(8)
FIF_PCD     Equate(9)
FIF_PCX     Equate(10)
FIF_PGM     Equate(11)
FIF_PGMRAW  Equate(12)
FIF_PNG     Equate(13)
FIF_PPM     Equate(14)
FIF_PPMRAW  Equate(15)
FIF_RAS     Equate(16)
FIF_TARGA   Equate(17)
FIF_TIFF    Equate(18)
FIF_WBMP    Equate(19)
FIF_PSD     Equate(20)
FIF_CUT     Equate(21)
FIF_XBM     Equate(22)
FIF_XPM     Equate(23)
FIF_DDS     Equate(24)
FIF_GIF     Equate(25)
FIF_HDR     Equate(26)
FIF_FAXG3   Equate(27)  !Version 3.9.0
FIF_SGI     Equate(28)  !Version 3.9.0

FIF_EXR     Equate(29)  !Version 3.10.0
FIF_J2K     Equate(30)  !Version 3.10.0
FIF_JP2     Equate(31)  !Version 3.10.0
FIF_PFM     Equate(32)  !Version 3.12.0

FIF_PICT    Equate(33)  !Version 3.15.4
FIF_RAW     Equate(34)  !Version 3.15.4

!
!/** Image type used in FreeImage.
!*/
FREE_IMAGE_TYPE Equate(UNSIGNED)
FIT_UNKNOWN Equate(0)    ! unknown type
FIT_BITMAP  Equate(1)    ! standard image           : 1-, 4-, 8-, 16-, 24-, 32-bit
FIT_UINT16  Equate(2)    ! array of unsigned short  : unsigned 16-bit
FIT_INT16   Equate(3)    ! array of short           : signed 16-bit
FIT_UINT32  Equate(4)    ! array of unsigned long   : unsigned 32-bit
FIT_INT32   Equate(5)    ! array of long            : signed 32-bit
FIT_FLOAT   Equate(6)    ! array of float           : 32-bit IEEE floating point
FIT_DOUBLE  Equate(7)    ! array of double          : 64-bit IEEE floating point
FIT_COMPLEX Equate(8)    ! array of FICOMPLEX       : 2 x 64-bit IEEE floating point
FIT_RGB16   Equate(9)    ! 48-bit RGB image         : 3 x 16-bit
FIT_RGBA16  Equate(10)   ! 64-bit RGBA image        : 4 x 16-bit
FIT_RGBF    Equate(11)   ! 96-bit RGB float image   : 3 x 32-bit IEEE floating point
FIT_RGBAF   Equate(12)   ! 128-bit RGBA float image : 4 x 32-bit IEEE floating point


!See FICOMPLEX
!!/** Data structure for COMPLEX type (complex number)
!!*/
!FreeImageComplex Group,Type
!r                   Real    !/ real part
!i                   Real    !/ imaginary part
!                 End


!/** Image color type used in FreeImage.
!*/
FREE_IMAGE_COLOR_TYPE  Equate(UNSIGNED)
FIC_MINISWHITE Equate(0)     ! min value is white
FIC_MINISBLACK Equate(1)     ! min value is black
FIC_RGB        Equate(2)     ! RGB color model
FIC_PALETTE    Equate(3)     ! color map indexed
FIC_RGBALPHA   Equate(4)     ! RGB color model with alpha channel
FIC_CMYK       Equate(5)      ! CMYK color model


!/** Color quantization algorithms.
!Constants used in FreeImage_ColorQuantize.
!*/
FREE_IMAGE_QUANTIZE  Equate(UNSIGNED)
FIQ_WUQUANT Equate(0)        ! Xiaolin Wu color quantization algorithm
FIQ_NNQUANT Equate(1)        ! NeuQuant neural-net quantization algorithm by Anthony Dekker


!/** Dithering algorithms.
!Constants used FreeImage_Dither.
!*/
FREE_IMAGE_DITHER  Equate(Long)
FID_NONE        EQUATE(-1)   !LLS
FID_FS          Equate(0)    ! Floyd & Steinberg error diffusion
FID_BAYER4x4    Equate(1)    ! Bayer ordered dispersed dot dithering (order 2 dithering matrix)
FID_BAYER8x8    Equate(2)    ! Bayer ordered dispersed dot dithering (order 3 dithering matrix)
FID_CLUSTER6x6  Equate(3)    ! Ordered clustered dot dithering (order 3 - 6x6 matrix)
FID_CLUSTER8x8  Equate(4)    ! Ordered clustered dot dithering (order 4 - 8x8 matrix)
FID_CLUSTER16x16 Equate(5)   ! Ordered clustered dot dithering (order 8 - 16x16 matrix)
FID_BAYER16x16   Equate(6)   ! Bayer ordered dispersed dot dithering (order 4 dithering matrix) (ver 3.9.3)

!/** Lossless JPEG transformations
!Constants used in FreeImage_JPEGTransform
!*/
FREE_IMAGE_JPEG_OPERATION Equate(UNSIGNED)
FIJPEG_OP_NONE          Equate(0)    ! no transformation
FIJPEG_OP_FLIP_H        Equate(1)    ! horizontal flip
FIJPEG_OP_FLIP_V        Equate(2)    ! vertical flip
FIJPEG_OP_TRANSPOSE     Equate(3)    ! transpose across UL-to-LR axis
FIJPEG_OP_TRANSVERSE    Equate(4)    ! transpose across UR-to-LL axis
FIJPEG_OP_ROTATE_90     Equate(5)    ! 90-degree clockwise rotation
FIJPEG_OP_ROTATE_180    Equate(6)    ! 180-degree rotation
FIJPEG_OP_ROTATE_270    Equate(7)    ! 270-degree clockwise (or 90 ccw)


!/** Tone mapping operators.
!Constants used in FreeImage_ToneMapping.
!*/
FREE_IMAGE_TMO Equate(UNSIGNED)
FITMO_DRAGO03     Equate(0)    ! Adaptive logarithmic mapping (F. Drago, 2003)
FITMO_REINHARD05  Equate(1)    ! Dynamic range reduction inspired by photoreceptor physiology (E. Reinhard, 2005)
FITMO_FATTAL02    Equate(2)    !Version 3.10.0  Gradient domain high dynamic range compression (R. Fattal, 2002)

!/** Upsampling / downsampling filters.
!Constants used in FreeImage_Rescale.
!*/
FREE_IMAGE_FILTER  Equate(UNSIGNED)
FILTER_BOX        Equate(0)  ! Box, pulse, Fourier window, 1st order (constant) b-spline
FILTER_BICUBIC    Equate(1)  ! Mitchell & Netravali's two-param cubic filter
FILTER_BILINEAR   Equate(2)  ! Bilinear filter
FILTER_BSPLINE    Equate(3)  ! 4th order (cubic) b-spline
FILTER_CATMULLROM Equate(4)  ! Catmull-Rom spline, Overhauser spline
FILTER_LANCZOS3   Equate(5)  ! Lanczos3 filter
FILTER_NONE       Equate(6)
FILTER_LAST       Equate(FILTER_NONE)

!/** Color channels.
!Constants used in color manipulation routines.
!*/
FREE_IMAGE_COLOR_CHANNEL  Equate(UNSIGNED)
FICC_RGB    Equate(0)    ! Use red green and blue channels
FICC_RED    Equate(1)    ! Use red channel
FICC_GREEN  Equate(2)    ! Use green channel
FICC_BLUE   Equate(3)    ! Use blue channel
FICC_ALPHA  Equate(4)    ! Use alpha channel
FICC_BLACK  Equate(5)    ! Use black channel
FICC_REAL   Equate(6)    ! Complex images: use real part
FICC_IMAG   Equate(7)    ! Complex images: use imaginary part
FICC_MAG    Equate(8)    ! Complex images: use magnitude
FICC_PHASE  Equate(9)     ! Complex images: use phase


! Metadata support ---------------------------------------------------------

!/**
!  Tag data type information (based on TIFF specifications)
!
!  Note: RATIONALs are the ratio of two 32-bit integer values.
!*/
FREE_IMAGE_MDTYPE Equate(UNSIGNED)
FIDT_NOTYPE     Equate(0)    ! placeholder
FIDT_BYTE       Equate(1)    ! 8-bit unsigned integer
FIDT_ASCII      Equate(2)    ! 8-bit bytes w/ last byte null
FIDT_SHORT      Equate(3)    ! 16-bit unsigned integer
FIDT_LONG       Equate(4)    ! 32-bit unsigned integer
FIDT_RATIONAL   Equate(5)    ! 64-bit unsigned fraction
FIDT_SBYTE      Equate(6)    ! 8-bit signed integer
FIDT_UNDEFINED  Equate(7)    ! 8-bit untyped data
FIDT_SSHORT     Equate(8)    ! 16-bit signed integer
FIDT_SLONG      Equate(9)    ! 32-bit signed integer
FIDT_SRATIONAL  Equate(10)   ! 64-bit signed fraction
FIDT_FLOAT      Equate(11)   ! 32-bit IEEE floating point
FIDT_DOUBLE     Equate(12)   ! 64-bit IEEE floating point
FIDT_IFD        Equate(13)   ! 32-bit unsigned integer (offset)
FIDT_PALETTE    Equate(14)   ! 32-bit RGBQUAD

FIDT_LONG8      Equate(16)   ! 64-bit unsigned integer 
FIDT_SLONG8     Equate(17)   ! 64-bit signed integer
FIDT_IFD8       Equate(18)   ! 64-bit unsigned integer (offset)


!/**
!  Metadata models supported by FreeImage
!*/
FREE_IMAGE_MDMODEL Equate(UNSIGNED)
FIMD_NODATA         Equate(-1)
FIMD_COMMENTS       Equate(0)    ! single comment or keywords
FIMD_EXIF_MAIN      Equate(1)    ! Exif-TIFF metadata
FIMD_EXIF_EXIF      Equate(2)    ! Exif-specific metadata
FIMD_EXIF_GPS       Equate(3)    ! Exif GPS metadata
FIMD_EXIF_MAKERNOTE Equate(4)    ! Exif maker note metadata
FIMD_EXIF_INTEROP   Equate(5)    ! Exif interoperability metadata
FIMD_IPTC           Equate(6)    ! IPTC/NAA metadata
FIMD_XMP            Equate(7)    ! Abobe XMP metadata
FIMD_GEOTIFF        Equate(8)    ! GeoTIFF metadata
FIMD_ANIMATION      Equate(9)    ! Animation metadata
FIMD_CUSTOM         Equate(10)   ! Used to attach other metadata types to a dib       
FIMD_EXIF_RAW       Equate(11)   ! Exif metadata as a raw buffer
FIMD_LAST           Equate(12)   ! *SA* Added to allow enumeration of metadata models,

!--------------------------------- Load / Save Constants
           
FIF_LOAD_NOPIXELS   Equate(8000h)  ! Version 3.15.4 loading: load the image header only (not supported by all plugins, default to full loading)

BMP_DEFAULT         Equate(0)                     
BMP_SAVE_RLE        Equate(1)
CUT_DEFAULT         Equate(0)
DDS_DEFAULT         Equate(0)
EXR_DEFAULT         Equate(0)       ! Version 3.10.0 save data as half with piz-based wavelet compression
EXR_FLOAT           Equate(0001h)   ! Version 3.10.0 save data as float instead of as half (not recommended)
EXR_NONE            Equate(0002h)   ! Version 3.10.0 save with no compression
EXR_ZIP             Equate(0004h)   ! Version 3.10.0 save with zlib compression, in blocks of 16 scan lines
EXR_PIZ             Equate(0008h)   ! Version 3.10.0 save with piz-based wavelet compression
EXR_PXR24           Equate(0010h)   ! Version 3.10.0 save with lossy 24-bit float compression
EXR_B44             Equate(0020h)   ! Version 3.10.0 save with lossy 44% float compression - goes to 22% when combined with EXR_LC
EXR_LC              Equate(0040h)   ! Version 3.10.0 save images with one luminance and two chroma channels, rather than as RGB (lossy compression)
FAXG3_DEFAULT       Equate(0)       ! Version 3.9.0
GIF_DEFAULT         Equate(0)
GIF_LOAD256         Equate(1)       ! Load the image as a 256 color image with ununsed palette entries, if it's 16 or 2 color
GIF_PLAYBACK        Equate(2)       ! 'Play' the GIF to generate each frame (as 32bpp) instead of returning raw frame data when loading
HDR_DEFAULT         Equate(0)
ICO_DEFAULT         Equate(0)
ICO_MAKEALPHA       Equate(1)       ! convert to 32bpp and create an alpha channel from the AND-mask when loading
IFF_DEFAULT         Equate(0)
J2K_DEFAULT         Equate(0)       ! Version 3.10.0 save with a 16:1 rate
JP2_DEFAULT         Equate(0)       ! Version 3.10.0 save with a 16:1 rate
JPEG_DEFAULT        Equate(0)
JPEG_FAST           Equate(0001h)
JPEG_ACCURATE       Equate(0002h)
JPEG_CMYK           Equate(0004h)   ! load separated CMYK "as is" (use BOR to combine with other flags) !version 3.9.3
JPEG_EXIFROTATE     EQUATE(0008h)   ! load and rotate according to Exif 'Orientation' tag if available
JPEG_GREYSCALE      EQUATE(0010h)   ! load and convert to a 8-bit greyscale image


JPEG_QUALITYSUPERB  Equate(80h)
JPEG_QUALITYGOOD    Equate(100h)
JPEG_QUALITYNORMAL  Equate(200h)
JPEG_QUALITYAVERAGE Equate(400h)
JPEG_QUALITYBAD     Equate(800h)
JPEG_PROGRESSIVE    Equate(2000h)  ! save as a progressive-JPEG (use BOR to combine with other flags)   !Version 3.9.0

JPEG_SUBSAMPLING_411 Equate(01000h) !3.12.0    save with high 4x1 chroma subsampling (4:1:1)
JPEG_SUBSAMPLING_420 Equate(04000h) !3.12.0    save with medium 2x2 medium chroma subsampling (4:2:0) - default value
JPEG_SUBSAMPLING_422 Equate(08000h) !3.12.0    save with low 2x1 chroma subsampling (4:2:2)
JPEG_SUBSAMPLING_444 Equate(010000h) !3.12.0   save with no chroma subsampling (4:4:4)
JPEG_OPTIMIZE        Equate(20000h)  ! on saving, compute optimal Huffman coding tables (can reduce a few percent of file size)
JPEG_BASELINE        Equate(40000h)  ! save basic JPEG, without metadata or any markers

KOALA_DEFAULT       Equate(0)
LBM_DEFAULT         Equate(0)
MNG_DEFAULT         Equate(0)
PCD_DEFAULT         Equate(0)
PCD_BASE            Equate(1)       ! load the bitmap sized 768 x 512
PCD_BASEDIV4        Equate(2)       ! load the bitmap sized 384 x 256
PCD_BASEDIV16       Equate(3)       ! load the bitmap sized 192 x 128
PCX_DEFAULT         Equate(0)
PFM_DEFAULT         Equate(0)       !3.12.0
PICT_DEFAULT        EQUATE(0)       !PICT_DEFAULT 0


PNG_DEFAULT         Equate(0)
PNG_IGNOREGAMMA     Equate(1)       ! avoid gamma correction

PNG_Z_BEST_SPEED            Equate(00001h) !3.12.0  save using ZLib level 1 compression flag (default value is 6)
PNG_Z_DEFAULT_COMPRESSION   Equate(00006h) !3.12.0  save using ZLib level 6 compression flag (default recommended value)
PNG_Z_BEST_COMPRESSION      Equate(00009h) !3.12.0  save using ZLib level 9 compression flag (default value is 6)
PNG_Z_NO_COMPRESSION        Equate(00100h) !3.12.0  save without ZLib compression
PNG_INTERLACED              Equate(00200h) !3.12.0  save using Adam7 interlacing (use | to combine with other save flags)

PNM_DEFAULT         Equate(0)
PNM_SAVE_RAW        Equate(0)       ! If set the writer saves in RAW format (i.e. P4, P5 or P6)
PNM_SAVE_ASCII      Equate(1)       ! If set the writer saves in ASCII format (i.e. P1, P2 or P3)
PSD_DEFAULT         Equate(0)
PSD_CMYK            EQUATE(1)       ! reads tags for separated CMYK (default is conversion to RGB)
PSD_LAB             EQUATE(2)       ! reads tags for CIELab (default is conversion to RGB)
RAS_DEFAULT         EQUATE(0)       ! RAS_DEFAULT 0
RAW_DEFAULT         EQUATE(0)       ! load the file as linear RGB 48-bit
RAW_PREVIEW         EQUATE(1)       ! try to load the embedded JPEG preview with included Exif Data or default to RGB 24-bit
RAW_DISPLAY         EQUATE(2)       ! load the file as RGB 24-bit
RAW_HALFSIZE        EQUATE(4)       ! output a half-size color image

SGI_DEFAULT         Equate(0)       ! Version 3.9.0
TARGA_DEFAULT       Equate(0)
TARGA_LOAD_RGB888   Equate(1)       ! If set the loader converts RGB555 and ARGB8888 -> RGB888.
TARGA_SAVE_RLE      EQUATE(2)       ! If set, the writer saves with RLE compression
TIFF_DEFAULT        Equate(0)
TIFF_CMYK           Equate(0001)  ! reads/stores tags for separated CMYK (use | to combine with compression flags)
TIFF_PACKBITS       Equate(0100h)  ! save using PACKBITS compression
TIFF_DEFLATE        Equate(0200h)  ! save using DEFLATE compression (a.k.a. ZLIB compression)
TIFF_ADOBE_DEFLATE  Equate(0400h)  ! save using ADOBE DEFLATE compression
TIFF_NONE           Equate(0800h)  ! save without any compression
TIFF_CCITTFAX3      Equate(1000h)  ! save using CCITT Group 3 fax encoding
TIFF_CCITTFAX4      Equate(2000h)  ! save using CCITT Group 4 fax encoding
TIFF_LZW            Equate(4000h)  ! save using LZW compression
TIFF_JPEG           Equate(8000h)  ! save using JPEG compression
TIFF_LOGLUV         EQUATE(10000h) ! save using LogLuv compression
WBMP_DEFAULT        Equate(0)
XBM_DEFAULT         Equate(0)
XPM_DEFAULT         Equate(0)

! Background filling options ---------------------------------------------------------
! Constants used in FreeImage_FillBackground and FreeImage_EnlargeCanvas

FI_COLOR_IS_RGB_COLOR             EQUATE(00h)                         !FI_COLOR_IS_RGB_COLOR 0x00 // RGBQUAD color is a RGB color (contains no valid alpha channel)
FI_COLOR_IS_RGBA_COLOR            EQUATE(01h)                         !FI_COLOR_IS_RGBA_COLOR 0x01 // RGBQUAD color is a RGBA color (contains a valid alpha channel)
FI_COLOR_FIND_EQUAL_COLOR         EQUATE(02h)                         !FI_COLOR_FIND_EQUAL_COLOR 0x02 // For palettized images: lookup equal RGB color from palette
FI_COLOR_ALPHA_IS_INDEX           EQUATE(04h)                         !FI_COLOR_ALPHA_IS_INDEX 0x04 // The color's rgbReserved member (alpha) contains the palette index to be used
FI_COLOR_PALETTE_SEARCH_MASK      EQUATE(FI_COLOR_FIND_EQUAL_COLOR + FI_COLOR_ALPHA_IS_INDEX)  !FI_COLOR_PALETTE_SEARCH_MASK (FI_COLOR_FIND_EQUAL_COLOR | FI_COLOR_ALPHA_IS_INDEX) // No color lookup is performed

FIMETADATA  Group,Type
data            Long
            End

FITAG       Group,Type
key             Long
description     Long
id              Short
type            Short
count           Long
length          Long
Value           Long
            End

FreeImageIO Group,Type
read_proc       Long
write_proc      Long
seek_proc       Long
tell_proc       Long
            End 

FIMEMORY    Equate(UNSIGNED)

Plugin      Group,Type
format_proc         Long
description_proc    Long
extension_proc      Long
regexpr_proc        Long
open_proc           Long
close_proc          Long
pagecount_proc      Long
pagecapability_proc Long
load_proc           Long
save_proc           Long
validate_proc       Long
mime_proc           Long
supports_export_bpp_proc Long
supports_export_type_proc Long
supports_icc_profiles_proc Long
        End 

LPFIBITMAP      Equate(Long)
LPFIMEMORY      Equate(Long)
LPFIMULTIBITMAP Equate(Long)

HISTO   Group,Type
data      LONG,Dim(256)
        End

FI_FLIPHORIZ    EQUATE(0)
FI_FLIPVERT     EQUATE(1)

COLORDEPTH  Equate(Long)
            Itemize(0)
FI_1BIT       Equate
FI_4BIT       Equate
FI_8BIT       Equate
FI_8BITCOLOR  Equate
FI_8BITGRAY   Equate
FI_16BIT      Equate
FI_16_555BIT  Equate
FI_16_565BIT  Equate
FI_24BIT      Equate
FI_32BIT      Equate
             End

FIFF_SUPPORTSREAD     Equate(1)
FIFF_SUPPORTSWRITE    Equate(0)

SECTION('Prototypes')
  Module('FreeImgLib')

    ! Init / Error routines ----------------------------------------------------
    FreeImage_Initialise(BOOL load_local_plugins_only=false),Pascal, Name('_FreeImage_Initialise@4'),DLL(1)
    FreeImage_DeInitialise(),Pascal, Name('_FreeImage_DeInitialise@0'),DLL(1)


    ! Version routines ---------------------------------------------------------
    FreeImage_GetVersion(),UNSIGNED, Pascal, Raw, Name('_FreeImage_GetVersion@0'),DLL(1)
    FreeImage_GetCopyrightMessage(),UNSIGNED, Pascal, Raw, Name('_FreeImage_GetCopyrightMessage@0'),DLL(1)

    ! Message output functions -------------------------------------------------
    FreeImage_SetOutputMessage(UNSIGNED omf), Pascal, Name('_FreeImage_SetOutputMessage@4'),DLL(1)

    !DLL_API void DLL_CALLCONV FreeImage_SetOutputMessageStdCall(FreeImage_OutputMessageFunctionStdCall omf);
    !DLL_API void DLL_CALLCONV FreeImage_OutputMessageProc(int fif, const char *fmt, ...);


    ! Allocate / Clone / Unload routines ---------------------------------------

    FreeImage_Allocate(Long width, |
                       Long height, |
                       Long bpp, |
                       unsigned red_mask=0, |
                       unsigned green_mask=0, |
                       unsigned blue_mask=0 |
                       ),LPFIBITMAP , Pascal, Name('_FreeImage_Allocate@24'),DLL(1)

    FreeImage_AllocateT(FREE_IMAGE_TYPE fitype, |
                        Long width, |
                        Long height, |
                        Long bpp=8, |
                        unsigned red_mask=0, |
                        unsigned green_mask=0, |
                        unsigned blue_mask=0 |
                        ), LPFIBITMAP, Pascal, Name('_FreeImage_AllocateT@28'),DLL(1)

    FreeImage_Clone(LPFIBITMAP lbdib), LPFIBITMAP, Pascal, Name('_FreeImage_Clone@4'),DLL(1)

    FreeImage_Unload(LPFIBITMAP lbdib), Pascal, Name('_FreeImage_Unload@4'),DLL(1)

    ! Header loading routines --------------------------------------------------
    FreeImage_HasPixels(LPFIBITMAP lpdib),BOOL, Pascal, Name('_FreeImage_HasPixels@4'),DLL(1)


    ! Load / Save routines -----------------------------------------------------    
    FreeImage_Load(FREE_IMAGE_FORMAT fif, |
                   *Cstring szfilename, |
                   Long flags=0|
                   ),LPFIBITMAP, Pascal, Raw Name('_FreeImage_Load@12'),DLL(1)

    FreeImage_LoadU(FREE_IMAGE_FORMAT fif, |    3.8
                    *? wfilename, |
                    Long flags=0  |
                   ),LPFIBITMAP, Pascal, Raw Name('_FreeImage_LoadU@12'),DLL(1)



    FreeImage_LoadFromHandle(FREE_IMAGE_FORMAT fif, |
                             *FreeImageIo lpio,     |
                             HANDLE _fihandle, |
                             LONG flags=0|
                             ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_LoadFromHandle@16'),DLL(1)

    FreeImage_Save(FREE_IMAGE_FORMAT fif,   |
                   LPFIBITMAP lbdib,          |
                   *Cstring filename,       |
                   Long flags=0), BOOL, Pascal, Raw, Name('_FreeImage_Save@16'),DLL(1)

   FreeImage_SaveU(FREE_IMAGE_FORMAT fif,   |   3.8
                   FIBITMAP lpdib,          |
                   *? wfilename,       |
                   Long flags=0), BOOL, Pascal, Raw, Name('_FreeImage_SaveU@16'),DLL(1)



    FreeImage_SaveToHandle(FREE_IMAGE_FORMAT fif,   |
                           LPFIBITMAP lbdib,          |
                           *FreeImageIO io,         |
                           HANDLE _fihandle,        |
                           Long flags=0), BOOL, Pascal, Raw, Name('_FreeImage_SaveToHandle@20'),DLL(1)


    ! Memory I/O stream routines -----------------------------------------------   
    FreeImage_OpenMemory(Long data=0, |
                         UNSIGNED size_in_bytes=0 |
                        ), LPFIMEMORY, Pascal, Name('_FreeImage_OpenMemory@8'),DLL(1)


    FreeImage_CloseMemory(LPFIMEMORY mstream), Pascal, Name('_FreeImage_CloseMemory@4'),DLL(1)


    FreeImage_LoadFromMemory(FREE_IMAGE_FORMAT fif, |
                             LPFIMEMORY stream,     |
                             Long flags=0           |
                            ), LPFIBITMAP, Pascal, Name('_FreeImage_LoadFromMemory@12'),DLL(1)

    FreeImage_SaveToMemory(FREE_IMAGE_FORMAT fif, |
                           LPFIBITMAP lbdib, |
                           LPFIMEMORY stream, |
                           Long flags=0), Bool, Pascal, Name('_FreeImage_SaveToMemory@16'),DLL(1)

    FreeImage_TellMemory(LPFIMEMORY stream), Long, Pascal, Name('_FreeImage_TellMemory@4'),DLL(1)

    FreeImage_SeekMemory(LPFIMEMORY stream, |
                         Long offset, |
                         Long origin |
                        ), Bool, Pascal, Name('_FreeImage_SeekMemory@12'),DLL(1)

    FreeImage_AcquireMemory(LPFIMEMORY stream, |
                            *Long lplpdata,    |
                            *Unsigned size_in_bytes|
                            ),Bool, Pascal, Raw, Name('_FreeImage_AcquireMemory@12'),DLL(1)

    FreeImage_ReadMemory(*? memBuffer, |        ! Version 3.9.0
                         unsigned cbSize, |
                         unsigned ncount, |
                         LPFIMEMORY memStream|
                         ),UNSIGNED, Pascal, Raw, Name('_FreeImage_ReadMemory@16'),DLL(1)

    FreeImage_WriteMemory(*? memBuffer, |       ! Version 3.9.0
                          unsigned cbSize, |
                          unsigned ncount, |
                          LPFIMEMORY memStream|
                          ),UNSIGNED, Pascal, Raw, Name('_FreeImage_WriteMemory@16'),DLL(1)

    FreeImage_LoadMultiBitmapFromMemory(FREE_IMAGE_FORMAT fif, | ! Version 3.9.0
                           LPFIMEMORY stream, |
                           Long flags=0       |
                           ),UNSIGNED, Pascal, Raw, Name('_FreeImage_LoadMulitiBitmapFromMemory@12'),DLL(1)

    FreeImage_SaveMultiBitmapToMemory(FREE_IMAGE_FORMAT fif, |
                                      LPFIMULTIBITMAP lpbitmap, |
                                      LPFIMEMORY stream, |
                                      Long flags=0), BOOL,  Pascal, Raw, Name('_FreeImage_SaveMultiBitmapToMemory@16'),DLL(1)


    ! Plugin Interface ---------------------------------------------------------
    FreeImage_RegisterLocalPlugin(FI_InitProc proc_address, |
                                  <*Cstring szformat>, |
                                  <*Cstring szdescription>, |
                                  <*Cstring extension>, |
                                  <*Cstring regexpr> |
                                  ), FREE_IMAGE_FORMAT, Pascal, Raw, Name('_FreeImage_RegisterLocalPlugin@20'),DLL(1)

    FreeImage_RegisterExternalPlugin(*Cstring szpath, |
                                     <*Cstring szformat>, |
                                     <*Cstring szdescription>, |
                                     <*Cstring extension>, |
                                     <*Cstring regexpr> |
                                     ), FREE_IMAGE_FORMAT, Pascal, Raw, Name('_FreeImage_RegisterExternalPlugin@20'),DLL(1)

    FreeImage_GetFIFCount(), Long, Pascal, Name('_FreeImage_GetFIFCount@0'),DLL(1)

    FreeImage_SetPluginEnabled(FREE_IMAGE_FORMAT fif, |
                               BOOL bEnable |
                               ),Long, Pascal, Name('_FreeImage_SetPluginEnabled@8'),DLL(1)

    FreeImage_IsPluginEnabled(FREE_IMAGE_FORMAT fif),Long, Pascal, Name('_FreeImage_IsPluginEnabled@4'),DLL(1)
    FreeImage_GetFIFFromFormat(*Cstring szformat), FREE_IMAGE_FORMAT, Pascal, Raw, Name('_FreeImage_GetFIFFromFormat@4'),DLL(1)
    FreeImage_GetFIFFromMime(*Cstring mime), FREE_IMAGE_FORMAT, Pascal, Raw, Name('_FreeImage_GetFIFFromMime@4'),DLL(1)

    FreeImage_GetFormatFromFIF(FREE_IMAGE_FORMAT fif), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetFormatFromFIF@4'),DLL(1)
    FreeImage_GetFIFExtensionList(FREE_IMAGE_FORMAT fif), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetFIFExtensionList@4'),DLL(1)
    FreeImage_GetFIFDescription(FREE_IMAGE_FORMAT fif), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetFIFDescription@4'),DLL(1)
    FreeImage_GetFIFRegExpr(FREE_IMAGE_FORMAT fif), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetFIFRegExpr@4'),DLL(1)
    FreeImage_GetFIFMimeType(FREE_IMAGE_FORMAT fif), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetFIFMimeType@4'),DLL(1)

    FreeImage_GetFIFFromFilename(*Cstring filename), FREE_IMAGE_FORMAT, Pascal, Raw, Name('_FreeImage_GetFIFFromFilename@4'),DLL(1)
    FeeImage_GetFIFFromFilenameU(*? wfilename), FREE_IMAGE_FORMAT, Pascal, Raw, Name('_FreeImage_GetFIFFromFilenameU@4'),DLL(1) !3.8
    FreeImage_FIFSupportsReading(FREE_IMAGE_FORMAT fif), BOOL, Pascal, Raw, Name('_FreeImage_FIFSupportsReading@4'),DLL(1)
    FreeImage_FIFSupportsWriting(FREE_IMAGE_FORMAT fif), BOOL, Pascal, Raw, Name('_FreeImage_FIFSupportsWriting@4'),DLL(1)
    FreeImage_FIFSupportsExportBPP(FREE_IMAGE_FORMAT fif, Long bpp), BOOL, Pascal, Raw, Name('_FreeImage_FIFSupportsExportBPP@8'),DLL(1)
    FreeImage_FIFSupportsExportType(FREE_IMAGE_FORMAT fif, FREE_IMAGE_TYPE type), BOOL, Pascal, Raw, Name('_FreeImage_FIFSupportsExportType@8'),DLL(1)
    FreeImage_FIFSupportsICCProfiles(FREE_IMAGE_FORMAT fif), BOOL, Pascal, Raw, Name('_FreeImage_FIFSupportsICCProfiles@4'),DLL(1)
    FreeImage_FIFSupportsNoPixels(FREE_IMAGE_FORMAT fif), BOOL, Pascal, Raw, Name('_FreeImage_FIFSupportsNoPixels@4'),DLL(1)

    ! Multipaging interface ----------------------------------------------------

    FreeImage_OpenMultiBitmap(FREE_IMAGE_FORMAT fif, |
                              *Cstring filename, |
                              BOOL create_new, |
                              BOOL read_only, |
                              BOOL keep_cache_in_memory=0,|
                              LONG flags=0 |
                              ),LPFIMULTIBITMAP, Pascal, Raw, Name('_FreeImage_OpenMultiBitmap@24'),DLL(1)

    FreeImage_OpenMultiBitmapFromHandle(FREE_IMAGE_FORMAT fif, |
                                        UNSIGNED lpio, |
                                        fi_handle phandle, |
                                        Long flags=0 |
                                        ),LPFIMULTIBITMAP, Pascal, Raw, Name('_FreeImage_OpenMultiBitmapFromHandle@16'),DLL(1)
                                        
    FreeImage_SaveMultiBitmapToHandle(FREE_IMAGE_FORMAT fif, |
                                      LPFIMULTIBITMAP lpbitmap, |
                                      UNSIGNED lpio,         |
                                      fi_handle handle,      |
                                      Long flags=0           |
                                      ),BOOL, Pascal, Raw, Name('_FreeImage_SaveMultiBitmapToHandle@20'),DLL(1)

    FreeImage_CloseMultiBitmap(LPFIMULTIBITMAP lpbitmap, |
                               Long flags=0 |
                               ), Bool, Pascal, Raw, Name('_FreeImage_CloseMultiBitmap@8'),DLL(1)

    FreeImage_GetPageCount(LPFIMULTIBITMAP lpbitmap), Long, Pascal, Raw, Name('_FreeImage_GetPageCount@4'),DLL(1)



    FreeImage_AppendPage(LPFIMULTIBITMAP lpbitmap, |
                         LPFIBITMAP lpdata), Pascal, Raw, Name('_FreeImage_AppendPage@8'),DLL(1)

    FreeImage_InsertPage(LPFIMULTIBITMAP lpbitmap, |
                         Long npage, |
                         LPFIBITMAP lpdata), Pascal, Raw, Name('_FreeImage_InsertPage@12'),DLL(1)

    FreeImage_DeletePage(LPFIMULTIBITMAP lpbitmap, |
                         Long npage), Pascal, Raw, Name('_FreeImage_DeletePage@8'),DLL(1)

    FreeImage_LockPage(LPFIMULTIBITMAP lpbitmap, |
                       Long npage), LPFIBITMAP, Pascal, Raw, Name('_FreeImage_LockPage@8'),DLL(1)

    FreeImage_UnlockPage(LPFIMULTIBITMAP lpbitmap, |
                         LPFIBITMAP lpdata, |
                         BOOL changed), Pascal, Raw, Name('_FreeImage_UnlockPage@12'),DLL(1)

    FreeImage_MovePage(LPFIMULTIBITMAP lpbitmap, |
                       Long ntargetPage, |
                       Long sourcePage), Bool, Pascal, Raw, Name('_FreeImage_MovePage@12'),DLL(1)

    FreeImage_GetLockedPageNumbers(LPFIMULTIBITMAP lpbitmap, |
                                   Long lpPageArray, |  !pass null for size of array
                                   *Long lpCount), Bool, Pascal, Raw, Name('_FreeImage_GetLockedPageNumbers@12'),DLL(1)


    ! Filetype request routines ------------------------------------------------
    FreeImage_GetFileType(*Cstring filename, |
                          Long nsize=0|
                          ), FREE_IMAGE_FORMAT, Pascal, Raw, Name('_FreeImage_GetFileType@8'),DLL(1)

    FreeImage_GetFileTypeU(*? wfilename, |  3.8
                           Long nsize=0 |
                          ), FREE_IMAGE_FORMAT, Pascal, Raw, Name('_FreeImage_GetFileTypeU@8'),DLL(1)

    FreeImage_GetFileTypeFromHandle(*FreeImageIO lpfio, |
                                    HANDLE fihandle, |
                                    Long nsize=0 |
                                    ), FREE_IMAGE_FORMAT, Pascal, Raw, Name('_FreeImage_GetFileTypeFromHandle@12'),DLL(1)

    FreeImage_GetFileTypeFromMemory(LPFIMEMORY lpstream, |
                                    Long nsize=0 |
                                    ), FREE_IMAGE_FORMAT, Pascal, Raw, Name('_FreeImage_GetFileTypeFromMemory@8'),DLL(1)


    ! Image type request routine -----------------------------------------------
    FreeImage_GetImageType(LPFIBITMAP lpdib), FREE_IMAGE_TYPE, Pascal, Raw, Name('_FreeImage_GetImageType@4'),DLL(1)


    ! FreeImage helper routines ------------------------------------------------
    FreeImage_IsLittleEndian(), BOOL, Pascal, Raw, Name('_FreeImage_IsLittleEndian@0'),DLL(1)

    FreeImage_LookupX11Color(*Cstring szColor, |
                             *BYTE nRed, |
                             *BYTE nGreen, |
                             *BYTE nBlue |
                             ), BOOL, Pascal, Raw, Name('_FreeImage_LookupX11Color@16'),DLL(1)

    FreeImage_LookupSVGColor(*Cstring szColor, |
                             *BYTE nRed, |
                             *BYTE nGreen, |
                             *BYTE nBlue |
                             ), BOOL, Pascal, Raw, Name('_FreeImage_LookupSVGColor@16'),DLL(1)



    ! Pixel access routines ----------------------------------------------------

    FreeImage_GetBits(LPFIBITMAP lpdib), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetBits@4'),DLL(1)

    FreeImage_GetScanLine(LPFIBITMAP lpdib, |
                          Long scanline), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetScanLine@8'),DLL(1)

    FreeImage_GetPixelIndex(LPFIBITMAP lpdib, |
                            unsigned x, unsigned y, |
                            *BYTE nvalue|
                            ), BOOL, Pascal, Raw, Name('_FreeImage_GetPixelIndex@16'),DLL(1)

    FreeImage_GetPixelColor(LPFIBITMAP lpdib, |
                            unsigned x, unsigned y, |
                            *SA_RGBQUAD nvalue |
                            ), BOOL, Pascal, Raw, Name('_FreeImage_GetPixelColor@16'),DLL(1)

    FreeImage_SetPixelIndex(LPFIBITMAP lpdib, |
                            unsigned x, unsigned y, |
                            *BYTE nvalue), BOOL, Pascal, Raw, Name('_FreeImage_SetPixelIndex@16'),DLL(1)

    FreeImage_SetPixelColor(LPFIBITMAP lpdib, |
                            unsigned x, unsigned y, |
                            *SA_RGBQUAD nvalue), BOOL, Pascal, Raw, Name('_FreeImage_SetPixelColor@16'),DLL(1)


! DIB info routines --------------------------------------------------------

    FreeImage_GetColorsUsed(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetColorsUsed@4'),DLL(1)

    FreeImage_GetBPP(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetBPP@4'),DLL(1)

    FreeImage_GetWidth(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetWidth@4'),DLL(1)
    FreeImage_GetHeight(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetHeight@4'),DLL(1)

    FreeImage_GetLine(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetLine@4'),DLL(1)

    FreeImage_GetPitch(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetPitch@4'),DLL(1)

    FreeImage_GetDIBSize(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetDIBSize@4'),DLL(1)
    FreeImage_GetPalette(LPFIBITMAP lpdib), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetPalette@4'),DLL(1)

    FreeImage_GetDotsPerMeterX(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetDotsPerMeterX@4'),DLL(1)
    FreeImage_GetDotsPerMeterY(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetDotsPerMeterY@4'),DLL(1)
    FreeImage_SetDotsPerMeterX(LPFIBITMAP lpdib, |
                               unsigned res |
                               ), Pascal, Raw, Name('_FreeImage_SetDotsPerMeterX@8'),DLL(1)

    FreeImage_SetDotsPerMeterY(LPFIBITMAP lpdib, |
                               unsigned res |
                               ), Pascal, Raw, Name('_FreeImage_SetDotsPerMeterY@8'),DLL(1)

    FreeImage_GetInfoHeader(LPFIBITMAP lpdib), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetInfoHeader@4'),DLL(1)
    FreeImage_GetInfo(LPFIBITMAP lpdib), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetInfo@4'),DLL(1)
    FreeImage_GetColorType(LPFIBITMAP lpdib), FREE_IMAGE_COLOR_TYPE, Pascal, Raw, Name('_FreeImage_GetColorType@4'),DLL(1)

    FreeImage_GetRedMask(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetRedMask@4'),DLL(1)
    FreeImage_GetGreenMask(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetGreenMask@4'),DLL(1)
    FreeImage_GetBlueMask(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetBlueMask@4'),DLL(1)

    FreeImage_GetTransparencyCount(LPFIBITMAP lpdib), unsigned, Pascal, Raw, Name('_FreeImage_GetTransparencyCount@4'),DLL(1)
    FreeImage_GetTransparencyTable(LPFIBITMAP lpdib), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetTransparencyCount@4'),DLL(1)

    FreeImage_SetTransparent(LPFIBITMAP lpdib, |
                              BOOL enabled |
                             ), Pascal, Raw, Name('_FreeImage_SetTransparent@8'),DLL(1)

    FreeImage_SetTransparencyTable(LPFIBITMAP lpdib, |
                                   UNSIGNED lptable, |   use Address()
                                   Long count |
                                   ), Pascal, Raw, Name('_FreeImage_SetTransparencyTable@12'),DLL(1)

    FreeImage_IsTransparent(LPFIBITMAP lpdib), BOOL, Pascal, Raw, Name('_FreeImage_IsTransparent@4'),DLL(1)
    FreeImage_SetTransparentIndex(LPFIBITMAP lpdib, Long index), Pascal, Raw, Name('_FreeImage_SetTransparentIndex@8'),DLL(1) ! Version 3.10.0
    FreeImage_GetTransparentIndex(LPFIBITMAP lpdib), Long , Pascal, Raw, Name('_FreeImage_GetTransparentIndex@4'),DLL(1)      ! Version 3.10.0

    FreeImage_HasBackgroundColor(LPFIBITMAP lpdib), BOOL, Pascal, Raw, Name('_FreeImage_HasBackgroundColor@4'),DLL(1)
    FreeImage_GetBackgroundColor(LPFIBITMAP lpdib, |
                                 *SA_RGBQUAD bkcolor |
                                 ), BOOL, Pascal, Raw, Name('_FreeImage_GetBackgroundColor@8'),DLL(1)

    FreeImage_SetBackgroundColor(LPFIBITMAP lpdib, |
                                 *SA_RGBQUAD bkcolor |
                                 ), BOOL, Pascal, Raw, Name('_FreeImage_SetBackgroundColor@8'),DLL(1)

    FreeImage_GetThumbnail(LPFIBITMAP lpdib),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_GetThumbnail@4'),DLL(1)
    FreeImage_SetThumbnail(LPFIBITMAP lpdib, LPFIBITMAP lpthumbnail),BOOL, Pascal, Raw, Name('_FreeImage_SetThumbnail@8'),DLL(1)


    ! ICC profile routines -----------------------------------------------------
    FreeImage_GetICCProfile(LPFIBITMAP lpdib), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetICCProfile@4'),DLL(1)

    FreeImage_CreateICCProfile(LPFIBITMAP lpdib, |
                               *? lpdata, |
                               Long nsize |
                               ), UNSIGNED, Pascal, Raw, Name('_FreeImage_CreateICCProfile@12'),DLL(1)

    FreeImage_DestroyICCProfile(LPFIBITMAP lpdib), Pascal, Raw, Name('_FreeImage_DestroyICCProfile@4'),DLL(1)



    ! Line conversion routines -------------------------------------------------

    FreeImage_ConvertLine1To4(*? lptarget,  |
                              *? lpsource,  |
                              Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine1To4@12'),DLL(1)

    FreeImage_ConvertLine8To4(*? lptarget, |
                              *? lpsource, |
                              Long width_in_pixels, |
                              *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine8To4@16'),DLL(1)

    FreeImage_ConvertLine16To4_555(*? lptarget,  |
                                    *? lpsource, |
                                    Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine16To4_555@12'),DLL(1)

    FreeImage_ConvertLine16To4_565(*? lptarget,  |
                                    *? lpsource, |
                                    Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine16To4_565@12'),DLL(1)

    FreeImage_ConvertLine24To4(*? lptarget,  |
                               *? lpsource, |
                               Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine24To4@12'),DLL(1)

    FreeImage_ConvertLine32To4(*? lptarget,  |
                               *? lpsource, |
                               Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine32To4@12'),DLL(1)

    FreeImage_ConvertLine1To8(*? lptarget,  |
                              *? lpsource, |
                              Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine1To8@12'),DLL(1)

    FreeImage_ConvertLine4To8(*? lptarget,  |
                              *? lpsource, |
                              Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine4To8@12'),DLL(1)

    FreeImage_ConvertLine16To8_555(*? lptarget,  |
                                   *? lpsource, |
                                   Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine16To8_555@12'),DLL(1)

    FreeImage_ConvertLine16To8_565(*? lptarget,  |
                                   *? lpsource, |
                                   Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine16To8_565@12'),DLL(1)

    FreeImage_ConvertLine24To8(*? lptarget,  |
                               *? lpsource, |
                               Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine24To8@12'),DLL(1)

    FreeImage_ConvertLine32To8(*? lptarget,  |
                               *? lpsource, |
                               Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine32To8@12'),DLL(1)

    FreeImage_ConvertLine1To16_555(*? lptarget,  |
                                   *? lpsource, |
                                   Long width_in_pixels, |
                                   *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine1To16_555@16'),DLL(1)

    FreeImage_ConvertLine4To16_555(*? lptarget,  |
                                   *? lpsource, |
                                   Long width_in_pixels, |
                                   *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine4To16_555@16'),DLL(1)

    FreeImage_ConvertLine8To16_555(*? lptarget,  |
                                   *? lpsource, |
                                   Long width_in_pixels, |
                                   *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine8To16_555@16'),DLL(1)

    FreeImage_ConvertLine16_565_To16_555(*? lptarget,  |
                                         *? lpsource, |
                                         Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine16_565_To16_555@12'),DLL(1)

    FreeImage_ConvertLine24To16_555(*? lptarget,  |
                                    *? lpsource, |
                                    Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine24To16_555@12'),DLL(1)

    FreeImage_ConvertLine32To16_555(*? lptarget,  |
                                    *? lpsource, |
                                    Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine32To16_555@12'),DLL(1)

    FreeImage_ConvertLine1To16_565(*? lptarget,  |
                                   *? lpsource, |
                                   Long width_in_pixels, |
                                   *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine1To16_565@16'),DLL(1)

    FreeImage_ConvertLine4To16_565(*? lptarget,  |
                                   *? lpsource, |
                                   Long width_in_pixels,|
                                   *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine4To16_565@16'),DLL(1)

    FreeImage_ConvertLine8To16_565(*? lptarget,  |
                                   *? lpsource, |
                                   Long width_in_pixels, |
                                   *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine8To16_565@16'),DLL(1)

    FreeImage_ConvertLine16_555_To16_565(*? lptarget,  |
                                         *? lpsource, |
                                         Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine16_555_To16_565@12'),DLL(1)

    FreeImage_ConvertLine24To16_565(*? lptarget,  |
                                    *? lpsource, |
                                    Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine24To16_565@12'),DLL(1)

    FreeImage_ConvertLine32To16_565(*? lptarget,  |
                                    *? lpsource, |
                                    Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine32To16_565@12'),DLL(1)

    FreeImage_ConvertLine1To24(*? lptarget,  |
                               *? lpsource, |
                               Long width_in_pixels, |
                               *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine1To24@16'),DLL(1)

    FreeImage_ConvertLine4To24(*? lptarget,  |
                               *? lpsource, |
                               Long width_in_pixels, |
                               *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine4To24@16'),DLL(1)

    FreeImage_ConvertLine8To24(*? lptarget,  |
                               *? lpsource, |
                               Long width_in_pixels, |
                               *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine8To24@16'),DLL(1)

    FreeImage_ConvertLine16To24_555(*? lptarget,  |
                                    *? lpsource, |
                                    Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine16To24_555@12'),DLL(1)

    FreeImage_ConvertLine16To24_565(*? lptarget, |
                                    *? lpsource, |
                                    Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine16To24_565@12'),DLL(1)

    FreeImage_ConvertLine32To24(*? lptarget,  |
                                *? lpsource, |
                                Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine32To24@12'),DLL(1)

    FreeImage_ConvertLine1To32(*? lptarget,  |
                               *? lpsource, |
                               Long width_in_pixels, |
                               *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine1To32@16'),DLL(1)

    FreeImage_ConvertLine4To32(*? lptarget,  |
                               *? lpsource, |
                               Long width_in_pixels, |
                               *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine4To32@16'),DLL(1)

    FreeImage_ConvertLine8To32(*? lptarget,  |
                               *? lpsource, |
                               Long width_in_pixels, |
                               *SA_RGBQUAD lppalette), Pascal, Raw, Name('_FreeImage_ConvertLine8To32@16'),DLL(1)

    FreeImage_ConvertLine16To32_555(*? lptarget,  |
                                    *? lpsource, |
                                    Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine16To32_555@12'),DLL(1)

    FreeImage_ConvertLine16To32_565(*? lptarget,  |
                                    *? lpsource, |
                                    Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine16To32_565@12'),DLL(1)

    FreeImage_ConvertLine24To32(*? lptarget,  |
                                *? lpsource, |
                                Long width_in_pixels), Pascal, Raw, Name('_FreeImage_ConvertLine24To32@12'),DLL(1)



    ! Smart conversion routines ------------------------------------------------

    FreeImage_ConvertTo4Bits(LPFIBITMAP lpdib),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertTo4Bits@4'),DLL(1)
    FreeImage_ConvertTo8Bits(LPFIBITMAP lpdib),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertTo8Bits@4'),DLL(1)
    FreeImage_ConvertToGreyscale(LPFIBITMAP lpdib),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertToGreyscale@4'),DLL(1) !3.8

    FreeImage_ConvertTo16Bits555(LPFIBITMAP lpdib),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertTo16Bits555@4'),DLL(1)
    FreeImage_ConvertTo16Bits565(LPFIBITMAP lpdib),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertTo16Bits565@4'),DLL(1)
    FreeImage_ConvertTo24Bits(LPFIBITMAP lpdib),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertTo24Bits@4'),DLL(1)
    FreeImage_ConvertTo32Bits(LPFIBITMAP lpdib),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertTo32Bits@4'),DLL(1)
    FreeImage_ColorQuantize(LPFIBITMAP lpdib, |
                            FREE_IMAGE_QUANTIZE quantize |
                            ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ColorQuantize@8'),DLL(1)

    FreeImage_ColorQuantizeEx(LPFIBITMAP lpdib, |
                              FREE_IMAGE_QUANTIZE quantize=FIQ_WUQUANT,|
                              Long PaletteSize=256, |
                              Long ReserveSize=0, |
                              *SA_RGBQUAD ReservePalette |
                              ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ColorQuantizeEx@20'),DLL(1)


    FreeImage_Threshold(LPFIBITMAP lpdib, |
                        BYTE T |
                        ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_Threshold@8'),DLL(1)

    FreeImage_Dither(LPFIBITMAP lpdib, |
                     FREE_IMAGE_DITHER algorithm |
                     ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_Dither@8'),DLL(1)

    FreeImage_ConvertFromRawBits(*? lpbits, |
                                 Long nwidth, |
                                 Long nheight, |
                                 Long npitch, |
                                 unsigned bpp, |
                                 unsigned red_mask, |
                                 unsigned green_mask, |
                                 unsigned blue_mask, |
                                 BOOL topdown=false |
                                 ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertFromRawBits@36'),DLL(1)

    FreeImage_ConvertToRawBits(*? lpbits, |
                               LPFIBITMAP lpdib, |
                               Long pitch, |
                               unsigned bpp, |
                               unsigned red_mask, |
                               unsigned green_mask, |
                               unsigned blue_mask, |
                               BOOL topdown=false |
                               ), Pascal, Raw, Name('_FreeImage_ConvertToRawBits@32'),DLL(1)

    FreeImage_ConvertToRGBF(LPFIBITMAP lpdib),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertToRGBF@4'),DLL(1)

    FreeImage_ConvertToStandardType(LPFIBITMAP lpsrc, |
                                    BOOL scale_linear=true |
                                    ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertToStandardType@8'),DLL(1)

    FreeImage_ConvertToType(LPFIBITMAP lpsrc, |
                            FREE_IMAGE_TYPE dst_type, |
                            BOOL scale_linear=true |
                            ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ConvertToType@12'),DLL(1)

    ! tone mapping operators
    FreeImage_ToneMapping(LPFIBITMAP lpdib, |
                          FREE_IMAGE_TMO tmo, |
                          Real first_param=0, |
                          Real second_param=0 |
                          ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_ToneMapping@24'),DLL(1)

    FreeImage_TmoDrago03(LPFIBITMAP lpsrc, |
                         Real gamma=2.2, |
                         Real exposure=0 |
                         ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_TmoDrago03@20'),DLL(1)

    FreeImage_TmoReinhard05(LPFIBITMAP lpsrc, |
                            Real intensity=0, |
                            Real contrast=0 |
                            ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_TmoReinhard05@20'),DLL(1)     
                            
    FreeImage_TmoReinhard05Ex(LPFIBITMAP lpsrc, |
                              Real intensity=0, |
                              Real contrast=0,  |
                              Real adaptation=0,|
                              Real color_correction=0|
                              )LPFIBITMAP, Pascal, Raw, Name('_FreeImage_TmoReinhard05Ex@36'),DLL(1)
                        

    FreeImage_TmoFattal02(LPFIBITMAP lpsrc, |
                          Real color_saturation=0.5, |
                          Real attenuation=0.85|
                          ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_TmoFattal02@20'),DLL(1) ! Version 3.10.0



! ZLib interface -----------------------------------------------------------

    FreeImage_ZLibCompress(*? lptarget, |
                           UNSIGNED target_size, |
                           *? lpsource, |
                           UNSIGNED source_size), UNSIGNED, Pascal, Raw, Name('_FreeImage_ZLibCompress@16'),DLL(1)

    FreeImage_ZLibUncompress(*? lptarget, |
                             UNSIGNED target_size, |
                             *? lpsource, |
                             UNSIGNED source_size), UNSIGNED, Pascal, Raw, Name('_FreeImage_ZLibUncompress@16'),DLL(1)

    FreeImage_ZLibGZip(*? lptarget, |
                       UNSIGNED target_size, |
                       *? lpsource, |
                       UNSIGNED source_size), UNSIGNED, Pascal, Raw, Name('_FreeImage_ZLibGZip@16'),DLL(1)

    FreeImage_ZLibGUnzip(*? lptarget, |
                         UNSIGNED target_size, |
                         *? lpsource, |
                         UNSIGNED source_size|
                         ),UNSIGNED, Pascal, Raw, Name('_FreeImage_ZLibGUnZip@16'),DLL(1)

    FreeImage_ZLibCRC32(UNSINGED crc, |
                        *? lpsource,  |
                        UNSIGNED source_size), UNSIGNED, Pascal, Raw, Name('_FreeImage_ZLibCRC32@12'),DLL(1)



! --------------------------------------------------------------------------
! Metadata routines --------------------------------------------------------
! --------------------------------------------------------------------------

! tag creation / destruction ---------------------------
    FreeImage_CreateTag(), UNSIGNED, Pascal, Raw, Name('_FreeImage_CreateTag@0'),DLL(1)
    FreeImage_DeleteTag(*FITAG tag), Pascal, Raw, Name('_FreeImage_DeleteTag@4'),DLL(1)
    FreeImage_CloneTag(*FITAG tag), UNSIGNED, Pascal, Raw, Name('_FreeImage_CloneTag@4'),DLL(1)


! tag getters and setters ----------------------------
    FreeImage_GetTagKey(*FITAG tag), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetTagKey@4'),DLL(1)
    FreeImage_GetTagDescription(*FITAG tag), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetTagDescription@4'),DLL(1)
    FreeImage_GetTagID(*FITAG tag),Ushort, Pascal, Raw, Name('_FreeImage_GetTagID@4'),DLL(1)
    FreeImage_GetTagType(*FITAG tag),FREE_IMAGE_MDTYPE, Pascal, Raw, Name('_FreeImage_GetTagType@4'),DLL(1)
    FreeImage_GetTagCount(*FITAG tag), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetTagCount@4'),DLL(1)
    FreeImage_GetTagLength(*FITAG tag), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetTagLength@4'),DLL(1)
    FreeImage_GetTagValue(*FITAG tag), UNSIGNED, Pascal, Raw, Name('_FreeImage_GetTagValue@4'),DLL(1)


    FreeImage_SetTagKey(*FITAG tag, *Cstring szKey), BOOL, Pascal, Raw, Name('_FreeImage_SetTagKey@8'),DLL(1)
    FreeImage_SetTagDescription(*FITAG tag, *Cstring description), BOOL, Pascal, Raw, Name('_FreeImage_SetTagDescription@8'),DLL(1)
    FreeImage_SetTagID(*FITAG tag, Ushort id), BOOL, Pascal, Raw, Name('_FreeImage_SetTagID@8'),DLL(1)
    FreeImage_SetTagType(*FITAG tag, FREE_IMAGE_MDTYPE mdtype), BOOL, Pascal, Raw, Name('_FreeImage_SetTagType@8'),DLL(1)
    FreeImage_SetTagCount(*FITAG tag, UNSIGNED count), BOOL, Pascal, Raw, Name('_FreeImage_SetTagCount@8'),DLL(1)
    FreeImage_SetTagLength(*FITAG tag, UNSIGNED length), BOOL, Pascal, Raw, Name('_FreeImage_SetTagLength@8'),DLL(1)
    FreeImage_SetTagValue(*FITAG tag, *? lpvalue), BOOL, Pascal, Raw, Name('_FreeImage_SetTagValue@8'),DLL(1)


! iterator
    FreeImage_FindFirstMetadata(FREE_IMAGE_MDMODEL model, |
                                LPFIBITMAP lpdib, |
                                *Long lplptag), UNSIGNED , Pascal, Raw, Name('_FreeImage_FindFirstMetadata@12'),DLL(1)
                               ! *FIMETADATA
    FreeImage_FindNextMetadata(UNSIGNED mdhandle, |
                               *Long lplptag), BOOL, Pascal, Raw, Name('_FreeImage_FindNextMetadata@8'),DLL(1)
                                ! *FIMETADATA
    FreeImage_FindCloseMetadata(UNSIGNED mdhandle), Pascal, Raw, Name('_FreeImage_FindCloseMetadata@4'),DLL(1)


    ! metadata setter and getter
    FreeImage_SetMetadata(FREE_IMAGE_MDMODEL model, |
                          LPFIBITMAP lpdib, |
                          UNSIGNED lpszkey, |
                          *FITAG tag), BOOL, Pascal, Raw, Name('_FreeImage_SetMetadata@16'),DLL(1)

    FreeImage_GetMetadata(FREE_IMAGE_MDMODEL model, |
                          LPFIBITMAP lpdib, |
                          *Cstring szkey, |
                          *Long lplptag), BOOL, Pascal, Raw, Name('_FreeImage_GetMetadata@16'),DLL(1)


! helpers
    FreeImage_GetMetadataCount(FREE_IMAGE_MDMODEL model, |
                               LPFIBITMAP lpdib), Unsigned, Pascal, Raw, Name('_FreeImage_GetMetadataCount@8'),DLL(1)

    FreeImage_CloneMetadata(LPFIBITMAP lpdst, lpFIBITMAP lpsrc),BOOL, Pascal, Raw, Name('_FreeImage_CloneMetadata@8'),DLL(1) !3.12.0


! tag to C string conversion
    FreeImage_TagToString(FREE_IMAGE_MDMODEL model, |
                          *FITAG tag, |
                          <*Cstring Make>), UNSIGNED, Pascal, Raw, Name('_FreeImage_TagToString@12'),DLL(1)


! --------------------------------------------------------------------------
! Image manipulation toolkit -----------------------------------------------
! --------------------------------------------------------------------------

! rotation and flipping
    FreeImage_RotateClassic(LPFIBITMAP lpdib, |
                            Real angle),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_RotateClassic@12'),DLL(1)

    FreeImage_Rotate(LPFIBITMAP lpdib, |
                            Real angle,|
                            <*? BkColor>),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_Rotate@16'),DLL(1)     


    FreeImage_RotateEx(LPFIBITMAP lpdib, |
                       Real fangle, |
                       Real x_shift, |
                       Real y_shift, |
                       Real x_origin, |
                       Real y_origin, |
                       BOOL use_mask),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_RotateEx@48'),DLL(1)

    FreeImage_FlipHorizontal(LPFIBITMAP lpdib), BOOL, Pascal, Raw, Name('_FreeImage_FlipHorizontal@4'),DLL(1)
    FreeImage_FlipVertical(LPFIBITMAP lpdib), BOOL, Pascal, Raw, Name('_FreeImage_FlipVertical@4'),DLL(1)
    FreeImage_JPEGTransform(*Cstring src_file, |
                            *Cstring dst_file, |
                            FREE_IMAGE_JPEG_OPERATION operation, |
                            BOOL perfect=FALSE|
                            ), BOOL, Pascal, Raw, Name('_FreeImage_JPEGTransform@16'),DLL(1)

    FreeImage_JPEGTransformU(*? Wide_src_filename, |        !Unicode variant
                             *? Wide_dst_filename, |
                             FREE_IMAGE_JPEG_OPERATION operation, |
                             BOOL perfect=0 |
                             ), BOOL, Pascal, Raw, Name('_FreeImage_JPEGTransformU@16'),DLL(1)

  
    ! upsampling / downsampling
    FreeImage_Rescale(LPFIBITMAP lpdib, |
                      Long dst_width, |
                      Long dst_height, |
                      FREE_IMAGE_FILTER fifilter),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_Rescale@16'),DLL(1)

    FreeImage_MakeThumbnail(LPFIBITMAP lpdib, |         ! Version 3.9.0
                            Long max_pixel_size, |
                            BOOL convert=1       |
                            ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_MakeThumbnail@12'),DLL(1)


    ! color manipulation routines (point operations)
    FreeImage_AdjustCurve(LPFIBITMAP lpdib, |
                          *? lpLUT, |
                          FREE_IMAGE_COLOR_CHANNEL channel), BOOL, Pascal, Raw, Name('_FreeImage_AdjustCurve@12'),DLL(1)

    FreeImage_AdjustGamma(LPFIBITMAP lpdib, |
                          Real gamma), BOOL, Pascal, Raw, Name('_FreeImage_AdjustGamma@12'),DLL(1)

    FreeImage_AdjustBrightness(LPFIBITMAP lpdib, |
                               Real percentage), BOOL, Pascal, Raw, Name('_FreeImage_AdjustBrightness@12'),DLL(1)

    FreeImage_AdjustContrast(LPFIBITMAP lpdib, |
                             Real percentage), BOOL, Pascal, Raw, Name('_FreeImage_AdjustContrast@12'),DLL(1)

    FreeImage_Invert(LPFIBITMAP lpdib), BOOL, Pascal, Raw, Name('_FreeImage_Invert@4'),DLL(1)

    FreeImage_GetHistogram(LPFIBITMAP lpdib, |
                           *HISTO histogram,  |
                           FREE_IMAGE_COLOR_CHANNEL channel=FICC_BLACK), BOOL, Pascal, Raw, Name('_FreeImage_GetHistogram@12'),DLL(1)

    FreeImage_GetAdjustColorsLookupTable(*? LUT, |
                                         Real brightness, |
                                         Real contrast, |
                                         Real gamma, |
                                         BOOL invert|
                                         ),Long , Pascal, Raw, Name('_FreeImage_GetAdjustColorsLookupTable@32'),DLL(1)  ! Version 3.10.0

    FreeImage_AdjustColors(LPFIBITMAP lpdib, |
                           Real brightness,  |
                           Real contrast,    |
                           Real gamma, |
                           BOOL invert=FALSE|
                           ), BOOL, Pascal, Raw, Name('_FreeImage_AdjustColors@32'),DLL(1) ! Version 3.10.0

    FreeImage_ApplyColorMapping(LPFIBITMAP lpdib, |
                                *SA_RGBQUAD srccolors, |
                                *SA_RGBQUAD dstcolors, |
                                unsigned count, |
                                BOOL ignore_alpha, |
                                BOOL swap|
                                ),unsigned , Pascal, Raw, Name('_FreeImage_ApplyColorMapping@24'),DLL(1)  ! Version 3.10.0

    FreeImage_SwapColors(LPFIBITMAP lpdib, |
                         *SA_RGBQUAD color_a, |
                         *SA_RGBQUAD color_b, |
                         BOOL ignore_alpha |
                         ),unsigned , Pascal, Raw, Name('_FreeImage_SwapColors@16'),DLL(1) ! Version 3.10.0

    FreeImage_ApplyPaletteIndexMapping(LPFIBITMAP lpdib, |
                                       *Byte srcindices,    |
                                       *Byte dstindices,    |
                                       unsigned count,   |
                                       BOOL swap|
                                       ),unsigned , Pascal, Raw, Name('_FreeImage_ApplyPaletteIndexMapping@20'),DLL(1) ! Version 3.10.0

    FreeImage_SwapPaletteIndices(FIBLPFIBITMAPITMAP lpdib, |
                                 *Byte index_a, |
                                 *Byte index_b |
                                 ),unsigned , Pascal, Raw, Name('_FreeImage_SwapPaletteIndices@12'),DLL(1) ! Version 3.10.0


    ! channel processing routines
    FreeImage_GetChannel(LPFIBITMAP lpdib, |
                         FREE_IMAGE_COLOR_CHANNEL channel), LPFIBITMAP, Pascal, Raw, Name('_FreeImage_GetChannel@8'),DLL(1)

    FreeImage_SetChannel(LPFIBITMAP lpdib, |
                         LPFIBITMAP lpdib8, |
                         FREE_IMAGE_COLOR_CHANNEL channel), BOOL, Pascal, Raw, Name('_FreeImage_SetChannel@12'),DLL(1)

    FreeImage_GetComplexChannel(LPFIBITMAP lpsrc, |
                                FREE_IMAGE_COLOR_CHANNEL channel), LPFIBITMAP, Pascal, Raw, Name('_FreeImage_GetComplexChannel@8'),DLL(1)

    FreeImage_SetComplexChannel(LPFIBITMAP lpdst, |
                                LPFIBITMAP lpsrc, |
                                FREE_IMAGE_COLOR_CHANNEL channel), BOOL, Pascal, Raw, Name('_FreeImage_SetComplexChannel@12'),DLL(1)


    ! copy / paste / composite routines
    FreeImage_Copy(LPFIBITMAP lpdib, |
                   Long nleft, |
                   Long ntop, |
                   Long nright, |
                   Long nbottom), LPFIBITMAP, Pascal, Raw, Name('_FreeImage_Copy@20'),DLL(1)

    FreeImage_Paste(LPFIBITMAP lpdst, |
                    LPFIBITMAP lpsrc, |
                    Long nleft, |
                    Long ntop, |
                    Long alpha), BOOL, Pascal, Raw, Name('_FreeImage_Paste@20'),DLL(1)

    FreeImage_Composite(LPFIBITMAP lpfg, |
                        BOOL useFileBkg=false, |
                        UNSIGNED lpRGBappBkColor=0, |
                        LPFIBITMAP lpbg=0), LPFIBITMAP, Pascal, Raw, Name('_FreeImage_Composite@16'),DLL(1)

    FreeImage_JPEGCrop(*Cstring src_file, | !Version 3.9.2
                       *Cstring dst_file, |
                       Long left, Long top,   |
                       Long right, Long bottom|
                       ),BOOL, Pascal, Raw, Name('_FreeImage_JPEGCrop@24'),DLL(1)

    FreeImage_JPEGCropU(*? src_file, | !Unicode variant
                       *? dst_file, |
                       Long left, Long top,   |
                       Long right, Long bottom|
                       ),BOOL, Pascal, Raw, Name('_FreeImage_JPEGCropU@24'),DLL(1)

    FreeImage_PreMultiplyWithAlpha(LPFIBITMAP lpdib),BOOL, Pascal, Raw, Name('_FreeImage_PreMultiplyWithAlpha@4'),DLL(1) ! Version 3.10.0

   ! background filling routines
    FreeImage_FillBackground(LPFIBITMAP lpdib, |
                             *? lpcolor,       |
                             Long options=0    |
                             ), BOOL, Pascal, Raw, Name('_FreeImage_FillBackground@12'),DLL(1)
    
    FreeImage_EnlargeCanvas(LPFIBITMAP lpsrc,         |
                            Long left, Long top,      |
                            Long right, Long bottom,  |
                            *? lpcolor,               |
                            Long options=0            |
                            )LPFIBITMAP, Pascal, Raw, Name('_FreeImage_EnlargeCanvas@28'),DLL(1)              
                            
    FreeImage_AllocateEx(Long width,      |
                         Long height,     |
                         Long bpp,        |
                         const *SA_RGBQUAD lpcolor, |
                         Long options=0,  |
                         <const *SA_RGBQUAD lppalette>, |
                         unsigned red_mask=0, |
                         unsigned green_mask=0, |
                         unsigned blue_mask=0   |
                         ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_AllocateEx@36'),DLL(1)   
                         
    FreeImage_AllocateExT(FREE_IMAGE_TYPE ptype, |
                          Long width,            |
                          Long height,           |
                          Long bpp,              |
                          const *? color,    |
                          Long options=0,    |
                          <const *SA_RGBQUAD lppalette>, |
                          unsigned red_mask=0,  |
                          unsigned green_mask=0,|
                          unsigned blue_mask=0  |
                          ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_AllocateExT@40'),DLL(1) 



    ! miscellaneous algorithms

    FreeImage_MultigridPoissonSolver(LPFIBITMAP Laplacian, |
                                     Long ncycle=3|
                                     ),LPFIBITMAP, Pascal, Raw, Name('_FreeImage_MultigridPoissonSolver@8'),DLL(1)  ! Version 3.10.0



    End !Module