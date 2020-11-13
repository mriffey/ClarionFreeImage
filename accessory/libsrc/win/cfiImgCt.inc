!ABCIncludeFile(FREEIMAGE)
!============================================================
! cfiImgCt.inc -- Clarion FreeImage Control header file
!
!  Copyright © 2005-2006 Sand & Associates, Larry@sand-associates.com
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

  OMIT('_EndOfcfiImageControlClass_',_cfiImageControlClassIncluded_)
_cfiImageControlClassIncluded_  EQUATE(1)

  Include('FreeImcl.inc'),Once  !the freeimage base class
  Include('SaSbarCl.inc'),Once  !Scroll bar class
  Include('SaApiInt.inc'),Once  !Api interfaces
  Include('SaClipCl.inc'),Once  !Clipboard class
  Include('SaGdiCl.inc'),Once   !Gdi primitives, pens, rectangles, lines


GraphicPrimitiveQueue   Queue,Type

                        End

!Image control window border style
         Itemize(0)
CFIBS_NONE          Equate
CFIBS_LINE          Equate
CFIBS_SUNKEN        Equate
CFIBS_SUNKENDEEP    Equate
         End

!Image control scroll bars (bitmap)
CFISB_NONE    Equate(0)
CFISB_VERT    Equate(0001b)
CFISB_HORIZ   Equate(0010b)
CFISB_BOTH    Equate(0011b)

!Image control drawing flags (bitmapped)
CFIDRAW_RESETTHUMBS   EQUATE(01h) !reset the scroll bar thumbs
CFIDRAW_ERASEBKG      EQUATE(02h) !erase the background


!System color to use when no background color is specified
CFI_DEFAULTBKGSYSTEMCOLORINDEX Equate(SA_COLOR_BTNFACE)


iImageControl   Interface,Type
Init            Procedure(Long CtrlFeq=0, UNSIGNED BorderStyle=CFIBS_NONE, UNSIGNED nScrollbars=CFISB_BOTH),BOOL,Proc
Draw            Procedure(UNSIGNED cfiDrawOptions=0)
Reset           Procedure()
Hide            Procedure(BOOL bHide=True)
Show            Procedure(BOOL bShow=True)
GetPosition     Procedure(*SA_RECT rcWindowPos)
SetPosition     Procedure(SA_RECT rcWindowPos)
SetZoomFactor   Procedure(SReal ZoomFactor),BOOL,Proc
GetZoomFactor   Procedure(),SReal
ClientToBitmap  Procedure(*SA_RECT rcDest, *SA_RECT rcSrc),Protected  !convert client rect to bitmap coordinates
ClientToBitmap  Procedure(*SA_POINTS ptsDst, *SA_POINTS ptsSrc),Protected
SetZoomFilter   Procedure(UNSIGNED ZoomFilter)
GetZoomFilter   Procedure(),UNSIGNED
FitTo           Procedure(UNSIGNED FitMethod=CFIFIT_BOTH, UNSIGNED fiFilter=FILTER_BICUBIC)
FitTo           Procedure(*cfiImageControl dstImage, UNSIGNED FitMethod=CFIFIT_BOTH, UNSIGNED fiFilter=FILTER_BICUBIC)
SetBkgColor     Procedure(UNSIGNED BkgColor)
GetBkgColor     Procedure(),UNSIGNED
CopyToClipboard Procedure()
PasteFromClipboard Procedure()
Close           Procedure()
Blank           Procedure()
                End


!=====================================================================================
!=====================================================================================
cfiImageControlClass Class(FreeImageClass), Implements(iImageControl), Implements(iMouse) Type, Thread, Module('cfiImgCt.clw'), Link('cfiImgCt.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)

CtrlFeq         UNSIGNED,Protected !The clarion control to replace with created window
hWndCtrl        SA_HWND,Protected
LogPixelsX      UNSIGNED,Protected
LogPixelsY      UNSIGNED,Protected

BorderStyle     UNSIGNED,Protected !Create a border around control.  See CFIBS_xxxx equates
dwStyle         UNSIGNED,Protected !Additional window styles for createwindowex()
dwExStyle       UNSIGNED,Protected !extended window styles for createwindowex()

hDcMem          SA_HDC,Protected      !Memory device context for double buffering
hBm             SA_HBITMAP,Protected  !handle to device dependant bitmap
DDBitmapInfo    Like(SA_BITMAP),Protected
hBmOld          SA_HBITMAP,Protected  !DDB selected out of hDCMem
hPal            SA_HANDLE,Protected   !handle to the images palette if any
deviceBpp       UNSIGNED,Protected    !bit depth of the display device

BkgColor        SA_COLORREF,Private !Use color equates or any RGB color
hBkgBrush       SA_HBRUSH,Private   !Handle to a brush used to erase the background of the control

hsb             &SA_ScrollBarClass,Protected
vsb             &SA_ScrollBarClass,Protected

SC              &SA_SelectionClass

bResized            BOOL,Private        !Control was resized
bNoResetSBThumbs    BOOL,Protected      !Don't reset the scroll bar thumbs when true

ZoomFactor          SReal,Protected     !Zooming factor
ZoomFilter          UNSIGNED,Protected   !Resampling filter to use while zooming

RegisterClass       Procedure(),Protected
CreateChildWindow   Procedure(),BOOL,Protected
ResetScrollbars     Procedure(BOOL bNoResetSBThumbs=False),Protected
Scrollbars          Procedure(UNSIGNED nScrollbars=CFISB_BOTH),Protected

GetDDBInfo      Procedure(),BOOL,Private,Proc,Protected

Init            Procedure(Long CtrlFeq=0, UNSIGNED BorderStyle=CFIBS_NONE, UNSIGNED nScrollbars=CFISB_BOTH),BOOL,Proc,Virtual,Protected 

InitBuffer      Procedure(),Protected,Virtual !,FINAL
FreeBuffer      Procedure(),Protected,Virtual !,FINAL

Draw            Procedure(UNSIGNED cfiDrawOptions=0),Protected 
Reset           Procedure(),Protected

Hide            Procedure(BOOL bHide=True),Protected
Show            Procedure(BOOL bShow=True),Protected

GetPosition     Procedure(*SA_RECT rcWindowPos),Protected
SetPosition     Procedure(SA_RECT rcWindowPos),Protected

SetZoomFactor   Procedure(SReal ZoomFactor),BOOL,Proc,Protected
GetZoomFactor   Procedure(),SReal,Protected

ClientToBitmap  Procedure(*SA_RECT rcDest, *SA_RECT rcSrc),Protected  !convert client rect to bitmap coordinates
ClientToBitmap  Procedure(*SA_POINTS ptsDst, *SA_POINTS ptsSrc),Protected

SetZoomFilter   Procedure(UNSIGNED ZoomFilter),Protected
GetZoomFilter   Procedure(),UNSIGNED,Protected

FitTo           Procedure(UNSIGNED FitMethod=CFIFIT_BOTH, UNSIGNED fiFilter=FILTER_BICUBIC),Protected
FitTo           Procedure(*cfiImageControl dstImage, UNSIGNED FitMethod=CFIFIT_BOTH, UNSIGNED fiFilter=FILTER_BICUBIC),Protected

SetBkgColor     Procedure(UNSIGNED BkgColor),Protected
GetBkgColor     Procedure(),UNSIGNED,Protected

GetClientAreaNoScrollbars Procedure(*SA_RECT rcClient),Protected

CopyToClipboard     Procedure(),Protected
PasteFromClipboard  Procedure(),Protected

Construct       Procedure(),Protected !,FINAL
Destruct        Procedure(),Protected !,FINAL


OnWm_Paint      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected,Virtual
OnWm_Destroy    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected
OnWm_Size       Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected
OnWm_Move       Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected
OnWm_EraseBkGnd Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected
OnWM_HScroll    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected
OnWM_VScroll    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected

OnWm_SettingChange      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected
OnWM_EnterSizeMove      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected
OnWM_ExitSizeMove       Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected
OnWM_QueryNewPalette    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected
OnWM_PaletteChanged     Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG,Protected

!-------------------
TranslateMouseKey  Procedure(Long wParam),Long !Return clarion mouse key equate
OnMouseButtonDown  Procedure(SA_POINTS mousePos, Long mouseKeycode),Virtual
OnMouseButtonUp    Procedure(SA_POINTS mousePos, Long mouseKeycode),Virtual
OnMouseDoubleClick Procedure(SA_POINTS mousePos, Long mouseKeycode),Virtual
OnMouseMove        Procedure(SA_POINTS mousePos),Virtual

BeginCropSelection Procedure(),Virtual
OnEndCropSelection Procedure(*SA_RECT rcSelection),BOOL,Virtual
OnSelection        Procedure(*SA_RECT rcSelection),Virtual
               End




!=====================================================================================
!=====================================================================================
cfiImageControl Class(cfiImageControlClass),Type,Thread, Module('cfiImgCt.clw'), Link('cfiImgCt.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)
OnSelection         Procedure(),Virtual
                End



!=====================================================================================
!=====================================================================================
cfiSignatureControl  Class(cfiImageControlClass),Type, Thread, Module('cfiImgCt.clw'), Link('cfiImgCt.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)

bDrawingSignature  BOOL,Protected

polyline           &SA_PolylineClass,Protected
iPen               &iGdiPen

OnMouseButtonDown  Procedure(SA_POINTS mousePos, Long mouseKeycode),Virtual
OnMouseButtonUp    Procedure(SA_POINTS mousePos, Long mouseKeycode),Virtual
OnMouseMove        Procedure(SA_POINTS mousePos),Virtual


Construct          Procedure() !,FINAL
Destruct           Procedure() !,FINAL
                End




!=====================================================================================
!=====================================================================================
cfiDrawingControl   Class(cfiImageControlClass),Type, Thread, Module('cfiImgCt.clw'), Link('cfiImgCt.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)
nDrawingObject        Long
penColor              Long
fillColor             Long
penWidth              Long

DrawingObject         Procedure(Long nDrawingObject)
DrawingObject         Procedure(),Long

PenColor              Procedure(Long penColor)
PenColor              Procedure(),Long

FillColor             Procedure(Long fillColor)
FillColor             Procedure(),Long

PenWidth              Procedure(Long penWidth)
PenWidth              Procedure(),Long

StartSelection        Procedure()
OnSelection           Procedure(*SA_RECT rcSelection),Derived
OnMouseButtonDown     Procedure(SA_POINTS mousePos, Long mouseKeycode),Virtual
OnMouseButtonUp       Procedure(SA_POINTS mousePos, Long mouseKeycode),Virtual
Construct             Procedure()
                    End

 _EndOfcfiImageControlClass_