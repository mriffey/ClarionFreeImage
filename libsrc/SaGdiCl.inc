!ABCIncludeFile(SAAPI)
!============================================================
! SaGdiCl.inc -- Api GDI class headers file
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
! 2007.03.21 Restructured GDI Primitives, added rectangle

 OMIT('_EndOfGDIClassInclude_',_SaGDIClassIncluded_)
_SaGDIClassIncluded_ EQUATE(1)

  Include('SaWapi.inc','Defines'),Once  !Windows API
  Include('SaApiInt.inc'),Once          !Interface declarations



   Itemize(0)
CFISELECTACTION_NONE Equate
CFISELECTACTION_CROP Equate    !Crop the image to selection
CFISELECTACTION_SELECT Equate  !just return selection
   End



!-------------------------------------------------------------------------------------
!  See SaApiInt.inc for Interface declarations
!-------------------------------------------------------------------------------------


!=====================================================================================
SA_SelectionClass Class,Implements(iSelection), Implements(iMouse),Type,Thread, |
                        Module('SaGdiCl.clw'), |
                        Link('SaGdiCl.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)

bDrawingSelection   BOOL,Protected
bSelectionActive    BOOL,Protected
ptsBegin            Like(SA_POINTS),Protected
Rect                Like(SA_RECT),Protected
Action              Long,Protected
hBrushSelection     SA_HBRUSH,Protected

RemoveSelection     Procedure(UNSIGNED hWnd)
DrawSelection       Procedure(UNSIGNED hWnd, UNSIGNED hDC, <*SA_RECT rcSelection>),Virtual

Construct           Procedure() !,FINAL
Destruct            Procedure() !,FINAL
                   End


!=====================================================================================
!=====================================================================================
SA_PenClass     Class,Implements(iGdiPen),Type,Thread, Module('SaGdiCl.clw'), Link('SaGdiCl.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)
hPen              SA_HPEN
LogBr             Like(SA_LOGBRUSH)

nPenColor         Long,Protected
nPenWidth         Long,Protected
nPenType          Long,Protected
nPenStyle         Long,Protected
nEndCapStyle      Long,Protected
nJoinStyle        Long,Protected
bUpdateNow        BOOL

UpdatePen         Procedure()

Construct         Procedure() !,FINAL
Destruct          Procedure() !,FINAL
                End


     Itemize(0)
SA_GDI_NONE       Equate
SA_GDI_POLYLINE   Equate
SA_GDI_RECTANGLE  Equate
     End
!=====================================================================================
!=====================================================================================
SA_GdiPrimitiveClass Class,Implements(iGdiPrimitive),Type,Thread, Module('SaGdiCl.clw'), Link('SaGdiCl.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)
DefaultPen        &SA_PenClass
IPen              &iGdiPen
SetPen            Procedure(iGdiPen iPen)
Render            Procedure(Long hDC),Virtual
ObjectType        Procedure(),Long,Virtual
Construct         Procedure()
Destruct          Procedure()

                     End


!=====================================================================================
!=====================================================================================
SA_PolyLineClass  Class(SA_GdiPrimitiveClass),Type,Thread, Module('SaGdiCl.clw'), Link('SaGdiCl.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)
aPts               Like(SA_POINT),Dim(1024)      !Each poly line is limited to 1024 points
nPts               Long
scrollOffset       Like(SA_POINT)

Reset             Procedure()
AddPoint          Procedure(*SA_POINTS Pt)
Render            Procedure(Long hDC),Derived
SetScrollOffset   Procedure(Long x, Long y)
RemoveScrollOffset Procedure()
ObjectType        Procedure(),Long,Derived
Construct         Procedure() !,FINAL
Destruct          Procedure() !,FINAL
                End


!=====================================================================================
!=====================================================================================
SA_RectClass    Class(SA_GdiPrimitiveClass),Type,Thread, Module('SaGdiCl.clw'),Link('SaGdiCl.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)
rcRect           Like(SA_RECT)
fillColor         Long

Reset             Procedure()
Rectangle         Procedure(*SA_Rect rcRect)
Render            Procedure(Long hDC),Derived
ObjectType        Procedure(),Long,Derived

Construct         Procedure() !,FINAL
Destruct          Procedure() !,FINAL
                End



_EndOfGDIClassInclude_