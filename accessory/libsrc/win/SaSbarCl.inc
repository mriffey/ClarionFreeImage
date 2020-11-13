!ABCIncludeFile(SAAPI)
!============================================================
! SaSbarCl.inc -- Scrollbar class header file
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
!
! Note: these are not scrollbar controls.  This class communicates
!       with a window's builtin scrollbar class.
!
!============================================================
 OMIT('_EndOfScrollBarClass_',_ScrollBarClassIncluded_)
_ScrollBarClassIncluded_  EQUATE(1)
 
  Include('saWApi.inc','Defines'),Once
  Include('SaApiInt.inc'),Once

SA_ScrollBarClass  Class,Implements(iMouse),Type,Thread|
                      Module('SaSbarCl.clw'), |
                      Link('SaSbarCl.clw',_SALinkMode_), DLL(_SADLLMode_)

si                  Like(SA_SCROLLINFO)

hwnd                SA_HWND

HSbHeight           UNSIGNED
VSbWidth            UNSIGNED

nHorzLineSize       UNSIGNED
nVertLineSize       UNSIGNED
bSbVisible          Bool

sbUpdateFlags       UNSIGNED

Style               UNSIGNED  !SA_SBS_HORIZ or SA_SBS_VERT
ScrollBar           UNSIGNED  !SA_SB_VERT or SA_SB_HORZ
bKeepHidden         BOOL      !When true never unhide the scrollbar

CurrentPos          UNSIGNED

WHEEL_DELTA         UNSIGNED !120
MouseWheelDelta     UNSIGNED
MouseWheelScrollLines UNSIGNED



Init              Procedure(UNSIGNED phwnd, UNSIGNED sbStyle=SA_SB_HORZ)
SetMin            Procedure(SIGNED nMin)
GetMin            Procedure(),SIGNED

SetMax            Procedure(SIGNED nMax)
GetMax            Procedure(),SIGNED

SetPageSize       Procedure(UNSIGNED nPage)
GetPageSize       Procedure(),UNSIGNED

SetThumbPos       Procedure(SIGNED nPos, BOOL bRedraw=False)
GetThumbPos       Procedure(),SIGNED

GetTrackPos       PROCEDURE(),SIGNED

Update            Procedure(BOOL bRedraw=True),BOOL,Proc

Disable           Procedure(UNSIGNED Arrows=SA_ESB_DISABLE_BOTH)
Hide              Procedure(BOOL bHideScrollBar=True)
Show              Procedure(BOOL bShowScrollbar=True)

IsVisible         Procedure(),BOOL

GetWidth          Procedure(),UNSIGNED
GetHeight         Procedure(),UNSIGNED

SetScrollInfo     Procedure(*SA_SCROLLINFO si, BOOL bRedraw=False),UNSIGNED,Proc

GetScrollInfo     Procedure(*SA_SCROLLINFO si),UNSIGNED,Proc


OnWM_HScroll      Procedure(UNSIGNED hWnd, UNSIGNED uMsg, LONG wParam, LONG lParam  ),BOOL,VIRTUAL

OnWM_VScroll      Procedure(UNSIGNED hWnd, UNSIGNED uMsg, LONG wParam, LONG lParam  ),BOOL,VIRTUAL



Construct         Procedure()
Destruct          Procedure()
                END

!_EndOfScrollBarClass_