!ABCIncludeFile(SAAPI)
!============================================================
! SaApiInt.inc -- Windows API interfaces
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
OMIT('_EndOfSaApiInterfaces_',_SaApiInterfacesIncluded_)
_SaApiInterfacesIncluded_  EQUATE(1)

!=====================================================================================
iSelection   Interface,Type
SetAction        Procedure(Long Action)
GetAction        Procedure(),Long
GetRect          Procedure(*SA_RECT rcSelection)
OnSelection      Procedure(),Virtual
            End

!=====================================================================================
iMouse      Interface,Type
OnLButtonDblClk  Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG
OnLButtonDown    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG
OnLButtonUp      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG

OnMButtonDblClk  Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG
OnMButtonDown    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG
OnMButtonUp      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG

OnRButtonDblClk  Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG
OnRButtonDown    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG
OnRButtonUp      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG

OnXButtonDblClk  Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG
OnXButtonDown    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG
OnXButtonUp      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG

OnMouseWheel     Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG
OnMouseMove      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG

!posted after trackmouseevent() api function is called
OnMouseHover     Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG
OnMouseLeave     Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam),LONG

            End


!=====================================================================================

iGdiPen         Interface
CreatePen         Procedure()
SetPenColor       Procedure(Long nPenColor=Color:Black)
GetPenColor       Procedure(),Long
SetPenWidth       Procedure(Long nPenWidth=1)
GetPenWidth       Procedure(),Long
SetPenStyle       Procedure(Long nPenStyle=SA_PS_SOLID)
GetPenStyle       Procedure(),Long
SetPenType        Procedure(Long nPenType=SA_PS_GEOMETRIC)
GetPenType        Procedure(),Long
SetPenJoinStyle   Procedure(Long nJoinSytle=SA_PS_JOIN_BEVEL)
GetPenJoinStyle   Procedure(),Long
SetPenBrush       Procedure(*SA_LOGBRUSH lb)
GetPenBrush       Procedure(*SA_LOGBRUSH lb)
GetPen            Procedure(),Long ! returns handle to current pen
                End

!=====================================================================================
iGdiPrimitive   Interface
Render            Procedure(Long hDC)

                End

!_EndOfSaApiInterfaces_