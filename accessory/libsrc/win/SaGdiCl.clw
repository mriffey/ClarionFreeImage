 !============================================================
! SaGdiCl.clw -- Api GDI class Implementations
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

  MEMBER()

  INCLUDE('SaGdiCl.inc'),Once

  MAP
    INCLUDE('SaWApi.inc','Prototypes'),ONCE
  End


!==============================================================================
!==============================================================================
!==============================================================================

!-----------------------------------------------------------------------------------------------
SA_SelectionClass.Construct Procedure()
!-----------------------------------------------------------------------------------------------
SelectionLogBrush Like(SA_LOGBRUSH)
  Code
  Self.bDrawingSelection = False
  Self.bSelectionActive  = False
  SA_ZeroMemory(Self.Rect, Size(Self.Rect))
  Self.Action = CFISELECTACTION_NONE
  SelectionLogBrush.lbStyle = SA_BS_HATCHED
  SelectionLogBrush.lbColor = COLOR:Black
  SelectionLogBrush.lbHatch = SA_HS_DIAGCROSS
  Self.hBrushSelection = SA_CreateBrushIndirect(SelectionLogBrush)
  Return

!-----------------------------------------------------------------------------------------------
SA_SelectionClass.Destruct Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  If Self.hBrushSelection <> 0
    SA_DeleteObject(Self.hBrushSelection)
  End

  Return


!-----------------------------------------------------------------------------------------------
SA_SelectionClass.ISelection.SetAction    Procedure(Long Action)
!-----------------------------------------------------------------------------------------------
  Code
  Self.Action = Action
  If Self.Action = CFISELECTACTION_NONE
    Self.bSelectionActive = False
    Clear(Self.Rect)
  End
  Return

!-----------------------------------------------------------------------------------------------
SA_SelectionClass.ISelection.GetAction    Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.Action


!-----------------------------------------------------------------------------------------------
SA_SelectionClass.ISelection.GetRect      Procedure(*SA_RECT rcSelection)
!-----------------------------------------------------------------------------------------------
  Code
  rcSelection = Self.Rect
  Return

!-----------------------------------------------------------------------------------------------
SA_SelectionClass.ISelection.OnSelection  Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Return


!-----------------------------------------------------------------------------------------------
SA_SelectionClass.RemoveSelection     Procedure(UNSIGNED hWnd)
!-----------------------------------------------------------------------------------------------
  Code
  If Self.bSelectionActive = True  !Erase any old selection
    Self.DrawSelection(hWnd, 0, Self.Rect)
    Self.bSelectionActive = False
    Clear(Self.Rect)
  End
  Return


!-----------------------------------------------------------------------------------------------
SA_SelectionClass.DrawSelection       Procedure(UNSIGNED hWnd, SA_HDC hDC=0, <*SA_RECT rcSelection>)
!-----------------------------------------------------------------------------------------------
bReleaseHDC BOOL,Auto
rcThisSelection Like(SA_RECT)
  Code
  If Self.Action = CFISELECTACTION_NONE Then Return.

  bReleaseHDC = False
  If Omitted(4)
    rcThisSelection = Self.Rect
  Else
    rcThisSelection = rcSelection
  End

  If hDC = 0
    hDC = SA_GetDc(hWnd)
    bReleaseHDC = True
  End
  SA_DrawFocusRect(hDC, rcThisSelection)
  If bReleaseHDC = True
    SA_ReleaseDC(hWnd, hDC)
  End
  Return
                     



!-----------------------------------------------------------------------------------------------
SA_SelectionClass.iMouse.OnLButtonDown  Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!-----------------------------------------------------------------------------------------------
mPos LIKE(SA_POINTS),OVER(lParam)
rcClient LIKE(SA_RECT)
hDC  SA_HDC
  CODE
  If Self.Action = CFISELECTACTION_NONE Then Return 0.

  If Self.bSelectionActive = True  !Erase any old selection
    Self.DrawSelection(hWnd, 0, Self.Rect)
  End

  !Ensure all mouse input goes to this control
  SA_SetCapture(hWnd)

  !Get the client coordinates from the object
  SA_GetWindowRect(hWnd, rcClient)

  !Clip the mouse cursor to the bounding rectangle
  SA_ClipCursor(rcClient)

  Self.ptsBegin = mPos !current mouse position
  
  Self.bDrawingSelection = False
  Self.bSelectionActive = False

  Return 0



!-----------------------------------------------------------------------------------------------
SA_SelectionClass.iMouse.OnMouseMove    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!-----------------------------------------------------------------------------------------------
wHdc    SA_HDC
mPos    LIKE(SA_POINTS),OVER(lParam)
  CODE
  If Self.Action = CFISELECTACTION_NONE Then Return 0.

  IF BAND(wParam, SA_MK_LBUTTON) !Left mouse button is held down

    wHdc = SA_GetDc(hWnd)

    !redraw the old rectangle to erase it (xor)
    IF SELF.bDrawingSelection
      Self.DrawSelection(hWnd, wHdc)
    END

    ! First ensure that it's a valid rectangle
    ! with upper left and lower right coordinates.  This allows
    ! the user to click and then drag to any quadrant to form the rectangle
    IF  mPos.x > SELF.ptsBegin.x AND mPos.y < SELF.ptsBegin.y
      SA_SetRect(Self.Rect, SELF.ptsBegin.X, mPos.y, mPos.x, SELF.ptsBegin.Y)

    ELSIF mPos.x <  SELF.ptsBegin.x  AND mPos.y < SELF.ptsBegin.y
      SA_SetRect(Self.Rect, mPos.x, mPos.y, SELF.ptsBegin.x, SELF.ptsBegin.Y)

    ELSIF mPos.x < SELF.ptsBegin.x   AND mPos.y > SELF.ptsBegin.y
      SA_SetRect(Self.Rect, mPos.x,  SELF.ptsBegin.Y, SELF.ptsBegin.x, mPos.y)

    ELSE
      SA_SetRect(Self.Rect, SELF.ptsBegin.X, SELF.ptsBegin.Y, mPos.x, mPos.y)
    END

    !Draw the new rectangle
    Self.DrawSelection(hWnd, wHdc)
    SA_ReleaseDC(hWnd, wHdc)
    SELF.bDrawingSelection = True
  END

  Return 0


!-----------------------------------------------------------------------------------------------
SA_SelectionClass.iMouse.OnLButtonUp    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!-----------------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(lParam)
  CODE
  If Self.Action = CFISELECTACTION_NONE Then Return 0.

  IF Self.bDrawingSelection
    Self.bSelectionActive = True
    Self.ISelection.OnSelection()
    Self.bDrawingSelection = False
  END

  SA_ClipCursor()
  SA_ReleaseCapture()
  If SA_IsRectEmpty(Self.Rect)
    Self.bSelectionActive = False
    Self.bDrawingSelection = False
  End

  Return 0


SA_SelectionClass.iMouse.OnRButtonDown    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnRButtonUp      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnMouseWheel     Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnLButtonDblClk  Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnMButtonDblClk      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnMButtonDown      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnMButtonUp      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnRButtonDblClk      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnXButtonDblClk      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnXButtonDown      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnXButtonUp      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnMouseHover      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_SelectionClass.iMouse.OnMouseLeave      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0



!==============================================================================
!==============================================================================
!==============================================================================
SA_PenClass.iGdiPen.CreatePen         Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  If Self.hPen <> 0
    SA_DeleteObject(Self.hPen)
  End
  Self.hpen = SA_ExtCreatePen(Self.nPenType + Self.nPenStyle + Self.nEndCapStyle + Self.nJoinStyle, |
                              Self.nPenWidth, Self.LogBr, 0, )
  Return 

!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.SetPenColor       Procedure(Long nPenColor=Color:Black)
!-----------------------------------------------------------------------------------------------
  Code
  Self.nPenColor = nPenColor
  Self.LogBr.lbColor = Self.nPenColor
  If Self.bUpdateNow = True
    Self.iGdiPen.CreatePen()
  End
  Return

!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.GetPenColor       Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.nPenColor

!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.SetPenWidth       Procedure(Long nPenWidth=1)
!-----------------------------------------------------------------------------------------------
  Code
  Self.nPenWidth = Choose(nPenWidth >= 0, nPenWidth, 1)
  If Self.bUpdateNow = True
    Self.iGdiPen.CreatePen()
  End
  Return

!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.GetPenWidth       Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.nPenWidth

!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.SetPenStyle       Procedure(Long nPenStyle=SA_PS_SOLID)
!-----------------------------------------------------------------------------------------------
  Code
  Self.nPenStyle = nPenStyle
  If Self.bUpdateNow = True
    Self.iGdiPen.CreatePen()
  End
  Return

!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.GetPenStyle       Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.nPenStyle


!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.SetPenType        Procedure(Long nPenType=SA_PS_GEOMETRIC)
!-----------------------------------------------------------------------------------------------
  Code
  Self.nPenType = Choose(nPenType <= SA_PS_GEOMETRIC, nPenType, SA_PS_GEOMETRIC)
  If Self.bUpdateNow = True
    Self.iGdiPen.CreatePen()
  End
  Return


!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.GetPenType        Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.nPenType


!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.SetPenJoinStyle   Procedure(Long nJoinStyle=SA_PS_JOIN_BEVEL)
!-----------------------------------------------------------------------------------------------
  Code
  Self.nJoinStyle = nJoinStyle
  If Self.bUpdateNow = True
    Self.iGdiPen.CreatePen()
  End
  Return

!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.GetPenJoinStyle   Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.nJoinStyle


!-----------------------------------------------------------------------------------------------
SA_PenClass.UpdatePen        Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Self.iGdiPen.CreatePen()
  Return


!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.GetPen       Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.hPen

!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.SetPenBrush       Procedure(*SA_LOGBRUSH lb)
!-----------------------------------------------------------------------------------------------
  Code
  Self.LogBr = lb
  If Self.bUpdateNow = True
    Self.iGdiPen.CreatePen()
  End
  Return

!-----------------------------------------------------------------------------------------------
SA_PenClass.iGdiPen.GetPenBrush       Procedure(*SA_LOGBRUSH lb)
!-----------------------------------------------------------------------------------------------
  Code
  lb = Self.LogBr
  Return 

!-----------------------------------------------------------------------------------------------
SA_PenClass.Construct         Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Self.nPenStyle = SA_PS_SOLID
  Self.nPenColor = Color:Black
  Self.nPenWidth = 1
  Self.nEndCapStyle = SA_PS_ENDCAP_ROUND !SA_PS_ENDCAP_ROUND ! SA_PS_ENDCAP_FLAT  SA_PS_ENDCAP_SQUARE
  Self.nPenType = SA_PS_GEOMETRIC        ! SA_PS_COSMETIC
  Self.nJoinStyle = SA_PS_JOIN_BEVEL

  Self.LogBr.lbStyle = SA_BS_SOLID
  Self.LogBr.lbColor = Self.nPenColor
  Self.LogBr.lbHatch = 0
  Self.bUpdateNow = True

  Self.iGdiPen.CreatePen()  !create the default pen

  Return

!-----------------------------------------------------------------------------------------------
SA_PenClass.Destruct          Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  SA_DeleteObject(Self.hPen)
  Return







!==============================================================================
!===============     SA_GdiPrimitiveClass        ==============================
!==============================================================================

!-----------------------------------------------------------------------------------------------
SA_GdiPrimitiveClass.iGdiPrimitive.Render   Procedure(Long hDC)
!-----------------------------------------------------------------------------------------------
  Code
  If Self.iPen &= Null
    Self.iPen &= Self.DefaultPen.iGdiPen
    If Self.iPen &= Null
      Return
    End
  End

  Self.Render(hDC)
  Return

!-----------------------------------------------------------------------------------------------
SA_GdiPrimitiveClass.Render   Procedure(Long hDC)
!-----------------------------------------------------------------------------------------------
  Code

  Return

!-----------------------------------------------------------------------------------------------
SA_GdiPrimitiveClass.SetPen            Procedure(iGdiPen iPen)
!-----------------------------------------------------------------------------------------------
  Code
  Self.iPen &= iPen
  If Self.iPen &= Null
    Assert(false)!null pen
  End
  Return

!-----------------------------------------------------------------------------------------------
SA_GdiPrimitiveClass.ObjectType           Procedure()!,Long,Virtual
 !-----------------------------------------------------------------------------------------------
  Code
  Return SA_GDI_NONE

 !-----------------------------------------------------------------------------------------------
SA_GdiPrimitiveClass.Construct         Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Self.defaultPen &= New SA_PenClass
  Self.iPen &= Self.DefaultPen.iGdiPen
  Return

!-----------------------------------------------------------------------------------------------
SA_GdiPrimitiveClass.Destruct          Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Self.iPen &= Null
  Dispose(Self.defaultPen)
  Return





!==============================================================================
!=====================         SA_PolyLineClass     ===========================
!==============================================================================

!-----------------------------------------------------------------------------------------------
SA_PolyLineClass.Render            Procedure(Long hDC)
!-----------------------------------------------------------------------------------------------
hOldPen SA_HPEN
  Code
  If Self.nPts > 1 !need at least two points for a polyline
    If hDC <> 0
      hOldPen = SA_SelectObject(hDC, Self.iPen.GetPen())
      SA_Polyline(hDC, Self.aPts[1], Self.nPts)
      SA_SelectObject(hDC, hOldPen)
    End
  End
  Return




!-----------------------------------------------------------------------------------------------
SA_PolyLineClass.Reset          Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Clear(Self.aPts)
  Self.nPts = 0
  Clear(Self.scrollOffset)
  Return

!-----------------------------------------------------------------------------------------------
SA_PolyLineClass.AddPoint          Procedure(*SA_POINTS Pt)
!-----------------------------------------------------------------------------------------------
  Code
  If Self.nPts < Maximum(Self.aPts, 1)
    Self.nPts += 1
    Self.aPts[Self.nPts].x = Pt.x - Self.scrollOffset.x
    Self.aPts[Self.nPts].y = Pt.y - Self.scrollOffset.y
  End
  Return


!-----------------------------------------------------------------------------------------------
SA_PolyLineClass.SetScrollOffset   Procedure(Long x, Long y)
!-----------------------------------------------------------------------------------------------
  Code
  Self.scrollOffset.x = x
  Self.scrollOffset.y = y
  Return


!-----------------------------------------------------------------------------------------------
SA_PolyLineClass.RemoveScrollOffset Procedure()
!-----------------------------------------------------------------------------------------------
i UNSIGNED,Auto
  Code
    Loop i = 1 to Self.nPts
      Self.aPts[i].x += Self.scrollOffset.x
      Self.aPts[i].y += Self.scrollOffset.y
    End
  Return


!-----------------------------------------------------------------------------------------------
SA_PolyLineClass.ObjectType           Procedure()!,Long,Virtual
 !-----------------------------------------------------------------------------------------------
  Code
  Return SA_GDI_POLYLINE

!-----------------------------------------------------------------------------------------------
SA_PolyLineClass.Construct         Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Self.Reset()
  Return

!-----------------------------------------------------------------------------------------------
SA_PolyLineClass.Destruct          Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Return







!===============================================================================================
!=============================         SA_RectClass         ====================================
!===============================================================================================
! New primitive Rect 2007.03.20

!-----------------------------------------------------------------------------------------------
SA_RectClass.Render            Procedure(Long hDC)
!-----------------------------------------------------------------------------------------------
hOldPen SA_HPEN
hBrush       SA_HBRUSH
hOldBrush    SA_HBRUSH
lb           Like(SA_LOGBRUSH)
  Code

  !Fill color for rectangle
  Case Self.fillColor
  Of Color:None
    lb.lbStyle = SA_BS_NULL
  Else
    lb.lbStyle = SA_BS_SOLID
  End
  lb.lbColor = Self.fillColor
  lb.lbHatch = 0 !ignored for solid and hollow
  hBrush = SA_CreateBrushIndirect(lb)



  If hDC <> 0
    hOldBrush= SA_SelectObject(hDC, hBrush)
    hOldPen = SA_SelectObject(hDC, Self.iPen.GetPen())
    SA_Rectangle(hDC, Self.rcRect.left, Self.rcRect.top, Self.rcRect.right, Self.rcRect.bottom)
    SA_SelectObject(hDC, hOldPen)
    SA_SelectObject(hDC, hOldBrush)
  End
  If hBrush <>0
    SA_DeleteObject(hBrush)
  End
  Return

!-----------------------------------------------------------------------------------------------
SA_RectClass.Rectangle         Procedure(*SA_Rect rcRect)
!-----------------------------------------------------------------------------------------------
  Code
  Self.rcRect = rcRect
  Return


!-----------------------------------------------------------------------------------------------
SA_RectClass.Reset             Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Return

!-----------------------------------------------------------------------------------------------
SA_RectClass.ObjectType           Procedure()!,Long,Derived
!-----------------------------------------------------------------------------------------------
  Code
  Return SA_GDI_RECTANGLE


!-----------------------------------------------------------------------------------------------
SA_RectClass.Construct         Procedure() !,FINAL
!-----------------------------------------------------------------------------------------------
  Code
  Self.fillColor = Color:None
  Return


!-----------------------------------------------------------------------------------------------
SA_RectClass.Destruct          Procedure() !,FINAL
!-----------------------------------------------------------------------------------------------
  Code
  Return

