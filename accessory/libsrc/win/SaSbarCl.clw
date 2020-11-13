!============================================================
! SaSbarCl.clw -- Scrollbar Class implementation
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
! Communicate with standard scroll bars of a window.
! NOTE: These are not scroll bar controls.
!
  Member
  INCLUDE('SaSbarCl.inc'),ONCE
  MAP
     INCLUDE('SaWApi.inc','Prototypes'),ONCE
  END


!---------------------------------------------------------------------------------------
SA_ScrollBarClass.Construct          PROCEDURE()
!---------------------------------------------------------------------------------------
  CODE
  SELF.hwnd = 0
  SELF.bSbVisible = True
  Self.bKeepHidden= False
  SA_ZeroMemory(SELF.si, SIZE(SA_SCROLLINFO))
  SELF.sbUpdateFlags = 0
  SELF.Style = SA_SBS_HORZ

  SELF.HSbHeight = SA_GetSystemMetrics(SA_SM_CYHSCROLL)
  SELF.VSbWidth = SA_GetSystemMetrics(SA_SM_CXVSCROLL)

  SELF.nHorzLineSize = 1
  SELF.nVertLineSize = 1

  SELF.CurrentPos = 0

  SELF.WHEEL_DELTA = 120            !should be constant
  SELF.MouseWheelDelta = 0          !Accumulate amount until >= WHEEL_DELTA

  !Get number of lines to scroll per mouse wheel click, default to 3
  SELF.MouseWheelScrollLines = SA_SystemParametersInfo(SA_SPI_GETWHEELSCROLLLINES)
  SELF.MouseWheelScrollLines = CHOOSE(SELF.MouseWheelScrollLines = 0, 3, SELF.MouseWheelScrollLines)



!---------------------------------------------------------------------------------------
SA_ScrollBarClass.Destruct          PROCEDURE()
!---------------------------------------------------------------------------------------
  CODE



!---------------------------------------------------------------------------------------
SA_ScrollBarClass.Init              PROCEDURE(SA_HWND phwnd, UNSIGNED sbStyle=SA_SBS_HORZ)
!---------------------------------------------------------------------------------------
  CODE
  SELF.Style = sbStyle
  If Self.Style = SA_SBS_VERT
    Self.ScrollBar = SA_SB_VERT
  Else
    Self.ScrollBar = SA_SB_HORZ
  End

  SELF.hwnd = phwnd
  Return

!------------------------------------------------------------------------------
SA_ScrollBarClass.SetMin            Procedure(SIGNED nMin)
!---------------------------------------------------------------------------------------
  Code
  SELF.sbUpdateFlags = BOR(SELF.sbUpdateFlags, SA_SIF_RANGE)
  SELF.si.nMin = nMin
  Return


!------------------------------------------------------------------------------
SA_ScrollBarClass.GetMin            Procedure()!,SIGNED
!---------------------------------------------------------------------------------------
  Code
  SELF.si.cbSize = SIZE(SELF.si)
  SELF.si.fMask = SA_SIF_RANGE
  IF SA_GetScrollInfo(SELF.hwnd, Self.ScrollBar, SELF.si) = 0
      !GetLastError()
  END
  Return SELF.si.nMin


!------------------------------------------------------------------------------
SA_ScrollBarClass.SetMax            Procedure(SIGNED nMax)
!---------------------------------------------------------------------------------------
  Code
  SELF.sbUpdateFlags = BOR(SELF.sbUpdateFlags, SA_SIF_RANGE)
  SELF.si.nMax = nMax
  Return


!------------------------------------------------------------------------------
SA_ScrollBarClass.GetMax            Procedure()!,SIGNED
!---------------------------------------------------------------------------------------
  Code
  SELF.si.cbSize = SIZE(SELF.si)
  SELF.si.fMask = SA_SIF_RANGE
  IF SA_GetScrollInfo(SELF.hwnd, Self.ScrollBar, SELF.si) = 0
      !GetLastError()
  END
  Return SELF.si.nMax


!------------------------------------------------------------------------------
SA_ScrollBarClass.SetPageSize       Procedure(UNSIGNED nPage)
  Code
  SELF.sbUpdateFlags = BOR(SELF.sbUpdateFlags, SA_SIF_PAGE)
  SELF.si.nPage = nPage
  Return


!------------------------------------------------------------------------------
SA_ScrollBarClass.GetPageSize       Procedure()!,UNSIGNED
!---------------------------------------------------------------------------------------
  Code
  SELF.si.cbSize = SIZE(SELF.si)
  SELF.si.fMask = SA_SIF_PAGE
  If SA_GetScrollInfo(SELF.hwnd, Self.ScrollBar, SELF.si).
  Return SELF.si.nPage

                  

!------------------------------------------------------------------------------
SA_ScrollBarClass.SetThumbPos       Procedure(SIGNED nPos, BOOL bRedraw=False)
!---------------------------------------------------------------------------------------
si  LIKE(SA_SCROLLINFO)
  Code
  IF bRedraw
    si.cbSize = SIZE(si)
    si.fMask = SA_SIF_POS
    si.nPos = nPos
    SELF.SetScrollInfo(si, bRedraw)
  ELSE !Defer the drawing until the update method is called
    SELF.sbUpdateFlags = BOR(SELF.sbUpdateFlags, SA_SIF_POS)
    SELF.si.nPos = nPos
  END
  Return 



!------------------------------------------------------------------------------
SA_ScrollBarClass.GetThumbPos       Procedure()!,SIGNED SIF_POS
!---------------------------------------------------------------------------------------
  Code
  If Not Self.bSbVisible Then Return 0.
  SELF.si.cbSize = SIZE(SELF.si)
  SELF.si.fMask = SA_SIF_POS
  IF SA_GetScrollInfo(SELF.hwnd, Self.ScrollBar, SELF.si) = 0
      !GetLastError()
      SELF.si.nPos = -1
  END
  Return SELF.si.nPos


!------------------------------------------------------------------------------
SA_ScrollBarClass.GetTrackPos Procedure()!,SIGNED
!---------------------------------------------------------------------------------------
  CODE
  If Not Self.bSbVisible Then Return 0.
  SELF.si.cbSize = SIZE(SELF.si)
  SELF.si.fMask = SA_SIF_TRACKPOS
  IF SA_GetScrollInfo(SELF.hwnd, Self.ScrollBar, SELF.si) = 0
      !GetLastError()
  END
  Return SELF.si.nTrackPos


!------------------------------------------------------------------------------
SA_ScrollBarClass.Update       Procedure(BOOL bRedraw=True)!,BOOL
!---------------------------------------------------------------------------------------
RetCde  BOOL,AUTO
  Code
  RetCde = True
  IF SELF.sbUpdateFlags
    SELF.si.cbSize = SIZE(SELF.si)
    SELF.si.fMask = SELF.sbUpdateFlags
    If SELF.SetScrollInfo(SELF.si, bRedraw).

    !Now read the scrollinfo back because Windows may change the setting slightly
    SELF.si.cbSize = SIZE(SELF.si)
    SELF.si.fMask = SA_SIF_ALL
    If SA_GetScrollInfo(SELF.hwnd, Self.ScrollBar, SELF.si).
    SELF.sbUpdateFlags = 0
  END
  Return RetCde


!------------------------------------------------------------------------------
SA_ScrollBarClass.Disable           PROCEDURE(UNSIGNED sbArrows=SA_ESB_DISABLE_BOTH)
!------------------------------------------------------------------------------
! ESB_DISABLE_BOTH
!   Disables both arrows on a scroll bar.
! ESB_DISABLE_DOWN
!   Disables the down arrow on a vertical scroll bar.
! ESB_DISABLE_LEFT
!   Disables the left arrow on a horizontal scroll bar.
! ESB_DISABLE_LTUP
!   Disables the left arrow on a horizontal scroll bar
!   or the up arrow of a vertical scroll bar.
! ESB_DISABLE_RIGHT
!   Disables the right arrow on a horizontal scroll bar.
! ESB_DISABLE_RTDN
!   Disables the right arrow on a horizontal scroll bar
!   or the down arrow of a vertical scroll bar.
! ESB_DISABLE_UP
!   Disables the up arrow on a vertical scroll bar.
! ESB_ENABLE_BOTH
!   Enables both arrows on a scroll bar.
!------------------------------------------------------------------------------

  CODE
  !-----------------------------------------------------------------
  ! The ESB_ENABLE_BOTH flag does not enable the scroll bar button(s)
  ! under common control version 6.
  !
  ! When disabling arrows it's necessary to redraw the scroll bar.
  ! Updating the position will cause the scrollbar to repaint and
  ! enable the buttons.
  !
  ! *Updating the thumb position in code always enables the scrollbar
  ! buttons, even if you do not call this method to enable the scrollbars
  ! first.
  !-----------------------------------------------------------------
  !If sbArrows <> ESB_ENABLE_BOTH
  !  EnableScrollBar(SELF.hwndScrollBar, SB_CTL, ESB_ENABLE_BOTH)
  !End

  SELF.si.cbSize = SIZE(SELF.si)
  SELF.si.fMask = SELF.sbUpdateFlags
  IF SELF.SetScrollInfo(SELF.si, True) = 0
  END

  SA_EnableScrollBar(SELF.hwnd, Self.ScrollBar, sbArrows)

  Return


!---------------------------------------------------------------------------------------
SA_ScrollBarClass.Hide              PROCEDURE(BOOL bHideScrollBar=True)
!---------------------------------------------------------------------------------------
  CODE
  Self.Show(CHOOSE(bHideScrollBar, False, True))
  Return

!---------------------------------------------------------------------------------------
SA_ScrollBarClass.Show              PROCEDURE(BOOL bShowScrollBar=True)
!---------------------------------------------------------------------------------------
  CODE
  If Self.bKeepHidden Then bShowScrollbar = False.

  SELF.bSbVisible = bShowScrollBar
  IF SA_ShowScrollBar(SELF.hwnd, Self.ScrollBar, SELF.bSbVisible) = 0
    !
  END
  Return

!---------------------------------------------------------------------------------------
SA_ScrollBarClass.IsVisible         Procedure()!,Bool
!---------------------------------------------------------------------------------------
  Code
  Return SELF.bSbVisible

!---------------------------------------------------------------------------------------
SA_ScrollBarClass.GetWidth          Procedure()!,UNSIGNED
!---------------------------------------------------------------------------------------
  Code
  Return SELF.VSbWidth

!---------------------------------------------------------------------------------------
SA_ScrollBarClass.GetHeight         Procedure()!,UNSIGNED
!---------------------------------------------------------------------------------------
  Code
  Return SELF.HSbHeight

!---------------------------------------------------------------------------------------
SA_ScrollBarClass.SetScrollInfo     Procedure(*SA_SCROLLINFO si, BOOL bRedraw=False)
!---------------------------------------------------------------------------------------
RetCde  UNSIGNED,AUTO
  Code
  RetCde = SA_SetScrollInfo(SELF.Hwnd, Self.ScrollBar, si, bRedraw)
  IF RetCde = 0
    !Throw Error
  END
  Return RetCde


!---------------------------------------------------------------------------------------
SA_ScrollBarClass.GetScrollInfo     Procedure(*SA_SCROLLINFO si)
!---------------------------------------------------------------------------------------
RetCde  UNSIGNED,AUTO
  Code
  RetCde = SA_GetScrollInfo(SELF.Hwnd, Self.ScrollBar, si)
  IF RetCde = 0
    !Error
  ELSE
    SELF.si = si
  END

  Return RetCde



!---------------------------------------------------------------------------------------
SA_ScrollBarClass.OnWM_HScroll  Procedure( Long hWnd, UNSIGNED uMsg, Long wParam, Long lParam)
!---------------------------------------------------------------------------------------
SB  GROUP,OVER(wParam)
Msg     USHORT
Pos     USHORT
    END
NewThumbPos UNSIGNED,AUTO
CurrentThumbPos  UNSIGNED,AUTO
  Code
  SELF.si.fMask = SA_SIF_ALL
  SELF.GetScrollInfo(SELF.si)
  CurrentThumbPos = SELF.si.nPos
  Case SB.Msg
    Of SA_SB_LEFT
      NewThumbPos = SELF.si.nMin
    Of SA_SB_RIGHT
      NewThumbPos = SELF.si.nMax
    Of SA_SB_LINELEFT
      NewThumbPos = CurrentThumbPos - SELF.nHorzLineSize
    Of SA_SB_LINERIGHT
      NewThumbPos = CurrentThumbPos + SELF.nHorzLineSize
    Of SA_SB_PAGELEFT
      NewThumbPos = CurrentThumbPos - SELF.si.nPage
    Of SA_SB_PAGERIGHT
      NewThumbPos = CurrentThumbPos + SELF.si.nPage
    Of SA_SB_THUMBTRACK OrOf SA_SB_THUMBPOSITION
      NewThumbPos = SB.Pos
  ELSE
    Return 0
  END
  SELF.SetThumbPos(NewThumbPos, False)
  SELF.Update(True)

  Return 1


!---------------------------------------------------------------------------------------
SA_ScrollBarClass.OnWM_VScroll  Procedure(SA_HWND hWnd, UNSIGNED uMsg, LONG wParam, LONG lParam)
!---------------------------------------------------------------------------------------
SB  GROUP,OVER(wParam)
Msg     USHORT
Pos     USHORT
    END
NewThumbPos UNSIGNED,AUTO
CurrentThumbPos  UNSIGNED,AUTO
  Code
  SELF.si.fMask = SA_SIF_ALL
  SELF.GetScrollInfo(SELF.si)
  CurrentThumbPos = SELF.si.nPos
  Case SB.Msg
    Of SA_SB_TOP
      NewThumbPos = SELF.si.nMin
    Of SA_SB_BOTTOM
      NewThumbPos = SELF.si.nMax
    Of SA_SB_LINEUP
      NewThumbPos = CurrentThumbPos - SELF.nVertLineSize
    Of SA_SB_LINEDOWN
      NewThumbPos = CurrentThumbPos + SELF.nVertLineSize
    Of SA_SB_PAGEUP
      NewThumbPos = CurrentThumbPos - SELF.si.nPage
    Of SA_SB_PAGEDOWN
      NewThumbPos = CurrentThumbPos + SELF.si.nPage
    Of SA_SB_THUMBTRACK OrOf SA_SB_THUMBPOSITION
      NewThumbPos = SB.Pos
  ELSE
    Return 0
  END
  SELF.SetThumbPos(NewThumbPos, False)
  SELF.Update(True)

  Return 1


!---------------------------------------------------------------------------------------
SA_ScrollBarClass.iMouse.OnMouseWheel Procedure(LONG hWnd, UNSIGNED uMsg, LONG wParam, LONG lParam)
                                      !,Long,VIRTUAL
!---------------------------------------------------------------------------------------
wp GROUP,OVER(wParam)
wplow  SHORT
wpHigh SHORT
   END
lp GROUP,OVER(lParam)
lpLow  USHORT
lpHigh USHORT
   END

MwWparam LONG
mw GROUP,OVER(MwWParam)
SBMessage    USHORT
High         USHORT
   END

  CODE
  If wp.wpHigh > 0
    If Self.Scrollbar = SA_SB_VERT
      mw.SBMessage = SA_SB_LINEUP    
    Else
      mw.SBMessage = SA_SB_LEFT
    End
  Else
    If Self.Scrollbar = SA_SB_VERT
      mw.SBMessage = SA_SB_LINEDOWN
    Else
      mw.SBMessage = SA_SB_RIGHT
    End
    wp.wpHigh = -wp.wpHigh
  End
  SELF.MouseWheelDelta += wp.wpHigh
  If SELF.MouseWheelDelta >= SELF.WHEEL_DELTA
    LOOP SELF.MouseWheelDelta * SELF.MouseWheelScrollLines / SELF.WHEEL_DELTA TIMES
      SELF.MouseWheelDelta = 0
      If Self.Scrollbar = SA_SB_VERT
        If SELF.OnWm_VScroll(hwnd, uMsg, MwWParam, lParam).
      Else
        If SELF.OnWm_HScroll(hwnd, uMsg, MwWParam, lParam).
      End
    END
  Else
    SELF.MouseWheelDelta += wp.wpHigh
  End

  RETURN 1

SA_ScrollBarClass.iMouse.OnMouseMove      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0

SA_ScrollBarClass.iMouse.OnLButtonDown    Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0

SA_ScrollBarClass.iMouse.OnLButtonUp      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0

SA_ScrollBarClass.iMouse.OnRButtonDown    Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0

SA_ScrollBarClass.iMouse.OnRButtonUp      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0


SA_ScrollBarClass.iMouse.OnLButtonDblClk  Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_ScrollBarClass.iMouse.OnMButtonDblClk      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_ScrollBarClass.iMouse.OnMButtonDown      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_ScrollBarClass.iMouse.OnMButtonUp      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_ScrollBarClass.iMouse.OnRButtonDblClk      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_ScrollBarClass.iMouse.OnXButtonDblClk      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_ScrollBarClass.iMouse.OnXButtonDown      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_ScrollBarClass.iMouse.OnXButtonUp      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0

SA_ScrollBarClass.iMouse.OnMouseHover      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0
SA_ScrollBarClass.iMouse.OnMouseLeave      Procedure(SA_HWND hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
  Code
  Return 0



