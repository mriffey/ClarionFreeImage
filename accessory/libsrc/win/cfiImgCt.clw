!============================================================
! cfiImgCt.clw -- Clarion FreeImage Control implementation
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
! 2008.11.26    Added conditional compile for C7 for initial
!               position of image control
!
! 2007.03.15    Added Drawing control
!
! 2006.02.01    Corrected error in clientToBitmap methods when
!               image was scaled.
!
! 2009.07.13    Modify GetPosition to use Getwindowrect() and MapWindowPoints()
!               Change width calc in SetPosition()
!
! 2010.01.21    Add Blank() method
!============================================================

 Member
  Include('cfiImgCt.inc'),Once
  Include('keycodes.clw'),Once

  MAP
    Include('SaWApi.inc','Prototypes'),Once
    Module('')
      OutputDebugString(*CString),raw,pascal,Name('OutputDebugStringA')
    End

!Window procedure for the image control's child window.
!processes Windows messages and dispatches them to the correct instance
!of the class.
CtrlWinProc   Procedure(UNSIGNED hWnd,   |
                      Long uMsg,        |
                      Long wParam,      |
                      Long lParam       |
                     ),Long,PASCAL

  END

WProp_PcfiImageControlClassAtom     SA_ATOM,STATIC
WPROP_PCFIIMAGECONTROL              Cstring('CFI_ImageControl'),STATIC
WCLASSNAME_CFI                      Cstring('CFI_ImageControl'),STATIC

!---------------------------------------------------------------------------------------
cfiImageControlClass.Construct   Procedure()
!---------------------------------------------------------------------------------------
  Code
  Self.bResized         = False
  Self.BorderStyle      = CFIBS_NONE
  Self.dwStyle          = 0
  Self.dwExStyle        = 0
  Self.hBkgBrush        = 0
  Self.BkgColor         = 0
  Self.hPal             = 0
  Self.hBm              = 0
  Self.hDCMem           = 0
  Self.hBmOld           = 0
  Self.CtrlFeq          = 0
  Self.hWndCtrl         = 0
  Self.ZoomFactor       = 1.0         !100%
  Self.ZoomFilter       = FILTER_BOX  !default to the box resampling filter
  Self.deviceBpp        = SA_GetDeviceCaps(SA_GetDC(0), SA_BITSPIXEL)

  Self.RegisterClass()

  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.Destruct   Procedure()
!---------------------------------------------------------------------------------------
  Code

  Dispose(Self.SC)
  If Self.hBkgBrush
    SA_DeleteObject(Self.hBkgBrush)
  End
  Self.FreeBuffer()
  Dispose(Self.vsb)
  Dispose(Self.hsb)

  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.CopyToClipboard Procedure()
!---------------------------------------------------------------------------------------
  Code
  Self.CopyToClipboard()
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.CopyToClipboard Procedure()
!---------------------------------------------------------------------------------------
cb  SaClipboardClass
  Code
  If Self.hWndCtrl = 0 Then Return.
  If cb.OpenClipboard(Self.hWndCtrl) = True
    cb.SetClipboard(Self.iImage.GetBMInfo(), Self.iImage.GetDIBSize())
    cb.CloseClipboard()
  End
  Return                                         

!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.PasteFromClipboard Procedure()
!---------------------------------------------------------------------------------------
  Code
  Self.PasteFromClipboard()
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.PasteFromClipboard Procedure()
!---------------------------------------------------------------------------------------
cb  SaClipboardClass
hMem UNSIGNED,Auto
  Code
  If Self.hWndCtrl = 0 Then Return.
  If cb.OpenClipboard(Self.hWndCtrl) = True
    hMem = cb.GetClipboardCopy(SA_CF_DIB)
    If hMem <> 0
      If Self.iImage.Load(hMem, SA_GlobalSize(hMem)) = True
        Self.iImageControl.Reset()
      End
      SA_GlobalFree(hMem)
    End
    cb.CloseClipboard()
  End
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.RegisterClass    Procedure()
!---------------------------------------------------------------------------------------
cfiwndclass    Like(SA_WNDCLASSEX)
  Code
  cfiwndclass.cbSize           = Size(SA_WNDCLASSEX)
  cfiwndclass.style            = SA_CS_DBLCLKS
  cfiwndclass.lpfnWndProc      = Address(CtrlWinProc)
  cfiwndclass.cbClsExtra       = 0
  cfiwndclass.cbWndExtra       = 0
  cfiwndclass.hinstance        = System{Prop:AppInstance}
  cfiwndclass.hIcon            = 0  !No Icon
  cfiwndclass.hcursor          = 0  !Use default
  cfiwndclass.hbrBackground    = 0  !No background brush
  cfiwndclass.lpszMenuName     = 0  !No menu
  cfiwndclass.lpszClassName    = Address(WCLASSNAME_CFI)
  cfiwndclass.hIconSm          = 0  !No small icon
  If SA_RegisterClassEx(cfiwndclass) = 0
    if SA_GetLastError() <> 1410
      Self.iImage.TakeError(SA_GetLastError(),'Unable to register the window class: '&WCLASSNAME_CFI)
    End
  End

  return

!---------------------------------------------------------------------------------------
cfiImageControlClass.CreateChildWindow  Procedure()
!---------------------------------------------------------------------------------------
dwStyle UNSIGNED
rcCtrl  Like(SA_RECT)
hWndParent SA_HWND
hdc Long
NOSBCLASS                                   Equate('Unable to instantiate scroll bar class, CreateChildWindow()')
  Code

  Self.SC &= New SA_SelectionClass
  If Self.SC &= Null
     Self.iImage.TakeError(OUT_OF_MEMORY, 'Unable to instantiate Selection class, CreateChildWindow()')
     Return False
  End


  hWndParent = SA_GetParent(Self.CtrlFeq{Prop:Handle})
  SA_GetClientRect(Self.CtrlFeq{Prop:Handle}, rcCtrl)

  COMPILE('**C7**',_C70_)
    0{PROP:Pixels} = True 
    rcCtrl.top = Self.CtrlFeq{Prop:YPos}
    rcCtrl.left = Self.CtrlFeq{Prop:XPos}
    rcCtrl.right = Self.CtrlFeq{Prop:Width} 
    rcCtrl.bottom = Self.CtrlFeq{Prop:Height} 
    0{PROP:Pixels} = False
  !**C7**

  dwStyle = SA_WS_CHILD + SA_WS_VISIBLE + SA_WS_TABSTOP !+ SA_WS_SIZEBOX

  Case Self.BorderStyle
  Of CFIBS_LINE
    dwStyle = BOR(dwStyle, SA_WS_BORDER)
  Of CFIBS_SUNKEN
    Self.dwExStyle = BOR(Self.dwExStyle, SA_WS_EX_STATICEDGE)
  Of CFIBS_SUNKENDEEP
    Self.dwExStyle = BOR(Self.dwExStyle, SA_WS_EX_STATICEDGE)
    Self.dwExStyle = BOR(Self.dwExStyle, SA_WS_EX_CLIENTEDGE)
  End
  dwStyle = BOR(dwStyle, Self.dwStyle)

  Self.hWndCtrl = SA_CreateWindowEx(Self.dwExStyle,|
                             WCLASSNAME_CFI,|
                             ,|  !window title
                             dwstyle,|
                             rcCtrl.left,|
                             rcCtrl.top,|
                             rcCtrl.right,|
                             rcCtrl.bottom,|
                             hWndParent, | !Parent
                             0, |
                             System{Prop:AppInstance},|
                             0)
  If Self.hWndCtrl = 0
    Self.iImage.TakeError(SA_GetLastError(),'Unable to create a window for the image control.')
    Return False
  Else

    hdc = SA_GetDC(Self.hWndCtrl)
    Self.LogPixelsX = SA_GetDeviceCaps(hdc, SA_LogPixelsX)
    Self.LogPixelsY = SA_GetDeviceCaps(hdc, SA_LogPixelsY)
    SA_ReleaseDC(Self.hWndCtrl, hdc)

    Self.CtrlFeq{Prop:Handle} = Self.hWndCtrl
    Self.vsb &= New SA_ScrollbarClass
    If Self.vsb &= Null
      Self.iImage.TakeError(OUT_OF_MEMORY, NOSBCLASS)
      Return False
    End
    Self.vsb.init(Self.hWndCtrl, SA_SB_VERT)
    Self.vsb.hide()

    Self.hsb &= New SA_ScrollbarClass
    If Self.hsb &= Null
      Self.iImage.TakeError(OUT_OF_MEMORY, NOSBCLASS)
      Return False
    End
    Self.hsb.init(Self.hWndCtrl, SA_SB_HORZ)
    Self.hsb.hide()
  End

  Return True



!---------------------------------------------------------------------------------------
cfiImageControlClass.ResetScrollbars       Procedure(BOOL bResetSBThumbs=False)
!---------------------------------------------------------------------------------------
rcClient Like(SA_RECT)
vPageSize   Long,Auto
hPageSize   Long,Auto

bShowVsb      BOOL,Auto  !Will vsb be visible when done?
bShowHsb      BOOL,Auto  !Will hsb be visible when done?

nWidth   UNSIGNED,Auto
nHeight  UNSIGNED,Auto
  Code

  Self.GetClientAreaNoScrollbars(rcClient)

  !Hide/Show the scrollbars as necessary
  nWidth = Self.DDBitmapInfo.bmWidth -1
  nHeight = Self.DDBitmapInfo.bmHeight -1

  bShowVsb = Choose(rcClient.bottom <= nHeight )
  bShowHsb = Choose(rcClient.right  <= nWidth  )

  bShowVsb =Choose(rcClient.bottom - Choose(bShowHsb, SELF.hsb.GetHeight(), 0)  <= nHeight)
  bShowHsb =Choose(rcClient.right - Choose(bShowVsb, SELF.vsb.GetWidth(), 0) <= nWidth)

  Self.hsb.Show(bShowHsb)
  Self.vsb.Show(bShowVsb)

  !Now get the final height and width with/without scrollbars
  SA_GetClientRect(Self.hWndCtrl, rcClient)

  !Don't reset the thumb when user is resizing the window
  If Self.bResized = False  And bResetSBThumbs
    Self.vsb.SetThumbPos(0)
    Self.hsb.SetThumbPos(0)
  End

  !--- Set Vertical scroll bar ---
  vPageSize = rcClient.bottom
  Self.vsb.SetMin(0)
  Self.vsb.SetMax(nHeight )
  Self.vsb.SetPageSize(vPageSize)

  !--- Set Horizontal scroll bar ---
  hPageSize = rcClient.right
  Self.hsb.SetMin(0)
  Self.hsb.SetMax(nWidth)
  Self.hsb.SetPageSize(hPageSize)

  !Apply the changes and redraw scroll bars
  Self.vsb.Update(True)
  Self.hsb.Update(True)


  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.Scrollbars          Procedure(UNSIGNED nScrollbars=CFISB_BOTH)
!---------------------------------------------------------------------------------------
  CODE
  Self.vsb.bKeepHidden = Choose(BAND(nScrollbars, CFISB_VERT)=0)
  Self.hsb.bKeepHidden = Choose(BAND(nScrollbars, CFISB_HORIZ)=0)
  Return



!---------------------------------------------------------------------------------------
cfiImageControlClass.GetDDBInfo       Procedure()!,BOOL,Private
!---------------------------------------------------------------------------------------
  Code
  If SA_GetObject(Self.hBm, Size(Self.DDBitmapInfo), SELF.DDBitmapInfo) <> 0
    Return True
  Else
    Return False
  End


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.Close           Procedure()
  Code
  SA_DestroyWindow(Self.hWndCtrl)

  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.Blank   Procedure()
!---------------------------------------------------------------------------------------
  Code
  Self.iImage.Blank()
  Self.Reset()
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.Init   Procedure(Long CtrlFeq=0, UNSIGNED BorderStyle=CFIBS_NONE, UNSIGNED nScrollbars=CFISB_BOTH )
!---------------------------------------------------------------------------------------
  Code
  Return Self.Init(CtrlFeq, BorderStyle, nScrollbars)

!---------------------------------------------------------------------------------------
cfiImageControlClass.Init   Procedure(Long CtrlFeq=0, UNSIGNED BorderStyle=CFIBS_NONE, UNSIGNED nScrollbars=CFISB_BOTH )
!---------------------------------------------------------------------------------------
  Code
  Self.CtrlFeq = CtrlFeq
  Self.BorderStyle = BorderStyle
  WProp_PcfiImageControlClassAtom = SA_GlobalAddAtom(WPROP_PCFIIMAGECONTROL)
  IF WProp_PcfiImageControlClassAtom = 0
    SELF.iImage.TakeError(SA_GetLastError(),'Add atom failed')
  Else
  
    !Only one instance per hWnd
    IF SA_GetProp(CtrlFeq{Prop:Handle}, WProp_PcfiImageControlClassAtom) Then Return False.

    If Self.CreateChildWindow() = False
      !This is a fatal error
      Return False
    End

    Self.Scrollbars(nScrollbars)

    !-----------------------------------------------------------
    !Save this object's instance address in the window property
    Omit('beforeC6', _C60_ )
      If SA_SetProp(Self.hWndCtrl, WProp_PcfiImageControlClassAtom, |
                    Address(Self) ) <> 0
      End
    !beforeC6

    Compile('C6andLater', _C60_ )
      If SA_SetProp(Self.hWndCtrl, WProp_PcfiImageControlClassAtom, |
                    Instance(Self, Thread())) <> 0
      End
    !C6andLater
    Return True
  End

  Return False



!---------------------------------------------------------------------------------------
cfiImageControlClass.InitBuffer   Procedure()
!---------------------------------------------------------------------------------------
hDC         SA_HDC,Auto
bmih        &SA_BITMAPINFOHEADER
ScaledBM    &FreeImageClass
ZoomFilter  FREE_IMAGE_FILTER
  Code

  Self.FreeBuffer()

  hDC = SA_GetDC(0)
  Self.hDCmem = SA_CreateCompatibleDC(hDC)
  If Self.hDCmem = 0
    Self.iImage.TakeError(OUT_OF_MEMORY, 'Unable to create memory device context: ' &SA_GetLastError())
    Return
  End

  If Self.ZoomFactor = 1 !Or (Self.ZoomFilter = FILTER_NONE And Self.ZoomFactor > 1)
    bmih &= Self.iImage.GetBMInfoHeader()
    Self.hBm  = SA_CreateDIBitmap(hDC, Address(bmih), |
                                SA_CBM_INIT, Self.iImage.GetImageBits(),  |
                                Self.iImage.GetBmInfo(), SA_DIB_RGB_COLORS)
  Elsif Self.ZoomFactor > 0
    ScaledBM &= New FreeImageClass
    If Not ScaledBM &= Null
      If Self.ZoomFilter = FILTER_NONE
        ZoomFilter = FILTER_BOX
      Else
        ZoomFilter = Self.ZoomFilter
      End
      If Self.iImage.Rescale(ScaledBM.iImage, Self.ZoomFactor * 100, ZoomFilter)
        bmih &= ScaledBM.iImage.GetBMInfoHeader()
        Self.hBm  = SA_CreateDIBitmap(hDC, Address(bmih), |
                                  SA_CBM_INIT, ScaledBM.iImage.GetImageBits(),  |
                                  ScaledBM.iImage.GetBmInfo(), SA_DIB_RGB_COLORS)

      Else
        Self.iImage.TakeError(OUT_OF_MEMORY,'Unable to create zoomed bitmap')
      End
      Dispose(ScaledBM)
    Else
      Self.iImage.TakeError(OUT_OF_MEMORY,'Unable to create zoom object')
    End
  End

  SA_ReleaseDC(0, hDC)

  If Self.hBm

    !Select the bitmap into the device context
    SELF.hBmOld = SA_SelectObject(SELF.hdcMem, SELF.hBm)

    IF Not Self.hBmOld
      Self.iImage.TakeError(OUT_OF_MEMORY, 'Unable to select bitmap into memory DC')
    End
    Self.GetDDBInfo()

  Else
    Self.iImage.TakeError(OUT_OF_MEMORY, 'Unable to create buffer bitmap.||'&|
                   'If you''re not out of memory, this usually occurs|'&|
                   'because an image is not loaded into the control.')
  End 
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.FreeBuffer   Procedure()
!---------------------------------------------------------------------------------------
  Code
  Clear(Self.DDBitmapInfo)

  IF Self.hBmOld
    SA_DeleteObject(SA_SelectObject(SELF.hDCmem, SELF.hBmOld))
    Self.hBmOld = 0
  End
  Self.hBm = 0

  If Self.hDCmem
    SA_DeleteDC(Self.hDCmem)
    Self.hDCmem = 0
  End
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.Draw     Procedure(UNSIGNED cfiDrawOptions=0)
!---------------------------------------------------------------------------------------
  Code
  Self.Draw(cfiDrawOptions)

!---------------------------------------------------------------------------------------
cfiImageControlClass.Draw            Procedure(UNSIGNED cfiDrawOptions=0)
!---------------------------------------------------------------------------------------
rcClient    Like(SA_RECT)
  Code
  Self.InitBuffer()         !move the DIB into the DDB in memory
  Self.ResetScrollBars(Choose(BAND(cfiDrawOptions, CFIDRAW_RESETTHUMBS)>0) )

  SA_GetClientRect(Self.hWndCtrl, rcClient)
  SA_InvalidateRect(Self.hWndCtrl, rcClient, Choose(BAND(cfiDrawOptions, CFIDRAW_ERASEBKG)>0) ) !invalidate entire client area of control
  SA_UpdateWindow(Self.hWndCtrl)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.Reset           Procedure()
!---------------------------------------------------------------------------------------
  Code
  Self.Reset()
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.Reset           Procedure()
!---------------------------------------------------------------------------------------
  Code
  Self.Draw(CFIDRAW_RESETTHUMBS + CFIDRAW_ERASEBKG)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.Hide            Procedure(BOOL bHide=True)
!---------------------------------------------------------------------------------------
  Code
  Self.Hide(bHide)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.Hide            Procedure(BOOL bHide=True)
!---------------------------------------------------------------------------------------
  Code
  Self.Show(Choose(bHide,False,True))
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.Show            Procedure(BOOL bShow=True)
!---------------------------------------------------------------------------------------
  Code
  Self.Show(bShow)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.Show            Procedure(BOOL bShow=True)
!---------------------------------------------------------------------------------------
nCmdShow    Long
  Code
  nCmdShow= Choose(bShow, SA_SW_SHOW, SA_SW_HIDE)
  SA_ShowWindow(Self.hWndCtrl, nCmdShow)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.GetPosition     Procedure(*SA_RECT rcWindowPos)
!---------------------------------------------------------------------------------------
  Code
  Self.GetPosition(rcWindowPos)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.GetPosition     Procedure(*SA_RECT rcWindowPos)
!---------------------------------------------------------------------------------------
  Code
  SA_GetWindowRect(Self.hWndCtrl, rcWindowPos)
  rcWindowPos.right+=1
  rcWindowPos.bottom+=1
  SA_MapWindowPoints( 0, 0{Prop:ClientHandle}, rcWindowPos, 2)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.SetPosition     Procedure(SA_RECT rcWindowPos)
!---------------------------------------------------------------------------------------
  Code
  Self.SetPosition(rcWindowPos)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.SetPosition     Procedure(SA_RECT rcWindowPos)
!---------------------------------------------------------------------------------------
  Code
  SA_MoveWindow(Self.hWndCtrl,    |
                rcWindowPos.left, |
                rcWindowPos.top,  |
                rcWindowPos.right - rcWindowPos.left, |
                rcWindowPos.bottom - rcWindowPos.top, |
                True)
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.SetZoomFactor   Procedure(SReal ZoomFactor)!,BOOL,Proc
!---------------------------------------------------------------------------------------
  Code
  Return Self.SetZoomFactor(ZoomFactor)

!---------------------------------------------------------------------------------------
cfiImageControlClass.SetZoomFactor   Procedure(SReal ZoomFactor)!,BOOL,Proc
!---------------------------------------------------------------------------------------
bEraseBkg BOOL,Auto
rcClient  Like(SA_RECT)
  Code
  If ZoomFactor > 0.0
    !current zoom is less than last so ensure the background is erased.
    bEraseBkg = Choose(Self.ZoomFactor > ZoomFactor)
    Self.ZoomFactor = ZoomFactor
    Self.InitBuffer()
    Self.ResetScrollBars(False) !Don't reset the thumbs

    SA_GetClientRect(Self.hWndCtrl, rcClient)
    SA_InvalidateRect(Self.hWndCtrl, rcClient, bEraseBkg ) !invalidate entire client area of control
    
    Return True
  Else
    Return False
  End

!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.GetZoomFactor  Procedure()!,SReal
!---------------------------------------------------------------------------------------
  Code
  Return Self.GetZoomFactor()

!---------------------------------------------------------------------------------------
cfiImageControlClass.GetZoomFactor  Procedure()!,SReal
!---------------------------------------------------------------------------------------
  Code
  Return Self.ZoomFactor


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.ClientToBitmap  Procedure(*SA_RECT rcDest, *SA_RECT rcSrc) !,Protected
!---------------------------------------------------------------------------------------
  Code
  Self.ClientToBitmap(rcDest, rcSrc)


!---------------------------------------------------------------------------------------
cfiImageControlClass.ClientToBitmap  Procedure(*SA_RECT rcDest, *SA_RECT rcSrc) !,Protected
!---------------------------------------------------------------------------------------
  Code
  rcDest = rcSrc
  SA_OffsetRect(rcDest, Self.hsb.GetThumbPos(), Self.vsb.GetThumbpos())

  If Self.ZoomFactor = 0.0 then Return; End
  If Self.ZoomFactor <> 1.0
    rcDest.left   /=  Self.ZoomFactor
    rcDest.right  /=  Self.ZoomFactor
    rcDest.top    /=  Self.ZoomFactor
    rcDest.bottom /=  Self.ZoomFactor
  End
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.ClientToBitmap  Procedure(*SA_POINTS ptsDst, *SA_POINTS ptsSrc)!,Protected
!---------------------------------------------------------------------------------------
  Code
  Self.ClientToBitmap(ptsDst, ptsSrc)
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.ClientToBitmap  Procedure(*SA_POINTS ptsDst, *SA_POINTS ptsSrc)!,Protected
!---------------------------------------------------------------------------------------
  Code
  ptsDst.x = ptsSrc.x + Self.hsb.GetThumbPos()
  ptsDst.y = ptsSrc.y + Self.vsb.GetThumbPos()
  If Self.ZoomFactor = 0.0 Then Return; End
  If Self.ZoomFactor <> 1.0
    ptsDst.x /= Self.ZoomFactor
    ptsDst.y /= Self.ZoomFactor
  End

  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.SetZoomFilter   Procedure(FREE_IMAGE_FILTER ZoomFilter)
!---------------------------------------------------------------------------------------
  Code
  Self.SetZoomFilter(ZoomFilter)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.SetZoomFilter   Procedure(FREE_IMAGE_FILTER ZoomFilter)
!---------------------------------------------------------------------------------------
  Code
  If ZoomFilter => FILTER_BOX And ZoomFilter <= FILTER_LAST
    Self.ZoomFilter = ZoomFilter
  End
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.GetZoomFilter   Procedure()!,FREE_IMAGE_FILTER
!---------------------------------------------------------------------------------------
  Code
  Return Self.GetZoomFilter()

!---------------------------------------------------------------------------------------
cfiImageControlClass.GetZoomFilter   Procedure()!,FREE_IMAGE_FILTER
!---------------------------------------------------------------------------------------
  Code
  Return Self.ZoomFilter

!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.FitTo         Procedure(UNSIGNED FitMethod=CFIFIT_BOTH,  |
                                        FREE_IMAGE_FILTER fiFilter=FILTER_BICUBIC)

! Fit image to control at 100% zoom
!---------------------------------------------------------------------------------------
   Code
   Self.FitTo(FitMethod, fiFilter)
   Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.FitTo         Procedure(UNSIGNED FitMethod=CFIFIT_BOTH,  |
                                        FREE_IMAGE_FILTER fiFilter=FILTER_BICUBIC)

! Fit image to control at 100% zoom
!---------------------------------------------------------------------------------------
rcClient        Like(SA_RECT)
fAspectRatio    Real,Auto   !Aspect ratio of image
  Code

  If FitMethod = CFIFIT_NONE Then Return.
  Self.GetClientAreaNoScrollbars(rcClient)

  !Now scale the image to fit the control
  Case FitMethod  
  Of CFIFIT_BEST    !Contributed by Frank O'Classen 2013.09.19
    fAspectRatio = Self.iImage.GetHeight() / Self.iImage.GetWidth()
    If rcClient.right * fAspectRatio +1 > rcClient.bottom
       fAspectRatio = Self.iImage.GetWidth() / Self.iImage.GetHeight()
       Self.iImage.Rescale(rcClient.bottom * fAspectRatio, rcClient.bottom, fiFilter)
    Else
       Self.iImage.Rescale(rcClient.right, rcClient.right * fAspectRatio, fiFilter)
    End

  Of CFIFIT_BOTH
    Self.iImage.Rescale(rcClient.right, rcClient.bottom, fiFilter)

  Of CFIFIT_WIDTH
    fAspectRatio = Self.iImage.GetHeight() / Self.iImage.GetWidth()
    !Ajust the destination size if scroll bar will become visible
    If rcClient.right * fAspectRatio +1 > rcClient.bottom
      rcClient.right -= Self.vsb.GetWidth()
    End
    Self.iImage.Rescale(rcClient.right, rcClient.right * fAspectRatio, fiFilter)

  Of CFIFIT_HEIGHT
    fAspectRatio = Self.iImage.GetWidth() / Self.iImage.GetHeight()
    !Ajust the destination size if scroll bar will become visible
    If rcClient.bottom * fAspectRatio +1 > rcClient.right
      rcClient.bottom -= Self.hsb.GetHeight()
    End
    Self.iImage.Rescale(rcClient.bottom * fAspectRatio, rcClient.bottom, fiFilter)
  End
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.FitTo Procedure(*cfiImageControl dstImage, UNSIGNED FitMethod=CFIFIT_BOTH,  |
                                                   FREE_IMAGE_FILTER fiFilter=FILTER_BICUBIC)
  Code
  Self.FitTo(dstImage, FitMethod, fiFilter)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.FitTo         Procedure(*cfiImageControl dstImage, UNSIGNED FitMethod=CFIFIT_BOTH,  |
                                        FREE_IMAGE_FILTER fiFilter=FILTER_BICUBIC)
!---------------------------------------------------------------------------------------
rcClient        Like(SA_RECT)
fAspectRatio    Real,Auto   !Aspect ratio of image
  Code

  If FitMethod = CFIFIT_NONE Then Return.

  dstImage.GetClientAreaNoScrollbars(rcClient)

  !Now scale the image to fit the control
  Case FitMethod
  Of CFIFIT_BOTH
    Self.iImage.Rescale(dstImage.iImage, rcClient.right, rcClient.bottom, fiFilter)

  Of CFIFIT_WIDTH
    fAspectRatio = Self.iImage.GetHeight() / Self.iImage.GetWidth()
    !Ajust the destination size if scroll bar will become visible
    If rcClient.right * fAspectRatio  > rcClient.bottom
      rcClient.right -= dstImage.vsb.GetWidth()
    End
    Self.iImage.Rescale(dstImage.iImage, rcClient.right, rcClient.right * fAspectRatio, fiFilter)

  Of CFIFIT_HEIGHT
    fAspectRatio = Self.iImage.GetWidth()/ Self.iImage.GetHeight()
    !Ajust the destination size if scroll bar will become visible
    If rcClient.bottom * fAspectRatio +1 > rcClient.right
      rcClient.bottom -= dstImage.hsb.GetHeight()
    End
    Self.iImage.Rescale(dstImage.iImage, rcClient.bottom * fAspectRatio, rcClient.bottom, fiFilter)
  End
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.SetBkgColor     Procedure(SA_COLORREF BkgColor)
!---------------------------------------------------------------------------------------
  Code
  Self.SetBkgColor(BkgColor)

!---------------------------------------------------------------------------------------
cfiImageControlClass.SetBkgColor     Procedure(SA_COLORREF BkgColor)
!---------------------------------------------------------------------------------------
  Code
  !Remove the existing brush if one exists
  If Self.hBkgBrush
    SA_DeleteObject(Self.hBkgBrush)
    Self.hBkgBrush = 0
  End

  Self.BkgColor = BkgColor
  !Don't create a brush for system colors, the cached brush is retreived
  ! in the OnWm_EraseBkGnd method
  If Not (Self.BkgColor >= COLOR:SCROLLBAR AND Self.BkgColor <= 800000FFH)
    If Self.BkgColor = COLOR:NONE
      Self.BkgColor = COLOR:BTNFACE
    Else
      Self.hBkgBrush = SA_CreateSolidBrush(Self.BkgColor)
    End
  End
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.iImageControl.GetBkgColor     Procedure()!,SA_COLORREF
!---------------------------------------------------------------------------------------
  Code
  Return Self.GetBkgColor()

!---------------------------------------------------------------------------------------
cfiImageControlClass.GetBkgColor     Procedure()!,SA_COLORREF
!---------------------------------------------------------------------------------------
  Code
  Return Self.BkgColor


!---------------------------------------------------------------------------------------
cfiImageControlClass.GetClientAreaNoScrollbars Procedure(*SA_RECT rcClient)
!---------------------------------------------------------------------------------------
  Code
  !Find the size of client area Without H and V scroll bars
  SA_GetClientRect(Self.hWndCtrl, rcClient)
  SA_InflateRect(rcClient, Choose(Self.vsb.IsVisible(),Self.vsb.GetWidth(),0), |
                           Choose(Self.hsb.IsVisible(), Self.hsb.GetHeight(), 0) )
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWm_Paint      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------

hDC     Long,Auto
ps      Like(SA_PAINTSTRUCT),Auto
bmi     &SA_BITMAPINFO
hOldPal SA_HPALETTE,Auto
bsbVisible BOOL,Auto
rcClient Like(SA_RECT)
hsbThumbPos UNSIGNED,Auto
vsbThumbPos UNSIGNED,Auto
Saved       Long,Auto
  code

  bmi &= Self.iImage.GetBMInfo()
  SA_GetClientRect(hWnd, rcClient)

  hDC = SA_BeginPaint(hWnd, ps)
    hsbThumbPos = Self.hsb.GetThumbpos()
    vsbThumbPos = Self.vsb.GetThumbpos()

    Saved= SA_SaveDC(hDC)
    !--------------------------------------------------------
    ! Get the palette for this image and select it into the DC
    ! NOTE: Need to finish Palette support...
    !--------------------------------------------------------
    Self.hPal = 0 !FreeImage_GetPalette()
    If Self.hPal
      hOldPal = SA_SelectPalette(hDC, Self.hPal, FALSE)
    End

     SA_BitBlt(hDC, |
                ps.rcPaint.left,   |
                ps.rcPaint.top,    |
                ps.rcPaint.right,  |
                ps.rcPaint.bottom, |
                SELF.hDCmem, |
                hsbThumbPos + ps.rcPaint.left, |
                vsbThumbPos + ps.rcPaint.top, |
                SA_SRCCOPY)


    If Self.hPal
      SA_SelectPalette(hDC, hOldPal, False)
    End
    SA_RestoreDC(hDC, Saved)
  SA_EndPaint(hWnd, ps)

  !Force vsb and hsb to redraw on a resize event
  !only necessary when new size < old size
  If Self.bResized
    bsbVisible = Self.vsb.IsVisible()
    If bsbVisible = True
      Self.vsb.Hide(bsbVisible)
      Self.vsb.Show(bsbVisible)
    End
    bsbVisible = Self.hsb.IsVisible()
    If bsbVisible = True And Self.vsb.IsVisible() = False
      Self.hsb.Hide(bsbVisible)
      Self.hsb.Show(bsbVisible)
    End
    Self.bResized = False
  End
  Return 0



!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWm_Destroy    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWm_Size       Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
nSize       Group,Over(lParam)
width         UShort
height        UShort
            End
  Code
  Self.bResized = True
  Self.ResetScrollBars()
  SA_InvalidateRect(Self.hWndCtrl, ,False)

  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWm_Move       Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWm_EraseBkGnd Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
rcUpdate  Like(SA_RECT)
rcBitmap  Like(SA_RECT)
rcClient  Like(SA_RECT)
hBkgBrush SA_HBRUSH

bmWidth   UNSIGNED,Auto
bmHeight  UNSIGNED,Auto

  Code
  SA_GetClientRect(hWnd, rcClient)

  bmWidth = Self.DDBitmapInfo.bmWidth
  bmHeight = Self.DDBitmapInfo.bmHeight

  If (Self.BkgColor >= COLOR:SCROLLBAR AND Self.BkgColor <= 800000FFH)
    !Get a cached system brush
    hBkgBrush = SA_GetSysColorBrush(BAND(Self.BkgColor, 000000FFh))
  Else
    If Self.hBkgBrush
      !use the brush created in SetBkgColor()
      hBkgBrush = Self.hBkgBrush
    End
  End

  !If no brush, use a system color
  If hBkgBrush = 0
    hBkgBrush = SA_GetSysColorBrush(CFI_DEFAULTBKGSYSTEMCOLORINDEX)
  End

  If bmWidth < rcClient.right
    rcUpdate = rcClient
    rcUpdate.left = bmWidth
    SA_FillRect(wParam, rcUpdate, hBkgBrush )
  End

  If bmHeight < rcClient.Bottom
    rcUpdate = rcClient
    rcUpdate.right = bmWidth
    rcUpdate.top   = bmHeight
    SA_FillRect(wParam, rcUpdate, hBkgBrush )
  End

  Return 1 !1=Don't let windows erase the background 


!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnMouseWheel Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
  Code
  If Self.vsb.iMouse.OnMouseWheel(hWnd, uMsg, wParam, lParam)
    SA_InvalidateRect(hWnd, , False)
    Return 1
  End
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWM_HScroll Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
RetVal  Long
  Code
  RetVal = Self.hsb.OnWM_HScroll(hWnd, uMsg, wParam, lParam)
  SA_InvalidateRect(hWnd, , False)
  Return RetVal

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWM_VScroll Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
RetVal  Long
  Code
  RetVal = Self.vsb.OnWM_VScroll(hWnd, uMsg, wParam, lParam)
  SA_InvalidateRect(hWnd, , False)
  Return RetVal


!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWm_SettingChange Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWM_EnterSizeMove Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWM_ExitSizeMove Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWM_QueryNewPalette Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnWM_PaletteChanged Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnMouseButtonDown  Procedure(SA_POINTS mousePos, Long mouseKeycode)!,Virtual
!---------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(mousePos)
  CODE
  Self.ClientToBitmap(mPos, mPos)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnMouseButtonUp    Procedure(SA_POINTS mousePos, Long mouseKeycode)!,Virtual
!---------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(mousePos)
  CODE
  Self.ClientToBitmap(mPos, mPos)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnMouseDoubleClick Procedure(SA_POINTS mousePos, Long mouseKeycode)!,Virtual
!---------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(mousePos)
  CODE
  Self.ClientToBitmap(mPos, mPos)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnMouseMove Procedure(SA_POINTS mousePos)!,Virtual
!---------------------------------------------------------------------------------------
  Code
  Self.ClientToBitmap(mousePos, mousePos)
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.TranslateMouseKey  Procedure(Long wParam)!,Long !Return clarion mouse key equate
!---------------------------------------------------------------------------------------
mClarionKeycode  Long,Auto
  Code
  mClarionKeycode = 0
  !-------------------------------------------
  !First get the button
  !Clarion doesn't have keycodes for Xbuttons
  If Band(wParam, SA_MK_LBUTTON)
    mClarionKeyCode = MouseLeft
  ElsIf Band(wParam, SA_MK_RBUTTON)
    mClarionKeyCode = MouseRight
  ElsIf Band(wParam, SA_MK_MBUTTON)
    mClarionKeyCode = MouseCenter
  End

  !-------------------------------------------
  !add any keys pressed with the mouse button
  If Band(wParam, SA_MK_CONTROL) <> 0
    mClarionKeyCode += 200h
  End

  If Band(wParam, SA_MK_SHIFT) <> 0
    mClarionKeyCode += 100h
  End

  If SA_GetKeyState(SA_VK_MENU) < 0   !detect the ALT key
    !-------------------------------------------
    !clarion doesn't have a keycode for CtrlAltShiftMouseButton
    If mClarionKeycode < CtrlAltMouseLeft
      mClarionKeyCode += 400h
    End
  End

  Return mClarionKeycode

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnLButtonDown Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
retCode Long,Auto
mPos Like(SA_POINTS),Over(lParam)
  CODE
  retCode = Self.SC.iMouse.OnLButtonDown(hWnd, uMsg, wParam, lParam)
  Self.OnMouseButtonDown(mPos, Self.TranslateMouseKey(wParam))
  Return retCode

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnMouseMove Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
retCode Long,Auto
mPos Like(SA_POINTS),Over(lParam)
  CODE
  retCode = Self.SC.iMouse.OnMouseMove(hWnd, uMsg, wParam, lParam)
  Self.OnMouseMove(mPos)
  Return retCode


!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnLButtonUp  Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG,Protected,Virtual
!---------------------------------------------------------------------------------------
retCode Long,Auto
rcBitmapArea    Like(SA_RECT)
rcSelection     Like(SA_RECT)
mPos Like(SA_POINTS),Over(lParam)
  CODE
  Self.OnMouseButtonUp(mPos, Self.TranslateMouseKey(wParam+SA_MK_LBUTTON))

  retCode = Self.SC.iMouse.OnLButtonUp(hWnd, uMsg, wParam, lParam)
  Case Self.SC.ISelection.GetAction()
  Of CFISELECTACTION_CROP
    Self.SC.ISelection.GetRect(rcSelection)
    If Self.OnEndCropSelection(rcSelection) = True
      Self.ClientToBitmap(rcBitmapArea, rcSelection)
      Self.iImage.Crop(rcBitmapArea)
      Self.SC.ISelection.SetAction(CFISELECTACTION_NONE)
      Self.Reset()
    End
  Of CFISELECTACTION_SELECT
    Self.SC.ISelection.GetRect(rcSelection)
    Self.OnSelection(rcSelection)
    Self.Sc.RemoveSelection(Self.hwndCtrl)
    Self.SC.ISelection.SetAction(CFISELECTACTION_NONE)
  End


  Return retCode

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnSelection        Procedure(*SA_RECT rcSelection)!,Virtual
!---------------------------------------------------------------------------------------
  Code
  Return


!---------------------------------------------------------------------------------------
cfiImageControlClass.BeginCropSelection Procedure()!,Virtual
!---------------------------------------------------------------------------------------
  Code
  Self.SC.ISelection.SetAction(CFISELECTACTION_CROP)
  Return

!---------------------------------------------------------------------------------------
cfiImageControlClass.OnEndCropSelection   Procedure(*SA_RECT rcSelection)!,BOOL,Virtual
!---------------------------------------------------------------------------------------
  Code
  Return True

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnRButtonDown    Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(lParam)
  Code
  Self.OnMouseButtonDown(mPos, Self.TranslateMouseKey(wParam))
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnRButtonUp  Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(lParam)
  CODE
  Self.OnMouseButtonUp(mPos, Self.TranslateMouseKey(wParam+SA_MK_RBUTTON))
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnLButtonDblClk  Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(lParam)
  Code
  Self.OnMouseDoubleClick(mPos, Self.TranslateMouseKey(wParam))
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnMButtonDblClk      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(lParam)
  Code
  Self.OnMouseDoubleClick(mPos, Self.TranslateMouseKey(wParam))
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnMButtonDown      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(lParam)
  Code
  Self.OnMouseButtonDown(mPos, Self.TranslateMouseKey(wParam))
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnMButtonUp      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(lParam)
  CODE
  Self.OnMouseButtonUp(mPos, Self.TranslateMouseKey(wParam+SA_MK_MBUTTON))
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnRButtonDblClk      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
mPos Like(SA_POINTS),Over(lParam)
  Code
  Self.OnMouseDoubleClick(mPos, Self.TranslateMouseKey(wParam))
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnXButtonDblClk      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnXButtonDown      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnXButtonUp      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnMouseHover      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
  Code
  Return 0

!---------------------------------------------------------------------------------------
cfiImageControlClass.iMouse.OnMouseLeave      Procedure(UNSIGNED hWnd, LONG uMsg, LONG wParam, LONG lParam)!,LONG
!---------------------------------------------------------------------------------------
  Code
  Return 0





!==============================================================
!==============================================================
! Window Procedure for window class
!==============================================================
CtrlWinProc     Procedure(UNSIGNED hWnd,   |
                      Long uMsg,   |
                      Long wParam, |
                      Long lParam)
This            &cfiImageControl
  Code
  This &= (SA_GetProp(hWnd, WProp_PcfiImageControlClassAtom))
  If Not This &= Null
    Case uMsg

    Of SA_WM_NCDESTROY
      SA_RemoveProp(hWnd,  WProp_PcfiImageControlClassAtom)
      SA_GlobalDeleteAtom(WProp_PcfiImageControlClassAtom)

    Of SA_WM_PAINT
      If This.OnWm_Paint(hWnd, uMsg, wParam, lParam) = 0
        Return 1
      End

    Of SA_WM_ERASEBKGND
      If This.OnWm_EraseBkgnd(hWnd, uMsg, wParam, lParam)
        Return 1 
      End

    Of SA_WM_HSCROLL
      If This.OnWM_HScroll(hWnd, uMsg, wParam, lParam)
        Return 0
      End

    Of SA_WM_VSCROLL
      If This.OnWM_VScroll(hWnd, uMsg, wParam, lParam)
        Return 0
      End

    Of SA_WM_SIZE
      If This.OnWm_Size(hWnd, uMsg, WParam, lParam)
        Return 0
      End

    Of SA_WM_LBUTTONDOWN
      If This.iMouse.OnLButtonDown(hWnd, uMsg, wParam, lParam)
        Return 0
      End
    Of SA_WM_LBUTTONDBLCLK
      If This.iMouse.OnLButtonDblClk(hWnd, uMsg, wParam, lParam)
        Return 0
      End
    Of SA_WM_LBUTTONUP
      If This.iMouse.OnLButtonUP(hWnd, uMsg, wParam, lParam)
        Return 0
      End

    Of SA_WM_RBUTTONDOWN
      If This.iMouse.OnRButtonDown(hWnd, uMsg, wParam, lParam)
        Return 0
      End
    Of SA_WM_RBUTTONUP
      If This.iMouse.OnRButtonUp(hWnd, uMsg, wParam, lParam)
        Return 0
      End
    Of SA_WM_RBUTTONDBLCLK
      If This.iMouse.OnRButtonDblClk(hWnd, uMsg, wParam, lParam)
        Return 0
      End

    Of SA_WM_MBUTTONDOWN
      If This.iMouse.OnMButtonDown(hWnd, uMsg, wParam, lParam)
        Return 0
      End
    Of SA_WM_MBUTTONUP
      If This.iMouse.OnMButtonUp(hWnd, uMsg, wParam, lParam)
        Return 0
      End
    Of SA_WM_MBUTTONDBLCLK
      If This.iMouse.OnMButtonDblClk(hWnd, uMsg, wParam, lParam)
        Return 0
      End

    Of SA_WM_XBUTTONDOWN
      If This.iMouse.OnXButtonDown(hWnd, uMsg, wParam, lParam)
        Return 0
      End
    Of SA_WM_XBUTTONUP
      If This.iMouse.OnXButtonUp(hWnd, uMsg, wParam, lParam)
        Return 0
      End
    Of SA_WM_XBUTTONDBLCLK
      If This.iMouse.OnXButtonDblClk(hWnd, uMsg, wParam, lParam)
        Return 0
      End

    ! The hover and leave events are only posted after the
    ! TrackMouseEvent() api is called
    Of SA_WM_MOUSEHOVER
      If This.iMouse.OnMouseHover(hWnd, uMsg, wParam, lParam)
        Return 0
      End
    Of SA_WM_MOUSELEAVE
      If This.iMouse.OnMouseLeave(hWnd, uMsg, wParam, lParam)
        Return 0
      End


     Of SA_WM_MOUSEMOVE
      If This.iMouse.OnMouseMove(hWnd, uMsg, wParam, lParam)
        Return 0
      End

    Of SA_WM_MOUSEWHEEL !OROF (windows 95) MSH_MOUSEWHEEL
      If This.iMouse.OnMouseWheel(hwnd, uMsg, wParam, lParam)
         Return 0 !Always return zero if this event is handled
      End

    Of SA_WM_SETTINGCHANGE
      If This.OnWm_SettingChange(hWnd, uMsg, wParam, lParam)
        Return 0
      End

    Of SA_WM_ENTERSIZEMOVE
      If This.OnWM_EnterSizeMove(hWnd, uMsg, wParam, lParam)
        Return 0
      End
    Of SA_WM_EXITSIZEMOVE
      If This.OnWM_ExitSizeMove(hWnd, uMsg, wParam, lParam)
        Return 0
      End

    Of SA_WM_QUERYNEWPALETTE
      Return This.OnWM_QueryNewPalette(hWnd, uMsg, wParam, lParam)

    Of SA_WM_PALETTECHANGED
      If This.OnWM_PaletteChanged(hWnd, uMsg, wParam, lParam).


    End !Case msg
  End
  Return SA_DefWindowProc( hWnd, uMsg, wParam, lParam)





!-----------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------
cfiImageControl.OnSelection         Procedure()!,Virtual
  Code

  Return





!-----------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------
cfiSignatureControl.Construct         Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Self.polyline &= New SA_PolylineClass
  If Not Self.polyline &= Null
    Self.polyline.defaultPen &= New SA_PenClass
    Self.iPen &= Self.polyline.iPen
  End

  Return

!-----------------------------------------------------------------------------------------------
cfiSignatureControl.Destruct         Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Dispose(Self.polyline.defaultPen)
  Dispose(Self.polyline)
  Return

!-----------------------------------------------------------------------------------------------
cfiSignatureControl.OnMouseButtonDown  Procedure(SA_POINTS mousePos, Long mouseKeycode)!,Virtual
!-----------------------------------------------------------------------------------------------
rcClient    Like(SA_RECT)
  Code
  Parent.OnMouseButtonDown(mousePos, mouseKeycode)
  Case mouseKeycode
  Of MouseLeft
    SA_SetCapture(Self.hWndCtrl)  !Ensure all mouse input goes to this control

    !Clip the mouse cursor to the bounding rectangle
    SA_GetWindowRect(Self.hWndCtrl, rcClient)   !Get the client coordinates from the object
    SA_InflateRect(rcClient, -1, -1)
    SA_ClipCursor(rcClient)
    Self.polyline.Reset()

    Self.Polyline.SetScrollOffset(Self.hsb.GetThumbPos(), Self.vsb.GetThumbPos())

    Self.polyline.AddPoint(mousePos)
    Self.bDrawingSignature = True
  End
  Return


!-----------------------------------------------------------------------------------------------
cfiSignatureControl.OnMouseButtonUp    Procedure(SA_POINTS mousePos, Long mouseKeycode)!,Virtual
!-----------------------------------------------------------------------------------------------
  Code
  Parent.OnMouseButtonUp(mousePos, mouseKeycode)
  Case mouseKeycode
  Of MouseLeft
    If Self.bDrawingSignature = True
      Self.Polyline.RemoveScrollOffset()
      Self.Polyline.Render(Self.hDCMem)

      ! Save the drawing into the DIB
      ! If .reset() or .draw() is called prior to this call the changes to the image are erased
      SA_GetDIBits(Self.hDCMem, Self.hBm, 0, Self.iImage.GetHeight(), Self.iImage.GetImageBits(), Self.iImage.GetBMInfo(), SA_DIB_RGB_COLORS)

      !Release the mouse so it can move outside the control
      SA_ClipCursor()
      SA_ReleaseCapture()
      Self.bDrawingSignature = False
    End

  End

  Return

!-----------------------------------------------------------------------------------------------
cfiSignatureControl.OnMouseMove        Procedure(SA_POINTS mousePos)!,Virtual
!-----------------------------------------------------------------------------------------------
hDCsig  SA_HDC
  Code

  Parent.OnMouseMove(mousePos)

  If Self.bDrawingSignature
    Self.Polyline.AddPoint(mousePos)
    hDCSig = SA_GetDC(Self.hWndCtrl)
    If hDCSig <> 0
      Self.polyline.Render(hDCSig)
      SA_ReleaseDC(Self.hWndCtrl, hDCSig)
    End
  End
  Return











!-----------------------------------------------------------------------------------------------
cfiDrawingControl.DrawingObject         Procedure(Long nDrawingObject)
!-----------------------------------------------------------------------------------------------
  Code
  Self.nDrawingObject = nDrawingObject
  Return

!-----------------------------------------------------------------------------------------------
cfiDrawingControl.DrawingObject         Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.nDrawingObject


!-----------------------------------------------------------------------------------------------
cfiDrawingControl.PenColor              Procedure(Long penColor)
!-----------------------------------------------------------------------------------------------
  Code
  Self.penColor = penColor
  Return

!-----------------------------------------------------------------------------------------------
cfiDrawingControl.PenColor              Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.penColor



!-----------------------------------------------------------------------------------------------
cfiDrawingControl.FillColor              Procedure(Long fillColor)
!-----------------------------------------------------------------------------------------------
  Code
  Self.fillColor = fillColor
  Return

!-----------------------------------------------------------------------------------------------
cfiDrawingControl.FillColor              Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.fillColor


!-----------------------------------------------------------------------------------------------
cfiDrawingControl.PenWidth              Procedure(Long penWidth)
!-----------------------------------------------------------------------------------------------
  Code
  Self.penWidth = penWidth
  Return

!-----------------------------------------------------------------------------------------------
cfiDrawingControl.PenWidth              Procedure()!,Long
!-----------------------------------------------------------------------------------------------
  Code
  Return Self.penWidth

!-----------------------------------------------------------------------------------------------
cfiDrawingControl.StartSelection  Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Self.Sc.iSelection.SetAction(CFISELECTACTION_SELECT)
  Return

!-----------------------------------------------------------------------------------------------
cfiDrawingControl.OnSelection        Procedure(*SA_RECT rcSelection)!,Virtual
!-----------------------------------------------------------------------------------------------
oRect &SA_RectClass
hDC  SA_HDC
rcBitmapArea Like(SA_RECT)
  Code


  Case Self.nDrawingObject

  Of SA_GDI_POLYLINE

  Of SA_GDI_RECTANGLE
    oRect &= New SA_RectClass
    oRect.Rectangle(rcSelection)
    oRect.defaultPen.iGdiPen.SetPenWidth(Self.penWidth)
    oRect.defaultPen.iGdiPen.SetPenColor(Self.penColor)
    oRect.fillColor = Self.fillColor
    hDC = SA_GetDC(Self.hWndCtrl)
    Self.Sc.RemoveSelection(Self.hwndCtrl)

    !Render the object on the display
    !This updates the display instantly
    If hDC <> 0
      oRect.Render(hDC)
      SA_ReleaseDC(Self.hWndCtrl, hDC)
    End

    !Now convert to the bitmap coordinates and draw on the memory DC
    !This assures the object is displayed when scrolling or invalidated
    Self.ClientToBitmap(rcBitmapArea, rcSelection)
    oRect.Rectangle(rcBitmapArea)
    oRect.Render(Self.hDCMem)
    Dispose(oRect)

    !Save the drawing from the memory DC to the DIB
    SA_GetDIBits(Self.hDCMem, Self.hBm, 0, Self.iImage.GetHeight(), Self.iImage.GetImageBits(), Self.iImage.GetBMInfo(), SA_DIB_RGB_COLORS)


  End


  Return


!-----------------------------------------------------------------------------------------------
cfiDrawingControl.OnMouseButtonDown  Procedure(SA_POINTS mousePos, Long mouseKeycode)!,Virtual
!-----------------------------------------------------------------------------------------------
  Code
  Parent.OnMouseButtonDown(mousePos, mouseKeycode)
  Case mouseKeycode
  Of MouseLeft
  End

  Return

!-----------------------------------------------------------------------------------------------
cfiDrawingControl.OnMouseButtonUp  Procedure(SA_POINTS mousePos, Long mouseKeycode)!,Virtual
!-----------------------------------------------------------------------------------------------
  Code
  Return

!-----------------------------------------------------------------------------------------------
cfiDrawingControl.Construct             Procedure()
!-----------------------------------------------------------------------------------------------
  Code
  Self.nDrawingObject = SA_GDI_NONE
  Self.fillColor = Color:None
  Return






