!============================================================
! cfiDlgs.clw -- Clarion FreeImage Dialogs implementation
!
!  Copyright © 2005 - 2006 Sand & Associates, Larry@sand-associates.com
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
  Member
  Include('cfiDlgs.inc'),Once

  Map
  End

!--------------------------------------------------------------------------
cfiDialogsClass.Init    Procedure(*iImageControl iImgCtrl, *iImage iImg)
!--------------------------------------------------------------------------
  Code
  Self.iImgCtrl &= iImgCtrl
  Self.iImg &= iImg
  Return


!--------------------------------------------------------------------------
cfiDialogsClass.BrightnessContrastDialog    Procedure(<String sTitle>)
!--------------------------------------------------------------------------
previewImage    cfiImageControl
bufferImage     FreeImageClass
originalImage   FreeImageClass

NewBrightness   Real(0)
NewContrast     Real(0)

bApplied        BOOL,Auto
bOk             BOOL(False)

 !Edit the window in the .trn file to localize or modify the window
 Include('cfiImgCt.trn', 'cfiBrightContrastDialogWindow'),Once


 Code
  If Self.iImg &= Null Or Self.iImgCtrl &= Null Then Return bOk; End

  Open(Window)
  If Not Omitted(2)
    Window{Prop:Text} = Clip(sTitle)
  End
  SetCursor(Cursor:Wait)
  previewImage.iImageControl.Init(?ImageControl, CFIBS_SUNKEN, CFISB_NONE)

  !Make a copy of the image passed scaled to the preview width
  Self.iImgCtrl.FitTo(previewImage, CFIFIT_WIDTH, FILTER_BICUBIC)
  previewImage.iImageControl.Draw()
  previewImage.iImage.CopyImage(bufferImage.iImage)
  Self.iImg.CopyImage(originalImage.iImage)
  bApplied = False
  SetCursor

  Accept
    Case Field()
    Of ?OKButton
      If Event() = Event:Accepted
        bOk = True
        Post(Event:Accepted, ?ApplyButton)
        Post(Event:CloseWindow)
      End

    Of ?NewBrightness
      Case Event()
      Of Event:NewSelection OrOf Event:Accepted
        Enable(?ApplyButton)
        Post(EVENT:UpdatPreviewImage)
      End

    Of ?NewContrast
      Case Event()
      Of Event:NewSelection OrOf Event:Accepted
        Enable(?ApplyButton)
        Post(EVENT:UpdatPreviewImage)
      End

    Of ?ApplyButton
     If Event() = Event:Accepted
       If bApplied
         originalImage.iImage.CopyImage(Self.iImg)
       End
       Self.iImg.Brightness(NewBrightness)
       Self.iImg.Contrast(NewContrast)
       Self.iImgCtrl.Draw()
       bApplied = True
       Disable(?ApplyButton)
     End


    Of ?CancelButton
      If Event() = Event:Accepted
        Post(Event:CloseWindow)
      End
    End

    Case Event()
    Of EVENT:CloseWindow
      If bOk = False And bApplied
        originalImage.iImage.CopyImage(Self.iImg)
        Self.iImgCtrl.Draw()
      End
    Of EVENT:UpdatPreviewImage
      bufferImage.iImage.CopyImage(previewImage.iImage)
      If NewBrightness <> 0
        previewImage.iImage.Brightness(NewBrightness)
      End
      If NewContrast <> 0
        previewImage.iImage.Contrast(NewContrast)
      End
      previewImage.iImageControl.Draw()
    End
  End
  Close(Window)
  Return bOk


!--------------------------------------------------------------------------
cfiDialogsClass.GammaDialog    Procedure(<String sTitle>)!,Bool,Virtual
!--------------------------------------------------------------------------
previewImage    cfiImageControl
bufferImage     FreeImageClass
originalImage   FreeImageClass

NewGamma        Real(1.0)

bApplied        BOOL,Auto
bOk             BOOL(False)

 !Edit the window in the .trn file to localize or modify the window
 Include('cfiImgCt.trn', 'cfiGammaDialogWindow'),Once


 Code
  If Self.iImg &= Null Or Self.iImgCtrl &= Null Then Return bOk; End
  Open(Window)
  If Not Omitted(2)
    Window{Prop:Text} = Clip(sTitle)
  End
  SetCursor(Cursor:Wait)
  previewImage.iImageControl.Init(?ImageControl, CFIBS_SUNKEN, CFISB_NONE)

  !Make a copy of the image passed scaled to the preview width
  Self.iImgCtrl.FitTo(previewImage, CFIFIT_WIDTH, FILTER_BICUBIC)
  previewImage.iImageControl.Draw()
  previewImage.iImage.CopyImage(bufferImage.iImage)
  Self.iImg.CopyImage(originalImage.iImage)
  bApplied = False

 SetCursor

  Accept
    Case Field()
    Of ?OKButton
      If Event() = Event:Accepted
        bOk = True
        Post(Event:Accepted, ?ApplyButton)
        Post(Event:CloseWindow)
      End

    Of ?NewGamma
      Case Event()
      Of Event:NewSelection OrOf Event:Accepted
        Enable(?ApplyButton)
        Post(EVENT:UpdatPreviewImage)
      End

    Of ?ApplyButton
     If Event() = Event:Accepted
       If bApplied
         originalImage.iImage.CopyImage(Self.iImg)
       End
       Self.iImg.Gamma(NewGamma)
       Self.iImgCtrl.Draw()
       bApplied = True
       Disable(?ApplyButton)
     End

    Of ?CancelButton
      If Event() = Event:Accepted
        Post(Event:CloseWindow)
      End
    End

    Case Event()
    Of EVENT:CloseWindow
      If bOk = False And bApplied
        originalImage.iImage.CopyImage(Self.iImg)
        Self.iImgCtrl.Draw()
      End

    Of EVENT:UpdatPreviewImage
      bufferImage.iImage.CopyImage(previewImage.iImage)
      If NewGamma <> 1.0
        previewImage.iImage.Gamma(NewGamma)
      End
      previewImage.iImageControl.Draw()
    End
  End
  Close(Window)
  Return bOK



!--------------------------------------------------------------------------
cfiDialogsClass.RescaleDialog    Procedure(<String sTitle>)!,Bool,Virtual
!--------------------------------------------------------------------------
originalImage   FreeImageClass
bApplied        BOOL,Auto
RescaleDimType  Long(1)
bMaintainAspectRatio BOOL(True)
bPixelSize      BOOL(True)
PixelWidth      UNSIGNED,Auto
PixelHeight     UNSIGNED,Auto
PercentWidth    Real,Auto
PercentHeight   Real,Auto

ResamplingFilter   FREE_IMAGE_FILTER(FILTER_BOX)
fAspectRatio    Real,Auto
OldWidth        Long,Auto
bOK             BOOL(False)


 !Edit the window in the .trn file to localize or modify the window
 Include('cfiImgCt.trn', 'cfiRescaleDialogWindow'),Once


 Code
  If Self.iImg &= Null Or Self.iImgCtrl &= Null Then Return bOk; End

  Open(Window)
  If Not Omitted(2)
    Window{Prop:Text} = Clip(sTitle)
  End
  SetCursor(Cursor:Wait)
  Self.iImg.CopyImage(originalImage.iImage)
  PixelWidth = Self.iImg.GetWidth()
  PixelHeight = Self.iImg.GetHeight()
  PercentWidth = 100.0
  PercentHeight = 100.0
  fAspectRatio = PixelHeight/ PixelWidth
  OldWidth = PixelWidth

  Do AspectControlState

  bApplied = False
  SetCursor

  Accept
    Case Field()
    Of ?RescaleDimType
      If Event() = Event:Accepted
        Do AspectControlState
      End

    Of ?PixelWidth
      Case Event()
      Of Event:Accepted
        If bMaintainAspectRatio = True
          PixelHeight += ((PixelWidth - OldWidth) * fAspectRatio)
          OldWidth = PixelWidth
          Display(?PixelHeight)
          Enable(?ApplyButton)
        End
      Of Event:NewSelection
        Enable(?ApplyButton)
      End

    Of ?PercentWidth
      Case Event()
      Of Event:Accepted
        If bMaintainAspectRatio = True
          PercentHeight = PercentWidth
          Display(?PercentHeight)
        End
        Enable(?ApplyButton)
      Of Event:NewSelection
        Enable(?ApplyButton)
      End


    Of ?bMaintainAspectRatio
      If Event() = Event:Accepted
        Do AspectControlState
      End

    Of ?ResamplingFilter
      Case Event()
      Of Event:NewSelection
        Enable(?ApplyButton)
      End

    Of ?OKButton
      If Event() = Event:Accepted
        bOK  = True
        If ?ApplyButton{Prop:Disable} = False
          Post(Event:Accepted, ?ApplyButton)
        End
        Post(Event:CloseWindow)
      End


    Of ?ApplyButton
     If Event() = Event:Accepted
       If bApplied = True
         originalImage.iImage.CopyImage(Self.iImg)
       End

       Case RescaleDimType
       Of 1
         Self.iImg.Rescale(PixelWidth, PixelHeight, ResamplingFilter)
       Of 2
         If bMaintainAspectRatio
            Self.iImg.Rescale(PercentWidth, ResamplingFilter)
         Else
            Self.iImg.Rescale(PercentWidth/100*Self.iImg.GetWidth(), PercentHeight/100*Self.iImg.GetHeight(), ResamplingFilter)
         End
       End
       Self.iImgCtrl.Draw(CFIDRAW_ERASEBKG)
       bApplied = True
       Disable(?ApplyButton)
     End


    Of ?CancelButton
      If Event() = Event:Accepted
        Post(Event:CloseWindow)
      End
    End
    Case Event()

    Of EVENT:CloseWindow
      If bOk = False And bApplied
        originalImage.iImage.CopyImage(Self.iImg)
        Self.iImgCtrl.Draw(CFIDRAW_ERASEBKG)
      End

    End

  End
  Close(Window)
  Return bOk


AspectControlState  Routine
        Case RescaleDimType
        Of 1
          Disable(?PercentSizeGroup)
          Enable(?PixelSizeGroup)

          ?PixelHeightPrompt{Prop:Disable} = bMaintainAspectRatio
          ?PixelHeight{Prop:Disable} = bMaintainAspectRatio
        Of 2
          Enable(?PercentSizeGroup)
          Disable(?PixelSizeGroup)

          ?PercentHeightPrompt{Prop:Disable} = bMaintainAspectRatio
          ?PercentHeight{Prop:Disable} = bMaintainAspectRatio
        End


!--------------------------------------------------------------------------
cfiDialogsClass.RotateCenterDialog    Procedure(<String sTitle>)!,Bool,Virtual
!--------------------------------------------------------------------------
previewImage    cfiImageControl
bufferImage     FreeImageClass
originalImage   FreeImageClass

bApplied        BOOL,Auto


NewAngle Real
RotationDirection Long(ROTATECLOCKWISE)
bOK      BOOL(False)

 !Edit the window in the .trn file to localize or modify the window
 Include('cfiImgCt.trn', 'cfiRotateCenterDialogWindow'),Once

 Code
  If Self.iImg &= Null Or Self.iImgCtrl &= Null Then Return bOk; End

  Open(Window)
  If Not Omitted(2)
    Window{Prop:Text} = Clip(sTitle)
  End
  SetCursor(Cursor:Wait)
  previewImage.iImageControl.Init(?ImageControl, CFIBS_SUNKEN, CFISB_NONE)

  !Make a copy of the image passed scaled to the preview width
  Self.iImgCtrl.FitTo(previewImage, CFIFIT_WIDTH, FILTER_BICUBIC)
  previewImage.iImageControl.Draw()
  previewImage.iImage.CopyImage(bufferImage.iImage)
  Self.iImg.CopyImage(originalImage.iImage)
  bApplied = False

 SetCursor

  Accept
    Case Field()

    Of ?RotationDirection
      If Event() = Event:Accepted
        Post(EVENT:UpdatPreviewImage)
      End

    Of ?OKButton
      If Event() = Event:Accepted
        bOK = True
        Post(Event:Accepted, ?ApplyButton)
        Post(Event:CloseWindow)
      End

    Of ?NewAngle
      Case Event()
      Of Event:NewSelection OrOf Event:Accepted
        Post(EVENT:UpdatPreviewImage)
      End


    Of ?ApplyButton
     If Event() = Event:Accepted
       If bApplied
         originalImage.iImage.CopyImage(Self.iImg)
       End
       Self.iImg.Rotate(Choose(RotationDirection, -NewAngle, NewAngle))
       Self.iImgCtrl.Draw(CFIDRAW_ERASEBKG)
       bApplied = True
       Disable(?ApplyButton)
     End


    Of ?CancelButton
      If Event() = Event:Accepted
        originalImage.iImage.CopyImage(Self.iImg)
        Self.iImgCtrl.Draw(CFIDRAW_ERASEBKG)
        Post(Event:CloseWindow)
      End
    End

    Case Event()
    Of EVENT:CloseWindow
      If bOK = False And bApplied
        originalImage.iImage.CopyImage(Self.iImg)
        Self.iImgCtrl.Draw(CFIDRAW_ERASEBKG)
      End

    Of EVENT:UpdatPreviewImage
      Enable(?ApplyButton)
      bufferImage.iImage.CopyImage(previewImage.iImage)
      If NewAngle <> 0.0
        previewImage.iImage.Rotate(Choose(RotationDirection, -NewAngle, NewAngle))
      End
      previewImage.iImageControl.Draw(CFIDRAW_ERASEBKG)
    End
  End
  Close(Window)
  Return bOk

!--------------------------------------------------------------------------
cfiDialogsClass.RotateSkewDialog    Procedure(<String sTitle>)!,Bool,Virtual
!--------------------------------------------------------------------------
previewImage    cfiImageControl
bufferImage     FreeImageClass
originalImage   FreeImageClass

bApplied        BOOL,Auto

NewAngle Real
RotationDirection Long(ROTATECLOCKWISE)

xShift      Real(0.0)   !coordinates from upper left corner
yShift      Real(0.0)
xOrigin     Real(0.0)
yOrigin     Real(0.0)
bUseMask    BOOL(False) !True=use black mask, False=interpolate image for new background

PreviewZoom Real
bOk         BOOL(False)



 !Edit the window in the .trn file to localize or modify the window
 Include('cfiImgCt.trn', 'cfiRotateSkewDialogWindow'),Once

 Code
  If Self.iImg &= Null Or Self.iImgCtrl &= Null Then Return bOk; End

  Open(Window)
  If Not Omitted(2)
    Window{Prop:Text} = Clip(sTitle)
  End
  SetCursor(Cursor:Wait)
  previewImage.iImageControl.Init(?ImageControl, CFIBS_SUNKEN, CFISB_NONE)

  !Make a copy of the image passed scaled to the preview width
  Self.iImgCtrl.FitTo(previewImage, CFIFIT_WIDTH, FILTER_BICUBIC)
  previewImage.iImageControl.Draw()
  previewImage.iImage.CopyImage(bufferImage.iImage)
  Self.iImg.CopyImage(originalImage.iImage)
  bApplied = False
  PreviewZoom = Self.iImg.GetWidth()/previewImage.iImage.GetWidth()
  SetCursor

  Accept
    Case Field()

    Of ?PreviewButton
      If Event() = Event:Accepted
        Post(EVENT:UpdatPreviewImage)
      End

    Of ?OKButton
      If Event() = Event:Accepted
        bOK = True
        Post(Event:Accepted, ?ApplyButton)
        Post(Event:CloseWindow)
      End

    Of ?ApplyButton
     If Event() = Event:Accepted
       If bApplied
         originalImage.iImage.CopyImage(Self.iImg)
       End
       Self.iImg.Rotate(Choose(RotationDirection, -NewAngle, NewAngle), |
                    xShift, yShift,  |
                    xOrigin, yOrigin,|
                    bUseMask)
       Self.iImgCtrl.Draw(CFIDRAW_ERASEBKG)
       bApplied = True
       Disable(?ApplyButton)
     End


    Of ?CancelButton
      If Event() = Event:Accepted
        Post(Event:CloseWindow)
      End
    End

    Case Event()
    Of EVENT:CloseWindow
      If bOK = False And bApplied
        originalImage.iImage.CopyImage(Self.iImg)
        Self.iImgCtrl.Draw(CFIDRAW_ERASEBKG)
      End

    Of EVENT:UpdatPreviewImage
      Enable(?ApplyButton)
      bufferImage.iImage.CopyImage(previewImage.iImage)
      If NewAngle <> 0.0
        previewImage.iImage.Rotate(Choose(RotationDirection, -NewAngle, NewAngle), |
                            xShift/PreviewZoom, yShift/PreviewZoom,  |
                            xOrigin/PreviewZoom, yOrigin/PreviewZoom,|
                            bUseMask)
      End
      previewImage.iImageControl.Draw(CFIDRAW_ERASEBKG)
    End
  End
  Close(Window)
  Return bOk


!--------------------------------------------------------------------------
cfiDialogsClass.MetadataDialog    Procedure(<String sTitle>)!,Bool,Virtual
!--------------------------------------------------------------------------
bOk             BOOL(False)
MetadataQ       Queue(MetaDataQueue)
                End

MetadataModelQ  Queue
Description       String(32)
Model             FREE_IMAGE_MDMODEL
                End
MetaDataTag     String(128)
nMetaElements   Long,Auto


 !Edit the window in the .trn file to localize or modify the window
 Include('cfiImgCt.trn', 'cfiMetaDataDialogWindow'),Once

 Code
  If Self.iImg &= Null Or Self.iImgCtrl &= Null Then Return bOk; End

  MetadataModelQ.Description = CFI_METACOMMENTS
  MetadataModelQ.Model = FIMD_COMMENTS
  Add(MetadataModelQ)

  MetadataModelQ.Description = CFI_METAEXIFTIFF
  MetadataModelQ.Model = FIMD_EXIF_MAIN
  Add(MetadataModelQ)

  MetadataModelQ.Description = CFI_METAEXIFEXIF
  MetadataModelQ.Model = FIMD_EXIF_EXIF
  Add(MetadataModelQ)

  MetadataModelQ.Description = CFI_METAEXIFGPS
  MetadataModelQ.Model = FIMD_EXIF_GPS
  Add(MetadataModelQ)

  MetadataModelQ.Description = CFI_METAEXIFMAKER
  MetadataModelQ.Model = FIMD_EXIF_MAKERNOTE
  Add(MetadataModelQ)

  MetadataModelQ.Description =  CFI_METAEXIFINTER
  MetadataModelQ.Model = FIMD_EXIF_INTEROP
  Add(MetadataModelQ)

  MetadataModelQ.Description = CFI_METAIPCT
  MetadataModelQ.Model = FIMD_IPTC
  Add(MetadataModelQ)

  MetadataModelQ.Description = CFI_METAADOBEXMP
  MetadataModelQ.Model = FIMD_XMP
  Add(MetadataModelQ)

  MetadataModelQ.Description = CFI_METAGEOTIFF
  MetadataModelQ.Model = FIMD_GEOTIFF
  Add(MetadataModelQ)

  MetadataModelQ.Description = CFI_METAANIMATION
  MetadataModelQ.Model = FIMD_ANIMATION
  Add(MetadataModelQ)

  MetadataModelQ.Description = CFI_METACUSTOM
  MetadataModelQ.Model = FIMD_CUSTOM
  Add(MetadataModelQ)
  Get(MetadataModelQ, 1)


  Open(Window)
  nMetaElements = Self.iImg.EnumMetadata(MetaDataQ)
  Select(?MetadataModelList, 1)
  Accept
    Case Event()
    Of Event:OpenWindow
      If nMetaElements = 0
        Message(CFI_NOELEMENTSTR, CFI_MSGTITLE, Icon:Exclamation)
      End
    End

    Case Field()
    Of ?MetadataModelList
      If Event() = Event:Accepted
        Get(MetadataModelQ, Choice(?MetadataModelList))
      End

    Of ?GetMetadataButton
      If Event() = Event:Accepted
         Message(Self.iImg.GetMetadata(MetadataModelQ.Model, Clip(MetadataTag)), CFI_MSGTITLE)
      End

    Of ?OkButton
      If Event() = Event:Accepted
        Post(Event:CloseWindow)
      End
    End

  End
  Close(Window)
  Return bOK







