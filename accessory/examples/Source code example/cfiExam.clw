  Program
!============================================================
! cfiExam.clw -- Clarion FreeImage Project example program
!
!  Copyright © 2005 - 2009 Sand & Associates, Larry@sand-associates.com
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


  Include('cfiImgCt.inc'),Once  !the image control
  Include('cfiDlgs.inc'),Once   !the image control dialogs
  Include('keycodes.clw'),Once  !Clarion keycode equates
  include('safilecl.inc'),Once
  
  Map
HelpAbout                   Procedure()
ImageWindow                 Procedure(String sFilename) !Pass filename of image or '' for file dialog
GetColorChannels            Procedure(*cfiImageControl cfiIC)      
JPEGTransfoms               Procedure()
  End

AppFrame APPLICATION('Clarion FreeImage Project Example'),AT(,,646,406),CENTER,MASK, |
         SYSTEM,MAX,ICON('pict.ico'),STATUS(-1,80,120,45),FONT('MS Sans Serif', |
         8,,FONT:regular),RESIZE
      MENUBAR,USE(?Menubar)
         MENU('&File'),USE(?FileMenu)
            ITEM('&New<9>Ctrl+N'),USE(?NewImage),KEY(CtrlN)
            ITEM('&Open...<9>Ctrl+O'),USE(?OpenImage),KEY(CtrlO)
            ITEM(' Lossless &JPEG transforms'),USE(?JPEGTransfoms)
            ITEM(''),SEPARATOR,USE(?SEPARATOR1),LAST
            ITEM('E&xit'),USE(?Exit),MSG('Exit this application'),STD(STD:Close),LAST
         END
         MENU('&Edit'),USE(?EditMenu)
            ITEM('Cu&t'),USE(?Cut),MSG('Remove item to Windows Clipboard'), |
                  STD(STD:Cut)
            ITEM('&Copy'),USE(?Copy),MSG('Copy item to Windows Clipboard'), |
                  STD(STD:Copy)
            ITEM('&Paste'),USE(?Paste),MSG('Paste contents of Windows Clipboard'), |
                  STD(STD:Paste)
         END
         MENU('&Window'),USE(?WindowMenu),STD(STD:WindowList)
            ITEM('T&ile'),USE(?Tile),MSG('Make all open windows visible'), |
                  STD(STD:TileWindow)
            ITEM('&Cascade'),USE(?Cascade),MSG('Stack all open windows'), |
                  STD(STD:CascadeWindow)
            ITEM('&Arrange Icons'),USE(?Arrange),MSG('Align all window icons'), |
                  STD(STD:ArrangeIcons)
         END
         MENU('&Help'),USE(?HelpMenu)
            ITEM('&Contents'),USE(?Helpindex),MSG('View the contents of the help file'), |
                  STD(STD:HelpIndex)
            ITEM('&Search for Help On...'),USE(?HelpSearch),MSG('Search for help' & |
                  ' on a subject'),STD(STD:HelpSearch)
            ITEM('&How to Use Help'),USE(?HelpOnHelp),MSG('How to use Windows Help'), |
                  STD(STD:HelpOnHelp)
            ITEM(''),SEPARATOR,USE(?SEPARATOR2)
            ITEM('&About...'),USE(?HelpAbout)
         END
      END
   END


  Code

  Open(AppFrame)
  Accept
    Case Accepted()
    Of ?HelpAbout
       HelpAbout()

    Of ?OpenImage
       Start(ImageWindow, 5000,'')

    Of ?NewImage
      Start(ImageWindow, 5000,'~N~')

    Of ?JPEGTransfoms
      Start(JPEGTransfoms, 5000)
    End   
    
      

  End
  Close(AppFrame)

  Return


!-------------------------------------------------------------------------------------------------
JPEGTransfoms      Procedure()
!-------------------------------------------------------------------------------------------------
fi FreeImageClass                       
srcFile        String(260)
dstFile        String(260)                      
transSrcFile   String(260)
transDstFile   String(260)                      
x              Long
y              Long
nWidth         Long
nHeight        Long
operation      Long                           
                           
W  WINDOW('Clarion FreeImage Lossless JPEG Transforms'),AT(,,380,283),MDI,GRAY,SYSTEM, |
         MAX,FONT('MS Sans Serif',8,,FONT:regular),RESIZE
      GROUP('JPEG Crop'),AT(21,15,332,135),USE(?Crop),BOXED
         PROMPT('Source File:'),AT(32,37),USE(?SrcFilePrompt)
         ENTRY(@s255),AT(92,33,204),USE(srcFile)
         BUTTON('...'),AT(301,33,17),USE(?CropSrcFileLookup)
         PROMPT('Destination File:'),AT(32,54),USE(?DestFilePrompt)
         ENTRY(@s255),AT(92,50,204),USE(dstFile)
         BUTTON('...'),AT(301,50,17,12),USE(?CropDstFileLookup)
         GROUP('Area to Crop'),AT(32,80,311,33),USE(?CropGroup),BOXED
            PROMPT('X:'),AT(96,95),USE(?XcoordPrompt)
            ENTRY(@N_4),AT(109,92),USE(X)
            PROMPT('Y:'),AT(147,95),USE(?YcoordPROMPT)
            ENTRY(@N_4),AT(159,92),USE(Y)
            PROMPT('Width:'),AT(206,95),USE(?WidthPrompt)
            ENTRY(@N_4),AT(233,92),USE(nWidth)
            PROMPT('Height:'),AT(274,94),USE(?HeightPrompt)
            ENTRY(@N_4),AT(301,91),USE(nHeight)
         END
         BUTTON('Crop'),AT(91,122,253),USE(?CropButton)
      END
      GROUP('JPEG Transforms'),AT(21,162,331,111),USE(?JpegTransformsGroup),BOXED
         PROMPT('Source File:'),AT(32,185),USE(?TransSrcFilePrompt)
         ENTRY(@s255),AT(93,182,203,11),USE(TransSrcFile)
         BUTTON('...'),AT(301,181,17,14),USE(?transSrcFileLookup)
         PROMPT('Destination File:'),AT(32,202),USE(?TransDestFilePrompt)
         ENTRY(@s255),AT(93,198,203,11),USE(transDstFile)
         BUTTON('...'),AT(301,198,17,12),USE(?TransDstFileLookup)
         PROMPT('Operation:'),AT(31,218),USE(?OperationPrompt)
         LIST,AT(92,215,225,15),USE(?Transforms),DROP(8),FROM('Flip Horizontal|Flip V' & |
               'ertical|Transpose UL to LR|Transpose UR to LL|Rotate 90 Clockwis' & |
               'e|Rotate 180|Rotate 270 Clockwise')
         BUTTON('Transform'),AT(91,250,227),USE(?TransformButton)
      END
   END
  Code   
  Open(W)
  Accept 
   Case Accepted()
   !============ Crop ================
   Of ?CropButton
     if fi.iImage.JpegCrop(srcFile, dstFile, x, y, nWidth, nHeight) 
       Start(ImageWindow, 5000, Clip(dstFile))
     else
       Message('Cropping operation failed')
     end
     
   of ?CropSrcFileLookup   
     if srcFile = ''
       srcFile = Path()
     end  
     if FileDialog('Source File', srcFile, 'JPEG | *.jpg; *.jpeg | All Files *.* | *.*',  FILE:KeepDir +FILE:LongName)
       Display(?srcFile)
     end    
     
   of ?CropDstFileLookup  
     if dstFile = ''
       dstFile = Path()
     end  
     if FileDialog('Destination File', dstFile, 'JPEG | *.jpg; *.jpeg',  File:Save +FILE:KeepDir +FILE:LongName)
       Display(?dstFile)
     end                      
     
   !============ Transforms ================ 
   of ?TransformButton
     if fi.iImage.JpegTransform(TransSrcFile, TransDstFile, Operation)
       Start(ImageWindow, 5000, Clip(TransDstFile)) 
     else
       Message('Transform operation failed')
     end
     
   of ?TransSrcFileLookup   
     if TransSrcFile = ''
       transSrcFile = Path()
     end  
     if FileDialog('Source File', transSrcFile, 'JPEG | *.jpg; *.jpeg | All Files *.* | *.*',  FILE:KeepDir +FILE:LongName)
       Display(?TransSrcFile)
     end    
     
   of ?TransDstFileLookup  
     if transDstFile = ''
       transDstFile = Path()
     end  
     if FileDialog('Destination File', transDstFile, 'JPEG | *.jpg; *.jpeg',  File:Save +FILE:KeepDir +FILE:LongName)
       Display(?TransDstFile)
     end
     
   of ?Transforms
     Operation = Choice(?Transforms)      
     
     
   End    
   

  End
  Close(W)
  Return
  
           
           
           
!-------------------------------------------------------------------------------------------------
HelpAbout           Procedure()
!-------------------------------------------------------------------------------------------------
fi FreeImageClass
W    WINDOW('About Clarion FreeImage Project Example'),AT(,,243,122),FONT('MS Sans Serif',8,,),SYSTEM, |
         GRAY
       PROMPT('Clarion FreeImage Project example programs and classes Copyright <169> 2005-2009 By Sand ' &|
           '&& Associates, All Rights Reserved.  Portions copyright FreeImage project.  See ' &|
           'FreeImage copyright notice below.'),AT(11,10,220,38),USE(?Prompt2)
       PROMPT('Copyright'),AT(11,51,220,49),USE(?Copyright)
       BUTTON('OK'),AT(193,103,45,14),USE(?OKButton)
     END
  Code
  Open(W)
  ?Copyright{Prop:Text} = fi.iImage.GetCopyright() &'<10,10>FreeImage library version: '& fi.iImage.GetVersion()
  Accept
    If Field() = ?OkButton
       If Event() = Event:Accepted
         Post(Event:CloseWindow)
       End
    End
  End
  Close(W)
  Return


!-------------------------------------------------------------------------------------------------
ImageWindow Procedure(String sFilename)
!-------------------------------------------------------------------------------------------------
Window WINDOW('Clarion FreeImage Control'),AT(,,421,276),FONT('MS Sans Serif',8,,FONT:regular),SYSTEM, |
         GRAY,MAX,RESIZE,MDI
       MENUBAR
         MENU('&File'),USE(?File)
           ITEM('&Save...<9>Ctrl+S'),USE(?FileSave),KEY(CtrlS)
           ITEM('Save &As...'),USE(?FileSaveAs)
         END
         MENU('&View'),USE(?View)
           MENU('&Zoom'),USE(?ViewZoom)
             ITEM('25%'),USE(?ViewZoom25)
             ITEM('50%'),USE(?ViewZoom50)
             ITEM('75%'),USE(?ViewZoom75)
             ITEM('100%'),USE(?ViewZoom100)
             ITEM('125%'),USE(?ViewZoom125)
             ITEM('150%'),USE(?ViewZoom150)
             ITEM('200%'),USE(?ViewZoom200)
             ITEM('300%'),USE(?ViewZoom300)
             ITEM('400%'),USE(?ViewZoom400)
             ITEM('500%'),USE(?ViewZoom500)
             ITEM('600%'),USE(?ViewZoom600)
           END
           MENU('&Set Zoom resampling filter'),USE(?ViewSetZoomFilter)
             ITEM('Box'),USE(?ViewSetZoomFilterBox)
             ITEM('Bicubic'),USE(?ViewSetZoomFilterBicubic)
             ITEM('Bilinear'),USE(?ViewSetZoomFilterBilinear)
             ITEM('B-Spline'),USE(?ViewSetZoomFilterBSpline)
             ITEM('Catmull-Rom'),USE(?ViewSetZoomFilterCatmullRom)
             ITEM('Lanczos3'),USE(?ViewSetZoomFilterLanczos3)
           END
         END
         MENU('&Image'),USE(?Image)
           ITEM('&Flip'),USE(?ImageFlip)
           ITEM('&Mirror'),USE(?ImageMirror)
           MENU('Ro&tate'),USE(?ImageRotate)
             ITEM('Rotate about &center point...'),USE(?ImageRotateCenter)
             ITEM('Rotate and &skew...'),USE(?ImageRotateSkew)
           END
           ITEM,SEPARATOR
           ITEM('&Rescale...'),USE(?ImageRescale)
           ITEM('&Fit To Control'),USE(?ImageFitTo)
           ITEM('&Fit To Width and Height'),USE(?ImageFitToWidthHeight)
           Item('&Thumbnail...'),USE(?Thumbnail)
           ITEM('&Crop'),USE(?ImageCrop)
           ITEM('&Alpha Blend'),USE(?ImageAlphaBlend)
           ITEM,SEPARATOR
           ITEM('Meta&data (EXIF and other models)'),USE(?ImageMetadata)
           ITEM,SEPARATOR
           ITEM('&Copy'),USE(?ImageCopy)
           ITEM('&Paste'),USE(?ImagePaste)
           ITEM,SEPARATOR                 
           ITEM(' Add Border'),USE(?ImageBorder)
         END 
         MENU('&Colors'),USE(?Colors)
           MENU('&Adjust'),USE(?ColorsAdjust)
             ITEM('&Brightness/Contrast...'),USE(?ColorAdjustBrightnessContrast)
             ITEM('&Gamma...'),USE(?ColorAdjustGamma)
             ITEM('&Invert...'),USE(?ColorAdjustInvert)
           END
           ITEM('&Grayscale, 256 shades'),USE(?ColorGrayscale)
           ITEM('&Split and display Color Channels...'),USE(?ColorsChannelSplit)
           MENU('Color &Depth')
             ITEM('&32 bit'),USE(?ColorDepth32bit)
             ITEM('&24 bit 16 million colors'),USE(?ColorDepth24bit)
             ITEM('1&6 bit True color'),USE(?ColorDepth16bit)
             ITEM('&8 bit 256 colors quantized palette'),USE(?ColorDepth8bit)
             ITEM('&4 bit Grayscale, 16 shades'),USE(?ColorDepth4bit)
             ITEM('&1 bit Black and white'),USE(?ColorDepth1bit)
           END
         END
       END
       region,AT(0,0),USE(?ImageControl),FULL
     END


theImage    cfiImageControl
cfiDialogs  cfiDialogsClass
csrc        &FreeImageClass  !reference to a FreeImage object to manipulate an image without displaying it on a control
  Code

  Open(Window)

  If theImage.iImageControl.Init(?ImageControl, CFIBS_SUNKEN) = False
    Message('Unable to initialize the image control', 'Clarion FreeImage project example',Icon:Exclamation)
    Return
  End

  If sFilename <> '~N~'
    If theImage.iImage.Load(sFileName) = True
      theImage.iImageControl.Reset()
    End
  Else
    ! create a 10x10 24 bit BMP image
    ! ToDo create a "New Image" dialog to prompt for parameters.
    If theImage.iImage.NewImage(10,10,24,FIF_BMP) = True
      theImage.iImageControl.Reset()
    End
  End
  cfiDialogs.Init(theImage.iImageControl, theImage.iImage)
  Accept

    Case Field()

    Of ?FileSave
      If Event() = Event:Accepted
        theImage.iImage.Save()
      End
    Of ?FileSaveAs
      If Event() = Event:Accepted
        theImage.iImage.SaveAs('')
      End

    Of ?ViewSetZoomFilterBox
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFilter(FILTER_BOX)
      End

    Of ?ViewSetZoomFilterBicubic
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFilter(FILTER_BICUBIC)
      End

    Of ?ViewSetZoomFilterBilinear
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFilter(FILTER_BILINEAR)
      End
    Of ?ViewSetZoomFilterBSpline
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFilter(FILTER_BSPLINE)
      End

    Of ?ViewSetZoomFilterCatmullRom
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFilter(FILTER_CATMULLROM)
      End

    Of ?ViewSetZoomFilterLanczos3
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFilter(FILTER_LANCZOS3)
      End

    Of ?ViewZoom25
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(0.25)
      End
    Of ?ViewZoom50
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(0.50)
      End
    Of ?ViewZoom75
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(0.75)
      End
    Of ?ViewZoom100
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(1.0)
      End
    Of ?ViewZoom125
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(1.25)
      End
    Of ?ViewZoom150
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(1.50)
      End
    Of ?ViewZoom200
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(2.0)
      End
    Of ?ViewZoom300
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(3.0)
      End
    Of ?ViewZoom400
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(4.0)
      End
    Of ?ViewZoom500
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(5.0)
      End
    Of ?ViewZoom600
      If Event() = Event:Accepted
        theImage.iImageControl.SetZoomFactor(6.0)
      End



    Of ?ImageFlip
      If Event() = Event:Accepted
        theImage.iImage.Flip(FI_FLIPVERT)
        theImage.iImageControl.Draw()
      End

    Of ?ImageMirror
      If Event() = Event:Accepted
        theImage.iImage.Flip(FI_FLIPHORIZ)
        theImage.iImageControl.Draw()
      End

    Of ?ImageRotateCenter
      If Event() = Event:Accepted
        cfiDialogs.RotateCenterDialog()
      End

    Of ?ImageRotateSkew
      If Event() = Event:Accepted
        cfiDialogs.RotateSkewDialog()
      End

    Of ?ImageRescale
      If Event() = Event:Accepted
        If cfiDialogs.RescaleDialog() = True
          theImage.iImageControl.Draw()
        End
      End

    Of ?ImageFitTo
      If Event() = Event:Accepted
        theImage.iImageControl.FitTo()
        theImage.iImageControl.Reset()
      End     
                  
    Of ?ImageFitToWidthHeight              
      If Event() = Event:Accepted
         theImage.iImage.FitTo(600, 400, FILTER_BILINEAR, CFIFIT_WIDTH, 400, False, False) 
         theImage.iImageControl.Reset()
      End                             
      
    Of ?Thumbnail
      If Event() = Event:Accepted
        If theImage.iImage.Thumbnail(100) !fit image into an n pixel square     
          theImage.iImageControl.Reset()
        End  
      End   
      
    Of ?ImageCrop
      If Event() = Event:Accepted
        theImage.SC.ISelection.SetAction(CFISELECTACTION_CROP)
      End

    Of ?ImageMetadata
      If Event() = Event:Accepted
        cfiDialogs.MetaDataDialog()
      End

    Of ?ImageAlphaBlend
      If Event()= Event:Accepted
        csrc &= New FreeImageClass  !Create a freeimage object for the sub image
        If Not csrc &= Null
          If csrc.iImage.Load('') = True !Display file open dialog to load the sub image
            If theImage.iImage.AlphaBlend(csrc.iImage, 0, 0, 127) = True !about 50% transparent
              theImage.iImageControl.Draw()
            Else
              Message('The image you''re attempting to alpha blend is either too large or the|'&|
                      'destination color depth is too low.  The main image must be at least same|'  &|
                      'color depth and dimension as the sub image.  Select another image to blend', |
                      'Clarion FreeImage Project',Icon:Exclamation)
            End
          End
          Dispose(csrc)
        End
      End

    Of ?ImageCopy
      If Event() = Event:Accepted
        theImage.iImageControl.CopyToClipboard()
      End
                                                                                   
    Of ?ImagePaste
      If Event() = Event:Accepted
        theImage.iImageControl.PasteFromClipboard()   
      End
 
    Of ?ImageBorder
      If Event() = Event:Accepted
        theImage.iImage.ResizeCanvas(5,5,5,5, Color:black) !Add a 5 pixel border to image 
        theImage.iImageControl.Draw()
      End

    Of ?ColorAdjustBrightnessContrast
      If Event() = Event:Accepted
        cfiDialogs.BrightnessContrastDialog()
        theImage.iImageControl.Draw()
      End

    Of ?ColorAdjustGamma
      If Event() = Event:Accepted
        If cfiDialogs.GammaDialog() = True
          theImage.iImageControl.Draw()
        End

      End

    Of ?ColorsChannelSplit
      If Event() = Event:Accepted
        GetColorChannels(theImage)
      End

    Of ?ColorAdjustInvert
      If Event() = Event:Accepted
        theImage.iImage.Invert()
        theImage.iImageControl.Draw()
      End

    Of ?ColorGrayscale
      If Event() = Event:Accepted
        theImage.iImage.ConvertToGrayscale()
        theImage.iImageControl.Draw()
      End

    Of ?ColorDepth32bit
      If Event() = Event:Accepted
        theImage.iImage.ConvertColorDepth(FI_32BIT, ,)
        theImage.iImageControl.Draw()
      End
    Of ?ColorDepth24bit
      If Event() = Event:Accepted
        theImage.iImage.ConvertColorDepth(FI_24BIT, ,)
        theImage.iImageControl.Draw()
      End
    Of ?ColorDepth16bit
      If Event() = Event:Accepted
        theImage.iImage.ConvertColorDepth(FI_16BIT, ,)
        theImage.iImageControl.Draw()
      End
    Of ?ColorDepth8bit
      If Event() = Event:Accepted
        theImage.iImage.ConvertColorDepth(FI_8BIT, FIQ_WUQUANT, )
        theImage.iImageControl.Draw()
      End
    Of ?ColorDepth4bit
      If Event() = Event:Accepted
        theImage.iImage.ConvertColorDepth(FI_4BIT, ,)
        theImage.iImageControl.Draw()
      End
    Of ?ColorDepth1bit
      If Event() = Event:Accepted
        theImage.iImage.ConvertColorDepth(FI_1BIT, , )
        theImage.iImageControl.Draw()
      End

    End

  End
  Close(Window)
  Return







!-------------------------------------------------------------------------------------------------
GetColorChannels            Procedure(*cfiImageControl cfiIC)
!-------------------------------------------------------------------------------------------------
redImage        cfiImageControl
blueImage       cfiImageControl
greenImage      cfiImageControl


Window WINDOW('Separate Color Channels'),AT(,,397,301),FONT('MS Sans Serif',8,,),SYSTEM,GRAY
       PROMPT('Red:'),AT(10,5),USE(?Prompt1)
       PROMPT('Blue:'),AT(207,4),USE(?Prompt2)
       REGION,AT(9,15,176,130),USE(?RedImageControl)
       REGION,AT(208,15,176,130),USE(?BlueImageControl)
       PROMPT('Green:'),AT(9,150),USE(?Prompt3)
       REGION,AT(9,160,176,130),USE(?GreenImageControl)
       BUTTON('Ok'),AT(338,277,45,14),USE(?OkButton)
     END

 Code

  Open(Window)
  SetCursor(Cursor:Wait)
  redImage.iImageControl.Init(?RedImageControl, CFIBS_SUNKEN, CFISB_NONE)
  blueImage.iImageControl.Init(?BlueImageControl, CFIBS_SUNKEN, CFISB_NONE)
  greenImage.iImageControl.Init(?GreenImageControl, CFIBS_SUNKEN, CFISB_NONE)

  cfiIC.iImage.GetChannel(redImage.iImage, FICC_RED)
  cfiIC.iImage.GetChannel(blueImage.iImage, FICC_BLUE)
  cfiIC.iImage.GetChannel(greenImage.iImage, FICC_GREEN)
  !------------------------------------------------------------------
  ! you shouldn't create rescaled versions of the color channels
  ! if you need the 8 bit DIB to merge with another image
  ! the FitTo method calls the rescale method and that automatically
  ! converts it to a 24 bit image.
  !------------------------------------------------------------------
  redImage.iImageControl.FitTo(redImage, CFIFIT_WIDTH, FILTER_BICUBIC)
  blueImage.iImageControl.FitTo(blueImage, CFIFIT_WIDTH, FILTER_BICUBIC)
  greenImage.iImageControl.FitTo(GreenImage, CFIFIT_WIDTH, FILTER_BICUBIC)
  redImage.iImageControl.Draw()
  blueImage.iImageControl.Draw()
  GreenImage.iImageControl.Draw()

 SetCursor

  Accept
    Case Field()
    Of ?OKButton
      If Event() = Event:Accepted
        Post(Event:CloseWindow)
      End
    End
  End
  Close(Window)
  Return








