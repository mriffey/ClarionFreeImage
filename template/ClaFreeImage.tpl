#TEMPLATE(ClarionFreeImage,'Clarion FreeImage Project, Version 3.15.5'),FAMILY('ABC'),Family('cw20') 
#!=========================================================================
#!2008.01.22 Family('cw20') added to support Capesoft write reports in Legacy
#!This does not mean that this template will work in Legacy
#!
#!2013.09.30 Capesoft Multi-Proj support updated
#!
#!=========================================================================
#EXTENSION(Activate_ClarionFreeImage,'Activate Clarion FreeImage for this Application'),APPLICATION
#!=========================================================================
#SHEET
  #TAB('Setup')
    #DISPLAY('This extension declares the classes and project')
    #DISPLAY('defines necessary to use the Clarion FreeImage')
    #DISPLAY('Project in this application. (ABC only)')
    #DISPLAY()
    #PROMPT('Include Dialog support',CHECK),%cfiIncludeDialogSupport,AT(10)
  #ENDTAB
#ENDSHEET
#! -------------------------------------------------
#! Add project defines.  Class headers must have
#! !ABCIncludeFile(SAAPI)
#! !ABCIncludeFile(FREEIMAGE)
#! respectively
#! -------------------------------------------------
#AT (%BeforeGenerateApplication)
  #! add project defines for _SALink/DLLMode_
  #! the freeimage classes links objects in the SA Windows API classes
  #CALL(%AddCategory(ABC),'SAAPI')
  #CALL(%SetCategoryLocation(ABC), 'SAAPI', 'SA')
  #! add project defines for _FreeImageLink/DLLMode_
  #CALL(%AddCategory(ABC),'FREEIMAGE')
  #CALL(%SetCategoryLocation(ABC), 'FREEIMAGE', 'FreeImage')
#ENDAT
#! -------------------------------------------------
#! Add the freeimage.dll's link libary to the project
#! -------------------------------------------------
#AT(%CustomGlobalDeclarations)
  #PROJECT('FreeImage.lib')
#ENDAT
#! -------------------------------------------------
#! Declare the class header files
#! -------------------------------------------------
#AT(%BeforeGlobalIncludes)
   !Include the Clarion FreeImage Control header files
   Include('cfiImgCt.inc'),Once  !image control
 #IF (%cfiIncludeDialogSupport = %True)
   Include('cfiDlgs.inc'),Once   !image control dialogs
 #ENDIF
#ENDAT
#! -------------------------------------------------
#! CapeSoft Multi-Proj support 20070712
#! -------------------------------------------------
#AT(%mpDefineAll)
  #IF(%GlobalExternal)
%#pragma define(_SADllMode_=>1)
%#pragma define(_SALinkMode_=>0)
%#pragma define(_FreeImageDllMode_=>1)
%#pragma define(_FreeImageLinkMode_=>0)
  #ELSE
%#pragma define(_SADllMode_=>0)
%#pragma define(_SALinkMode_=>1)
%#pragma define(_FreeImageDllMode_=>0)
%#pragma define(_FreeImageLinkMode_=>1)
  #ENDIF
#ENDAT
#! -------------------------------------------------
#AT(%mpDefineAll7)
  #IF(%GlobalExternal)
  %%3b_SADllMode_=&gt;1
  %%3b_SALinkMode_=&gt;0
  %%3b_FreeImageDllMode_=&gt;1
  %%3b_FreeImageLinkMode_=&gt;0
  #ELSE
  %%3b_SADllMode_=&gt;0
  %%3b_SALinkMode_=&gt;1
  %%3b_FreeImageDllMode_=&gt;0
  %%3b_FreeImageLinkMode_=&gt;1
  #ENDIF
#ENDAT
#! -------------------------------------------------
#At(%mpLibAll)
%#pragma link ("FreeImage.lib")
#ENDAT
#! -------------------------------------------------
#At(%mpLibAll7)
    <Library Include="FreeImage.Lib" />
#ENDAT
#! -------------------------------------------------
#! End of CapeSoft Multi-Proj support 20070712
#! -------------------------------------------------
#!
#!=========================================================================
#!=========================================================================
#!=========================================================================
#CONTROL(ClarionFreeImageControl, 'Clarion FreeImage Image Control'),Description('Clarion FreeImage ('&%ThisControlName &')'),REQ(Activate_ClarionFreeImage),MULTI
#!=========================================================================
#PREPARE
  #CALL(%ReadABCFiles(ABC))
  #CALL(%SetClassDefaults(ABC), 'cfiClassDefault', 'cfiObject' & %ActiveTemplateInstance, 'cfiImageControl')
  #CALL(%cfiGetControlName(ClarionFreeImage)),%ThisControlName
#ENDPREPARE
#INSERT(%OOPPrompts(ABC))
#SHEET
  #TAB('Setup ' )
    #BOXED(''),AT(0,0),WHERE(%False),HIDE
      #PROMPT('ControlName',FROM(%Control)),%ThisControlName
      #INSERT(%OOPHiddenPrompts)
      #PROMPT('BlobsInFile',@s32),%cfiBlobFields, MULTI('')
     #ENDBOXED
#PREPARE
  #CALL(%cfiLoadBlobFieldNames, %cfiBlobFile, %cfiBlobFields, %cfiBlobField)
#ENDPREPARE
    #BOXED('')
      #INSERT(%cfiControlStylePrompts)
    #ENDBOXED
    #INSERT(%cfiLoadImageFromPrompts)
  #ENDTAB
  #INSERT(%cfiPopupSheetControls)
  #TAB('Classes')
    #WITH(%ClassItem, 'cfiClassDefault')
      #INSERT(%ClassPrompts(ABC))
    #ENDWITH
  #ENDTAB
#ENDSHEET
 CONTROLS
   REGION,USE(?ImageCtrl)
 END
#! --------------------
#ATSTART
  #CALL(%ReadABCFiles(ABC))
  #CALL(%SetClassDefaults(ABC), 'cfiClassDefault', 'cfiObject' & %ActiveTemplateInstance, 'cfiImageControl')
  #EQUATE(%cfiDialogObject, %ThisObjectName & 'DialogMgr')
  #EQUATE(%cfiPopupObject,  %ThisObjectName & 'PopUpMgr')
#ENDAT
#! --------------------
#AT(%GenerateInstanceUpdate)
  #CALL(%AddModuleIncludeFile(ABC),%PopupClass)
#ENDAT
#! --------------------
#AT(%GatherObjects)
  #CALL(%ReadABCFiles(ABC))
  #CALL(%AddObjectList(ABC), 'cfiClassDefault')
#ENDAT
#! --------------------
#AT(%LocalDataClasses)
#CALL(%SetClassItem(ABC), 'cfiClassDefault')
#INSERT(%GenerateClassDefinition(ABC), %ClassLines)
  #IF (%cfiIncludeDialogSupport = %True)
%cfiDialogObject  cfiDialogsClass
  #ENDIF
  #IF(%cfiUseContextMenu = %True)
%cfiPopupObject   PopupClass
  #ENDIF
#ENDAT
#! --------------------
#AT(%cfiImageControlMethodCodeSection, %ActiveTemplateInstance), PRIORITY(5000), DESCRIPTION('Parent Call'), WHERE(%ParentCallValid())
  #CALL(%GenerateParentCall(ABC))
#ENDAT
#! --------------------
#AT(%cfiImageControlMethodCodeSection,%ActiveTemplateInstance,'OnMouseButtonUp','(SA_POINTS mousePos,Long mouseKeycode)')
 #IF (%cfiUseContextMenu = %True)
If mouseKeycode = MouseRight
  Case %cfiPopupObject.Ask()
  Of 'Load'
    If Self.iImage.Load('')
      Self.iImageControl.Reset()
    End
  Of 'SaveAs'
    If Self.iImage.SaveAs('')
      Self.iImageControl.Reset()
    End
  Of 'Rescale'
    #INSERT(%IfcfiDialogsSupported, 'RescaleDialog()')
  Of 'BrightContrast'
    #INSERT(%IfcfiDialogsSupported, 'BrightnessContrastDialog()')
  Of 'Gamma'
    #INSERT(%IfcfiDialogsSupported, 'GammaDialog()')
  Of 'Invert'
    If Self.iImage.Invert()
      Self.iImageControl.Draw()
    End
  Of 'RotateCenter'
    #INSERT(%IfcfiDialogsSupported, 'RotateCenterDialog()')
  Of 'RotateSkew'
    #INSERT(%IfcfiDialogsSupported, 'RotateSkewDialog()')
  Of 'RotateSkew'
    #INSERT(%IfcfiDialogsSupported, 'RotateSkewDialog()')
  Of 'Flip'
    Self.iImage.Flip(FI_FLIPHORIZ)
    Self.iImageControl.Reset()

  Of 'Mirror'
    Self.iImage.Flip(FI_FLIPVERT)
    Self.iImageControl.Reset()
 #ENDIF
  #PRIORITY(7500),DESCRIPTION('Case Popup.Ask')

  #PRIORITY(7700)
 #IF (%cfiUseContextMenu = %True)

  End
End 
 #ENDIF
#ENDAT
#! --------------------
#AT(%WindowManagerMethodCodeSection,'Init'),PRIORITY(8020)
#INSERT(%cfiAddPopupMenuItems)
#ENDAT
#! --------------------
#AT(%WindowManagerMethodCodeSection,'Kill'),PRIORITY(4800)
 #IF(%cfiUseContextMenu = %True)
%cfiPopupObject.Kill()
 #ENDIF
#ENDAT
#! --------------------
#AT(%WindowManagerMethodCodeSection,'Init'),PRIORITY(8030)
If %ThisObjectName.iImageControl.Init(%ThisControlName, %CFIBorderStyle, %CFIScrollbars) = True
 #IF (%cfiIncludeDialogSupport = %True)
  %cfiDialogObject.Init(%ThisObjectName.iImageControl, %ThisObjectName.iImage)
 #ENDIF
 #IF(%cfiLoadImageFrom <> 'Don''t Load')
   #IF(%cfiLoadImageFrom = 'BLOB')
  If %ThisObjectName.iImage.Load(%cfiBlobField) = True
   #ENDIF
   #IF(%cfiLoadImageFrom = 'Disk File')
     #IF(%ImageFilename)
  If %ThisObjectName.iImage.Load(%ImageFilename) = True
     #ELSE
  If %ThisObjectName.iImage.Load('') = True
     #ENDIF
   #ENDIF
    %ThisObjectName.iImageControl.Reset()
  End
 #ENDIF
Else !image control failed to initialize, this is a fatal error
  Return Level:Fatal
End
#ENDAT
#! --------------------
#AT(%LocalProcedures)
  #CALL(%GenerateVirtuals(ABC), 'cfiClassDefault', 'Local Objects|Abc Objects|Clarion FreeImage Controls(' & %ThisControlName &')', '%cfiImageControlEmbedVirtuals(ClarionFreeImage)' )
#ENDAT
#!
#!
#!=========================================================================
#GROUP(%cfiImageControlEmbedVirtuals, %TreeText, %DataText, %CodeText)
#!=========================================================================
#EMBED(%cfiImageControlMethodDataSection, 'FreeImage method data section'), %ActiveTemplateInstance, %pClassMethod, %pClassMethodPrototype, LABEL, DATA, Tree(%TreeText & %DataText)
  #?CODE
  #EMBED(%cfiImageControlMethodCodeSection, 'FreeImage method code section'), %ActiveTemplateInstance, %pClassMethod, %pClassMethodPrototype, TREE(%TreeText & %CodeText)
#!
#!=========================================================================
#GROUP(%ParentCallValid),AUTO
#!=========================================================================
#DECLARE(%RVal)
#CALL(%ParentCallValid(ABC)),%RVal
#SET(%RVal, %True)
#RETURN(%RVal)

#!=========================================================================
#CODE(SetZoomFilter,'Set the rescaling filter used for zooming'),Description('Set the rescaling filter used for zooming (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#DISPLAY('')
#PROMPT('Zoom Filter', Drop('Box[FILTER_BOX]|Bicubic[FILTER_BICUBIC]|Bilinear[FILTER_BILINEAR]|BSpline[FILTER_BSPLINE]|Catmull-Rom[FILTER_CATMULLROM]|Lanczos3[FILTER_LANCZOS3]')),%ZoomFilter, DEFAULT( 'FILTER_BOX' )
#DISPLAY('')
%ThisObjectName.iImageControl.SetZoomFilter(%ZoomFilter)
#!
#!=========================================================================
#CODE(LoadImageFromFile,'Load the image from a file'),Description('Load the image from a file (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#DISPLAY('Leave the filename variable blank to call the file open dialog.')
#DISPLAY('')
#PROMPT('Variable for filename', Field),%FileNameVariable
#DISPLAY('')
#INSERT(%cfiDrawPromptText)
#IF(%FileNameVariable = '')
If %ThisObjectName.iImage.Load('') = True
#ELSE
If %ThisObjectName.iImage.Load(%FileNameVariable) = True
#ENDIF
 #IF(%cfiRedrawImage)
  %ThisObjectName.iImageControl.Reset()
 #ENDIF
End
#!
#!=========================================================================
#CODE(LoadImageFromBLOB,'Load the image from a BLOB'),Description('Load the image from a BLOB (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#INSERT(%cfiLoadImageFromBLOB)
#!
#!=========================================================================
#CODE(SaveImageToBLOB,'Save the image in a BLOB'),Description('Save the image in a BLOB (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#INSERT(%cfiSaveImageToBLOB)
#!
#!=========================================================================
#CODE(SaveAsFileDialog,'Save the image to a file'),Description('Save the image to a file (' & %ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#INSERT(%cfiSaveAsFileDialog)
#!
#!=========================================================================
#CODE(AdjustBrightnessContrastDialog,'Call the dialog to adjust brightness and contrast'),Description('Call the dialog to adjust brightness and contrast (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#INSERT(%IfcfiDialogsSupported, 'BrightnessContrastDialog()')
#!
#!=========================================================================
#CODE(AdjustGammaDialog,'Call the dialog to adjust the gamma'),Description('Call the dialog to adjust the gamma (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#INSERT(%IfcfiDialogsSupported, 'GammaDialog()')
#!
#!=========================================================================
#CODE(RotateCenterDialog,'Call the dialog to rotate the image about a center point'),Description('Call the dialog to rotate the image about a center point (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#INSERT(%IfcfiDialogsSupported, 'RotateCenterDialog()')
#!
#!=========================================================================
#CODE(RotateSkewDialog,'Call the dialog to rotate and skew the image'),Description('Call the dialog to rotate and skew the image (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#INSERT(%IfcfiDialogsSupported, 'RotateSkewDialog()')
#!
#!=========================================================================
#CODE(RescaleDialog,'Call the dialog to rescale the image'),Description('Call the dialog to rescale the image (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#PREPARE
  #SET(%cfiRedrawImage, %True)
#ENDPREPARE
#INSERT(%cfiDrawPromptText)
  #IF (%cfiIncludeDialogSupport = %True)
If %cfiDialogObject.RescaleDialog() = True
 #IF(%cfiRedrawImage)
  %ThisObjectName.iImageControl.Draw()
 #ENDIF
End
  #ELSE
    #INSERT(%cfiDialogSupportErrorMsg)
  #ENDIF
#!
#!=========================================================================
#CODE(MetadataDialog,'Call the dialog to display the metadata for the image'),Description('Call the dialog to display the metadata for the image (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#INSERT(%DialogsSupported, 'MetaDataDialog()')
#!
#!=========================================================================
#CODE(InvertImage,'Invert the colors of the loaded image.'),Description('Invert the colors of the loaded image (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#INSERT(%cfiDrawPromptText)
If %ThisObjectName.iImage.Invert()
 #IF(%cfiRedrawImage)
  %ThisObjectName.iImageControl.Draw()
 #ENDIF
End
#!
#!=========================================================================
#CODE(BeginCropSelection,'Start selecting an area with the mouse to crop image'),Description('Start selecting an area with the mouse to crop image (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
%ThisObjectName.BeginCropSelection()
#!
#!=========================================================================
#CODE(FlipImage,'Flip the image horizontally or vertically'),Description('Flip the image horizontally or vertically (' &%ThisControlName &')'), REQ(ClarionFreeImageControl)
#!=========================================================================
#PROMPT('Select direction to flip: ',Drop('Horizontal[FI_FLIPHORIZ]|Vertically[FI_FLIPVERT]')),%FlipHorV
#INSERT(%cfiDrawPromptText)
%ThisObjectName.iImage.Flip(%FlipHorV)
 #IF(%cfiRedrawImage)
%ThisObjectName.iImageControl.Reset()
 #ENDIF
#!
#!
#!=========================================================================
#!=========================================================================
#CONTROL(cfiSignatureControl, 'Clarion FreeImage Signature Control'),Description('Clarion FreeImage Signature('&%ThisControlName &')'),REQ(Activate_ClarionFreeImage),MULTI
#!=========================================================================
#PREPARE
  #CALL(%ReadABCFiles(ABC))
  #CALL(%SetClassDefaults(ABC), 'cfiSignatureClassDefault', 'cfiSigObject' & %ActiveTemplateInstance, 'cfiSignatureControl')
  #CALL(%cfiGetControlName(ClarionFreeImage)),%ThisControlName
#ENDPREPARE
#INSERT(%OOPPrompts(ABC))
#SHEET
  #TAB('Setup ' )
    #BOXED(''),AT(0,0),WHERE(%False),HIDE
      #PROMPT('ControlName',FROM(%Control)),%ThisControlName
      #INSERT(%OOPHiddenPrompts)
      #PROMPT('BlobsInFile',@s32),%cfiBlobFields, MULTI('')
     #ENDBOXED
#PREPARE
  #CALL(%cfiLoadBlobFieldNames, %cfiBlobFile, %cfiBlobFields, %cfiBlobField)
#ENDPREPARE
    #BOXED('')
      #INSERT(%cfiControlStylePrompts)
    #ENDBOXED
    #BOXED('')
      #INSERT(%cfiGdiPenPrompts)
    #ENDBOXED
    #INSERT(%cfiLoadImageFromPrompts)
  #ENDTAB
  #TAB('Classes')
    #WITH(%ClassItem, 'cfiSignatureClassDefault')
      #INSERT(%ClassPrompts(ABC))
    #ENDWITH
  #ENDTAB
#ENDSHEET
 CONTROLS
   REGION,USE(?SignatureCtrl)
 END
#! --------------------
#ATSTART
  #CALL(%ReadABCFiles(ABC))
  #CALL(%SetClassDefaults(ABC), 'cfiSignatureClassDefault', 'cfiSigObject' & %ActiveTemplateInstance, 'cfiSignatureControl')
#ENDAT
#! --------------------
#AT(%GatherObjects)
  #CALL(%ReadABCFiles(ABC))
  #CALL(%AddObjectList(ABC), 'cfiSignatureClassDefault')
#ENDAT
#! --------------------                                                                              
#AT(%LocalDataClasses)
  #CALL(%SetClassItem(ABC), 'cfiSignatureClassDefault')
#INSERT(%GenerateClassDefinition(ABC), %ClassLines)
#ENDAT
#! --------------------
#AT(%cfiSignatureControlMethodCodeSection, %ActiveTemplateInstance), PRIORITY(5000), DESCRIPTION('Parent Call'), WHERE(%ParentCallValid())
  #CALL(%GenerateParentCall(ABC))
#ENDAT
#! --------------------
#AT(%WindowManagerMethodCodeSection,'Init'),PRIORITY(8030)
If %ThisObjectName.iImageControl.Init(%ThisControlName, %CFIBorderStyle, %CFIScrollbars) = True
 #IF(%cfiLoadImageFrom <> 'Don''t Load')
   #IF(%cfiLoadImageFrom = 'BLOB')
  If %ThisObjectName.iImage.Load(%cfiBlobField) = True
   #ENDIF
   #IF(%cfiLoadImageFrom = 'Disk File')
     #IF(%ImageFilename)
  If %ThisObjectName.iImage.Load(%ImageFilename) = True
     #ELSE
  If %ThisObjectName.iImage.Load('') = True
     #ENDIF
   #ENDIF
    %ThisObjectName.iImageControl.Reset()
  Else
    #INSERT(%cfiCreateNewImageToControlSize)
  End
 #ELSE
  #INSERT(%cfiCreateNewImageToControlSize)
 #ENDIF
Else !image control failed to initialize, this is a fatal error
  Return Level:Fatal
End
 #IF(%cfiGdiPenWidth <> '')
%ThisObjectName.iPen.SetPenWidth(%cfiGdiPenWidth)
 #ENDIF
 #IF(%cfiGdiPenColor <> '')
%ThisObjectName.iPen.SetPenColor(%cfiGdiPenColor)
 #ENDIF
 #IF(%cfiGdiPenStyle <> '')
%ThisObjectName.iPen.SetPenStyle(%cfiGdiPenStyle)
 #ENDIF
#ENDAT
#! --------------------
#AT(%LocalProcedures)
  #CALL(%GenerateVirtuals(ABC), 'cfiSignatureClassDefault', 'Local Objects|Abc Objects|Clarion FreeImage Signature Controls(' & %ThisControlName &')', '%cfiSignatureControlEmbedVirtuals(ClarionFreeImage)' )
#ENDAT
#!
#!
#!=========================================================================
#GROUP(%cfiSignatureControlEmbedVirtuals, %TreeText, %DataText, %CodeText)
#!=========================================================================
#EMBED(%cfiSignatureControlMethodDataSection, 'FreeImage signature method data section'), %ActiveTemplateInstance, %pClassMethod, %pClassMethodPrototype, LABEL, DATA, Tree(%TreeText & %DataText)
  #?CODE
  #EMBED(%cfiSignatureControlMethodCodeSection, 'FreeImage signature method code section'), %ActiveTemplateInstance, %pClassMethod, %pClassMethodPrototype, TREE(%TreeText & %CodeText)
#!
#!=========================================================================
#GROUP(%cfiCreateNewImageToControlSize)
#!=========================================================================
0{Prop:Pixels} = True
If %ThisObjectName.iImage.NewImage(%ThisControlName{Prop:Width}-2,%ThisControlName{Prop:Height}-2, 24, FIF_TIFF)
  %ThisObjectName.iImage.Invert() !A new image is black (this will give the image a white background)
  %ThisObjectName.iImageControl.Reset()
End
0{Prop:Pixels} = False
#!
#!=========================================================================
#CODE(LoadSignatureFromBLOB,'Load the signature from a BLOB'),Description('Load the signature from a BLOB (' &%ThisControlName &')'), REQ(cfiSignatureControl)
#!=========================================================================
#INSERT(%cfiLoadImageFromBLOB)
#!
#!=========================================================================
#CODE(SaveSignatureToBLOB,'Save the Signature in a BLOB'),Description('Save the signature in a BLOB (' &%ThisControlName &')'), REQ(cfiSignatureControl)
#!=========================================================================
#INSERT(%cfiSaveImageToBLOB)
#!
#!=========================================================================
#CODE(SaveSignatureAsFileDialog,'Save the signature to a file'),Description('Save the signature to a file (' & %ThisControlName &')'), REQ(cfiSignatureControl)
#!=========================================================================
#INSERT(%cfiSaveAsFileDialog)
#!
#!=========================================================================
#CODE(SetSignaturePenWidth,'Set the pen width used to draw a signature'),Description('Set the pen width used to draw a signature (' & %ThisControlName &')'), REQ(cfiSignatureControl)
#!=========================================================================
  #PROMPT('Pen Width',Field),%cfiGdiPenWidth
%ThisObjectName.iPen.SetPenWidth(%cfiGdiPenWidth)
#!=========================================================================
#CODE(SetSignaturePenColor,'Set the pen color used to draw a signature'),Description('Set the pen color used to draw a signature (' & %ThisControlName &')'), REQ(cfiSignatureControl)
#!=========================================================================
  #PROMPT('Pen Color',Field),%cfiGdiPenColor
%ThisObjectName.iPen.SetPenColor(%cfiGdiPenColor)
#!
#!=========================================================================
#CODE(SetSignaturePenStyle,'Set the pen style used to draw a signature'),Description('Set the pen style used to draw a signature (' & %ThisControlName &')'), REQ(cfiSignatureControl)
#!=========================================================================
  #PROMPT('Pen Style',Field),%cfiGdiPenStyle
%ThisObjectName.iPen.SetPenStyle(%cfiGdiPenStyle)
#!
#!
#!=========================================================================
#!=========================================================================
#!========================== GROUPS =======================================
#!=========================================================================
#!=========================================================================
#!
#!=========================================================================
#GROUP(%cfiLoadImageFromBLOB)
#!=========================================================================
#BOXED(''),AT(0,0),WHERE(%False),HIDE
  #PROMPT('BlobsInFile',@s32),%cfiBlobFields, MULTI('Blob Fields')
#ENDBOXED
#PREPARE
  #CALL(%cfiLoadBlobFieldNames, %cfiBlobFile, %cfiBlobFields, %cfiBlobField)
#ENDPREPARE
#DISPLAY('')
#DISPLAY('')
#PROMPT('Blob file',File),%cfiBlobFile, WhenAccepted( CALL(%cfiLoadBlobFieldNames, %cfiBlobFile, %cfiBlobFields, %cfiBlobField)), REQ
#PROMPT('Blob field',From(%cfiBlobFields)), %cfiBlobField, REQ
#DISPLAY('')
#INSERT(%cfiDrawPromptText)
If %ThisObjectName.iImage.Load(%cfiBlobField) = True
 #IF(%cfiRedrawImage)
  %ThisObjectName.iImageControl.Reset()
 #ENDIF
End
#!
#!=========================================================================
#GROUP(%cfiSaveImageToBLOB)
#!=========================================================================
#BOXED(''),AT(0,0),WHERE(%False),HIDE
  #PROMPT('BlobsInFile',@s32),%cfiBlobFields, MULTI('Blob Fields')
#ENDBOXED
#PREPARE
 #CALL(%cfiLoadBlobFieldNames, %cfiBlobFile, %cfiBlobFields, %cfiBlobField)
#ENDPREPARE
#DISPLAY('This code template assumes that you have previously')
#DISPLAY('retrieved the record to save the image file into its BLOB.')
#DISPLAY('')
#PROMPT('Blob file',File),%cfiBlobFile, WhenAccepted( CALL(%cfiLoadBlobFieldNames, %cfiBlobFile, %cfiBlobFields, %cfiBlobField)), REQ
#PROMPT('Blob field',From(%cfiBlobFields)), %cfiBlobField, REQ
#PROMPT('Image format',Drop('Use Existing[FIF_UNKNOWN]|JPEG JFIF[FIF_JPEG]|TIFF[FIF_TIFF]|Portable Network Graphic (PNG)[FIF_PNG]|Windows Bitmap[FIF_BMP]|Compuserve GIF[FIF_GIF]|Adobe Photoshop[FIF_PSD]') ),%ImageFormat
#PROMPT('Update file if successful?',Check),%cfiUpdateFileOnSuccess
#DISPLAY('')
If %ThisObjectName.iImage.Save(%cfiBlobField, %ImageFormat) = True
 #IF(%cfiUpdateFileOnSuccess)
  If Access:%cfiBlobFile.Update() <> Level:Benign
    !not updated
  End
 #ENDIF
End
#!
#!=========================================================================
#GROUP(%cfiSaveAsFileDialog)
#!=========================================================================
#PROMPT('Display File dialog?',CHECK),%cfiDisplayFileDialog
#DISPLAY('Or use this Variable for the filename')
#ENABLE(%cfiDisplayFileDialog = %False)
#PROMPT('filename variable:', FROM(%LocalData)),%FileNameVariable,DEFAULT(''),REQ
#ENDENABLE
#!
#IF(%cfiDisplayFileDialog = %True)
%ThisObjectName.iImage.SaveAs('')
#ELSE
%ThisObjectName.iImage.SaveAs(%FileNameVariable)
#ENDIF
#!
#!=========================================================================
#GROUP(%cfiControlStylePrompts)
#!=========================================================================
  #PROMPT('Border style',DROP('None[CFIBS_NONE]|Boxed[CFIBS_LINE]|Sunken[CFIBS_SUNKEN]|Sunken Deep[CFIBS_SUNKENDEEP]')),%CFIBorderStyle, DEFAULT( 'CFIBS_NONE' )
  #PROMPT('Scrollbars',DROP('None[CFISB_NONE]|Both[CFISB_BOTH]|Vertical[CFISB_VERT]|Horizontal[CFISB_HORIZ]')),%CFIScrollbars, DEFAULT( 'CFISB_BOTH' )
#!
#!=========================================================================
#GROUP(%cfiLoadBlobFieldNames, * %pBlobFile, * %pBlobFields, * %pBlobField)
#!=========================================================================
#FREE(%pBlobFields)
#IF(%pBlobFile)
  #FIX(%File, %pBlobFile)
  #FOR(%Field),WHERE(%FieldType = 'BLOB')
    #ADD(%pBlobFields, %Field)
    #IF(%pBlobField = '')
      #SET(%pBlobField, %Field)
    #ENDIF
  #ENDFOR
  #FOR(%pBlobFields)
    #IF(%pBlobFields = %pBlobField)
      #BREAK
    #ENDIF
  #ENDFOR
  #IF(%pBlobFields <> %pBlobField)
    #SET(%pBlobField, '')
  #ENDIF
#ENDIF
#!
#!
#!=========================================================================
#GROUP(%IfcfiDialogsSupported, %CodeToInclude)
#!=========================================================================
  #IF (%cfiIncludeDialogSupport = %True)
%cfiDialogObject.%CodeToInclude
  #ELSE                                                    
    #INSERT(%cfiDialogSupportErrorMsg)
  #ENDIF
#!=========================================================================
#GROUP(%cfiDialogSupportErrorMsg)
#!=========================================================================
#ERROR('Please check the "Include Dialog support" box on the FreeImage global extension properties.')
#!
#!=========================================================================
#GROUP(%cfiDrawPromptText)
#!=========================================================================
#BOXED('')
#PROMPT('Redraw image after this code template',CHECK),%cfiRedrawImage ,AT(10)
#DISPLAY('')
#DISPLAY('You may perform many operations on an image before it''s')
#DISPLAY('displayed.  You need to check this on the last code')
#DISPLAY('template.  It will generate this code:')
#DISPLAY('    ' & %ThisObjectName& '.iImageControl.Draw()')
#ENDBOXED
#!
#!=========================================================================
#GROUP(%cfiGetControlName)
#!=========================================================================
  #FOR(%Control), WHERE(%ActiveTemplateInstance=%ControlInstance)
    #RETURN(%Control)
  #ENDFOR
  #RETURN('')
#!
#!=========================================================================
#GROUP(%cfiPopupSheetControls)
#!=========================================================================
  #TAB('Popup Menu')
    #PROMPT('Use popup menu?', Check), %cfiUseContextMenu
    #ENABLE(%cfiUseContextMenu = %True)
      #BOXED('Menu text')
        #DISPLAY('')
        #DISPLAY('Erase the menu text to remove an item from')
        #DISPLAY('the popup menu.')
        #PROMPT('Load:',@s64),%LoadPopupMenuStr, DEFAULT('&Load...')
        #PROMPT('Save As:',@s64),%SaveAsPopupMenuStr, DEFAULT('&Save As...')
        #PROMPT('Rescale:',@s64),%RescalePopupMenuStr, DEFAULT('Rescale...')
        #PROMPT('Brightness and Contrast:',@s64),%BrightContrastPopupMenuStr, DEFAULT('&Brightness and Contrast...')
        #PROMPT('Gamma:',@s64),%GammaPopupMenuStr, DEFAULT('&Gamma...')
        #PROMPT('Invert Colors:',@s64),%InvertPopupMenuStr, DEFAULT('&Invert colors')
        #PROMPT('Rotate about center:',@s64),%RotateCenterPopupMenuStr, DEFAULT('&Rotate...')
        #PROMPT('Rotate and skew:',@s64),%RotateSkewPopupMenuStr, DEFAULT('Rotate and S&kew...')
        #PROMPT('Flip:',@s64),%FlipPopupMenuStr, DEFAULT('&Flip')
        #PROMPT('Mirror:',@s64),%MirrorPopupMenuStr, DEFAULT('&Mirror')
      #ENDBOXED
    #ENDENABLE      
  #ENDTAB
#!
#!=========================================================================
#GROUP(%cfiAddPopupMenuItems)
#!=========================================================================
 #IF(%cfiUseContextMenu = %True)

%cfiPopupObject.Init()
   #IF(%LoadPopupMenuStr <> '')
%cfiPopupObject.AddItem('%LoadPopupMenuStr', 'Load')
   #ENDIF
   #IF(%SaveAsPopupMenuStr <> '')
%cfiPopupObject.AddItem('%SaveAsPopupMenuStr', 'SaveAs')
   #ENDIF
   #IF(%SaveAsPopupMenuStr <> '' Or %LoadPopupMenuStr <> '')
%cfiPopupObject.AddItem('-','FileSep')
   #ENDIF
   #IF(%RescalePopupMenuStr <> '')
%cfiPopupObject.AddItem('%RescalePopupMenuStr', 'Rescale')
%cfiPopupObject.AddItem('-','RescaleSep')
   #ENDIF
   #IF(%BrightContrastPopupMenuStr <> '')
%cfiPopupObject.AddItem('%BrightContrastPopupMenuStr', 'BrightContrast')
   #ENDIF
   #IF(%GammaPopupMenuStr <> '')
%cfiPopupObject.AddItem('%GammaPopupMenuStr', 'Gamma')
   #ENDIF
   #IF(%InvertPopupMenuStr <> '')
%cfiPopupObject.AddItem('%InvertPopupMenuStr', 'Invert')
   #ENDIF
   #IF(%BrightContrastPopupMenuStr <> '' Or %GammaPopupMenuStr <> '' Or %InvertPopupMenuStr <> '')
%cfiPopupObject.AddItem('-','ColorSep')
   #ENDIF
   #IF(%RotateCenterPopupMenuStr <> '')
%cfiPopupObject.AddItem('%RotateCenterPopupMenuStr', 'RotateCenter')
   #ENDIF
   #IF(%RotateSkewPopupMenuStr <> '')
%cfiPopupObject.AddItem('%RotateSkewPopupMenuStr', 'RotateSkew')
   #ENDIF
   #IF(%FlipPopupMenuStr <> '')
%cfiPopupObject.AddItem('%FlipPopupMenuStr', 'Mirror')
   #ENDIF
   #IF(%MirrorPopupMenuStr <> '')
%cfiPopupObject.AddItem('%MirrorPopupMenuStr', 'Flip')
   #ENDIF
#!   #IF(%RotateCenterPopupMenuStr <> '' Or %RotateSkewPopupMenuStr <> '' Or %FlipPopupMenuStr <> '' Or %MirrorPopupMenuStr <> '')
#!%cfiPopupObject.AddItem('-','RotateSep')
#!   #ENDIF
 #ENDIF
#!
#!=========================================================================
#GROUP(%cfiLoadImageFromPrompts)
#!=========================================================================
    #BOXED('Load image from this data source when window opens:')
      #PROMPT('Load Image from:',DROP('Disk File|BLOB|Don''t Load')),%cfiLoadImageFrom
      #ENABLE(%cfiLoadImageFrom = 'Disk File')
        #BOXED('Disk File')
          #PROMPT('Variable for filename',Field),%ImageFilename
          #DISPLAY('Leave this field blank and the object will')
          #DISPLAY('display the file open dialog.')
        #ENDBOXED
      #ENDENABLE
      #ENABLE(%cfiLoadImageFrom = 'BLOB')
        #BOXED('BLOB')
          #PROMPT('Blob file',File),%cfiBlobFile, WhenAccepted( CALL(%cfiLoadBlobFieldNames, %cfiBlobFile, %cfiBlobFields, %cfiBlobField)),REQ
          #PROMPT('Blob field',From(%cfiBlobFields)),%cfiBlobField, REQ
        #ENDBOXED
      #ENDENABLE
   #ENDBOXED
#!
#!=========================================================================
#GROUP(%cfiGdiPenPrompts)
#!=========================================================================
  #PROMPT('Pen Color',Field),%cfiGdiPenColor
  #PROMPT('Pen Width',Field),%cfiGdiPenWidth
  #PROMPT('Pen Style',Field),%cfiGdiPenStyle
#!
#!=========================================================================
#!
