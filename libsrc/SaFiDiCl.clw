!============================================================
! SaFiDiCl.clw -- File Dialog Class implementation
!
!  Copyright © 2005 -2007 Sand & Associates, Larry@sand-associates.com
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
! 2007.03.19    Fix buffer over-run in SetDialogFilterNulls
!
! 2006.01.31    Added code to dispose szFilter when SetDialogFilter() called
!               more than once.
!============================================================

  Member
  INCLUDE('SaFiDiCl.inc'),ONCE
  MAP
    INCLUDE('SaWApi.inc','Prototypes'),ONCE
  END



!--------------------------------------------------------------------------
! Expects a correctly formed file dialog filter string in the Clarion format.  
! That is, the description/extension pairs separated by "|" characters.  This
! method copies the string into a Cstring and replaces the "|" characters
! with null characters <0> and then terminates it with a double null <0,0>
! as required by the windows api
!--------------------------------------------------------------------------
FileDialogClass.SetDialogFilter    Procedure(String sFilter)
!--------------------------------------------------------------------------
   Code
   If Not Self.szFilter &= Null
     Dispose(Self.szFilter)
   End
   Self.szFilter &= New Cstring(Len(Clip(sFilter)) +2) !+2 is for the terminating double null
   If Self.szFilter &= Null
     Return
   End

   Self.szFilter = Clip(sFilter)
   Self.SetDialogFilterNulls(Self.szFilter)
   Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FileDialogClass.SetDialogFilterNulls  Procedure(*CString szFilter)
!--------------------------------------------------------------------------
i   Long,Auto
FilterStrLength Long,Auto
   Code
   FilterStrLength = Len(szFilter)
   If FilterStrLength+2 < Size(szFilter)
     Return
   End
   Loop i = 1 to FilterStrLength
    If szFilter[i] = '|'
      szFilter[i] = '<0>'
    End
   End
   szFilter[i : i+1] = '<0,0>'
   Return

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FileDialogClass.SetDialogOptions        Procedure(ULong ofn_Options)
!--------------------------------------------------------------------------
  Code
  Self.OptionFlags = BOR(Self.OptionFlags, ofn_Options) !See OFN_ flags on MSDN
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FileDialogClass.SetDefaultPath          Procedure(String sPath)
!--------------------------------------------------------------------------
  Code
  Self.szDefaultPath = Clip(sPath)
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FileDialogClass.Show    Procedure(<String sTitle>, |
                                  Long ClarionOptionFlags=0, |
                                  Long InitialFilterIndex=1, |
                                  <ULong ofn_Options>)!,Bool,Virtual
!--------------------------------------------------------------------------
bSaveDialog     BOOL,Auto
bResult         BOOL,Auto
lcl_Ofn_Options ULong,Auto
ofn             Like(SA_OPENFILENAME)
  Code

  If Band(ClarionOptionFlags, FILE:Directory) > 0
   !use BrowseForDialogClass instead, see SaBrFdCl.inc/clw
   Return False
  End


  bResult = False
  If Self.szFilter &= Null
    Self.SetDialogFilter('All Files|*.*')
    If Self.szFilter &= Null
      Return bResult
    End
  End

  SA_ZeroMemory(ofn, Size(ofn))

  !------Save or Open File Dialog---------------------------------
  bSaveDialog = Choose(Band(ClarionOptionFlags, FILE:Save)<>0)
  lcl_Ofn_Options = 0
  If bSaveDialog = True
    lcl_Ofn_Options = Bor(lcl_Ofn_Options, SA_OFN_OVERWRITEPROMPT)           
  Else
    lcl_Ofn_Options = Bor(lcl_Ofn_Options, SA_OFN_FILEMUSTEXIST)
  End


  !---- Set the Clarion comapatible options ----
  If Band(ClarionOptionFlags, FILE:KeepDir) > 0
    lcl_Ofn_Options = Bor(lcl_Ofn_Options, SA_OFN_NOCHANGEDIR)
  End

  If Band(ClarionOptionFlags, FILE:NoError) > 0
    If bSaveDialog = True
      lcl_Ofn_Options = Choose(Band(lcl_Ofn_Options, SA_OFN_OVERWRITEPROMPT)>0, Bxor(lcl_Ofn_Options, SA_OFN_OVERWRITEPROMPT), lcl_Ofn_Options)
    Else
      lcl_Ofn_Options = Choose(Band(lcl_Ofn_Options, SA_OFN_FILEMUSTEXIST)>0, Bxor(lcl_Ofn_Options, SA_OFN_FILEMUSTEXIST), lcl_Ofn_Options)
    End
  End

  If Band(ClarionOptionFlags, FILE:Multi) > 0
    lcl_Ofn_Options = Bor(lcl_Ofn_Options, SA_OFN_ALLOWMULTISELECT)
  End

  If Band(ClarionOptionFlags, FILE:LongName) > 0
    lcl_Ofn_Options = Bor(lcl_Ofn_Options, SA_OFN_LONGNAMES)
  End


  Self.SetDialogOptions(lcl_Ofn_Options)


  !---- Set custom title if one is supplied ----
  If Not Omitted(2)
    Self.szDialogTitle = Clip(sTitle)
  End

  !---- Set additional dialog box options, see msdn for documentation ----
  If Not Omitted(5)
    Self.SetDialogOptions(ofn_Options)
  End

  Self.szFullFilename=''

  !---- Set the initial path for the file dialog, if none was specified ----
  !     Use the current path
  If Self.szDefaultPath = ''
    Self.SetDefaultPath(Path())
  End

  ofn.lStructSize = SA_OPENFILENAME_SIZE_VERSION_400 !SIZE(SA_OPENFILENAME)-12
  ofn.hwndOwner = 0{Prop:Handle}

  ofn.lpstrFilter = Address(Self.szFilter)

  ofn.lpstrCustomFilter = 0
  ofn.nMaxCustFilter = 0

  Self.nFilterIndex = InitialFilterIndex
  ofn.nFilterIndex = Self.nFilterIndex


  ofn.lpstrFileTitle = Address(Self.szFileTitle)
  ofn.nMaxFileTitle = Size(Self.szFileTitle)

  ofn.lpstrInitialDir = Address(Self.szDefaultPath)
  ofn.lpstrTitle = Address(Self.szDialogTitle)

  ofn.Flags = Self.OptionFlags

  ofn.nFileOffset = 0
  ofn.nFileExtension = 0
  ofn.lpstrDefExt = Address(Self.szDefaultExt)

  ofn.lpstrFile = Address(Self.szFullFilename)
  ofn.nMaxFile = Size(Self.szFullFilename)

  Self.OnCallDialog(ofn)
  If bSaveDialog = True
    unlockthread()
    bResult = SA_GetSaveFileName(ofn)
    lockthread()
    If bResult = False
      Self.TakeError(SA_CommDlgExtendedError())
    End
  Else
    unlockthread()
    bResult = SA_GetOpenFileName(ofn)
    lockthread()
    If bResult = False
      Self.TakeError(SA_CommDlgExtendedError())
    End
  End
  Self.nFilterIndex = ofn.nFilterIndex !this is the index of selected filter
  Self.nFileOffset = ofn.nFileOffset
  Self.nFileExtension = ofn.nFileExtension

  Return bResult


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FileDialogClass.OnCallDialog    Procedure(*SA_OPENFILENAME ofn)!,Virtual
  Code
  !Derive this method to make any changes before the open/save dialog is called
  Return


!--------------------------------------------------------------------------
! Returns just the filename without extension.  See GetFileTitle() for the
! filename with the extension.
!--------------------------------------------------------------------------
FileDialogClass.GetFilename             Procedure()!,String
!--------------------------------------------------------------------------
nFilenameOffset Long,Auto
  Code
  nFilenameOffset = Self.nFileOffset+1
  If Self.nFileExtension <> 0
    Return Sub(Self.szFullFilename, nFilenameOffset, Self.nFileExtension - nFilenameOffset)
  Else
    Return Sub(Self.szFullFilename, nFilenameOffset, Len(Self.szFullFilename-1) - nFilenameOffset)
  End
!--------------------------------------------------------------------------
! Returns the file extension.  Returns an empty string if the last character
! of the filename is a "." or there is no extension.
!--------------------------------------------------------------------------
FileDialogClass.GetFileExtension             Procedure()!,String
!--------------------------------------------------------------------------
nFilenExtensionOffset Long,Auto
  Code
  nFilenExtensionOffset = Self.nFileExtension+1
  If Self.nFileExtension <> 0
    Return Sub(Self.szFullFilename, nFilenExtensionOffset, Len(Self.szFullFilename))
  Else
    Return ''
  End
!--------------------------------------------------------------------------
! Returns Drive:\path or \\sharename\path
!--------------------------------------------------------------------------
FileDialogClass.GetPath             Procedure()!,String
!--------------------------------------------------------------------------
  Code
  Return Sub(Self.szFullFilename, 1, Self.nFileOffset-1)


!--------------------------------------------------------------------------
!Returns Drive:\path\filename.ext or \\sharename\path\filename.ext
!--------------------------------------------------------------------------
FileDialogClass.GetPathAndFilename             Procedure()!,String
!--------------------------------------------------------------------------
  Code
  Return Self.szFullFilename


!--------------------------------------------------------------------------
! GetFileTitle returns the filename.ext
!--------------------------------------------------------------------------
FileDialogClass.GetFileTitle             Procedure()!,String
!--------------------------------------------------------------------------
  Code
  Return self.szFileTitle
   


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FileDialogClass.TakeError    Procedure(Ulong nErrorCode)!,Protected,Virtual
!--------------------------------------------------------------------------
cbRequiredBuffer Ushort, Over(Self.szFullFilename)
sErrMsg String(256)
  Code
  Case nErrorCode
  Of SA_FNERR_BUFFERTOOSMALL
    !If this error is thrown by the dialog cbRequiredBuffer is
    !the minimum size required.
    sErrMsg = 'The filename buffer is too small.|'&|
              'Increase the size of the szFullFilename|'&|
              'property to at least: '&cbRequiredBuffer&' characters.'


  Of SA_CDERR_DIALOGFAILURE
    sErrMsg = 'Dialog Failure'
  Of SA_CDERR_FINDRESFAILURE
    sErrMsg = 'Find resource failure'
  Of SA_CDERR_NOHINSTANCE
    sErrMsg = 'No HInstance'
  Of SA_CDERR_INITIALIZATION
    sErrMsg = 'Initialization error'
  Of SA_CDERR_NOHOOK
    sErrMsg = 'No Hook'
  Of SA_CDERR_LOCKRESFAILURE
    sErrMsg = 'Lock resource failure'
  Of SA_CDERR_NOTEMPLATE
    sErrMsg = 'No Template'
  Of SA_CDERR_LOADRESFAILURE
    sErrMsg = 'Load Resource failure'
  Of SA_CDERR_STRUCTSIZE
    sErrMsg = 'Structure size problem'
  Of SA_CDERR_LOADSTRFAILURE
    sErrMsg = 'Load String failure'
  Of SA_CDERR_MEMALLOCFAILURE
    sErrMsg = 'Unable to allocate memory'
  Of SA_FNERR_INVALIDFILENAME
    sErrMsg = 'Invalid file name'
  Of SA_CDERR_MEMLOCKFAILURE
    sErrMsg = 'Unable to lock memory'
  Of SA_FNERR_SUBCLASSFAILURE
    sErrMsg = 'Unable to create subclass'
  End

  If Self.bDebug = True
    Message(sErrMsg,'File Dialog Class', Icon:Exclamation)
  End

  Return



!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FileDialogClass.Construct   Procedure()
  Code
  Self.bDebug = FDC_DEBUGSTATE
  Self.szFullFilename = ''
  Self.szDialogTitle  = ''
  Self.szFileTitle    = ''
  Self.szDefaultPath  = ''
  Self.szDefaultExt   = ''
  Self.OptionFlags    = 0
  Self.nFilterIndex   = 0
 
  Return


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
FileDialogClass.Destruct    Procedure()
  Code
  Dispose(Self.szFilter)
  Return
