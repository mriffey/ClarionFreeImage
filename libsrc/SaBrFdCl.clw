!============================================================
! SaBrFdCl.clw -- Shell browse for folders Class implementation
!
!  Copyright © 2006 Sand & Associates, Larry@sand-associates.com
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
!============================================================

  Member
  INCLUDE('SaBrFdCl.inc'),ONCE
  MAP
    INCLUDE('SaWApi.inc','Prototypes'),ONCE

BrowseCallbackProc  PROCEDURE(SA_HWND hwnd, |
                              UNSIGNED uMsg, |
                              LONG lParam,   |
                              LONG lpData    |
                             ),LONG,PASCAL   
  END



!------------------------------------------------------------------------------
! Browse for a folder
!------------------------------------------------------------------------------
BrowseForDialogClass.Folder  Procedure(String sTitle, |
                                       String sInitialPath, |
                                       ULong pCSIDL=0, |
                                       ULong ulFlags=SA_BIF_NEWDIALOGSTYLE + SA_BIF_VALIDATE)!,String,Virtual

!------------------------------------------------------------------------------
  Code
  Return Self.ShowDialog(sTitle, sInitialPath, pCSIDL, ulFlags)




!------------------------------------------------------------------------------
! Call the shell functions to setup and show the browse for folders dialog
!------------------------------------------------------------------------------
BrowseForDialogClass.ShowDialog  Procedure(String sTitle, |
                                       String sInitialPath, |
                                       ULong pCSIDL=0, |
                                       ULong ulFlags=SA_BIF_NEWDIALOGSTYLE + SA_BIF_VALIDATE)!,String,Virtual
!------------------------------------------------------------------------------
pidl            Long
brwInfo         LIKE(SA_BROWSEINFO)
CoInitResult    SA_HRESULT
ParhWnd         SA_HWND
szInitalPath    CSTRING(File:MaxFilePath+1)
szTitle         CSTRING(256)
szPath          CSTRING(264)

  Code
  ParhWnd = 0{PROP:Handle}
  CoInitResult = SA_CoInitialize()
  If CoInitResult = SA_S_OK OR CoInitResult = SA_S_FALSE
    szInitalPath = Clip(sInitialPath)
    szTitle = Clip(sTitle)
    Clear(brwInfo)
    If SA_SHGetSpecialFolderLocation(ParhWnd, pCSIDL, brwInfo.pidlRoot) <> SA_S_OK
      Clear(brwInfo.pidlRoot)
      Self.TakeError(BRWFLD_ERR_UNKNOWN_CSIDL)
    End

    brwInfo.hwndOwner = ParhWnd
    brwInfo.pszDisplayName = Address(Self.szDisplayName)
    brwInfo.lpszTitle = Address(szTitle)
    brwInfo.ulFlags = ulFlags
    brwInfo.lpfn = Address(BrowseCallbackProc)
    If szInitalPath <> ''
      brwInfo.lParam = Address(szInitalPath)
    Else
      brwInfo.lParam = 0
    End

    Self.OnCallDialog(brwInfo)
    UnlockThread()
    pidl = SA_SHBrowseForFolder(brwInfo)
    LockThread()

    IF pidl
      SA_SHGetPathFromIDList(pidl, szPath)
      SA_CoTaskMemFree(pidl)
    END
    SA_CoTaskMemFree(brwInfo.pidlRoot)

    SA_CoUninitialize()
  END
  Return szPath



!------------------------------------------------------------------------------
! Derive this method to change any of the BROWSEINFO parameters before the
! ShBrowseForFolder() function is called.  For instance, you could change the
! Callback function pointer to point to your own callback with different
! functionality.
!------------------------------------------------------------------------------
BrowseForDialogClass.OnCallDialog  Procedure(*SA_BROWSEINFO pbrwInfo)!,Virtual
!------------------------------------------------------------------------------
  Code
  Return



!------------------------------------------------------------------------------
! Return the display name.  Valid after a call to ShowDialog
!------------------------------------------------------------------------------
BrowseForDialogClass.GetDisplayName  Procedure()!,String
!------------------------------------------------------------------------------
  Code
  Return Self.szDisplayName

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
BrowseForDialogClass.TakeError    Procedure(Ulong nErrorCode)!,Protected,Virtual
!------------------------------------------------------------------------------
sError  Cstring(256)
  Code
  Case nErrorCode
  Of BRWFLD_ERR_UNKNOWN_CSIDL
    sError = 'Unknown CSIDL.|Please look up correct values on MSDN.|Using Desktop instead.'
  End

  If BRWFLD_DEBUG
    Message(sError,'Browse Folder Dialog Class', Icon:Exclamation)
  End
  Return




!=======================================================================
! SHBrowseForFolder callback function.  This callback implements the
! setting of an initial folder.
!=======================================================================
BrowseCallbackProc  PROCEDURE(UNSIGNED hWnd,  |
                              UNSIGNED uMsg,  |
                              LONG lParam,    |
                              LONG lpData     |
                             )
  CODE
  CASE uMsg
  OF SA_BFFM_INITIALIZED
    If lpData <> 0
      SA_SendMessage(hWnd, SA_BFFM_SETSELECTIONA, True, lpData)
    End
  END
  RETURN 0