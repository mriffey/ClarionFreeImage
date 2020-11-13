!ABCIncludeFile(SAAPI)
!============================================================
! SaBrFdCl.inc -- Browse for folder Dialog Class header file
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

  OMIT('_EndOfSaBrowseForDialogClass_',_SaBrowseForDialogClassIncluded_)
_SaBrowseForDialogClassIncluded_  EQUATE(1)

  INCLUDE('saWApi.inc','Defines'),ONCE


! Browsing for directory.
SA_BIF_NONE               EQUATE(0)
SA_BIF_RETURNONLYFSDIRS   EQUATE(0001h)  ! For finding a folder to start document searching
SA_BIF_DONTGOBELOWDOMAIN  EQUATE(0002h)  ! For starting the Find Computer
SA_BIF_STATUSTEXT         EQUATE(0004h)  ! Top of the dialog has 2 lines of text for BROWSEINFO.lpszTitle and one line if
                                         ! this flag is set.  Passing the message BFFM_SETSTATUSTEXTA to the hwnd can set the
                                         ! rest of the text.  This is not used with BIF_USENEWUI and BROWSEINFO.lpszTitle gets
                                         ! all three lines of text.
SA_BIF_RETURNFSANCESTORS  EQUATE(0008h)
SA_BIF_EDITBOX            EQUATE(0010h)  ! Add an editbox to the dialog
SA_BIF_VALIDATE           EQUATE(0020h)  ! insist on valid result (or CANCEL)

SA_BIF_NEWDIALOGSTYLE     EQUATE(0040h)  ! Use the new dialog layout with the ability to resize
                                        ! Caller needs to call OleInitialize() before using this API

SA_BIF_USENEWUI           EQUATE(SA_BIF_NEWDIALOGSTYLE + SA_BIF_EDITBOX)

SA_BIF_BROWSEINCLUDEURLS  EQUATE(0080h)  ! Allow URLs to be displayed or entered. (Requires BIF_USENEWUI)

SA_BIF_BROWSEFORCOMPUTER  EQUATE(1000h)  ! Browsing for Computers.
SA_BIF_BROWSEFORPRINTER   EQUATE(2000h)  ! Browsing for Printers
SA_BIF_BROWSEINCLUDEFILES EQUATE(4000h)  ! Browsing for Everything
SA_BIF_SHAREABLE          EQUATE(8000h)  ! sharable resources displayed (remote shares, requires BIF_USENEWUI)

! message from browser
SA_BFFM_INITIALIZED        EQUATE(1)
SA_BFFM_SELCHANGED         EQUATE(2)
SA_BFFM_VALIDATEFAILEDA    EQUATE(3)   ! lParam:szPath ret:1(cont),0(EndDialog)
SA_BFFM_VALIDATEFAILEDW    EQUATE(4)   ! lParam:wzPath ret:1(cont),0(EndDialog)

! messages to browser
SA_BFFM_SETSTATUSTEXT      EQUATE(EVENT:USER + 100)
SA_BFFM_ENABLEOK           EQUATE(EVENT:USER + 101)
SA_BFFM_SETSELECTIONA      EQUATE(EVENT:USER + 102)
SA_BFFM_SETSELECTIONW      EQUATE(EVENT:USER + 103)
SA_BFFM_SETSTATUSTEXTW     EQUATE(EVENT:USER + 104)

! registry entries for special paths are kept in :
SA_REGSTR_PATH_SPECIAL_FOLDERS    EQUATE('\\Shell Folders')
                        Itemize(1)
BRWFLD_ERR_UNKNOWN_CSIDL    Equate
                        End

BRWFLD_DEBUG    Equate(true)
!=============================================================================
!=============================================================================
BrowseForDialogClass   Class,Type, Module('SaBrFdCl.clw'),|
                    LINK('SaBrFdCl.clw',_SALinkMode_), |
                    DLL(_SADLLMode_)

szDisplayName   CSTRING(264)

Folder          Procedure(String sTitle, String sInitialPath, ULong pCSIDL=0, ULong ulFlags=SA_BIF_NEWDIALOGSTYLE + SA_BIF_VALIDATE),String
ShowDialog      Procedure(String sTitle, String sInitialPath, ULong pCSIDL=0, ULong ulFlags=SA_BIF_NEWDIALOGSTYLE + SA_BIF_VALIDATE),String
OnCallDialog    Procedure(*SA_BROWSEINFO pbrwInfo),Virtual
GetDisplayName  Procedure(),String

TakeError   Procedure(Ulong nErrorCode),Protected,Virtual
                 End
!_EndOfSaBrowseForDialogClass_