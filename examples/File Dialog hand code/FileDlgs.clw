  Program
!============================================================
! FileDlgs.clw  Demonstration of properties available from API File
!               and Browse for Folder dialogs
!
!  Copyright © 2005 - 2007 Sand & Associates, Larry@sand-associates.com
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

  Include('SaFiDiCl.inc'),Once
  Include('SaBrFdCl.inc'),Once
  Map
  End


!------------------------------------------------------------------------
! Don't forget to add the link and DLL mode defines to your project:
!  _SALinkMode_=>1
!  _SADLLMode_=>0
!------------------------------------------------------------------------
OpenFileDialog      FileDialogClass
BrowseFor           BrowseForDialogClass
sFolderPath         String(FILE:MaxFilePath)
sDisplayName        String(FILE:MaxFilePath)
BrowseRoot         ULong(SA_CSIDL_DESKTOP)
bStartAtCurrent    BOOL
sInitialPath       String(FILE:MaxFilePath)
W    WINDOW('File and Folder Dialog test'),AT(,,231,134),FONT('tahoma',8,,),SYSTEM,GRAY,CENTER
       GROUP('Files'),AT(14,4,204,41),USE(?Group2),BOXED
         BUTTON('Get File Name'),AT(26,20,67,14),USE(?GetFileButton)
       END
       GROUP('Folders'),AT(14,54,204,71),USE(?Group1),BOXED
         LIST,AT(115,70,85,11),USE(BrowseRoot),LEFT(4),FORMAT('20L(2)|M'),DROP(10),FROM('Desktop|#0|My Computer|#17|My Documents|#5|Network|#18')
         CHECK('Start at current folder'),AT(115,87),USE(bStartAtCurrent)
         PROMPT('Browse root'),AT(27,70),USE(?BrowseFromPrompt)
         BUTTON('Get Folder'),AT(113,104,85,14),USE(?GetFolderButton)
       END
     END

  Code

  Open(W)

  Accept
    Case Field()
    ! use the file open/save common dialog
    Of ?GetFileButton
      If Event() = Event:Accepted
        OpenFileDialog.SetDialogFilter('All programs (*.exe)|*.exe|All files (*.*)|*.*')
        If OpenFileDialog.Show('Choose a chart file', FILE:KeepDir+FILE:LongName,,SA_OFN_HIDEREADONLY)
          Message('You selected this file: |'& OpenFileDialog.GetPathAndFilename() &'||'&|
                  'File name:<9>'&  OpenFileDialog.GetFilename()  &'|'&|
                  'File Extension:<9>'&  OpenFileDialog.GetFileExtension()  &'|'&|
                  'Path:<9><9>'&  OpenFileDialog.GetPath()     &'|'&|
                  'File Title:<9>'&  OpenFileDialog.GetFileTitle(), 'Selected file information', ICON:Asterisk )
        End
      End

      ! Use the browse for folder common dialog
      Of ?GetFolderButton
        If Event() = Event:Accepted

          If bStartAtCurrent
            sInitialPath = Path()
          Else
            sInitialPath = ''
          End

          sFolderPath = BrowseFor.Folder('Select the folder from the list below.', sInitialPath, BrowseRoot)
          If sFolderPath <> ''
             !If you have a use for the display name, save it to a variable
             sDisplayName = BrowseFor.GetDisplayName()
             Message('You selected this folder:|'& sFolderPath,'Selected folder path', ICON:Asterisk)
          End
        End
    End
  End
  Close(W)
  Return


