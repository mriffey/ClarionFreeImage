!ABCIncludeFile(SAAPI)
!============================================================
! SaFiDiCl.inc -- File Dialog Class header file
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

  OMIT('_EndOfSaFileDialogClass_',_SaFileDialogClassIncluded_)
_SaFileDialogClassIncluded_  EQUATE(1)

  INCLUDE('saWApi.inc','Defines'),ONCE




FDC_DEBUGSTATE Equate(False)


FileDialogClass  Class,Type, Module('SaFiDiCl.clw'),|
                 LINK('SaFiDiCl.clw',_SALinkMode_), |
                 DLL(_SADLLMode_)

bDebug              BOOL

szFullFilename      Cstring(1024)
nFileOffset         Long
nFileExtension      Long
szExtension         Cstring(32)
szDialogTitle       Cstring(128),Protected
szFileTitle         Cstring(256),Protected
szDefaultPath       Cstring(512),Protected
szDefaultExt        Cstring(4),Protected
szFilter            &Cstring,Private
OptionFlags         ULong,Protected
nFilterIndex        Long        !Contains index of selected filter after call to .show() method



SetDialogFilter         Procedure(String sFilter)
SetDialogFilterNulls    Procedure(*CString szFilter),Protected
SetDialogOptions        Procedure(ULong ofn_Options),Protected,Virtual
SetDefaultPath          Procedure(String sPath)

Show                    Procedure(<String sTitle>, Long ClarionOptionFlags=0, Long InitialFilterIndex=1, <ULong ofn_Options> ),BOOL,Virtual

OnCallDialog            Procedure(*SA_OPENFILENAME ofn),Virtual !Called just before the api file dialog is called
GetFilename             Procedure(),String
GetFileExtension        Procedure(),String
GetPath                 Procedure(),String
GetPathAndFilename      Procedure(),String
GetFileTitle            Procedure(),String

TakeError               Procedure(Ulong nErrorCode),Protected,Virtual

Construct   Procedure(),Protected
Destruct    Procedure(),Protected


                 End
!_EndOfSaFileDialogClass_
