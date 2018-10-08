!ABCIncludeFile(SAAPI)
!============================================================
! SaClipCl.inc -- Clipboard Class header file
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
! 2006.28.01    Added ability to get DIB from clipboard to TopSpeed BLOB
!               Formatting DIB with BITMAPFILEHEADER so it can be read by
!               the standard Clarion Image Control
!============================================================

  OMIT('_EndOfSaClipboardClass_',_SaClipboardClassIncluded_)
_SaClipboardClassIncluded_  EQUATE(1)

  Include('saWApi.inc','Defines'),ONCE


SaClipboardClass  Class,Type,Thread, Module('SaClipCl.clw'), LINK('SaClipCl.clw',_SALinkMode_), DLL(_SADLLMode_)
bOpened             BOOL
LastErrorCde        ULONG
lpMem               UNSIGNED,Protected
hMem                SA_HGLOBAL,Protected

OpenClipboard       Procedure(WINDOW w),BOOL
OpenClipboard       Procedure(Long hWnd),BOOL
CloseClipboard      Procedure()

SetClipboard        Procedure(UNSIGNED pBitmapinfo, UNSIGNED DIBSize),BOOL,Virtual,Proc  !copy a dib
SetClipboard        Procedure(String sText),BOOL,Virtual                                 !copy a string
SetClipboard        Procedure(*Cstring szText),BOOL,Virtual
SetClipboardData    Procedure(UNSIGNED CBFormat, UNSIGNED pData, UNSIGNED cbData),BOOL,Virtual,Protected

GetClipboard        Procedure(),String
GetClipboard        Procedure(*BLOB theBlob, UNSIGNED CbFormat=SA_CF_DIB),BOOL,Proc
GetClipboard        Procedure(*Window theWindow, Long theImageControl),BOOL,Proc

GetClipboardCopy    Procedure(UNSIGNED CBFormat),UNSIGNED  !Returns HGLOBAL, caller must call GlobalFree(HGLOBAL)

GetClipboardData    Procedure(UNSIGNED CBFormat),UNSIGNED,Protected  !returns handle to global memory (HGLOBAL)
OnGetClipboardData  Procedure(UNSIGNED CBFormat, UNSIGNED hMem),Virtual

Construct   Procedure(),Protected
Destruct    Procedure(),Protected


                 End
!_EndOfSaClipboardClass_