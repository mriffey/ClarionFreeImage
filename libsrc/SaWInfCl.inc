!ABCIncludeFile(SAAPI)
!============================================================
! SaWInfCl.inc -- Windows Information class header file
!
!  Copyright © 1999-2013 Sand & Associates, Larry@sand-associates.com
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
! 2009.06.05  Add support for Windows 7, todo:runtime load GetProductInfo() to find edition
! 2013.07.05  Add support for 2008, 2008R2, 8, 8.1, 2012, 2012R2
!============================================================

 OMIT('_EndOfInclude_',_SaWinInfoClassIncluded_)
_SaWinInfoClassIncluded_ EQUATE(1)

  Include('SaWapi.inc','Defines'),Once


SAWINFO_WINVERINFO GROUP,TYPE
szPlatform    CSTRING(64)
szMajor       CSTRING(8)
szMinor       CSTRING(8)
szBuild       CSTRING(16)
szServicePack CSTRING(32)
            END

nWinVer     Itemize(100),Pre
Windows95     Equate
Windows98     Equate
WindowsME     Equate
WindowsNt4    Equate
Windows2000   Equate
WindowsXp     Equate
Windows2003   Equate
WindowsVista  Equate
Windows2008   Equate
Windows2008R2 Equate
Windows7      Equate
Windows8      Equate
Windows81     Equate
Windows2012   Equate
Windows2012R2 Equate
            End

SaWinInfClass   CLASS,TYPE,MODULE('SaWInfCl.clw'),LINK('SaWInfCl.clw',_SALinkMode_), DLL(_SADLLMode_)

osvi               LIKE(SA_OSVERSIONINFOEX),Protected
WinVerInfo         Like(SAWINFO_WINVERINFO),Protected
WinVersionValid    BOOL             !True if WinVersionEx was already called

WinVersionEx        Procedure(),Protected

GetWinVerInfo           Procedure(*SAWINFO_WINVERINFO wi)
GetWinVersionMajor      Procedure(),ULONG
GetWinVersionMinor      Procedure(),ULONG
GetWinVersionPlatformId Procedure(),ULONG
GetWinVersionServicePack Procedure(),String

GetWindowsVersion  Procedure(),String !Return a string that's in the same format as c6's System{Prop:WindowsVersion}

!CompareVersion returns true if specified version and current version match
CompareVersion     Procedure(ULONG Majorversion, ULONG MinorVersion, ULONG PlatformId, ULONG BuildNumber=0),BOOL

IsWindowsNT        Procedure(),BOOL !Returns True if NT, 2000, XP, Or 2003
IsWindowsNT4       Procedure(),BOOL 
IsWindowsNT3       Procedure(),BOOL 
IsWindows9x        Procedure(),BOOL !Returns True if 95, 98, or ME
IsWindows2000      Procedure(),BOOL
IsWindowsXP        Procedure(),BOOL
IsWindows2003      Procedure(),BOOL
IsWindowsVista     Procedure(),BOOL
IsWindows95        Procedure(),BOOL
IsWindows98        Procedure(),BOOL
IsWindowsME        Procedure(),BOOL
IsWindows2008      Procedure(),BOOL
IsWindows7         Procedure(),BOOL
IsWindows2008R2    Procedure(),BOOL
IsWindows8         Procedure(),BOOL
IsWindows81        Procedure(),BOOL
IsWindows2012      Procedure(),BOOL
IsWindows2012R2    Procedure(),BOOL

WindowsVersionAtLeast Procedure(Long nWinVer),BOOL
WindowsVersionAtLeast Procedure(ULong PlatformID, ULong MajorVersion, ULong MinorVersion),BOOL

Construct          Procedure()
                END

_EndOfInclude_
