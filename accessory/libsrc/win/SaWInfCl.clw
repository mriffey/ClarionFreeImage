!============================================================
! SaWInfCl.clw -- Windows Information class implementation
!
!  Copyright © 1999-2006 Sand & Associates, Larry@sand-associates.com
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

MEMBER
  INCLUDE('SaWInfCl.inc'),ONCE

  MAP
    Include('SaWApi.inc','Prototypes'),Once
  END


!======================================================================
SaWinInfClass.Construct Procedure()
  Code
  Self.WinVersionValid = False


!======================================================================
! This method is adapted from code available on MSDN
!======================================================================
SaWinInfClass.WinVersionEx   PROCEDURE()
mw     GROUP,OVER(SELF.osvi.dwBuildNumber)
LowWord     USHORT
HighWord    USHORT
     END


bOsVersionInfoEx    BOOL,AUTO
hRegKey             UNSIGNED,AUTO
szProductType       CSTRING(16),AUTO
szRegValueName      CSTRING(16),AUTO
szRegSubKey         CSTRING(64),AUTO
dwBufLen            LONG,AUTO
si                  Like(SA_SYSTEM_INFO)

  CODE

  CLEAR(Self.WinVerInfo)

  !-----------------------------------------------
  ! First try to use the _ex structure
  !-----------------------------------------------
  Self.osvi.dwOsVersionInfoSize = SIZE(SA_OSVERSIONINFOEX)
  bOsVersionInfoEx = SA_GetVersionEx(Self.osvi)

  !-----------------------------------------------
  ! The _ex structure didn't work, try the normal one
  !-----------------------------------------------
  IF NOT bOsVersionInfoEx
    CLEAR(Self.osvi)
    Self.osvi.dwOsVersionInfoSize = SIZE(SA_OSVERSIONINFO)
    IF NOT SA_GetVersionEx(Self.osvi)
      !----------------------------------------- 
      ! Nothing worked, so ensure the structures
      ! are cleared.
      !-----------------------------------------
      CLEAR(Self.osvi)
      RETURN
    END
  END

  Self.WinVersionValid = True
  SA_GetSystemInfo(si)

  CASE Self.osvi.dwPlatformID
  !-----------------------------------------------
  ! It's an NT derivative
  !-----------------------------------------------
  OF SA_VER_PLATFORM_WIN32_NT

    !-----------------------------------------------
    ! First find the Major version type
    !-----------------------------------------------
    If Self.osvi.dwMajorVersion > 0 and Self.osvi.dwMajorversion <= 4
      Self.WinVerInfo.szPlatform = 'Windows NT'
    ElsIf Self.IsWindows2000()
      Self.WinVerInfo.szPlatform = 'Windows 2000'
    ElsIf Self.IsWindowsXP()
      Self.WinVerInfo.szPlatform = 'Windows XP'
    ElsIf Self.IsWindows2003()
      Self.WinVerInfo.szPlatform = 'Windows Server 2003'
    ElsIf Self.IsWindowsVista()
      Self.WinVerInfo.szPlatform = 'Windows Vista'
    ElsIf Self.IsWindows7()
      Self.WinVerInfo.szPlatform = 'Windows 7'
    ElsIf Self.IsWindows2008()
      Self.WinVerInfo.szPlatform = 'Windows Server 2008'
    ElsIf Self.IsWindows2008R2()
      Self.WinVerInfo.szPlatform = 'Windows Server 2008 R2'
    ElsIf Self.IsWindows8()
      Self.WinVerInfo.szPlatform = 'Windows 8'
    ElsIf Self.IsWindows81()
      Self.WinVerInfo.szPlatform = 'Windows 8.1'
    ElsIf Self.IsWindows2012()
      Self.WinVerInfo.szPlatform = 'Windows Server 2012'
    ElsIf Self.IsWindows2012R2()
      Self.WinVerInfo.szPlatform = 'Windows Server 2012 R2'
    Else
      Self.WinVerInfo.szPlatform = 'New Windows Version (Unknown)'
    End

    !-----------------------------------------------
    ! Now find the specific version of the major type.
    ! If _Ex structure is filled, then use it's info
    ! to determine the version.
    !-----------------------------------------------
    IF bOsVersionInfoEx 
      CASE Self.osvi.wProductType

      
      OF SA_VER_NT_WORKSTATION

        If Self.IsWindows2003()
          If si.wProcessorArchitecture = SA_PROCESSOR_ARCHITECTURE_AMD64
            Self.WinVerInfo.szPlatform = 'Windows XP Professional x64 Edition'
          End

        ElsIf Self.IsWindowsXP()
          If SA_GetSystemMetrics(SA_SM_MEDIACENTER)
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Media Center Edition'
            !original, 2004, 2005
          ElsIf SA_GetSystemMetrics(SA_SM_TABLETPC)
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Tablet PC Edition'
          ElsIf SA_GetSystemMetrics(SA_SM_STARTER)
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Starter Edition'
          ElsIf BAND(Self.osvi.wSuiteMask, SA_VER_SUITE_PERSONAL)
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Home'
          ELSE
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Professional'
          End
        Elsif Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 6, 0)
        !si.dwoemid is always zero
!          If si.wProcessorArchitecture = SA_PROCESSOR_ARCHITECTURE_AMD64
!            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' 64-bit'
!          ElsIf si.wProcessorArchitecture = SA_PROCESSOR_ARCHITECTURE_INTEL
!            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' 32-bit'
!          End
        End

      OF SA_VER_NT_SERVER OrOf SA_VER_NT_DOMAIN_CONTROLLER
        If Self.IsWindowsVista()
          Self.WinVerInfo.szPlatform = 'Windows Server Longhorn'

        Elsif Self.IsWindows2003()
          IF BAND(Self.osvi.wSuiteMask, SA_VER_SUITE_DATACENTER)
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Datacenter Edition'
          ELSIF BAND(Self.osvi.wSuiteMask, SA_VER_SUITE_ENTERPRISE)
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Enterprise Edition'
          ELSIF BAND(Self.osvi.wSuiteMask, SA_VER_SUITE_BLADE)
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Web Edition'
          ELSE
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Standard Edition'
          END

          If si.wProcessorArchitecture = SA_PROCESSOR_ARCHITECTURE_IA64
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' for Itanium-based Systems'
          ElsIf si.wProcessorArchitecture = SA_PROCESSOR_ARCHITECTURE_AMD64
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' for x64 Systems'
          End

          If SA_GetSystemMetrics(SA_SM_SERVERR2)
             Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' R2'
          End

        
        ElsIf Self.IsWindows2000()
          IF BAND(Self.osvi.wSuiteMask, SA_VER_SUITE_DATACENTER)
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Datacenter Server'
          ELSIF BAND(Self.osvi.wSuiteMask, SA_VER_SUITE_ENTERPRISE)
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Advanced Server'
          ELSE
            Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Server'
          END

        !Nt4
        Else   
          If BAND(Self.osvi.wSuiteMask, SA_VER_SUITE_ENTERPRISE)
             Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Server 4.0, Enterprise Edition'
          Else
             Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Server 4.0'
          End
        End
      END


    !-----------------------------------------------
    ! The _Ex structure wasn't filled, so use  the
    ! registry entries to determine the version
    !-----------------------------------------------
    ELSE 
      szProductType = ''
      szRegSubKey = 'SYSTEM\CurrentControlSet\Control\ProductOptions'
      hRegKey = 0
      IF SA_RegOpenKeyEx( SA_HKEY_LOCAL_MACHINE, szRegSubKey, , SA_KEY_QUERY_VALUE+SA_STANDARD_RIGHTS_READ, hRegKey )=0
        szRegValueName = 'ProductType'
        dwBufLen = SIZE(szRegValueName)-1
        If SA_RegQueryValueEx( hRegKey, szRegValueName, , , szProductType, dwBufLen).
        SA_RegCloseKey( hRegKey )
      END

      IF UPPER(szProductType) = 'WINNT'
         Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Workstation'
      ELSIF UPPER(szProductType) = 'LANMANNT'
         Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Server'
      ELSIF UPPER(szProductType) = 'SERVERNT'
         Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' Advanced Server'
      END

    END
    Self.WinVerInfo.szServicePack = Self.osvi.szCSDVersion

    If Self.osvi.dwMajorVersion = 4 And Self.WinVerInfo.szServicePack = 'Service Pack 6'
      szProductType = ''
      !This registry key indicates the presence of the 6a hotfix
      szRegSubKey = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009'
      hRegKey = 0
      IF SA_RegOpenKeyEx( SA_HKEY_LOCAL_MACHINE, szRegSubKey, , SA_KEY_QUERY_VALUE+SA_STANDARD_RIGHTS_READ, hRegKey )=0
        Self.WinVerInfo.szServicePack = Self.WinVerInfo.szServicePack & 'a'  !SP6a
      END
      SA_RegCloseKey( hRegKey )

    End

  !-----------------------------------------------
  ! It's Windows 95, 98, or ME
  !-----------------------------------------------
  OF SA_VER_PLATFORM_WIN32_WINDOWS

    IF Self.IsWindows95()
      Self.WinVerInfo.szPlatform = 'Windows 95'
      IF Self.osvi.szCSDVersion[2] = 'C'
        Self.WinVerInfo.szPlatform = Self.WinVerInfo.szPlatform & ' OSR2'
      END
    ELSIF Self.IsWindows98()
      Self.WinVerInfo.szPlatform = 'Windows 98'
      IF Self.osvi.szCSDVersion[2] = 'A'
        Self.WinVerInfo.szPlatform  = Self.WinVerInfo.szPlatform & ' SE'
      END
    ELSIF Self.IsWindowsME()
      Self.WinVerInfo.szPlatform = 'Windows ME'
    ELSE
      Self.WinVerInfo.szPlatform = 'New Windows Version (Unknown)'
    END
    
  END

  Self.WinVerInfo.szMajor = Self.osvi.dwMajorVersion
  Self.WinVerInfo.szMinor = Self.osvi.dwMinorVersion
  Self.WinVerInfo.szBuild = BAND(Self.osvi.dwBuildNumber,0FFFFh)
  RETURN


!======================================================================
!======================================================================
SaWinInfClass.GetWinVerInfo       Procedure(*SAWINFO_WINVERINFO wi)
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  wi = Self.WinVerInfo
  RETURN


!======================================================================
!======================================================================
SaWinInfClass.GetWinVersionMajor   PROCEDURE()
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  RETURN SELF.osvi.dwMajorVersion

!======================================================================
!======================================================================
SaWinInfClass.GetWinVersionMinor   PROCEDURE()
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  RETURN SELF.osvi.dwMinorVersion

!======================================================================
!======================================================================
SaWinInfClass.GetWinVersionPlatformId   PROCEDURE()
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  RETURN SELF.osvi.dwPlatformId

!======================================================================
!======================================================================
SaWinInfClass.GetWinVersionServicePack Procedure()!,String
  Code
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.WinVerInfo.szServicePack

!======================================================================
!======================================================================
SaWinInfClass.IsWindowsNT       PROCEDURE()
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Choose(Self.GetWinVersionPlatformId() = SA_VER_PLATFORM_WIN32_NT, True, False)

!======================================================================
!======================================================================
SaWinInfClass.IsWindows9x        PROCEDURE()!,BOOL !Returns True if 95, 98, or ME
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Choose(Self.GetWinVersionPlatformId() = SA_VER_PLATFORM_WIN32_WINDOWS, True, False)

!======================================================================
!======================================================================
SaWinInfClass.IsWindowsNT4        PROCEDURE()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.CompareVersion(4, 0, SA_VER_PLATFORM_WIN32_NT)

!======================================================================
!======================================================================
SaWinInfClass.IsWindowsNT3        PROCEDURE()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Choose(Self.osvi.dwMajorVersion < 4 AND |
                Self.GetWinVersionPlatformId() = SA_VER_PLATFORM_WIN32_NT, True, False)


!======================================================================
!======================================================================
SaWinInfClass.IsWindows2000        PROCEDURE()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.CompareVersion(5, 0, SA_VER_PLATFORM_WIN32_NT)

!======================================================================
!======================================================================
SaWinInfClass.IsWindowsXP        PROCEDURE()!,BOOL
si Like(SA_SYSTEM_INFO)
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  SA_GetSystemInfo(si)
  If si.wProcessorArchitecture = SA_PROCESSOR_ARCHITECTURE_AMD64 |
     And Self.osvi.wProductType = SA_VER_NT_WORKSTATION          |
     And Self.IsWindows2003() = True
    !This is XP Pro for AMD x64
    Return True
  End
  Return Self.CompareVersion(5, 1, SA_VER_PLATFORM_WIN32_NT) 

!======================================================================
!======================================================================
SaWinInfClass.IsWindows2003       PROCEDURE()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  If Self.osvi.wProductType = SA_VER_NT_WORKSTATION
    Return False
  End
  Return Self.CompareVersion(5, 2, SA_VER_PLATFORM_WIN32_NT)

!======================================================================
!======================================================================
SaWinInfClass.IsWindowsVista       PROCEDURE()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.CompareVersion(6, 0, SA_VER_PLATFORM_WIN32_NT)

!======================================================================
!======================================================================
SaWinInfClass.IsWindows2008      Procedure()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  If Self.osvi.wProductType = SA_VER_NT_WORKSTATION
    Return False
  End
  Return Self.CompareVersion(6, 0, SA_VER_PLATFORM_WIN32_NT)


!======================================================================
!======================================================================
SaWinInfClass.IsWindows7         Procedure()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.CompareVersion(6, 1, SA_VER_PLATFORM_WIN32_NT)


!======================================================================
!======================================================================
SaWinInfClass.IsWindows2008R2    Procedure()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  If Self.osvi.wProductType = SA_VER_NT_WORKSTATION
    Return False
  End
  Return Self.CompareVersion(6, 1, SA_VER_PLATFORM_WIN32_NT)


!======================================================================
!======================================================================
SaWinInfClass.IsWindows8         Procedure()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.CompareVersion(6, 2, SA_VER_PLATFORM_WIN32_NT)


!======================================================================
!======================================================================
SaWinInfClass.IsWindows81         Procedure()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.CompareVersion(6, 3, SA_VER_PLATFORM_WIN32_NT)
  
  
!======================================================================
!======================================================================
SaWinInfClass.IsWindows2012      Procedure()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  If Self.osvi.wProductType = SA_VER_NT_WORKSTATION
    Return False
  End
  Return Self.CompareVersion(6, 2, SA_VER_PLATFORM_WIN32_NT)

  
!======================================================================
!======================================================================
SaWinInfClass.IsWindows2012R2     Procedure()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  If Self.osvi.wProductType = SA_VER_NT_WORKSTATION
    Return False
  End
  Return Self.CompareVersion(6, 3, SA_VER_PLATFORM_WIN32_NT)

!======================================================================
!======================================================================
SaWinInfClass.IsWindows95        PROCEDURE()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.CompareVersion(4, 0, SA_VER_PLATFORM_WIN32_WINDOWS)

!======================================================================
!======================================================================
SaWinInfClass.IsWindows98        PROCEDURE()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.CompareVersion(4, 10, SA_VER_PLATFORM_WIN32_WINDOWS)

!======================================================================
!======================================================================
SaWinInfClass.IsWindowsME        PROCEDURE()!,BOOL
  CODE
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.CompareVersion(4, 90, SA_VER_PLATFORM_WIN32_WINDOWS)

!======================================================================
!======================================================================
SaWinInfClass.WindowsVersionAtLeast Procedure(Long nWinVer)!,BOOL
bResult BOOL,Auto
  Code
  bResult = False
  Case nWinVer
  Of nWinVer:Windows95
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_WINDOWS, 4, 0)
  Of nWinVer:Windows98
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_WINDOWS, 4, 10)
  Of nWinVer:WindowsME
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_WINDOWS, 4, 90)
  Of nWinVer:WindowsNt4
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 4, 0)
  Of nWinVer:Windows2000
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 5, 0)
  Of nWinVer:WindowsXP
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 5, 1)
  Of nWinVer:Windows2003
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 5, 2)
  Of nWinVer:WindowsVista
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 6, 0)
  Of nWinVer:Windows7
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 6, 1)
  Of nWinVer:Windows2008
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 6, 0)
  Of nWinVer:Windows2008R2
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 6, 1)
  Of nWinVer:Windows8
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 6, 2)   
  Of nWinVer:Windows81
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 6, 3)
  Of nWinVer:Windows2012
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 6, 2)
  Of nWinVer:Windows2012R2
    bResult = Self.WindowsVersionAtLeast(SA_VER_PLATFORM_WIN32_NT, 6, 3)
    
  End
  Return bResult

!======================================================================
!======================================================================
SaWinInfClass.WindowsVersionAtLeast Procedure(ULong PlatformID, ULong MajorVersion, ULong MinorVersion)!,BOOL
result BOOL,Auto
  Code
  result = False
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  IF SELF.osvi.dwPlatformId = PlatformId
    IF SELF.osvi.dwMajorVersion >= MajorVersion
      If SELF.osvi.dwMajorVersion = MajorVersion
        If SELF.osvi.dwMinorVersion >= MinorVersion
          result = True
        End
      Else
         result = True
      End
    End
  ElsIf SELF.osvi.dwPlatformId > PlatformId
    result = True
  End   
  Return result


!======================================================================
!======================================================================
SaWinInfClass.CompareVersion     PROCEDURE(ULONG Majorversion, |
                                           ULONG MinorVersion, |
                                           ULONG PlatformId,   |
                                           ULONG buildNumber=0)

RetCode BOOL,Auto

  CODE
  RetCode = False

  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  IF SELF.osvi.dwPlatformId = PlatformId
    IF SELF.osvi.dwMajorVersion = MajorVersion And SELF.osvi.dwMinorVersion = MinorVersion
      IF BuildNumber = 0
        RetCode = True
      ELSE
        IF SELF.osvi.dwBuildNumber = BuildNumber
          RetCode = True
        ELSE
          RetCode = False
        END
      END
    END
  END

  RETURN RetCode

!======================================================================
!======================================================================
SaWinInfClass.GetWindowsVersion       Procedure()!,String
  Code
  IF NOT SELF.WinVersionValid
    SELF.WinVersionEx()
  END
  Return Self.WinVerInfo.szPlatform & |
         Choose(Self.WinVerInfo.szServicePack<>'', ' '&Self.WinVerInfo.szServicePack ,'') & |
         Choose(Self.WinVerInfo.szBuild<>'',' Build '&Self.WinVerInfo.szBuild ,'')
