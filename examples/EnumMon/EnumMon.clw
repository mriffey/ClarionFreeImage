!============================================================
! ******                                   ********
! ****** Only works on W98, ME,  and >=W2K ********
! ******                                   ********
!
! EnumMon.clw -- Enumerate Multiple Display Monitors implementation
!
!  Copyright © 2007 Sand & Associates, Larry@sand-associates.com
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

Program
  Include('SaWapi.inc', 'Defines'),Once

SA_MONITORINFOEX Group,Type
cbSize          UNSIGNED
rcMonitor       Like(SA_RECT)
rcWork          Like(SA_RECT)
dwFlags         UNSIGNED
szDevice        CString(32)
  End

SA_MONITORINFO  Group,Type
cbSize          UNSIGNED
rcMonitor       Like(SA_RECT)
rcWork          Like(SA_RECT)
dwFlags         UNSIGNED
             End

SA_MONITOR_DEFAULTTONULL       Equate(00000000h)
SA_MONITOR_DEFAULTTOPRIMARY    Equate(00000001h)
SA_MONITOR_DEFAULTTONEAREST    Equate(00000002h)

SA_MONITORINFOF_PRIMARY        Equate(00000001h)

  Map
    Module('enumMonApi')     

      SA_EnumDisplayMonitors(SA_HDC hdc,                    | handle to display DC
                             <*SA_RECT lprcClip>,           | clipping rectangle
                             UNSIGNED lpfnEnum,             | callback function
                             Long dwData                    | data for callback function
                            ),BOOL,Pascal,Raw,Name('EnumDisplayMonitors')

      SA_GetMonitorInfo( SA_HANDLE hMonitor,      | handle to display monitor
                        *SA_MONITORINFO lpmi      | display monitor information
                       ),BOOL,Pascal,Raw,Name('GetMonitorInfoA')

      SA_GetMonitorInfo( SA_HANDLE hMonitor,      | handle to display monitor
                        *SA_MONITORINFOEX lpmix   | display monitor information
                       ),BOOL,Pascal,Raw,Name('GetMonitorInfoA')


     End

MonitorEnumProc Procedure(SA_HANDLE hMonitor,   | handle to display monitor
                          SA_HDC hdcMonitor,    | handle to monitor DC (may be null)
                          UNSIGNED lprcMonitor, | monitor intersection rectangle
                          Long dwData           | data
                         ),BOOL,Pascal

  End

DisplayQ    Queue
sDevice       String(32)
sMonitorArea  String(64)
sWorkingArea  String(64)
bIsPrimary    Bool
            End

W    WINDOW('Enum Display Monitors'),AT(,,510,200),FONT('MS Sans Serif',,,),SYSTEM,GRAY,RESIZE
       BUTTON('&Enumerate Monitors'),AT(9,7,98,14),USE(?EnumMonitorsButton),FONT(,,,FONT:bold)
       LIST,AT(0,27),USE(?MonitorList),FULL,FORMAT('89L(2)|M~Device Name~177L(2)|M~Monitor area~177L(2)|M~Work area~5L(2)|M~Is Prima' &|
           'ry~'),FROM(DisplayQ)
     END

  Code

  Open(W)
  Accept
    Case Event()
    Of Event:OpenWindow
      Post(Event:Accepted,?EnumMonitorsButton)
    End

    Case Accepted()
    Of ?EnumMonitorsButton
      Free(DisplayQ)
      !------------------------------------------------------------------
      ! Pass the address of your callback procedure to the EnumDisplayMonitors
      ! function.  Your callback procedure will be called once for each
      ! display device
      !------------------------------------------------------------------
      If SA_EnumDisplayMonitors(0 , ,Address(MonitorEnumProc), 0) = 0
        Message('Unable to enumerate display montitors')
      End
    End
  End

  Close(W)
  Return


!-------------------------------------------------------------------------
! This procedure is called once for each monitor by EnumDisplayMonitors.
! Don't forget the PASCAL attribute on the prototype.
!-------------------------------------------------------------------------
MonitorEnumProc Procedure(SA_HANDLE hMonitor,     | handle to display monitor
                            SA_HDC hdcMonitor,    | handle to monitor DC (May be null)
                            UNSIGNED lprcMonitor, | monitor intersection rectangle
                            Long dwData           | data
                           )!,BOOL,Pascal

mix Like(SA_MONITORINFOEX)

   Code
   mix.cbSize= Size(mix)
   If SA_GetMonitorInfo(hMonitor, mix) = 0
     Message('unable to get monitor info')
   Else
     !rectangle coordinates are negative for left and above the primary display.
     Clear(DisplayQ)
     DisplayQ.sDevice = mix.szDevice
     DisplayQ.sMonitorArea =  'Left: ' & mix.rcMonitor.left &'  Right: '& mix.rcMonitor.Right &'  Top: '& mix.rcMonitor.top &'  Bottom: '&  mix.rcMonitor.bottom
     DisplayQ.sWorkingArea =  'Left: ' & mix.rcWork.left &'  Right: '& mix.rcWork.Right &'  Top: '& mix.rcWork.top &' Bottom: '& mix.rcWork.bottom
     DisplayQ.bIsPrimary = mix.dwFlags ! currently the only flag defined is SA_MONITORINFOF_PRIMARY
     Add(DisplayQ)
     If Errorcode()
       Message('Unable to add monitor info to the queue.')
     End
   End
   Return 1 !Return 1 to keep enumerating