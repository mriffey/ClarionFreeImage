 SECTION('Defines')

!This file is derived from svapi.inc
!All labels have SA_ added as a prefix to prevent name clashes
!Additional Constants, structures and prototypes have been added.
!This file is NOT identical to svapi.inc

 !
 ! Portions Copyright © 2000 - 2007 Sand & Associates, Larry@sand-associates.com
 !

  OMIT('_EndOfSaWapiDefs_',_ApiDefsIncluded_)
_ApiDefsIncluded_  EQUATE(1)

SA_ULARGE_INTEGER                 group,type
LowPart                           long
HighPart                          long
                                end

SA_I64                             equate(long) ! where used, use the push functions for pushing the 64bit longlong

SA_MAX_PATH                        equate(260)
SA_HANDLE                          equate(long)
SA_HWND                            equate(SA_HANDLE)
SA_HINSTANCE                       equate(SA_HANDLE)
SA_HMODULE                         equate(SA_HANDLE)
SA_HMENU                           equate(SA_HANDLE)
SA_HDC                             equate(SA_HANDLE)
SA_HICON                           equate(SA_HANDLE)
SA_HCURSOR                         equate(SA_HANDLE)
SA_HBRUSH                          equate(SA_HANDLE)
SA_HBITMAP                         equate(SA_HANDLE)
SA_HGDIOBJ                         equate(SA_HANDLE)
SA_HFONT                           equate(SA_HANDLE)
SA_HRGN                            equate(SA_HANDLE)
SA_HGLOBAL                         equate(SA_HANDLE)
SA_HPEN                            equate(SA_HANDLE)
SA_HRESULT                         equate(SA_HANDLE)
SA_HPALETTE                        equate(SA_HANDLE)
SA_HMETA                           equate(SA_HANDLE)

SA_COLORREF                        equate(long)
SA_ATOM                            equate(long)

SA_RECT                         group,type
left                              signed
top                               signed
right                             signed
bottom                            signed
                                end
SA_RECTL                        Group,type
left                              Long
top                               Long
right                             Long
bottom                            Long
                                end

SA_POINT                        group,type
x                                 signed
y                                 signed
                                end

SA_SIZE                         group,type
cx                                signed
cy                                signed
                                end

SA_SIZEL                        Group,type
cx                                Long
cy                                Long
                                end

SA_WNDCLASS                     group,type
style                             unsigned
lpfnWndProc                       long
cbClsExtra                        signed
cbWndExtra                        signed
hInstance                         SA_HINSTANCE
hIcon                             SA_HICON
hCursor                           SA_HCURSOR
hbrBackground                     SA_HBRUSH
lpszMenuName                      long
lpszClassName                     long
                                end

SA_WNDCLASSEX                   group,type
cbSize                            unsigned
                                  group(SA_WNDCLASS).
hIconSm                           SA_HICON
                                end

SA_TCURRENCY                    group,type ! lo + as dec bshift(hi, 32) / 10000
lo                                long
hi                                long
                                end
SA_BROWSEINFO  GROUP,TYPE
hwndOwner       SA_HWND
pidlRoot        LONG
pszDisplayName  LONG
lpszTitle       LONG                 
ulFlags         UNSIGNED
lpfn            LONG
lParam          LONG
iImage          LONG
            END


SA_OPENFILENAME    GROUP,TYPE
lStructSize ULONG        !Use SA_OPENFILENAME_SIZE_VERSION_400
hwndOwner   SA_HWND
hInstance   UNSIGNED
lpstrFilter LONG
lpstrCustomFilter LONG
nMaxCustFilter  ULONG
nFilterIndex    ULONG
lpstrFile       LONG
nMaxFile        ULONG
lpstrFileTitle  LONG
nMaxFileTitle   ULONG
lpstrInitialDir LONG
lpstrTitle      LONG
Flags           ULONG
nFileOffset     USHORT
nFileExtension  USHORT
lpstrDefExt     LONG
lCustData       LONG
lpfnHook        LONG
lpTemplateName  LONG
pvReserved      LONG
dwReserved      ULONG
FlagsEx         ULONG
                END

SA_OPENFILENAME_SIZE_VERSION_400 Equate(Size(SA_OPENFILENAME)-12)

SA_OFN_READONLY                 Equate(00000001h)
SA_OFN_OVERWRITEPROMPT          Equate(00000002h)
SA_OFN_HIDEREADONLY             Equate(00000004h)
SA_OFN_NOCHANGEDIR              Equate(00000008h)
SA_OFN_SHOWHELP                 Equate(00000010h)
SA_OFN_ENABLEHOOK               Equate(00000020h)
SA_OFN_ENABLETEMPLATE           Equate(00000040h)
SA_OFN_ENABLETEMPLATEHANDLE     Equate(00000080h)
SA_OFN_NOVALIDATE               Equate(00000100h)
SA_OFN_ALLOWMULTISELECT         Equate(00000200h)
SA_OFN_EXTENSIONDIFFERENT       Equate(00000400h)
SA_OFN_PATHMUSTEXIST            Equate(00000800h)
SA_OFN_FILEMUSTEXIST            Equate(00001000h)
SA_OFN_CREATEPROMPT             Equate(00002000h)
SA_OFN_SHAREAWARE               Equate(00004000h)
SA_OFN_NOREADONLYRETURN         Equate(00008000h)
SA_OFN_NOTESTFILECREATE         Equate(00010000h)
SA_OFN_NONETWORKBUTTON          Equate(00020000h)
SA_OFN_NOLONGNAMES              Equate(00040000h)  ! force no long names for 4.x modules


!<W2k
SA_OFN_EXPLORER                 Equate(00080000h)  ! new look commdlg
SA_OFN_NODEREFERENCELINKS       Equate(00100000h)
SA_OFN_LONGNAMES                Equate(00200000h)  ! force long names for 3.x modules
SA_OFN_ENABLEINCLUDENOTIFY      Equate(00400000h)  ! send include message to callback
SA_OFN_ENABLESIZING             Equate(00800000h)

!Nt5>
SA_OFN_DONTADDTORECENT          Equate(02000000h)
SA_OFN_FORCESHOWHIDDEN          Equate(10000000h)   ! Show All files including System and hidden files
                
! Define the dwOpenMode values for CreateNamedPipe
!
SA_PIPE_ACCESS_INBOUND        Equate(00000001h)
SA_PIPE_ACCESS_OUTBOUND       Equate(00000002h)
SA_PIPE_ACCESS_DUPLEX         Equate(00000003h)

! Define the Named Pipe End flags for GetNamedPipeInfo
!
SA_PIPE_CLIENT_END            Equate(00000000h)
SA_PIPE_SERVER_END            Equate(00000001h)

! Define the dwPipeMode values for CreateNamedPipe
!
SA_PIPE_WAIT                  Equate(00000000h)
SA_PIPE_NOWAIT                Equate(00000001h)
SA_PIPE_READMODE_BYTE         Equate(00000000h)
SA_PIPE_READMODE_MESSAGE      Equate(00000002h)
SA_PIPE_TYPE_BYTE             Equate(00000000h)
SA_PIPE_TYPE_MESSAGE          Equate(00000004h)

! Define the well known values for CreateNamedPipe nMaxInstances
!
SA_PIPE_UNLIMITED_INSTANCES Equate(255)                
                
!File System values                
SA_GENERIC_READ                    equate(80000000h)
SA_GENERIC_WRITE                   equate(40000000h)
SA_GENERIC_EXECUTE                 equate(20000000h)
SA_GENERIC_ALL                     equate(10000000h)
SA_FILE_SHARE_READ                 equate(00000001h)
SA_FILE_SHARE_WRITE                equate(00000002h)
SA_FILE_SHARE_DELETE               equate(00000004h)
SA_FILE_ATTRIBUTE_READONLY         equate(00000001h)
SA_FILE_ATTRIBUTE_HIDDEN           equate(00000002h)
SA_FILE_ATTRIBUTE_SYSTEM           equate(00000004h)
SA_FILE_ATTRIBUTE_DIRECTORY        equate(00000010h)
SA_FILE_ATTRIBUTE_ARCHIVE          equate(00000020h)
SA_FILE_ATTRIBUTE_NORMAL           equate(00000080h)
SA_FILE_ATTRIBUTE_TEMPORARY        equate(00000100h)
SA_FILE_ATTRIBUTE_COMPRESSED       equate(00000800h)
SA_FILE_ATTRIBUTE_OFFLINE          equate(00001000h)
SA_FILE_NOTIFY_CHANGE_FILE_NAME    equate(00000001h)
SA_FILE_NOTIFY_CHANGE_DIR_NAME     equate(00000002h)
SA_FILE_NOTIFY_CHANGE_ATTRIBUTES   equate(00000004h)
SA_FILE_NOTIFY_CHANGE_SIZE         equate(00000008h)
SA_FILE_NOTIFY_CHANGE_LAST_WRITE   equate(00000010h)
SA_FILE_NOTIFY_CHANGE_LAST_ACCESS  equate(00000020h)
SA_FILE_NOTIFY_CHANGE_CREATION     equate(00000040h)
SA_FILE_NOTIFY_CHANGE_SECURITY     equate(00000100h)
SA_FILE_ACTION_ADDED               equate(00000001h)
SA_FILE_ACTION_REMOVED             equate(00000002h)
SA_FILE_ACTION_MODIFIED            equate(00000003h)
SA_FILE_ACTION_RENAMED_OLD_NAME    equate(00000004h)
SA_FILE_ACTION_RENAMED_NEW_NAME    equate(00000005h)
SA_FILE_CASE_SENSITIVE_SEARCH      equate(00000001h)
SA_FILE_CASE_PRESERVED_NAMES       equate(00000002h)
SA_FILE_UNICODE_ON_DISK            equate(00000004h)
SA_FILE_PERSISTENT_ACLS            equate(00000008h)
SA_FILE_FILE_COMPRESSION           equate(00000010h)
SA_FILE_VOLUME_IS_COMPRESSED       equate(00008000h)
SA_CREATE_NEW                      equate(1)
SA_CREATE_ALWAYS                   equate(2)
SA_OPEN_EXISTING                   equate(3)
SA_OPEN_ALWAYS                     equate(4)
SA_TRUNCATE_EXISTING               equate(5)
SA_FILE_FLAG_WRITE_THROUGH         equate(80000000h)
SA_FILE_FLAG_OVERLAPPED            equate(40000000h)
SA_FILE_FLAG_NO_BUFFERING          equate(20000000h)
SA_FILE_FLAG_RANDOM_ACCESS         equate(10000000h)
SA_FILE_FLAG_SEQUENTIAL_SCAN       equate(08000000h)
SA_FILE_FLAG_DELETE_ON_CLOSE       equate(04000000h)
SA_FILE_FLAG_BACKUP_SEMANTICS      equate(02000000h)
SA_FILE_FLAG_POSIX_SEMANTICS       equate(01000000h)

SA_FILE_BEGIN                      equate(0)
SA_FILE_CURRENT                    equate(1)
SA_FILE_END                        equate(2)

SA_INVALID_HANDLE_VALUE    EQUATE(-1)
SA_INVALID_FILE_SIZE       EQUATE(0FFFFFFFFh)
SA_INVALID_SET_FILE_POINTER EQUATE(-1)


SA_PAGE_NOACCESS                   equate(01h)
SA_PAGE_READONLY                   equate(02h)
SA_PAGE_READWRITE                  equate(04h)
SA_PAGE_WRITECOPY                  equate(08h)
SA_PAGE_EXECUTE                    equate(10h)
SA_PAGE_EXECUTE_READ               equate(20h)
SA_PAGE_EXECUTE_READWRITE          equate(40h)
SA_PAGE_EXECUTE_WRITECOPY          equate(80h)
SA_PAGE_GUARD                      equate(100h)
SA_PAGE_NOCACHE                    equate(200h)
SA_MEM_COMMIT                      equate(1000h)
SA_MEM_RESERVE                     equate(2000h)
SA_MEM_DECOMMIT                    equate(4000h)
SA_MEM_RELEASE                     equate(8000h)
SA_MEM_FREE                        equate(10000h)
SA_MEM_PRIVATE                     equate(20000h)
SA_MEM_MAPPED                      equate(40000h)
SA_MEM_RESET                       equate(80000h)
SA_MEM_TOP_DOWN                    equate(100000h)
SA_SEC_FILE                        equate(800000h)
SA_SEC_IMAGE                       equate(1000000h)
SA_SEC_RESERVE                     equate(4000000h)
SA_SEC_COMMIT                      equate(8000000h)
SA_SEC_NOCACHE                     equate(10000000h)

SA_FILETIME          GROUP,TYPE
dwLowDateTime      ULONG
dwHighDateTime     ULONG
                  END

SA_SYSTEMTIME        GROUP,TYPE
wYear               USHORT
wMonth              USHORT
wDayOfWeek          USHORT
wDay                USHORT
wHour               USHORT
wMinute             USHORT
wSecond             USHORT
wMilliseconds       USHORT
                  END

!Standard Cursor IDs

SA_IDC_ARROW                       equate(32512)
SA_IDC_IBEAM                       equate(32513)
SA_IDC_WAIT                        equate(32514)
SA_IDC_CROSS                       equate(32515)
SA_IDC_UPARROW                     equate(32516)
SA_IDC_SIZE                        equate(32640)  !OBSOLETE: use IDC_SIZEALL
SA_IDC_ICON                        equate(32641)  !OBSOLETE: use IDC_ARROW
SA_IDC_SIZENWSE                    equate(32642)
SA_IDC_SIZENESW                    equate(32643)
SA_IDC_SIZEWE                      equate(32644)
SA_IDC_SIZENS                      equate(32645)
SA_IDC_SIZEALL                     equate(32646)
SA_IDC_NO                          equate(32648)  !not in win3.1
SA_IDC_APPSTARTING                 equate(32650)  !not in win3.1
SA_IDC_HELP                        equate(32651)

SA_PAINTSTRUCT                     group,type
hdc                               SA_HDC
fErase                            bool
rcPaint                           group(SA_RECT).
fRestore                          bool
fIncUpdate                        bool
rgbReserved                       string(32)
                                end

! Owner draw control types

SA_OSA_DT_MENU                        equate(1)
SA_OSA_DT_LISTBOX                     equate(2)
SA_OSA_DT_COMBOBOX                    equate(3)
SA_OSA_DT_BUTTON                      equate(4)

OSA_DT_STATIC                      equate(5)

! Owner draw actions

SA_ODA_DRAWENTIRE                  equate(00001h)
SA_ODA_SELECT                      equate(00002h)
SA_ODA_FOCUS                       equate(00004h)

! Owner draw state
SA_ODS_SELECTED                    equate(00001h)
SA_ODS_GRAYED                      equate(00002h)
SA_ODS_DISABLED                    equate(00004h)
SA_ODS_CHECKED                     equate(00008h)
SA_ODS_FOCUS                       equate(00010h)

SA_ODS_DEFAULT                     equate(00020h)
SA_ODS_COMBOBOXEDIT                equate(01000h)

SA_ODS_HOTLIGHT                    equate(00040h)
SA_ODS_INACTIVE                    equate(00080h)

SA_ODS_NOACCEL                     equate(0100h)
SA_ODS_NOFOCUSRECT                 equate(0200h)


SA_DRAWITEMSTRUCT                  group,type
CtlType                           long
CtlID                             long
itemID                            long
itemAction                        long
itemState                         long
hwndItem                          SA_HWnd
hDC                               SA_HDC
rcItem                            like(SA_RECT)
itemData                          long
                                end

!Bitmap Header Definition
SA_BITMAPFILEHEADER             Group,Type
bfType                            Ushort
bfSize                            Ulong
bfReserved1                       Ushort
bfReserved2                       Ushort
bfOffbits                         Ulong
                                End

SA_BITMAP                       Group,type
bmType                            long
bmWidth                           long
bmHeight                          long
bmWidthBytes                      long
bmPlanes                          short
bmBitsPixel                       short
bmBits                            long
                                end

SA_BITMAPINFOHEADER             group,type
biSize                            long
biWidth                           long
biHeight                          long
biPlanes                          short
biBitCount                        short
biCompression                     long
biSizeImage                       long
biXPelsPerMeter                   long
biYPelsPerMeter                   long
biClrUsed                         long
biClrImportant                    long
                                end

SA_RGBQUAD                      group,type
rgbBlue                           byte
rgbGreen                          byte
rgbRed                            byte
rgbReserved                       byte
                                end

SA_BITMAPINFO                      group,type
                                  group(SA_BITMAPINFOHEADER).
bmiColors                         group(SA_RGBQUAD),dim(1).
                                end

SA_BITMAPINFOCLASS                 class,type
BitmapData                        group(SA_BITMAP).
                                end

SA_DIBSECTION                   Group,Type
dsBm                                Group(SA_BITMAP).
dsBmih                              Group(SA_BITMAPINFOHEADER).
dsBitFields                         Long,Dim(3)
dshSection                          SA_HANDLE
dsOffset                            Long
                                End

! constants for CreateDIBitmap

SA_CBM_INIT                        equate(04h)     ! initialize bitmap

!Pen Styles

SA_PS_SOLID                        equate(0)
SA_PS_DASH                         equate(1)       ! -------
SA_PS_DOT                          equate(2)       ! .......
SA_PS_DASHDOT                      equate(3)       ! _._._._
SA_PS_DASHDOTDOT                   equate(4)       ! _.._.._
SA_PS_NULL                         equate(5)
SA_PS_INSIDEFRAME                  equate(6)
SA_PS_USERSTYLE                    equate(7)
SA_PS_ALTERNATE                    equate(8)
SA_PS_STYLE_MASK                   equate(0000000Fh)
SA_PS_ENDCAP_ROUND                 equate(00000000h)
SA_PS_ENDCAP_SQUARE                equate(00000100h)
SA_PS_ENDCAP_FLAT                  equate(00000200h)
SA_PS_ENDCAP_MASK                  equate(00000F00h)
SA_PS_JOIN_ROUND                   equate(00000000h)
SA_PS_JOIN_BEVEL                   equate(00001000h)
SA_PS_JOIN_MITER                   equate(00002000h)
SA_PS_JOIN_MASK                    equate(0000F000h)
SA_PS_COSMETIC                     equate(00000000h)
SA_PS_GEOMETRIC                    equate(00010000h)
SA_PS_TYPE_MASK                    equate(000F0000h)

!DIB color table identifiers

SA_DIB_RGB_COLORS                  equate(0) !color table in RGBs
SA_DIB_PAL_COLORS                  equate(1) !color table in palette indices

!/* constants for the biCompression field */
SA_BI_RGB        equate(0)
SA_BI_RLE8       equate(1)
SA_BI_RLE4       equate(2)
SA_BI_BITFIELDS  equate(3)
SA_BI_JPEG       equate(4)
SA_BI_PNG        equate(5)

!Standard Windows Messages

SA_WM_NULL                         equate(00H)
SA_WM_CREATE                       equate(01H)
SA_WM_DESTROY                      equate(02H)
SA_WM_MOVE                         equate(03H)
SA_WM_SIZE                         equate(05H)
SA_WM_ACTIVATE                     equate(06H)
SA_WM_SETFOCUS                     equate(07H)
SA_WM_KILLFOCUS                    equate(08H)
SA_WM_ENABLE                       equate(0AH)
SA_WM_SETREDRAW                    equate(0BH)
SA_WM_SETTEXT                      equate(0CH)
SA_WM_GETTEXT                      equate(0DH)
SA_WM_GETTEXTLENGTH                equate(0EH)
SA_WM_PAINT                        equate(0FH)
SA_WM_CLOSE                        equate(010H)
SA_WM_QUERYENDSESSION              equate(011H)
SA_WM_QUIT                         equate(012H)
SA_WM_QUERYOPEN                    equate(013H)
SA_WM_ERASEBKGND                   equate(014H)
SA_WM_SYSCOLORCHANGE               equate(015H)
SA_WM_ENDSESSION                   equate(016H)
SA_WM_SHOWWINDOW                   equate(018H)
SA_WM_CTLCOLOR                     equate(019H)
SA_WM_WININICHANGE                 equate(01AH)
SA_WM_DEVMODECHANGE                equate(01BH)
SA_WM_ACTIVATEAPP                  equate(01CH)
SA_WM_FONTCHANGE                   equate(01DH)
SA_WM_TIMECHANGE                   equate(01EH)
SA_WM_CANCELMODE                   equate(01FH)
SA_WM_SETCURSOR                    equate(020H)
SA_WM_MOUSEACTIVATE                equate(021H)
!SA_WM_DESTROY                     equate(022H)
SA_WM_QUEUESYNC                    equate(023H)
SA_WM_GETMINMAXINFO                equate(024H)
SA_WM_PAINTICON                    equate(026H)
SA_WM_ICONERASEBKGND               equate(027H)
SA_WM_NEXTDLGCTL                   equate(028H)
SA_WM_SPOOLERSTATUS                equate(02AH)
SA_WM_DRAWITEM                     equate(02BH)
SA_WM_MEASUREITEM                  equate(02CH)
SA_WM_DELETEITEM                   equate(02DH)
SA_WM_VKEYTOITEM                   equate(02EH)
SA_WM_CHARTOITEM                   equate(02FH)
SA_WM_SETFONT                      equate(030H)
SA_WM_GETFONT                      equate(031H)
SA_WM_QUERYDRAGICON                equate(037H)
SA_WM_COMPAREITEM                  equate(039H)
SA_WM_COMPACTING                   equate(041H)
SA_WM_WINDOWPOSCHANGING            equate(0046H)
SA_WM_WINDOWPOSCHANGED             equate(0047H)
SA_WM_CONTEXTMENU                  equate(07Bh)
SA_WM_STYLECHANGING                equate(07Ch)
SA_WM_STYLECHANGED                 equate(07Dh)
SA_WM_DISPLAYCHANGE                equate(07Eh)
SA_WM_GETICON                      equate(07Fh)
SA_WM_SETICON                      equate(080h)
SA_WM_NCCREATE                     equate(081H)
SA_WM_NCDESTROY                    equate(082H)
SA_WM_NCCALCSIZE                   equate(083H)
SA_WM_NCHITTEST                    equate(084H)
SA_WM_NCPAINT                      equate(085H)
SA_WM_NCACTIVATE                   equate(086H)
SA_WM_GETDLGCODE                   equate(087H)
SA_WM_NCMOUSEMOVE                  equate(0A0H)
SA_WM_NCLBUTTONDOWN                equate(0A1H)
SA_WM_NCLBUTTONUP                  equate(0A2H)
SA_WM_NCLBUTTONDBLCLK              equate(0A3H)
SA_WM_NCRBUTTONDOWN                equate(0A4H)
SA_WM_NCRBUTTONUP                  equate(0A5H)
SA_WM_NCRBUTTONDBLCLK              equate(0A6H)
SA_WM_NCMBUTTONDOWN                equate(0A7H)
SA_WM_NCMBUTTONUP                  equate(0A8H)
SA_WM_NCMBUTTONDBLCLK              equate(0A9H)
SA_WM_KEYFIRST                     equate(0100H)
SA_WM_KEYDOWN                      equate(0100H)
SA_WM_KEYUP                        equate(0101H)
SA_WM_CHAR                         equate(0102H)
SA_WM_DEADCHAR                     equate(0103H)
SA_WM_SYSKEYDOWN                   equate(0104H)
SA_WM_SYSKEYUP                     equate(0105H)
SA_WM_SYSCHAR                      equate(0106H)
SA_WM_SYSDEADCHAR                  equate(0107H)
SA_WM_KEYLAST                      equate(0108H)
SA_WM_INITDIALOG                   equate(0110H)
SA_WM_COMMAND                      equate(0111H)
SA_WM_SYSCOMMAND                   equate(0112H)
SA_WM_TIMER                        equate(0113H)
SA_WM_HSCROLL                      equate(0114H)
SA_WM_VSCROLL                      equate(0115H)
SA_WM_INITMENU                     equate(0116H)
SA_WM_INITMENUPOPUP                equate(0117H)
SA_WM_MENUSELECT                   equate(011FH)
SA_WM_MENUCHAR                     equate(0120H)
SA_WM_ENTERIDLE                    equate(0121H)
SA_WM_MOUSEFIRST                   equate(0200H)
SA_WM_MOUSEMOVE                    equate(0200H)
SA_WM_LBUTTONDOWN                  equate(0201H)
SA_WM_LBUTTONUP                    equate(0202H)
SA_WM_LBUTTONDBLCLK                equate(0203H)
SA_WM_RBUTTONDOWN                  equate(0204H)
SA_WM_RBUTTONUP                    equate(0205H)
SA_WM_RBUTTONDBLCLK                equate(0206H)
SA_WM_MBUTTONDOWN                  equate(0207H)
SA_WM_MBUTTONUP                    equate(0208H)
SA_WM_MBUTTONDBLCLK                equate(0209H)

SA_WM_MOUSEHOVER                   equate(02A1H)
SA_WM_MOUSELEAVE                   equate(02A3H)
SA_WM_XBUTTONDOWN                  equate(020BH)
SA_WM_XBUTTONUP                    equate(020CH)
SA_WM_XBUTTONDBLCLK                equate(020DH)
SA_WM_MOUSELAST                    equate(020AH)

SA_WM_PARENTNOTIFY                 equate(0210H)
SA_WM_MDICREATE                    equate(0220H)
SA_WM_MDIDESTROY                   equate(0221H)
SA_WM_MDIACTIVATE                  equate(0222H)
SA_WM_MDIRESTORE                   equate(0223H)
SA_WM_MDINEXT                      equate(0224H)
SA_WM_MDIMAXIMIZE                  equate(0225H)
SA_WM_MDITILE                      equate(0226H)
SA_WM_MDICASCADE                   equate(0227H)
SA_WM_MDIICONARRANGE               equate(0228H)
SA_WM_MDIGETACTIVE                 equate(0229H)
SA_WM_MDISETMENU                   equate(0230H)
SA_WM_CUT                          equate(0300H)
SA_WM_COPY                         equate(0301H)
SA_WM_PASTE                        equate(0302H)
SA_WM_CLEAR                        equate(0303H)
SA_WM_UNDO                         equate(0304H)
SA_WM_RENDERFORMAT                 equate(0305H)
SA_WM_RENDERALLFORMATS             equate(0306H)
SA_WM_DESTROYCLIPBOARD             equate(0307H)
SA_WM_DRAWCLIPBOARD                equate(0308H)
SA_WM_PAINTCLIPBOARD               equate(0309H)
SA_WM_VSCROLLCLIPBOARD             equate(030AH)
SA_WM_SIZECLIPBOARD                equate(030BH)
SA_WM_ASKCBFORMATNAME              equate(030CH)
SA_WM_CHANGECBCHAIN                equate(030DH)
SA_WM_HSCROLLCLIPBOARD             equate(030EH)
SA_WM_QUERYNEWPALETTE              equate(030FH)
SA_WM_PALETTEISCHANGING            equate(0310H)
SA_WM_PALETTECHANGED               equate(0311H)
SA_WM_USER                         equate(0400H)

SA_WM_NOTIFY                       equate(0004Eh)
SA_WM_INPUTLANGCHANGEREQUEST       equate(00050h)
SA_WM_INPUTLANGCHANGE              equate(00051h)
SA_WM_TCARD                        equate(00052h)
SA_WM_HELP                         equate(00053h)
SA_WM_USERCHANGED                  equate(00054h)
SA_WM_NOTIFYFORMAT                 equate(00055h)

SA_NFR_ANSI                        equate(1)
SA_NFR_UNICODE                     equate(2)
SA_NF_QUERY                        equate(3)
SA_NF_REQUERY                      equate(4)

!Virtual keys
SA_VK_LBUTTON        Equate(01h)
SA_VK_RBUTTON        Equate(02h)
SA_VK_CANCEL         Equate(03h)
SA_VK_MBUTTON        Equate(04h)    !/* NOT contiguous with L & RBUTTON */
SA_VK_XBUTTON1       Equate(05h)    !/* NOT contiguous with L & RBUTTON */
SA_VK_XBUTTON2       Equate(06h)    !/* NOT contiguous with L & RBUTTON */

SA_VK_SHIFT          Equate(10h)
SA_VK_CONTROL        Equate(11h)
SA_VK_MENU           Equate(12h)
SA_VK_PAUSE          Equate(13h)
SA_VK_CAPITAL        Equate(14h)
!Key State Masks for Mouse Messages

SA_MK_LBUTTON                      equate(0001h)
SA_MK_RBUTTON                      equate(0002h)
SA_MK_SHIFT                        equate(0004h)
SA_MK_CONTROL                      equate(0008h)
SA_MK_MBUTTON                      equate(0010h)

SA_NM_FIRST                        equate(0)
SA_NM_CLICK                        equate(SA_NM_FIRST - 2)
SA_NM_DBLCLK                       equate(SA_NM_FIRST - 3)
SA_NM_HOVER                        equate(SA_NM_FIRST - 13)
SA_NM_KILLFOCUS                    equate(SA_NM_FIRST - 8)
SA_NM_RCLICK                       equate(SA_NM_FIRST - 5)
SA_NM_RDBLCLK                      equate(SA_NM_FIRST - 6)
SA_NM_RETURN                       equate(SA_NM_FIRST - 4)
SA_NM_SETFOCUS                     equate(SA_NM_FIRST - 7)

SA_NMHDR                        group,type
hwndFrom                          SA_HWND
idFrom                            long
code                              long
                                end

SA_NMMOUSE          GROUP,TYPE
hdr                     LIKE(SA_NMHDR)
dwItemSpec              UNSIGNED
dwItemData              UNSIGNED
pt                      LIKE(SA_POINT)
dwHitInfo               UNSIGNED
          END

!Window Styles

SA_WS_OVERLAPPED                   equate(00000000h)
SA_WS_POPUP                        equate(80000000h)
SA_WS_CHILD                        equate(40000000h)
SA_WS_MINIMIZE                     equate(20000000h)
SA_WS_VISIBLE                      equate(10000000h)
SA_WS_DISABLED                     equate(08000000h)
SA_WS_CLIPSIBLINGS                 equate(04000000h)
SA_WS_CLIPCHILDREN                 equate(02000000h)
SA_WS_MAXIMIZE                     equate(01000000h)
SA_WS_CAPTION                      equate(00C00000h)  !SA_WS_BORDER | SA_WS_DLGFRAME
SA_WS_BORDER                       equate(00800000h)
SA_WS_DLGFRAME                     equate(00400000h)
SA_WS_VSCROLL                      equate(00200000h)
SA_WS_HSCROLL                      equate(00100000h)
SA_WS_SYSMENU                      equate(00080000h)
SA_WS_THICKFRAME                   equate(00040000h)
SA_WS_GROUP                        equate(00020000h)
SA_WS_TABSTOP                      equate(00010000h)

SA_WS_MINIMIZEBOX                  equate(00020000h)
SA_WS_MAXIMIZEBOX                  equate(00010000h)

!Common Window Styles

SA_WS_OVERLAPPEDWINDOW             equate(SA_WS_OVERLAPPED     + |
                                       SA_WS_CAPTION        + |
                                       SA_WS_SYSMENU        + |
                                       SA_WS_THICKFRAME     + |
                                       SA_WS_MINIMIZEBOX    + |
                                       SA_WS_MAXIMIZEBOX)

SA_WS_POPUPWINDOW                  equate(SA_WS_POPUP          + |
                                       SA_WS_BORDER         + |
                                       SA_WS_SYSMENU)

SA_WS_CHILDWINDOW                  equate(SA_WS_CHILD)

SA_WS_TILED                        equate(SA_WS_OVERLAPPED)
SA_WS_ICONIC                       equate(SA_WS_MINIMIZE)
SA_WS_SIZEBOX                      equate(SA_WS_THICKFRAME)
SA_WS_TILEDWINDOW                  equate(SA_WS_OVERLAPPEDWINDOW)

!Extended Window Styles

SA_WS_EX_DLGMODALFRAME             equate(00000001h)
SA_WS_EX_NOPARENTNOTIFY            equate(00000004h)
SA_WS_EX_TOPMOST                   equate(00000008h)
SA_WS_EX_ACCEPTFILES               equate(00000010h)
SA_WS_EX_TRANSPARENT               equate(00000020h)
SA_WS_EX_MDICHILD                  equate(00000040h)
SA_WS_EX_TOOLWINDOW                equate(00000080h)
SA_WS_EX_WINDOWEDGE                equate(00000100h)
SA_WS_EX_CLIENTEDGE                equate(00000200h)
SA_WS_EX_CONTEXTHELP               equate(00000400h)
SA_WS_EX_RIGHT                     equate(00001000h)
SA_WS_EX_LEFT                      equate(00000000h)
SA_WS_EX_RTLREADING                equate(00002000h)
SA_WS_EX_LTRREADING                equate(00000000h)
SA_WS_EX_LEFTSCROLLBAR             equate(00004000h)
SA_WS_EX_RIGHTSCROLLBAR            equate(00000000h)
SA_WS_EX_CONTROLPARENT             equate(00010000h)
SA_WS_EX_STATICEDGE                equate(00020000h)
SA_WS_EX_APPWINDOW                 equate(00040000h)
SA_WS_EX_OVERLAPPEDWINDOW          equate(SA_WS_EX_WINDOWEDGE + SA_WS_EX_CLIENTEDGE)
SA_WS_EX_PALETTEWINDOW             equate(SA_WS_EX_WINDOWEDGE + SA_WS_EX_TOOLWINDOW + SA_WS_EX_TOPMOST)

!Button Control Styles

SA_BS_PUSHBUTTON                   equate(00000000h)
SA_BS_DEFPUSHBUTTON                equate(00000001h)
SA_BS_CHECKBOX                     equate(00000002h)
SA_BS_AUTOCHECKBOX                 equate(00000003h)
SA_BS_RADIOBUTTON                  equate(00000004h)
SA_BS_3STATE                       equate(00000005h)
SA_BS_AUTO3STATE                   equate(00000006h)
SA_BS_GROUPBOX                     equate(00000007h)
SA_BS_USERBUTTON                   equate(00000008h)
SA_BS_AUTORADIOBUTTON              equate(00000009h)
SA_BS_OWNERDRAW                    equate(0000000Bh)
SA_BS_LEFTTEXT                     equate(00000020h)
SA_BS_TEXT                         equate(00000000h)
SA_BS_ICON                         equate(00000040h)
SA_BS_BITMAP                       equate(00000080h)
SA_BS_LEFT                         equate(00000100h)
SA_BS_RIGHT                        equate(00000200h)
SA_BS_CENTER                       equate(00000300h)
SA_BS_TOP                          equate(00000400h)
SA_BS_BOTTOM                       equate(00000800h)
SA_BS_VCENTER                      equate(00000C00h)
SA_BS_PUSHLIKE                     equate(00001000h)
SA_BS_MULTILINE                    equate(00002000h)
SA_BS_NOTIFY                       equate(00004000h)
SA_BS_FLAT                         equate(00008000h)
SA_BS_RIGHTBUTTON                  equate(SA_BS_LEFTTEXT)


SA_SW_SCROLLCHILDREN               equate(0001h)  ! Scroll children within *lprcScroll.
SA_SW_INVALIDATE                   equate(0002h)  ! Invalidate after scrolling
SA_SW_ERASE                        equate(0004h)  ! If SW_INVALIDATE, don't send SA_WM_ERASEBACKGROUND

SA_SW_SMOOTHSCROLL                 equate(0010h)  ! Use smooth scrolling


SA_LOGBRUSH         Group,Type
lbStyle                 Long
lbColor                 SA_COLORREF
lbHatch                 Long
                    End


!Stock Logical Objects

SA_WHITE_BRUSH                     equate(0)
SA_LTGRAY_BRUSH                    equate(1)
SA_GRAY_BRUSH                      equate(2)
SA_DKGRAY_BRUSH                    equate(3)
SA_BLACK_BRUSH                     equate(4)
SA_NULL_BRUSH                      equate(5)
SA_HOLLOW_BRUSH                    equate(SA_NULL_BRUSH)
SA_WHITE_PEN                       equate(7)
SA_NULL_PEN                        equate(8)
SA_OEM_FIXED_FONT                  equate(10)
SA_ANSI_FIXED_FONT                 equate(11)
SA_ANSI_VAR_FONT                   equate(12)
SA_SYSTEM_FONT                     equate(13)
SA_DEVICE_DEFAULT_FONT             equate(14)
SA_DEFAULT_PALETTE                 equate(15)
SA_SYSTEM_FIXED_FONT               equate(16)
SA_DEFAULT_GUI_FONT                equate(17)

!Class styles

SA_CS_VREDRAW                      equate(0001h)
SA_CS_HREDRAW                      equate(0002h)
SA_CS_KEYCVTWINDOW                 equate(0004h)
SA_CS_DBLCLKS                      equate(0008h)
SA_CS_OWNDC                        equate(0020h)
SA_CS_CLASSDC                      equate(0040h)
SA_CS_PARENTDC                     equate(0080h)
SA_CS_NOKEYCVT                     equate(0100h)
SA_CS_NOCLOSE                      equate(0200h)
SA_CS_SAVEBITS                     equate(0800h)
SA_CS_BYTEALIGNCLIENT              equate(1000h)
SA_CS_BYTEALIGNWINDOW              equate(2000h)
SA_CS_GLOBALCLASS                  equate(4000h)
SA_CS_IME                          equate(00010000h)

!SetWindowPos Flags

SA_SWP_NOSIZE                      equate(0001h)
SA_SWP_NOMOVE                      equate(0002h)
SA_SWP_NOZORDER                    equate(0004h)
SA_SWP_NOREDRAW                    equate(0008h)
SA_SWP_NOACTIVATE                  equate(0010h)
SA_SWP_FRAMECHANGED                equate(0020h)  !The frame changed: send SA_WM_NCCALCSIZE
SA_SWP_SHOWWINDOW                  equate(0040h)
SA_SWP_HIDEWINDOW                  equate(0080h)
SA_SWP_NOCOPYBITS                  equate(0100h)
SA_SWP_NOOWNERZORDER               equate(0200h)  !Don't do owner Z ordering
SA_SWP_NOSENDCHANGING              equate(0400h)  !Don't send SA_WM_WINDOWPOSCHANGING

SA_SWP_DRAWFRAME                   equate(SA_SWP_FRAMECHANGED)
SA_SWP_NOREPOSITION                equate(SA_SWP_NOOWNERZORDER)

SA_SWP_DEFERERASE                  equate(2000h)
SA_SWP_ASYNCWINDOWPOS              equate(4000h)

SA_HWND_TOP                        equate(0)
SA_HWND_BOTTOM                     equate(1)
SA_HWND_TOPMOST                    equate(-1)
SA_HWND_NOTOPMOST                  equate(-2)

!3D border styles

SA_BDR_RAISEDOUTER                 equate(0001h)
SA_BDR_SUNKENOUTER                 equate(0002h)
SA_BDR_RAISEDINNER                 equate(0004h)
SA_BDR_SUNKENINNER                 equate(0008h)
SA_BDR_OUTER                       equate(0003h)
SA_BDR_INNER                       equate(000ch)
SA_BDR_RAISED                      equate(0005h)
SA_BDR_SUNKEN                      equate(000ah)

SA_EDGE_RAISED                     equate(SA_BDR_RAISEDOUTER + SA_BDR_RAISEDINNER)
SA_EDGE_SUNKEN                     equate(SA_BDR_SUNKENOUTER + SA_BDR_SUNKENINNER)
SA_EDGE_ETCHED                     equate(SA_BDR_SUNKENOUTER + SA_BDR_RAISEDINNER)
SA_EDGE_BUMP                       equate(SA_BDR_RAISEDOUTER + SA_BDR_SUNKENINNER)

!Border flags

SA_BF_LEFT                         equate(0001h)
SA_BF_TOP                          equate(0002h)
SA_BF_RIGHT                        equate(0004h)
SA_BF_BOTTOM                       equate(0008h)

SA_BF_TOPLEFT                      equate(SA_BF_TOP + SA_BF_LEFT)
SA_BF_TOPRIGHT                     equate(SA_BF_TOP + SA_BF_RIGHT)
SA_BF_BOTTOMLEFT                   equate(SA_BF_BOTTOM + SA_BF_LEFT)
SA_BF_BOTTOMRIGHT                  equate(SA_BF_BOTTOM + SA_BF_RIGHT)
SA_BF_RECT                         equate(SA_BF_LEFT + SA_BF_TOP + SA_BF_RIGHT + SA_BF_BOTTOM)
SA_BF_DIAGONAL                     equate(0010h)

SA_BF_DIAGONAL_ENDTOPRIGHT         equate(SA_BF_DIAGONAL + SA_BF_TOP + SA_BF_RIGHT)
SA_BF_DIAGONAL_ENDTOPLEFT          equate(SA_BF_DIAGONAL + SA_BF_TOP + SA_BF_LEFT)
SA_BF_DIAGONAL_ENDBOTTOMLEFT       equate(SA_BF_DIAGONAL + SA_BF_BOTTOM + SA_BF_LEFT)
SA_BF_DIAGONAL_ENDBOTTOMRIGHT      equate(SA_BF_DIAGONAL + SA_BF_BOTTOM + SA_BF_RIGHT)

SA_BF_MIDDLE                       equate(0800h)  !Fill in the middle
SA_BF_SOFT                         equate(1000h)  !For softer buttons
SA_BF_ADJUST                       equate(2000h)  !Calculate the space left over
SA_BF_FLAT                         equate(4000h)  !For flat rather than 3D borders
SA_BF_MONO                         equate(8000h)  !For monochrome borders

!DrawText() Format Flags

SA_DT_TOP                          equate(00000000h)
SA_DT_LEFT                         equate(00000000h)
SA_DT_CENTER                       equate(00000001h)
SA_DT_RIGHT                        equate(00000002h)
SA_DT_VCENTER                      equate(00000004h)
SA_DT_BOTTOM                       equate(00000008h)
SA_DT_WORDBREAK                    equate(00000010h)
SA_DT_SINGLELINE                   equate(00000020h)
SA_DT_EXPANDTABS                   equate(00000040h)
SA_DT_TABSTOP                      equate(00000080h)
SA_DT_NOCLIP                       equate(00000100h)
SA_DT_EXTERNALLEADING              equate(00000200h)
SA_DT_CALCRECT                     equate(00000400h)
SA_DT_NOPREFIX                     equate(00000800h)
SA_DT_INTERNAL                     equate(00001000h)
SA_DT_EDITCONTROL                  equate(00002000h)
SA_DT_PATH_ELLIPSIS                equate(00004000h)
SA_DT_END_ELLIPSIS                 equate(00008000h)
SA_DT_MODIFYSTRING                 equate(00010000h)
SA_DT_RTLREADING                   equate(00020000h)
SA_DT_WORD_ELLIPSIS                equate(00040000h)

SA_DT_HIDEPREFIX               equate(00100000h)
SA_DT_PREFIXONLY               equate(00200000h)

!Background Modes

SA_TRANSPARENT                     equate(1)
SA_OPAQUE                          equate(2)
SA_BKMODE_LAST                     equate(2)

!Font Weights

SA_FW_DONTCARE                     equate(0)
SA_FW_THIN                         equate(100)
SA_FW_EXTRALIGHT                   equate(200)
SA_FW_LIGHT                        equate(300)
SA_FW_NORMAL                       equate(400)
SA_FW_MEDIUM                       equate(500)
SA_FW_SEMIBOLD                     equate(600)
SA_FW_BOLD                         equate(700)
SA_FW_EXTRABOLD                    equate(800)
SA_FW_HEAVY                        equate(900)

SA_FW_ULTRALIGHT                   equate(SA_FW_EXTRALIGHT)
SA_FW_REGULAR                      equate(SA_FW_NORMAL)
SA_FW_DEMIBOLD                     equate(SA_FW_SEMIBOLD)
SA_FW_ULTRABOLD                    equate(SA_FW_EXTRABOLD)
SA_FW_BLACK                        equate(SA_FW_HEAVY)

SA_PANOSE_COUNT                    equate(10)
SA_PAN_FAMILYTYPE_INDEX            equate(0)
SA_PAN_SERIFSTYLE_INDEX            equate(1)
SA_PAN_WEIGHT_INDEX                equate(2)
SA_PAN_PROPORTION_INDEX            equate(3)
SA_PAN_CONTRAST_INDEX              equate(4)
SA_PAN_STROKEVARIATION_INDEX       equate(5)
SA_PAN_ARMSTYLE_INDEX              equate(6)
SA_PAN_LETTERFORM_INDEX            equate(7)
SA_PAN_MIDLINE_INDEX               equate(8)
SA_PAN_XHEIGHT_INDEX               equate(9)

SA_PAN_CULTURE_LATIN               equate(0)

SA_ANSI_CHARSET                    equate(0)
SA_DEFAULT_CHARSET                 equate(1)
SA_SYMBOL_CHARSET                  equate(2)
SA_SHIFTJIS_CHARSET                equate(128)
SA_HANGEUL_CHARSET                 equate(129)
SA_GB2312_CHARSET                  equate(134)
SA_CHINESEBIG5_CHARSET             equate(136)
SA_OEM_CHARSET                     equate(255)
SA_JOHAB_CHARSET                   equate(130)
SA_HEBREW_CHARSET                  equate(177)
SA_ARABIC_CHARSET                  equate(178)
SA_GREEK_CHARSET                   equate(161)
SA_TURKISH_CHARSET                 equate(162)
SA_VIETNAMESE_CHARSET              equate(163)
SA_THAI_CHARSET                    equate(222)
SA_EASTEUROPE_CHARSET              equate(238)
SA_RUSSIAN_CHARSET                 equate(204)

SA_MAC_CHARSET                     equate(77)
SA_BALTIC_CHARSET                  equate(186)

SA_FS_LATIN1                       equate(00000001h)
SA_FS_LATIN2                       equate(00000002h)
SA_FS_CYRILLIC                     equate(00000004h)
SA_FS_GREEK                        equate(00000008h)
SA_FS_TURKISH                      equate(00000010h)
SA_FS_HEBREW                       equate(00000020h)
SA_FS_ARABIC                       equate(00000040h)
SA_FS_BALTIC                       equate(00000080h)
SA_FS_VIETNAMESE                   equate(00000100h)
SA_FS_THAI                         equate(00010000h)
SA_FS_JISJAPAN                     equate(00020000h)
SA_FS_CHINESESIMP                  equate(00040000h)
SA_FS_WANSUNG                      equate(00080000h)
SA_FS_CHINESETRAD                  equate(00100000h)
SA_FS_JOHAB                        equate(00200000h)
SA_FS_SYMBOL                       equate(80000000h)

SA_OUT_DEFAULT_PRECIS              equate(0)
SA_OUT_STRING_PRECIS               equate(1)
SA_OUT_CHARACTER_PRECIS            equate(2)
SA_OUT_STROKE_PRECIS               equate(3)
SA_OUT_TT_PRECIS                   equate(4)
SA_OUT_DEVICE_PRECIS               equate(5)
SA_OUT_RASTER_PRECIS               equate(6)
SA_OUT_TT_ONLY_PRECIS              equate(7)
SA_OUT_OUTLINE_PRECIS              equate(8)
SA_OUT_SCREEN_OUTLINE_PRECIS       equate(9)

SA_CLIP_DEFAULT_PRECIS             equate(0)
SA_CLIP_CHARACTER_PRECIS           equate(1)
SA_CLIP_STROKE_PRECIS              equate(2)
SA_CLIP_MASK                       equate(0fh)
SA_CLIP_LH_ANGLES                  equate(8)
SA_CLIP_TT_ALWAYS                  equate(32)
SA_CLIP_EMBEDDED                   equate(128)

SA_DEFAULT_QUALITY                 equate(0)
SA_DRAFT_QUALITY                   equate(1)
SA_PROOF_QUALITY                   equate(2)
SA_NONANTIALIASED_QUALITY          equate(3)
SA_ANTIALIASED_QUALITY             equate(4)

!Font Families

SA_FF_DONTCARE                     equate(0)  !Don't care or don't know.
SA_FF_ROMAN                        equate(8)  !Variable stroke width, serifed.
                                           !   Times Roman, Century Schoolbook, etc.
SA_FF_SWISS                        equate(32) !Variable stroke width, sans-serifed.
                                           !   Helvetica, Swiss, etc.
SA_FF_MODERN                       equate(48) !Constant stroke width, serifed or sans-serifed.
                                           !   Pica, Elite, Courier, etc.
SA_FF_SCRIPT                       equate(64) !Cursive, etc.
SA_FF_DECORATIVE                   equate(80) !Old English, etc.



!/* EnumFonts Masks */
SA_RASTER_FONTTYPE     EQUATE(1h)
SA_DEVICE_FONTTYPE     EQUATE(2h)
SA_TRUETYPE_FONTTYPE   EQUATE(4h)


!Predefined Resource Types

SA_RT_CURSOR                       equate(1)
SA_RT_BITMAP                       equate(2)
SA_RT_ICON                         equate(3)
SA_RT_MENU                         equate(4)
SA_RT_DIALOG                       equate(5)
SA_RT_STRING                       equate(6)
SA_RT_FONTDIR                      equate(7)
SA_RT_FONT                         equate(8)
SA_RT_ACCELERATOR                  equate(9)
SA_RT_RCDATA                       equate(10)
SA_RT_MESSAGETABLE                 equate(11)

SA_DIFFERENCE                      equate(11)
SA_RT_GROUP_CURSOR                 equate(SA_RT_CURSOR + SA_DIFFERENCE)
SA_RT_GROUP_ICON                   equate(SA_RT_ICON + SA_DIFFERENCE)
SA_RT_VERSION                      equate(16)
SA_RT_DLGINCLUDE                   equate(17)
SA_RT_PLUGPLAY                     equate(19)
SA_RT_VXD                          equate(20)
SA_RT_ANICURSOR                    equate(21)
SA_RT_ANIICON                      equate(22)

!ternary raster operations

SA_SRCCOPY                         equate(00CC0020h) ! dest = source
SA_SRCPAINT                        equate(00EE0086h) ! dest = source OR dest
SA_SRCAND                          equate(008800C6h) ! dest = source AND dest
SA_SRCINVERT                       equate(00660046h) ! dest = source XOR dest
SA_SRCERASE                        equate(00440328h) ! dest = source AND (NOT dest )
SA_NOTSRCCOPY                      equate(00330008h) ! dest = (NOT source)
SA_NOTSRCERASE                     equate(001100A6h) ! dest = (NOT src) AND (NOT dest)
SA_MERGECOPY                       equate(00C000CAh) ! dest = (source AND pattern)
SA_MERGEPAINT                      equate(00BB0226h) ! dest = (NOT source) OR dest
SA_PATCOPY                         equate(00F00021h) ! dest = pattern
SA_PATPAINT                        equate(00FB0A09h) ! dest = DPSnoo
SA_PATINVERT                       equate(005A0049h) ! dest = pattern XOR dest
SA_DSTINVERT                       equate(00550009h) ! dest = (NOT dest)
SA_BLACKNESS                       equate(00000042h) ! dest = BLACK
SA_WHITENESS                       equate(00FF0062h) ! dest = WHITE

!PolyFill() Modes

SA_ALTERNATE                       equate(1)
SA_WINDING                         equate(2)
SA_POLYFILL_LAST                   equate(2)

SA_IMAGE_BITMAP                    equate(0)
SA_IMAGE_ICON                      equate(1)
SA_IMAGE_CURSOR                    equate(2)
SA_IMAGE_ENHMETAFILE               equate(3)

SA_LR_DEFAULTCOLOR                 equate(0000h)
SA_LR_MONOCHROME                   equate(0001h)
SA_LR_COLOR                        equate(0002h)
SA_LR_COPYRETURNORG                equate(0004h)
SA_LR_COPYDELETEORG                equate(0008h)
SA_LR_LOADFROMFILE                 equate(0010h)
SA_LR_LOADTRANSPARENT              equate(0020h)
SA_LR_DEFAULTSIZE                  equate(0040h)
SA_LR_VGACOLOR                     equate(0080h)
SA_LR_LOADMAP3DCOLORS              equate(1000h)
SA_LR_CREATEDIBSECTION             equate(2000h)
SA_LR_COPYFROMRESOURCE             equate(4000h)
SA_LR_SHARED                       equate(8000h)

!RedrawWindow() flags

SA_RDW_INVALIDATE                  equate(0001h)
SA_RDW_INTERNALPAINT               equate(0002h)
SA_RDW_ERASE                       equate(0004h)
SA_RDW_VALIDATE                    equate(0008h)
SA_RDW_NOINTERNALPAINT             equate(0010h)
SA_RDW_NOERASE                     equate(0020h)
SA_RDW_NOCHILDREN                  equate(0040h)
SA_RDW_ALLCHILDREN                 equate(0080h)
SA_RDW_UPDATENOW                   equate(0100h)
SA_RDW_ERASENOW                    equate(0200h)
SA_RDW_FRAME                       equate(0400h)
SA_RDW_NOFRAME                     equate(0800h)

!flags for DrawFrameControl

SA_DFC_CAPTION                     equate(1)
SA_DFC_MENU                        equate(2)
SA_DFC_SCROLL                      equate(3)
SA_DFC_BUTTON                      equate(4)

SA_DFCS_CAPTIONCLOSE               equate(0000h)
SA_DFCS_CAPTIONMIN                 equate(0001h)
SA_DFCS_CAPTIONMAX                 equate(0002h)
SA_DFCS_CAPTIONRESTORE             equate(0003h)
SA_DFCS_CAPTIONHELP                equate(0004h)
SA_DFCS_MENUARROW                  equate(0000h)
SA_DFCS_MENUCHECK                  equate(0001h)
SA_DFCS_MENUBULLET                 equate(0002h)
SA_DFCS_MENUARROWRIGHT             equate(0004h)
SA_DFCS_SCROLLUP                   equate(0000h)
SA_DFCS_SCROLLDOWN                 equate(0001h)
SA_DFCS_SCROLLLEFT                 equate(0002h)
SA_DFCS_SCROLLRIGHT                equate(0003h)
SA_DFCS_SCROLLCOMBOBOX             equate(0005h)
SA_DFCS_SCROLLSIZEGRIP             equate(0008h)
SA_DFCS_SCROLLSIZEGRIPRIGHT        equate(0010h)
SA_DFCS_BUTTONCHECK                equate(0000h)
SA_DFCS_BUTTONRADIOIMAGE           equate(0001h)
SA_DFCS_BUTTONRADIOMASK            equate(0002h)
SA_DFCS_BUTTONRADIO                equate(0004h)
SA_DFCS_BUTTON3STATE               equate(0008h)
SA_DFCS_BUTTONPUSH                 equate(0010h)
SA_DFCS_INACTIVE                   equate(0100h)
SA_DFCS_PUSHED                     equate(0200h)
SA_DFCS_CHECKED                    equate(0400h)
SA_DFCS_ADJUSTRECT                 equate(2000h)
SA_DFCS_FLAT                       equate(4000h)
SA_DFCS_MONO                       equate(8000h)

SA_ETO_OPAQUE                      equate(00002h)
SA_ETO_CLIPPED                     equate(00004h)
!#if(WINVER >= 0x0400)
SA_ETO_GLYPH_INDEX                 equate(00010h)
SA_ETO_RTLREADING                  equate(00080h)
SA_ETO_NUMERICSLOCAL               equate(00400h)
SA_ETO_NUMERICSLATIN               equate(00800h)
SA_ETO_IGNORELANGUAGE              equate(01000h)
!#endif /* WINVER >= 0x0400 */
!#if (_WIN32_WINNT >= 0x0500)
SA_ETO_PDY                         equate(02000h)
!#endif // (_WIN32_WINNT >= 0x0500)


SA_RGN_ERROR                       equate(0)
SA_NULLREGION                      equate(1)
SA_SIMPLEREGION                    equate(2)
SA_COMPLEXREGION                   equate(3)

!/* CombineRgn() Styles */
SA_RGN_AND                         Equate(1)
SA_RGN_OR                          Equate(2)
SA_RGN_XOR                         Equate(3)
SA_RGN_DIFF                        Equate(4)
SA_RGN_COPY                        Equate(5)
SA_RGN_MIN                         Equate(SA_RGN_AND)
SA_RGN_MAX                         Equate(SA_RGN_COPY)


SA_RGNDATAHEADER                group,type
dwSize                            long
iType                             long
nCount                            long
nRgnSize                          long
rcBound                           Like(SA_RECT)
                                end

SA_RGNDATA                      group,type
rdh                               Like(SA_RGNDATAHEADER)
Buffer                            &string
                                end

SA_COLOR_SCROLLBAR                 equate(0)
SA_COLOR_BACKGROUND                equate(1)
SA_COLOR_ACTIVECAPTION             equate(2)
SA_COLOR_INACTIVECAPTION           equate(3)
SA_COLOR_MENU                      equate(4)
SA_COLOR_WINDOW                    equate(5)
SA_COLOR_WINDOWFRAME               equate(6)
SA_COLOR_MENUTEXT                  equate(7)
SA_COLOR_WINDOWTEXT                equate(8)
SA_COLOR_CAPTIONTEXT               equate(9)
SA_COLOR_ACTIVEBORDER              equate(10)
SA_COLOR_INACTIVEBORDER            equate(11)
SA_COLOR_APPWORKSPACE              equate(12)
SA_COLOR_HIGHLIGHT                 equate(13)
SA_COLOR_HIGHLIGHTTEXT             equate(14)
SA_COLOR_BTNFACE                   equate(15)
SA_COLOR_BTNSHADOW                 equate(16)
SA_COLOR_GRAYTEXT                  equate(17)
SA_COLOR_BTNTEXT                   equate(18)
SA_COLOR_INACTIVECAPTIONTEXT       equate(19)
SA_COLOR_BTNHIGHLIGHT              equate(20)
SA_COLOR_3DDKSHADOW                equate(21)
SA_COLOR_3DLIGHT                   equate(22)
SA_COLOR_INFOTEXT                  equate(23)
SA_COLOR_INFOBK                    equate(24)
SA_COLOR_DESKTOP                   equate(SA_COLOR_BACKGROUND)
SA_COLOR_3DFACE                    equate(SA_COLOR_BTNFACE)
SA_COLOR_3DSHADOW                  equate(SA_COLOR_BTNSHADOW)
SA_COLOR_3DHIGHLIGHT               equate(SA_COLOR_BTNHIGHLIGHT)
SA_COLOR_3DHILIGHT                 equate(SA_COLOR_BTNHIGHLIGHT)
SA_COLOR_BTNHILIGHT                equate(SA_COLOR_BTNHIGHLIGHT)

!Brush Styles
SA_BS_SOLID            equate(0)
SA_BS_NULL             equate(1)
SA_BS_HOLLOW           equate(SA_BS_NULL)
SA_BS_HATCHED          equate(2)
SA_BS_PATTERN          equate(3)
SA_BS_INDEXED          equate(4)
SA_BS_DIBPATTERN       equate(5)
SA_BS_DIBPATTERNPT     equate(6)
SA_BS_PATTERN8X8       equate(7)
SA_BS_DIBPATTERN8X8    equate(8)
SA_BS_MONOPATTERN      equate(9)

!Hatch Styles

SA_HS_HORIZONTAL                   equate(0)       ! -----
SA_HS_VERTICAL                     equate(1)       ! |||||
SA_HS_FDIAGONAL                    equate(2)       ! \\\\\
SA_HS_BDIAGONAL                    equate(3)       ! /////
SA_HS_CROSS                        equate(4)       ! +++++
SA_HS_DIAGCROSS                    equate(5)       ! xxxxx

!Window field offsets for GetWindowLong() / SetWindowLong()

SA_GWL_WNDPROC                     equate(-4)
SA_GWL_HINSTANCE                   equate(-6)
SA_GWL_HWNDPARENT                  equate(-8)
SA_GWL_STYLE                       equate(-16)
SA_GWL_EXSTYLE                     equate(-20)
SA_GWL_USERDATA                    equate(-21)
SA_GWL_ID                          equate(-12)

!ShowWindow() Commands

SA_SW_HIDE                         equate(0)
SA_SW_SHOWNORMAL                   equate(1)
SA_SW_NORMAL                       equate(1)
SA_SW_SHOWMINIMIZED                equate(2)
SA_SW_SHOWMAXIMIZED                equate(3)
SA_SW_MAXIMIZE                     equate(3)
SA_SW_SHOWNOACTIVATE               equate(4)
SA_SW_SHOW                         equate(5)
SA_SW_MINIMIZE                     equate(6)
SA_SW_SHOWMINNOACTIVE              equate(7)
SA_SW_SHOWNA                       equate(8)
SA_SW_RESTORE                      equate(9)
SA_SW_SHOWDEFAULT                  equate(10)
SA_SW_MAX                          equate(10)

!Old ShowWindow() Commands

SA_HIDE_WINDOW                     equate(0)
SA_SHOW_OPENWINDOW                 equate(1)
SA_SHOW_ICONWINDOW                 equate(2)
SA_SHOW_FULLSCREEN                 equate(3)
SA_SHOW_OPENNOACTIVATE             equate(4)

!Identifiers for the SA_WM_SHOWWINDOW message

SA_SW_PARENTCLOSING                equate(1)
SA_SW_OTHERZOOM                    equate(2)
SA_SW_PARENTOPENING                equate(3)
SA_SW_OTHERUNZOOM                  equate(4)

!User Button Notification Codes

SA_BN_CLICKED                      equate(0)
SA_BN_PAINT                        equate(1)
SA_BN_HILITE                       equate(2)
SA_BN_UNHILITE                     equate(3)
SA_BN_DISABLE                      equate(4)
SA_BN_DOUBLECLICKED                equate(5)
SA_BN_PUSHED                       equate(SA_BN_HILITE)
SA_BN_UNPUSHED                     equate(SA_BN_UNHILITE)
SA_BN_DBLCLK                       equate(SA_BN_DOUBLECLICKED)
SA_BN_SETFOCUS                     equate(6)
SA_BN_KILLFOCUS                    equate(7)

!Button Control Messages

SA_BM_GETCHECK                     equate(00F0h)
SA_BM_SETCHECK                     equate(00F1h)
SA_BM_GETSTATE                     equate(00F2h)
SA_BM_SETSTATE                     equate(00F3h)
SA_BM_SETSTYLE                     equate(00F4h)
SA_BM_CLICK                        equate(00F5h)
SA_BM_GETIMAGE                     equate(00F6h)
SA_BM_SETIMAGE                     equate(00F7h)

SA_BST_UNCHECKED                   equate(0000h)
SA_BST_CHECKED                     equate(0001h)
SA_BST_INDETERMINATE               equate(0002h)
SA_BST_PUSHED                      equate(0004h)
SA_BST_FOCUS                       equate(0008h)

!MessageBox() Flags

SA_MB_OK                           equate(00000000h)
SA_MB_OKCANCEL                     equate(00000001h)
SA_MB_ABORTRETRYIGNORE             equate(00000002h)
SA_MB_YESNOCANCEL                  equate(00000003h)
SA_MB_YESNO                        equate(00000004h)
SA_MB_RETRYCANCEL                  equate(00000005h)

SA_MB_ICONHAND                     equate(00000010h)
SA_MB_ICONQUESTION                 equate(00000020h)
SA_MB_ICONEXCLAMATION              equate(00000030h)
SA_MB_ICONASTERISK                 equate(00000040h)

SA_MB_USERICON                     equate(00000080h)
SA_MB_ICONWARNING                  equate(SA_MB_ICONEXCLAMATION)
SA_MB_ICONERROR                    equate(SA_MB_ICONHAND)

SA_MB_ICONINFORMATION              equate(SA_MB_ICONASTERISK)
SA_MB_ICONSTOP                     equate(SA_MB_ICONHAND)

SA_MB_DEFBUTTON1                   equate(00000000h)
SA_MB_DEFBUTTON2                   equate(00000100h)
SA_MB_DEFBUTTON3                   equate(00000200h)

SA_MB_DEFBUTTON4                   equate(00000300h)

SA_MB_APPLMODAL                    equate(00000000h)
SA_MB_SYSTEMMODAL                  equate(00001000h)
SA_MB_TASKMODAL                    equate(00002000h)

SA_MB_HELP                         equate(00004000h) ! Help Button

SA_MB_NOFOCUS                      equate(00008000h)
SA_MB_SETFOREGROUND                equate(00010000h)
SA_MB_DEFAULT_DESKTOP_ONLY         equate(00020000h)

SA_MB_TOPMOST                      equate(00040000h)
SA_MB_RIGHT                        equate(00080000h)
SA_MB_RTLREADING                   equate(00100000h)

SA_MB_TYPEMASK                     equate(0000000Fh)
SA_MB_ICONMASK                     equate(000000F0h)
SA_MB_DEFMASK                      equate(00000F00h)
SA_MB_MODEMASK                     equate(00003000h)
SA_MB_MISCMASK                     equate(0000C000h)

!Global Memory Flags

SA_GMEM_FIXED                      equate(0000h)
SA_GMEM_MOVEABLE                   equate(0002h)
SA_GMEM_NOCOMPACT                  equate(0010h)
SA_GMEM_NODISCARD                  equate(0020h)
SA_GMEM_ZEROINIT                   equate(0040h)
SA_GMEM_MODIFY                     equate(0080h)
SA_GMEM_DISCARDABLE                equate(0100h)
SA_GMEM_NOT_BANKED                 equate(1000h)
SA_GMEM_SHARE                      equate(2000h)
SA_GMEM_DDESHARE                   equate(2000h)
SA_GMEM_NOTIFY                     equate(4000h)
SA_GMEM_LOWER                      equate(SA_GMEM_NOT_BANKED)
SA_GMEM_VALID_FLAGS                equate(7F72h)
SA_GMEM_INVALID_HANDLE             equate(8000h)

SA_GHND                            equate(SA_GMEM_MOVEABLE + SA_GMEM_ZEROINIT)
SA_GPTR                            equate(SA_GMEM_FIXED + SA_GMEM_ZEROINIT)

!Flags returned by GlobalFlags (in addition to SA_GMEM_DISCARDABLE)

SA_GMEM_DISCARDED                  equate(4000h)
SA_GMEM_LOCKCOUNT                  equate(00FFh)

! GetSystemMetrics() codes

SA_SM_CXSCREEN                     equate(0)
SA_SM_CYSCREEN                     equate(1)
SA_SM_CXVSCROLL                    equate(2)
SA_SM_CYHSCROLL                    equate(3)
SA_SM_CYCAPTION                    equate(4)
SA_SM_CXBORDER                     equate(5)
SA_SM_CYBORDER                     equate(6)
SA_SM_CXDLGFRAME                   equate(7)
SA_SM_CYDLGFRAME                   equate(8)
SA_SM_CYVTHUMB                     equate(9)
SA_SM_CXHTHUMB                     equate(10)
SA_SM_CXICON                       equate(11)
SA_SM_CYICON                       equate(12)
SA_SM_CXCURSOR                     equate(13)
SA_SM_CYCURSOR                     equate(14)
SA_SM_CYMENU                       equate(15)
SA_SM_CXFULLSCREEN                 equate(16)
SA_SM_CYFULLSCREEN                 equate(17)
SA_SM_CYKANJIWINDOW                equate(18)
SA_SM_MOUSEPRESENT                 equate(19)
SA_SM_CYVSCROLL                    equate(20)
SA_SM_CXHSCROLL                    equate(21)
SA_SM_DEBUG                        equate(22)
SA_SM_SWAPBUTTON                   equate(23)
SA_SM_RESERVED1                    equate(24)
SA_SM_RESERVED2                    equate(25)
SA_SM_RESERVED3                    equate(26)
SA_SM_RESERVED4                    equate(27)
SA_SM_CXMIN                        equate(28)
SA_SM_CYMIN                        equate(29)
SA_SM_CXSIZE                       equate(30)
SA_SM_CYSIZE                       equate(31)
SA_SM_CXFRAME                      equate(32)
SA_SM_CYFRAME                      equate(33)
SA_SM_CXMINTRACK                   equate(34)
SA_SM_CYMINTRACK                   equate(35)
SA_SM_CXDOUBLECLK                  equate(36)
SA_SM_CYDOUBLECLK                  equate(37)
SA_SM_CXICONSPACING                equate(38)
SA_SM_CYICONSPACING                equate(39)
SA_SM_MENUDROPALIGNMENT            equate(40)
SA_SM_PENWINDOWS                   equate(41)
SA_SM_DBCSENABLED                  equate(42)
SA_SM_CMOUSEBUTTONS                equate(43)

SA_SM_CXFIXEDFRAME                 equate(SA_SM_CXDLGFRAME)  ! ;win40 name change
SA_SM_CYFIXEDFRAME                 equate(SA_SM_CYDLGFRAME)  ! ;win40 name change
SA_SM_CXSIZEFRAME                  equate(SA_SM_CXFRAME)     ! ;win40 name change
SA_SM_CYSIZEFRAME                  equate(SA_SM_CYFRAME)     ! ;win40 name change

SA_SM_SECURE                       equate(44)
SA_SM_CXEDGE                       equate(45)
SA_SM_CYEDGE                       equate(46)
SA_SM_CXMINSPACING                 equate(47)
SA_SM_CYMINSPACING                 equate(48)
SA_SM_CXSMICON                     equate(49)
SA_SM_CYSMICON                     equate(50)
SA_SM_CYSMCAPTION                  equate(51)
SA_SM_CXSMSIZE                     equate(52)
SA_SM_CYSMSIZE                     equate(53)
SA_SM_CXMENUSIZE                   equate(54)
SA_SM_CYMENUSIZE                   equate(55)
SA_SM_ARRANGE                      equate(56)
SA_SM_CXMINIMIZED                  equate(57)
SA_SM_CYMINIMIZED                  equate(58)
SA_SM_CXMAXTRACK                   equate(59)
SA_SM_CYMAXTRACK                   equate(60)
SA_SM_CXMAXIMIZED                  equate(61)
SA_SM_CYMAXIMIZED                  equate(62)
SA_SM_NETWORK                      equate(63)
SA_SM_CLEANBOOT                    equate(67)
SA_SM_CXDRAG                       equate(68)
SA_SM_CYDRAG                       equate(69)
SA_SM_SHOWSOUNDS                   equate(70)
SA_SM_CXMENUCHECK                  equate(71)   ! Use instead of GetMenuCheckMarkDimensions()!
SA_SM_CYMENUCHECK                  equate(72)
SA_SM_SLOWMACHINE                  equate(73)
SA_SM_MIDEASTENABLED               equate(74)

SA_SM_MOUSEWHEELPRESENT    equate(75)
SA_SM_XVIRTUALSCREEN       equate(76)
SA_SM_YVIRTUALSCREEN       equate(77)
SA_SM_CXVIRTUALSCREEN      equate(78)
SA_SM_CYVIRTUALSCREEN      equate(79)
SA_SM_CMONITORS            equate(80)
SA_SM_SAMEDISPLAYFORMAT    equate(81)
SA_SM_IMMENABLED           equate(82)
SA_SM_CXFOCUSBORDER        equate(83)
SA_SM_CYFOCUSBORDER        equate(84)
SA_SM_TABLETPC             equate(86)
SA_SM_MEDIACENTER          equate(87)
SA_SM_STARTER              equate(88) !xp starter addition
SA_SM_SERVERR2             equate(89)


! Scroll Bar Styles

SA_SBS_HORZ                        equate(0000)
SA_SBS_VERT                        equate(0001)
SA_SBS_TOPALIGN                    equate(0002)
SA_SBS_LEFTALIGN                   equate(0002)
SA_SBS_BOTTOMALIGN                 equate(0004)
SA_SBS_RIGHTALIGN                  equate(0004)
SA_SBS_SIZEBOXTOPLEFTALIGN         equate(0002)
SA_SBS_SIZEBOXBOTTOMRIGHTALIGN     equate(0004)
SA_SBS_SIZEBOX                     equate(0008)
SA_SBS_SIZEGRIP                    equate(0010)

! Scroll bar messages

SA_SBM_SETPOS                      equate(00E0h) !not in win3.1
SA_SBM_GETPOS                      equate(00E1h) !not in win3.1
SA_SBM_SETRANGE                    equate(00E2h) !not in win3.1
SA_SBM_SETRANGEREDRAW              equate(00E6h) !not in win3.1
SA_SBM_GETRANGE                    equate(00E3h) !not in win3.1
SA_SBM_ENABLE_ARROWS               equate(00E4h) !not in win3.1
SA_SBM_SETSCROLLINFO               equate(00E9h)
SA_SBM_GETSCROLLINFO               equate(00EAh)

SA_SIF_RANGE                       equate(0001)
SA_SIF_PAGE                        equate(0002)
SA_SIF_POS                         equate(0004)
SA_SIF_DISABLENOSCROLL             equate(0008)
SA_SIF_TRACKPOS                    equate(0010)
SA_SIF_ALL                         equate(SA_SIF_RANGE + SA_SIF_POS + SA_SIF_TRACKPOS)

SA_SCROLLINFO                  group,type
cbSize                            unsigned
fMask                             unsigned
nMin                              signed
nMax                              signed
nPage                             unsigned
nPos                              signed
nTrackPos                         signed
                                end

! Scroll Bar Constants

SA_SB_HORZ                         equate(0)
SA_SB_VERT                         equate(1)
SA_SB_CTL                          equate(2)
SA_SB_BOTH                         equate(3)

! Scroll Bar Commands

SA_SB_LINEUP                       equate(0)
SA_SB_LINELEFT                     equate(0)
SA_SB_LINEDOWN                     equate(1)
SA_SB_LINERIGHT                    equate(1)
SA_SB_PAGEUP                       equate(2)
SA_SB_PAGELEFT                     equate(2)
SA_SB_PAGEDOWN                     equate(3)
SA_SB_PAGERIGHT                    equate(3)
SA_SB_THUMBPOSITION                equate(4)
SA_SB_THUMBTRACK                   equate(5)
SA_SB_TOP                          equate(6)
SA_SB_LEFT                         equate(6)
SA_SB_BOTTOM                       equate(7)
SA_SB_RIGHT                        equate(7)
SA_SB_ENDSCROLL                    equate(8)

SA_GUITHREADINFO                   group,type
cbSize                            long
flags                             long
hwndActive                        SA_HWND
hwndFocus                         SA_HWND
hwndCapture                       SA_HWND
hwndMenuOwner                     SA_HWND
hwndMoveSize                      SA_HWND
hwndCaret                         SA_HWND
rcCaret                           like(SA_RECT)
                                end

! Menu Equates

SA_MENUINFO                        group,type
cbSize                            long
fMask                             long
dwStyle                           long
cyMax                             long
hbrBack                           SA_HBRUSH
dwContextHelpID                   long
dwMenuData                        long
                                end

SA_MENUITEMINFO                    group,type
cbSize                            long
fMask                             long
fType                             long
fState                            long
wID                               long
hSubMenu                          SA_HMENU
hbmpChecked                       SA_HBITMAP
hbmpUnchecked                     SA_HBITMAP
dwItemData                        long
dwTypeData                        long
cch                               long
hbmpItem                          SA_HBITMAP
                                end

!#if(WINVER >= 0400h)
SA_MIIM_STATE                      equate(00000001h)
SA_MIIM_ID                         equate(00000002h)
SA_MIIM_SUBMENU                    equate(00000004h)
SA_MIIM_CHECKMARKS                 equate(00000008h)
SA_MIIM_TYPE                       equate(00000010h)
SA_MIIM_DATA                       equate(00000020h)
!#endif ! WINVER >= 0400h

!#if(WINVER >= 0500h)
SA_MIIM_STRING                     equate(00000040h)
SA_MIIM_BITMAP                     equate(00000080h)
SA_MIIM_FTYPE                      equate(00000100h)

SA_HBMMENU_CALLBACK                equate(-1)
SA_HBMMENU_SYSTEM                  equate(1)
SA_HBMMENU_MBAR_RESTORE            equate(2)
SA_HBMMENU_MBAR_MINIMIZE           equate(3)
SA_HBMMENU_MBAR_CLOSE              equate(5)
SA_HBMMENU_MBAR_CLOSE_D            equate(6)
SA_HBMMENU_MBAR_MINIMIZE_D         equate(7)
SA_HBMMENU_POPUP_CLOSE             equate(8)
SA_HBMMENU_POPUP_RESTORE           equate(9)
SA_HBMMENU_POPUP_MAXIMIZE          equate(10)
SA_HBMMENU_POPUP_MINIMIZE          equate(11)
!#endif ! WINVER >= 0500h

SA_SECURITY_ATTRIBUTES             group,type
nLength                           long
lpSecurityDescriptor              long
bInheritHandle                    bool
                                end

SA_CRITICAL_SECTION                group,type
DebugInfo                         long       !Ptr to _RTL_CRITICAL_SECTION_DEBUG

    !
    !  The following three fields control entering and exiting the critical
    !  section for the resource
    !

LockCount                         long
RecursionCount                    long
OwningThread                      SA_HANDLE        ! from the thread's ClientId->UniqueThread
LockSemaphore                     SA_HANDLE
Reserved                          long
                                end

SA_HEAP_NO_SERIALIZE               equate(00000001h)
SA_HEAP_GROWABLE                   equate(00000002h)
SA_HEAP_GENERATE_EXCEPTIONS        equate(00000004h)
SA_HEAP_ZERO_MEMORY                equate(00000008h)
SA_HEAP_REALLOC_IN_PLACE_ONLY      equate(00000010h)
SA_HEAP_TAIL_CHECKING_ENABLED      equate(00000020h)
SA_HEAP_FREE_CHECKING_ENABLED      equate(00000040h)
SA_HEAP_DISABLE_COALESCE_ON_FREE   equate(00000080h)
SA_HEAP_CREATE_ALIGN_16            equate(00010000h)
SA_HEAP_CREATE_ENABLE_TRACING      equate(00020000h)
SA_HEAP_MAXIMUM_TAG                equate(0FFFh)
SA_HEAP_PSEUDO_TAG_FLAG            equate(08000h)
SA_HEAP_TAG_SHIFT                  equate(18)

SA_DST_COMPLEX                     equate(00000h)
SA_DST_TEXT                        equate(00001h)
SA_DST_PREFIXTEXT                  equate(00002h)
SA_DST_ICON                        equate(00003h)
SA_DST_BITMAP                      equate(00004h)

! State type

SA_DSS_NORMAL                      equate(00000h)
SA_DSS_UNION                       equate(00010h)  ! Gray string appearance
SA_DSS_DISABLED                    equate(00020h)
SA_DSS_MONO                        equate(00080h)
SA_DSS_RIGHT                       equate(08000h)

SA_FLAG_ICC_FORCE_CONNECTION       equate(01h)

!*
!* Queue status flags for GetQueueStatus() and MsgWaitForMultipleObjects()
!*

SA_QS_KEY                          equate(0001h)
SA_QS_MOUSEMOVE                    equate(0002h)
SA_QS_MOUSEBUTTON                  equate(0004h)
SA_QS_POSTMESSAGE                  equate(0008h)
SA_QS_TIMER                        equate(0010h)
SA_QS_PAINT                        equate(0020h)
SA_QS_SENDMESSAGE                  equate(0040h)
SA_QS_HOTKEY                       equate(0080h)
SA_QS_ALLPOSTMESSAGE               equate(0100h)

SA_QS_MOUSE                        equate(SA_QS_MOUSEMOVE + SA_QS_MOUSEBUTTON)
SA_QS_INPUT                        equate(SA_QS_MOUSE + SA_QS_KEY)
SA_QS_ALLEVENTS                    equate(SA_QS_INPUT +       |
                                       SA_QS_POSTMESSAGE + |
                                       SA_QS_TIMER +       |
                                       SA_QS_PAINT +       |
                                       SA_QS_HOTKEY)
SA_QS_ALLINPUT                     equate(SA_QS_INPUT +       |
                                       SA_QS_POSTMESSAGE + |
                                       SA_QS_TIMER +       |
                                       SA_QS_PAINT +       |
                                       SA_QS_HOTKEY +      |
                                       SA_QS_SENDMESSAGE)

SA_STATUS_WAIT_0                   equate(000000000h)
SA_STATUS_ABANDONED_WAIT_0         equate(000000080h)
SA_STATUS_USER_APC                 equate(0000000C0h)
SA_STATUS_TIMEOUT                  equate(000000102h)
SA_STATUS_PENDING                  equate(000000103h)
SA_STATUS_SEGMENT_NOTIFICATION     equate(040000005h)
SA_STATUS_GUARD_PAGE_VIOLATION     equate(080000001h)
SA_STATUS_DATATYPE_MISALIGNMENT    equate(080000002h)
SA_STATUS_BREAKPOINT               equate(080000003h)
SA_STATUS_SINGLE_STEP              equate(080000004h)
SA_STATUS_ACCESS_VIOLATION         equate(0C0000005h)
SA_STATUS_IN_PAGE_ERROR            equate(0C0000006h)
SA_STATUS_INVALID_HANDLE           equate(0C0000008h)
SA_STATUS_NO_MEMORY                equate(0C0000017h)
SA_STATUS_ILLEGAL_INSTRUCTION      equate(0C000001Dh)
SA_STATUS_NONCONTINUABLE_EXCEPTION equate(0C0000025h)
SA_STATUS_INVALID_DISPOSITION      equate(0C0000026h)
SA_STATUS_ARRAY_BOUNDS_EXCEEDED    equate(0C000008Ch)
SA_STATUS_FLOAT_DENORMAL_OPERAND   equate(0C000008Dh)
SA_STATUS_FLOAT_DIVIDE_BY_ZERO     equate(0C000008Eh)
SA_STATUS_FLOAT_INEXACT_RESULT     equate(0C000008Fh)
SA_STATUS_FLOAT_INVALID_OPERATION  equate(0C0000090h)
SA_STATUS_FLOAT_OVERFLOW           equate(0C0000091h)
SA_STATUS_FLOAT_STACK_CHECK        equate(0C0000092h)
SA_STATUS_FLOAT_UNDERFLOW          equate(0C0000093h)
SA_STATUS_INTEGER_DIVIDE_BY_ZERO   equate(0C0000094h)
SA_STATUS_INTEGER_OVERFLOW         equate(0C0000095h)
SA_STATUS_PRIVILEGED_INSTRUCTION   equate(0C0000096h)
SA_STATUS_STACK_OVERFLOW           equate(0C00000FDh)
SA_STATUS_CONTROL_C_EXIT           equate(0C000013Ah)
SA_STATUS_FLOAT_MULTIPLE_FAULTS    equate(0C00002B4h)
SA_STATUS_FLOAT_MULTIPLE_TRAPS     equate(0C00002B5h)
SA_STATUS_ILLEGAL_VLM_REFERENCE    equate(0C00002C0h)

SA_MAXIMUM_WAIT_OBJECTS            equate(64)     ! Maximum number of wait objects

SA_WAIT_FAILED                     equate(0FFFFFFFFh)
SA_WAIT_OBJECT_0                   equate((SA_STATUS_WAIT_0) + 0)
SA_WAIT_ABANDONED                  equate((SA_STATUS_ABANDONED_WAIT_0) + 0)
SA_WAIT_ABANDONED_0                equate((SA_STATUS_ABANDONED_WAIT_0) + 0)
SA_WAIT_TIMEOUT                    equate(SA_STATUS_TIMEOUT)
SA_WAIT_IO_COMPLETION              equate(SA_STATUS_USER_APC)
SA_STILL_ACTIVE                    equate(SA_STATUS_PENDING)

SA_MSG                             group,type
hwnd                              SA_HWND
message                           long
wParam                            unsigned
lParam                            long
time                              long
pt                                like(SA_POINT)
                                end

!*
!* PeekMessage() Options
!*

SA_PM_NOREMOVE                     equate(00000h)
SA_PM_REMOVE                       equate(00001h)
SA_PM_NOYIELD                      equate(00002h)

!*
!*  Code Page Default Values.
!*

SA_CP_ACP                          equate(0)           ! default to ANSI code page
SA_CP_OEMCP                        equate(1)           ! default to OEM  code page
SA_CP_MACCP                        equate(2)           ! default to MAC  code page
SA_CP_THREAD_ACP                   equate(3)           ! current thread's ANSI code page
SA_CP_SYMBOL                       equate(42)          ! SYMBOL translations

SA_CP_UTF7                         equate(65000)       ! UTF-7 translation
SA_CP_UTF8                         equate(65001)       ! UTF-8 translation

!
!  String Length Maximums.
!

SA_MAX_LEADBYTES                   equate(12)         ! 5 ranges, 2 bytes ea., 0 term.
SA_MAX_DEFAULTCHAR                 equate(2)          ! single or double byte


!
!  MBCS and Unicode Translation Flags.
!

SA_MB_PRECOMPOSED                  equate(00000001h)  ! use precomposed chars
SA_MB_COMPOSITE                    equate(00000002h)  ! use composite chars
SA_MB_USEGLYPHCHARS                equate(00000004h)  ! use glyph chars, not ctrl chars
SA_MB_ERR_INVALID_CHARS            equate(00000008h)  ! error for invalid chars

SA_WC_COMPOSITECHECK               equate(00000200h)  ! convert composite to precomposed
SA_WC_DISCARDNS                    equate(00000010h)  ! discard non-spacing chars
SA_WC_SEPCHARS                     equate(00000020h)  ! generate separate chars
SA_WC_DEFAULTCHAR                  equate(00000040h)  ! replace w/ default char

SA_WC_NO_BEST_FIT_CHARS            equate(00000400h)  ! do not use best fit chars


SA_INFINITE                        equate(0FFFFFFFFh) ! Infinite timeout

!*
!* Predefined Clipboard Formats
!*

SA_CF_TEXT                         equate(1)
SA_CF_BITMAP                       equate(2)
SA_CF_METAFILEPICT                 equate(3)
SA_CF_SYLK                         equate(4)
SA_CF_DIF                          equate(5)
SA_CF_TIFF                         equate(6)
SA_CF_OEMTEXT                      equate(7)
SA_CF_DIB                          equate(8)
SA_CF_PALETTE                      equate(9)
SA_CF_PENDATA                      equate(10)
SA_CF_RIFF                         equate(11)
SA_CF_WAVE                         equate(12)
SA_CF_UNICODETEXT                  equate(13)
SA_CF_ENHMETAFILE                  equate(14)

!#if(WINVER >= 0x0400)

SA_CF_HDROP                        equate(15)
SA_CF_LOCALE                       equate(16)
SA_CF_MAX                          equate(17)

!#endif /* WINVER >= 0x0400 */

SA_CF_OWNERDISPLAY                 equate(0080h)
SA_CF_DSPTEXT                      equate(0081h)
SA_CF_DSPBITMAP                    equate(0082h)
SA_CF_DSPMETAFILEPICT              equate(0083h)
SA_CF_DSPENHMETAFILE               equate(008Eh)

!*
!* "Private" formats don't get GlobalFree()'d
!*

SA_CF_PRIVATEFIRST                 equate(0200h)
SA_CF_PRIVATELAST                  equate(02FFh)

!*
!* "GDIOBJ" formats do get DeleteObject()'d
!*

SA_CF_GDIOBJFIRST                  equate(0300h)
SA_CF_GDIOBJLAST                   equate(03FFh)

SA_TYPEFACENAME_LEN          EQUATE(32)

SA_ALIGN    ITEMIZE,PRE
SA_LEFT       EQUATE
SA_RIGHT      EQUATE
SA_TOP        EQUATE
SA_BOTTOM     EQUATE
SA_CENTER     EQUATE
         END

SA_DRAWTEXTPARAMS GROUP,TYPE
cbSize           LONG     !Structure size, in bytes
iTabLength       LONG     !in average character width units
iLeftMargin      LONG     !in average character width units
iRightMargin     LONG     !in average character width units
uiLengthDrawn    LONG     !Receives the number of characters processed by DrawTextEx
              END

SA_MappingModes ITEMIZE(1)
SA_MM_TEXT         EQUATE
SA_MM_LOMETRIC     EQUATE
SA_MM_HIMETRIC     EQUATE
SA_MM_LOENGLISH    EQUATE
SA_MM_HIENGLISH    EQUATE
SA_MM_TWIPS        EQUATE
SA_MM_ISOTROPIC    EQUATE
SA_MM_ANISOTROPIC  EQUATE
             END

!/*
! * EnableScrollBar() flags
! */
SA_ESB_ENABLE_BOTH     EQUATE(0000h)
SA_ESB_DISABLE_BOTH    EQUATE(0003h)

SA_ESB_DISABLE_LEFT    EQUATE(0001h)
SA_ESB_DISABLE_RIGHT   EQUATE(0002h)

SA_ESB_DISABLE_UP      EQUATE(0001h)
SA_ESB_DISABLE_DOWN    EQUATE(0002h)

SA_ESB_DISABLE_LTUP    EQUATE(SA_ESB_DISABLE_LEFT)
SA_ESB_DISABLE_RTDN    EQUATE(SA_ESB_DISABLE_RIGHT)

! Windows Messages
SA_WM_CTLCOLORSCROLLBAR            EQUATE(0137h)

SA_WM_SETTINGCHANGE                EQUATE(SA_WM_WININICHANGE)
SA_WM_ENTERSIZEMOVE                EQUATE(0231h)
SA_WM_EXITSIZEMOVE                 EQUATE(0232h)


! Mouse Wheel Support
!
! Key State Masks for Mouse Messages
SA_MK_XBUTTON1         EQUATE(0020h)
SA_MK_XBUTTON2         EQUATE(0040h)

! MOUSEKEYS dwFlags field
!
SA_MKF_MOUSEKEYSON     EQUATE(00000001h)
SA_MKF_AVAILABLE       EQUATE(00000002h)
SA_MKF_HOTKEYACTIVE    EQUATE(00000004h)
SA_MKF_CONFIRMHOTKEY   EQUATE(00000008h)
SA_MKF_HOTKEYSOUND     EQUATE(00000010h)
SA_MKF_INDICATOR       EQUATE(00000020h)
SA_MKF_MODIFIERS       EQUATE(00000040h)
SA_MKF_REPLACENUMBERS  EQUATE(00000080h)

!#if(_WIN32_WINNT >= EQUATE(0500)
SA_MKF_LEFTBUTTONSEL   EQUATE(10000000h)
SA_MKF_RIGHTBUTTONSEL  EQUATE(20000000h)
SA_MKF_LEFTBUTTONDOWN  EQUATE(01000000h)
SA_MKF_RIGHTBUTTONDOWN EQUATE(02000000h)
SA_MKF_MOUSEMODE       EQUATE(80000000h)

! Windows 98+
SA_WM_MOUSEWHEEL                   EQUATE(020Ah)



! CreateWindow flags
SA_CW_USEDEFAULT       EQUATE(080000000h)


! SystemParametersInfo()
SA_SPI_GETWHEELSCROLLLINES    EQUATE(0068h)
SA_SPI_SETWHEELSCROLLLINES    EQUATE(0069h)


!/* Device Parameters for GetDeviceCaps() */
SA_DRIVERVERSION EQUATE(0 )    !/* Device driver version                    */
SA_TECHNOLOGY    EQUATE(2 )    !/* Device classification                    */
SA_HORZSIZE      EQUATE(4 )    !/* Horizontal size in millimeters           */
SA_VERTSIZE      EQUATE(6 )    !/* Vertical size in millimeters             */
SA_HORZRES       EQUATE(8 )    !/* Horizontal width in pixels               */
SA_VERTRES       EQUATE(10)    !/* Vertical height in pixels                */
SA_BITSPIXEL     EQUATE(12)    !/* Number of bits per pixel                 */
SA_PLANES        EQUATE(14)    !/* Number of planes                         */
SA_NUMBRUSHES    EQUATE(16)    !/* Number of brushes the device has         */
SA_NUMPENS       EQUATE(18)    !/* Number of pens the device has            */
SA_NUMMARKERS    EQUATE(20)    !/* Number of markers the device has         */
SA_NUMFONTS      EQUATE(22)    !/* Number of fonts the device has           */
SA_NUMCOLORS     EQUATE(24)    !/* Number of colors the device supports     */
SA_PDEVICESIZE   EQUATE(26)    !/* Size required for device descriptor      */
SA_CURVECAPS     EQUATE(28)    !/* Curve capabilities                       */
SA_LINECAPS      EQUATE(30)    !/* Line capabilities                        */
SA_POLYGONALCAPS EQUATE(32)    !/* Polygonal capabilities                   */
SA_TEXTCAPS      EQUATE(34)    !/* Text capabilities                        */
SA_CLIPCAPS      EQUATE(36)    !/* Clipping capabilities                    */
SA_RASTERCAPS    EQUATE(38)    !/* Bitblt capabilities                      */
SA_ASPECTX       EQUATE(40)    !/* Length of the X leg                      */
SA_ASPECTY       EQUATE(42)    !/* Length of the Y leg                      */
SA_ASPECTXY      EQUATE(44)    !/* Length of the hypotenuse                 */

SA_LOGPIXELSX    EQUATE(88)    !/* Logical pixels/inch in X                 */
SA_LOGPIXELSY    EQUATE(90)    !/* Logical pixels/inch in Y                 */

SA_SIZEPALETTE  EQUATE(104)    !/* Number of entries in physical palette    */
SA_NUMRESERVED  EQUATE(106)    !/* Number of reserved entries in palette    */
SA_COLORRES     EQUATE(108)    !/* Actual color resolution                  */

SA_PHYSICALWIDTH   EQUATE(110)    ! /* Physical Width in device units           */
SA_PHYSICALHEIGHT  EQUATE(111)    ! /* Physical Height in device units          */
SA_PHYSICALOFFSETX EQUATE(112)    ! /* Physical Printable Area x margin         */
SA_PHYSICALOFFSETY EQUATE(113)    ! /* Physical Printable Area y margin         */
SA_SCALINGFACTORX  EQUATE(114)    ! /* Scaling factor x                         */
SA_SCALINGFACTORY  EQUATE(115)    ! /* Scaling factor y                         */





!
SA_OUT_PS_ONLY_PRECIS          EQUATE(10)


SA_DEFAULT_PITCH           EQUATE(0)
SA_FIXED_PITCH             EQUATE(1)
SA_VARIABLE_PITCH          EQUATE(2)
!#if(WINVER >= 0x0400)
SA_MONO_FONT               EQUATE(8)
!#endif /* WINVER >= 0x0400 */


!/* Graphics Modes */
SA_GM_COMPATIBLE       EQUATE(1)
SA_GM_ADVANCED         EQUATE(2)
SA_GM_LAST             EQUATE(2)

SA_LOGFONT GROUP,TYPE
lfHeight            LONG
lfWidth             LONG
lfEscapement        LONG
lfOrientation       LONG
lfWeight            LONG
lfItalic            BYTE
lfUnderline         BYTE
lfStrikeOut         BYTE
lfCharSet           BYTE
lfOutPrecision      BYTE
lfClipPrecision     BYTE
lfQuality           BYTE
lfPitchAndFamily    BYTE
lfFaceName          CSTRING(32)
         END

SA_ENUMLOGFONTEX GROUP,TYPE
elfLogFont      LIKE(SA_LOGFONT)
elfFullName     CSTRING(64) 
elfStyle        CSTRING(32)
elfScript       CSTRING(32)
              END

SA_ABC GROUP,TYPE
abcA    SIGNED
abcB    UNSIGNED
abcC    SIGNED
     END

SA_PANOSE GROUP,TYPE
bFamilyType         BYTE
bSerifStyle         BYTE
bWeight             BYTE
bProportion         BYTE
bContrast           BYTE
bStrokeVariation    BYTE 
bArmStyle           BYTE
bLetterform         BYTE
bMidline            BYTE
bXHeight            BYTE
  END

SA_TEXTMETRIC GROUP,TYPE
tmHeight            LONG
tmAscent            LONG
tmDescent           LONG
tmInternalLeading   LONG
tmExternalLeading   LONG
tmAveCharWidth      LONG 
tmMaxCharWidth      LONG 
tmWeight            LONG
tmOverhang          LONG
tmDigitizedAspectX  LONG
tmDigitizedAspectY  LONG
tmFirstChar         BYTE
tmLastChar          BYTE
tmDefaultChar       BYTE
tmBreakChar         BYTE
tmItalic            BYTE
tmUnderlined        BYTE
tmStruckOut         BYTE
tmPitchAndFamily    BYTE
tmCharSet           BYTE
        END

SA_NEWTEXTMETRIC GROUP(SA_TEXTMETRIC),TYPE
ntmFlags        ULONG
ntmSizeEM       UNSIGNED
ntmCellHeight   UNSIGNED
ntmAvgWidth     UNSIGNED
              END

SA_FONTSIGNATURE GROUP,TYPE
fsUsb ULONG,DIM(4)
fsCsb ULONG,DIM(2)
              END

SA_NEWTEXTMETRICEX GROUP,TYPE
ntmTm             LIKE(SA_NEWTEXTMETRIC)
ntmFontSig        LIKE(SA_FONTSIGNATURE)
                END

SA_OUTLINETEXTMETRIC GROUP,TYPE
otmSize                 UNSIGNED
otmTextMetrics          LIKE(SA_TEXTMETRIC)
otmFiller               BYTE
Filler1 BYTE,DIM(3)  !align structures, not part of structure
otmPanoseNumber         LIKE(SA_PANOSE)
Filler2 BYTE         !align structures, not part of structure
otmfsSelection          UNSIGNED
otmfsType               UNSIGNED
otmsCharSlopeRise       SIGNED
otmsCharSlopeRun        SIGNED
otmItalicAngle          SIGNED
otmEMSquare             UNSIGNED
otmAscent               SIGNED
otmDescent              SIGNED
otmLineGap              UNSIGNED
otmsCapEmHeight         UNSIGNED
otmsXHeight             UNSIGNED
otmrcFontBox            LIKE(SA_RECT)
otmMacAscent            SIGNED
otmMacDescent           SIGNED
otmMacLineGap           UNSIGNED
otmusMinimumPPEM        UNSIGNED
otmptSubscriptSize      LIKE(SA_POINT)
otmptSubscriptOffset    LIKE(SA_POINT)
otmptSuperscriptSize    LIKE(SA_POINT)
otmptSuperscriptOffset  LIKE(SA_POINT)
otmsStrikeoutSize       UNSIGNED
otmsStrikeoutPosition   SIGNED
otmsUnderscoreSize      SIGNED
otmsUnderscorePosition  SIGNED
otmpFamilyName          LONG      !Offset from start of struct to string
otmpFaceName            LONG
otmpStyleName           LONG
otmpFullName            LONG
   END

! Get Character Placement
SA_GCP_RESULTS GROUP,TYPE
lStructSize     ULONG  !DWORD
lpOutString     UNSIGNED !LPTSTR
lpOrder         UNSIGNED !UINT *
lpDx            UNSIGNED !int *
lpCaretPos      UNSIGNED !int *
lpClass         UNSIGNED !LPSTR
lpGlyphs        UNSIGNED !LPWSTR *
nGlyphs         UNSIGNED !UINT
nMaxFit         SIGNED !int
            END
SA_GCP_DBCS           EQUATE(0001h)
SA_GCP_REORDER        EQUATE(0002h)
SA_GCP_USEKERNING     EQUATE(0008h)
SA_GCP_GLYPHSHAPE     EQUATE(0010h)
SA_GCP_LIGATE         EQUATE(0020h)
!////SA_GCP_GLYPHINDEXING  EQUATE(0080h)
SA_GCP_DIACRITIC      EQUATE(0100h)
SA_GCP_KASHIDA        EQUATE(0400h)
SA_GCP_ERROR          EQUATE(8000h)
SA_FLI_MASK           EQUATE(103Bh)


SA_KERNINGPAIR GROUP,TYPE
wFirst        USHORT
wSecond       USHORT
iKernAmount   SIGNED
            END

!/* Text Alignment Options */
SA_TA_NOUPDATECP                EQUATE(0)
SA_TA_UPDATECP                  EQUATE(1)

SA_TA_LEFT                      EQUATE(0)
SA_TA_RIGHT                     EQUATE(2)
SA_TA_CENTER                    EQUATE(6)

SA_TA_TOP                       EQUATE(0)
SA_TA_BOTTOM                    EQUATE(8)
SA_TA_BASELINE                  EQUATE(24)
!#if (WINVER >= 0x0400)
!TA_RTLREADING                256
!TA_MASK       (TA_BASELINE+TA_CENTER+TA_UPDATECP+TA_RTLREADING)
!#else
!TA_MASK       (TA_BASELINE+TA_CENTER+TA_UPDATECP)
!#endif



SA_DOC_INFO_1  GROUP,TYPE
pDocName      LONG
pOutputFile   LONG
pDatatype     LONG
            END

SA_CCHDEVICENAME   EQUATE(32)
SA_CCHFORMNAME     EQUATE(32)

SA_DEVMODE       GROUP,TYPE
dmDeviceName    CSTRING(SA_CCHDEVICENAME)
dmSpecVersion   USHORT
dmDriverVersion USHORT
dmSize          USHORT
dmDriverExtra   USHORT
dmFields        ULONG
dmOrientation   SHORT
dmPaperSize     SHORT
dmPaperLength   SHORT
dmPaperWidth    SHORT
dmScale         SHORT
dmCopies        SHORT
dmDefaultSource SHORT
dmPrintQuality  SHORT
dmColor         SHORT
dmDuplex        SHORT
dmYResolution   SHORT
dmTTOption      SHORT
dmCollate       SHORT
dmFormName      CSTRING(SA_CCHFORMNAME)
dmLogPixels     USHORT
dmBitsPerPel    LONG
dmPelsWidth     LONG
dmPelsHeight    LONG
dmDisplayFlags  LONG
dmDisplayFrequency   LONG
dmICMMethod     LONG
dmICMIntent     LONG
dmMediaType     LONG
dmDitherType    LONG
dmReserved1     LONG
dmReserved2     LONG
dmBuffer        CSTRING(4096)       !Printer specific info
            END


SA_PRINTER_INFO_1 GROUP,TYPE
Flags           LONG
pDescription    LONG
pName           LONG
pComment        LONG
               END

SA_PRINTER_INFO_2 GROUP,TYPE
pServerName         LONG
pPrinterName        LONG
pShareName          LONG
pPortName           LONG
pDriverName         LONG
pComment            LONG
pLocation           LONG
pDevMode            LONG           !*DEVMODE
pSepFile            LONG
pPrintProcessor     LONG
pDatatype           LONG
pParameters         LONG
pSecurityDescriptor LONG(0)      !PSECURITY_DESCRIPTOR, Leave Null
Attributes          LONG
Priority            LONG
DefaultPriority     LONG
StartTime           LONG
UntilTime           LONG
Status              LONG
cJobs               LONG
AveragePPM          LONG
               END

SA_PRINTER_INFO_4 GROUP,TYPE !use with NT/2000
pPrinterName     LONG
pServerName      LONG
Attributes       LONG
               END

SA_PRINTER_INFO_5 GROUP,TYPE !Use with 95/98
pPrinterName             LONG
pPortName                LONG
Attributes               LONG
DeviceNotSelectedTimeout LONG
TransmissionRetryTimeout LONG
               END

SA_DOCINFO        GROUP,Type
cbSize           SIGNED
lpszDocName      Long
lpszOutput       Long
lpszDatatype     Long
fwtype           Ulong
cdocname         Cstring(file:MaxFilepath)
            end



SA_SPI_SETDRAGFULLWINDOWS      EQUATE(0025h)
SA_SPI_GETDRAGFULLWINDOWS      EQUATE(0026h)

SA_BGRQUAD   Group
bgrRed         Byte
bgrGreen       Byte
bgrBlue        Byte
bgrReserved    Byte
          End

SA_PALETTEENTRY Group,Type
peRed           BYTE
peGreen         BYTE
peBlue          BYTE
peFlags         BYTE
             End

SA_LOGPALETTE Group,Type
palVersion      Ushort
palNumEntries   Ushort
PalPalEntry     Like(SA_PALETTEENTRY)  !PALETTEENTRY palPalEntry[1];
           End

SA_LOGPALETTE256   Group,Type
palVersion      Ushort
palNumEntries   Ushort
palPalEntry     Like(SA_PALETTEENTRY),Dim(256)  !PALETTEENTRY palPalEntry[1];
              End

!/* StretchBlt() Modes */
SA_BLACKONWHITE        Equate(1)
SA_WHITEONBLACK        Equate(2)
SA_COLORONCOLOR        Equate(3)
SA_HALFTONE            Equate(4)
SA_MAXSTRETCHBLTMODE   Equate(4)

!/* New StretchBlt() Modes */
SA_STRETCH_ANDSCANS    Equate(SA_BLACKONWHITE)
SA_STRETCH_ORSCANS     Equate(SA_WHITEONBLACK )
SA_STRETCH_DELETESCANS Equate(SA_COLORONCOLOR)
SA_STRETCH_HALFTONE    Equate(SA_HALFTONE)


SA_POINTS       Group,Type
x                 Short
y                 Short
                End



SA_CDERR_DIALOGFAILURE   Equate(0FFFFh)

SA_CDERR_GENERALCODES     Equate(0000h)
SA_CDERR_STRUCTSIZE       Equate(0001h)
SA_CDERR_INITIALIZATION   Equate(0002h)
SA_CDERR_NOTEMPLATE       Equate(0003h)
SA_CDERR_NOHINSTANCE      Equate(0004h)
SA_CDERR_LOADSTRFAILURE   Equate(0005h)
SA_CDERR_FINDRESFAILURE   Equate(0006h)
SA_CDERR_LOADRESFAILURE   Equate(0007h)
SA_CDERR_LOCKRESFAILURE   Equate(0008h)
SA_CDERR_MEMALLOCFAILURE  Equate(0009h)
SA_CDERR_MEMLOCKFAILURE   Equate(000Ah)
SA_CDERR_NOHOOK           Equate(000Bh)
SA_CDERR_REGISTERMSGFAIL  Equate(000Ch)

SA_PDERR_PRINTERCODES     Equate(1000h)
SA_PDERR_SETUPFAILURE     Equate(1001h)
SA_PDERR_PARSEFAILURE     Equate(1002h)
SA_PDERR_RETDEFFAILURE    Equate(1003h)
SA_PDERR_LOADDRVFAILURE   Equate(1004h)
SA_PDERR_GETDEVMODEFAIL   Equate(1005h)
SA_PDERR_INITFAILURE      Equate(1006h)
SA_PDERR_NODEVICES        Equate(1007h)
SA_PDERR_NODEFAULTPRN     Equate(1008h)
SA_PDERR_DNDMMISMATCH     Equate(1009h)
SA_PDERR_CREATEICFAILURE  Equate(100Ah)
SA_PDERR_PRINTERNOTFOUND  Equate(100Bh)
SA_PDERR_DEFAULTDIFFERENT Equate(100Ch)

SA_CFERR_CHOOSEFONTCODES  Equate(2000h)
SA_CFERR_NOFONTS          Equate(2001h)
SA_CFERR_MAXLESSTHANMIN   Equate(2002h)

SA_FNERR_FILENAMECODES    Equate(3000h)
SA_FNERR_SUBCLASSFAILURE  Equate(3001h)
SA_FNERR_INVALIDFILENAME  Equate(3002h)
SA_FNERR_BUFFERTOOSMALL   Equate(3003h)

SA_FRERR_FINDREPLACECODES Equate(4000h)
SA_FRERR_BUFFERLENGTHZERO Equate(4001h)

SA_CCERR_CHOOSECOLORCODES Equate(5000h)


SA_OSVERSIONINFO     GROUP,TYPE
dwOSVersionInfoSize  ULONG
dwMajorVersion       ULONG
dwMinorVersion       ULONG
dwBuildNumber        ULONG
dwPlatformId         ULONG
szCSDVersion         CSTRING(128)
                   END

SA_OSVERSIONINFOEX   GROUP(SA_OSVERSIONINFO),TYPE
wServicePackMajor    USHORT
wServicePackMinor    USHORT
wSuiteMask           USHORT
wProductType         BYTE
wReserved            BYTE
                 END

SA_VER_PLATFORM_WIN32s         EQUATE(0)
SA_VER_PLATFORM_WIN32_WINDOWS  EQUATE(1)
SA_VER_PLATFORM_WIN32_NT       EQUATE(2)

! RtlVerifyVersionInfo() type mask bits
SA_VER_MINORVERSION                    EQUATE(0000001h)
SA_VER_MAJORVERSION                    EQUATE(0000002h)
SA_VER_BUILDNUMBER                     EQUATE(0000004h)
SA_VER_PLATFORMID                      EQUATE(0000008h)
SA_VER_SERVICEPACKMINOR                EQUATE(0000010h)
SA_VER_SERVICEPACKMAJOR                EQUATE(0000020h)
SA_VER_SUITENAME                       EQUATE(0000040h)
SA_VER_PRODUCT_TYPE                    EQUATE(0000080h)

! RtlVerifyVersionInfo() os product type values
SA_VER_NT_WORKSTATION                  EQUATE(0000001h)
SA_VER_NT_DOMAIN_CONTROLLER            EQUATE(0000002h)
SA_VER_NT_SERVER                       EQUATE(0000003h)

SA_VER_SUITE_SMALLBUSINESS             EQUATE(00000001h)
SA_VER_SUITE_ENTERPRISE                EQUATE(00000002h)
SA_VER_SUITE_BACKOFFICE                EQUATE(00000004h)
SA_VER_SUITE_COMMUNICATIONS            EQUATE(00000008h)
SA_VER_SUITE_TERMINAL                  EQUATE(00000010h)
SA_VER_SUITE_SMALLBUSINESS_RESTRICTED  EQUATE(00000020h)
SA_VER_SUITE_EMBEDDEDNT                EQUATE(00000040h)
SA_VER_SUITE_DATACENTER                EQUATE(00000080h)
SA_VER_SUITE_SINGLEUSERTS              EQUATE(00000100h)
SA_VER_SUITE_PERSONAL                  EQUATE(00000200h)

SA_VER_SERVER_NT                       EQUATE(80000000h)
SA_VER_WORKSTATION_NT                  EQUATE(40000000h)
SA_VER_SUITE_BLADE                     EQUATE(00000400h)
SA_VER_SUITE_EMBEDDED_RESTRICTED       EQUATE(00000800h)

SA_HKEY_LOCAL_MACHINE          EQUATE(80000002h)
SA_KEY_QUERY_VALUE             EQUATE(00000001h)
SA_STANDARD_RIGHTS_READ        EQUATE(00020000h)


SA_PROCESSOR_INTEL_386     EQUATE(386)
SA_PROCESSOR_INTEL_486     EQUATE(486)
SA_PROCESSOR_INTEL_PENTIUM EQUATE(586)
SA_PROCESSOR_INTEL_IA64    EQUATE(2200)
SA_PROCESSOR_AMD_X8664     EQUATE(8664)

SA_PROCESSOR_ARCHITECTURE_INTEL            EQUATE(0)
SA_PROCESSOR_ARCHITECTURE_MIPS             EQUATE(1)
SA_PROCESSOR_ARCHITECTURE_ALPHA            EQUATE(2)
SA_PROCESSOR_ARCHITECTURE_PPC              EQUATE(3)
SA_PROCESSOR_ARCHITECTURE_SHX              EQUATE(4)
SA_PROCESSOR_ARCHITECTURE_ARM              EQUATE(5)
SA_PROCESSOR_ARCHITECTURE_IA64             EQUATE(6)
SA_PROCESSOR_ARCHITECTURE_ALPHA64          EQUATE(7)
SA_PROCESSOR_ARCHITECTURE_MSIL             EQUATE(8)
SA_PROCESSOR_ARCHITECTURE_AMD64            EQUATE(9)
SA_PROCESSOR_ARCHITECTURE_IA32_ON_WIN64    EQUATE(10)

SA_PROCESSOR_ARCHITECTURE_UNKNOWN EQUATE(0FFFFh)

SA_SYSTEM_INFO Group,Type
dwOemId ULong
  Group,Over(dwOemId)
wProcessorArchitecture Ushort
wReserved Ushort
  End
dwPageSize Ulong
lpMinimumApplicationAddress Long
lpMaximumApplicationAddress Long
dwActiveProcessorMask Long
dwNumberOfProcessors Long
dwProcessorType Long
dwAllocationGranularity Long
wProcessorLevel Ushort
wProcessorRevision Ushort
        End

SA_CSIDL_DESKTOP                   EQUATE(0000h)        ! <desktop>
SA_CSIDL_INTERNET                  EQUATE(0001h)        ! Internet Explorer (icon on desktop)
SA_CSIDL_PROGRAMS                  EQUATE(0002h)        ! Start Menu\Programs
SA_CSIDL_CONTROLS                  EQUATE(0003h)        ! My Computer\Control Panel
SA_CSIDL_PRINTERS                  EQUATE(0004h)        ! My Computer\Printers
SA_CSIDL_PERSONAL                  EQUATE(0005h)        ! My Documents
SA_CSIDL_FAVORITES                 EQUATE(0006h)        ! <user name>\Favorites
SA_CSIDL_STARTUP                   EQUATE(0007h)        ! Start Menu\Programs\Startup
SA_CSIDL_RECENT                    EQUATE(0008h)        ! <user name>\Recent
SA_CSIDL_SENDTO                    EQUATE(0009h)        ! <user name>\SendTo
SA_CSIDL_BITBUCKET                 EQUATE(000ah)        ! <desktop>\Recycle Bin
SA_CSIDL_STARTMENU                 EQUATE(000bh)        ! <user name>\Start Menu
SA_CSIDL_DESKTOPDIRECTORY          EQUATE(0010h)        ! <user name>\Desktop
SA_CSIDL_DRIVES                    EQUATE(0011h)        ! My Computer
SA_CSIDL_NETWORK                   EQUATE(0012h)        ! Network Neighborhood
SA_CSIDL_NETHOOD                   EQUATE(0013h)        ! <user name>\nethood
SA_CSIDL_FONTS                     EQUATE(0014h)        ! windows\fonts
SA_CSIDL_TEMPLATES                 EQUATE(0015h)
SA_CSIDL_COMMON_STARTMENU          EQUATE(0016h)        ! All Users\Start Menu
SA_CSIDL_COMMON_PROGRAMS           EQUATE(0017h)        ! All Users\Programs
SA_CSIDL_COMMON_STARTUP            EQUATE(0018h)        ! All Users\Startup
SA_CSIDL_COMMON_DESKTOPDIRECTORY   EQUATE(0019h)        ! All Users\Desktop
SA_CSIDL_APPDATA                   EQUATE(001ah)        ! <user name>\Application Data
SA_CSIDL_PRINTHOOD                 EQUATE(001bh)        ! <user name>\PrintHood
SA_CSIDL_LOCAL_APPDATA             EQUATE(001ch)        ! <user name>\Local Settings\Applicaiton Data (non roaming)
SA_CSIDL_ALTSTARTUP                EQUATE(001dh)        ! non localized startup
SA_CSIDL_COMMON_ALTSTARTUP         EQUATE(001eh)        ! non localized common startup
SA_CSIDL_COMMON_FAVORITES          EQUATE(001fh)
SA_CSIDL_INTERNET_CACHE            EQUATE(0020h)
SA_CSIDL_COOKIES                   EQUATE(0021h)
SA_CSIDL_HISTORY                   EQUATE(0022h)
SA_CSIDL_COMMON_APPDATA            EQUATE(0023h)        ! All Users\Application Data
SA_CSIDL_WINDOWS                   EQUATE(0024h)        ! GetWindowsDirectory()
SA_CSIDL_SYSTEM                    EQUATE(0025h)        ! GetSystemDirectory()
SA_CSIDL_PROGRAM_FILES             EQUATE(0026h)        ! C:\Program Files
SA_CSIDL_MYPICTURES                EQUATE(0027h)        ! C:\Program Files\My Pictures
SA_CSIDL_PROFILE                   EQUATE(0028h)        ! USERPROFILE
SA_CSIDL_SYSTEMX86                 EQUATE(0029h)        ! x86 system directory on RISC
SA_CSIDL_PROGRAM_FILESX86          EQUATE(002ah)        ! x86 C:\Program Files on RISC
SA_CSIDL_PROGRAM_FILES_COMMON      EQUATE(002bh)        ! C:\Program Files\Common
SA_CSIDL_PROGRAM_FILES_COMMONX86   EQUATE(002ch)        ! x86 Program Files\Common on RISC
SA_CSIDL_COMMON_TEMPLATES          EQUATE(002dh)        ! All Users\Templates
SA_CSIDL_COMMON_DOCUMENTS          EQUATE(002eh)        ! All Users\Documents
SA_CSIDL_COMMON_ADMINTOOLS         EQUATE(002fh)        ! All Users\Start Menu\Programs\Administrative Tools
SA_CSIDL_ADMINTOOLS                EQUATE(0030h)        ! <user name>\Start Menu\Programs\Administrative Tools
SA_CSIDL_CONNECTIONS               EQUATE(0031h)        ! Network and Dial-up Connections

SA_CSIDL_FLAG_CREATE               EQUATE(8000h)        ! combine with CSIDL_ value to force folder creation in SHGetFolderPath()
SA_CSIDL_FLAG_DONT_VERIFY          EQUATE(4000h)        ! combine with CSIDL_ value to return an unverified folder path
SA_CSIDL_FLAG_MASK                EQUATE(0FF00h)        ! mask for all possible flag values

SA_S_OK            EQUATE(0)
SA_S_FALSE         EQUATE(1)

SA_SEE_MASK_CLASSNAME       Equate(00000001h)
SA_SEE_MASK_CLASSKEY        Equate(00000003h)
SA_SEE_MASK_IDLIST          Equate(00000004h)
SA_SEE_MASK_INVOKEIDLIST    Equate(0000000ch)
SA_SEE_MASK_ICON            Equate(00000010h)
SA_SEE_MASK_HOTKEY          Equate(00000020h)
SA_SEE_MASK_NOCLOSEPROCESS  Equate(00000040h)
SA_SEE_MASK_CONNECTNETDRV   Equate(00000080h)
SA_SEE_MASK_FLAG_DDEWAIT    Equate(00000100h)
SA_SEE_MASK_DOENVSUBST      Equate(00000200h)
SA_SEE_MASK_FLAG_NO_UI      Equate(00000400h)
SA_SEE_MASK_UNICODE         Equate(00004000h)
SA_SEE_MASK_NO_CONSOLE      Equate(00008000h)
SA_SEE_MASK_ASYNCOK         Equate(00100000h)
SA_SEE_MASK_HMONITOR        Equate(00200000h)

!== Mouse Tracking ==
SA__TRACKMOUSEEVENT Group,Type
cbSize  Long
dwFlags Long
hwndTrack SA_HWND
dwHoverTime Long
                   End

SA_TME_HOVER        Equate(1h)
SA_TME_LEAVE        Equate(2h)
SA_TME_NONCLIENT    Equate(10h)
SA_TME_QUERY        Equate(040000000h)
SA_TME_CANCEL       Equate(080000000h)
SA_HOVER_DEFAULT    Equate(0FFFFFFFFh)

SA_XFORM Group,Type
eM11 sReal
eM12 sReal
eM21 sReal
eM22 sReal
eDx sReal
eDy sReal
                 End            
                 
SA_OVERLAPPED    Group,Type
Internal         UNSIGNED
InternalHigh     UNSIGNED
OffsetGroup      Group
Offset             UNSIGNED
OffsetHigh         UNSIGNED
                 End
Pointer          UNSIGNED,Over(OffsetGroup)
hEvent           UNSIGNED
               End

SA_METAFILEPICT   Group,Type
mm                  LONG
xExt                LONG
yExt                LONG
hMF                 SA_HMETA
                  End

SA_RECT16         Group,Type
Left                SHORT
Top                 SHORT
Right               SHORT
Bottom              SHORT
                  END

! Aldus Placeable Metafile header (Clarion Creates this style of WMF)
SA_APMFILEHEADER  Group,Type     !http://support.microsoft.com/kb/66949 must be 22 bytes
key                 Long
hMF                 SHORT
bbox                Like(SA_RECT16)
inch                SHORT
reserved            Long
checksum            SHORT
                  End

SA_METAHEADER   Group,Type
mtType              UShort
mtHeaderSize        UShort
mtVersion           UShort
mtSize              ULong   !size in words
mtNoObjects         UShort
mtMaxRecord         ULong
mtNoParameters      UShort
                End

SA_ENHMETAHEADER   Group,Type
iType               Long
nSize               Long
rclBounds           Like(SA_RECTL)
rclFrame            Like(SA_RECTL)
dSignature          Long
nVersion            Long
nBytes              Long
nRecords            Long
nHandles            Ushort
sReserved           Ushort
nDescription        Long
offDescription      Long
nPalEntries         Long
szlDevice           Like(SA_SIZEL)
szlMillimeters      Like(SA_SIZEL)
cbPixelFormat       Long
offPixelFormat      Long
bOpenGl             Long
szlmicrometers      Like(SA_RECTL)
      End


SA_MONITOR_DEFAULTTONULL       Equate(00000000h)
SA_MONITOR_DEFAULTTOPRIMARY    Equate(00000001h)
SA_MONITOR_DEFAULTTONEAREST    Equate(00000002h)

SA_MONITORINFOF_PRIMARY        Equate(00000001h)

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

SA_SHELLEXECUTEINFO Group,Type
cbSize                  UNSIGNED
fMask                   UNSIGNED
hwnd                    SA_HWND
pVerb                   Long
lpFile                  Long
lpParameters            Long
lpDirectory             Long
nShow                   Long
hInstApp                SA_HINSTANCE
lpIDList                Long
lpClass                 Long
hkeyClass               SA_HANDLE
dwHotKey                Long
hIconOrMonitor          SA_HANDLE
hProcess                SA_HANDLE
                     End

               
! _EndOfSaWapiDefs_

!==================================================================================
!==================================================================================
 SECTION('Prototypes')


  MODULE('Win32Api')
    SA_MessageBox(SA_HWND hWnd, long lpText, long lpCaption, long uType),long,pascal,name('MessageBoxW'),proc,Name('MessageBox')

    SA_IsWindowEnabled(SA_HWND hWnd),bool,pascal,Name('IsWindowEnabled')

    SA_GetFileAttributes(*CString szFileName),long,pascal,raw,name('GetFileAttributesA')
    SA_CreateDirectory(*CString szPathName, *SA_SECURITY_ATTRIBUTES SecurityAttributes),bool,pascal,raw,name('CreateDirectoryA')

    SA_DefWindowProc(SA_HWND hWnd, unsigned nMsg, unsigned wParam, |
                  long lParam),long,pascal,name('DefWindowProcA')
    SA_SetWindowLong(SA_HWND hWnd, signed nIndex, long dwNewLong),long,pascal,proc,name('SetWindowLongA')
    SA_GetWindowLong(SA_HWND hWnd, signed nIndex),long,pascal,name('GetWindowLongA')
    SA_CallWindowProc(long lpPrevWndProc, SA_HWND hwnd, unsigned nMsg, unsigned wParam, |
                   long lParam),long,pascal,name('CallWindowProcA')

    SA_RegisterClass(*SA_WNDCLASS lpWndClass),SA_ATOM,pascal,raw,name('RegisterClassA')
    SA_RegisterClassEx(*SA_WNDCLASSEX lpWndClassEx),SA_ATOM,pascal,raw,name('RegisterClassExA')
    SA_UnRegisterClass(*CString lpClassName, SA_HINSTANCE hInstance),bool,raw,pascal,name('UnRegisterClassA')
    SA_GetClassInfo(SA_HINSTANCE hInst, *CString lpClassName, *SA_WNDCLASS |
                 lpWndClass),bool,raw,pascal,name('GetClassInfoA')

    SA_LoadCursor(SA_HINSTANCE hInst, long lpCursorName),SA_HCURSOR,pascal, |
               name('LoadCursorA')
    SA_DestroyIcon(SA_HICON hIcon),Bool,Pascal, Name('DestroyIcon')
    SA_ShowCursor(bool bShow),long,pascal,Name('ShowCursor')
    SA_SetCursor(long hCursor),long,name('SetCursor')
    SA_SetCursorPos(signed x, signed y),bool,pascal,proc,Name('SetCursorPos')
    SA_GetCursorPos(*SA_POINT),bool,raw,pascal,proc,Name('GetCursorPos')

    SA_CreateMenu(),SA_HMENU,pascal,Name('CreateMenu')
    SA_DestroyMenu(SA_HMENU hMenu),bool,pascal,proc,Name('DestroyMenu')

    SA_CreateWindowEx(unsigned dwExStyle, *CString lpClassName, <*CString lpWindowName>, |
                   unsigned dwStyle, signed X, signed Y, signed nWidth, signed nHeight, |
                   SA_HWND hWndParent, SA_HMENU hMenu, SA_HINSTANCE hInstance, long lpParam), |
                   SA_HWND,raw,pascal,name('CreateWindowExA')
    SA_UpdateWindow(SA_HWND hWnd),bool,pascal,proc,Name('UpdateWindow')
    SA_RedrawWindow(SA_HWND hWnd, long lprcUpdate, SA_HRGN hrgnUpdate, unsigned flags),bool,raw,pascal,Name('RedrawWindow')
    SA_BeginPaint(SA_HWND hWnd, *SA_PAINTSTRUCT lpPaint),SA_HDC,raw,pascal,Name('BeginPaint')
    SA_EndPaint(SA_HWND hWnd, *SA_PAINTSTRUCT lpPaint),bool,raw,pascal,proc,Name('EndPaint')
    SA_GetUpdateRect(SA_HWND hWnd, *SA_RECT lpRect, bool bErase),bool,pascal,raw,Name('GetUpdateRect')
    SA_GetUpdateRgn(SA_HWND hWnd, SA_HRGN hRgn, BOOL bErase),Long,Pascal,Name('GetUpdateRgn')
    SA_GetStockObject(signed),SA_HGDIOBJ,pascal,Name('GetStockObject')
    SA_DestroyWindow(SA_HWND hWnd),bool,pascal,proc,Name('DestroyWindow')
    SA_PostMessage(SA_HWND hWnd, unsigned nMsg, unsigned wParam, long lParam), |
                bool,pascal,proc,name('PostMessageA')
    SA_SendMessage(SA_HWND hWnd, long nMsg, long wParam, long lParam), |
                BOOL,pascal,proc,name('SendMessageA')

    SA_SendMessageTimeout(SA_HWND hWnd, Long uMsg, UNSIGNED wParam, Long lParam, UNSIGNED fuFlags, UNSIGNED uTimeout, *Long lpdwResult),Long,Pascal,Raw Name('SendMessageTimeoutA')
    SA_PeekMessage(long lpMsg, SA_HWND hWnd, long wMsgFilterMin, long wMsgFilterMax, long wRemoveMsg),bool,pascal,proc,name('PeekMessageA')
    SA_SetWindowPos(SA_HWND hWnd, SA_HWND hWndInsertAfter, unsigned x, unsigned y, |
                 unsigned cx, unsigned cy, unsigned uFlags),bool,pascal,proc,Name('SetWindowPos')
    SA_MoveWindow(SA_HWND hWnd, unsigned x, unsigned y, unsigned cx, unsigned cy, bool bRedraw),bool,pascal,proc,Name('MoveWindow')
    SA_SetWindowText(SA_HWND hWnd, *CString lpString),bool,raw,pascal,name('SetWindowTextA')
    SA_ShowWindow(SA_HWND hWnd, signed nCmdShow),bool,pascal,proc,Name('ShowWindow')
    SA_SetForegroundWindow(SA_HWND hwnd),bool,pascal,proc,Name('SetForegroundWindow')
    SA_GetLastActivePopup(SA_HWND hwnd),SA_HWND,pascal,Name('GetLastActivePopup')
    SA_IsIconic(SA_HWND hwnd),bool,pascal,Name('IsIconic')
    SA_ScreenToClient(SA_HWND hWnd, *SA_POINT lpPOINT),bool,raw,pascal,proc,Name('ScreenToClient')
    SA_ClientToScreen(SA_HWND hWnd, *SA_POINT lpPOINT),bool,raw,pascal,proc,Name('ClientToScreen')
    SA_GetWindowRect(SA_HWND hWnd, *SA_RECT lpRect),bool,raw,pascal,proc,Name('GetWindowRect')
    SA_GetClientRect(SA_HWND hWnd, *SA_RECT lpRect),bool,raw,pascal,proc,Name('GetClientRect')
    SA_DrawEdge(SA_HDC hdc, *SA_RECT qrc, unsigned edge, unsigned grfFlags), |
             bool,raw,pascal,proc,Name('DrawEdge')
    SA_DrawText(SA_HDC hdc, *CString lpszString, unsigned nCount, *SA_RECT lpRect, |
             unsigned uFormat),signed,raw,pascal,proc,name('DrawTextA')
    SA_DrawState(SA_HDC hdc, SA_HBRUSH hbr, long lpOutputFunc, long lData, long wData, long x, |
              long y, long cx, long cy, long fuFlags),bool,pascal,name('DrawStateA'),proc
    SA_Rectangle(SA_HDC hdc, signed nLeftRect, signed nTopRect, signed nRightRect, |
              signed nBottomRect),bool,pascal,proc,Name('Rectangle')
    SA_EqualRect(*SA_RECT, *SA_RECT),bool,raw,pascal,Name('EqualRect')
    SA_RoundRect(SA_HDC hdc, signed nLeftRect, signed nTopRect, signed nRightRect, |
              signed nBottomRect, signed nWidth, signed nHeight),bool,pascal,Name('RoundRect')
    SA_CopyRect(*SA_RECT rcDst, *SA_RECT rcSrc),bool,raw,pascal,proc,Name('CopyRect')
    SA_CopyRectRaw(long rcDst, long rcSrc),bool,pascal,proc,name('CopyRect')
    SA_SetPolyFillMode(SA_HDC hdc, long iPolyFillMode),long,pascal,proc,Name('SetPolyFillMode')
    SA_Polygon(SA_HDC hdc, long lpPt, long nCount),bool,proc,pascal,Name('Polygon')
    SA_SetBkMode(SA_HDC hdc, signed iBkMode),signed,pascal,proc,Name('SetBkMode')
    SA_SetBkColor(SA_HDC hdc, SA_COLORREF crColor),bool,pascal,proc,Name('SetBkColor')
    SA_SetTextColor(SA_HDC hdc, SA_COLORREF crColor),SA_COLORREF,pascal,proc,Name('SetTextColor')
    SA_GetTextExtentPoint(SA_HDC hdc, *cstring lpString, long cbString, *SA_SIZE lpSize),bool,raw,pascal,proc,name('GetTextExtentPointA')
    SA_GetTextExtentExPoint(SA_HDC hdc, *cstring szStr, long cchString, long nMaxExtent, |
                         *long nFit, <*long Dx>, *SA_SIZE lpSize),bool,raw,pascal,name('GetTextExtentExPointA')


EnumFontFamExProc PROCEDURE(LONG lpElf,    | pointer to ENUMLOGFONTEX or ENUMLOGFONTEXDV
                            LONG lpNtm,    | pointer to TEXTMETRIC or NEWTEXTMETRICEX or ENUMTEXTMETRIC
                            LONG FontType, | font type bitmap
                            LONG lParam    | optional user defined data
                            ),SIGNED,PASCAL,TYPE

    SA_EnumFontFamiliesEx(UNSIGNED hdc,             | handle to DC
                         *SA_LOGFONT lpLogfont,       | font information
                         *EnumFontFamExProc lpEnumFontFamExProc, | callback function
                         LONG lParam,              | additional data to pass to callback
                         LONG dwFlags=0            | not used; must be 0
                        ),SIGNED,PROC,PASCAL,RAW,NAME('EnumFontFamiliesExA')

    SA_CreateFont(signed nHeight, signed nWidth, signed nEscapement, |
               signed nOrientation, signed fnWeight, unsigned fdwItalic, |
               unsigned fdwUnderline, unsigned fdwStrikeOut, unsigned |
               fdwCharSet, unsigned fdwOutputPrecision, unsigned |
               fdwClipPrecision, unsigned fdwQuality, unsigned |
               fdwPitchAndFamily, *CString lpszFace),SA_HFONT,pascal,raw, |
               name('CreateFontA')
    SA_ExtTextOut(SA_HDC hdc, long X, long Y, long fuOptions, *SA_RECT rc, <*cstring lpString>, long cbCount, long lpDx),bool,raw,pascal,name('ExtTextOutA'),proc

    SA_DeleteObject(SA_HGDIOBJ hgdiobj),bool,pascal,proc,Name('DeleteObject')
    SA_SelectObject(SA_HDC hdc, SA_HGDIOBJ hgdiobj),SA_HGDIOBJ,pascal,proc,Name('SelectObject')
    SA_CreateCompatibleDC(SA_HDC hdc),SA_HDC,pascal,Name('CreateCompatibleDC')

    SA_LoadBitmap(SA_HINSTANCE hInst, long lpBitmapName),SA_HBITMAP,pascal, |
               name('LoadBitmapA')
    SA_LoadImage(SA_HINSTANCE hinst, long lpszName, unsigned uType, signed cxDesired, |
              signed cyDesired, unsigned fuLoad),SA_HANDLE,pascal,raw,name('LoadImageA')
    SA_CreateBitmap(long nWidth, long nHeight, long cPlanes, long cBitsPerPel, long lpvBits),long,pascal,Name('CreateBitmap')

    SA_CreateCompatibleBitmap(SA_HDC hdc, signed nWiidth, signed nHeight),SA_HBITMAP,pascal,Name('CreateCompatibleBitmap')
    SA_CreateDIBitmap(SA_HDC hdc, *SA_BITMAPINFOHEADER bmih, long fdwInit, long lpbInit, *SA_BITMAPINFO bmi, long fuUsage),SA_HBITMAP,raw,pascal,Name('CreateDIBitmap')

    SA_GetBitmapDimensionEx(SA_HBITMAP hBitmap, *SA_SIZE lpDimension),bool,raw,pascal,Name('GetBitmapDimensionsEx')
    SA_BitBlt(SA_HDC hdcDest, signed nXDest, signed nYDest, signed nWidth, signed |
           nHeight, SA_HDC hdcSrc, signed nXSrc, signed nYSrc, long dwRop),bool,pascal,proc,Name('BitBlt')
    SA_PatBlt(SA_HDC hdc,      | handle to DC
              Long nXLeft,   | x-coord of upper-left rectangle corner
              Long nYLeft,   | y-coord of upper-left rectangle corner
              Long nWidth,   | width of rectangle
              Long nHeight,  | height of rectangle
              ULong dwRop   | raster operation code
             ),BOOL,Pascal,Raw,Name('PatBlt')

    SA_GetDIBits(SA_HDC hdc, SA_HBITMAP hbmp, unsigned uStartScan, unsigned cScanLines, |
              long lpvBits, long lpbi, unsigned uUsage),signed,raw,pascal,proc,Name('GetDIBits')
    SA_GetDIBits(SA_HDC hdc, SA_HBITMAP hbmp, unsigned uStartScan, unsigned cScanLines, |
              long lpvBits, *SA_BITMAPINFO lpbi, unsigned uUsage),signed,raw,pascal,proc,Name('GetDIBits')
    SA_GetBitmapBits(SA_HBITMAP hbmp, long cbBuffer, long lpvBits),long,pascal,proc,Name('GetBitmapBits')

    SA_GetDeskTopWindow(),SA_HWND, Pascal, Name('GetDesktopWindow')
    SA_DeleteDC(SA_HDC hdc),bool,pascal,proc,Name('DeleteDC')
    SA_ReleaseDC(SA_HWND hwnd, SA_HDC hdc),bool,pascal,proc,Name('ReleaseDC')
    SA_CreateDC(*CString lpszDriver, <*CString lpszDevice>, <*CString lpszOutput>, |
             <long lpInitData>),SA_HDC,raw,pascal,name('CreateDCA')
    SA_GetDC(SA_HWND hwnd),SA_HDC,pascal,Name('GetDC')
    SA_GetObject(SA_HGDIOBJ hgdiobj, signed cbBuffer, *? lpVoid),signed,pascal,raw,name('GetObjectA')
    SA_CreatePolygonRgn(long lppt, unsigned cPoints, signed fnPolyFillMode),SA_HRGN,pascal,Name('CreatePolygonRgn')
    SA_CreateRectRgn(signed nLeftRect, signed nTopRect, signed nRightRect, signed nBottomRect),SA_HRGN,pascal,Name('CreateRectRgn')
    SA_CombineRgn(SA_HRGN hRgnDst, SA_HRGN hrgnSrc1, SA_HRGN hrgnsrc2, Long fnCombineMode),Long, Pascal, Proc, Name('CombineRgn')
    SA_PtInRegion(SA_HRGN hrgn, signed x, signed y),bool,pascal,Name('PtInRegion')
    SA_PtInRect(*SA_RECT, SA_POINT),bool,raw,pascal,Name('PtInRect')
    SA_GetRegionDataRaw(SA_HRGN hrgn, long dwCount, <*? lpRgnData>),long,pascal, Raw,Name('GetRegionData')
    SA_GetRegionData(SA_HRGN hrgn, long dwCount, UNSIGNED lpRgnData),long,pascal, Raw,Name('GetRegionData')
    SA_ExtCreateRegionRaw(<*SA_XFORM lpXform>, UNSIGNED nCount, *? lpRgnData),SA_HRGN,pascal, Raw,Name('ExtCreateRegion')
    SA_ExtCreateRegion(<*SA_XFORM lpXform>, UNSIGNED nCount, UNSIGNED lpRgnData),SA_HRGN,pascal, Raw,Name('ExtCreateRegion')

    SA_CreateEllipticRgnIndirect(*SA_RECT lprc),SA_HRGN,Pascal, Raw, Name('CreateEllipticRgnIndirect')
    SA_SetWindowRgn(SA_HWND hWnd, SA_HRGN hRgn, BOOL bRedraw),Long,Pascal,Name('SetWindowRgn')
    SA_GetWindowRgn(SA_HWND hWnd, SA_HRGN hRgn),Long,Pascal,Proc, Name('GetWindowRgn')
    SA_GetRgnBox(SA_HWND hWnd, *SA_RECT lprc),Long,Pascal,Raw, Proc, Name('GetRgnBox')
    SA_FrameRgn(SA_HDC hdc, SA_HRGN hrgn, SA_HBRUSH hbr, Long nWidth, Long nHeight ),BOOL,Pascal,Proc,Name('FrameRgn')

    SA_IntersectClipRect(SA_HDC hdc, long nLeftRect, long nTopRect, long nRightRect, long nBottomRect),long,pascal,proc,Name('Inte5rsectClipRect')
    SA_ExcludeClipRect(SA_HDC hdc, long nLeftRect, long nTopRect, long nRightRect, long nBottomRect),long,pascal,proc,Name('ExcludeClipRect')
    SA_SelectClipRgn(SA_HDC DC, SA_HRGN hRgn),long,pascal,proc,Name('SelectClipRgn')
    SA_DrawFocusRect(SA_HDC hdc, *SA_RECT lprc),bool,raw,pascal,proc,Name('DrawFocusRect')
    SA_DrawFrameControl(SA_HDC hdc, *SA_RECT lprc, unsigned uType, unsigned uState),bool,raw,pascal,Name('DrawFrameControl')
    SA_InvalidateRect(SA_HWND hwnd, <*SA_RECT lpRect>, bool bErase),bool,raw,pascal,proc,Name('InvalidateRect')
    SA_ValidateRect(SA_HWND hwnd, *SA_RECT lpRect),bool,raw,pascal,proc,Name('ValidateRect')
    SA_GetParent(SA_HWND hwnd),SA_HWND,pascal,Name('GetParent')
    SA_SetParent(SA_HWND hWndChild, SA_HWND hWndNewParent),long,pascal,proc,Name('SetParent')
    SA_FillRect(SA_HDC hdc, *SA_RECT lprc, SA_HBRUSH hbr),bool,raw,pascal,proc,Name('FillRect')
    SA_FillRgn(SA_HDC hdc, SA_HRGN hrgn, SA_HBRUSH hbr),BOOL,Pascal,proc,Name('FillRgn')
    SA_CreateSolidBrush(SA_COLORREF crColor),SA_HBRUSH,pascal,Name('CreateSolidBrush')
    SA_CreateHatchBrush(signed fnStyle, SA_COLORREF clrref),SA_HBRUSH,pascal,Name('CreateHatchBrush')
    SA_GetSysColor(signed nIndex),long,pascal,Name('GetSysColor')
    SA_MoveToEx(SA_HDC hdc, signed x, signed y, *SA_POINT lpPoint),bool,raw,pascal,proc,Name('MoveToEx')
    SA_LineTo(SA_HDC hdc, signed x, signed y),bool,pascal,proc,Name('LineTo')
    SA_PolyLine(SA_HDC hdc, *? pArrayPts, Long nPts),BOOL, pascal, proc, raw, name('PolyLine')

    SA_SetCapture(SA_HWND hwnd),SA_HWND,pascal,proc,Name('SetCapture')
    SA_ReleaseCapture(),bool,pascal,proc,Name('ReleaseCapture')
    SA_GetCapture(),SA_HWND,pascal,Name('GetCapture')
    SA_IsWindow(SA_HWND hwnd),bool,pascal,Name('IsWindow')
    SA_AdjustWindowRectEx(*SA_RECT lpRect, long dwStyle, bool bMenu, long dwExStyle),bool,raw,pascal,Name('AdjustWindowRectEx')
    SA_AdjustWindowRect(*SA_RECT lpRect, long dwStyle, bool bMenu),bool,raw,pascal,proc,Name('AdjustWindowRect')
    SA_GetSysColorBrush(unsigned nIndex),SA_HBRUSH,pascal,name('GetSysColorBrush')
    SA_CreatePen(signed fnPenStyle, signed nWidth, SA_COLORREF crColor),SA_HPEN,pascal,name('CreatePen')

    SA_ExtCreatePen(Long dwPenStyle, Long dwWidth, *SA_LOGBRUSH lpLogbrush, Long dwStyleCount, <*? lpStyle>),SA_HPEN,pascal,raw,name('ExtCreatePen')

    SA_GetSystemMetrics(signed nIndex),signed,pascal,Name('GetSystemMetrics')

    SA_EnumResNameProc(HINSTANCE hInst, long lpszType, long lpszName, |
                    long lParam),bool,pascal,type

    SA_EnumResTypeProc(HINSTANCE hInst, long lpszType, long lParam),bool,pascal,type

    SA_EnumResourceNames(HINSTANCE hModule, long lpszType, SA_EnumResNameProc |
                      lpEnumProc, long lParam),bool,pascal,name('EnumResourceNamesA')
    SA_EnumResourceTypes(HINSTANCE hModule, SA_EnumResTypeProc lpEnumProc, long |
                      lParam),bool,pascal,name('EnumResourceTypesA')

    SA_LoadLibrary(*CString lpLibFileName),SA_HINSTANCE,pascal,raw,name('LoadLibraryA')
    SA_FreeLibrary(long hModule),bool,pascal,proc,Name('FreeLibrary')
    SA_GetProcAddress(long hModule, *CString lpszProcName), long,pascal,raw,name('GetProcAddress')
    SA_GetModuleHandle(*CString lpModuleName),SA_HMODULE,pascal,raw,name('GetModuleHandleA')

    SA_LoadLibraryEx32W(*CString lpszLibFile, long hFile, long dwFlags),long,raw,pascal,Name('LoadLibraryEx32W')
    SA_FreeLibrary32W(long hInst),bool,pascal,Name('FreeLibary32W')
    SA_GetProcAddress32W(long hInst, *CString lpszProc),long,raw,pascal,Name('GetProcAddress32W')

    SA_SetScrollInfo(SA_HWND hwnd, signed fnBar, *SA_SCROLLINFO lpsi, bool fRedraw),signed,raw,pascal,proc,Name('SetScrollInfo')
    SA_GetScrollInfo(SA_HWND hwnd, signed fnBar, *SA_SCROLLINFO lpsi),bool,raw,pascal,Name('GetScrollInfo')
    SA_SetScrollRange(SA_HWND hWnd, signed nBar, signed nMinPos, signed nMaxPos, bool bRedraw),bool,pascal,proc,Name('SetScrollRange')
    SA_ShowScrollBar(SA_HWND hwnd, signed wBar, bool bShow),bool,pascal,proc,Name('ShowScrollBar')

    SA_ScrollWindowEx(SA_HWND hWnd, signed dx, signed dy, <*SA_RECT rcScroll>, *SA_RECT rcClip, SA_HRGN hrgnUpdate, <*SA_RECT rcUpdate>, unsigned flags),signed,raw,pascal,proc,Name('ScrollWindowEx')

    SA_SetFocus(SA_HWND hwnd),SA_HWND,pascal,proc,Name('SetFocus')
    SA_GetFocus(),SA_HWND,pascal,proc,Name('GetFocus')
    SA_GetLastError(),long,pascal,Name('GetLastError')

    SA_CreateFile( *CSTRING lpFileName,                      | file name
                  ULONG dwDesiredAccess,                     | access mode
                  ULONG dwShareMode,                         | share mode
                  <*SA_SECURITY_ATTRIBUTES lpSecurityAttributes>, | SD, null for default
                  ULONG dwCreationDisposition,               | how to create
                  ULONG dwFlagsAndAttributes,                | file attributes
                  UNSIGNED hTemplateFile                     | handle to template file
                ),SA_HANDLE,PASCAL,RAW,NAME('CreateFileA')
    SA_ReadFile  (SA_HANDLE hFile,                           | handle to file
                  UNSIGNED  lpBuffer,                        | data buffer
                  ULONG nNumberOfBytesToRead,                | number of bytes to read
                  <*ULONG lpNumberOfBytesRead>,              | number of bytes read
                  <*? lpOverlapped>                          | overlapped buffer
                ),BOOL,PASCAL,RAW,NAME('ReadFile')
    SA_WriteFile (SA_HANDLE hFile,                           | handle to file
                  UNSIGNED lpBuffer,                         | pointer to data buffer
                  ULONG nNumberOfBytesToWrite,               | number of bytes to write
                  *ULONG lpNumberOfBytesWritten,             | number of bytes written
                  <*? lpOverlapped>                          | overlapped buffer
                ),BOOL,PASCAL,RAW,NAME('WriteFile')
    SA_FlushFileBuffers(SA_HANDLE hFile                      | handle to file
                      ),BOOL,PASCAL,PROC,NAME('FlushFileBuffers')
    SA_LockFile(SA_HANDLE hFile,                             | handle to file
                ULONG dwFileOffsetLow,                       | low-order word of offset
                ULONG dwFileOffsetHigh,                      | high-order word of offset
                ULONG nNumberOfBytesToLockLow,               | low-order word of length
                ULONG nNumberOfBytesToLockHigh               | high-order word of length
              ),BOOL,PASCAL,NAME('LockFile')
    SA_UnlockFile(SA_HANDLE hFile,                           | handle to file
                  ULONG dwFileOffsetLow,                     | low-order word of offset
                  ULONG dwFileOffsetHigh,                    | high-order word of offset
                  ULONG nNumberOfBytesToUnLockLow,           | low-order word of length
                  ULONG nNumberOfBytesToUnLockHigh           | high-order word of length
                ),BOOL,PASCAL,NAME('UnlockFile')
    SA_GetFileSize(SA_HANDLE hFile,                          | handle to file
                  <*ULONG lpFileSizeHigh>                    | high-order word of file size
                 ),ULONG,PASCAL,RAW,NAME('GetFileSize')
    SA_SetFilePointer(SA_HANDLE hFile,                  | handle to file
                    LONG lDistanceToMove,               | bytes to move pointer
                    <*LONG lpDistanceToMoveHigh>,       | bytes to move pointer !Omit for null
                    ULONG dwMoveMethod                  | starting point FILE_BEGIN, FILE_CURRENT, FILE_END
                   ),LONG,PASCAL,RAW,NAME('SetFilePointer')
    SA_DeleteFile(*cstring szFileName),bool,name('DeleteFileA'),raw,pascal
    SA_ReplaceFile(*cstring szFileName, *cstring szReplacementFileName, long lpBackupFileName, Ulong dwReplaceFlags, |
                long lpExclude = 0, long lpReserved = 0),bool,raw,pascal,name('ReplaceFileA')
    SA_CopyFile(*cstring szFileName, *cstring szNewFileName, bool bFailIfExists),bool,raw,pascal,name('CopyFileA')
    SA_SetEndOfFile(SA_HANDLE hFile),bool,pascal,proc,Name('SetEndoOfFile')

    SA_GetWindowsDirectory(*CString lpBuffer, unsigned uSize),unsigned,pascal,raw,proc,name('GetWindowsDirectoryA')
    SA_GetCurrentDirectory(long nBufferLength, *CString lpBuffer),long,raw,pascal,proc,name('GetCurrentDirectoryA')
    SA_GetModuleFileName(SA_HMODULE hModule, *CString lpFilename, long nSize),long,pascal,raw,proc,name('GetModuleFileNameA')

    SA_GetCurrentProcessID(),long,pascal,name('GetCurrentProcessId')
    SA_GetCurrentThreadID(),long,pascal,name('GetCurrentThreadId')

    !SA_TimerProc(SA_HWND hwnd, unsigned uMsg, unsigned idEvent, long dwTime),pascal,type

    SA_SetTimer(SA_HWND hWnd, unsigned nIDEvent, unsigned uElapse, long lpTimerFunc),unsigned,pascal,proc,Name('SetTimer')
    SA_KillTimer(SA_HWND hWnd, unsigned uIDEvent),bool,pascal,proc,Name('KillTimer')

    SA_InitializeCriticalSection(*Long lpCriticalSection),raw,pascal,Name('InitializeCriticalSection')
    SA_EnterCriticalSection(*SA_CRITICAL_SECTION lpCriticalSection),raw,pascal,Name('EnterCriticalSection')
    SA_LeaveCriticalSection(*SA_CRITICAL_SECTION lpCriticalSection),raw,pascal,Name('LeaveCriticalSection')
    SA_DeleteCriticalSection(*SA_CRITICAL_SECTION lpCriticalSection),raw,pascal,Name('DeleteCriticalSection')

    SA_GlobalAlloc(long uFlags, long dwBytes),long,pascal,Name('GlobalAlloc')
    SA_GlobalFree(SA_HGLOBAL hMem),SA_HGLOBAL,pascal,proc,Name('GlobalFree')
    SA_GlobalLock(SA_HGLOBAL hMem),long,pascal,proc,Name('GlobalLock')
    SA_GlobalUnlock(SA_HGLOBAL hMem),bool,pascal,proc,Name('GlobalUnlock')
    SA_GlobalHandle(long lpmem),SA_HGLOBAL,pascal,Name('GlobalHandle')
    SA_GlobalSize(SA_HGLOBAL hMem),UNSIGNED,pascal,Name('GlobalSize')

    SA_VirtualAlloc(long lpAddress, long dwSize, long flAllocationType, long flProtect),long,pascal,Name('VirtualAlloc')
    SA_HeapAlloc(long hHeap, long dwFlags, long dwBytes),long,pascal,Name('HeapAlloc')
    SA_VirtualFree(long lpAddress, long dwSize, long dwFreeType),bool,pascal,Name('VirtualFree')
    SA_HeapFree(long hHeap, long dwFlags, long lpMem),bool,pascal,Name('HeapFree')
    SA_GetProcessHeap(),long,pascal,Name('GetProcessHeap')

    SA_CloseHandle(SA_HANDLE),bool,raw,pascal,proc,Name('CloseHandle')

    SA_IsBadReadPtr(long SourcePtr,long dwLength),bool,raw,pascal,Name('IsBadReadPtr')
    SA_IsBadWritePtr(long DestinationPtr,long dwLength),bool,raw,pascal,Name('IsBadWritePtr')

    SA_MoveMemory(long DestinationPtr,long SourcePtr,long dwLength),bool,raw,proc,pascal,name('RtlMoveMemory')
    SA_FillMemory(long DestinationPtr,long dwLength, long dValue),pascal,Raw,name('RtlFillMemory')
    SA_ZeroMemory(long DestinationPtr,long dwLength),raw,pascal,name('RtlZeroMemory')

    SA_lstrcpy(long DestinationPtr, long SourcePtr),long,pascal,Name('lstrcpy')
    SA_lstrcat(long DestinationPtr, long SourcePtr),long,pascal,Name('lstrcat')
    SA_lstrcmp(long DestinationPtr, long SourcePtr),long,pascal,Name('lstrcmp')
    SA_lstrlenW(*? lpWString),signed,proc,pascal,Raw,Name('lstrlenW')

    SA_wsprintf(long lpBuffer, long lpFormat, long Value),long,C,raw,name('wsprintfA')

    SA_GetTickCount(),long,pascal,Name('GetTickCount')

    SA_GetMenu(SA_HWND hWnd),SA_HMENU,pascal,Name('GetMenu')
    SA_GetSubMenu(HMENU hMenu, long nPos),SA_HMENU,pascal,Name('GetSubMenu')
    SA_GetMenuItemCount(HMENU hMenu),long,pascal,Name('GetMenuItemCount')
    SA_GetMenuItemID(HMENU hMenu, long nPos),long,pascal,Name('GetMenuItemId')
    SA_GetMenuItemInfo(HMENU hMenu, long uItem, bool fByPosition, *MENUITEMINFO mii),bool,raw,pascal,name('GetMenuItemInfoA')
    SA_SetMenuItemInfo(HMENU hMenu, long uItem, bool fByPosition, *MENUITEMINFO mii),bool,raw,pascal,name('SetMenuItemInfoA')

    SA_BringWindowToTop(SA_HWND hWnd),short,proc,pascal,Name('BrintWindowToTop')
    SA_FindWindow(<*cstring lpszClassName>, <*cstring lpszWindowName>),SA_HWND,raw,pascal,name('FindWindowA')
    SA_EnumWindows(UNSIGNED lpEnumFunc, Long lParam ),Bool, Pascal, Name('EnumWindowsA')
    SA_EnumChildWindows(SA_HWND hWndParent, UNSIGNED lpEnumFunc, Long lParam ),Bool, Pascal, Name('EnumChildWindows')

    SA_GetForegroundWindow(),SA_hwnd,pascal,Name('GetForegroundWindow')
    SA_GetGUIThreadInfo(long idThread, *GUITHREADINFO lpgui),bool,raw,pascal,Name('GetGUIThreadInfo')
    SA_WindowFromPoint(real PtByValue),SA_HWND,pascal,Name('WindowFromPoint')

    SA_InterlockedIncrement(*long Var),long,pascal,proc,Name('InterlockedIncrement')
    SA_InterlockedDecrement(*long Var),long,pascal,proc,Name('InterlockedDecrement')

    SA_CoInitialize(long = 0),SA_HRESULT,pascal,Name('CoInitialize')
    SA_CoUninitialize(),pascal,Name('CoUninitialize')
    SA_CoTaskMemFree(LONG pv),PASCAL,Name('CoTaskMemFree')
    SA_OleInitialize(long pvReserved),long,pascal,Name('OleInitialize')
    SA_OleUninitialize(),pascal,Name('OleUnititialize')
    SA_CoGetClassObject(REFCLSID rclsid, long dwClsContext, long pvReserved, |
                     REFIID riid, *long ppvObject),long,raw,pascal,Name('CoGetClassObject')
    SA_CoCreateInstance(REFCLSID rclsid, long pUnkOuter, long dwClsContext, |
                     REFIID riid, *long ppvObject),long,raw,pascal,Name('CoCreateInstance')
    SA_CoLockObjectExternal(long pUnk, bool fLock, bool fLastUnlockReleases),SA_HRESULT,pascal,Name('CoLockObjectExternal')
    SA_RegisterDragDrop(SA_HWND hwnd, long pDropTarget),long,pascal,Name('RegisterDragDrop')
    SA_RevokeDragDrop(SA_HWND hwnd),long,pascal,Name('RevokeDrageDrop')
    SA_DoDragDrop(long pDataObj, long pDropSource, long dwOKEffects, *long dwEffect),long,pascal,Name('DoDragDrop')

    SA_GetRunningObjectTable(long reserved, long ppRot),SA_HRESULT,pascal,Name('GetRunningObjectTable')
    SA_OleRun(long pIUnk),SA_HRESULT,pascal,Name('OleRun')
    SA_GetActiveObject(REFCLSID rclsid, long pvReserved, *long pUnk),SA_HRESULT,pascal,Name('GetActiveObject')

    SA_WaitForSingleObject(SA_HANDLE hHandle, long dwMilliSeconds),long,pascal,Name('WaitForSingleObject')
    SA_MsgWaitForMultipleObjects(long nCount, long pHandles, bool bWaitAll, long dwMilliseconds, |
                              long dwWakeMask),long,pascal,Name('MsgWaitForMultipleObjects')

    SA_WaitForMultipleObjects(Long nCount, |
                                *? lpHandles, |
                                BOOL bWaitAll, |
                                ULong dwMilliseconds |
                               ),ULong, Pascal, Raw, Proc, Name('WaitForMultipleObjects')

    SA_WaitForSingleObjectEx(SA_HANDLE hHandle, |
                             ULong dwMilliseconds, |
                             BOOL bAlertable  |
                             ),ULong, Pascal, Proc, Name('WaitForSingleObjectEx')
    SA_WaitForMultipleObjectsEx(Long nCount, |
                                *? lpHandles, |
                                BOOL bWaitAll, |
                                ULong dwMilliseconds, |
                                BOOL bAlertable       |
                               ),ULong, Pascal, Raw, Proc, Name('WaitForMultipleObjectsEx')

    SA_ResetEvent(SA_HANDLE hEvent),bool,pascal,proc,Name('ResetEvent')
    SA_TranslateMessage(long lpMsg),long,pascal,proc,name('TranslateMessage')
    SA_DispatchMessage(long lpMsg),long,pascal,proc,name('DispatchMessageA')
    SA_WaitMessage(),bool,pascal,Name('WaitMessage')
    SA_GetMessage(long lpMsg, SA_HWND hWnd, long wMsgFilterMin, long wMsgFilterMax),bool,pascal,name('GetMessageA')

    SA_MultiByteToWideChar(long CodePage, long dwFlags, long lpMultiByteStr, long cbMultiByte, |
                        long lpWideCharStr, long cchWideCharStr),long,pascal,Name('MultiByteToWideChar')

    SA_WideCharToMultiByte(long CodePage, long dwFlags, long lpWideCharStr, long cchWideChar, |
                        long lpMultiByteStr, long cbMultiByte, long lpDefaultChar, |
                        long lpUsedDefaultChar),long,pascal,Name('WideCharToMultiByte')

!WINOLEAUTAPI VarI4FromDate(DATE dateIn, LONG * plOut);
    SA_VarI4FromDate(REAL dateIn, *LONG plOut),long,pascal,Name('VarI4FromDate')

    SA_VarBstrFromDate(REAL dateIn, LONG lcid, ULONG dwFlags, *LONG pbstrOut),long,pascal,Name('VarBstrFromDate')
    SA_VarBstrFromDate(REAL dateIn, LONG lcid, ULONG dwFlags, *bstring pbstrOut),long,pascal,Name('VarBstrFromDate')
    SA_VarDecFromR8(REAL dblIn, *DECIMAL pdecOut),long,pascal,Name('VarDecFromR8')

    SA_GetSystemTime(*SA_SYSTEMTIME),raw,pascal,Name('GetSystemTime')
    SA_GetSystemTimeAsFileTime(*SA_FILETIME SystemTimeAsFileTime),raw,pascal,Name('GetFileTime')
   
    SA_SystemTimeToFileTime(*SA_SYSTEMTIME lpSystemTime, *SA_FILETIME lpFileTime),BOOL, Pascal, Raw,Proc,Name('SystemTimeToFileTime')
    SA_FileTimeToLocalFileTime(*SA_FILETIME lpFileTime, *SA_FILETIME lpLocalFileTime),BOOL,Pascal, Raw,Proc,Name('FileTimeToLocalFileTime')
    SA_FileTimeToSystemTime(*SA_FILETIME lpFileTime, *SA_SYSTEMTIME lpSystemTime),BOOL, Pascal,Raw,Proc,Name('FileTimeToSystemTime')
    SA_LocalFileTimeToFileTime(*SA_FILETIME lpLocalFileTime, *SA_FILETIME lpFileTime),BOOL, Pascal, Raw,Proc,Name('LocalFileTimeToFileTime')                  

    SA_OutputDebugString(Const *Cstring lpOutputString),Pascal,Raw,Name('OutputDebugStringA')
   
    SA_GetThreadLocale(),long,pascal,Name('GetThreadLocale')

    SA_CreateEvent(<*SA_SECURITY_ATTRIBUTES lpSecurityAttributes>, bool bManualReset, |
                   bool bInitialState, long lpName),long,pascal,name('CreateEventA')
    SA_CreateEvent(<*SA_SECURITY_ATTRIBUTES lpSecurityAttributes>, bool bManualReset, |
                   bool bInitialState, <*CString lpName>),long,pascal, Raw, Name('CreateEventA')
                   
    SA_SetEvent(SA_HANDLE hEvent),bool,pascal,proc,Name('SetEvent')

    SA_CreateThread(<*SA_SECURITY_ATTRIBUTES lpSecurityAttributes>, long Stacksize, long lpFunction, long lpParameter, |
                 long CreationFlags, *long ThreadID),long,raw,pascal,Name('CreateThread')

    SA_TerminateThread(long hThread, long dwExitCode),bool,pascal,proc,Name('TerminateThread')
    SA_PostQuitMessage(long nExitCode),pascal,Name('PostQuitMessage')

! Clipboard Manager Functions

    SA_OpenClipboard(long hWndNewOwner),bool,pascal,Name('OpenClipboard')
    SA_CloseClipboard(),BOOL,pascal,Name('CloseClipboard')
    SA_GetClipboardSequenceNumber(),long,pascal,Name('GetClipboardSequenceNumber')
    SA_GetClipboardOwner(),long,pascal,Name('GetClipboardOwner')
    SA_SetClipboardViewer(long hWndNewViewer),long,pascal,Name('SetClipboardViewer')
    SA_GetClipboardViewer(),long,pascal,Name('GetClipboardViewer')
    SA_ChangeClipboardChain(long hWndRemove, long hWndNewNext),bool,pascal,Name('GetClipboardChain')
    SA_SetClipboardData(long uFormat, long hMem),long,pascal,Name('SetClipboardData')
    SA_GetClipboardData(long uFormat),long,pascal,Name('GetClipboardData')
    SA_CountClipboardFormats(),long,pascal,Name('CountClipboardFormats')
    SA_EnumClipboardFormats(long lformat),long,pascal,Name('EnumClipboardFormats')
    SA_RegisterClipboardFormat(long lpszFormat),long,pascal,name('RegisterClipboardFormatA')
    SA_GetClipboardFormatName(long lformat, long lpszFormatName, long cchMaxCount),long,pascal,name('GetClipboardFormatNameA')

    SA_EmptyClipboard(),bool,pascal,Name('EmptyClipboard')
    SA_IsClipboardFormatAvailable(long lformat),bool,pascal,Name('IsClipboardFormatAvailable')
    SA_GetPriorityClipboardFormat(*long aFormatPriorityList, long cFormats),long,pascal,Name('GetPriorityClipboardFormat')
    SA_GetOpenClipboardWindow(),long,pascal,Name('GetOpenClipboardWindow')

! Registry Functions
    SA_RegOpenKey(long hKey, *Cstring pszSubKey, *long phkResult),long,pascal,name('RegOpenKeyA')
    SA_RegOpenKeyEx(Long hKey,*Cstring lpSubKey, <ULong ulOptions>, UNSIGNED samDesired, *UNSIGNED phkResult)Long,Pascal,Raw,Name('RegOpenKeyExA')

    SA_RegQueryValue(long hKey, *Cstring pszSubKey, *Cstring pszValue, *long cbValue),long,pascal,name('RegQueryValueA')
    SA_RegQueryValueEx(long hKey, *Cstring pszValueName, <long lpReserved>, <*long dwType>, *? lpData, *long cbData),long,pascal,name('RegQueryValueExA')
    SA_RegCloseKey(long hKey),long,proc,pascal, Name('RegCloseKey')

!-----

      SA_GlobalAddAtom(*CSTRING lpString),USHORT,PASCAL,RAW,NAME('GlobalAddAtomA')
      SA_GlobalGetAtomName(USHORT nAtom, |
                        *CSTRING lpBuffer, |
                        SIGNED nSize),ULONG,PASCAL,RAW,NAME('GlobalGetAtomNameA')
      SA_GlobalFindAtom(*CSTRING lpString),USHORT,PASCAL,RAW,NAME('GlobalFindAtomA')

      SA_GlobalDeleteAtom(UNSIGNED nAtom),USHORT,PASCAL,RAW,PROC,NAME('GlobalDeleteAtom')

      SA_RegisterWindowMessage(*CSTRING lpString),UNSIGNED,RAW,PASCAL,NAME('RegisterWindowMessageA')

      SA_SystemParametersInfo(UNSIGNED uiAction,     |
                           UNSIGNED uiParam=0,    |Depends upon the uiAction parameter
                           <*? pvParam>,          |
                           UNSIGNED fWinIni=0),BOOL,PASCAL,RAW,PROC,NAME('SystemParametersInfoA')

      SA_CreatePalette(*? pPal),SA_HANDLE,PASCAL,RAW,NAME('CreatePalette')
      SA_SelectPalette(SA_HDC Thishdc, SA_HANDLE hpal, BOOL bForceBackground),SA_HANDLE,PASCAL,PROC,Name('SelectPalette')
      SA_RealizePalette(SA_HDC Thishdc),UNSIGNED,PASCAL,PROC,Name('RealizePalette')
      SA_UpdateColors(SA_HDC hdc),Bool,PASCAL,PROC,Name('UpdateColors')

      SA_StretchBlt(LONG hdcDest,      | handle to destination DC
                 LONG nXOriginDest, | x-coord of destination upper-left corner
                 LONG nYOriginDest, | y-coord of destination upper-left corner
                 LONG nWidthDest,   | width of destination rectangle
                 LONG nHeightDest,  | height of destination rectangle
                 LONG hdcSrc,       | handle to source DC
                 LONG nXOriginSrc,  | x-coord of source upper-left corner
                 LONG nYOriginSrc,  | y-coord of source upper-left corner
                 LONG nWidthSrc,    | width of source rectangle
                 LONG nHeightSrc,   | height of source rectangle
                 LONG dwRop         | raster operation code
               ),BOOL,PASCAL,PROC,Name('StretchBlt')

      SA_ScrollWindowEx(SA_HWND hWnd, |
                        signed dx, signed dy, |
                        <*SA_RECT rcScroll>,   |
                        <*SA_RECT rcClip>,     |
                        <SA_HRGN hrgnUpdate>,    |
                        <*SA_RECT rcUpdate>,   |
                         unsigned flags),signed,raw,pascal,proc, Name('ScrollWindowEx')

      SA_SetMapMode(SA_HDC Thishdc, LONG fnMapMode),LONG,PASCAL,PROC,Name('SetMapMode')
      SA_SaveDC(SA_HDC Thishdc),SA_HDC,PASCAL,PROC,Name('SaveDC')
      SA_RestoreDC(SA_HDC Thishdc, LONG nSavedDC),BOOL,PASCAL,PROC,Name('RestoreDC')

      SA_SetViewportOrgEx(SA_HDC Thishdc,     | handle to device context
                       LONG X,          | new x-coordinate of viewport origin
                       LONG Y,          | new y-coordinate of viewport origin
                       <*SA_POINT lpPoint> | original viewport origin
                       ),BOOL,PASCAL,RAW,PROC,Name('SetViewportOrgEx')

      SA_CreateRectRgnIndirect(*SA_RECT lprc),SA_HANDLE,PASCAL,RAW,Name('CreateRectRgnIndirect')
      SA_PaintRgn(SA_HDC hdcThis,SA_HANDLE hrgn),BOOL,PASCAL,PROC,Name('PaintRgn')
      SA_InvalidateRgn(SA_HWND ThishWnd, SA_HRGN ThisHrgn, BOOL bErase),BOOL,PASCAL,PROC,Name('InvalidateRgn')
      SA_ValidateRgn(SA_HWND ThishWnd, SA_HRGN hRgn),BOOL,PASCAL,PROC,Name('ValidateRgn')

     SA_CreateBrushIndirect(*SA_LOGBRUSH lplb),SA_HBRUSH,PASCAL,RAW,Name('CreateBrushIndirect')

     SA_RedrawWindow(LONG hWnd,         | handle to window
                  <*SA_RECT lprcUpdate>,| update rectangle
                  LONG hrgnUpdate=0, | handle to update region
                  LONG flags         | array of redraw flags
                 ),BOOL,PASCAL,RAW,PROC,Name('RedrawWindow')


     SA_SetRect(*SA_RECT lprc,     | rectangle
             LONG xLeft,     | left side
             LONG yTop,      | top side
             LONG xRight,    | right side
             LONG yBottom    | bottom side
            ),BOOL,PASCAL,RAW,PROC,Name('SetRect')


     SA_OffsetRect(*SA_RECT lprc,  | rectangle
                LONG dx,     | horizontal offset
                LONG dy      | vertical offset
               ),BOOL,PASCAL,RAW,PROC,Name('OffsetRect')
 
     SA_InflateRect(*SA_RECT lprc,  | rectangle
                 LONG dx,       | amount to adjust width
                 LONG dy        | amount to adjust height
                ),BOOL,PASCAL,RAW,PROC,Name('InflateRect')

     SA_IntersectRect(*SA_RECT lprcDst,     | intersection buffer
                   *SA_RECT lprcSrc1,    | first rectangle
                   *SA_RECT lprcSrc2     | second rectangle
                  ),Bool,Pascal,Raw,Name('IntersectRect')

     SA_IsRectEmpty(*SA_RECT lprc),Bool,Pascal,Raw,Name('IsRectEmpty')

     SA_MapWindowPoints(SA_HWND hWndFrom,     | handle to source window
                     SA_HWND hWndTo,       | handle to destination window
                     *? lpPoints,       | array of points to map
                     UNSIGNED cPoints   | number of points in array
                    ),SIGNED,PASCAL,RAW,PROC,Name('MapWindowPoints')

 
     SA_GetBkColor(LONG hdc),LONG,PASCAL,Name('GetBkColor')

     SA_MulDiv(LONG nNumber,       | 32-bit signed multiplicand
            LONG nNumerator,    | 32-bit signed multiplier
            LONG nDenominator   | 32-bit signed divisor
           )LONG,PASCAL,Name('MulDiv')

     SA_CreateDIBitmap(LONG hdc,              | handle to DC
                    LONG lpbmih,           | bitmap data
                    LONG fdwInit,          | initialization option
                    LONG lpbInit,          | pointer to initialization data
                    LONG lpbmi,            | color-format data
                    LONG fuUsage           | color-data usage
                   ),LONG,PASCAL,RAW,Name('CreateDIBitmap')  !hBitmap

     SA_SetDIBits(LONG hdc,                  | handle to DC
               LONG hbmp,                 | handle to bitmap
               LONG uStartScan,           | starting scan line
               LONG cScanLines,           | number of scan lines
               LONG lpvBits,              | array of bitmap bits
               *BITMAPINFO lpbmi,        | bitmap data
               LONG fuColorUse            | type of color indexes to use
              ),LONG,PASCAL,RAW,Name('SetDIBits')

     SA_SetStretchBltMode(SA_HDC hdc,           | handle to DC
                       Long iStretchMode  | bitmap stretching mode
                      ),Long, PASCAL,RAW,Name('SetStretchBltMode')

     SA_StretchDIBits(SA_HDC hdc,                      | handle to DC
                        LONG XDest,                    | x-coord of destination upper-left corner
                        LONG YDest,                    | y-coord of destination upper-left corner
                        LONG nDestWidth,               | width of destination rectangle
                        LONG nDestHeight,              | height of destination rectangle
                        LONG XSrc,                     | x-coord of source upper-left corner
                        LONG YSrc,                     | y-coord of source upper-left corner
                        LONG nSrcWidth,                | width of source rectangle
                        LONG nSrcHeight,               | height of source rectangle
                        LONG lpBits,                   | bitmap bits
                        LONG lpBitsInfo,               | bitmap data
                        ULONG iUsage,                  | usage options
                        UNSIGNED dwRop                 |  raster operation code
                      ),LONG,PASCAL,RAW,Name('StretchDIBits')

      SA_LoadImage(LONG hisnst,  |
                *CSTRING lpszName, |
                LONG uType,    |
                LONG cxDesired,  |
                LONG cyDesired,  |
                LONG fuLoad    |
               ),LONG,PASCAL,RAW,NAME('LoadImageA')


     SA_CreateFontIndirect(*SA_LOGFONT lplf   | characteristics
                       ),LONG,PASCAL,RAW,NAME('CreateFontIndirectA')

     SA_GetDeviceCaps(LONG hdc,    | handle to DC
                   LONG nIndex  | index of capability
                  ),LONG,PASCAL,Name('GetDeviceCaps')

     SA_GetTextExtentPoint32(SA_HDC hdc,           | handle to DC
                          *CSTRING lpString,  | text string
                          LONG cbString,      | characters in string
                          *SA_SIZE lpSize     | string size SIZE structure
                         ),BOOL,PASCAL,RAW,PROC,NAME('GetTextExtentPoint32A')

     SA_GetCharABCWidths(LONG hdc,         | handle to DC
                       UNSIGNED uFirstChar, | first character in range
                       UNSIGNED uLastChar,  | last character in range
                       UNSIGNED lpabc       | pointer to array of character widths
                      ),BOOL,PASCAL,RAW,NAME('GetCharABCWidthsA')

     SA_GetCharacterPlacement(LONG hDC,                  | handle to DC
                            *CSTRING lpString,         | character string
                            SIGNED nCount,             | number of characters
                            SIGNED nMaxExtent,         | maximum extent for string
                            *SA_GCP_RESULTS lpResults,    | placement result
                            ULONG dwFlags              | placement options
                          ),ULONG,PASCAL,RAW,NAME('GetCharacterPlacementA')

     SA_SetTextJustification(SA_HDC hdc,          | handle to DC
                          SIGNED nBreakExtra,  | length of extra space
                          SIGNED nBreakCount   | count of space characters
                          ),BOOL,PASCAL,PROC,Name('SetTextJustification')

     SA_SetTextAlign(SA_HDC hDC, UNSIGNED fMode),UNSIGNED,PASCAL,PROC,Name('SetTextAlign')
     SA_GetTextAlign(SA_HDC hDC),UNSIGNED,PASCAL,PROC,Name('GetTextAlign')

     SA_GetFontLanguageInfo(LONG hdc),ULONG,PASCAL,Name('GetFontLanguageInfo')

     SA_GetOutlineTextMetrics(LONG hdc,                    | handle to DC
                           UNSIGNED cbData,             | size of metric data array
                           <*? lpOTM>                   | array of OTM, metric data array
                          ),UNSIGNED,PASCAL,RAW,NAME('GetOutlineTextMEtricsA')

     SA_GetTextMetrics(SA_HDC hdc, *TEXTMETRIC tm),Bool,Pascal,Raw,Proc,Name('GetTextMetricsA')

     SA_GetKerningPairs(SA_HDC hdc,                 | handle to DC
                     UNSIGNED nNumPairs,      | number of kerning pairs
                     <*? lpkrnpair> | array of kerning pairs
                    ),UNSIGNED,PASCAL,RAW,NAME('GetKerningPairsA')

     SA_TextOut(SA_HDC hdc,             | handle to DC
               LONG nXStart,       | x-coordinate of starting position
               LONG nYStart,       | y-coordinate of starting position
               *CSTRING lpString,  | character string
               LONG cbString       | number of characters
              ),BOOL,PASCAL,PROC,RAW,NAME('TextOutA')

     SA_TabbedTextOut(SA_HDC hDC,   |handle to DC
                      Long X,       |x-coord of start
                      Long Y,       | y-coord of start
                      *Cstring lpString, |character string
                      Long nCount,  | number of characters
                      Long nTabPositions, | number of tabs in array
                      *Long lpnTabStopPositions, |array of tab positions
                      Long nTabOrigin | start of tab expansion
                      ),Bool, Pascal, Raw, Proc, Name('TabbedTextOutA')

    SA_ExtTextOut(SA_HDC hDC,          | handle to DC
                LONG X,           | x-coordinate of reference point
                LONG Y,           | y-coordinate of reference point
                ULONG fuOptions,   | text-output options
                <*SA_RECT lprc>,     | optional dimensions
                *? lpString,| pointer to string or Glyph index array
                LONG cbCount,     | number of characters in string
                <*? lpDx>       | array of spacing values
               ),BOOL,RAW,PASCAL,NAME('ExtTextOutA'),PROC


     SA_DrawTextEx(LONG hDC,          | handle to DC
              *CSTRING lpString, | text to draw
              LONG nCount,       | text length
              *SA_RECT lpRect,      | formatting dimensions
              LONG uFormat,      | text-drawing options
              <*SA_DRAWTEXTPARAMS dtp>| Extended parameters
             ),LONG,PROC,PASCAL,RAW,NAME('DrawTextExA')

     SA_GetTextColor(LONG hdc),LONG,PASCAL,Name('GetTextColor')

     SA_SetGraphicsMode(LONG hdc,   | handle to device context
                      LONG iMode  | graphics mode
                     ),LONG,PASCAL,PROC,Name('SetGraphicsMode')

     SA_ClipCursor(<*SA_RECT lpRect>),BOOL,PASCAL,RAW,PROC,Name('ClipCursor')


     SA_OpenPrinter    ( *CSTRING pPrinterName,       | printer or server name
                       *LONG phPrinter,             | printer or server handle
                       LONG pDefault                | printer defaults
                     ),BOOL,PASCAL,RAW,NAME('OpenPrinterA')

     SA_WritePrinter   ( LONG hPrinter,             | handle to printer object
                       *? pBuf,                     | Pointer to array of printer data
                       LONG cbBuf,                 | size of array
                       *LONG pcBytesWritten         | Pointer to bytes received
                     ),BOOL,RAW,PASCAL,PROC,Name('WritePrinter')

      SA_GetPrinter     (LONG hPrinter,    | handle to printer
                      LONG Level,       | information level
                      *? pPrinter,      | printer information buffer
                      LONG cbBuf,       | size of buffer
                      *LONG pcbNeeded   | bytes received or required
                     ),BOOL,PASCAL,RAW,NAME('GetPrinterA')

      SA_ClosePrinter ( LONG hPrinter ),BOOL,RAW,PASCAL,PROC,Name('ClosePrinter')
      SA_StartDoc    (SA_HDC hdc, *SA_DOCINFO lpDocInfo),LONG,PASCAL,RAW,PROC,NAME('StartDocA')
      SA_EndDoc         ( LONG hdc ),LONG,PASCAL,RAW,PROC,Name('EndDoc')
      SA_StartPage      ( LONG hDC ),LONG,PASCAL,RAW,PROC,Name('StartPage')
      SA_EndPage     ( LONG hdc ),LONG,PASCAL,RAW,PROC,NAME('EndPage')

      SA_StartDocPrinter( LONG hPrinter,             | handle to printer object
                       LONG Level,                 | information level
                       *? pDocInfo                  | information buffer
                     ),LONG,PASCAL,PROC,RAW,NAME('StartDocPrinterA')
      SA_EndDocPrinter  ( LONG hPrinter ),BOOL,PASCAL,RAW,PROC,Name('EndDocPrinter')
      SA_StartPagePrinter( LONG hPrinter ),BOOL,PASCAL,RAW,PROC,Name('StartPagePrinter')
      SA_EndPagePrinter( LONG hPrinter ),BOOL,PASCAL,RAW,PROC,Name('EndPagePrinter')

      SA_EnumPrinters  ( LONG Flags,         | printer object types
                      <*CSTRING>,          | Ponter to string containing name of printer object
                      LONG Level,         | information level
                      *? pPrinterEnum,     | Pointer to printer information buffer
                      LONG cbBuf,         | size of printer information buffer
                      *? pcbNeeded,        | pointer to DWORD of bytes received or required
                      *? pcReturned        | pointer to DWORD of number of printers enumerated
                    ),BOOL,PASCAL,RAW,NAME('EnumPrintersA')


      SA_DeviceCapabilities(LONG pDevice,        | printer name
                         LONG pPort,          | port name
                         USHORT fwCapability, | device capability
                         LONG pOutput,        | output buffer, datatype depends on fwCapability
                         <*DEVMODE pDevMode>  | device data buffer
                       ),LONG,PASCAL,RAW,NAME('DeviceCapabilitiesA'),PROC


     SA_SetLastError(ULONG dwErrCode),PASCAL,Name('SetLastError')


     SA_SetProp(LONG hWnd, *? lpString, LONG hData),BOOL,PASCAL,RAW,NAME('SetPropA')
     SA_GetProp(LONG hWnd, *? lpString),LONG,PASCAL,RAW,NAME('GetPropA')
     SA_RemoveProp(LONG hWnd, *? lpString),LONG,PASCAL,RAW,NAME('RemovePropA'),PROC

     SA_FindResource(LONG hModule, LONG lpName, LONG lpType),LONG,PASCAL,NAME('FindResourceA')
     SA_FindResource(Long hModule, *Cstring pszName, *Cstring pszType), Long, Pascal,Raw,Name('FindResourceA'),DLL(1)
     SA_LoadResource(LONG hModule, LONG hResInfo),LONG,PASCAL,RAW,Name('LoadResource')
     SA_LockResource(LONG hResData),LONG,PASCAL,Name('LockResource')
     SA_SizeOfResource(SA_HMODULE hmodule, SA_HANDLE hres),ULONG, Pascal, Name('Sizeofresource')

     SA_EnableScrollBar(UNSIGNED ThishWnd, UNSIGNED wSBflags, UNSIGNED wArrows),BOOL,PASCAL,Proc,Name('EnableScrollBar')

     SA_GetKeyState(Long Vkey),Short,Pascal,Name('GetKeyState')

     SA_lstrlen(Long lpString),LONG,PASCAL,Name('lstrlen')
     SA_lstrlenRaw(*CSTRING lpString),LONG,PASCAL,RAW,Name('lstrlen')
     SA_lstrcpynRaw(*CSTRING lpDestStr, *CSTRING lpSrcStr, LONG cbCopy),PASCAL,RAW,Name('lstrcpyn')

     SA_memset(Long pDestination, Long nValue, Long SizeToFill),Long,proc,raw,Name('_MEMSET') !only low byte of value is used
     SA_memsetRawDst(*? pDestination, Long nValue, Long SizeToFill),Long,proc,raw,Name('_MEMSET')

     SA_memcpy(LONG Dest, LONG Src, LONG SizeToCpy),LONG,PROC,NAME('_MEMCPY')
     SA_memcpyRawDst(*? Dest, LONG Src, LONG SizeToCpy),LONG,PROC,RAW,NAME('_MEMCPY')
     SA_memcpyRawSrc(Long Dest, *? Src, LONG SizeToCpy),LONG,PROC,RAW,NAME('_MEMCPY')
     SA_memcpyRaw(*? Dest, *? Src, LONG SizeToCpy),LONG,PROC,RAW,NAME('_MEMCPY')

     SA_ZeroMemory(*? pDestination,   | memory block
                LONG cbLength      | size of memory block
                ),PASCAL,RAW,NAME('RtlZeroMemory')


     SA_GetSaveFileName(*SA_OPENFILENAME ofn),BOOL,Proc,Raw,Pascal,Name('GetSaveFileNameA')
     SA_GetOpenFileName(*SA_OPENFILENAME ofn),BOOL,Proc,Raw,Pascal,Name('GetOpenFileNameA')
     SA_InitCommonControls(),Pascal,Name('InitCommonControls')
     SA_CommDlgExtendedError(),ULONG,Pascal,Name('CommDlgExtendedError')

     SA_GetVersionEx(*SA_OSVERSIONINFO lpVersionInfo),BOOL,Pascal,Raw,Name('GetVersionExA')
     SA_GetSystemInfo(*SA_SYSTEM_INFO lpSystemInfo),Pascal,Raw,Name('GetSystemInfo')


    SA_SHBrowseForFolder(*SA_BROWSEINFO lpbi),Long,PASCAL,RAW,PROC,Name('SHBrowseForFolder')
    SA_SHGetPathFromIDList(Long pidl, |
                        *CSTRING pszPath    |
                       ),BOOL,PASCAL,RAW,PROC,Name('ShGetPathFromIDListA')

    SA_SHGetSpecialFolderLocation(SA_HWND hwndOwner, |
                               LONG nFolder,       |
                               *Long ppidl |
                              ),SA_HRESULT,PASCAL,RAW,PROC,Name('SHGetSpecialFolderLocation')

    SA_TrackMouseEvent(*SA__TRACKMOUSEEVENT lpEventTrack),BOOL,PASCAL,RAW,NAME('_TrackMouseEvent')
    SA_GetDlgItem(SA_HWND hDlg, Long nIDDlgItem),SA_HWND, Pascal, Name('GetDlgItem')

    !--- MetaFile functions
    SA_GetEnhMetaFile(*CSTRING szEnhMetaFilename),LONG,PASCAL,RAW,NAME('GetEnhMetaFileA')
    SA_GetMetaFile(*CSTRING szMetaFilename),LONG,PASCAL,RAW,NAME('GetMetaFileA')
    SA_GetMetaFileBitsEx(SA_HMETA hmf, ULONG nSize, <*? lpvData>),UNSIGNED,PASCAL,RAW,NAME('GetMetaFileBitsEx')
    SA_GetMetaFileBitsExAddr(SA_HMETA hmf, ULONG nSize, UNSIGNED lpvData=0),UNSIGNED,PASCAL,RAW,NAME('GetMetaFileBitsEx')

    SA_SetMetaFileBitsEx(UNSIGNED nSize, *? lpData),UNSIGNED,PASCAL,RAW,NAME('SetMetaFileBitsEx')
    SA_SetMetaFileBitsExAddr(UNSIGNED nSize, UNSIGNED lpData),UNSIGNED,PASCAL,RAW,NAME('SetMetaFileBitsEx')

    SA_PlayEnhMetaFile(SA_HDC hdc, SA_HMETA hemf, *SA_RECT lpRect),BOOL,PASCAL,RAW,NAME('PlayEnhMetaFile')
    SA_PlayMetaFile(SA_HDC hdc, SA_HMETA hmf),BOOL,PASCAL,PROC,NAME('PlayMetaFile')
    SA_SetWinMetaFileBits(ULONG cbBuffer, *? lpbBuffer, SA_HDC hdc, *SA_METAFILEPICT lpmfp),UNSIGNED,PASCAL,RAW,NAME('SetWinMetaFileBits')
    SA_DeleteEnhMetaFile(SA_HMETA hemf),BOOL,PASCAL,PROC,NAME('DeleteEnhMetaFile')
    SA_DeleteMetaFile(SA_HMETA hmf),BOOL,PASCAL,PROC,NAME('DeleteMetaFile')

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

    SA_Sleep(Long dwMilliseconds),Pascal, Name('Sleep')

    SA_ShellExecuteEx(*SA_SHELLEXECUTEINFO lpExecInfo),BOOL,Pascal,Name('ShellExecuteExA')

    END



