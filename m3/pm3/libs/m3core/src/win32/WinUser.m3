(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Thu Jun  1 10:18:09 PDT 1995 by kalsow   *)
(*      modified on Thu May 13 21:16:10 PDT 1993 by mjordan  *)
(*      modified on Fri Mar 19 11:14:26 PST 1993 by harrison *)

UNSAFE MODULE WinUser;

IMPORT WinBase;
FROM Ctypes IMPORT int;
FROM WinNT  IMPORT LPCSTR, LPCWSTR;
FROM WinDef IMPORT BOOL, DWORD, UINT, LPVOID, HWND, WPARAM, LPARAM,
                   LRESULT, HINSTANCE, HMENU, HHOOK, LPRECT, LPPOINT;

PROCEDURE ExitWindows (<*UNUSED*> dwReserved: UINT;
                       <*UNUSED*> Code: DWORD): BOOL =
  BEGIN
    RETURN ExitWindowsEx(EWX_LOGOFF, 16_FFFFFFFF);
  END ExitWindows;

PROCEDURE PostAppMessageA (idThread: DWORD;
                           wMsg    : UINT;
                           wParam  : WPARAM;
                           lParam  : LPARAM  ): BOOL =
  BEGIN
    RETURN PostThreadMessageA(idThread, wMsg, wParam, lParam);
  END PostAppMessageA;

PROCEDURE PostAppMessageW (idThread: DWORD;
                           wMsg    : UINT;
                           wParam  : WPARAM;
                           lParam  : LPARAM  ): BOOL =
  BEGIN
    RETURN PostThreadMessageW(idThread, wMsg, wParam, lParam);
  END PostAppMessageW;

PROCEDURE CreateWindowA (lpClassName : LPCSTR;
                         lpWindowName: LPCSTR;
                         dwStyle     : DWORD;
                         x           : int;
                         y           : int;
                         nWidth      : int;
                         nHeight     : int;
                         hwndParent  : HWND;
                         hMenu       : HMENU;
                         hInstance   : HINSTANCE;
                         lpParam     : LPVOID     ): HWND =
  BEGIN
    RETURN
      CreateWindowExA(0, lpClassName, lpWindowName, dwStyle, x, y, nWidth,
                      nHeight, hwndParent, hMenu, hInstance, lpParam);
  END CreateWindowA;

PROCEDURE CreateWindowW (lpClassName : LPCWSTR;
                         lpWindowName: LPCWSTR;
                         dwStyle     : DWORD;
                         x           : int;
                         y           : int;
                         nWidth      : int;
                         nHeight     : int;
                         hwndParent  : HWND;
                         hMenu       : HMENU;
                         hInstance   : HINSTANCE;
                         lpParam     : LPVOID     ): HWND =
  BEGIN
    RETURN
      CreateWindowExW(0, lpClassName, lpWindowName, dwStyle, x, y, nWidth,
                      nHeight, hwndParent, hMenu, hInstance, lpParam);
  END CreateWindowW;

PROCEDURE CreateDialogA (hInstance   : HINSTANCE;
                         lpName      : LPCSTR;
                         hwndParent  : HWND;
                         lpDialogFunc: DLGPROC    ): HWND =
  BEGIN
    RETURN
      CreateDialogParamA(hInstance, lpName, hwndParent, lpDialogFunc, 0);
  END CreateDialogA;

PROCEDURE CreateDialogW (hInstance   : HINSTANCE;
                         lpName      : LPCWSTR;
                         hwndParent  : HWND;
                         lpDialogFunc: DLGPROC    ): HWND =
  BEGIN
    RETURN
      CreateDialogParamW(hInstance, lpName, hwndParent, lpDialogFunc, 0);
  END CreateDialogW;

PROCEDURE CreateDialogIndirectA (hInstance   : HINSTANCE;
                                 lpTemplate  : LPCDLGTEMPLATEA;
                                 hwndParent  : HWND;
                                 lpDialogFunc: DLGPROC          ): HWND =
  BEGIN
    RETURN CreateDialogIndirectParamA(
             hInstance, lpTemplate, hwndParent, lpDialogFunc, 0);
  END CreateDialogIndirectA;

PROCEDURE CreateDialogIndirectW (hInstance   : HINSTANCE;
                                 lpTemplate  : LPCDLGTEMPLATEW;
                                 hwndParent  : HWND;
                                 lpDialogFunc: DLGPROC          ): HWND =
  BEGIN
    RETURN CreateDialogIndirectParamW(
             hInstance, lpTemplate, hwndParent, lpDialogFunc, 0);
  END CreateDialogIndirectW;

PROCEDURE DialogBoxA (hInstance     : HINSTANCE;
                      lpTemplateName: LPCSTR;
                      hWndParent    : HWND;
                      lpDialogFunc  : DLGPROC    ): int =
  BEGIN
    RETURN
      DialogBoxParamA(hInstance, lpTemplateName, hWndParent, lpDialogFunc, 0);
  END DialogBoxA;

PROCEDURE DialogBoxW (hInstance     : HINSTANCE;
                      lpTemplateName: LPCWSTR;
                      hWndParent    : HWND;
                      lpDialogFunc  : DLGPROC    ): int =
  BEGIN
    RETURN
      DialogBoxParamW(hInstance, lpTemplateName, hWndParent, lpDialogFunc, 0);
  END DialogBoxW;

PROCEDURE DialogBoxIndirectA (hInstance      : HINSTANCE;
                              hDialogTemplate: LPDLGTEMPLATEA;
                              hWndParent     : HWND;
                              lpDialogFunc   : DLGPROC         ): int =
  BEGIN
    RETURN DialogBoxIndirectParamA(
             hInstance, hDialogTemplate, hWndParent, lpDialogFunc, 0);
  END DialogBoxIndirectA;

PROCEDURE DialogBoxIndirectW (hInstance      : HINSTANCE;
                              hDialogTemplate: LPDLGTEMPLATEW;
                              hWndParent     : HWND;
                              lpDialogFunc   : DLGPROC         ): int =
  BEGIN
    RETURN DialogBoxIndirectParamW(
             hInstance, hDialogTemplate, hWndParent, lpDialogFunc, 0);
  END DialogBoxIndirectW;

PROCEDURE MessageBoxA (hWnd     : HWND;
                       lpText   : LPCSTR;
                       lpCaption: LPCSTR;
                       uType    : UINT    ): int =
  BEGIN
    RETURN MessageBoxExA(hWnd, lpText, lpCaption, uType, 0);
  END MessageBoxA;

PROCEDURE MessageBoxW (hWnd     : HWND;
                       lpText   : LPCWSTR;
                       lpCaption: LPCWSTR;
                       uType    : UINT    ): int =
  BEGIN
    RETURN MessageBoxExW(hWnd, lpText, lpCaption, uType, 0);
  END MessageBoxW;

PROCEDURE EnumTaskWindows (dwThreadId: DWORD;
                           lpfn      : WNDENUMPROC;
                           lParam    : LPARAM       ): BOOL =
  BEGIN
    RETURN EnumThreadWindows(dwThreadId, lpfn, lParam);
  END EnumTaskWindows;

PROCEDURE GetNextWindow (hWnd: HWND; uCmd: UINT): HWND =
  BEGIN
    RETURN GetWindow(hWnd, uCmd);
  END GetNextWindow;

PROCEDURE DefHookProc (nCode : int;
                       wParam: WPARAM;
                       lParam: LPARAM;
                       phhk  : UNTRACED REF HHOOK): LRESULT =
  BEGIN
    RETURN CallNextHookEx(phhk^, nCode, wParam, lParam);
  END DefHookProc;

(* hack to patch the buggy return values on Chicago *)

PROCEDURE GetClientRect (hWnd: HWND; lpRect: LPRECT): BOOL =
  VAR b := raw_GetClientRect (hWnd, lpRect);
  BEGIN
    IF is_chicago THEN b := ORD (b # 0); END;
    RETURN b;
  END GetClientRect;

PROCEDURE GetCursorPos (lpPoint: LPPOINT): BOOL =
  VAR b := raw_GetCursorPos (lpPoint);
  BEGIN
    IF is_chicago THEN b := ORD (b # 0); END;
    RETURN b;
  END GetCursorPos;

PROCEDURE ClientToScreen (hWnd: HWND; lpPoint: LPPOINT): BOOL =
  VAR b := raw_ClientToScreen (hWnd, lpPoint);
  BEGIN
    IF is_chicago THEN b := ORD (b # 0); END;
    RETURN b;
  END ClientToScreen;

PROCEDURE ScreenToClient (hWnd: HWND; lpPoint: LPPOINT): BOOL =
  VAR b := raw_ScreenToClient (hWnd, lpPoint);
  BEGIN
    IF is_chicago THEN b := ORD (b # 0); END;
    RETURN b;
  END ScreenToClient;

VAR
  os_version : WinBase.OSVERSIONINFO;
  is_chicago : BOOLEAN;
BEGIN
  os_version.dwOSVersionInfoSize := BYTESIZE (os_version);
  VAR b := WinBase.GetVersionEx (ADR (os_version)); BEGIN <*ASSERT b # 0*> END;
  is_chicago := (os_version.dwPlatformId = WinBase.VER_PLATFORM_WIN32_WINDOWS);

  RT_CURSOR := LOOPHOLE(1, ADDRESS);
  RT_BITMAP := LOOPHOLE(2, ADDRESS);
  RT_ICON := LOOPHOLE(3, ADDRESS);
  RT_MENU := LOOPHOLE(4, ADDRESS);
  RT_DIALOG := LOOPHOLE(5, ADDRESS);
  RT_STRING := LOOPHOLE(6, ADDRESS);
  RT_FONTDIR := LOOPHOLE(7, ADDRESS);
  RT_FONT := LOOPHOLE(8, ADDRESS);
  RT_ACCELERATOR := LOOPHOLE(9, ADDRESS);
  RT_RCDATA := LOOPHOLE(10, ADDRESS);
  RT_MESSAGETABLE := LOOPHOLE(11, ADDRESS);

  (* NOTE: if any new resource types are introduced above this point, then
     the ** value of DIFFERENCE must be changed.  ** (RT_GROUP_CURSOR -
     RT_CURSOR) must always be equal to DIFFERENCE ** (RT_GROUP_ICON -
     RT_ICON) must always be equal to DIFFERENCE *)
  RT_GROUP_CURSOR := LOOPHOLE(RT_CURSOR + DIFFERENCE, ADDRESS);
  (* The value RT_BITMAP+DIFFERENCE (13) is intentionally unused *)
  RT_GROUP_ICON := LOOPHOLE(RT_ICON + DIFFERENCE, ADDRESS);
  (* The value 15 is unused/obsolete *)
  RT_VERSION := LOOPHOLE(16, ADDRESS);
  RT_DLGINCLUDE := LOOPHOLE(17, ADDRESS);

  HWND_BROADCAST := LOOPHOLE(16_ffff, ADDRESS);

  HWND_DESKTOP := LOOPHOLE(0, ADDRESS);

  HWND_TOP := LOOPHOLE(0, ADDRESS);
  HWND_BOTTOM := LOOPHOLE(1, ADDRESS);
  HWND_TOPMOST := LOOPHOLE(-1, ADDRESS);
  HWND_NOTOPMOST := LOOPHOLE(-2, ADDRESS);

  IDC_ARROW := LOOPHOLE(32512, ADDRESS);
  IDC_IBEAM := LOOPHOLE(32513, ADDRESS);
  IDC_WAIT := LOOPHOLE(32514, ADDRESS);
  IDC_CROSS := LOOPHOLE(32515, ADDRESS);
  IDC_UPARROW := LOOPHOLE(32516, ADDRESS);
  IDC_SIZE := LOOPHOLE(32640, ADDRESS);
  IDC_ICON := LOOPHOLE(32641, ADDRESS);
  IDC_SIZENWSE := LOOPHOLE(32642, ADDRESS);
  IDC_SIZENESW := LOOPHOLE(32643, ADDRESS);
  IDC_SIZEWE := LOOPHOLE(32644, ADDRESS);
  IDC_SIZENS := LOOPHOLE(32645, ADDRESS);
  IDC_SIZEALL := LOOPHOLE(32646, ADDRESS); (* not in win3.1 *)
  IDC_NO := LOOPHOLE(32648, ADDRESS); (* not in win3.1 *)
  IDC_APPSTARTING := LOOPHOLE(32650, ADDRESS); (* not in win3.1 *)

  IDI_APPLICATION := LOOPHOLE(32512, ADDRESS);
  IDI_HAND := LOOPHOLE(32513, ADDRESS);
  IDI_QUESTION := LOOPHOLE(32514, ADDRESS);
  IDI_EXCLAMATION := LOOPHOLE(32515, ADDRESS);
  IDI_ASTERISK := LOOPHOLE(32516, ADDRESS);

  WC_DIALOG := LOOPHOLE(16_8002, ADDRESS);

END WinUser.
