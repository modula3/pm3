(* Copyright (C) 1996, Critcal Mass, Inc.  All rights reserved.   *)
(*                                                                *)
(* derived from Microsoft's TLHELP32.H version 1.0 by Bill Kalsow *)

(******************************************************************
*                                                                 *
* tlhelp32.h - WIN32 tool help functions, types, and definitions  *
*                                                                 *
*******************************************************************)

INTERFACE TlHelp32;

FROM Ctypes IMPORT char;
FROM WinDef IMPORT HANDLE, DWORD, BOOL, HMODULE, LONG, LPVOID,
                   LPCVOID, LPDWORD, LPBYTE, MAX_PATH;

CONST
  MAX_MODULE_NAME32 = 255;

(****** Shapshot function **********************************************)

<*EXTERNAL CreateToolhelp32Snapshot:WINAPI*>
PROCEDURE CreateToolhelp32Snapshot (dwFlags, th32ProcessID: DWORD): HANDLE;
(*
// The th32ProcessID argument is only used if TH32CS_SNAPHEAPLIST or
// TH32CS_SNAPMODULE is specified. th32ProcessID == 0 means the current
// process.
//
// NOTE that all of the snapshots are global except for the heap and module
//      lists which are process specific. To enumerate the heap or module
//      state for all WIN32 processes call with TH32CS_SNAPALL and the
//      current process. Then for each process in the TH32CS_SNAPPROCESS
//      list that isn't the current process, do a call with just
//      TH32CS_SNAPHEAPLIST and/or TH32CS_SNAPMODULE.
*)

CONST (* dwFlags for create snapshot *)
  TH32CS_SNAPHEAPLIST = 16_00000001;
  TH32CS_SNAPPROCESS  = 16_00000002;
  TH32CS_SNAPTHREAD   = 16_00000004;
  TH32CS_SNAPMODULE   = 16_00000008;
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST + TH32CS_SNAPPROCESS
                        + TH32CS_SNAPTHREAD + TH32CS_SNAPMODULE;
  TH32CS_INHERIT      = 16_80000000;

(*
// Use CloseHandle to destroy the snapshot
*)

(****** heap walking ***************************************************)

TYPE
  PHEAPLIST32 = UNTRACED REF HEAPLIST32;
  LPHEAPLIST32 = UNTRACED REF HEAPLIST32;
  HEAPLIST32 = RECORD
    dwSize        : DWORD;
    th32ProcessID : DWORD;  (* owning process *)
    th32HeapID    : DWORD;  (* heap (in owning process's context!) *)
    dwFlags       : DWORD;
  END;

CONST (* dwFlags for HEAPLIST32 *)
  HF32_DEFAULT = 1;  (* process's default heap *)
  HF32_SHARED  = 2;  (* is shared heap *)

<*EXTERNAL Heap32ListFirst:WINAPI*>
PROCEDURE Heap32ListFirst (hSnapshot: HANDLE;  lphl: LPHEAPLIST32): BOOL;

<*EXTERNAL Heap32ListNext:WINAPI*>
PROCEDURE Heap32ListNext (hSnapshot: HANDLE;  lphl: LPHEAPLIST32): BOOL;

TYPE
  PHEAPENTRY32 = UNTRACED REF HEAPENTRY32;
  LPHEAPENTRY32 = UNTRACED REF HEAPENTRY32;
  HEAPENTRY32 = RECORD
    dwSize        : DWORD;
    hHandle       : HANDLE; (* Handle of this heap block *)
    dwAddress     : DWORD;  (* Linear address of start of block *)
    dwBlockSize   : DWORD;  (* Size of block in bytes *)
    dwFlags       : DWORD;
    dwLockCount   : DWORD;
    dwResvd       : DWORD;
    th32ProcessID : DWORD;  (* owning process *)
    th32HeapID    : DWORD;  (* heap block is in *)
  END;

CONST (* dwFlags for HEAPENTRY32 *)
  LF32_FIXED    = 16_00000001;
  LF32_FREE     = 16_00000002;
  LF32_MOVEABLE = 16_00000004;


<*EXTERNAL Heap32First:WINAPI*>
PROCEDURE Heap32First (lphe          : LPHEAPENTRY32;
                       th32ProcessID : DWORD;
                       th32HeapID    : DWORD): BOOL;

<*EXTERNAL Heap32Next:WINAPI*>
PROCEDURE Heap32Next (lphe: LPHEAPENTRY32): BOOL;

<*EXTERNAL Toolhelp32ReadProcessMemory:WINAPI*>
PROCEDURE Toolhelp32ReadProcessMemory (th32ProcessID       : DWORD;
                                       lpBaseAddress       : LPCVOID;
                                       lpBuffer            : LPVOID;
                                       cbRead              : DWORD;
                                       lpNumberOfBytesRead : LPDWORD): BOOL;

(****** Process walking *************************************************)

TYPE
  PPROCESSENTRY32 = UNTRACED REF PROCESSENTRY32;
  LPPROCESSENTRY32 = UNTRACED REF PROCESSENTRY32;
  PROCESSENTRY32 = RECORD
    dwSize              : DWORD;
    cntUsage            : DWORD;
    th32ProcessID       : DWORD;  (* this process *)
    th32DefaultHeapID   : DWORD;
    th32ModuleID        : DWORD;  (* associated exe *)
    cntThreads          : DWORD;
    th32ParentProcessID : DWORD;  (* this process's parent process *)
    pcPriClassBase      : LONG;   (* Base priority of process's threads *)
    dwFlags             : DWORD;
    szExeFile           : ARRAY [0..MAX_PATH-1] OF char;    (* Path *)
  END;

<*EXTERNAL Process32First:WINAPI*>
PROCEDURE Process32First (hSnapshot: HANDLE;  lppe: LPPROCESSENTRY32): BOOL;

<*EXTERNAL Process32Next:WINAPI*>
PROCEDURE Process32Next (hSnapshot: HANDLE;  lppe: LPPROCESSENTRY32): BOOL;

(***** Thread walking **************************************************)

TYPE
  PTHREADENTRY32 = UNTRACED REF THREADENTRY32;
  LPTHREADENTRY32 = UNTRACED REF THREADENTRY32;
  THREADENTRY32 = RECORD
    dwSize             : DWORD;
    cntUsage           : DWORD;
    th32ThreadID       : DWORD; (* this thread *)
    th32OwnerProcessID : DWORD; (* Process this thread is associated with *)
    tpBasePri          : LONG;
    tpDeltaPri         : LONG;
    dwFlags            : DWORD;
  END;

<*EXTERNAL Thread32First:WINAPI*>
PROCEDURE Thread32First (hSnapshot: HANDLE;  lpte: LPTHREADENTRY32): BOOL;

<*EXTERNAL Thread32Next:WINAPI*>
PROCEDURE Thread32Next (hSnapshot: HANDLE;  lpte: LPTHREADENTRY32): BOOL;

(***** Module walking *************************************************)

TYPE
  PMODULEENTRY32 = UNTRACED REF MODULEENTRY32;
  LPMODULEENTRY32 = UNTRACED REF MODULEENTRY32;
  MODULEENTRY32 = RECORD
    dwSize        : DWORD;
    th32ModuleID  : DWORD;   (* This module *)
    th32ProcessID : DWORD;   (* owning process *)
    GlblcntUsage  : DWORD;   (* Global usage count on the module *)
    ProccntUsage  : DWORD;   (* Module usage count in th32ProcessID's context *)
    modBaseAddr   : LPBYTE;  (* Base address of module in th32ProcessID's context *)
    modBaseSize   : DWORD;   (* Size in bytes of module starting at modBaseAddr *)
    hModule       : HMODULE; (* The hModule of this module in th32ProcessID's context *)
    szModule      : ARRAY [0..MAX_MODULE_NAME32] OF char;
    szExePath     : ARRAY [0..MAX_PATH-1] OF char;
  END;

(*
// NOTE CAREFULLY that the modBaseAddr and hModule fields are valid ONLY
// in th32ProcessID's process context.
*)

<*EXTERNAL Module32First:WINAPI*>
PROCEDURE Module32First (hSnapshot: HANDLE;  lpme: LPMODULEENTRY32): BOOL;

<*EXTERNAL Module32Next:WINAPI*>
PROCEDURE Module32Next (hSnapshot: HANDLE;  lpme: LPMODULEENTRY32): BOOL;

END TlHelp32.
