(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Tue Nov 29 14:41:33 PST 1994 by kalsow     *)
(*      modified on Mon Feb 15 16:17:00 PST 1993 by mjordan    *)

UNSAFE MODULE RTOS;

IMPORT WinBase, WinNT, WinCon, WinDef;

(*--------------------------------------------------- process termination ---*)

PROCEDURE Exit (n: INTEGER) =
  BEGIN
    WinBase.ExitProcess (n);
  END Exit;

PROCEDURE Crash () =
  VAR ptr := LOOPHOLE(-99, UNTRACED REF INTEGER);
  BEGIN
    ptr^ := 99; (* try to get to the debugger... *)
    WinBase.FatalExit (-1);
  END Crash;

(*------------------------------------------------------------- allocator ---*)

PROCEDURE GetMemory (size: INTEGER): ADDRESS =
  (* Return the address of "size" bytes of unused storage *)
  BEGIN
    RETURN LOOPHOLE(WinBase.LocalAlloc(WinBase.LMEM_FIXED, size), ADDRESS);
  END GetMemory;

(*------------------------------------------------------------- collector ---*)
(* These procedures provide synchronization primitives for the allocator
   and collector.  This is the Windows/NT version, and depends on the NT
   thread implementation. *)

(* LockHeap() enters a critical section; the same thread may enter the
   critical section multiple times.  It could be written at user level
   as:

| VAR
|   mutex    : MUTEX            := NEW(MUTEX);
|   condition: Thread.Condition := NEW(Thread.Condition);
|   thread   : Thread.T         := NIL;
|   count    : CARDINAL         := 0;

| PROCEDURE LockHeap () =
|   BEGIN
|     LOCK mutex DO
|       IF count = 0 THEN
|         thread := Thread.Self();
|         INC(count);
|       ELSIF thread = Thread.Self() THEN
|         INC(count);
|       ELSE
|         Thread.Wait(mutex, condition);
|       END;
|     END;
|   END LockHeap;

   However, it must be possible to call it from anywhere in the
   collector. *)

VAR
  cs: WinBase.LPCRITICAL_SECTION := NIL;
  csstorage: WinNT.RTL_CRITICAL_SECTION;

PROCEDURE LockHeap () =
  BEGIN
    IF (cs = NIL) THEN
      cs := ADR(csstorage);
      WinBase.InitializeCriticalSection(cs);
    END;
    WinBase.EnterCriticalSection(cs);
  END LockHeap;

(* UnlockHeap() leaves the critical section.  It could be written at user
   level as:

| PROCEDURE UnlockHeap () =
|   BEGIN
|     LOCK mutex DO DEC(count); END;
|     IF count = 0 THEN Thread.Signal(condition); END;
|   END UnlockHeap;

   However, it must be possible to call it from anywhere inside the
   collector. *)

PROCEDURE UnlockHeap () =
  BEGIN
    WinBase.LeaveCriticalSection(cs);
  END UnlockHeap;

(*------------------------------------------------------------------- I/O ---*)

VAR
  ready  := FALSE;
  stderr : WinDef.HANDLE;
  (* Perhaps we should explicitly open CONERR$ et al,
     in case of redirection by parent? *)

PROCEDURE Write (a: ADDRESS;  n: INTEGER) =
  VAR nWritten: INTEGER;
  BEGIN
    IF NOT ready THEN
      EVAL WinCon.AllocConsole(); (* make sure we've got one! *)
      stderr := WinBase.GetStdHandle(WinBase.STD_ERROR_HANDLE);
      ready := TRUE;
    END;
    EVAL WinBase.WriteFile(stderr, a, n, ADR(nWritten), NIL);
  END Write;

BEGIN
END RTOS.



