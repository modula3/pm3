(*| Copyright (C) 1990, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Fri Nov 18 15:44:19 PST 1994 by kalsow     *)
(*|      modified on Sun Feb 21 14:15:00 PST 1993 by jdd        *)
(*|      modified on Wed Jan 27 22:27:27 PST 1993 by mjordan    *)

(* "RTOS" is a private interface that provides the low-level,
   OS-specific memory allocation and shutdown routines. *)

INTERFACE RTOS;


PROCEDURE Exit (n: INTEGER);
(* Terminate current process with return code "n". *)

PROCEDURE Crash ();
(* Terminate current process with a crash *)

PROCEDURE GetMemory (size: INTEGER): ADDRESS;
(* Return the address of "size" bytes of unused storage *)

PROCEDURE LockHeap ();
(* Enters an allocator/collector critical section; the same thread may
   enter the critical section multiple times.

   It could be written at user level as:

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

PROCEDURE UnlockHeap ();
(* Leaves the critical section.

   It could be written at user level as:

| PROCEDURE UnlockHeap () =
|   BEGIN
|     LOCK mutex DO DEC(count); END;
|     IF count = 0 THEN Thread.Signal(condition); END;
|   END UnlockHeap;

   However, it must be possible to call it from anywhere inside the
   collector. *)

PROCEDURE BroadcastHeap ();
(* Restarts all threads that called "WaitHeap" sometime after the
   allocator/collector critical section is released.  The caller
   must already be in the critical section. *)

PROCEDURE WaitHeap ();
(* Blocks the caller until "BroadcastHeap" has been called and
   the allocator/collector critical section is released.   The
   caller must not be in the critical section. *)

PROCEDURE Write (a: ADDRESS;  n: INTEGER);
(* Write the "n" bytes beginning at address "a" to the standard
   error output file or console. *)

END RTOS.
