(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Runtime.i3                                            *)
(* Last Modified On Mon Jul 25 15:22:56 PDT 1994 By kalsow     *)

(* This interface provides handle on the runtime procedures
   that the compiler calls directly. *)

INTERFACE Runtime;

IMPORT M3ID, Module, Procedure;

TYPE
  Hook = {
    Raise, ResumeRaise, PushEFrame, PopEFrame,
    Lock, Unlock,
    Concat,
    NewTracedRef, NewTracedArray,
    NewUntracedObj, NewUntracedRef, NewUntracedArray,
    DisposeRef, DisposeObj,
    AssertFault, ReturnFault, CaseFault, TypecaseFault, RangeFault,
    SubscriptFault, NarrowFault, NilFault, ShapeFault
  };

PROCEDURE Import ();
(* Import the standard interfaces containing runtime hooks. *)

PROCEDURE Bind (dest: Module.T;  VAR runtime: Module.T;  VAR id: M3ID.T);
(* bind the runtime interface as an import of 'dest' *)

PROCEDURE LookUpProc (h: Hook): Procedure.T;
(* return a handle on the procedure that implements hook 'h' *)

PROCEDURE Reset ();

END Runtime.
