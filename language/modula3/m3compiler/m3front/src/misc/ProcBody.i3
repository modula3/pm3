(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ProcBody.i3                                           *)
(* Last modified on Thu Jul 14 09:10:35 PDT 1994 by kalsow     *)

INTERFACE ProcBody;

(* This interface manages the order that procedure bodies are emitted. *)

IMPORT CG;

TYPE
  T <: T_;
  T_ = OBJECT
    parent     : T       := NIL; (* set by Push *)
    level      : INTEGER := 0;   (* set by Push *)
    name       : TEXT    := NIL; (* set by client *)
    cg_proc    : CG.Proc := NIL; (* set by client *)
    export_var : CG.Var  := NIL; (* set by client *)
    export_offs: INTEGER := 0;   (* set by client *)
  METHODS
    gen_decl ();
    gen_body ();
  END;

PROCEDURE Push (t: T);
(* pushes the procedure that will be written as a child of the current
   procedure.  *)

PROCEDURE Pop ();
(* pops the current procedure *)

PROCEDURE Schedule (t: T);
(* schedules "t" to be written as a top-level procedure *)

PROCEDURE DelayedInit (offset: INTEGER;  src: CG.Var;  src_offset: INTEGER);
(* generate the runtime code to initialize the pointer at "offset" with
   the value at "src+src_offset". *)

PROCEDURE EmitAll (VAR proc_info: INTEGER;  VAR link_proc: CG.Proc);
(* generate all the procedure bodies, build the global table,
   and generate the link proc if it's needed *)

PROCEDURE Reset ();

END ProcBody.
