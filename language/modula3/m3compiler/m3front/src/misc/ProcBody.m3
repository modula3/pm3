(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ProcBody.m3                                           *)
(* Last modified on Tue Dec 20 14:28:10 PST 1994 by kalsow     *)

MODULE ProcBody;

IMPORT Text;
IMPORT M3ID, CG, Host, Target, M3RT, Module;

REVEAL
  T = T_ BRANDED "ProcBody.T" OBJECT
    sibling  : T := NIL;
    children : T := NIL;
  END;

TYPE
  Delay = REF RECORD
    next       : Delay;
    offset     : INTEGER;
    src        : CG.Var;
    src_offset : INTEGER;
  END;

VAR
  cur   : T := NIL;
  head  : T := NIL;
  done  : T := NIL;
  depth : INTEGER := -1;
  delays: Delay := NIL;

PROCEDURE Push (t: T) =
  BEGIN
    <* ASSERT (t.parent = NIL) AND (t.sibling = NIL) AND (t.children = NIL) *>
    INC (depth);
    t.level  := depth;
    t.parent := cur;
    IF (cur = NIL) THEN
      (* depth = 0 *)
      t.sibling := head;
      head := t;
    ELSE
      t.sibling := cur.children;
      cur.children := t;
    END;
    cur := t;
  END Push;

PROCEDURE Pop () =
  BEGIN
    cur := cur.parent;
    DEC (depth);
  END Pop;

PROCEDURE Schedule (t: T) =
  BEGIN
    t.sibling := head;
    head := t;
  END Schedule;

PROCEDURE DelayedInit (offset: INTEGER;  src: CG.Var;  src_offset: INTEGER) =
  BEGIN
    delays := NEW (Delay, next := delays, offset := offset,
                     src := src, src_offset := src_offset);
  END DelayedInit;

PROCEDURE EmitAll (VAR proc_info: INTEGER;  VAR link_proc: CG.Proc) =
  VAR
    t         : T;
    base      : INTEGER := 0;
    n_base    : INTEGER;
    n, total  : INTEGER;
    globals   : CG.Var := Module.GlobalData (NIL);
    delay     : Delay;
    link_name : TEXT;
  BEGIN
    proc_info := 0;
    link_proc := NIL;

    (* generate the declarations and bodies *)
    WHILE (head # NIL) DO
      t := head;  head := NIL;  (* grab the guys that are waiting *)
      t := SourceOrder (t);     (* put'em in souce order *)
      EmitDecl (t);             (* generate their declarations *)
      EmitBody (t);             (* generate their bodies & build "done" list *)
    END;

    (* count the linker registrations *)
    t := done;  n := 0;
    WHILE (t # NIL) DO
      IF (t.cg_proc # NIL) AND ((t.name # NIL) OR (t.export_var # NIL)) THEN
        INC (n);
      END;
      t := t.sibling;
    END;

    IF (n > 0) THEN
      (* compute the total lengths of the procedure names *)
      t := done;  total := 0;
      WHILE (t # NIL) DO
        IF (t.cg_proc # NIL) AND (t.name # NIL) THEN
          INC (total, Text.Length (t.name) + 1);
        END;
        t := t.sibling;
      END;

      (* allocate the space we need for names *)
      total := total * Target.Char.size;
      n_base := Module.Allocate (total, Target.Address.align, "*proc names*");
      CG.Comment (n_base, "procedure names");

      (* allocate the space we need for proc info headers *)
      n := n * M3RT.PI_SIZE + Target.Address.size;
      base := Module.Allocate (n, Target.Address.align, "*proc info*");
      CG.Comment (base, "procedure table");
      proc_info := base;

      (* generate the procedure names *)
      t := done;  total := 0;
      WHILE (t # NIL) DO
        IF (t.cg_proc # NIL) AND (t.name # NIL) THEN
          CG.Init_chars (n_base + total, t.name);
          INC (total, Target.Char.size * (Text.Length (t.name) + 1));
        END;
        t := t.sibling;
      END;

      (* generate the linker registrations *)
      t := done;  total := 0;  n := proc_info;
      WHILE (t # NIL) DO
        IF (t.cg_proc # NIL) THEN
          IF (t.name # NIL) OR (t.export_var # NIL) THEN
            CG.Init_proc (n + M3RT.PI_proc, t.cg_proc);
          END;
          IF (t.name # NIL) THEN
            CG.Init_var (n + M3RT.PI_name, globals, n_base + total);
            INC (total, Target.Char.size * (Text.Length (t.name) + 1));
          END;
          IF (t.export_var # NIL) THEN
            CG.Init_var (n + M3RT.PI_export, t.export_var, t.export_offs);
          END;
          INC (n, M3RT.PI_SIZE);
        END;
        t := t.sibling;
      END;

    END;

    (* generate the link proc *)
    IF (delays # NIL) THEN
      link_name := "_LINK" & Module.Prefix (NIL);
      CG.Comment (-1, link_name);
      link_proc := CG.Declare_procedure (M3ID.Add (link_name), 0,
                    CG.Type.Void, lev := 0, cc := Target.DefaultCall,
                    exported := FALSE, parent := NIL);
      CG.Begin_procedure (link_proc);

      delay := delays;
      WHILE (delay # NIL) DO
        CG.Load_addr (delay.src, delay.src_offset);
        CG.Store_addr (globals, delay.offset);
        delay := delay.next;
      END;
      delays := NIL;

      CG.Exit_proc (CG.Type.Void);
      CG.End_procedure (link_proc);
    END;

  END EmitAll;

PROCEDURE SourceOrder (t: T): T =
  VAR a, b, c: T;
  BEGIN
    (* reverse the list *)
    a := t;  b := NIL;
    WHILE (a # NIL) DO
      c := a.sibling;
      a.sibling := b;
      b := a;
      a := c;
    END;
    t := b;

    (* recursively reorder the children *)
    WHILE (t # NIL) DO
      t.children := SourceOrder (t.children);
      t := t.sibling;
    END;
    
    RETURN b;
  END SourceOrder;

PROCEDURE EmitDecl (t: T) =
  BEGIN
    WHILE (t # NIL) DO
      t.gen_decl ();
      EmitDecl (t.children);
      t := t.sibling;
    END;
  END EmitDecl;

PROCEDURE EmitBody (t: T) =
  VAR a: T;
  BEGIN
    WHILE (t # NIL) DO
      IF (Host.nested_procs_first) THEN
        EmitBody (t.children);
        IF (t.name # NIL) THEN CG.Comment (-1, t.name) END;
        t.gen_body ();
      ELSE
        IF (t.name # NIL) THEN CG.Comment (-1, t.name) END;
        t.gen_body ();
        EmitBody (t.children);
      END;

      (* move to the next sibling, but leave this guy on the "done" list *)
      a := t.sibling;
      t.sibling := done;  done := t;
      t := a;
    END;
  END EmitBody;

PROCEDURE Reset () =
  BEGIN
    cur    := NIL;
    head   := NIL;
    done   := NIL;
    depth  := -1;
    delays := NIL;
  END Reset;

BEGIN
END ProcBody.
