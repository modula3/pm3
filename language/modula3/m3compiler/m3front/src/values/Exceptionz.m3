(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Exceptionz.m3                                         *)
(* Last Modified On Tue May 23 15:38:44 PDT 1995 by kalsow     *)
(*      Modified On Thu Dec  5 17:20:35 PST 1991 by muller     *)

MODULE Exceptionz;

IMPORT M3, M3ID, CG, Value, ValueRep, Type, Scope, Formal;
IMPORT Error, Expr, Token, Decl, Text, Procedure, AssignStmt;
IMPORT Target, RefType, ProcBody, Module, Runtime, Marker, M3Buf;
FROM Scanner IMPORT GetToken, Match, MatchID, cur;

TYPE
  T = Value.T BRANDED "Exceptionz.T" OBJECT
	tipe    : Type.T;
        refTipe : Type.T;
        offset  : INTEGER;
        next    : T;
      OVERRIDES
        typeCheck   := Check;
        set_globals := SetGlobals;
        load        := Load;
        declare     := Declarer;
        const_init  := ValueRep.NoInit;
        need_init   := ValueRep.Never;
        lang_init   := ValueRep.NoInit;
        user_init   := ValueRep.NoInit;
	toExpr      := ValueRep.NoExpr;
	toType      := ValueRep.NoType;
        typeOf      := ValueRep.TypeVoid;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := FPType;
      END;

TYPE
  Raiser = ProcBody.T OBJECT
    self   : T;
    arg    : CG.Var;
  OVERRIDES
    gen_decl := EmitDecl;
    gen_body := EmitBody;
  END;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes) =
  TYPE TK = Token.T;
  VAR t: T; id: M3ID.T;
  BEGIN
    IF att.isInline   THEN Error.Msg ("an exception cannot be inline"); END;
    IF att.isExternal THEN
      Error.Msg ("an exception cannot be external");
    ELSIF att.callingConv # NIL THEN
      Error.Msg ("an exception does not have a calling convention");
    END;

    Match (TK.tEXCEPTION);
    WHILE (cur.token = TK.tIDENT) DO
      id := MatchID ();
      t := NEW (T);
      ValueRep.Init (t, id, Value.Class.Exception);
      t.readonly := TRUE;
      t.unused   := att.isUnused;
      t.obsolete := att.isObsolete;
      t.tipe     := NIL;
      t.refTipe  := NIL;
      t.offset   := 0;
      IF (cur.token = TK.tLPAREN) THEN
        GetToken (); (* ( *)
        t.tipe := Type.Parse ();
        Match (TK.tRPAREN);
      END;
      Scope.Insert (t);
      Match (TK.tSEMI);
    END;
  END ParseDecl;

PROCEDURE EmitRaise (v: Value.T;  arg: Expr.T) =
  VAR
    t: T := Value.Base (v);
    expr_type: Type.T;
    arg_type: CG.Type;
    info: Type.Info;
    p := Runtime.LookUpProc (Runtime.Hook.Raise);
  BEGIN
    Value.Declare (t);
    <*ASSERT t.offset # 0*>

    IF (arg = NIL) THEN
      Procedure.StartCall (p);
      IF Target.DefaultCall.args_left_to_right THEN
        CG.Load_addr_of (Scope.ToUnit (t), t.offset, CG.Max_alignment);
        CG.Pop_param (CG.Type.Addr);
        CG.Load_nil ();
        CG.Pop_param (CG.Type.Addr);
      ELSE
        CG.Load_nil ();
        CG.Pop_param (CG.Type.Addr);
        CG.Load_addr_of (Scope.ToUnit (t), t.offset, CG.Max_alignment);
        CG.Pop_param (CG.Type.Addr);
      END;
      EVAL Procedure.EmitCall (p);
    ELSIF (t.refTipe = NIL) THEN
      expr_type := Expr.TypeOf (arg);
      expr_type := Type.CheckInfo (expr_type, info);
      arg_type := info.stk_type;
      Expr.Prep (arg);
      Procedure.StartCall (p);
      IF Target.DefaultCall.args_left_to_right THEN
        CG.Load_addr_of (Scope.ToUnit (t), t.offset, CG.Max_alignment);
        CG.Pop_param (CG.Type.Addr);
        AssignStmt.EmitCheck (t.tipe, arg);
        IF (arg_type # CG.Type.Addr) THEN
          CG.Loophole (arg_type, CG.Type.Addr);
        END;
        CG.Pop_param (CG.Type.Addr);
      ELSE
        AssignStmt.EmitCheck (t.tipe, arg);
        IF (arg_type # CG.Type.Addr) THEN
          CG.Loophole (arg_type, CG.Type.Addr);
        END;
        CG.Pop_param (CG.Type.Addr);
        CG.Load_addr_of (Scope.ToUnit (t), t.offset, CG.Max_alignment);
        CG.Pop_param (CG.Type.Addr);
      END;
      EVAL Procedure.EmitCall (p);
    ELSE (* large argument => call the raise procedure *)
      expr_type := Expr.TypeOf (arg);
      expr_type := Type.CheckInfo (expr_type, info);
      arg_type := info.stk_type;
      Expr.Prep (arg);
      CG.Start_call_indirect (CG.Type.Void, Target.DefaultCall);
      AssignStmt.EmitCheck (t.tipe, arg);
      IF (arg_type # CG.Type.Addr) THEN Formal.GenCopy (expr_type) END;
      CG.Pop_param (CG.Type.Addr);
      CG.Load_addr (Scope.ToUnit (t), t.offset + Target.Address.pack);
      CG.Call_indirect (CG.Type.Void, Target.DefaultCall);
      Marker.EmitExceptionTest (Procedure.Signature (p));
    END;
  END EmitRaise;

PROCEDURE ArgByReference (type: Type.T): BOOLEAN =
  VAR info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (type, info);
    RETURN (info.size > Target.Address.size) OR Type.IsStructured (type);
  END ArgByReference;

PROCEDURE Check (t: T;  <*UNUSED*> VAR cs: Value.CheckState) =
  VAR info: Type.Info;
  BEGIN
    IF (t.tipe # NIL) THEN
      t.tipe := Type.CheckInfo (t.tipe, info);
      IF (info.size < 0) THEN
        Error.ID (t.name, "argument type must have fixed length");
      END;
      IF ArgByReference (t.tipe) THEN
        t.refTipe := RefType.New (t.tipe, TRUE, NIL);
        t.refTipe := Type.Check (t.refTipe);
      END;
    END;
  END Check;

PROCEDURE ArgType (v: Value.T): Type.T =
  BEGIN
    TYPECASE Value.Base (v) OF
    | NULL => RETURN NIL;
    | T(t) => RETURN t.tipe;
    ELSE      RETURN NIL;
    END;
  END ArgType;

PROCEDURE CGOffset (v: Value.T): INTEGER =
  BEGIN
    TYPECASE Value.Base (v) OF
    | NULL => RETURN 0;
    | T(t) => RETURN t.offset;
    ELSE      RETURN 0;
    END;
  END CGOffset;

PROCEDURE Load (t: T) =
  BEGIN
    Value.Declare (t);
    CG.Load_addr_of (Scope.ToUnit (t), t.offset, CG.Max_alignment);
  END Load;

PROCEDURE SetGlobals (t: T) =
  VAR name: TEXT;  size: INTEGER;
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    (* Type.SetGlobals (t.refTipe); *)
    IF (t.offset # 0) OR (t.external) THEN RETURN END;

    name := Value.GlobalName (t, dots := TRUE, with_module := TRUE);
    size := Target.Address.size;
    IF (t.refTipe # NIL) THEN INC (size, Target.Address.size) END;
    INC (size, (Text.Length (name) + 1) * Target.Char.size);
    t.offset := Module.Allocate (size, Target.Address.align, name);
  END SetGlobals;

PROCEDURE Declarer (t: T): BOOLEAN =
  VAR name: TEXT;  unit: CG.Var;  r: Raiser;  arg_type: CG.TypeUID := 0;
  BEGIN
    Type.Compile (t.tipe);
    Type.Compile (t.refTipe);
    IF (t.tipe # NIL) THEN arg_type := Type.GlobalUID (t.tipe) END;

    IF t.external OR t.imported THEN
      (* force the import *)
      EVAL Scope.ToUnit (t);
    ELSE (* locally declared *)
      SetGlobals (t);
      name := Value.GlobalName (t, dots := TRUE, with_module := TRUE);
      unit := Module.GlobalData (NIL);
      CG.Declare_exception (t.name, arg_type, (t.refTipe#NIL), unit, t.offset);
      CG.Declare_global_field (t.name, t.offset, Target.Address.size, 0);
      IF (t.refTipe = NIL) THEN
        CG.Init_var   (t.offset, unit, t.offset + Target.Address.size);
        CG.Init_chars (t.offset + Target.Address.size, name);
      ELSE
        r := NEW (Raiser, self := t);
        r.name := Value.GlobalName (t, dots := FALSE, with_module := TRUE);
        r.name := r.name & "_RAISE";
        r.cg_proc := CG.Declare_procedure (M3ID.Add (r.name), 1, CG.Type.Void,
                                           lev := 0, cc := Target.DefaultCall,
                                           exported := FALSE, parent := NIL);
        r.arg := CG.Declare_param (M3ID.NoID, Target.Address.size,
                       Target.Address.align, CG.Type.Addr,
                       CG.Declare_indirect (Type.GlobalUID (t.tipe)),
                       in_memory := FALSE, up_level := FALSE, f := CG.Always);
        CG.Init_var (t.offset, unit, t.offset + 2 * Target.Address.size);
        CG.Init_proc (t.offset + Target.Address.size, r.cg_proc);
        CG.Init_chars (t.offset + 2 * Target.Address.size, name);
        ProcBody.Schedule (r);
      END;
    END;
    RETURN TRUE;
  END Declarer;

PROCEDURE EmitDecl (<*UNUSED*> x: Raiser) =
  BEGIN
  END EmitDecl;

PROCEDURE EmitBody (x: Raiser) =
  VAR
    t := x.self;
    arg := x.arg;
    ptr: CG.Val;
    align: INTEGER;
    proc: Procedure.T;
    info: Type.Info;
  BEGIN
    CG.Gen_location (t.origin);
    CG.Begin_procedure (x.cg_proc);
    EVAL Type.CheckInfo (t.tipe, info);
    align := info.alignment;

    (* ptr := NEW (REF t.type) *)
    proc := Runtime.LookUpProc (Runtime.Hook.NewTracedRef);
    Procedure.StartCall (proc);
    Type.LoadInfo (t.refTipe, -1);
    CG.Pop_param (CG.Type.Addr);
    ptr := Procedure.EmitCall (proc);

    (* ptr^ := arg^ *)
    CG.Push (ptr);
    CG.Boost_alignment (align);
    CG.Load_addr (arg);
    CG.Boost_alignment (align);
    CG.Copy (info.size, overlap := FALSE);

    (* RAISE (e, ptr) *)
    proc := Runtime.LookUpProc (Runtime.Hook.Raise);
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      CG.Load_addr_of (Scope.ToUnit (t), t.offset, CG.Max_alignment);
      CG.Pop_param (CG.Type.Addr);
      CG.Push (ptr);
      CG.Pop_param (CG.Type.Addr);
    ELSE
      CG.Push (ptr);
      CG.Pop_param (CG.Type.Addr);
      CG.Load_addr_of (Scope.ToUnit (t), t.offset, CG.Max_alignment);
      CG.Pop_param (CG.Type.Addr);
    END;
    EVAL Procedure.EmitCall (proc);

    CG.Free (ptr);
    CG.End_procedure (x.cg_proc);
  END EmitBody;

PROCEDURE AddFPSetTag (tt: Value.T;  VAR x: M3.FPInfo): CARDINAL =
  (* called for RAISES sets, doesn't include the interface record offset *)
  VAR t: T := Value.Base (tt);
  BEGIN
    M3Buf.PutChar (x.buf, '<');
    ValueRep.FPStart (t, x, "EXCEPT ", 0, global := TRUE);
    M3Buf.PutChar (x.buf, '>');
    RETURN ORD (t.tipe # NIL);
  END AddFPSetTag;

PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  VAR offset := t.offset DIV Target.Address.align;
  BEGIN
    ValueRep.FPStart (t, x, "EXCEPT ", offset, global := TRUE);
    RETURN ORD (t.tipe # NIL);
  END AddFPTag;

PROCEDURE FPType (t: T): Type.T =
  BEGIN
    RETURN t.tipe;
  END FPType;

BEGIN
END Exceptionz.
