(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu May 23 10:19:48 PDT 1996 by heydon     *)
(*      modified on Tue Feb 28 15:59:51 PST 1995 by kalsow     *)

MODULE QMachine EXPORTS QMachine, QMachRep;

IMPORT Atom, AtomList, IntRefTbl, Env, Fmt, Text, FileWr;
IMPORT Wr, Thread, OSError, TextSeq, TextF, Pipe;
IMPORT Pathname, Process, File, FS, FileRd, Rd;
IMPORT M3ID, M3Buf, M3File, RegularFile;
IMPORT QValue, QVal, QCode, QCompiler, QVTbl, QVSeq, QScanner, Stdio;
FROM Quake IMPORT Error;

TYPE
  QK = QValue.Kind;
  Op = QCode.Op;

REVEAL
  T = Rep BRANDED "QMachine.T" OBJECT
  OVERRIDES	
    init     := Init;
    evaluate := Evaluate;
    get      := Get;
    put      := Put;
    lookup   := LookUp;
    push     := Push;
    pop      := Pop;
    error    := Err;
    cleanup  := CleanUp;
    call     := Call;
    cur_wr   := CurWr;
  END;

(*-------------------------------------------------------- initialization ---*)

PROCEDURE Init (t: T; writer: Wr.T): T =
  VAR b: QValue.Binding;
  BEGIN
    t.scopes   := NEW (ScopeStack,  40);
    t.stack    := NEW (ValueStack,  100);
    t.loops    := NEW (LoopStack,   20);
    t.output   := NEW (OutputStack, 10);
    t.frames   := NEW (FrameStack,  40);
    t.includes := NEW (IncludeStack, 10);
    t.globals  := NEW (IntRefTbl.Default).init ();

    InitBuiltins ();
    FOR i := FIRST (Builtins) TO LAST (Builtins) DO
      b := Builtins[i];
      EVAL t.globals.put (b.name, b);
    END;

    EVAL PushScope (t);  (* so that "local" variables have a place to go *)

    t.writer := writer;
    RETURN t;
  END Init;

(*------------------------------------------------------------ evaluation ---*)

PROCEDURE Evaluate (t: T;  s: QCode.Stream)
  RAISES {Error} =
  BEGIN
    PushInclude (t, s, t.reg);
    Eval (t);
  END Evaluate;

PROCEDURE Eval (t: T; end_on_return: BOOLEAN := FALSE)
  RAISES {Error} =
  VAR
    op   : QCode.Op;
    arg  : INTEGER;
    val  : QValue.T;
    val2 : QValue.T;
    arr  : QVSeq.T;
    tbl  : QVTbl.T;
    int  : INTEGER;
    bind : QValue.Binding;
    txt  : TEXT;
    buf  : M3Buf.T;
    sfp  : INTEGER := 0;
  BEGIN
    IF end_on_return THEN sfp := t.reg.fp END;
    LOOP
      IF (t.tracing) THEN TraceInstruction (t) END;
      WITH z = t.reg.cp.instrs [t.reg.pc] DO op := z.op;  arg := z.a; END;
      INC (t.reg.pc);


      CASE op OF

      | Op.Integer =>
          val.kind := QK.Integer;
          val.int  := arg;
          val.ref  := NIL;
          Push (t, val);

      | Op.String =>
          val.kind := QK.String;
          val.int  := arg;
          val.ref  := NIL;
          Push (t, val);

      | Op.BuildArray =>
          arr := NEW (QVSeq.T).init (arg);
          FOR i := 0 TO arg-1 DO
            Pop (t, val);  
            IF val.kind = QValue.Kind.Array THEN
              WITH seq = QVal.ToArray(t, val) DO
                FOR i := seq.size() - 1 TO 0 BY -1 DO
                  arr.addlo(seq.get(i));
                END;
              END;
            ELSE
              arr.addlo (val);
            END;
          END;
          val.kind := QK.Array;
          val.int  := 0;
          val.ref  := arr;
          Push (t, val);
          arr := NIL;
          val.ref := NIL;

      | Op.BuildTable =>
          tbl := NEW (QVTbl.Default).init();
          FOR i := arg-1 TO 0 BY -2 DO
            Pop (t, val2);
            Pop (t, val);
            EVAL tbl.put (QVal.ToID (t, val), val2);
          END;
          val.kind := QK.Table;
          val.int  := 0;
          val.ref  := tbl;
          Push (t, val);
          tbl := NIL;
          val.ref := NIL;
          val2.ref := NIL;

      | Op.GetEnv =>
          PushString (t, Env.Get (M3ID.ToText (arg)));

      | Op.PushProc =>
          val.kind := QK.Proc;
          val.int  := 0;
          val.ref  := NEW (QValue.Proc, info := t.reg.cp.procs [arg],
                                        env  := t.scopes [t.reg.xp-1]);
          Push (t, val);
          val.ref := NIL;

      | Op.IsMember =>
          Pop (t, val);  int := QVal.ToID (t, val);
          Pop (t, val);  tbl := QVal.ToTable (t, val);
          PushBool (t, tbl.get (int, val));
          tbl := NIL;
          val.ref := NIL;

      | Op.Concat =>
          Pop (t, val2);
          Pop (t, val);
          buf := GetBuf (t);
          QVal.ToBuf (t, val, buf);
          QVal.ToBuf (t, val2, buf);
          PushString (t, M3Buf.ToText (buf));
          FreeBuf (t, buf);
          buf := NIL;
          val2.ref := NIL;
          val.ref := NIL;

      | Op.And =>
          Pop (t, val2);
          Pop (t, val);
          PushBool (t, QVal.ToBool (t, val) AND QVal.ToBool (t, val2));
          val2.ref := NIL;
          val.ref := NIL;

      | Op.Or =>
          Pop (t, val2);
          Pop (t, val);
          PushBool (t, QVal.ToBool (t, val) OR QVal.ToBool (t, val2));
          val2.ref := NIL;
          val.ref := NIL;

      | Op.Not =>
          Pop (t, val);
          PushBool (t, NOT QVal.ToBool (t, val));
          val.ref := NIL;
          
      | Op.IndexTable =>
          Pop (t, val);  int := QVal.ToID (t, val);
          Pop (t, val);  tbl := QVal.ToTable (t, val);
          IF NOT tbl.get (int, val) THEN
            Err (t, "table does not contain entry for: \""
                      & M3ID.ToText (int) & "\"");
          END;
          Push (t, val);
          tbl := NIL;
          val.ref := NIL;

      | Op.SubscriptArray =>
          Pop (t, val);  int := QVal.ToInt (t, val);
          Pop (t, val);  arr := QVal.ToArray (t, val);
          IF (int < 0) OR (arr.size() <= int) THEN
            Err (t, "array subscript out of bounds: " & Fmt.Int (int));
          END;
          Push (t, arr.get(int));
          arr := NIL;
          val.ref := NIL;

      | Op.InitForeach =>
          Pop (t, val);
          PushLoop (t, arg, val);
          val.ref := NIL;

      | Op.NextForeach =>
          IF NOT IterateLoop (t) THEN
            PopLoop (t);
            INC (t.reg.pc, arg);
          END;
        
      | Op.Goto =>
          INC (t.reg.pc, arg);

      | Op.IfFalse =>
          Pop (t, val);
          IF NOT QVal.ToBool (t, val) THEN INC (t.reg.pc, arg); END;
          val.ref := NIL;

      | Op.Halt =>
          PopInclude (t);
          IF (t.reg.ip <= 0) THEN EXIT; END;

      | Op.PushScope =>
          EVAL PushScope (t);

      | Op.PopScope =>
          PopScope (t);

      | Op.DefineG =>
          bind := DefineGlobal (t, arg, readonly := FALSE);
          Pop (t, bind.value);

      | Op.DefineGR =>
          bind := DefineGlobal (t, arg, readonly := TRUE);
          Pop (t, bind.value);

      | Op.DefineL =>
          bind := Define (t, arg, readonly := FALSE);
          Pop (t, bind.value);

      | Op.DefineLR =>
          bind := Define (t, arg, readonly := TRUE);
          Pop (t, bind.value);

      | Op.LoadVar =>
          bind := LookUp (t, arg);
          IF (bind # NIL) THEN
            Push (t, bind.value);
          ELSE
            Err (t, "undefined variable: " & M3ID.ToText (arg));
          END;

      | Op.Assign =>
          bind := LookUp (t, arg);
          IF (bind = NIL) THEN
            bind := DefineGlobal (t, arg, readonly := FALSE);
          ELSIF bind.readonly THEN
            Err (t, "cannot assign to readonly variable: " & M3ID.ToText(arg));
          END;
          Pop (t, val);
          bind.value := val;

      | Op.AssignTable =>
          Pop (t, val);
          Pop (t, val2);  int := QVal.ToID (t, val2);
          Pop (t, val2);  tbl := QVal.ToTable (t, val2);
          EVAL tbl.put (int, val);
          tbl := NIL;
          val.ref := NIL;
          val2.ref := NIL;

      | Op.AssignArray =>
          Pop (t, val);
          Pop (t, val2);  int := QVal.ToInt (t, val2);
          Pop (t, val2);  arr := QVal.ToArray (t, val2);
          IF (int < 0) OR (arr.size() <= int) THEN
            Err (t, "array subscript out of bounds: " & Fmt.Int (int));
          END;
          arr.put (int, val);
          arr := NIL;
          val.ref := NIL;
          val2.ref := NIL;

      | Op.Append =>
          Pop (t, val);
          Pop (t, val2);  arr := QVal.ToArray (t, val2);
          IF val.kind = QValue.Kind.Array THEN
            WITH seq = QVal.ToArray(t, val) DO
              FOR i := 0 TO seq.size() - 1 DO
                arr.addhi(seq.get(i));
              END;
            END;
          ELSE
            arr.addhi (val);
          END;
          arr := NIL;
          val.ref := NIL;
          val2.ref := NIL;

      | Op.StartRedirect =>
          Pop (t, val);  txt := QVal.ToText (t, val);
          PushOutput (t, txt, append := FALSE);
          txt := NIL;
          val.ref := NIL;

      | Op.StartAppendRedirect =>
          Pop (t, val);  txt := QVal.ToText (t, val);
          PushOutput (t, txt, append := TRUE);
          txt := NIL;
          val.ref := NIL;

      | Op.EndRedirect =>
          PopOutput (t);

      | Op.StartCall =>
          PushFrame (t);

      | Op.CallProc =>
          DoCall (t, arg, FALSE);

      | Op.CallFunc =>
          DoCall (t, arg, TRUE);

      | Op.SetLine =>
          t.reg.ln := arg;

      | Op.ReturnValue =>
          CheckReturn (t, TRUE);
          Pop (t, val);
          PopFrame (t);
          Push (t, val);
          IF end_on_return AND sfp - 1 = t.reg.fp THEN EXIT END;

      | Op.Return =>
          CheckReturn (t, FALSE);
          PopFrame (t);
          IF end_on_return AND sfp - 1 = t.reg.fp THEN EXIT END;

      END; (* case *)
    END; (* loop *)
  END Eval;

PROCEDURE TraceInstruction (t: T) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR op: QCode.Op;  arg: INTEGER;
  BEGIN
    IF (t.last_cp # t.reg.cp) THEN
      Wr.PutText (t.writer, "****** ");
      IF (t.reg.cp # NIL) THEN
        Wr.PutText (t.writer, M3ID.ToText (t.reg.cp.source_file));
      END;
      Wr.PutText (t.writer, " ******\n");
      t.last_cp := t.reg.cp;
    END;

    WITH z = t.reg.cp.instrs [t.reg.pc] DO op := z.op;  arg := z.a; END;

    FOR i := 1 TO t.reg.xp DO  Wr.PutText (t.writer, "."); END;
    Wr.PutText (t.writer, Fmt.Pad(Fmt.Int(t.reg.pc),4,' ',Fmt.Align.Left));
    Wr.PutChar (t.writer, ' ');
    Wr.PutText (t.writer, QCode.OpName[op]);
    CASE QCode.OpFormat [op] OF
    | 0 => (*done*)
    | 1 => Wr.PutText (t.writer, "  ");
           Wr.PutText (t.writer, Fmt.Int (arg));
    | 2 => Wr.PutText (t.writer, "  (");
           Wr.PutText (t.writer, Fmt.Int (arg));
           Wr.PutText (t.writer, ") \"");
           Wr.PutText (t.writer, M3ID.ToText (arg));
           Wr.PutText (t.writer, "\"");
    | 3 => Wr.PutText (t.writer, "  pc+(");
           Wr.PutText (t.writer, Fmt.Int (arg));
           Wr.PutText (t.writer, ") => ");
           Wr.PutText (t.writer, Fmt.Int (t.reg.pc + 1 + arg));
    END;
    Wr.PutText (t.writer, "\n");
    Wr.Flush(t.writer);
  END TraceInstruction;
(*------------------------------------------------------- procedure calls ---*)

PROCEDURE PushFrame (t: T)
  RAISES {Error} =
  VAR val: QValue.T;
  BEGIN
    Pop (t, val);  (* the procedure value *)
    IF (t.reg.fp >= NUMBER (t.frames^)) THEN ExpandFrames (t); END;
    WITH f = t.frames[t.reg.fp] DO
      f.proc  := QVal.ToProc (t, val);
      f.saved := t.reg;
    END;
    INC (t.reg.fp);
  END PushFrame;

PROCEDURE ExpandFrames (t: T) =
  VAR n := NUMBER (t.frames^);  new := NEW (FrameStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.frames^;
    t.frames := new;
  END ExpandFrames;

PROCEDURE DoCall (t: T;  n_args: INTEGER;  isFunc: BOOLEAN)
  RAISES {Error} =
  VAR p: QValue.Proc;  s: QValue.Scope;  val: QValue.T;
      normal := FALSE;
  BEGIN
    WITH f = t.frames[t.reg.fp-1] DO
      p := f.proc;
      IF (p.info.n_args # n_args) AND (p.info.n_args >= 0) THEN
        Err (t, "wrong number of parameters passed to procedure");
      END;
      IF (p.info.builtin) THEN
        (* we save and restore the registers in case a builtin
           procedure is on the stack when an error is raised *)
        f.saved.pi := t.reg.pi;
        f.saved.pc := t.reg.pc;
        f.saved.cp := t.reg.cp;
        f.saved.ln := t.reg.ln;
        f.saved.fn := t.reg.fn;
        t.reg.pi := p.info;
        t.reg.pc := 0;
        t.reg.cp := NIL;
        t.reg.ln := 0;
        t.reg.fn := isFunc;
        TRY
          p.info.handler (t, n_args);
          normal := TRUE; (* flag that no exception has been raised *)
        FINALLY
          IF normal THEN
            (* no exception raised *)
            CheckReturn (t, p.info.isFunc);
            IF p.info.isFunc THEN
              Pop (t, val);
              PopFrame (t);
              Push (t, val);
            ELSE
              PopFrame (t);
            END;
          ELSE
            (* the builtin raised an exception!!! *)
            PopFrame (t);
          END;
        END;
      ELSE
        f.saved.pi := t.reg.pi;
        f.saved.pc := t.reg.pc;
        f.saved.cp := t.reg.cp;
        f.saved.ln := t.reg.ln;
        t.reg.pi := p.info;
        t.reg.pc := p.info.entry;
        t.reg.cp := p.info.code;
        t.reg.fn := isFunc;
        s := PushScope (t);
        s.parent := p.env;  (* use procedure's static link *)
        <*ASSERT s.parent # s*>
      END;
    END;
  END DoCall;

PROCEDURE PopFrame (t: T)
  RAISES {Error} =
  VAR val: QValue.T;
  BEGIN
    DEC (t.reg.fp);
    WITH f = t.frames[t.reg.fp] DO
      f.proc := NIL;
      WHILE (t.reg.ip > f.saved.ip) DO PopInclude (t); END;
      t.reg.pi := f.saved.pi;  f.saved.pi := NIL;
      t.reg.cp := f.saved.cp;  f.saved.cp := NIL;
      t.reg.pc := f.saved.pc;
      t.reg.ln := f.saved.ln;
      t.reg.fn := f.saved.fn;
      WHILE (t.reg.xp > f.saved.xp) DO PopScope (t);   END;
      WHILE (t.reg.sp > f.saved.sp) DO Pop (t, val);   END;
      WHILE (t.reg.lp > f.saved.lp) DO PopLoop (t);    END;
      WHILE (t.reg.op > f.saved.op) DO PopOutput (t);  END;
    END;
  END PopFrame;

PROCEDURE CheckReturn (t: T;  with_value: BOOLEAN)
  RAISES {Error} =
  BEGIN
    IF (t.reg.fp < 1) THEN
      Err (t, "return not in a function or procedure");
    END;
    IF (t.reg.fn = with_value) THEN
      (* ok *)
    ELSIF (t.reg.fn) THEN
      Err (t, "expected return value is missing");
    ELSE
      Err (t, "unexpected return value");
    END;
  END CheckReturn;

PROCEDURE Call(t: T; proc: QValue.Proc; args: REF ARRAY OF QValue.T;
               isFunc: BOOLEAN) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    arg.kind := QValue.Kind.Proc;
    arg.int := 0;
    arg.ref := proc;
    t.push(arg);
    PushFrame(t);
    (* push the args *)
    IF args # NIL THEN
      FOR i := FIRST(args^) TO LAST(args^) DO
        t.push(args[i]);
      END;
      DoCall(t, NUMBER(args^), isFunc);
    ELSE
      DoCall(t, 0, isFunc);
    END;
    IF NOT proc.info.builtin THEN
      Eval(t, TRUE);
    END;
  END Call;



(*------------------------------------------------------- global bindings ---*)

PROCEDURE Get (t: T;  name: M3ID.T;  VAR(*OUT*) value: QValue.T): BOOLEAN =
  VAR ref: REFANY;
  BEGIN
    IF t.globals.get (name, ref) THEN
      value := NARROW (ref, QValue.Binding).value;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Get;

PROCEDURE Put (t: T;  name: M3ID.T;  READONLY value: QValue.T;
               readonly: BOOLEAN := FALSE)
  RAISES {Error} =
  VAR bind := DefineGlobal (t, name, readonly := readonly);
  BEGIN
    bind.value := value;
  END Put;

(*----------------------------------------------- scopes & local bindings ---*)

PROCEDURE PushScope (t: T): QValue.Scope =
  BEGIN
    IF (t.reg.xp >= NUMBER (t.scopes^)) THEN ExpandScopes (t); END;
    WITH s = t.scopes [t.reg.xp] DO
      s := NEW (QValue.Scope);
      IF (t.reg.xp > 0)
        THEN s.parent := t.scopes[t.reg.xp-1];
        ELSE s.parent := NIL;
      END;
      <*ASSERT s.parent # s*>
      INC (t.reg.xp);
      RETURN s;
    END;
  END PushScope;

PROCEDURE ExpandScopes (t: T) =
  VAR n := NUMBER (t.scopes^);  new := NEW (ScopeStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.scopes^;
    t.scopes := new;
  END ExpandScopes;

PROCEDURE PopScope (t: T) =
  VAR b, last_b: QValue.Binding;
  BEGIN
    DEC (t.reg.xp);
    WITH s = t.scopes [t.reg.xp] DO
      b := s.bindings;
      IF (b # NIL) THEN
        (* recycle the bindings *)
        WHILE (b # NIL) DO
          b.readonly  := FALSE;
          b.name      := M3ID.NoID;
          b.value.ref := NIL;
          last_b := b;
          b := b.next;
        END;
        last_b.next := t.bindings;
        t.bindings := s.bindings;
        s.bindings := NIL;
      END;
      s.parent := NIL;
    END;
  END PopScope;

PROCEDURE Define (t: T;  id: M3ID.T;  readonly: BOOLEAN): QValue.Binding
  RAISES {Error} =
  VAR old, new: QValue.Binding;
  BEGIN
    WITH s = t.scopes [t.reg.xp-1] DO
      old := s.bindings;
      new := NewBinding (t);
      new.next     := old;
      new.name     := id;
      new.readonly := readonly;
      WHILE (old # NIL) DO
        IF (old.name = id) THEN
          Err (t, "duplicate symbol defined: " & M3ID.ToText (id));
        END;
        old := old.next;
      END;
      s.bindings := new;
    END;
    RETURN new;
  END Define;

PROCEDURE DefineGlobal (t: T;  id: M3ID.T;  readonly: BOOLEAN): QValue.Binding
  RAISES {Error} =
  VAR ref: REFANY;  bind: QValue.Binding;
  BEGIN
    IF t.globals.get (id, ref) THEN
      bind := ref;
    ELSE
      bind := NewBinding (t);
      bind.name := id;
      bind.readonly := FALSE;
      EVAL t.globals.put (id, bind);
    END;
    IF (bind.readonly) THEN
      Err (t, "cannot redefine readonly global symbol: " & M3ID.ToText (id));
    END;
    bind.readonly := readonly;
    RETURN bind;
  END DefineGlobal;

PROCEDURE LookUp (t: T;  id: M3ID.T): QValue.Binding =
  VAR s: QValue.Scope;  b: QValue.Binding;  ref: REFANY;
  BEGIN
    (* try the local scopes first *)
    IF (t.reg.xp > 0) THEN
      s := t.scopes [t.reg.xp-1];
      WHILE (s # NIL) DO
        b := s.bindings;
        WHILE (b # NIL) DO
          IF (b.name = id) THEN RETURN b; END;
          b := b.next;
        END;
        s := s.parent;
      END;
    END;
    
    (* finally try the globals *)
    IF t.globals.get (id, ref)
      THEN RETURN ref;
      ELSE RETURN NIL;
    END;
  END LookUp;

PROCEDURE NewBinding (t: T): QValue.Binding =
  VAR b := t.bindings;
  BEGIN
    IF (b # NIL) THEN
      t.bindings := b.next;
      b.next := NIL;
    ELSE
      b := NEW (QValue.Binding);
    END;
    RETURN b;
  END NewBinding;

(*------------------------------------------------------------ data stack ---*)

PROCEDURE Push (t: T;  READONLY value: QValue.T) =
  BEGIN
    IF (t.reg.sp >= NUMBER (t.stack^)) THEN ExpandStack (t); END;
    t.stack [t.reg.sp] := value;
    INC (t.reg.sp);
  END Push;

PROCEDURE ExpandStack (t: T) =
  VAR n := NUMBER (t.stack^);  new := NEW (ValueStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.stack^;
    t.stack := new;
  END ExpandStack;

PROCEDURE Pop (t: T;  VAR(*OUT*) value: QValue.T) RAISES {Error} =
  BEGIN
    IF (t.reg.sp <= 0) THEN Err (t, "empty stack"); END;
    DEC (t.reg.sp);
    WITH z = t.stack [t.reg.sp] DO
      value := z;
      z.ref := NIL;
    END;
  END Pop;

PROCEDURE PushString (t: T;  s: TEXT) =
  VAR v: QValue.T;
  BEGIN
    IF (s = NIL) THEN s := ""; END;
    v.kind := QK.String;
    v.int  := M3ID.Add (s);
    v.ref  := NIL;
    Push (t, v);
  END PushString;

PROCEDURE PushBool (t: T;  b: BOOLEAN) =
  VAR v: QValue.T;
  BEGIN
    v.kind := QK.String;
    v.int  := QValue.BoolID [b];
    v.ref  := NIL;
    Push (t, v);
  END PushBool; 

(*---------------------------------------------------------- output stack ---*)

PROCEDURE PushOutput (t: T;  nm: TEXT;  append: BOOLEAN)
  RAISES {Error} =
  BEGIN
    IF (t.reg.op >= NUMBER (t.output^)) THEN ExpandOutput (t); END;
    WITH o = t.output [t.reg.op] DO
      o.name := nm;
      TRY
        IF (append)
          THEN o.wr := FileWr.OpenAppend (nm);
          ELSE o.wr := FileWr.Open (nm);
        END;
      EXCEPT OSError.E(ec) =>
        Err (t, "unable to open \"" & nm & "\" for writing" & OSErr(ec));
      END;
    END;
    INC (t.reg.op);
  END PushOutput;

PROCEDURE ExpandOutput (t: T) =
  VAR n := NUMBER (t.output^);  new := NEW (OutputStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.output^;
    t.output := new;
  END ExpandOutput;

PROCEDURE PopOutput (t: T)
  RAISES {Error} =
  BEGIN
    DEC (t.reg.op);
    WITH o = t.output [t.reg.op] DO
      TRY
        Wr.Close (o.wr);
        o.wr := NIL;
        o.name := NIL;
      EXCEPT
      | Wr.Failure(ec) =>
          Err (t, "unable to close \"" & o.name & "\"" & OSErr(ec));
      | Thread.Alerted =>
          Err (t, "unable to close \"" & o.name & "\": interrupted");
      END;
    END;
  END PopOutput;

PROCEDURE CurWr (t: T): Wr.T =
  BEGIN
    IF (t.reg.op <= 0)
      THEN RETURN t.writer;
      ELSE RETURN t.output [t.reg.op-1].wr;
    END;
  END CurWr;

(*------------------------------------------------------------ loop stack ---*)

PROCEDURE PushLoop (t: T;  nm: M3ID.T;  READONLY elts: QValue.T)
  RAISES {Error} =
  VAR tbl: QVTbl.T;  arr: QVSeq.T;
  BEGIN
    IF (t.reg.lp >= NUMBER (t.loops^)) THEN ExpandLoops (t); END;
    WITH x = t.loops [t.reg.lp] DO
      IF (elts.kind = QK.Table) THEN
        tbl := elts.ref;
        x.iter := tbl.iterate ();
      ELSIF (elts.kind = QK.Array) THEN
        arr := elts.ref;
        x.array := arr;
        x.next_elt := 0;
      ELSE
        Err (t, "\"foreach\" not applied to an array or table");
      END;
      EVAL PushScope (t);
      x.variable := Define (t, nm, readonly := TRUE);
    END;
    INC (t.reg.lp);
  END PushLoop;

PROCEDURE ExpandLoops (t: T) =
  VAR n := NUMBER (t.loops^);  new := NEW (LoopStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.loops^;
    t.loops := new;
  END ExpandLoops;

PROCEDURE IterateLoop (t: T): BOOLEAN =
  VAR int: INTEGER;  val: QValue.T;
  BEGIN
    WITH x = t.loops [t.reg.lp-1] DO
      IF (x.iter # NIL) THEN (* we're iterating over a table *)
        IF NOT x.iter.next (int, val) THEN RETURN FALSE; END;
        WITH z = x.variable.value DO
          z.kind := QK.String;
          z.int  := int;
          z.ref  := NIL;
        END;
      ELSE (* we're iterating over an array *)
        IF (x.next_elt >= x.array.size()) THEN RETURN FALSE; END;
        x.variable.value := x.array.get (x.next_elt);
        INC (x.next_elt);
      END;
    END;
    RETURN TRUE;
  END IterateLoop;

PROCEDURE PopLoop (t: T) =
  BEGIN
    PopScope (t);
    DEC (t.reg.lp);
    WITH x = t.loops [t.reg.lp] DO
      x.iter     := NIL;
      x.array    := NIL;
      x.variable := NIL;
    END;
  END PopLoop;

(*--------------------------------------------------------- include stack ---*)

PROCEDURE PushInclude (t: T;  s: QCode.Stream;  VAR reg: Registers) =
  BEGIN
    IF (reg.ip >= NUMBER (t.includes^)) THEN ExpandIncludes (t); END;
    WITH x = t.includes [reg.ip] DO
      x.file   := s;
      x.old_cp := reg.cp;
      x.old_pc := reg.pc;
    END;
    reg.cp := s;
    reg.pc := 0;
    INC (reg.ip);
  END PushInclude;

PROCEDURE ExpandIncludes (t: T) =
  VAR n := NUMBER (t.includes^);  new := NEW (IncludeStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.includes^;
    t.includes := new;
  END ExpandIncludes;

PROCEDURE PopInclude (t: T) =
  BEGIN
    DEC (t.reg.ip);
    WITH x = t.includes [t.reg.ip] DO
      t.reg.cp := x.old_cp;
      t.reg.pc := x.old_pc;
      x.file   := NIL;
      x.old_cp := NIL;
    END;
  END PopInclude;

(*---------------------------------------------------- builtin procedures ---*)

VAR (*READONLY*)
  init_done := FALSE;
  Builtins  : ARRAY [0..17] OF QValue.Binding;

PROCEDURE InitBuiltins () =
  BEGIN
    IF init_done THEN RETURN END;
    init_done := TRUE;

    Builtins [ 0] := NewBuiltin ("arglist",     DoArgList,   2, TRUE);
    Builtins [ 1] := NewBuiltin ("cp_if",       DoCopyIfNew, 2, FALSE);
    Builtins [ 2] := NewBuiltin ("defined",     DoDefined,   1, TRUE);
    Builtins [ 3] := NewBuiltin ("empty",       DoEmpty,     1, TRUE);
    Builtins [ 4] := NewBuiltin ("equal",       DoEqual,     2, TRUE);
    Builtins [ 5] := NewBuiltin ("error",       DoError,    -1, FALSE);
    Builtins [ 6] := NewBuiltin ("escape",      DoEscape,    1, TRUE);
    Builtins [ 7] := NewBuiltin ("exec",        DoExec,     -1, TRUE);
    Builtins [ 8] := NewBuiltin ("file",        DoFile,      0, TRUE);
    Builtins [ 9] := NewBuiltin ("format",      DoFormat,   -1, TRUE);
    Builtins [10] := NewBuiltin ("include",     DoInclude,   1, FALSE);
    Builtins [11] := NewBuiltin ("normalize",   DoNormalize, 2, TRUE);
    Builtins [12] := NewBuiltin ("path",        DoPath,      0, TRUE);
    Builtins [13] := NewBuiltin ("stale",       DoStale,     2, TRUE);
    Builtins [14] := NewBuiltin ("unlink_file", DoUnlink,    1, TRUE);
    Builtins [15] := NewBuiltin ("write",       DoWrite,    -1, FALSE);
    Builtins [16] := NewBuiltin ("TRACE_INSTR", DoTrace,     0, FALSE);
    Builtins [17] := NewBuiltin ("lookup",      DoLookUp,    2, TRUE);

  END InitBuiltins;

PROCEDURE NewBuiltin (nm      : TEXT;
                      handler : QCode.BuiltinProc;
                      n_args  : INTEGER;
                      isFunc  : BOOLEAN): QValue.Binding =
  VAR
    id   := M3ID.Add (nm);
    info := NEW (QCode.ProcInfo, name := id, isFunc := isFunc,
                   n_args := n_args, builtin := TRUE, handler := handler);
    proc := NEW (QValue.Proc, info := info, env := NIL);
    bind := NEW (QValue.Binding, name := info.name, readonly := TRUE);
  BEGIN
    bind.value.kind := QValue.Kind.Proc;
    bind.value.int  := 0;
    bind.value.ref  := proc;
    RETURN bind;
  END NewBuiltin;

PROCEDURE DoArgList (t: T;  n_args: INTEGER) RAISES {Error} =
  CONST
    Max_args       = 10;
    Max_arg_length = 1024;
  VAR
    prefix, args : TEXT;
    split        : TextSeq.T;
    val0, val1   : QValue.T;
    file         : TEXT;
    wr           : Wr.T;
    buf          : M3Buf.T;
  BEGIN
    <*ASSERT n_args = 2 *>
    buf := GetBuf (t);
    Pop (t, val1);
    QVal.ToBuf (t, val1, buf);
    args  := M3Buf.ToText (buf);
    split := SplitArgs (args);
    FreeBuf (t, buf);

    Pop (t, val0);
    prefix := QVal.ToText (t, val0);

    (* check for the easy case *)
    IF (split.size () <= Max_args)
      AND (Text.Length (args) <= Max_arg_length) THEN
      Push (t, val1);
      RETURN;
    END;

    TRY
      file := UniqueTempFile (t);
      wr := FileWr.Open (file);
      TRY
        FOR i := 0 TO split.size()-1  DO
          Wr.PutText (wr, split.get(i));
          Wr.PutText (wr, Wr.EOL);
        END;
      FINALLY
        Wr.Close (wr);
      END;

      PushString (t, prefix & file);
    EXCEPT
    | Thread.Alerted =>
        Err (t, "interrupted");
    | Wr.Failure(ec) =>
        Err (t, "unable to write on \"" & file & "\"" & OSErr (ec));
    | OSError.E(ec) =>
        Err (t, "unable to write on \"" & file & "\"" & OSErr (ec));
    END;
  END DoArgList;

PROCEDURE DoCopyIfNew (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR val: QValue.T;  src, dest: TEXT;
      equal:= FALSE;
  BEGIN
    <*ASSERT n_args = 2 *>
    Pop (t, val);  dest := QVal.ToText (t, val);
    Pop (t, val);  src  := QVal.ToText (t, val);

    IF M3File.IsDirectory (dest) THEN
      dest := Pathname.Join (dest, Pathname.Last (src), NIL);
    END;

    TRY equal := M3File.IsEqual (src, dest) EXCEPT
      OSError.E => (* SKIP *)
    END;
    TRY
      IF NOT equal THEN M3File.Copy (src, dest) END;
    EXCEPT OSError.E(ec) =>
      Err (t, "unable to copy \""& src &"\" to \""& dest &"\""& OSErr (ec));
    END;
  END DoCopyIfNew;

PROCEDURE DoDefined (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR val: QValue.T;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);
    PushBool (t, LookUp (t, QVal.ToID (t, val)) # NIL);
  END DoDefined;

PROCEDURE DoEmpty (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR empty := FALSE;  val: QValue.T;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);
    CASE val.kind OF
    | QK.Integer => empty := FALSE;
    | QK.String  => empty := (val.int = QValue.BoolID[FALSE]);
    | QK.Array   => empty := NARROW (val.ref, QVSeq.T).size() = 0;
    | QK.Table   => empty := NARROW (val.ref, QVTbl.T).size() = 0;
    ELSE
      Err (t, "\"empty\" not applied to a string, table, or array");
    END;
    PushBool (t, empty);
  END DoEmpty;

PROCEDURE DoEqual (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR v1, v2: QValue.T;  eq := FALSE;
  BEGIN
    <*ASSERT n_args = 2 *>
    Pop (t, v1);
    Pop (t, v2);
    IF (v1.kind = v2.kind) THEN
      CASE v1.kind OF
      | QK.Var     => eq := (v1.int = v2.int) AND (v1.ref = v2.ref);
      | QK.Integer => eq := (v1.int = v2.int);
      | QK.String  => eq := (v1.int = v2.int);
      | QK.Table   => eq := (v1.ref = v2.ref);
      | QK.Array   => eq := (v1.ref = v2.ref);
      | QK.Proc    => eq := (v1.ref = v2.ref);
      END;
    END;
    PushBool (t, eq);
  END DoEqual;

PROCEDURE DoError (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR buf := GetBuf(t);
  BEGIN
    <*ASSERT n_args > 0 *>
    FOR i := t.reg.sp - n_args TO t.reg.sp - 1 DO
      QVal.ToBuf (t, t.stack[i], buf);
      t.stack[i].ref := NIL;
    END;
    t.reg.sp := t.reg.sp - n_args;
    Err (t, M3Buf.ToText (buf));
  END DoError;

PROCEDURE DoEscape (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR
    val     : QValue.T;
    txt     : TEXT;
    buf     : M3Buf.T;
    ch      : CHAR;
    len     : INTEGER;
    out_len : INTEGER;
    out_buf : ARRAY [0..199] OF CHAR;
    in_buf  : ARRAY [0..199] OF CHAR;
    new_ch  : BOOLEAN := FALSE;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);  txt := QVal.ToText (t, val);
    len := Text.Length (txt);
    IF (len+len <= NUMBER (out_buf)) THEN
      out_len := 0;
      Text.SetChars (in_buf, txt);
      FOR i := 0 TO len-1 DO
        ch := Text.GetChar (txt, i);
        IF (ch = '\134') THEN
          out_buf[out_len] := ch; INC (out_len);
          new_ch := TRUE;
        END;
        out_buf [out_len] := ch;  INC (out_len);
      END;
      IF (new_ch)
        THEN PushString (t, Text.FromChars (SUBARRAY (out_buf, 0, out_len)));
        ELSE Push (t, val);
      END;
    ELSE
      buf := GetBuf (t);
      FOR i := 0 TO len - 1 DO
        ch := Text.GetChar (txt, i);
        IF (ch = '\134') THEN M3Buf.PutChar (buf, ch); new_ch := TRUE; END;
        M3Buf.PutChar (buf, ch);
      END;
      txt := M3Buf.ToText (buf);
      FreeBuf (t, buf);
      IF (new_ch)
        THEN PushString (t, txt);
        ELSE Push (t, val);
      END;
    END;
  END DoEscape;

PROCEDURE DoExec (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR 
    cmdval: QValue.T;
    ioval : QValue.T;
    wdval : QValue.T;
    envval: QValue.T;
    cmd   : TEXT;
    args  : REF ARRAY OF TEXT;
    wd    : TEXT;
    env   : REF ARRAY OF TEXT;
    seq   : QVSeq.T;
    size  : INTEGER;
    stdin : TEXT;
    stdout: TEXT;
    stderr: TEXT;
    ret   : INTEGER;
  BEGIN
    IF (n_args < 1) THEN 
      Err(t, "exec failed: must supply at least one argument!\n");
    END;
    (* pop the right arguments *)
    IF n_args >= 2 THEN
      IF n_args >= 3 THEN
        IF n_args = 4 THEN
          Pop(t, envval);
          seq := QVal.ToArray(t, envval);
          size := seq.size();
          IF size > 0 THEN
            env := NEW(REF ARRAY OF TEXT, size);
            FOR i := 0 TO LAST(env^) DO
              env[i] := QVal.ToText(t, seq.get(i));
            END;
          END;
        ELSIF n_args > 4 THEN
          Err(t, "exec failed: exec takes at most 4 arguments!\n");
        END;
        Pop(t, wdval);
        wd := QVal.ToText(t, wdval);
      END;
      Pop(t, ioval);
      seq := QVal.ToArray(t, ioval);
      size := seq.size();
      IF size > 0 THEN
        stdin := QVal.ToText(t, seq.get(0));
        IF Text.Equal(stdin, "") THEN stdin := NIL END;
      END;
      IF size > 1 THEN
        stdout := QVal.ToText(t, seq.get(1));
        IF Text.Equal(stdout, "") THEN stdout := NIL END;
      END;
      IF size > 2 THEN
        stderr := QVal.ToText(t, seq.get(2));
        IF Text.Equal(stderr, "") THEN stderr := NIL END;
      END;
      IF size > 3 THEN
        Err(t, "exec failed: the argument \"io\" must be an array of at most 3 lements!");
      END;
    END;
    Pop(t, cmdval);
    seq := QVal.ToArray(t, cmdval);
    size := seq.size();
    IF size < 1 THEN
      Err(t, "exec failed: the argument \"cmd\" must be an array of at least 1 element!");
    END;
    cmd := QVal.ToText(t, seq.get(0));
    args := NEW(REF ARRAY OF TEXT, size - 1);
    FOR i := 0 TO LAST(args^) DO
      args[i] := QVal.ToText(t, seq.get(i + 1));
    END;
    
    ret := Exec(t, cmd, args, stdin, stdout, stderr, wd, env);
    cmdval.kind := QValue.Kind.Integer;
    cmdval.int  := ret;
    cmdval.ref  := NIL;
    Push(t, cmdval);
  END DoExec;

PROCEDURE Exec (t: T;  cmd: TEXT; args: REF ARRAY OF TEXT;
                stdin, stdout, stderr: TEXT := NIL;
                wd: TEXT := NIL;
                env: REF ARRAY OF TEXT := NIL): INTEGER RAISES {Error} =
  VAR
    stdin_file, stdout_file, stderr_file: File.T := NIL;
    new_stdin, new_stdout, new_stderr: BOOLEAN := FALSE;
    n            : INTEGER := -1;
    handle       : Process.T;
    buffer       : ARRAY [0 .. 4095] OF CHAR;
    nb           : INTEGER;
    rd           : FileRd.T;
    hwChildOut, hrSelf : Pipe.T := NIL;
  BEGIN
    (* finally, execute the command *)
    TRY
      IF stdin # NIL THEN
        stdin_file := FS.OpenFile(stdin, 
                                  create := FS.CreateOption.Never,
                                  access := FS.AccessOption.ReadOnly);
        new_stdin := TRUE;
      END;

      IF stdout # NIL THEN
        IF (Text.Length(stdout) >= 2 AND Text.GetChar(stdout, 0) = '>') 
          AND (Text.GetChar(stdout, 1) = '>') THEN
          stdout_file := FS.OpenFile(Text.Sub(stdout, 2), FALSE);
          EVAL NARROW(stdout_file, RegularFile.T).seek(RegularFile.Origin.End, 
                                                     0);
        ELSE
          stdout_file := FS.OpenFile(stdout);
        END;
        new_stdout := TRUE;
      END;

      IF stderr # NIL THEN
        IF (Text.Length(stderr) >= 2 AND Text.GetChar(stderr, 0) = '>') 
          AND (Text.GetChar(stderr, 1) = '>') THEN
          stderr_file := FS.OpenFile(Text.Sub(stderr, 2), FALSE);
          EVAL NARROW(stderr_file, RegularFile.T).seek(RegularFile.Origin.End,
                                                      0);
        ELSE
          stderr_file := FS.OpenFile(stderr);
        END;
        new_stderr := TRUE;
      END;

      (* One of the output files is unspecified. Redirect to stdout if this
         is where the current output is going. Otherwise create a pipe
         to connect it to the current output writer. *)

      IF stdout_file = NIL OR stderr_file = NIL THEN
        IF t.writer = Stdio.stdout THEN
          VAR dumb: File.T; 
          BEGIN
            IF stdout_file = NIL THEN
              Process.GetStandardFileHandles(dumb, stdout_file, dumb);
            END;
            IF stderr_file = NIL THEN
              Process.GetStandardFileHandles(dumb, dumb, stderr_file);
            END;
          END;
        ELSE
          Pipe.Open (hr := hrSelf,  hw := hwChildOut);
          IF stdout_file = NIL THEN
            stdout_file := hwChildOut;
            new_stdout := TRUE;
          END;
          IF stderr_file = NIL THEN
            stderr_file := hwChildOut;
            new_stderr := TRUE;
          END;
        END;
      END;

      handle := Process.Create(cmd, args^, env, wd, stdin_file, stdout_file,
                               stderr_file);

      (* Close the file handles created *)

      IF new_stdin THEN stdin_file.close()  END;
      IF new_stdout THEN stdout_file.close() END;
      IF new_stderr AND stderr_file # stdout_file THEN stderr_file.close() END;

      (* If a pipe was created, pump its content to the output writer *)

      IF hrSelf # NIL THEN
        rd := NEW(FileRd.T).init(hrSelf);
        LOOP
          nb := Rd.GetSub(rd, buffer);
          IF nb = NUMBER(buffer) THEN
            Wr.PutString(t.writer, buffer);
          ELSE
            Wr.PutString(t.writer, SUBARRAY(buffer, 0, nb));
            EXIT;
          END;
        END;
        Rd.Close(rd);
        Wr.Flush(t.writer);
      END;
      n := Process.Wait(handle);

    EXCEPT 
      | OSError.E (ec) =>
        Err (t, "exec failed" & OSErr (ec) & " *** " & cmd);
      | Wr.Failure, Thread.Alerted, Rd.Failure => (* ignore *)
    END;
    RETURN n;
  END Exec;

PROCEDURE DoFile (t: T;  n_args: INTEGER) =
  VAR path := M3ID.ToText (t.includes[t.reg.ip-1].file.source_file);
  BEGIN
    <*ASSERT n_args = 0 *>
    PushString (t, path);
  END DoFile;

PROCEDURE DoFormat (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR
    val     : QValue.T;
    n       : INTEGER;
    format  : TEXT;
    strings := NEW (REF ARRAY OF TEXT, n_args - 1);
  BEGIN
    <*ASSERT n_args > 0 *>
    n := 0;
    FOR i := t.reg.sp - n_args + 1 TO t.reg.sp - 1 DO
      strings [n] := QVal.ToText (t, t.stack[i]);  INC (n);
      t.stack[i].ref := NIL;
    END;
    DEC (t.reg.sp, n_args - 1);
    Pop (t, val);
    format := QVal.ToText (t, val);
    PushString (t, Fmt.FN (format, strings^));
  END DoFormat;

PROCEDURE DoInclude (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR path: TEXT;  val: QValue.T;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);  path := QVal.ToText (t, val);
    Include(t, path);
  END DoInclude;

PROCEDURE Include (t: T;  path: TEXT) RAISES {Error} =
  VAR old_path: TEXT;  code: QCode.Stream;
  BEGIN
    IF NOT Pathname.Absolute (path) THEN
      old_path := M3ID.ToText (t.includes[t.reg.ip-1].file.source_file);
      path := Pathname.Join (Pathname.Prefix (old_path), path, NIL);
      TRY
        path := Pathname.Compose(CanonicalizePath(Pathname.Decompose(path)));
      EXCEPT
      | Pathname.Invalid => Err(t,"invalid path in include");
      END;
    END;

    TRY
      code := QCompiler.CompileFile (path);
    EXCEPT Error(msg) =>
      Err (t, msg);
    END;

    WITH f = t.frames [t.reg.fp-1] DO
      PushInclude (t, code, f.saved);
      t.reg.ip := f.saved.ip;
    END;
  END Include;

PROCEDURE DoNormalize (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR
    val: QValue.T;
    unfixed, prefix: TEXT;
    unfixedArcs, prefixArcs: Pathname.Arcs;
  BEGIN
    <*ASSERT n_args = 2 *>
    Pop (t, val);  unfixed := QVal.ToText (t, val);
    Pop (t, val);  prefix  := QVal.ToText (t, val);

    TRY
      unfixedArcs := Pathname.Decompose(unfixed);
    EXCEPT Pathname.Invalid =>
      Err (t, Fmt.F ("invalid path (\"%s\") in normalize", unfixed));
    END;

    TRY
      prefixArcs := Pathname.Decompose(prefix);
    EXCEPT Pathname.Invalid =>
      Err (t, Fmt.F ("invalid path (\"%s\") in normalize", prefix));
    END;

    TRY
      PushString (t, Pathname.Compose(StripPrefix(t, prefixArcs,
                                           CanonicalizePath(unfixedArcs))));
    EXCEPT Pathname.Invalid =>
      Err (t, "invalid path in normalize");
    END;
  END DoNormalize;

PROCEDURE DoPath (t: T;  n_args: INTEGER) =
  VAR path := M3ID.ToText (t.includes[t.reg.ip-1].file.source_file);
  BEGIN
    <*ASSERT n_args = 0 *>
    PushString (t, Pathname.Prefix (path));
  END DoPath;

PROCEDURE DoStale (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR
    val, val2 : QValue.T;
    arr       : QVSeq.T;
    dep       : TEXT;
    target    : TEXT;
    t_status  : File.Status;
    d_status  : File.Status;
  BEGIN
    <*ASSERT n_args = 2 *>
    Pop (t, val2);  (* dependencies *)
    Pop (t, val);   target := QVal.ToText (t, val);

    TRY
      t_status := FS.Status (target);
      IF (val2.kind = QK.Array) THEN
        arr := val2.ref;
        FOR i := 0 TO arr.size() - 1 DO
          dep := QVal.ToText (t, arr.get (i));
          d_status := FS.Status (dep);
          IF t_status.modificationTime < d_status.modificationTime THEN
            PushBool (t, TRUE);
            RETURN;
          END;
        END;
      ELSE
        dep := QVal.ToText (t, val2);
        d_status := FS.Status (dep);
        IF t_status.modificationTime < d_status.modificationTime THEN
          PushBool (t, TRUE);
          RETURN;
        END;
      END;
    EXCEPT OSError.E =>
      PushBool (t, TRUE);
      RETURN;
    END;

    PushBool (t, FALSE);
  END DoStale;

PROCEDURE DoUnlink (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR val: QValue.T;  ok := FALSE;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);
    TRY
      FS.DeleteFile (QVal.ToText (t, val));
      ok := TRUE;
    EXCEPT OSError.E =>
      ok := FALSE;
    END;
    PushBool (t, ok);
  END DoUnlink;

PROCEDURE DoWrite (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR wr := CurWr (t);  buf := GetBuf (t);
  BEGIN
    M3Buf.AttachDrain (buf, wr);
    (* write the arguments & pop the stack *)
    FOR i := t.reg.sp - n_args TO t.reg.sp - 1 DO
      QVal.ToBuf (t, t.stack[i], buf);
      t.stack[i].ref := NIL;
    END;
    t.reg.sp := t.reg.sp - n_args;
    M3Buf.Flush (buf, wr);
    M3Buf.AttachDrain (buf, NIL);
    FreeBuf (t, buf);
    Wr.Flush(wr);
  END DoWrite;

PROCEDURE DoTrace (t: T;  n_args: INTEGER) =
  BEGIN
    <*ASSERT n_args = 0*>
    t.tracing := NOT t.tracing;
  END DoTrace;

PROCEDURE DoLookUp (t: T; n_args: INTEGER) RAISES {Error} = 
  VAR name, default: QValue.T; binding: QValue.Binding;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(default);
    t.pop(name);
    binding := t.lookup(name.int);
    IF binding = NIL THEN
      t.push(default);
    ELSE
      t.push(binding.value);
    END;
  END DoLookUp;

(*-------------------------------------------------------- memory buffers ---*)
(* We don't use TRY/FINALLY or worry about buffers that aren't freed.
   In the rare cases when they're not returned, the collector get them. *)

PROCEDURE GetBuf (t: T): M3Buf.T =
  VAR buf: M3Buf.T;
  BEGIN
    IF (t.buffers.tos > 0) THEN
      DEC (t.buffers.tos);
      WITH z = t.buffers.bufs [t.buffers.tos] DO  buf := z; z := NIL;  END;
    ELSE
      buf := M3Buf.New ();
    END;
    RETURN buf;
  END GetBuf;

PROCEDURE FreeBuf (t: T;  buf: M3Buf.T) =
  BEGIN
    IF (t.buffers.tos < NUMBER (t.buffers.bufs)) THEN
      t.buffers.bufs [t.buffers.tos] := buf;
      INC (t.buffers.tos);
    END;
  END FreeBuf;

(*------------------------------------------------------------ temp files ---*)

PROCEDURE CleanUp (t: T) =
  VAR n: INTEGER;  path: TEXT;
  BEGIN
    IF (t.tmp_files # NIL) THEN
      n := t.tmp_files.size ();
      WHILE (n > 0) DO
        path := t.tmp_files.remlo ();
        TRY
          FS.DeleteFile (path);
        EXCEPT OSError.E =>
          (* ignore *)
        END;
        DEC (n);
      END;
    END;
  END CleanUp;

PROCEDURE Root (): TEXT =
  VAR root : TEXT := NIL;
  BEGIN
    root := Env.Get ("TEMP");
    IF root = NIL THEN
      root := "/tmp/";
    ELSE
      root := root & "\\";
    END;
    root := root & "qk";
    RETURN root;
  END Root;

PROCEDURE UniqueTempFile (t: T): TEXT RAISES {Error} =
  VAR root : TEXT := NIL;
      file := root;  seq := 0;
  BEGIN
    root := Root();
    IF root = NIL THEN
      Err (t, "Unable to create a temporary file");
    END;

    file := root;
    LOOP
      TRY
        EVAL FS.Status (file);
      EXCEPT OSError.E =>
        EXIT;
      END;
      INC (seq);
      file := root & "_" & Fmt.Int (seq);
    END;
    IF (t.tmp_files = NIL) THEN t.tmp_files := NEW (TextSeq.T).init(); END;
    t.tmp_files.addhi (file);
    RETURN file;
  END UniqueTempFile;

(*------------------------------------------------------------------ misc ---*)

PROCEDURE Err (t: T;  msg: TEXT) RAISES {Error} =
  VAR buf := GetBuf (t);  txt: TEXT;
  BEGIN
    M3Buf.PutText (buf, "quake error: ");   (* Added, Jerome Collin 08/08/6 *)
    M3Buf.PutText (buf, "runtime error: ");
    M3Buf.PutText (buf, msg);
    M3Buf.PutText (buf, "\n");
    M3Buf.PutText (buf, "\n--procedure--  -line-  -file---\n");
    DumpFrame (buf, t.reg);
    FOR i := t.reg.fp-1 TO 0 BY -1 DO
      DumpFrame (buf, t.frames[i].saved);
    END;
    txt := M3Buf.ToText (buf);
    FreeBuf (t, buf);
    RAISE Error (txt);
  END Err;

PROCEDURE DumpFrame (buf: M3Buf.T;  READONLY reg: Registers) =
  BEGIN
    IF (reg.pi = NIL)
      THEN Out (buf, "", 13);
      ELSE Out (buf, M3ID.ToText (reg.pi.name), 13);
    END;
    M3Buf.PutText (buf, "  ");
    IF (reg.ln > 0)
      THEN Out (buf, Fmt.Int (reg.ln), -6);
      ELSE Out (buf, "--", -6);
    END;
    M3Buf.PutText (buf, "  ");
    IF (reg.cp = NIL)
      THEN M3Buf.PutText (buf, "<builtin>");
      ELSE M3Buf.PutText (buf, M3ID.ToText (reg.cp.source_file));
    END;
    M3Buf.PutChar (buf, '\n');
  END DumpFrame;

PROCEDURE Out (buf: M3Buf.T;  txt: TEXT;  width: INTEGER) =
  VAR len := Text.Length (txt);
  BEGIN
    IF (width < 0) THEN
      width := -width;
      WHILE (len < width) DO M3Buf.PutChar (buf, ' '); INC (len); END;
      M3Buf.PutText (buf, txt);
    ELSE
      M3Buf.PutText (buf, txt);
      WHILE (len < width) DO M3Buf.PutChar (buf, ' '); INC (len); END;
    END;
  END Out;

PROCEDURE OSErr (args: AtomList.T): TEXT =
  VAR msg : TEXT := NIL;
  BEGIN
    WHILE (args # NIL) DO
      IF (msg = NIL) THEN  msg := ": ";  ELSE  msg := msg & "  ***  ";  END;
      msg  := msg & Atom.ToText (args.head);
      args := args.tail;
    END;
    RETURN msg;
  END OSErr;

PROCEDURE CanonicalizePath (path: Pathname.Arcs): Pathname.Arcs =
  (* Remove '..' and '.' components from "path". *)
  VAR found := FALSE;  arc: TEXT;  new: Pathname.Arcs;  pending: INTEGER;
  BEGIN
    FOR i := 0 TO path.size () - 1 DO
      arc := path.get (i);
      IF Text.Equal (arc, Pathname.Current)
        OR Text.Equal (arc, Pathname.Parent) THEN
        found := TRUE;
        EXIT;
      END;
    END;

    IF NOT found THEN RETURN path; END;

    new := NEW(Pathname.Arcs).init();
    pending := 0;
    FOR i := 0 TO path.size() - 1 DO
      arc := path.get(i);
      IF Text.Equal (arc, Pathname.Current) THEN
        (* skip it *)
      ELSIF Text.Equal(arc, Pathname.Parent) THEN
        IF pending > 0 THEN
          DEC(pending);
          EVAL new.remhi();
        ELSE
          new.addhi(arc);
        END;
      ELSE
        INC(pending);
        new.addhi(arc);
      END;
    END;

    RETURN new;
  END CanonicalizePath;

PROCEDURE StripPrefix (t: T;  prefix, path: Pathname.Arcs): Pathname.Arcs
  RAISES {Error} =
  BEGIN
    TRY
      IF NOT Pathname.Absolute(Pathname.Compose(path))
           OR NOT Pathname.Absolute(Pathname.Compose(prefix)) THEN
        RETURN path;
      END;
    EXCEPT Pathname.Invalid =>
      Err (t, "internal error: invalid pathname in StripPrefix");
    END;

    (* check whether prefix is  a prefix of path *)
    IF prefix.size() > path.size() THEN RETURN path; END;

    FOR i := 0 TO prefix.size() - 1 DO
      IF NOT Text.Equal(prefix.get(i), path.get(i)) THEN
        (* This is not a prefix *)
        RETURN path;
      END;
    END;

    VAR sub := TextSeq.Sub(path, prefix.size());
    BEGIN
      sub.addlo(NIL);           (* make relative *)
      RETURN sub;
    END;
  END StripPrefix;

PROCEDURE SplitArgs (txt: TEXT): TextSeq.T =
  VAR
    seq   := NEW (TextSeq.T).init ();
    i     := 0;
    len   := Text.Length (txt);
    start : INTEGER;
  BEGIN
    WHILE i < len DO
      WHILE i < len AND QScanner.WhiteSpace [txt [i]] DO INC(i); END;
      start := i;
      WHILE i < len AND NOT QScanner.WhiteSpace [txt [i]] DO INC(i); END;
      IF i > start THEN seq.addhi (Text.Sub (txt, start, i - start)); END;
    END;
    RETURN seq;
  END SplitArgs;

BEGIN
END QMachine.
