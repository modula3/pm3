(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE MODULE RTExFrame EXPORTS RTException, RTExFrame;

IMPORT RT0, RTError, RTIO, RTParams, RTOS, RTThread;
IMPORT Thread, Csetjmp;
IMPORT Cstring, RTProcedure, RTStack, RTProcedureSRC;

VAR
  DEBUG := FALSE;
  dump_enabled := FALSE;

EXCEPTION
  OUCH; (* to keep the compiler from complaining *)

PROCEDURE Raise (VAR act: RT0.RaiseActivation) RAISES ANY =
  VAR
    f := LOOPHOLE(RTThread.GetCurrentHandlers(), Frame);
    ex: ExceptionList;
  BEGIN
    IF DEBUG THEN
      PutExcept ("RAISE", act);
      RTIO.Flush ();
      DumpStack ();
    END;

    LOOP
      IF (f = NIL) THEN InvokeBackstop (act, raises := FALSE); END;

      CASE f.class OF
      | ORD (ScopeKind.Except) =>
          ex := LOOPHOLE (f, PF1).handles;
          WHILE (ex^ # 0) DO
            IF (ex^ = act.exception.uid) THEN ResumeRaise (act) END;
            INC (ex, ADRSIZE (ex^));
          END;
      | ORD (ScopeKind.ExceptElse) =>
          (* 's' is a TRY-EXCEPT-ELSE frame => go for it *)
          ResumeRaise (act);
      | ORD (ScopeKind.Finally),
        ORD (ScopeKind.FinallyProc),
        ORD (ScopeKind.Lock) =>
          (* ignore for this pass *)
      | ORD (ScopeKind.Raises) =>
          IF (act.exception.implicit = 0) THEN
            (* check that this procedure does indeed raise 'en' *)
            ex := LOOPHOLE (f, PF3).raises;
            IF ex = NIL THEN InvokeBackstop (act, raises := TRUE); END;
            LOOP
              IF (ex^ = 0) THEN  InvokeBackstop (act, raises := TRUE) END;
              IF (ex^ = act.exception.uid)  THEN  (* ok, it passes *) EXIT  END;
              INC (ex, ADRSIZE (ex^));
            END;
          END;
      | ORD (ScopeKind.RaisesNone) =>
          IF (act.exception.implicit = 0) THEN
            InvokeBackstop (act, raises := TRUE);
          END;
      ELSE
        BadStack ();
      END;

      f := f.next;   (* try the previous frame *)
    END;
  END Raise;

PROCEDURE ResumeRaise (VAR a: RT0.RaiseActivation) RAISES ANY =
  VAR
    f := LOOPHOLE(RTThread.GetCurrentHandlers(), Frame);
    ex: ExceptionList;
  BEGIN
    IF DEBUG THEN
      PutExcept ("RERAISE", a);
      RTIO.Flush ();
      DumpStack ();
    END;

    LOOP
      IF (f = NIL) THEN  BadStack ();  END;

      CASE f.class OF
      | ORD (ScopeKind.ExceptElse),
        ORD (ScopeKind.Finally) =>
          InvokeHandler (f, a);
      | ORD (ScopeKind.Except) =>
          ex := LOOPHOLE (f, PF1).handles;
          WHILE (ex^ # 0) DO
            IF (ex^ = a.exception.uid) THEN InvokeHandler (f, a) END;
            INC (ex, ADRSIZE (ex^));
          END;
      | ORD (ScopeKind.FinallyProc) =>
          InvokeFinallyHandler (f, a);
      | ORD (ScopeKind.Lock) =>
          ReleaseLock (f);
      | ORD (ScopeKind.Raises), ORD (ScopeKind.RaisesNone) =>
          (* already checked during the first pass *)
      ELSE
          BadStack ();
      END;

      RTThread.SetCurrentHandlers (f.next); (* cut to the new handler *)
      f := f.next;                         (* try the previous frame *)
    END;
  END ResumeRaise;

PROCEDURE InvokeHandler (f: Frame;  READONLY a: RT0.RaiseActivation) RAISES ANY =
  VAR p := LOOPHOLE (f, PF1);
  BEGIN
    IF DEBUG THEN
      PutExcept ("INVOKE HANDLER", a);
      RTIO.PutText ("  frame=");  RTIO.PutAddr (f);
      RTIO.PutText ("  class=");  RTIO.PutInt (f.class);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;
    RTThread.SetCurrentHandlers (f.next); (* cut to the new handler *)
    p.info := a;                         (* copy the exception to the new frame *)
    Csetjmp.ulongjmp (p.jmpbuf, 1);      (* and jump... *)
    RAISE OUCH;
  END InvokeHandler;

PROCEDURE InvokeFinallyHandler (f: Frame;  VAR a: RT0.RaiseActivation) RAISES ANY =
  VAR
    p := LOOPHOLE (f, PF2);
    cl: RT0.ProcedureClosure;
  BEGIN
    IF DEBUG THEN
      PutExcept ("INVOKE FINALLY HANDLER", a);
      RTIO.PutText ("  frame=");  RTIO.PutAddr (f);
      RTIO.PutText ("  class=");  RTIO.PutInt (f.class);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;

    (* build a nested procedure closure  *)
    cl.marker := RT0.ClosureMarker;
    cl.proc   := p.handler;
    cl.frame  := p.frame;
    
    RTThread.SetCurrentHandlers (f.next); (* cut to the new handler *)
    CallProc (LOOPHOLE (ADR (cl), FinallyProc), a);
  END InvokeFinallyHandler;

PROCEDURE CallProc (p: FinallyProc;  VAR a: RT0.RaiseActivation) RAISES ANY =
  (* we need to fool the compiler into generating a call
     to a nested procedure... *)
  BEGIN
    p (a);
  END CallProc;

PROCEDURE ReleaseLock (f: Frame) =
  VAR p := LOOPHOLE (f, PF4);
  BEGIN
    IF DEBUG THEN
      RTIO.PutText ("--> UNLOCK:");
      RTIO.PutText ("  frame=");  RTIO.PutAddr (p);
      RTIO.PutText ("  mutex=");  RTIO.PutAddr (LOOPHOLE (p.mutex, ADDRESS));
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;
    RTThread.SetCurrentHandlers (f.next); (* cut to the new handler *)
    Thread.Release (p.mutex);            (* and release the lock *)
  END ReleaseLock;

PROCEDURE BadStack () =
  BEGIN
    RTError.Msg (NIL, 0, "corrupt exception stack");
  END BadStack;

(*----------------------------------------------------------- diagnostics ---*)

PROCEDURE SanityCheck () =
  CONST Min_SK = ORD (FIRST (ScopeKind));
  CONST Max_SK = ORD (LAST (ScopeKind));
  VAR f := LOOPHOLE(RTThread.GetCurrentHandlers(), Frame);
  VAR i: INTEGER;
  BEGIN
    WHILE (f # NIL) DO
      i := f.class;
      IF (i < Min_SK) OR (Max_SK < i) THEN BadStack () END;
      f := f.next;
    END;
  END SanityCheck;

VAR 
  NoName := ARRAY [0..15] OF CHAR {'s','t','a','t','i','c',' ',
      'p','r','o','c','e','d','u','r','e'};

PROCEDURE DumpStack () =
  VAR
    f := LOOPHOLE(RTThread.GetCurrentHandlers(), Frame);
    here, sf: RTStack.Frame;
    name: RTProcedureSRC.Name;
    file: RTProcedureSRC.Name;
    proc: RTProcedure.Proc;
    offset: INTEGER;
  BEGIN
    IF NOT DEBUG AND NOT dump_enabled THEN 
      RTIO.PutText ("  use option @M3stackdump to get a stack trace\n");
      RETURN; 
    END;

    RTOS.LockHeap (); (* disable thread switching... (you wish!) *)

    IF RTStack.Has_walker THEN
      RTIO.PutText ("------------------------- STACK DUMP ---------------------------\n");
      RTIO.PutText ("----PC----  ----SP----  \n");
      RTStack.CurrentFrame (here);
      RTStack.PreviousFrame (here, sf); (* skip self *)

      WHILE (sf.pc # NIL) DO

        RTProcedureSRC.FromPC (sf.pc, proc, file, name);

        (* print the procedure's frame *)
        RTIO.PutAddr (sf.pc, 10);
        RTIO.PutText ("  ");
        RTIO.PutAddr (sf.sp, 10);

        IF (name # NIL) THEN
          offset := sf.pc - proc;
          IF (0 <= offset) AND (offset < 2048) THEN
            RTIO.PutText ("  ");  RTIO.PutString (name);
            IF (offset # 0) THEN 
              RTIO.PutText (" + "); RTIO.PutHex (offset); 
            END;
            IF (file # NIL) THEN 
              RTIO.PutText(" in "); RTIO.PutString(file); 
            END;
          END;
        END;
        name := RTStack.ProcName (sf);
        IF (name # NIL)
          AND Cstring.memcmp (name, ADR(NoName), NUMBER(NoName)) # 0 THEN
          RTIO.PutText ("  [");  RTIO.PutString (name);  RTIO.PutText ("]");
        END;
        RTIO.PutText ("\n");

        (* try the previous frame *)
        RTStack.PreviousFrame (sf, sf);
      END;
      RTIO.PutText ("----------------------------------------------------------------\n");
    END;

    RTIO.PutText ("------------------ EXCEPTION HANDLER STACK ---------------------\n");
    WHILE (f # NIL) DO
      RTIO.PutAddr (f);

      CASE f.class OF
      | ORD (ScopeKind.Except) =>
          RTIO.PutText (" TRY-EXCEPT ");
          DumpHandles (LOOPHOLE (f, PF1).handles);
      | ORD (ScopeKind.ExceptElse) =>
          RTIO.PutText (" TRY-EXCEPT-ELSE ");
      | ORD (ScopeKind.Finally) =>
          RTIO.PutText (" TRY-FINALLY ");
      | ORD (ScopeKind.FinallyProc) =>
          VAR x := LOOPHOLE (f, PF2); BEGIN
            RTIO.PutText (" TRY-FINALLY  proc = ");
            RTIO.PutAddr (x.handler);
            RTIO.PutText ("   frame = ");
            RTIO.PutAddr (x.frame);
          END;
      | ORD (ScopeKind.Raises) =>
          RTIO.PutText (" RAISES ");
          DumpHandles (LOOPHOLE (f, PF3).raises);
      | ORD (ScopeKind.RaisesNone) =>
          RTIO.PutText (" RAISES {}");
      | ORD (ScopeKind.Lock) =>
          VAR x := LOOPHOLE (f, PF4); BEGIN
            RTIO.PutText (" LOCK  mutex = ");
            RTIO.PutAddr (LOOPHOLE (x.mutex, ADDRESS));
          END;
      ELSE
         RTIO.PutText (" *** BAD EXCEPTION RECORD, class = ");
         RTIO.PutInt (f.class);
         RTIO.PutText (" ***\n");
         EXIT;
      END;
      RTIO.PutText ("\n");
      f := f.next;
    END;
    RTIO.PutText ("----------------------------------------------------------------\n");
    RTIO.Flush ();

    RTOS.UnlockHeap (); (* re-enable thread switching *)
  END DumpStack;

PROCEDURE DumpHandles (x: ExceptionList) =
  VAR first := TRUE;
  BEGIN
    RTIO.PutText (" {");
    IF (x # NIL) THEN
      WHILE (x^ # 0) DO
        IF (NOT first) THEN RTIO.PutText (", ");  END;
        first := FALSE;
        RTIO.PutHex (x^);
        INC (x, ADRSIZE (x^));
      END;
    END;
    RTIO.PutText ("}");
  END DumpHandles;

PROCEDURE PutExcept (tag: TEXT;  READONLY a: RT0.RaiseActivation) =
  BEGIN
    RTIO.PutText ("---> ");         RTIO.PutText (tag);
    RTIO.PutText (":  en=");        RTIO.PutAddr (a.exception);
    RTIO.PutText (" uid=");         RTIO.PutHex (a.exception.uid);
    RTIO.Flush ();
    RTIO.PutText (" ");             RTIO.PutString (a.exception.name);
    RTIO.PutText ("  arg=");        RTIO.PutAddr (a.arg);
    RTIO.PutText ("\n  module: ");  RTIO.PutAddr (a.module);
    IF (a.module # NIL) AND (a.module.file # NIL) THEN
      RTIO.PutText ("  ");          RTIO.PutString (a.module.file);
    END;
    RTIO.PutText ("\n  line: ");    RTIO.PutInt (a.line);
    RTIO.PutText ("   pc: ");       RTIO.PutAddr (a.pc);
    RTIO.PutText ("   info0: ");    RTIO.PutAddr (a.info0);
    RTIO.PutText ("   info1: ");    RTIO.PutAddr (a.info1);
    IF (a.un_except # NIL) THEN
      RTIO.PutText ("\n  unhandled: ");
      RTIO.PutText (" ");             RTIO.PutString (a.un_except.name);
      RTIO.PutText ("  arg=");        RTIO.PutAddr (a.un_arg);
    END;
    RTIO.PutText ("\n");
  END PutExcept;

BEGIN
  dump_enabled := RTParams.IsPresent ("stackdump");
  DEBUG := RTParams.IsPresent ("debugex");
  EVAL SanityCheck; (* avoid the unused warning *)
END RTExFrame.
