(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Fri Mar 17 11:12:50 PST 1995 by msm      *)
(*      modified on Thu Feb  2 13:56:06 PST 1995 by kalsow   *)
(*      modified on Mon Nov 22 13:47:01 PST 1993 by steveg   *)
(*      modified on Fri Jul 23 10:26:43 PDT 1993 by sfreeman *)
(* modified on Mon Feb 24 13:59:46 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE MODULE XProperties;

IMPORT XClient, XClientF, VBTClass, TrestleOnX, X, Thread, VBT, Cstring,
       TrestleComm, Text, Ctypes, RefSeq, XConfCtl, TrestleImpl, Point;

(* -- initialise X client -- *)

PROCEDURE InitialiseXClient (xcon: XClient.T) RAISES {TrestleComm.Failure} =
  BEGIN
    WITH wf = NEW(SelRequestWaitFor) DO
      wf.timelimit := -1;
      wf.types[0] := X.SelectionRequest;
      XClientF.RegisterWaiter(xcon, wf);
    END;
    WITH wf = NEW(ConfCtlWaitFor) DO
      wf.timelimit := -1;
      wf.types[0] := X.ClientMessage;
      wf.atom := XClient.ToAtom(xcon, "XMUX_MESSAGE");
      XClientF.RegisterWaiter(xcon, wf)
    END
  END InitialiseXClient;

TYPE
  SelRequestWaitFor = XClientF.WaitFor OBJECT
                      OVERRIDES
                        match  := SRWFMatch;
                        notify := SRWFNotify;
                      END;
  ConfCtlWaitFor = XClientF.WaitFor OBJECT
                     atom: X.Atom
                   OVERRIDES
                     match  := CctMatch;
                     notify := CctNotify;
                   END;

PROCEDURE SRWFMatch (<* UNUSED*>          wf: XClientF.WaitFor;
                                 READONLY ev: X.XEvent          ):
  BOOLEAN =
  BEGIN
    RETURN LOOPHOLE(ADR(ev), X.XAnyEventStar).type = X.SelectionRequest;
  END SRWFMatch;

PROCEDURE CctMatch (wf: ConfCtlWaitFor; READONLY ev: X.XEvent): BOOLEAN =
  BEGIN
    IF LOOPHOLE(ADR(ev), X.XAnyEventStar).type # X.ClientMessage THEN
      RETURN FALSE
    END;
    WITH e = LOOPHOLE(ADR(ev), X.XClientMessageEventStar) DO
      RETURN e.message_type = wf.atom
    END
  END CctMatch;

PROCEDURE SRWFNotify (         wf  : XClientF.WaitFor;
                      READONLY ev  : X.XEvent;
                               xcon: XClient.T         ) =
  BEGIN
    WITH e = LOOPHOLE(ADR(ev), X.XSelectionRequestEventStar) DO
      FOR s := FIRST(xcon.sel^) TO LAST(xcon.sel^) DO
        IF xcon.sel[s].name = e.selection THEN
          StartSelection(xcon, e.requestor, e.target, e.property,
                         VBT.Selection{s}, e.time);
        END;
      END;
    END;
    XClientF.RegisterWaiter(xcon, wf); (* wf will have been removed from
                                          the list *)
  END SRWFNotify;

VAR
  cctMu                      := NEW(MUTEX);
  cctCond                    := NEW(Thread.Condition);
  cctList, focList           := NEW(RefSeq.T).init();
  cctThread       : Thread.T := NIL;

TYPE CctClosure = Thread.Closure OBJECT OVERRIDES apply := CctApply END;

PROCEDURE CctApply (<* UNUSED *> cl: CctClosure): REFANY =
  VAR
    v  : VBT.T;
    cct: BOOLEAN;
  BEGIN
    LOOP
      LOCK cctMu DO
        WHILE cctList.size() + focList.size() = 0 DO
          Thread.Wait(cctMu, cctCond)
        END;
        cct := cctList.size() # 0;
        IF cct THEN v := cctList.remlo() ELSE v := focList.remlo() END
      END;
      IF cct THEN
        XConfCtl.Process(v)
      ELSE
        LOCK VBT.mu DO
          VBTClass.Mouse(
            v, VBT.MouseRec{
                 whatChanged := VBT.Modifier.Mouse4, time := 0, cp :=
                 VBT.CursorPosition{Point.Origin, -1, FALSE, FALSE},
                 modifiers :=
                 VBT.Modifiers{VBT.Modifier.Mod0, VBT.Modifier.Mod1,
                               VBT.Modifier.Mod2, VBT.Modifier.Mod3},
                 clickType := VBT.ClickType.LastUp, clickCount := 0})
        END
      END
    END
  END CctApply;

PROCEDURE CctNotify (         wf  : ConfCtlWaitFor;
                     READONLY ev  : X.XEvent;
                              xcon: XClient.T       ) =
  VAR ra: REFANY;
  BEGIN
    WITH e = LOOPHOLE(ADR(ev), X.XClientMessageEvent_l_star) DO
      IF xcon.vbts.get(e.window, ra) THEN
        LOCK cctMu DO
          IF e.data[0] = 2 THEN
            cctList.addhi(ra);
          ELSIF e.data[0] = 1 AND e.data[1] = 0 THEN
            focList.addhi(ra);
          END;
          IF cctThread = NIL THEN
            cctThread := Thread.Fork(NEW(CctClosure))
          ELSE
            Thread.Signal(cctCond)
          END
        END
      END
    END;
    XClientF.RegisterWaiter(xcon, wf); (* wf will have been removed from
                                          the list *)
  END CctNotify;

(* -- start selection -- *)

TYPE
  SelectionClosure = Thread.SizedClosure OBJECT
                       trsl      : XClient.T;
                       w         : X.Window;
                       type, prop: X.Atom;
                       sel       : VBT.Selection;
                       ts        : VBT.TimeStamp;
                     OVERRIDES
                       apply := DoXSelection
                     END;

PROCEDURE CopyBytes(src, dst: ADDRESS; n: INTEGER) =
  BEGIN
    EVAL Cstring.memcpy(dst, src, n)
  END CopyBytes;

PROCEDURE StartSelection (trsl      : XClient.T;
                          w         : X.Window;
                          type, prop: X.Atom;
                          sel       : VBT.Selection;
                          ts        : VBT.TimeStamp;
                          stackSize                   := 20000) =
  BEGIN
    EVAL Thread.Fork(
           NEW(SelectionClosure, stackSize := stackSize, trsl := trsl,
               w := w, type := type, prop := prop, sel := sel, ts := ts));

  END StartSelection;

PROCEDURE ExtendSel (VAR sa: XClientF.SelArray; s: VBT.Selection) =
  VAR
    n                     := NUMBER(sa^);
    na: XClientF.SelArray;
  BEGIN
    IF s.sel > LAST(sa^) THEN
      na := NEW(XClientF.SelArray, MAX(2 * n, s.sel + 1));
      SUBARRAY(na^, 0, n) := sa^;
      FOR i := n TO LAST(na^) DO na[i] := XClientF.SelectionRecord{} END;
      sa := na
    END
  END ExtendSel;

PROCEDURE ExtendOwns (VAR sa: XClientF.OwnsArray; s: VBT.Selection) =
  VAR
    n                      := NUMBER(sa^);
    na: XClientF.OwnsArray;
  BEGIN
    IF s.sel > LAST(sa^) THEN
      na := NEW(XClientF.OwnsArray, MAX(2 * n, s.sel + 1));
      SUBARRAY(na^, 0, n) := sa^;
      FOR i := n TO LAST(na^) DO na[i] := FALSE END;
      sa := na
    END
  END ExtendOwns;

PROCEDURE DoXSelection (cl: SelectionClosure): REFANY RAISES {} =
  VAR
    failed        := FALSE;
    alloc         := FALSE;
    v     : VBT.T;
  BEGIN
    TRY
      TRY
        TrestleOnX.Enter(cl.trsl);
        TRY
          ExtendSel(cl.trsl.sel, cl.sel);
          v := cl.trsl.sel[cl.sel.sel].v;
          IF v = NIL THEN failed := TRUE; RETURN NIL END;
          IF cl.prop = X.None THEN
            cl.prop := XClientF.NewAtom(v);
            alloc := TRUE
          END
        FINALLY
          TrestleOnX.Exit(cl.trsl)
        END;
        failed := NOT EvalSelection(cl, v, cl.type, cl.prop)
      FINALLY
        TrestleOnX.Enter(cl.trsl);
        TRY
          VAR ev: X.XSelectionEvent;
          BEGIN
            ev.type := X.SelectionNotify;
            ev.display := cl.trsl.dpy;
            ev.requestor := cl.w;
            ev.selection := cl.trsl.sel[cl.sel.sel].name;
            ev.target := cl.type;
            IF failed THEN
              ev.property := X.None
            ELSE
              ev.property := cl.prop
            END;
            ev.time := cl.ts;
            EVAL X.XSendEvent(cl.trsl.dpy, cl.w, X.False, 0,
                              LOOPHOLE(ADR(ev),X.XEventStar));
            IF alloc THEN XClientF.FreeAtom(v, cl.prop) END
          END
        FINALLY
          TrestleOnX.Exit(cl.trsl)
        END
      END
    EXCEPT
      X.Error, TrestleComm.Failure =>    (* skip *)
    END;
    RETURN NIL
  END DoXSelection;

PROCEDURE EvalSelection (cl        : SelectionClosure;
                         owner     : VBT.T;
                         type, prop: X.Atom            ): BOOLEAN
  RAISES {TrestleComm.Failure} =
  VAR
    ntype, nprop: X.Atom;
    format      : INTEGER;
    data        : REF ARRAY OF CHAR;
    p           : UNTRACED REF X.Atom;
    anyFail                           := FALSE;
    ts          : VBT.TimeStamp;
    multiple, atompair, targets, timestamp, string, text, atom, intatom,
    delete, insprop, inssel, null, sym, rsym, length: X.Atom;
    res: TEXT;
    ur : XClientF.Child;
  BEGIN
    TrestleOnX.Enter(cl.trsl);
    TRY
      length := XClient.ToAtom(cl.trsl, "LENGTH");
      multiple := XClient.ToAtom(cl.trsl, "MULTIPLE");
      atompair := XClient.ToAtom(cl.trsl, "ATOM_PAIR");
      targets := XClient.ToAtom(cl.trsl, "TARGETS");
      timestamp := XClient.ToAtom(cl.trsl, "TIMESTAMP");
      string := XClient.ToAtom(cl.trsl, "STRING");
      text := XClient.ToAtom(cl.trsl, "TEXT");
      atom := XClient.ToAtom(cl.trsl, "ATOM");
      intatom := XClient.ToAtom(cl.trsl, "INTEGER");
      delete := XClient.ToAtom(cl.trsl, "DELETE");
      insprop := XClient.ToAtom(cl.trsl, "INSERT_PROPERTY");
      inssel := XClient.ToAtom(cl.trsl, "INSERT_SELECTION");
      null := XClient.ToAtom(cl.trsl, "NULL");
      ExtendSel(cl.trsl.sel, cl.sel);
      ts := cl.trsl.sel[cl.sel.sel].ts
    FINALLY
      TrestleOnX.Exit(cl.trsl)
    END;
    IF type = multiple THEN
      IF NOT UnlockedGetProp(cl.trsl, cl.w, prop, ntype, data, format)
           OR format # 32 OR ntype # atompair OR NUMBER(data^) MOD 8 # 0 THEN
        RETURN FALSE
      END;
      FOR i := 0 TO LAST(data^) BY 8 DO
        p := LOOPHOLE(ADR(data[i]), UNTRACED REF X.Atom);
        ntype := p^;
        p := LOOPHOLE(ADR(data[i + 4]), UNTRACED REF X.Atom);
        nprop := p^;
        IF NOT EvalSelection(cl, owner, ntype, nprop) THEN
          p^ := X.None;
          anyFail := TRUE
        END
      END;
      IF anyFail THEN
        UnlockedPutProp(cl.trsl, cl.w, prop, atompair, data^, 32)
      END
    ELSIF type = targets THEN
      VAR
        td := ARRAY [0 .. 4] OF
                Ctypes.int{multiple, targets, timestamp, string, text};
      BEGIN
        UnlockedPutProp(cl.trsl, cl.w, prop, atom,
                        LOOPHOLE(td, ARRAY [0 .. 19] OF CHAR), 32)
      END
    ELSIF type = timestamp THEN
      VAR tts: Ctypes.int := ts; BEGIN
        UnlockedPutProp(cl.trsl, cl.w, prop, intatom,
                        LOOPHOLE(tts, ARRAY [0 .. 3] OF CHAR), 32)
      END
    ELSIF type = text OR type = string THEN
      TRY
        TYPECASE owner.read(cl.sel, TYPECODE(TEXT)).toRef() OF
          NULL => RETURN FALSE
        | TEXT (t) =>
            VAR buf := NEW(UNTRACED REF ARRAY OF CHAR, Text.Length(t));
            BEGIN
              IF Text.Length(t) > 0 THEN Text.SetChars(buf^, t) END;
              UnlockedPutProp(cl.trsl, cl.w, prop, string, buf^, 8);
              DISPOSE(buf)
            END
        ELSE
          RETURN FALSE
        END
      EXCEPT
        VBT.Error => RETURN FALSE
      END;
    ELSIF type = delete THEN
      TRY
        owner.write(cl.sel, VBT.FromRef(""), TYPECODE(TEXT))
      EXCEPT
        VBT.Error => RETURN FALSE
      END
    ELSIF type = insprop THEN
      IF NOT UnlockedGetProp(cl.trsl, cl.w, prop, ntype, data, format)
           OR ntype # string OR format # 8 THEN
        RETURN FALSE
      END;
      TRY
        owner.write(
          cl.sel, VBT.FromRef(Text.FromChars(data^)), TYPECODE(TEXT))
      EXCEPT
        VBT.Error => RETURN FALSE
      END
    ELSIF type = inssel THEN
      TrestleOnX.Enter(cl.trsl);
      TRY
        ur := owner.upRef;
        IF ur = NIL OR ur.xcage = X.None THEN RETURN FALSE END;
        IF NOT GetProp(cl.trsl, cl.w, prop, ntype, data, format)
             OR ntype # atompair OR format # 32 OR NUMBER(data^) # 8 THEN
          RETURN FALSE
        END;
        p := LOOPHOLE(ADR(data[0]), UNTRACED REF X.Atom);
        nprop := p^;
        p := LOOPHOLE(ADR(data[4]), UNTRACED REF X.Atom);
        ntype := p^;
        sym := XClientF.NewAtom(cl.trsl);
        TRY
          IF ntype = text THEN ntype := string END;
          rsym :=
            AwaitConversion(cl.trsl, ur.xcage, nprop, ntype, sym, cl.ts);
          IF rsym # sym THEN XClientF.FreeAtom(cl.trsl, sym) END;
          res := ReadXSelFromProp(cl.trsl, ur.xcage, rsym, ntype);
          XClientF.FreeAtom(cl.trsl, sym);
        EXCEPT
          VBT.Error (ec) =>
            IF ec # VBT.ErrorCode.TimeOut THEN
              XClientF.FreeAtom(cl.trsl, sym)
            END;
            RETURN FALSE
        END
      FINALLY
        TrestleOnX.Exit(cl.trsl)
      END;
      TRY
        owner.write(cl.sel, VBT.FromRef(res), TYPECODE(TEXT))
      EXCEPT
        VBT.Error => RETURN FALSE
      END
    ELSIF type = length THEN
      TRY
        TYPECASE owner.read(cl.sel, TYPECODE(TEXT)).toRef() OF
          NULL => RETURN FALSE
        | TEXT (t) =>
            VAR lnth: Ctypes.int := Text.Length(t);
            BEGIN
              UnlockedPutProp(cl.trsl, cl.w, prop, intatom,
                              LOOPHOLE(lnth, ARRAY [0 .. 3] OF CHAR), 32);
            END
        ELSE
          RETURN FALSE
        END
      EXCEPT
        VBT.Error => RETURN FALSE
      END
    ELSE
      RETURN FALSE
    END;
    RETURN TRUE
  END EvalSelection;

TYPE
  SelectionWaitFor = XClientF.SimpleWaitFor OBJECT
                       sel, type, prop: X.Atom;
                       sent           : BOOLEAN       := FALSE;
                       ts             : VBT.TimeStamp := 0;
                     OVERRIDES
                       match := SelectionMatch;
                     END;

PROCEDURE SelectionMatch (wf: SelectionWaitFor; READONLY ev: X.XEvent):
  BOOLEAN =
  BEGIN
    IF NOT XClientF.SimpleWaitFor.match(wf, ev) THEN RETURN FALSE END;
    WITH e    = LOOPHOLE(ADR(ev), X.XAnyEventStar),
         type = e.type                              DO
      IF type # X.SelectionNotify THEN RETURN TRUE END;
      WITH pe = LOOPHOLE(e, X.XSelectionEventStar) DO
        wf.prop := pe.property;
        wf.sent := pe.send_event # X.False;
        RETURN
          pe.selection = wf.sel AND pe.target = wf.type AND pe.time = wf.ts
      END
    END
  END SelectionMatch;

PROCEDURE AwaitConversion (v              : XClient.T;
                           w              : X.Window;
                           name, type, sym: X.Atom;
                           ts             : VBT.TimeStamp;
                           limit                            := 10): X.Atom
  RAISES {VBT.Error} =
  VAR
    wf := NEW(SelectionWaitFor, d := w, sel := name, ts := ts,
              type := type, prop := X.None);
  BEGIN
    TRY
      wf.reqno := X.XNextRequest(v.dpy);
      X.XConvertSelection(v.dpy, name, type, sym, w, ts);
      wf.types[0] := 0;
      wf.types[1] := X.SelectionNotify;
      IF XClientF.Await(v, wf, limit) = 1 THEN
        RAISE VBT.Error(VBT.ErrorCode.TimeOut)
      ELSIF wf.prop = X.None THEN
        IF wf.sent THEN
          RAISE VBT.Error(VBT.ErrorCode.Unreadable)
        ELSE
          RAISE VBT.Error(VBT.ErrorCode.UnownedSelection)
        END
      END;
      RETURN wf.prop
    EXCEPT
      X.Error, TrestleComm.Failure => RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    END
  END AwaitConversion;

PROCEDURE ReadXSelFromProp (v: XClient.T; w: X.Window; prop, type: X.Atom):
  TEXT RAISES {VBT.Error} =
  VAR
    propType: X.Atom;
    format  : INTEGER;
    res     : REF ARRAY OF CHAR;
    resT                        := "";
    pwf                         := NEW(PropertyWaitFor);
  BEGIN
    TRY
      IF NOT GetProp(v, w, prop, propType, res, format) THEN
        RAISE VBT.Error(VBT.ErrorCode.Unreadable)
      ELSIF propType # type AND propType # XClient.ToAtom(v, "INCR") THEN
        RAISE VBT.Error(VBT.ErrorCode.WrongType)
      ELSIF propType = type THEN
        IF type # XClient.ToAtom(v, "STRING") THEN Crash() END;
        RETURN Text.FromChars(res^)
      ELSE
        pwf.types[0] := X.PropertyNotify;
        pwf.d := w;
        pwf.a := prop;
        LOOP
          IF XClientF.Await(v, pwf, 10) = 1 THEN
            RAISE VBT.Error(VBT.ErrorCode.TimeOut)
          ELSIF pwf.state = X.PropertyNewValue THEN
            IF NOT GetProp(v, w, prop, propType, res, format) THEN
              RAISE VBT.Error(VBT.ErrorCode.Unreadable)
            ELSIF propType # type THEN
              RAISE VBT.Error(VBT.ErrorCode.WrongType)
            ELSIF NUMBER(res^) = 0 THEN
              IF type # XClient.ToAtom(v, "STRING") THEN Crash() END;
              RETURN resT
            END;
            resT := resT & Text.FromChars(res^)
          END
        END
      END
    EXCEPT
      TrestleComm.Failure => RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    END
  END ReadXSelFromProp;

REVEAL
  PropertyWaitFor =
    PWF_Public BRANDED OBJECT OVERRIDES match := PropertyMatch END;

PROCEDURE PropertyMatch (wf: PropertyWaitFor; READONLY ev: X.XEvent):
  BOOLEAN =
  BEGIN
    IF NOT XClientF.SimpleWaitFor.match(wf, ev) THEN RETURN FALSE END;
    WITH e    = LOOPHOLE(ADR(ev), X.XAnyEventStar),
         type = e.type                              DO
      IF type # X.PropertyNotify THEN RETURN TRUE END;
      WITH pe = LOOPHOLE(e, X.XPropertyEventStar) DO
        wf.ts := pe.time;
        wf.state := pe.state;
        RETURN pe.atom = wf.a
      END
    END
  END PropertyMatch;

PROCEDURE UnlockedPutProp (         trsl      : XClient.T;
                                    w         : X.Window;
                                    prop, type: X.Atom;
                           READONLY data      : ARRAY OF CHAR;
                                    format    : INTEGER        )
  RAISES {TrestleComm.Failure} =
  BEGIN
    TrestleOnX.Enter(trsl);
    TRY
      PutProp(trsl, w, prop, type, data, format)
    FINALLY
      TrestleOnX.Exit(trsl)
    END
  END UnlockedPutProp;

PROCEDURE PutProp (         v         : XClient.T;
                            w         : X.Window;
                            prop, type: X.Atom;
                   READONLY data      : ARRAY OF CHAR;
                            format    : INTEGER        )
  RAISES {TrestleComm.Failure} =
  VAR
    st               := 0;
    len              := NUMBER(data);
    n      : INTEGER;
    mode             := X.PropModeReplace;
    maxSize          := 4 * (X.XMaxRequestSize(v.dpy) - 50);
    p      : ADDRESS;
    format8          := format DIV 8;
  BEGIN
    TRY
    REPEAT
      n := MIN(len - st, maxSize);
      IF n # 0 THEN p := ADR(data[st]) ELSE p := ADR(p) END;
      X.XChangeProperty(
        v.dpy, w, prop, type, format, mode, p, n DIV format8);
      INC(st, n);
      mode := X.PropModeAppend
    UNTIL st = len;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END PutProp;

PROCEDURE UnlockedGetProp (            trsl  : XClient.T;
                                       w     : X.Window;
                                       prop  : X.Atom;
                           VAR (*OUT*) type  : X.Atom;
                           VAR (*OUT*) data  : REF ARRAY OF CHAR;
                           VAR (*OUT*) format: INTEGER            ):
  BOOLEAN RAISES {TrestleComm.Failure} =
  BEGIN
    TrestleOnX.Enter(trsl);
    TRY
      RETURN GetProp(trsl, w, prop, type, data, format)
    FINALLY
      TrestleOnX.Exit(trsl)
    END
  END UnlockedGetProp;

PROCEDURE GetProp (            v     : XClient.T;
                               w     : X.Window;
                               prop  : X.Atom;
                   VAR (*OUT*) type  : X.Atom;
                   VAR (*OUT*) res   : REF ARRAY OF CHAR;
                   VAR (*OUT*) format: INTEGER            ): BOOLEAN
  RAISES {TrestleComm.Failure} =
  VAR
    len, remaining: INTEGER;
    data          : Ctypes.char_star;
    maxSize                           := X.XMaxRequestSize(v.dpy) - 50;
    st                                := 0;
    fmt: Ctypes.int := 0;
  BEGIN
    TRY
    IF X.XGetWindowProperty(
         v.dpy, w, prop, 0, maxSize, X.True, X.AnyPropertyType, ADR(type),
         ADR(fmt), ADR(len), ADR(remaining),
         LOOPHOLE(ADR(data), Ctypes.unsigned_char_star_star)) # X.Success THEN
      RETURN FALSE
    END;
    format := fmt;
    len := len * (format DIV 8);
    res := NEW(REF ARRAY OF CHAR, len + remaining);
    LOOP
      IF len # 0 THEN
        CopyBytes(data, ADR(res[st]), MIN(len, NUMBER(res^) - st))
      END;
      INC(st, len);
      X.XFree(data);
      IF remaining = 0 OR st >= NUMBER(res^) THEN
        RETURN remaining = 0 AND st = NUMBER(res^)
      END;
      IF X.XGetWindowProperty(
           v.dpy, w, prop, st DIV 4, maxSize, X.True, X.AnyPropertyType,
           ADR(type), ADR(fmt), ADR(len), ADR(remaining),
           LOOPHOLE(ADR(data), Ctypes.unsigned_char_star_star))
           # X.Success THEN
        RETURN FALSE
      END;
      format := fmt;
      IF len = 0 THEN X.XFree(data); RETURN FALSE END;
      len := len * (format DIV 8)
    END
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END GetProp;

EXCEPTION FatalError;

PROCEDURE Crash() =        
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError
  END Crash;

BEGIN
END XProperties.

