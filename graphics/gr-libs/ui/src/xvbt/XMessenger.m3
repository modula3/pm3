(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Tue Jan 31 09:06:40 PST 1995 by kalsow *)
(*      modified on Fri Jul  8 17:10:22 PDT 1994 by msm    *)
(*      modified on Mon Nov 22 13:56:14 PST 1993 by steveg *)
(* modified on Mon Feb 24 13:59:46 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE MODULE XMessenger;

IMPORT XClient, XClientF, TrestleClass, VBT, VBTClass, XEventQueue, X,
       Thread, Split, TrestleComm, VBTRep, Ctypes, Word, Rect, XProperties,
       Region, DpyFilter, TrestleOnX, MiscDetail, Text, Point, IntRefTbl;

TYPE
  Closure = Thread.SizedClosure OBJECT
              xcon: XClient.T
            OVERRIDES
              apply := Messenger
            END;

PROCEDURE Start (trsl: XClient.T; stackSize := 20000) =
  BEGIN
    EVAL Thread.Fork(NEW(Closure, xcon := trsl, stackSize := stackSize));
  END Start;

TYPE
  Last = RECORD
           x, y                             : INTEGER  := 0;
           root                             : X.Window := X.None;
           time                             : X.Time   := 0;
           button                                      := -1;
           clickCount                       : CARDINAL := 0;
           safetyRadius, doubleClickInterval: CARDINAL := 0;
           keysym                           : X.KeySym := X.None;
           xcompstatus := X.XComposeStatus{NIL, 0};
         END;
(* last{x,y} = position of last mouseclick; lastRoot = root window of last
   mouseclick; lastTime = time of last mouseClick; lastClickCount =
   clickcount of last mouseclick, as defined in the VBT interface;
   lastButton = button that last went up or down. *)

PROCEDURE Owns (ur: XClientF.Child; s: VBT.Selection): BOOLEAN =
  BEGIN
    RETURN s.sel < NUMBER(ur.owns^) AND ur.owns[s.sel]
  END Owns;

PROCEDURE Messenger (self: Closure): REFANY RAISES {} =
  VAR
    evRec          : X.XEvent;
    ev             := LOOPHOLE(ADR(evRec), X.XAnyEventStar);
    ra             : REFANY;
    v              : VBT.T;
    ur             : XClientF.Child;
    last                             := Last{};
    lost, takeFocus: BOOLEAN;
    xcon                             := self.xcon;
  <*FATAL XEventQueue.Exhausted*>
  BEGIN
    TRY
      LOCK xcon DO XClientF.AdjustCoverage(xcon, 1) END;
      LOOP
        LOCK xcon DO
          XClientF.AdjustCoverage(xcon, -1);
          WHILE XEventQueue.IsEmpty(xcon.evq) AND NOT xcon.dead DO
            Thread.Wait(xcon, xcon.evc)
          END;
          IF xcon.dead THEN EXIT END;
          evRec := XEventQueue.Remove(xcon.evq);
          IF ev.type = X.MappingNotify THEN
            v := NIL;
            ur := NIL;
            WITH e = LOOPHOLE(ev, X.XMappingEventStar) DO
              IF e.request # X.MappingPointer THEN
                X.XRefreshKeyboardMapping(
                    LOOPHOLE(ADR(evRec), X.XMappingEventStar))
              END
            END;
            XClientF.SetUngrabs(xcon)
          ELSIF xcon.vbts.get(ev.window, ra) THEN
            v := ra;
            ur := v.upRef;
            IF ur # NIL THEN
              VAR
                owns  := Owns(ur, VBT.KBFocus);
                ownsX := ur.isXFocus OR ur.inside AND ur.underXFocus;
              BEGIN
                lost := owns AND NOT ownsX;
                takeFocus := NOT owns AND ownsX AND ur.recentlyOutside
              END
            ELSE
              lost := FALSE;
              takeFocus := FALSE
            END;
            last.safetyRadius := 3 (* xcon.params.safetyRadius*);
            last.doubleClickInterval :=
              500 (* xcon.params.doubleClickInterval*);
          ELSE
            v := NIL;
            ur := NIL;
            lost := FALSE;
            takeFocus := FALSE
          END
        END;
        XClientF.AdjustCoverage(xcon, 1);
        IF ur # NIL
             OR (ev.type = X.EnterNotify OR ev.type = X.MotionNotify)
                  AND ev.window = LOOPHOLE(ev, X.XMotionEventStar).root THEN
          HandleEvent(v, xcon, ur, last, lost, takeFocus, ev)
        END
      END
    EXCEPT
      X.Error, TrestleComm.Failure =>    (* skip *)
    END;
    LOCK VBT.mu DO
      VAR
        vbts : REF ARRAY OF VBT.T;
        iter: IntRefTbl.Iterator;
        key, i: INTEGER;
        val: REFANY;
      BEGIN
        LOCK xcon DO
          vbts := NEW(REF ARRAY OF VBT.T, xcon.vbts.size());
          iter := xcon.vbts.iterate();
          i := 0;
          WHILE iter.next(key, val) DO vbts[i] := val; INC(i); END;
        END;
        FOR i := 0 TO LAST(vbts^) DO
          XClientF.Delete(xcon, vbts[i], vbts[i].upRef);
        END
      END
    END;
    RETURN NIL
  END Messenger;

PROCEDURE HandleEvent (    v              : VBT.T;
                           xcon           : XClient.T;
                           ur             : XClientF.Child;
                       VAR last           : Last;
                           lost, takeFocus: BOOLEAN;
                           ev             : X.XAnyEventStar )
  RAISES {TrestleComm.Failure} =
  VAR
    width, height: CARDINAL;
    junk         : ARRAY [0 .. 0] OF Ctypes.char;
    junkRef      : REFANY;
  BEGIN
    LOCK VBT.mu DO
      IF v = NIL OR v.upRef # NIL THEN
        VBTRep.CoverRedisplay();
        TRY
          CASE ev.type OF
            X.KeyPress, X.KeyRelease =>
              WITH e = LOOPHOLE(ev, X.XKeyEventStar) DO
                LOCK xcon DO
                  EVAL X.XLookupString(
                         e, ADR(junk[0]), NUMBER(junk), ADR(last.keysym),
                         ADR(last.xcompstatus))
                END;
                VAR state: INTEGER := e.state;
                BEGIN
                  VBTClass.Key(
                    v, VBT.KeyRec{last.keysym, e.time, e.type = X.KeyPress,
                                  LOOPHOLE(state, VBT.Modifiers)})
                END
              END
          | X.ButtonPress, X.ButtonRelease =>
              WITH e = LOOPHOLE(ev, X.XButtonEventStar) DO
                ButtonEvent(v, xcon, ur, last, e)
              END
          | X.EnterNotify, X.LeaveNotify =>
              WITH e = LOOPHOLE(ev, X.XCrossingEventStar) DO
                EnterLeave(v, xcon, ur, lost, takeFocus, e)
              END
          | X.MotionNotify =>
            (* WITH e = LOOPHOLE(ev, X.MotionEventStar) DO VAR cd:
               VBT.PositionRec; BEGIN cd.time := e.time; cd.modifiers :=
               LOOPHOLE(e.state, VBT.Modifiers); cd.cp.pt.h := ... *)
          | X.FocusIn, X.FocusOut =>
              IF lost THEN
                LOCK xcon DO
                  XProperties.ExtendOwns(ur.owns, VBT.KBFocus);
                  XProperties.ExtendSel(xcon.sel, VBT.KBFocus);
                  ur.owns[VBT.KBFocus.sel] := FALSE;
                  IF xcon.sel[VBT.KBFocus.sel].v = v THEN
                    xcon.sel[VBT.KBFocus.sel].v := NIL
                  END
                END;
                VBTClass.Misc(
                  v, VBT.MiscRec{VBT.Lost, VBT.NullDetail, 0, VBT.KBFocus})
              END
          | X.Expose, X.GraphicsExpose => DeliverBadRegion(v, ur)
          | X.DestroyNotify =>
              IF ev.window = ur.w THEN
                XClientF.Delete(xcon, v, ur)
              ELSE
                LOCK xcon DO EVAL xcon.vbts.delete(ev.window, junkRef) END
              END
          | X.ConfigureNotify =>
              WITH e = LOOPHOLE(ev, X.XConfigureEventStar) DO
                LOCK xcon DO XClientF.GetDomain(ur, width, height) END;
                XClientF.Reshape(v, width, height, e.send_event # X.False)
              END
          | X.UnmapNotify, X.MapNotify =>
              LOCK xcon DO XClientF.GetDomain(ur, width, height) END;
              XClientF.Reshape(v, width, height)
          | X.VisibilityNotify =>
              WITH e = LOOPHOLE(ev, X.XVisibilityEventStar) DO
                VBTClass.Misc(v, VBT.MiscRec{TrestleOnX.Visibility,
                                          VBT.MiscCodeDetail{e.state, 0},
                                          0, VBT.NilSel});
              END
          | X.SelectionClear =>
              WITH e = LOOPHOLE(ev, X.XSelectionClearEventStar) DO
                FOR s := FIRST(xcon.sel^) TO LAST(xcon.sel^) DO
                  IF xcon.sel[s].name = e.selection THEN
                    VAR mustDeliver: BOOLEAN;
                    BEGIN
                      LOCK xcon DO
                        XProperties.ExtendOwns(ur.owns, VBT.Selection{s});
                        mustDeliver := ur.owns[s];
                        ur.owns[s] := FALSE;
                        IF xcon.sel[s].v = v THEN xcon.sel[s].v := NIL END
                      END;
                      IF mustDeliver THEN
                        VBTClass.Misc(
                          v, VBT.MiscRec{VBT.Lost, VBT.NullDetail, e.time,
                                         VBT.Selection{s}})
                      END
                    END
                  END
                END
              END
          | X.ClientMessage =>
              WITH e = LOOPHOLE(ev, X.XClientMessageEvent_l_star) DO
                ClientMessage(v, xcon, ur, takeFocus, e)
              END
          ELSE
            (* skip *)
          END                    (* CASE *)
        FINALLY
          VBTRep.UncoverRedisplay()
        END
      END                        (* IF v.ur # NIL *)
    END                          (* LOCK VBT.mu *)
  END HandleEvent;
PROCEDURE ButtonEvent (    v   : VBT.T;
                           xcon: XClient.T;
                           ur  : XClientF.Child;
                       VAR last: Last;
                           e   : X.XButtonEventStar)
  RAISES {TrestleComm.Failure} =
  VAR
    cd: VBT.MouseRec;
    mf               := xcon.mouseFocus;
    state : INTEGER := e.state;
  CONST
    NonButtons = VBT.Modifiers{FIRST(VBT.Modifier).. LAST(VBT.Modifier)}
                   - VBT.Buttons;
  BEGIN
    TRY
    IF e.root = last.root
         AND Word.Minus(e.time, last.time) <= last.doubleClickInterval
         AND ABS(last.x - e.x) <= last.safetyRadius
         AND ABS(last.y - e.y) <= last.safetyRadius
         AND last.button = e.button THEN
      INC(last.clickCount)
    ELSE
      last.clickCount := 0;
      last.root := e.root;
      last.x := e.x;
      last.y := e.y;
      last.button := e.button
    END;
    last.time := e.time;
    cd.modifiers := LOOPHOLE(state, VBT.Modifiers);
    cd.whatChanged := FIRST(VBT.Button);
    INC(cd.whatChanged, e.button - X.Button1);
    IF e.type = X.ButtonPress THEN
      IF cd.modifiers <= NonButtons THEN
        cd.clickType := VBT.ClickType.FirstDown;
        xcon.mouseFocus := v;
        xcon.mouseFocusRoot := ScreenNumber(xcon, e.root)
      ELSE
        cd.clickType := VBT.ClickType.OtherDown
      END
    ELSE
      IF cd.modifiers <= NonButtons + VBT.Modifiers{cd.whatChanged} THEN
        cd.clickType := VBT.ClickType.LastUp;
        xcon.mouseFocus := NIL
      ELSE
        cd.clickType := VBT.ClickType.OtherUp
      END
    END;
    cd.time := e.time;
    cd.cp.pt.h := e.x;
    cd.cp.pt.v := e.y;
    cd.cp.offScreen := e.same_screen = X.False;
    LOCK xcon DO
      cd.cp.gone := cd.cp.offScreen OR e.subwindow # ur.w;
      ur.cageCovered := TRUE;
    END;
    TRY
      cd.cp.screen := ScreenNumber(xcon, e.root);
      cd.clickCount := last.clickCount;
      DeliverPosition(xcon, VBT.PositionRec{cd.cp, cd.time, cd.modifiers},
                      e.x_root, e.y_root, v, xcon.current, mf);
      VBTClass.Mouse(v, cd);
    FINALLY
      LOCK xcon DO ur.cageCovered := FALSE END
    END;
    LOCK v DO xcon.setcage(v) END;
    IF mf # NIL AND mf # v THEN
      cd.cp.offScreen := e.root # xcon.mouseFocusRoot;
      cd.cp.pt.h := e.x_root;
      cd.cp.pt.v := e.y_root;
      cd.cp.gone := TRUE;
      IF NOT cd.cp.offScreen THEN
        VAR mfur: XClientF.Child := mf.upRef;
        BEGIN
          TrestleOnX.Enter(xcon);
          TRY
            XClientF.ValidateNW(xcon, mfur, mf.st);
            DEC(cd.cp.pt.h, mfur.nw.h);
            DEC(cd.cp.pt.v, mfur.nw.v)
          FINALLY
            TrestleOnX.Exit(xcon)
          END
        END
      END;
      VBTClass.Mouse(mf, cd)
    END;
    TrestleOnX.Enter(xcon);
    TRY
      FOR s := FIRST(xcon.sel^) TO LAST(xcon.sel^) DO
        WITH sr = xcon.sel[s] DO
          IF s = VBT.KBFocus.sel THEN
            IF sr.v = v AND ur.isXFocus THEN
              X.XSetInputFocus(xcon.dpy, ur.w, X.RevertToParent, e.time);
              sr.ts := e.time
            END
          ELSIF sr.v = v THEN
            X.XSetSelectionOwner(xcon.dpy, sr.name, ur.w, e.time);
            sr.ts := e.time
          END
        END
      END
    FINALLY
      TrestleOnX.Exit(xcon)
    END
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END ButtonEvent;

PROCEDURE EnterLeave (v              : VBT.T;
                      xcon           : XClient.T;
                      ur             : XClientF.Child;
                      lost, takeFocus: BOOLEAN;
                      e              : X.XCrossingEventStar) =
  VAR cd: VBT.PositionRec;
      state: INTEGER := e.state;
  BEGIN
    cd.time := e.time;
    cd.modifiers := LOOPHOLE(state, VBT.Modifiers);
    cd.cp.pt.h := e.x;
    cd.cp.pt.v := e.y;
    cd.cp.gone := e.type = X.LeaveNotify;
    cd.cp.offScreen := e.same_screen = X.False;
    cd.cp.screen := ScreenNumber(xcon, e.root);
    IF cd.cp.gone AND v = xcon.current THEN
      xcon.current := NIL;
      DeliverPosition(xcon, cd, e.x_root, e.y_root, v, xcon.mouseFocus)
    ELSE
      VAR oc := xcon.current;
      BEGIN
        IF NOT cd.cp.gone AND v # NIL THEN
          xcon.current := v
        ELSE
          oc := NIL
        END;
        DeliverPosition(
          xcon, cd, e.x_root, e.y_root, v, oc, xcon.mouseFocus)
      END
    END;
    IF ur # NIL AND lost THEN
      LOCK xcon DO
        XProperties.ExtendOwns(ur.owns, VBT.KBFocus);
        ur.owns[VBT.KBFocus.sel] := FALSE;
        IF xcon.sel[VBT.KBFocus.sel].v = v THEN
          xcon.sel[VBT.KBFocus.sel].v := NIL
        END
      END;
      VBTClass.Misc(
        v, VBT.MiscRec{VBT.Lost, VBT.NullDetail, 0, VBT.KBFocus})
    ELSIF takeFocus THEN
      LOCK xcon DO ur.recentlyOutside := FALSE END;
      VBTClass.Misc(v, VBT.MiscRec{VBT.TakeSelection, VBT.NullDetail,
                                   e.time, VBT.KBFocus})
    END
  END EnterLeave;

PROCEDURE ClientMessage (v        : VBT.T;
                         xcon     : XClient.T;
                         ur       : XClientF.Child;
                         takeFocus: BOOLEAN;
                         e        : X.XClientMessageEvent_l_star)
  RAISES {TrestleComm.Failure} =
  <* FATAL Split.NotAChild *>
  BEGIN
    WITH data = e.data DO
      IF e.message_type = xcon.protocols THEN
        IF data[0] = xcon.deleteWindow THEN
          Split.Delete(xcon, v)
        ELSIF data[0] = xcon.takeFocus THEN
          LOCK xcon DO
            XProperties.ExtendOwns(ur.owns, VBT.KBFocus);
            takeFocus := NOT ur.owns[VBT.KBFocus.sel]
          END;
          IF takeFocus THEN
            VAR
              tsi: Ctypes.int := data[1];
              ts              := LOOPHOLE(tsi, Ctypes.unsigned_int);
            BEGIN
              VBTClass.Misc(
                v, VBT.MiscRec{
                     VBT.TakeSelection, VBT.NullDetail, ts, VBT.KBFocus})
            END
          END
        END
      ELSIF e.message_type = xcon.decTakeFocus THEN
        LOCK xcon DO
          XProperties.ExtendOwns(ur.owns, VBT.KBFocus);
          takeFocus := NOT ur.owns[VBT.KBFocus.sel]
        END;
        IF takeFocus THEN
          VAR
            tsi: Ctypes.int := data[0];
            ts              := LOOPHOLE(tsi, Ctypes.unsigned_int);
          BEGIN
            VBTClass.Misc(v, VBT.MiscRec{VBT.TakeSelection, VBT.NullDetail,
                                         ts, VBT.KBFocus})
          END
        END
      ELSIF e.message_type = xcon.paNewScreen
              OR e.message_type = xcon.paNewDisplay
              OR e.message_type = xcon.paAddDisplay THEN
        NewScreen(v, xcon, ur, LOOPHOLE(e, X.XClientMessageEvent_s_star))
      ELSIF e.message_type = xcon.miscAtom THEN
        (* data[0] is an externalized MiscCodeType, data[2] is an
           externalized Selection *)
        data[0] := VBT.GetMiscCodeType(XClient.ToName(xcon, data[0])).typ;
        data[2] := VBT.GetSelection(XClient.ToName(xcon, data[2])).sel;
        VBTClass.Misc(
          v, VBT.MiscRec{VBT.MiscCodeType{data[0]},
                         ARRAY [0 .. 1] OF INTEGER{data[3], data[4]},
                         data[1], VBT.Selection{data[2]}})
      ELSIF e.message_type = xcon.wmMoved THEN
        LOCK xcon DO ur.nwValid := FALSE END
      END
    END
  END ClientMessage;

PROCEDURE NewScreen (v   : VBT.T;
                     xcon: XClient.T;
                     ur  : XClientF.Child;
                     e   : X.XClientMessageEvent_s_star)
  RAISES {TrestleComm.Failure} =
  VAR
    id                                    := -1;
    prop, type, type2                     := X.None;
    len, len2, remaining := 0;
    format, format2: Ctypes.int := 0;
    addr, addr2: Ctypes.unsigned_char_star;
    hasprop2                               := FALSE;
  BEGIN
    WITH shData = e.data DO
      VAR
        screen  := shData[0];
        x       := shData[1];
        y       := shData[2];
        width   := shData[3];
        height  := shData[4];
        hasprop := shData[5] # 0;
        state   := shData[6];
      BEGIN
        TRY
          TRY
            TrestleOnX.Enter(xcon);
            TRY
              IF hasprop
                   AND X.Success
                         # X.XGetWindowProperty(
                             xcon.dpy, ur.w, xcon.paNewScreen, 0,
                             X.XMaxRequestSize(xcon.dpy) - 50, X.True,
                             X.AnyPropertyType, ADR(type), ADR(format),
                             ADR(len), ADR(remaining), ADR(addr)) THEN
                hasprop := FALSE
              END;
              IF (e.message_type = xcon.paNewDisplay
                    OR e.message_type = xcon.paAddDisplay)
                   AND X.Success
                         = X.XGetWindowProperty(
                             xcon.dpy, ur.w, e.message_type, 0,
                             MIN(4096, X.XMaxRequestSize(xcon.dpy) - 50),
                             X.True, X.AnyPropertyType, ADR(type2),
                             ADR(format2), ADR(len2), ADR(remaining),
                             ADR(addr2)) THEN
                hasprop2 := TRUE;
                IF format2 = 8 AND len2 > 0 THEN id := -2 END
              ELSE
                FOR i := FIRST(xcon.screens^) TO LAST(xcon.screens^) DO
                  IF xcon.screens[i].screenID = screen THEN id := i END
                END
              END
            FINALLY
              TrestleOnX.Exit(xcon)
            END;
            IF id >= 0 THEN
              XClient.InnerOverlap(
                xcon, v, id, Point.T{x, y}, TRUE,
                iconic := state = X.IconicState, userPosition := TRUE,
                prop := prop, type := type, len := len, format := format,
                addr := addr)
            ELSIF id = -2 THEN
              VAR
                nsp := NEW(XClientF.NewScreenProp);
                m   := NEW(DpyFilter.Message);
                xx  := MiscDetail.FromRef(m);
                yy  := MiscDetail.FromRef(NIL);
                a := LOOPHOLE(
                       addr2, UNTRACED REF ARRAY [0 .. 4095] OF CHAR);
              BEGIN
                IF prop # X.None THEN
                  nsp.prop := prop;
                  nsp.type := type;
                  nsp.len := len;
                  nsp.format := format;
                  nsp.data :=
                    NEW(REF ARRAY OF Ctypes.char, len * format DIV 8);
                  nsp.data^ :=
                    SUBARRAY(LOOPHOLE(addr, UNTRACED REF
                                      ARRAY [0 .. 4095] OF Ctypes.char)^,
                             0, NUMBER(nsp.data^));
                  yy := MiscDetail.FromRef(nsp)
                END;
                m.x := x;
                m.y := y;
                m.width := width;
                m.height := height;
                m.screen := screen;
                m.iconic := state = X.IconicState;
                m.status := TRUE;
                ComputeHeads(a, m, len2);
                IF e.message_type = xcon.paNewDisplay THEN
                  VBTClass.Misc(v, VBT.MiscRec{DpyFilter.ChangeDisplay,
                                               VBT.MiscCodeDetail{xx, yy},
                                               0, VBT.NilSel})
                ELSE
                  VBTClass.Misc(v, VBT.MiscRec{DpyFilter.AddDisplay,
                                               VBT.MiscCodeDetail{xx, yy},
                                               0, VBT.NilSel})
                END;
                MiscDetail.Delete(xx);
                MiscDetail.Delete(yy);
                IF NOT m.status THEN
                  TrestleOnX.Enter(xcon);
                  TRY
                    X.XDeleteProperty(xcon.dpy, ur.w, xcon.paNewDisplay)
                  FINALLY
                    TrestleOnX.Exit(xcon)
                  END
                END
              END
            END
          FINALLY
            IF hasprop THEN X.XFree(LOOPHOLE(addr, Ctypes.char_star)) END;
            IF hasprop2 THEN X.XFree(LOOPHOLE(addr2, Ctypes.char_star)) END
          END
        EXCEPT
          X.Error => RAISE TrestleComm.Failure
        END;
      END
    END
  END NewScreen;

PROCEDURE ComputeHeads (READONLY a   : UNTRACED REF ARRAY [0..4095] OF CHAR;
                                 m   : DpyFilter.Message;
                                 len2: INTEGER             ) =
  CONST NumDisplays = 20;
  VAR
    heads: ARRAY [0 .. NumDisplays] OF INTEGER;
    cnt                                        := 0;
    tail                                       := a[len2 - 1] = '\000';
  BEGIN
    heads[0] := 0;
    FOR i := 0 TO len2 DO
      IF a[i] = '\000' AND cnt < NumDisplays THEN
        INC(cnt);
        heads[cnt] := i + 1
      END
    END;
    IF NOT tail AND cnt < NumDisplays THEN
      INC(cnt);
      heads[cnt] := len2 + 2
    END;
    IF cnt > 1 AND heads[2] > heads[1] + 1 THEN
      m.oldAuth :=
        Text.FromChars(SUBARRAY(a^, heads[1], heads[2] - heads[1] - 1))
    ELSE
      m.oldAuth := NIL
    END;
    IF cnt > 2 AND heads[3] > heads[2] + 1 THEN
      m.newAuth :=
        Text.FromChars(SUBARRAY(a^, heads[2], heads[3] - heads[2] - 1))
    ELSE
      m.newAuth := NIL
    END;
    m.newDisplay := NEW(REF ARRAY OF TEXT, MAX(1, cnt - 2));
    m.newDisplay[0] :=
      Text.FromChars(SUBARRAY(a^, heads[0], heads[1] - heads[0] - 1));
    FOR i := 3 TO cnt - 1 DO
      m.newDisplay[i - 2] :=
        Text.FromChars(SUBARRAY(a^, heads[i], heads[i + 1] - heads[i] - 1))
    END
  END ComputeHeads;

PROCEDURE DeliverPosition (         t        : XClient.T;
                           READONLY cd       : VBT.PositionRec;
                                    h, v     : INTEGER;
                                    w, s1, s2: VBT.T             := NIL) =
  <*FATAL Split.NotAChild*>
  (* Deliver the position in cd to all the children of t, starting with s1,
     including s2, and ending with w. *)
  VAR
    goneCd          := cd;
    others: BOOLEAN;
    ch    : VBT.T;
  BEGIN
    goneCd.cp.gone := TRUE;
    LOCK t DO others := t.otherCages; t.otherCages := FALSE END;
    IF s1 # NIL AND s1 # w THEN DoPosition(t, s1, goneCd, h, v) END;
    IF others THEN
      ch := Split.Succ(t, NIL);
      WHILE ch # NIL DO
        IF ch # s1 AND ch # w THEN DoPosition(t, ch, goneCd, h, v) END;
        ch := Split.Succ(t, ch)
      END
    ELSIF s2 # NIL AND s2 # w AND s2 # s1 THEN
      DoPosition(t, s2, goneCd, h, v)
    END;
    IF w # NIL THEN VBTClass.Position(w, cd) END
  END DeliverPosition;

PROCEDURE ScreenNumber (t: XClient.T; w: X.Window): INTEGER =
  BEGIN
    IF t.screens = NIL THEN RETURN -1 END;
    IF w = t.currentRootWindow THEN RETURN t.currentRoot END;
    LOCK t DO
      t.otherCages := TRUE;
      FOR i := FIRST(t.screens^) TO LAST(t.screens^) DO
        IF t.screens[i].root = w THEN
          t.currentRootWindow := w;
          t.currentRoot := t.screens[i].screenID;
          RETURN t.currentRoot
        END
      END;
      t.currentRootWindow := X.None;
      t.currentRoot := -1;
      RETURN -1
    END
  END ScreenNumber;

PROCEDURE DeliverBadRegion (v: VBT.T; ur: XClientF.Child) =
  (* Join v's x-bad-region into v's child's ordinary bad region, call its
     repaint method, and clear its x-bad-region.  LL = VBT.mu. *)
  BEGIN
    LOCK v DO
      LOCK v.parent DO
        VBTClass.ForceRepaint(v, ur.badR, FALSE);
        ur.badR := Region.Empty
      END
    END;
    VBTClass.Repaint(v, Region.Empty)
  END DeliverBadRegion;

PROCEDURE DoPosition (<*UNUSED*>     t   : XClient.T;
                                     w   : VBT.T;
                                 VAR cd  : VBT.PositionRec;
                      <*UNUSED*>     h, v: INTEGER          ) =
  VAR cg := VBTClass.Cage(w);
  BEGIN
    IF (cg.screen = cd.cp.screen OR cg.screen = VBT.AllScreens)
         AND TRUE IN cg.inOut THEN
      IF Rect.Equal(cg.rect, Rect.Full) THEN RETURN END;
    END
  END DoPosition;

BEGIN
END XMessenger.

