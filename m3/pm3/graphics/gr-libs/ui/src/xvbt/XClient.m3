(* Copyright C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Mar  6 17:44:15 PST 1995 by msm      *)
(*      modified on Tue Jan 31 16:38:46 PST 1995 by kalsow   *)
(*      modified on Fri Mar 11 17:00:20 PST 1994 by gnelson  *)
(*      modified on Mon Nov 22 13:35:56 PST 1993 by steveg   *)
(*      modified on Fri Oct 29 16:21:33 PDT 1993 by sfreeman *)

(* modified on Mon Feb 24 13:59:43 PST 1992 by muller *)


<*PRAGMA LL*>

(* INTERFACE XEventQueue = RingBuffer(XEvent) END XEventQueue. *)

(* INTERFACE XEvent; IMPORT X; TYPE T = X.XEvent END XEvent. *)

(* INTERFACE XScrollQueue = RingBuffer(XScrollQElt) END XScrollQueue. *)

(* INTERFACE XScrollQElt; IMPORT PaintPrivate; TYPE T =
   PaintPrivate.ScrollRec END XScrollQElt. *)

UNSAFE MODULE XClient;

IMPORT X, VBT, TrestleClass, TrestleComm, Ctypes, M3toC, XEventQueue,
       XScreenType, Point, Rect, ProperSplit, Trestle, Thread, VBTClass,
       Axis, Text, Split, ScrnCursor, ScrnColorMap, VBTRep, XClientF,
       XPaint, TrestleImpl, ASCII, XProperties, TrestleOnX, XScrnCmap,
       Cstring, Unix, Xatom, Fmt, XScrnPxmp;

FROM TrestleClass IMPORT Decoration;
FROM XClientF IMPORT Child, SimpleWaitFor;
FROM TrestleOnX IMPORT Enter, Exit;

REVEAL
  T = XPaint.T BRANDED OBJECT
      OVERRIDES
        beChild          := BeChild;
        replace          := Replace;
        setcage          := SetCage;
        sync             := Sync;
        setcursor        := SetCursor;
        newShape         := NewShape;
        readUp           := ReadUp;
        writeUp          := WriteUp;
        redisplay        := Redisplay;
        acquire          := Acquire;
        release          := Release;
        put              := Put;
        forge            := Forge;
        attach           := Attach;
        decorate         := Decorate;
        iconize          := Iconize;
        overlap          := Overlap;
        moveNear         := MoveNear;
        installOffscreen := InstallOffscreen;
        setColorMap      := SetColorMap;
        getScreens       := GetScreens;
        allCeded         := AllCeded;
        tickTime         := TickTime;
        screenOf         := ScreenOf;
        trestleId        := TrestleID;
        windowId         := WindowID;
        updateChalk      := UpdateChalk;
        updateBuddies    := UpdateBuddies;
      END;

PROCEDURE TrestleID (t: T; v: VBT.T): TEXT =
  BEGIN
    IF v = NIL THEN
      RETURN t.inst
    ELSE
      TYPECASE v.st OF
        NULL =>
      | XScreenType.T (xst) =>
          RETURN t.fullinst & "." & Fmt.Int(xst.screenID)
      ELSE
      END;
      RETURN t.fullinst & "." & Fmt.Int(X.XDefaultScreen(t.dpy)) 
    END
  END TrestleID;

PROCEDURE WindowID (<* UNUSED *> t: T; v: VBT.T): TEXT =
  BEGIN
    RETURN Fmt.Unsigned(TrestleOnX.Drawable(v), base := 10)
  END WindowID;

PROCEDURE UpdateChalk (t: T; v: VBT.T; chalk: TEXT) =
  BEGIN
    TYPECASE v.upRef OF
      NULL =>
    | Child (ur) =>
        VAR ip: REF ARRAY OF CHAR;
        BEGIN
          ip := NEW(REF ARRAY OF CHAR, Text.Length(chalk) + 1);
          Text.SetChars(ip^, chalk);
          ip[LAST(ip^)] := ASCII.NUL;
          TRY
            Enter(t);
            TRY
              IF ur.w # X.None AND ur.xcage # X.None THEN
                XProperties.PutProp(t, ur.w, ToAtom(t, "XMUX_CHALKHOLDER"),
                                    Xatom.XA_STRING, ip^, 8);
              END
            FINALLY
              Exit(t)
            END
          EXCEPT
            TrestleComm.Failure =>
          END
        END
    ELSE
    END
  END UpdateChalk;

PROCEDURE UpdateBuddies (t: T; v: VBT.T; READONLY trsls, ids: ARRAY OF TEXT) =
  BEGIN
    TYPECASE v.upRef OF
      NULL =>
    | Child (ur) =>
        VAR
          trslProp, idProp                    := "";
          tp, ip          : REF ARRAY OF CHAR;
        BEGIN
          FOR i := 0 TO LAST(trsls) - 1 DO
            trslProp := trslProp & trsls[i] & ","
          END;
          IF NUMBER(trsls) > 0 THEN
            trslProp := trslProp & trsls[LAST(trsls)]
          END;
          tp := NEW(REF ARRAY OF CHAR, Text.Length(trslProp) + 1);
          Text.SetChars(tp^, trslProp);
          tp[LAST(tp^)] := ASCII.NUL;
          FOR i := 0 TO LAST(ids) DO idProp := idProp & ids[i] & " " END;
          ip := NEW(REF ARRAY OF CHAR, Text.Length(idProp) + 1);
          Text.SetChars(ip^, idProp);
          ip[LAST(ip^)] := ASCII.NUL;
          TRY
            Enter(t);
            TRY
              IF ur.w # X.None AND ur.xcage # X.None THEN
                XProperties.PutProp(t, ur.w, ToAtom(t, "XMUX_HOSTS"),
                                    Xatom.XA_STRING, tp^, 8);
                XProperties.PutProp(
                  t, ur.w, ToAtom(t, "XMUX_IDS"), Xatom.XA_STRING, ip^, 8);
              END
            FINALLY
              Exit(t)
            END
          EXCEPT
            TrestleComm.Failure =>
          END
        END
    ELSE
    END
  END UpdateBuddies;

<* UNUSED *> PROCEDURE Outside (VAR cp: VBT.CursorPosition; v: VBT.T):
  BOOLEAN =
  VAR st: XScreenType.T := v.st;
  BEGIN
    cp.offScreen := cp.screen = st.screenID;
    RETURN VBT.Outside(cp, VBTClass.Cage(v))
  END Outside;

PROCEDURE BeChild (trsl: T; ch: VBT.T) RAISES {} =
  BEGIN
    IF ch.upRef = NIL THEN
      ch.upRef := NEW(Child, ch := ch, owns := NEW(XClientF.OwnsArray, 0))
    ELSE
      WITH ur = NARROW(ch.upRef, Child) DO
        ur.ch := ch;
        ur.owns := NEW(XClientF.OwnsArray, 0)
      END
    END;
    ch.parent := trsl;
  END BeChild;

PROCEDURE Replace (trsl: T; ch, new: VBT.T) RAISES {} =
  VAR ur: Child := ch.upRef;
  BEGIN
    IF new # NIL THEN Crash() END;
    TRY
      Enter(trsl);
      TRY
        IF ur.xcage = X.None THEN
          IF ur.captureOnWrite # NIL THEN
            VAR
              st : XScreenType.T := ch.st;
              pm : X.Pixmap      := ur.w;
              dpy                := trsl.dpy;
              id                 := ur.captureOnWrite.id;
            BEGIN
              IF XScrnPxmp.IsLazy(st, id) THEN
                IF Rect.Equal(XScrnPxmp.PixmapDomain(st, id), ch.domain) THEN
                  XScrnPxmp.FinishCapture(st, id, pm);
                  ur.w := X.None
                ELSE
                  XPaint.ForceCapturePM(st, dpy, id);
                  ur.captureOnWrite := NIL
                END
              END
            END
          END;
          IF ur.w # X.None THEN X.XFreePixmap(trsl.dpy, ur.w) END
        ELSE
          X.XDestroyWindow(trsl.dpy, ur.w)
        END
      FINALLY
        Exit(trsl)
      END
    EXCEPT
      X.Error, TrestleComm.Failure => (* skip *)
    END;
    XClientF.Delete(trsl, ch, ur)
  END Replace;

PROCEDURE Attach (trsl: T; v: VBT.T) RAISES {} =
  BEGIN
    LOCK v DO LOCK trsl DO ProperSplit.Insert(trsl, NIL, v) END END
  END Attach;

CONST
  DefaultWidth  = 133.0;
  DefaultHeight = 100.0;
(* mm *)

PROCEDURE MoveNear (trsl: T; v, w: VBT.T) RAISES {TrestleComm.Failure} =
  VAR
    st : XScreenType.T;
    nw                 := Point.T{50, 50};
    ch : Child;
    wtr: Trestle.T;
    id                 := Trestle.NoScreen;
  BEGIN
    LOOP
      IF w = NIL THEN EXIT END;
      IF NOT TrestleImpl.RootChild(w, wtr, w) THEN w := NIL; EXIT END;
      IF wtr = trsl THEN EXIT END;
      w := w.parent;
    END;
    IF w = v THEN w := NIL END;
    IF w # NIL THEN
      ch := w.upRef;
      IF w.st = NIL OR ch.xcage = X.None THEN w := NIL END
    END;
    IF w # NIL THEN
      st := w.st;
      Enter(trsl);
      TRY
        XClientF.ValidateNW(trsl, ch, st);
        nw := Point.Add(nw, ch.nw)
      FINALLY
        Exit(trsl)
      END;
      id := st.screenID
    END;
    InnerOverlap(trsl, v, id, nw, w # NIL)
  END MoveNear;

PROCEDURE InstallOffscreen (trsl         : T;
                            v            : VBT.T;
                            width, height: CARDINAL;
                            prefst       : VBT.ScreenType)
  RAISES {TrestleComm.Failure} =
  VAR st := MatchScreenType(trsl, prefst);
  BEGIN
    CreateXPixmap(trsl, v, st, width, height)
  END InstallOffscreen;

PROCEDURE MatchScreenType (trsl: T; prefst: VBT.ScreenType):
  XScreenType.T =
  BEGIN
    TYPECASE prefst OF
      XScreenType.T (xst) =>
        IF xst # NIL AND xst.trsl = trsl THEN RETURN xst END
    ELSE
    END;
    IF prefst.depth = 1 THEN
      RETURN trsl.screens[0].bits
    ELSE
      RETURN trsl.screens[0]
    END
  END MatchScreenType;

PROCEDURE ScreenOf (trsl: T; ch: VBT.T; READONLY pt: Point.T):
  Trestle.ScreenOfRec RAISES {} =
  VAR
    ur : Child               := ch.upRef;
    st : XScreenType.T       := ch.st;
    res: Trestle.ScreenOfRec;
  BEGIN
    res.trsl := trsl;
    IF st = NIL OR ur = NIL OR ur.w # X.None AND ur.xcage = X.None THEN
      res.id := Trestle.NoScreen
    ELSE
      TRY
        Enter(trsl);
        TRY
          res.id := st.screenID;
          res.dom := st.rootDom;
          IF ur.w # X.None THEN
            XClientF.ValidateNW(trsl, ur, st);
            res.q := Point.Add(pt, ur.nw)
          ELSE
            res.q := pt
          END
        FINALLY
          Exit(trsl)
        END
      EXCEPT
        TrestleComm.Failure => res.id := Trestle.NoScreen
      END
    END;
    RETURN res
  END ScreenOf;

PROCEDURE Overlap (         trsl: T;
                            v   : VBT.T;
                            id  : Trestle.ScreenID;
                   READONLY nw  : Point.T           )
  RAISES {TrestleComm.Failure} =
  BEGIN
    InnerOverlap(trsl, v, id, nw, TRUE)
  END Overlap;

PROCEDURE InnerOverlap (         trsl         : T;
                                 v            : VBT.T;
                                 id           : Trestle.ScreenID;
                        READONLY nw           : Point.T;
                                 knownPosition: BOOLEAN;
                        iconic, userPosition := FALSE;
                        prop, type           := X.None;
                        len, format          := 0;
                        addr: Ctypes.unsigned_char_star := NIL)
  RAISES {TrestleComm.Failure} =
  VAR
    st           : XScreenType.T;
    alreadyMapped: BOOLEAN;
  BEGIN
    TRY
      LOCK trsl DO
        IF id < FIRST(trsl.screens^) OR id > LAST(trsl.screens^) THEN
          id := trsl.defaultScreen
        END;
        st := trsl.screens[id];
        IF knownPosition OR v.st = NIL OR v.st = st THEN
          alreadyMapped := v.st = st
        ELSE
          alreadyMapped := FALSE;
          FOR i := FIRST(trsl.screens^) TO LAST(trsl.screens^) DO
            IF trsl.screens[i] = v.st THEN
              alreadyMapped := TRUE;
              st := v.st
            END
          END
        END
      END (* LOCK *);
      IF alreadyMapped THEN
        VAR
          ur  : Child            := v.upRef;
          xwc : X.XWindowChanges;
          mask                   := X.CWStackMode;
        BEGIN
          xwc.x := nw.h;
          xwc.y := nw.v;
          xwc.stack_mode := X.Above;
          IF knownPosition THEN INC(mask, X.CWX + X.CWY) END;
          Enter(trsl);
          TRY
            X.XConfigureWindow(trsl.dpy, ur.w, mask, ADR(xwc));
            IF iconic AND NOT ur.decorated THEN
              X.XUnmapWindow(trsl.dpy, ur.w)
            ELSIF iconic THEN
              EVAL X.XIconifyWindow(trsl.dpy, ur.w, st.screenID)
            ELSE
              X.XMapWindow(trsl.dpy, ur.w)
            END
          FINALLY
            Exit(trsl)
          END
        END
      ELSE
        CreateXWindow(
          trsl, v, st, nw.h, nw.v, userPosition := userPosition,
          iconic := iconic, prop := prop, type := type, len := len,
          addr := addr, format := format)
      END
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END
  END InnerOverlap;

PROCEDURE Iconize (trsl: T; v: VBT.T) RAISES {TrestleComm.Failure} =
  VAR alreadyMapped: BOOLEAN;
  BEGIN
    TRY
    alreadyMapped := v.st # NIL;
    IF alreadyMapped THEN
      VAR
        xst: XScreenType.T := v.st;
        ur : Child         := v.upRef;
      BEGIN
        Enter(trsl);
        TRY
          IF NOT ur.decorated THEN
            X.XUnmapWindow(trsl.dpy, ur.w)
          ELSE
            EVAL X.XIconifyWindow(trsl.dpy, ur.w, xst.screenID)
          END
        FINALLY
          Exit(trsl)
        END
      END
    ELSE
      CreateXWindow(trsl, v, NIL, iconic := TRUE)
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END Iconize;

CONST
  StandardInputMask = X.KeyPressMask + X.KeyReleaseMask + X.ButtonPressMask
                        + X.ButtonReleaseMask + X.EnterWindowMask
                        + X.LeaveWindowMask + X.ExposureMask
                        + X.StructureNotifyMask + X.FocusChangeMask
                        + X.OwnerGrabButtonMask + X.VisibilityChangeMask;

<* UNUSED *> CONST
  MotionHintMask = StandardInputMask + X.PointerMotionMask
                     + X.PointerMotionHintMask;

VAR
  MyHostName: X.XTextProperty;
  brokenOpenWin := FALSE;

CONST
  TrestleCopArray = ARRAY [0 .. 9] OF
                      CHAR{
                      'T', 'r', 'e', 's', 't', 'l', 'e', 'C', 'o', 'p'};
  TrestleArray = ARRAY [0 .. 11] OF
                   CHAR{'T', 'r', 'e', 's', 't', 'l', 'e', 'C', 'h', 'i',
                        'l', 'd'};

PROCEDURE CreateXWindow (trsl                : T;
                         v                   : VBT.T;
                         st                  : XScreenType.T;
                         x, y                                  := 50;
                         iconic, userPosition                  := FALSE;
                         prop, type                            := X.None;
                         len, format                           := 0;
                         addr: Ctypes.unsigned_char_star := NIL)
  RAISES {TrestleComm.Failure} =
  VAR
    cs           : ScrnCursor.T;
    width, height: CARDINAL;
    ur           : Child                    := v.upRef;
    xwa          : X.XSetWindowAttributes;
    xhints       : X.XWMHints;
    xSizeHints   : X.XSizeHints;
    atm          : ARRAY [0 .. 4] OF X.Atom;
    cmid         : X.Colormap;
    dec   : Decoration           := VBT.GetProp(v, TYPECODE(Decoration));
    serial: Ctypes.unsigned_long;
    bstore                       := X.NotUseful;
    owns  : XClientF.OwnsArray   := NIL;
    nsp: XClientF.NewScreenProp := VBT.GetProp(
                                     v, TYPECODE(XClientF.NewScreenProp));
    app         := TrestleImpl.ChildApp(v);
    firstAttach := FALSE;
  BEGIN
    IF app # NIL THEN
      firstAttach := (app.primary = trsl.conf) AND trsl.confEnabled
    END;
    TRY
      IF nsp # NIL THEN
        VBT.RemProp(v, TYPECODE(XClientF.NewScreenProp));
        prop := nsp.prop;
        type := nsp.type;
        len := nsp.len;
        format := nsp.format;
        addr := LOOPHOLE(ADR(nsp.data^[0]), Ctypes.unsigned_char_star);
      END;
      Enter(trsl);
      TRY
        IF v.st # NIL THEN
          owns := ur.owns;
          FOR s := FIRST(owns^) TO LAST(owns^) DO
            IF trsl.sel[s].v = v THEN trsl.sel[s].v := NIL END
          END;
          ur.owns := NEW(XClientF.OwnsArray, 0);
          X.XDestroyWindow(trsl.dpy, ur.w);
          ur.w := X.None;
        END;
        IF st = NIL THEN st := trsl.screens[trsl.defaultScreen] END;
        atm[0] := trsl.takeFocus;
        atm[1] := trsl.deleteWindow;
        atm[2] := trsl.paNewScreen;
        atm[3] := trsl.paNewDisplay;
        atm[4] := trsl.paAddDisplay;
        bstore := st.backing_store
      FINALLY
        Exit(trsl)
      END;
      IF st.cmap = NIL THEN
        cmid := X.CopyFromParent
      ELSE
        cmid := XScrnCmap.ColorMapID(st.cmap.standard());
        IF cmid = X.None THEN cmid := X.CopyFromParent END
      END;
      VBTClass.Rescreen(v, st);
      width := ROUND(VBT.MMToPixels(v, DefaultWidth, Axis.T.Hor));
      height := ROUND(VBT.MMToPixels(v, DefaultHeight, Axis.T.Ver));
      VAR
        s  := VBTClass.GetShapes(v);
        sh := s[Axis.T.Hor];
        sv := s[Axis.T.Ver];
      BEGIN
        ur.sh := sh;
        ur.sv := sv;
        SetSizeHints(xSizeHints, width, height, sh, sv, st);
      END;
      xwa.border_pixel := 0;
      xwa.bit_gravity := X.NorthWestGravity;
      xwa.event_mask := StandardInputMask;
      xwa.colormap := cmid;
      IF dec = NIL THEN
        xwa.override_redirect := X.True;
        xwa.save_under := X.True;
        xwa.backing_store := X.NotUseful
      ELSE
        xwa.override_redirect := X.False;
        xwa.save_under := X.False;
        xwa.backing_store := bstore
      END;
      xhints.input := X.False;
      IF iconic THEN
        xhints.initial_state := X.IconicState
      ELSE
        xhints.initial_state := X.NormalState
      END;
      xhints.flags := X.InputHint + X.StateHint;
      LOCK v DO
        cs := v.getcursor();
        Enter(trsl);
        TRY
          IF cs = ScrnCursor.DontCare THEN
            ur.csid := X.None
          ELSE
            ur.csid := cs.id
          END;
          ur.userPosition := userPosition;
          ur.w :=
            X.XCreateWindow(
              trsl.dpy, st.root, x := x, y := y, width := width,
              height := height, border := 0, depth := st.depth,
              class := X.InputOutput, visual := st.visual,
              valuemask := X.CWBorderPixel + X.CWBitGravity + X.CWEventMask
                             + X.CWColormap + X.CWOverrideRedirect
                             + X.CWSaveUnder + X.CWBackingStore,
              attributes := ADR(xwa));
          X.XDefineCursor(trsl.dpy, ur.w, ur.csid);
          IF firstAttach THEN
            XProperties.PutProp(trsl, ur.w, ToAtom(trsl, "XMUX_AGENT"),
                                Xatom.XA_STRING, TrestleCopArray, 8)
          ELSE
            XProperties.PutProp(trsl, ur.w, ToAtom(trsl, "XMUX_AGENT"),
                                Xatom.XA_STRING, TrestleArray, 8)
          END;
          X.XSetWMClientMachine(trsl.dpy, ur.w, ADR(MyHostName));
          IF brokenOpenWin THEN
            VAR min, max: Ctypes.int;
            BEGIN
              X.XDisplayKeycodes(
                trsl.dpy, LOOPHOLE(ADR(min), Ctypes.int_star),
                LOOPHOLE(ADR(max), Ctypes.int_star));
              FOR i := min TO max DO
                X.XGrabKey(trsl.dpy, i, X.AnyModifier, ur.w, X.True,
                           X.GrabModeAsync, X.GrabModeAsync)
              END
            END
          ELSE
            X.XGrabKey(trsl.dpy, X.AnyKey, X.AnyModifier, ur.w, X.True,
                       X.GrabModeAsync, X.GrabModeAsync);
          END;
          FOR i := FIRST(trsl.ungrab) TO LAST(trsl.ungrab) DO
            WITH cd = trsl.ungrab[i] DO
              IF cd # 0 THEN
                X.XUngrabKey(trsl.dpy, cd, X.AnyModifier, ur.w)
              END
            END
          END;
          IF prop # X.None THEN
            X.XChangeProperty(trsl.dpy, ur.w, prop, type, format,
                              X.PropModeReplace, addr, len);
            IF nsp # NIL THEN DISPOSE(nsp.data) END
          END;
          xwa.event_mask := X.PropertyChangeMask;
          ur.xcage :=
            X.XCreateWindow(
              trsl.dpy, ur.w, x := -1, y := -1, width := 1, height := 1,
              border := 0, depth := 0, class := X.InputOnly,
              visual := LOOPHOLE(X.CopyFromParent, X.VisualStar),
              valuemask := X.CWEventMask, attributes := ADR(xwa));
          ur.cageRect := Rect.FromEdges(-1, 0, -1, 0);
          ur.width := width;
          ur.height := height;
          EVAL trsl.vbts.put(ur.w, v);
          X.XMapSubwindows(trsl.dpy, ur.w);
          ur.decorated := dec # NIL;
          IF dec # NIL THEN
            SetDecoration(trsl, ur.w, NIL, dec);
            X.XSetWMHints(trsl.dpy, ur.w, ADR(xhints));
            X.XSetWMNormalHints(trsl.dpy, ur.w, ADR(xSizeHints));
            EVAL
              X.XSetWMProtocols(trsl.dpy, ur.w, ADR(atm[0]), NUMBER(atm));
          END;
          serial := X.XNextRequest(trsl.dpy);
          IF dec # NIL OR NOT iconic THEN X.XMapWindow(trsl.dpy, ur.w) END;
          IF dec = NIL AND NOT iconic THEN
            VAR
              ev: X.XEvent;
              e            := LOOPHOLE(ADR(ev), X.XMapEventStar);
            BEGIN
              e.type := X.MapNotify;
              e.serial := serial;
              e.send_event := X.False;
              e.display := trsl.dpy;
              e.event := ur.w;
              e.window := ur.w;
              e.override_redirect := X.True;
              XClientF.BackDoor(trsl, ev)
            END
          END
        FINALLY
          Exit(trsl)
        END
      END;
      IF owns # NIL THEN
        FOR i := FIRST(owns^) TO LAST(owns^) DO
          IF owns[i] THEN
            VBTClass.Misc(v, VBT.MiscRec{VBT.Lost, VBT.NullDetail, 0,
                                         VBT.Selection{i}})
          END
        END
      END;
      TrestleImpl.UpdateBuddies(v);
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END CreateXWindow;

PROCEDURE CreateXPixmap (trsl         : T;
                         v            : VBT.T;
                         st           : XScreenType.T;
                         width, height: CARDINAL       )
  RAISES {TrestleComm.Failure} =
  VAR ur: Child := v.upRef;
  BEGIN
    TRY
    VBTClass.Rescreen(v, st);
    LOCK v DO
      Enter(trsl);
      TRY
        ur.w := X.XCreatePixmap(trsl.dpy, st.root, width := width,
                                height := height, depth := st.depth);
        ur.xcage := X.None;
        ur.cageRect := Rect.Full;
        ur.width := width;
        ur.height := height;
        EVAL trsl.vbts.put(ur.w, v);
        ur.reshapeComing := FALSE;
        ur.mapped := TRUE
      FINALLY
        Exit(trsl)
      END
    END;
    VBTClass.Reshape(v, Rect.FromSize(width, height), Rect.Empty);
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END CreateXPixmap;

PROCEDURE SetColorMap (trsl: T; v: VBT.T; cm: ScrnColorMap.T) RAISES {} =
  VAR
    xid        := XScrnCmap.ColorMapID(cm);
    ur : Child := v.upRef;
  BEGIN
    TRY
      Enter(trsl);
      TRY X.XSetWindowColormap(trsl.dpy, ur.w, xid) FINALLY Exit(trsl) END
    EXCEPT
      X.Error, TrestleComm.Failure =>     (* skip *)
    END
  END SetColorMap;

PROCEDURE TickTime (<*UNUSED*> trsl: T): INTEGER =
  BEGIN
    RETURN 1000
  END TickTime;

PROCEDURE AllCeded (trsl: T): BOOLEAN RAISES {} =
  BEGIN
    TRY
      Enter(trsl);
      TRY
        X.XSync(trsl.dpy, X.False);
        RETURN XEventQueue.IsEmpty(trsl.evq)
                 AND (X.XEventsQueued(trsl.dpy, X.QueuedAfterReading) = 0)
      FINALLY
        Exit(trsl)
      END
    EXCEPT
      X.Error, TrestleComm.Failure => RETURN FALSE
    END
  END AllCeded;

PROCEDURE GetScreens (trsl: T): Trestle.ScreenArray RAISES {} =
  VAR res: Trestle.ScreenArray;
  BEGIN
    LOCK trsl DO
      res := NEW(Trestle.ScreenArray, NUMBER(trsl.screens^));
      FOR i := 0 TO LAST(res^) DO
        res[i].id := i;
        res[i].dom := trsl.screens[i].rootDom;
        res[i].delta := Point.Origin;
        res[i].type := trsl.screens[i]
      END
    END;
    RETURN res
  END GetScreens;

PROCEDURE SetDecoration (trsl: T; w: X.Window; old, new: Decoration)
  RAISES {TrestleComm.Failure} =
  (* The decorations for w have changed from old to new; this procedure
     relays this change to the X window manager.  LL = trsl. *)
  VAR xClassHint: X.XClassHint;
  BEGIN
    TRY
      IF new = NIL OR w = X.None THEN RETURN END;
      IF (old = NIL) OR NOT Text.Equal(old.windowTitle, new.windowTitle) THEN
        WITH s = M3toC.TtoS(new.windowTitle) DO
          X.XStoreName(trsl.dpy, w, s)
        END
      END;
      IF (old = NIL) OR NOT Text.Equal(old.iconTitle, new.iconTitle) THEN
        WITH s = M3toC.TtoS(new.iconTitle) DO
          X.XSetIconName(trsl.dpy, w, s)
        END
      END;
      IF (old = NIL) OR NOT Text.Equal(old.inst, new.inst)
           OR NOT Text.Equal(old.applName, new.applName) THEN
        xClassHint.res_name := M3toC.TtoS(new.inst);
        xClassHint.res_class := M3toC.TtoS(new.applName);
        X.XSetClassHint(trsl.dpy, w, ADR(xClassHint));
        X.XSetCommand(trsl.dpy, w,
                      LOOPHOLE(ADR(xClassHint.res_class), X.Argv), 1)
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END SetDecoration;

PROCEDURE Decorate (trsl: T; v: VBT.T; old, new: Decoration)
  RAISES {TrestleComm.Failure} =
  BEGIN
    TYPECASE v.upRef OF
      NULL =>                    (*skip*)
    | Child (ch) =>
        Enter(trsl);
        TRY SetDecoration(trsl, ch.w, old, new) FINALLY Exit(trsl) END
    ELSE                         (* skip*)
    END
  END Decorate;

PROCEDURE MoveResize (         dpy : X.DisplayStar;
                               w   : X.Window;
                      READONLY r, s: Rect.T         )
  RAISES {TrestleComm.Failure} =
  BEGIN
    TRY
    IF NOT Rect.Equal(r, s) THEN
      X.XMoveResizeWindow(
        dpy, w, r.west, r.north, r.east - r.west, r.south - r.north)
    END;
   EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END MoveResize;

CONST
  LargeRect = Rect.T{0, 10000, 0, 10000};
  SmallRect = Rect.T{-1, 0, -1, 0};

PROCEDURE SetCage (v: T; ch: VBT.T) RAISES {} =
  VAR
    ur   : Child         := ch.upRef;
    dpy  : X.DisplayStar;
    xcage: X.Window;
    newCg: Rect.T;
  BEGIN
    WITH cage = VBTClass.Cage(ch) DO
      IF ch.st = NIL OR ur = NIL OR ch.parent # v THEN
        IF NOT (TRUE IN cage.inOut) THEN VBTClass.ForceEscape(ch) END;
        RETURN
      END;
      TRY
        Enter(v);
        TRY
          IF ur.cageCovered THEN RETURN END;
          dpy := v.dpy;
          xcage := ur.xcage;
          IF ur.inside THEN
            IF NOT (FALSE IN cage.inOut) THEN
              newCg := SmallRect
            ELSE
              newCg := Rect.Meet(LargeRect, cage.rect);
              IF Rect.IsEmpty(newCg) THEN newCg := SmallRect END
            END;
            MoveResize(dpy, xcage, LargeRect, ur.cageRect);
            MoveResize(dpy, xcage, newCg, LargeRect);
            ur.cageRect := newCg
          ELSE
            MoveResize(dpy, xcage, SmallRect, ur.cageRect);
            ur.cageRect := SmallRect;
            IF NOT (TRUE IN cage.inOut) THEN VBTClass.ForceEscape(ch) END
          END
        FINALLY
          Exit(v)
        END
      EXCEPT
        TrestleComm.Failure =>   (* skip *)
      END
    END
  END SetCage;

PROCEDURE Sync (v: T; ch: VBT.T; wait := TRUE) RAISES {} =
  VAR
    ur  : Child   := ch.upRef;
    data: INTEGER;
    wf            := NEW(XProperties.PropertyWaitFor);
    win           := ur.xcage;
  BEGIN
    IF ch.st = NIL THEN RETURN END;
    TRY
      Enter(v);
      TRY
        Thread.Release(ch);
        IF NOT wait THEN
          X.XFlush(v.dpy);
        ELSE
          IF win = X.None THEN
            X.XSync(v.dpy, X.False)
          ELSE
            wf.types[0] := 0;
            wf.types[1] := X.PropertyNotify;
            wf.d := win;
            wf.reqno := X.XNextRequest(v.dpy);
            wf.a := v.miscAtom;
            wf.ts := 0;
            X.XChangeProperty(v.dpy, win, v.miscAtom, v.miscAtom, 32,
                              X.PropModeAppend,
                              LOOPHOLE(ADR(data),
                                       Ctypes.const_unsigned_char_star),
                              0);
            IF XClientF.Await(v, wf, 10) # X.PropertyNotify AND NOT v.dead THEN
              X.XSync(v.dpy, X.False)
            END
          END
        END
      FINALLY
        Exit(v);
        Thread.Acquire(ch)
      END
    EXCEPT
      X.Error, TrestleComm.Failure =>     (* skip *)
    END
  END Sync;

PROCEDURE SetCursor (v: T; ch: VBT.T) RAISES {} =
  VAR
    ur  : Child    := ch.upRef;
    csid: X.Cursor;
  BEGIN
    IF ch.st = NIL OR ur.w = X.None THEN RETURN END;
    WITH cs = ch.getcursor() DO
      TRY
        Enter(v);
        TRY
          IF cs = ScrnCursor.DontCare THEN
            csid := X.None
          ELSE
            csid := cs.id
          END;
          IF ur.csid # csid THEN
            ur.csid := csid;
            X.XDefineCursor(v.dpy, ur.w, csid)
          END
        FINALLY
          Exit(v)
        END
      EXCEPT
        X.Error, TrestleComm.Failure =>   (* skip *)
      END
    END
  END SetCursor;

PROCEDURE TypeCodeToXType (v: T; tc: INTEGER): X.Atom
  RAISES {TrestleComm.Failure} =
  BEGIN
    IF tc = -1 OR tc = TYPECODE(TEXT) THEN
      RETURN ToAtom(v, "STRING")
    ELSE
      RETURN X.None
    END
  END TypeCodeToXType;

PROCEDURE SelectionAtom (v: T; s: VBT.Selection): X.Atom
  RAISES {TrestleComm.Failure} =
  <* LL.sup = v *>
  VAR name := v.sel[s.sel].name;
  BEGIN
    IF name = X.None THEN
      WITH txt = VBT.SelectionName(s) DO
        IF txt # NIL THEN
          name := ToAtom(v, txt);
          v.sel[s.sel].name := name
        END
      END
    END;
    RETURN name
  END SelectionAtom;

PROCEDURE WriteUp (           v  : T;
                              ch : VBT.T;
                   <*UNUSED*> w  : VBT.T;
                              s  : VBT.Selection;
                              ts : VBT.TimeStamp;
                              val: VBT.Value;
                              tc : CARDINAL       ) RAISES {VBT.Error} =
  VAR
    sc                : VBT.T                        := NIL;
    multi             : ARRAY [0 .. 3] OF Ctypes.int;
    buf               : UNTRACED REF ARRAY OF CHAR;
    error                                            := FALSE;
    ec                : VBT.ErrorCode;
    vl                : REFANY;
    name, type, mulsym: X.Atom;
    ur                : Child;
  BEGIN
    TRY
      LOCK v DO
        XProperties.ExtendSel(v.sel, s);
        IF v.sel[s.sel].v # NIL THEN sc := v.sel[s.sel].v END
      END;
      IF sc # NIL THEN
        sc.write(s, val, tc)
      ELSE
        vl := val.toRef();
        Enter(v);
        TRY
          name := SelectionAtom(v, s);
          ur := ch.upRef;
          type := TypeCodeToXType(v, tc);
          IF name = X.None THEN
            RAISE VBT.Error(VBT.ErrorCode.Unwritable)
          ELSIF ur.xcage = X.None THEN
            RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
          ELSIF vl # NIL AND TYPECODE(vl) # tc OR type = X.None THEN
            RAISE VBT.Error(VBT.ErrorCode.WrongType)
          END;
          mulsym := XClientF.NewAtom(v);
          multi[0] := ToAtom(v, "DELETE");
          multi[1] := XClientF.NewAtom(v);
          multi[2] := ToAtom(v, "INSERT_PROPERTY");
          multi[3] := XClientF.NewAtom(v);
          XProperties.PutProp(v, ur.xcage, mulsym, ToAtom(v, "ATOM_PAIR"),
                              LOOPHOLE(multi, ARRAY [0 .. 15] OF CHAR), 32);
          IF vl # NIL AND tc = TYPECODE(TEXT) THEN
            buf := NEW(UNTRACED REF ARRAY OF CHAR, Text.Length(vl));
            IF Text.Length(vl) > 0 THEN Text.SetChars(buf^, vl) END;
            XProperties.PutProp(v, ur.xcage, multi[3], type, buf^, 8);
            DISPOSE(buf);
          ELSE
            RAISE VBT.Error(VBT.ErrorCode.WrongType)
          END;
          TRY
            EVAL
              XProperties.AwaitConversion(
                v, ur.xcage, name, ToAtom(v, "MULTIPLE"), mulsym, ts, 20)
          EXCEPT
            VBT.Error (err) =>
              error := TRUE;
              IF err = VBT.ErrorCode.Unreadable THEN
                ec := VBT.ErrorCode.Unwritable
              ELSE
                ec := err
              END
          END;
          IF NOT error OR ec # VBT.ErrorCode.TimeOut THEN
            XClientF.FreeAtom(v, mulsym);
            VAR atm: X.Atom;
            BEGIN
              atm := multi[1];
              XClientF.FreeAtom(v, atm);
              multi[1] := atm;
              atm := multi[3];
              XClientF.FreeAtom(v, atm);
              multi[3] := atm
            END;
          END;
          IF error THEN RAISE VBT.Error(ec) END
        FINALLY
          Exit(v)
        END
      END
    EXCEPT
      TrestleComm.Failure => RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    END
  END WriteUp;

PROCEDURE ReadUp (           v : T;
                             ch: VBT.T;
                  <*UNUSED*> w : VBT.T;
                             s : VBT.Selection;
                             ts: VBT.TimeStamp;
                             tc: CARDINAL       ): VBT.Value
  RAISES {VBT.Error} =
  VAR
    sc : VBT.T  := NIL;
    res: REFANY;
  BEGIN
    TRY
      Enter(v);
      TRY
        XProperties.ExtendSel(v.sel, s);
        IF v.sel[s.sel].v # NIL THEN
          sc := v.sel[s.sel].v
        ELSE
          VAR
            name              := SelectionAtom(v, s);
            ur       : Child  := ch.upRef;
            type              := TypeCodeToXType(v, tc);
            sym, rsym: X.Atom;
          BEGIN
            IF name = X.None THEN
              RAISE VBT.Error(VBT.ErrorCode.Unreadable)
            ELSIF ur.xcage = X.None THEN
              RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
            ELSIF type = X.None THEN
              RAISE VBT.Error(VBT.ErrorCode.WrongType)
            END;
            sym := XClientF.NewAtom(v);
            TRY
              rsym := XProperties.AwaitConversion(
                        v, ur.xcage, name, type, sym, ts);
              IF rsym # sym THEN XClientF.FreeAtom(v, sym) END;
              res := XProperties.ReadXSelFromProp(v, ur.xcage, rsym, type);
              XClientF.FreeAtom(v, sym);
            EXCEPT
              VBT.Error (ec) =>
                IF ec # VBT.ErrorCode.TimeOut THEN
                  XClientF.FreeAtom(v, sym)
                END;
                RAISE VBT.Error(ec)
            END
          END
        END
      FINALLY
        Exit(v)
      END;
      IF sc # NIL THEN
        RETURN sc.read(s, tc)
      ELSE
        RETURN VBT.FromRef(res)
      END
    EXCEPT
      TrestleComm.Failure => RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    END
  END ReadUp;

PROCEDURE SetSizeHints (VAR      xhints       : X.XSizeHints;
                        VAR      width, height: CARDINAL;
                        READONLY sh, sv       : VBT.SizeRange;
                                 st           : XScreenType.T ) =
  BEGIN
    IF sv.hi < VBT.DefaultShape.hi OR sh.hi < VBT.DefaultShape.hi
      THEN xhints.flags := X.PMinSize + X.PMaxSize + X.PResizeInc + X.PBaseSize
      ELSE xhints.flags := X.PMinSize + X.PResizeInc + X.PBaseSize
    END;
    xhints.base_width := 0;
    xhints.base_height := 0;
    xhints.width_inc := 1;
    xhints.height_inc := 1;
    xhints.min_width := sh.lo;
    xhints.max_width :=
      MAX(MIN(sh.hi - 1, Rect.HorSize(st.rootDom)), sh.lo);
    xhints.min_height := sv.lo;
    xhints.max_height :=
      MAX(MIN(sv.hi - 1, Rect.VerSize(st.rootDom)), sv.lo);
    IF sh.pref # 0 THEN
      width := MIN(sh.pref, xhints.max_width)
    ELSIF (sh.hi > 1) AND (sh.hi <= width) THEN
      width := xhints.max_width
    END;
    IF sv.pref # 0 THEN
      height := MIN(sv.pref, xhints.max_height)
    ELSIF (sv.hi > 1) AND (sv.hi <= height) THEN
      height := xhints.max_height
    END;
  END SetSizeHints;

PROCEDURE SetXShape (v: T; ch: VBT.T) =
  (* LL = VBT.mu *)
  VAR
    xhints       : X.XSizeHints;
    seqno        : Ctypes.unsigned_long;
    mustReshape                         := FALSE;
    width, height: CARDINAL             := 0;
    ur           : Child                := ch.upRef;
    wf                                  := NEW(MessageWaitFor);
    st           : XScreenType.T        := ch.st;
    s                                   := VBTClass.GetShapes(ch);
    sh                                  := s[Axis.T.Hor];
    sv                                  := s[Axis.T.Ver];
  BEGIN
    SetSizeHints(xhints, width, height, sh, sv, st);
    TRY
      Enter(v);
      TRY
        IF sh = ur.sh AND sv = ur.sv THEN RETURN END;
        ur.sh := sh;
        ur.sv := sv;
        X.XSetWMNormalHints(v.dpy, ur.w, ADR(xhints));
        IF ((width # ur.width) OR (height # ur.height)) AND (width # 0)
             AND (height # 0) THEN
          seqno := X.XNextRequest(v.dpy);
          X.XResizeWindow(v.dpy, ur.w, width, height);
          IF ur.mapped THEN
            wf.types[0] := X.ConfigureNotify;
            wf.types[1] := X.UnmapNotify;
            wf.types[2] := 0;
            wf.types[3] := X.ClientMessage;
            wf.atm := ToAtom(v, "WM_CONFIGURE_DENIED");
            wf.d := ur.w;
            wf.reqno := seqno;
            WITH type = XClientF.Await(v, wf, 2) DO
              IF type > 1 AND type # X.ClientMessage THEN
                mustReshape := TRUE;
                XClientF.GetDomain(ur, width, height)
              END
            END
          END
        END
      FINALLY
        Exit(v);
        IF mustReshape THEN XClientF.Reshape(ch, width, height) END
      END
    EXCEPT
      X.Error, TrestleComm.Failure => (* skip *)
    END
  END SetXShape;

TYPE
  MessageWaitFor =
    SimpleWaitFor OBJECT atm: X.Atom OVERRIDES match := MessageMatch END;

PROCEDURE MessageMatch (wf: MessageWaitFor; READONLY ev: X.XEvent):
  BOOLEAN =
  BEGIN
    IF NOT SimpleWaitFor.match(wf, ev) THEN RETURN FALSE END;
    WITH e    = LOOPHOLE(ADR(ev), X.XAnyEventStar),
         type = e.type                              DO
      IF type # X.ClientMessage THEN RETURN TRUE END;
      WITH cm = LOOPHOLE(e, X.XClientMessageEventStar) DO
        RETURN cm.message_type = wf.atm
      END
    END
  END MessageMatch;

PROCEDURE NewShape (v: T; ch: VBT.T) RAISES {} =
  BEGIN
    IF ch.st = NIL THEN RETURN END;
    VBT.Mark(v)
  END NewShape;

PROCEDURE Redisplay (v: T) =
  <*FATAL Split.NotAChild*>
  VAR ch := Split.Succ(v, NIL);
  BEGIN
    WHILE ch # NIL DO
      IF VBTClass.HasNewShape(ch) AND ch.st # NIL THEN
        SetXShape(v, ch)
      END;
      ch := Split.Succ(v, ch)
    END
  END Redisplay;

PROCEDURE ToName (trsl: T; a: X.Atom): TEXT RAISES {TrestleComm.Failure} =
  VAR name: TEXT;
  BEGIN
    TRY
    IF trsl.atoms.get(a, name) THEN
      RETURN name
    ELSE
      VAR
        cname       := X.XGetAtomName(trsl.dpy, a);
        name : TEXT;
      BEGIN
        IF cname = NIL THEN
          name := "Unknown atom 0" & Fmt.Unsigned(a) & "H"
        ELSE
          name := M3toC.CopyStoT(cname);
          X.XFree(cname)
        END;
        EVAL trsl.atoms.put(a, name);
        EVAL trsl.names.put(name, a);
        RETURN name
      END
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END ToName;

PROCEDURE ToAtom (trsl: T; name: TEXT): X.Atom
  RAISES {TrestleComm.Failure} =
  VAR a: X.Atom;
  BEGIN
    TRY
    IF trsl.names.get(name, a) THEN
      RETURN a
    ELSE
      VAR
        s         := M3toC.TtoS(name);
        a: X.Atom;
      BEGIN
        a := X.XInternAtom(trsl.dpy, s, X.False);
        EVAL trsl.atoms.put(a, name);
        EVAL trsl.names.put(name, a);
        RETURN a
      END
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END ToAtom;

PROCEDURE Acquire (           v    : T;
                              chVBT: VBT.T;
                   <*UNUSED*> w    : VBT.T;
                              s    : VBT.Selection;
                              ts   : VBT.TimeStamp  ) RAISES {VBT.Error} =
  VAR
    ch    : Child;
    focus : X.Window;
    revert: Ctypes.int;
  BEGIN
    TRY
      Enter(v);
      TRY
        ch := chVBT.upRef;
        IF ch = NIL OR ch.xcage = X.None THEN
          RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
        END;
        XProperties.ExtendOwns(ch.owns, s);
        XProperties.ExtendSel(v.sel, s);
        IF s = VBT.KBFocus THEN
          IF NOT (ch.isXFocus OR ch.inside AND ch.underXFocus) THEN
            X.XSetInputFocus(v.dpy, ch.w, X.RevertToParent, ts);
            X.XGetInputFocus(v.dpy, ADR(focus), ADR(revert));
            IF focus # ch.w THEN
              RAISE VBT.Error(VBT.ErrorCode.EventNotCurrent)
            END;
            ch.isXFocus := TRUE;
            ch.underXFocus := TRUE
          ELSIF ch.isXFocus THEN
            X.XSetInputFocus(v.dpy, ch.w, X.RevertToParent, ts)
          END;
          ch.owns[s.sel] := TRUE;
          v.sel[s.sel].v := chVBT;
          v.sel[s.sel].ts := ts
        ELSE
          X.XSetSelectionOwner(v.dpy, SelectionAtom(v, s), ch.w, ts);
          IF ch.w = X.XGetSelectionOwner(v.dpy, v.sel[s.sel].name) THEN
            ch.owns[s.sel] := TRUE;
            IF v.sel[s.sel].v # NIL THEN
              VAR
                ev: X.XEvent;
                ur: Child    := v.sel[s.sel].v.upRef;
              BEGIN
                WITH e = LOOPHOLE(ADR(ev), X.XSelectionClearEventStar) DO
                  e.type := X.SelectionClear;
                  e.send_event := X.False;
                  e.display := v.dpy;
                  e.window := ur.w;
                  e.selection := v.sel[s.sel].name;
                  e.time := 0;
                END;
                XClientF.BackDoor(v, ev)
              END
            END;
            v.sel[s.sel].v := chVBT;
            v.sel[s.sel].ts := ts
          ELSE
            RAISE VBT.Error(VBT.ErrorCode.EventNotCurrent)
          END
        END
      FINALLY
        Exit(v)
      END
    EXCEPT
      X.Error, TrestleComm.Failure =>
        RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    END
  END Acquire;

PROCEDURE Release (           v    : T;
                              chVBT: VBT.T;
                   <*UNUSED*> w    : VBT.T;
                              s    : VBT.Selection) RAISES {} =
  VAR ch: Child;
  BEGIN
    TRY
      Enter(v);
      TRY
        ch := chVBT.upRef;
        XProperties.ExtendOwns(ch.owns, s);
        XProperties.ExtendSel(v.sel, s);
        IF v.sel[s.sel].v = chVBT THEN
          IF s = VBT.KBFocus THEN
            IF ch.isXFocus THEN
              X.XSetInputFocus(
                v.dpy, X.PointerRoot, X.RevertToParent, v.sel[s.sel].ts)
            END
          ELSE
            X.XSetSelectionOwner(
              v.dpy, SelectionAtom(v, s), X.None, v.sel[s.sel].ts)
          END;
          v.sel[s.sel].v := NIL
        END
      FINALLY
        Exit(v)
      END
    EXCEPT
      X.Error, TrestleComm.Failure =>     (* skip *)
    END
  END Release;

PROCEDURE Forge (                    v     : T;
                                     chVBT : VBT.T;
                 <*UNUSED*>          w     : VBT.T;
                 <*UNUSED*>          type  : VBT.MiscCodeType;
                 <*UNUSED*> READONLY detail: ARRAY [0 .. 1] OF INTEGER)
  RAISES {VBT.Error} =
  VAR ch: Child := chVBT.upRef;
  BEGIN
    TRY
      Enter(v);
      TRY
        IF ch = NIL OR ch.xcage = X.None THEN
          RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
        END;
        EVAL Thread.Fork(NEW(ForgeClosure, v := v, chVBT := chVBT,
                             win := ch.xcage, stackSize := 20000));
      FINALLY
        Exit(v)
      END
    EXCEPT
      TrestleComm.Failure => RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    END
  END Forge;

PROCEDURE DoForge (self: ForgeClosure): REFANY RAISES {} =
  VAR
    data: INTEGER;
    wf            := NEW(XProperties.PropertyWaitFor);
  BEGIN
    TRY
      Enter(self.v);
      TRY
        wf.types[0] := 0;
        wf.types[1] := X.PropertyNotify;
        wf.d := self.win;
        wf.reqno := X.XNextRequest(self.v.dpy);
        wf.a := self.v.miscAtom;
        wf.ts := 0;
        X.XChangeProperty(
          self.v.dpy, self.win, self.v.miscAtom, self.v.miscAtom, 32,
          X.PropModeAppend, LOOPHOLE(ADR(data), Ctypes.unsigned_char_star)
        , 0);
        EVAL XClientF.Await(self.v, wf, 5);
      FINALLY
        Exit(self.v)
      END
    EXCEPT
      X.Error, TrestleComm.Failure => wf.ts := 0
    END;
    LOCK VBT.mu DO
      VBTClass.Misc(
        self.chVBT, VBT.MiscRec{VBT.TrestleInternal, VBT.NullDetail, wf.ts,
                                VBT.Forgery})
    END;
    RETURN NIL
  END DoForge;

TYPE
  ForgeClosure = Thread.SizedClosure OBJECT
                   v    : T;
                   chVBT: VBT.T;
                   win  : X.Window
                 OVERRIDES
                   apply := DoForge
                 END;

PROCEDURE Put (                    v     : T;
                                   chVBT : VBT.T;
               <*UNUSED*>          w     : VBT.T;
                                   s     : VBT.Selection;
                                   ts    : VBT.TimeStamp;
                                   type  : VBT.MiscCodeType;
                          READONLY detail: ARRAY [0 .. 1] OF INTEGER)
  RAISES {VBT.Error} =
  VAR
    ch : Child                   := chVBT.upRef;
    win: X.Window;
    ev : X.XClientMessageEvent_l;
  BEGIN
    TRY
      Enter(v);
      TRY
        IF ch = NIL OR ch.xcage = X.None THEN
          RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
        END;
        XProperties.ExtendSel(v.sel, s);
        IF s = VBT.KBFocus THEN
          win := X.InputFocus
        ELSE
          win := X.XGetSelectionOwner(v.dpy, SelectionAtom(v, s))
        END;
        IF win = X.None THEN
          RAISE VBT.Error(VBT.ErrorCode.UnownedSelection)
        END;
        ev.type := X.ClientMessage;
        ev.window := win;
        ev.message_type := v.miscAtom;
        ev.format := 32;
        WITH data = ev.data DO
          data[0] := ToAtom(v, VBT.MiscCodeTypeName(type));
          data[1] := ts;
          data[2] := ToAtom(v, VBT.SelectionName(s));
          data[3] := detail[0];
          data[4] := detail[1]
        END;
        EVAL
          X.XSendEvent(v.dpy, win, X.False, 0, LOOPHOLE(ADR(ev), X.XEventStar));
      FINALLY
        Exit(v)
      END
    EXCEPT
      X.Error, TrestleComm.Failure => RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    END
  END Put;

VAR namebuf: ARRAY [0 .. 255] OF Ctypes.char;

PROCEDURE Init () =
  BEGIN
    EVAL Unix.gethostname(ADR(namebuf[0]),
                          NUMBER(namebuf));
    MyHostName := X.XTextProperty{LOOPHOLE(ADR(namebuf[0]),
                                           Ctypes.unsigned_char_star),
                                  Xatom.XA_STRING, 8,
                                  Cstring.strlen(ADR(namebuf[0])) + 1};
    TrestleClass.RegisterConnectClosure(
      NEW(TrestleClass.ConnectClosure, apply := DoConnect))
  END Init;

PROCEDURE DoConnect (self     : TrestleClass.ConnectClosure;
                     inst     : TEXT;
                     localOnly: BOOLEAN;
         VAR (*OUT*) t        : Trestle.T): BOOLEAN =
  VAR
    res := XClientF.DoConnect (self, inst, localOnly, t);
    xt  : T := t;
    cp  : Ctypes.char_star;
    txt : TEXT;
  BEGIN
    IF (xt # NIL) AND NOT brokenOpenWin THEN
      cp  := X.XServerVendor (xt.dpy);
      IF (cp # NIL) THEN
        txt := M3toC.StoT (cp);
        IF Text.Equal (Text.Sub (txt, 0, 10), "X11/NeWS -") THEN
          brokenOpenWin := TRUE;
        END;
      END;
    END;
    RETURN res;
  END DoConnect;

EXCEPTION FatalError;

PROCEDURE Crash() =        
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError
  END Crash;

BEGIN
END XClient.
