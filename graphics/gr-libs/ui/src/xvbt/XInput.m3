(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Tue Feb  7 15:43:56 PST 1995 by kalsow   *)
(*      modified on Fri Jul  8 17:10:20 PDT 1994 by msm      *)
(*      modified on Tue Nov 23 14:52:08 PST 1993 by steveg   *)
(*      modified on Tue Jul 20 17:28:39 PDT 1993 by sfreeman *)
(*      modified on Mon Feb 24 13:59:46 PST 1992 by muller   *)
<*PRAGMA LL*>

UNSAFE MODULE XInput;

IMPORT XClient, XClientF, XEventQueue, XScrollQueue, X, 
       Thread, Unix, SchedulerPosix, Word, Rect, Region,
       TrestleClass, VBTClass, VBT, Ctypes;

PROCEDURE Start (trsl: XClient.T; stackSize := 20000) =
  BEGIN
    EVAL
      Thread.Fork(
        NEW(WaitForXInputClosure, xcon := trsl, stackSize := stackSize));
    EVAL Thread.Fork(
           NEW(FilterXInputClosure, xcon := trsl, stackSize := stackSize));
  END Start;

TYPE
  WaitForXInputClosure = Thread.SizedClosure OBJECT
                           xcon: XClient.T
                         OVERRIDES
                           apply := WaitForXInput
                         END;

PROCEDURE WaitForXInput (self: WaitForXInputClosure): REFANY RAISES {} =
  VAR
    checkIfClosed          := FALSE;
    n            : Ctypes.int;
  BEGIN
    TRY
      WITH xcon = self.xcon,
           dpy  = xcon.dpy,
           file = X.XConnectionNumber(dpy) DO
        LOOP
          LOCK xcon DO
            WHILE (X.XEventsQueued(dpy, X.QueuedAfterReading) # 0)
                    AND NOT xcon.dead DO
              checkIfClosed := FALSE;
              Thread.Signal(xcon.qNonEmpty);
              Thread.Wait(xcon, xcon.qEmpty)
            END;
            IF xcon.dead THEN EXIT END;
            IF checkIfClosed THEN
              IF (SchedulerPosix.IOWait(file, TRUE, 0.0D0)
                    # SchedulerPosix.WaitResult.Timeout)
                   AND (Unix.ioctl(file, Unix.FIONREAD, ADR(n)) < 0
                          OR n = 0) THEN
                XClientF.Kill(xcon);
                EXIT
              END
            END
          END;
          EVAL SchedulerPosix.IOWait(file, TRUE);
          checkIfClosed := TRUE
        END
      END
    EXCEPT
      X.Error => (* skip *)
    END;
    RETURN NIL
  END WaitForXInput;

TYPE
  FilterXInputClosure = Thread.SizedClosure OBJECT
                          xcon: XClient.T
                        OVERRIDES
                          apply := FilterXInput
                        END;

PROCEDURE FilterXInput (self: FilterXInputClosure): REFANY RAISES {} =
  VAR
    evRec  : X.XEvent;
    ev     := LOOPHOLE(ADR(evRec), X.XAnyEventStar);
    waiter : XClientF.WaitFor;
    ra     : REFANY;
    enqueue: BOOLEAN;
    v      : VBT.T;
    popped := FALSE;             (* = in the middle of a sequence of
                                    GraphicsExpose *)
    ur  : XClientF.Child;
    xcon                 := self.xcon;
    dpy                  := xcon.dpy;
  <*FATAL XEventQueue.Exhausted*>
  BEGIN
    TRY
      LOCK xcon DO
        LOOP
          WHILE (X.XEventsQueued(dpy, X.QueuedAfterReading) = 0)
                  AND NOT xcon.dead AND XEventQueue.IsEmpty(xcon.errq)
                  AND XEventQueue.IsEmpty(xcon.ooq) DO
            Thread.Signal(xcon.qEmpty);
            Thread.Wait(xcon, xcon.qNonEmpty)
          END;
          IF xcon.dead THEN EXIT END;
          IF NOT XEventQueue.IsEmpty(xcon.errq) THEN
            evRec := XEventQueue.Remove(xcon.errq)
          ELSIF XEventQueue.IsEmpty(xcon.ooq) THEN
            X.XNextEvent(dpy, ADR(evRec))
          ELSIF X.XEventsQueued(dpy, X.QueuedAlready) = 0 THEN
            evRec := XEventQueue.Remove(xcon.ooq)
          ELSE
            VAR
              ooevr                  := XEventQueue.Peek(xcon.ooq);
              ooev := LOOPHOLE(ADR(ooevr), X.XAnyEventStar);
            BEGIN
              X.XPeekEvent(dpy, ADR(evRec));
              IF Word.Minus(ev.serial, ooev.serial) < 0 THEN
                X.XNextEvent(dpy, ADR(evRec))
              ELSE
                evRec := XEventQueue.Remove(xcon.ooq)
              END
            END
          END;
          IF (ev.type # 0) AND xcon.vbts.get(ev.window, ra) THEN
            v := ra;
            ur := v.upRef
          ELSE
            v := NIL;
            ur := NIL
          END;
          waiter := XClientF.FindWaiter(xcon, evRec);
          CASE ev.type OF
            X.KeyPress, X.KeyRelease =>
              WITH e = LOOPHOLE(ev, X.XKeyEventStar) DO
                IF (ur # NIL) AND NOT ur.nwValid
                     AND (e.same_screen # X.False) THEN
                  ur.nw.h := e.x_root - e.x;
                  ur.nw.v := e.y_root - e.y;
                  ur.nwValid := TRUE
                END
              END;
              enqueue := ur # NIL
          | X.ButtonPress, X.ButtonRelease =>
              WITH e = LOOPHOLE(ev, X.XButtonEventStar) DO
                IF ur # NIL AND e.same_screen # X.False THEN
                  IF NOT ur.nwValid THEN
                    ur.nw.h := e.x_root - e.x;
                    ur.nw.v := e.y_root - e.y;
                    ur.nwValid := TRUE
                  END;
                  IF ur.inside THEN
                    e.subwindow := ur.w
                  ELSE
                    e.subwindow := X.None
                  END
                END
              END;
              enqueue := ur # NIL
          | X.EnterNotify, X.LeaveNotify =>
              WITH e = LOOPHOLE(ev, X.XCrossingEventStar) DO
                IF (ur # NIL) AND NOT ur.nwValid
                     AND (e.same_screen # X.False) THEN
                  ur.nw.h := e.x_root - e.x;
                  ur.nw.v := e.y_root - e.y;
                  ur.nwValid := TRUE
                END;
                enqueue :=
                  (e.root = e.window AND e.detail # X.NotifyInferior)
                    OR ((ur # NIL)
                          AND ((e.type = X.EnterNotify)
                                 AND (e.detail # X.NotifyVirtual)
                                 AND (e.detail # X.NotifyNonlinearVirtual)
                                 OR (e.type = X.LeaveNotify)
                                      AND (e.detail # X.NotifyInferior)));
                IF ur # NIL THEN
                  ur.inside :=
                    e.type = X.EnterNotify OR e.detail = X.NotifyInferior;
                  IF NOT ur.inside THEN ur.recentlyOutside := TRUE END;
                  ur.underXFocus := e.focus # X.False
                END;
              END
          | X.MotionNotify =>
              WITH e = LOOPHOLE(ev, X.XMotionEventStar) DO
                IF (ur # NIL) AND NOT ur.nwValid
                     AND (e.same_screen # X.False) THEN
                  ur.nw.h := e.x_root - e.x;
                  ur.nw.v := e.y_root - e.y;
                  ur.nwValid := TRUE
                END;
              END;
              enqueue := TRUE
          | X.FocusIn, X.FocusOut =>
              WITH e = LOOPHOLE(ev, X.XFocusChangeEventStar) DO
                enqueue := ur # NIL AND e.mode # X.NotifyGrab
                             AND e.mode # X.NotifyUngrab;
                IF enqueue THEN
                  ur.isXFocus :=
                    e.type = X.FocusIn AND e.detail # X.NotifyPointer;
                  ur.underXFocus :=
                    e.type = X.FocusIn OR e.detail = X.NotifyAncestor;
                END
              END
          | X.UnmapNotify =>
              IF (ur # NIL) AND ur.mapped THEN
                enqueue := NOT ur.reshapeComing;
                IF enqueue THEN
                  ur.reshapeComing := TRUE;
                  ur.oldWidth := ur.width;
                  ur.oldHeight := ur.height
                END;
                ur.mapped := FALSE;
                ur.serial := ev.serial
              ELSE
                enqueue := FALSE
              END
          | X.MapNotify =>
              IF (ur # NIL) AND NOT ur.mapped THEN
                enqueue := NOT ur.reshapeComing;
                IF enqueue THEN
                  ur.reshapeComing := TRUE;
                  ur.oldWidth := 0;
                  ur.oldHeight := 0
                END;
                ur.mapped := TRUE;
                ur.serial := ev.serial
              ELSE
                enqueue := FALSE
              END
          | X.ReparentNotify =>
              IF ur # NIL THEN ur.nwValid := FALSE END;
              enqueue := FALSE
          | X.ConfigureNotify =>
              WITH e = LOOPHOLE(ev, X.XConfigureEventStar) DO
                IF ur # NIL THEN
                  IF e.send_event # X.False THEN
                    ur.nw.h := e.x + e.border_width;
                    ur.nw.v := e.y + e.border_width;
                    ur.nwValid := TRUE
                  ELSE
                    ur.nwValid := FALSE
                  END;
                  IF ur.width = e.width AND ur.height = e.height THEN
                    e.send_event := X.True
                  ELSE
                    e.send_event := X.False
                  END;
                  enqueue := NOT ur.reshapeComing AND ur.mapped;
                  IF NOT ur.reshapeComing THEN
                    ur.oldWidth := ur.width;
                    ur.oldHeight := ur.height;
                    ur.reshapeComing := enqueue
                  END;
                  ur.width := e.width;
                  ur.height := e.height;
                  ur.serial := e.serial
                ELSE
                  enqueue := FALSE
                END
              END
          | X.PropertyNotify => enqueue := FALSE
          | X.GraphicsExpose =>
              enqueue := FALSE;
              IF (waiter = NIL) AND (ur # NIL) THEN
                WITH e = LOOPHOLE(ev, X.XGraphicsExposeEventStar) DO
                  IF NOT popped THEN PopScroll(ur) END;
                  ExpandBadRegion(
                    ur, XClientF.ToRect(e.x, e.y, e.width, e.height));
                  popped := e.count # 0;
                  IF NOT popped THEN enqueue := TRUE END
                END
              END
          | X.NoExpose =>
              IF (waiter = NIL) AND (ur # NIL) THEN PopScroll(ur) END;
              enqueue := FALSE
          | X.Expose =>
              IF (waiter = NIL) AND (ur # NIL) THEN
                WITH e = LOOPHOLE(ev, X.XExposeEventStar) DO
                  IF e.serial = ur.serial THEN
                    ExpandBadRegion(
                      ur, Rect.Meet(
                            XClientF.ToRect(e.x, e.y, e.width, e.height),
                            Rect.FromSize(ur.oldWidth, ur.oldHeight)))
                  ELSE
                    ExpandBadRegion(
                      ur, XClientF.ToRect(e.x, e.y, e.width, e.height))
                  END;
                  enqueue := e.count = 0
                END
              ELSE
                enqueue := FALSE
              END
          | X.MappingNotify => enqueue := TRUE
          | X.DestroyNotify, X.SelectionClear, X.ClientMessage,
              X.VisibilityNotify =>
              enqueue := ur # NIL
          ELSE
            enqueue := FALSE
          END;
          IF waiter # NIL THEN
            waiter.notify(evRec, xcon);

            (* waiter.turn := TRUE; waiter.ev := evRec; waiter.timeout :=
               FALSE; Thread.Signal(waiter); WHILE waiter.turn AND NOT
               xcon.dead DO Thread.Wait(xcon, waiter) END *)

            (* selection requests now have a waiter *)
            (* ELSIF ev.type = X.SelectionRequest THEN WITH e =
               LOOPHOLE(ev, X.XSelectionRequestEventStar) DO FOR s :=
               FIRST(xcon.sel^) TO LAST(xcon.sel^) DO IF xcon.sel[s].name =
               e.selection THEN XProperties.StartSelection( xcon,
               e.requestor, e.target, e.property, VBT.Selection{s}, e.time)
               END END END *)
          ELSIF enqueue THEN
            IF XEventQueue.IsEmpty(xcon.evq) THEN
              Thread.Signal(xcon.evc)
            END;
            XEventQueue.Insert(xcon.evq, evRec)
          ELSIF xcon.eventHook # NIL THEN
            xcon.eventHook(xcon, evRec)
          END
        END
      END
    EXCEPT
      X.Error => (* skip *)
    END;
    RETURN NIL
  END FilterXInput;

PROCEDURE PopScroll (ur: XClientF.Child) =
  (* Remove a scroll from the queue of unacknowleged scrolls in ur.  LL =
     ur.ch.parent *)
  <*FATAL XScrollQueue.Exhausted*>
  BEGIN
    EVAL XScrollQueue.Remove(ur.scrollQ)
  END PopScroll;

PROCEDURE ExpandBadRegion (ur: XClientF.Child; READONLY br: Rect.T) =
  (* Every x-vbt has an x-bad-region, which this expands.  If you call
     this, you must eventually call DeliverBadRegion.  LL = ur.ch.parent *)
  VAR
    i   := ur.scrollQ.lo;
    bad := Region.FromRect(br);
  BEGIN
    WITH hi   = ur.scrollQ.hi,
         buff = ur.scrollQ.buff DO
      WHILE i # hi DO
        WITH sc = buff[i] DO
          bad := Region.Join(bad, Region.Add(Region.MeetRect(
                                               Rect.Sub(sc.clip, sc.delta),
                                               bad), sc.delta))
        END;
        INC(i);
        IF i = NUMBER(buff^) THEN i := 0 END
      END
    END;
    ur.badR := Region.Join(ur.badR, bad)
  END ExpandBadRegion;

BEGIN
END XInput.

