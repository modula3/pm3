(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Thu May 16 16:08:08 PDT 1996 by mhb      *)
(*      modified on Mon Sep 09 10:06:44 EDT 1995 by dagenais *)
(*      modified on Tue Jan 31 09:42:47 PST 1995 by kalsow   *)
(*      modified on Wed Mar 18 15:46:44 PST 1992 by msm      *)
(*      modified on Tue Mar 10 19:07:02 1992 by steveg   *)
(*      modified on Mon Feb 24 13:52:20 PST 1992 by muller   *)
(*      modified on Sun Nov 10 21:30:22 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE AnchorHelpVBT;

IMPORT VBT, Filter, ZSplit, Point, Rect, Trestle, Axis,
Split, VBTClass, TrestleComm, Time, Thread;

REVEAL 
  T = Public BRANDED OBJECT
    n: CARDINAL;		(* number of ZSplit to skip *)
    hfudge, vfudge: REAL;	(* where to pop the help window *)
    active: BOOLEAN;            (* help window popped *)
    in: BOOLEAN;                (* position is inside *)
  OVERRIDES 
    position := Position;
    init := Be
  END;

PROCEDURE Be(
  v: T;
  ch: VBT.T; 
  help: VBT.T;
  n: CARDINAL := 0;
  hfudge := 0.0;
  vfudge := 1.0): T RAISES {} =
  BEGIN
    v.help := help;
    v.n := n;
    v.hfudge := hfudge;
    v.vfudge := vfudge;
    v.active := FALSE;
    v.in := FALSE;
    EVAL Filter.T.init(v, ch);
    VBT.SetCage(v, VBT.GoneCage);
    RETURN v;
  END Be; 
  
PROCEDURE New(
  ch: VBT.T; 
  help: VBT.T;
  n: CARDINAL := 0;
  hfudge := 0.0;
  vfudge := 1.0): T RAISES {} =
  VAR res := NEW(T);
  BEGIN
    RETURN Be(res, ch, help, n, hfudge, vfudge);
  END New;

(* Simply calls Enter and Leave which do all the work. A simpler
   overridden "position" method may simply call Activate and
   Deactivate. *)

PROCEDURE Position(v: T; READONLY cd: VBT.PositionRec) RAISES {} =
  BEGIN
    IF cd.cp.gone THEN 
      Leave(v);
      VBT.SetCage(v, VBT.GoneCage);
    ELSE
      Enter(v);
      VBT.SetCage(v, VBT.InsideCage);
    END;

    Filter.T.position(v, cd);
    (* The nested child should be entered last because the timer
       needs to keep the innermost T entered. *)

  END Position;

(* This section is almost verbatim from AnchorBtnVBT, it should somehow
   be shared. *)

PROCEDURE GetZSplit(v: T): ZSplit.T =
  VAR m := v.n; z := v.parent;  
  BEGIN
    LOOP
      IF z = NIL THEN RETURN NIL END;
      IF ISTYPE(z, ZSplit.T) THEN
        IF m = 0 THEN RETURN z ELSE DEC(m) END;
      END;
      z := z.parent;
    END;
  END GetZSplit; 

(* Pop up the help window and remember that it is active. *)    

PROCEDURE Activate(v: T) =
  VAR
    pt := Point.MoveHV(Rect.SouthWest(VBT.Domain(v)), 
      ROUND(VBT.MMToPixels(v, v.hfudge, Axis.T.Hor)),
      ROUND(VBT.MMToPixels(v, v.vfudge, Axis.T.Ver)));
    z := GetZSplit(v);
    dom: Rect.T;
  BEGIN
    IF v.active THEN RETURN; END;
    v.active := TRUE;

    IF v.help.st # v.st THEN VBTClass.Rescreen(v.help, v.st) END;
    IF z = NIL THEN
      (* insert help as top-level window *)
      WITH srec = Trestle.ScreenOf(v, pt) DO
        IF srec.trsl # NIL THEN
          dom := Shift(MinRect(v.help, srec.q), srec.dom);
          TRY
            Trestle.Attach(v.help, srec.trsl);
            Trestle.Overlap(v.help, srec.id, Rect.NorthWest(dom));
          EXCEPT
            TrestleComm.Failure => v.active := FALSE;
          END
        END
      END
    ELSE
      (* insert menu in z *)
      dom := Shift(MinRect(v.help, pt), VBT.Domain(z));
      ZSplit.Insert(z, v.help, dom);
    END;
  END Activate;

(* From AnchorBtnVBT *)

PROCEDURE Shift(READONLY menu, parent: Rect.T): Rect.T =
  (* Shift the menu left until it is entirely contained in parent or until its
     left edge coincides with the left edge of parent, unless it needs
     shifting to the right, in which shift until the left edge of menu is
     visible. Do the same thing vertically. *)
  VAR dh, dv: INTEGER;
  BEGIN
    dh := MAX(MIN(0, parent.east - menu.east), parent.west - menu.west);
    dv := MAX(MIN(0, parent.south - menu.south), parent.north - menu.north);
    RETURN Rect.MoveHV(menu, dh, dv);
  END Shift;

PROCEDURE MinRect(v: VBT.T; READONLY pt: Point.T): Rect.T =
  BEGIN
    RETURN 
      Rect.FromCorner(pt,
        VBTClass.GetShape(v, Axis.T.Hor, 0).lo, 
        VBTClass.GetShape(v, Axis.T.Ver, 0).lo)
  END MinRect;

(* Remove the help window and remember that it is inactive. *)

PROCEDURE Deactivate(v: T) =
  <* FATAL Split.NotAChild *>
  BEGIN
    IF NOT v.active THEN RETURN END;
    v.active := FALSE;

    WITH z = GetZSplit(v) DO
      IF z = NIL THEN
        Trestle.Delete(v.help)
      ELSE
        Split.Delete(z, v.help);
      END;
    END;
  END Deactivate;
        
PROCEDURE IsActive(v: T): BOOLEAN =
  BEGIN
    IF VBT.Parent(v) = NIL THEN RETURN FALSE END;
    RETURN v.active;
  END IsActive;

PROCEDURE Set(v: T; n: CARDINAL; 
  hfudge, vfudge: REAL) =
  BEGIN 
    IF IsActive(v) THEN Crash() END;
    v.n := n; v.hfudge := hfudge; v.vfudge := vfudge;
  END Set;
  
PROCEDURE Get(v: T; VAR n: CARDINAL; VAR hfudge, vfudge: REAL) =
  BEGIN
    n := v.n; hfudge := v.hfudge; vfudge := v.vfudge;
  END Get;

(* The Help timer is installed as a property of the top level VBT
   in the tree. It remembers how long the position has been out/in
   an AnchorHelpVBT. *)

TYPE
  HelpTimer = MUTEX OBJECT
      inHelp: INTEGER;		(* number of nested T we are in *)
      help: T;                  (* innermost T visited *)
      inHelpTime: LONGREAL;     (* entry time in T *)
      outHelpTime: LONGREAL;	(* time of exit of T *)
      inDelay: LONGREAL;        (* delay before entering help mode *)
      outDelay: LONGREAL;       (* delay before leaving help mode *)
      helpMode: BOOLEAN;        (* are we in help mode *)
      thread: Thread.T;         (* thread associated with the timer *)
    METHODS
      init(): HelpTimer := InitHelpTimer;
    END;

  HelpThread = Thread.Closure OBJECT
      timer: HelpTimer;
    OVERRIDES
      apply := ApplyTimer;
    END;

PROCEDURE InitHelpTimer(self: HelpTimer): HelpTimer =
  BEGIN
    self.inHelp := 0;
    self.help := NIL;
    self.inHelpTime := Time.Now();
    self.outHelpTime := Time.Now();
    self.inDelay := 0.8D0;
    self.outDelay := 0.4D0;
    self.helpMode := FALSE;
    self.thread := Thread.Fork(NEW(HelpThread, timer := self));
    RETURN self;
  END InitHelpTimer;

PROCEDURE ApplyTimer(self: HelpThread): REFANY =
  VAR
    timer := self.timer;
    now, delay, wait: LONGREAL;
    notify: T;
  BEGIN
    LOOP
      TRY
        LOCK timer DO
          wait := 100.0D0;
          notify := NIL;

          (* We are inside one or more T, should we enter help mode *)

          IF (timer.inHelp > 0) AND (NOT timer.helpMode) THEN
            now := Time.Now();
            delay := now - timer.inHelpTime;
            IF delay >= timer.inDelay THEN
              timer.helpMode := TRUE;
              notify := timer.help;
            ELSE
              wait := timer.inDelay - delay;
            END;

          (* We are outside any T, should we leave help mode *)

          ELSIF (timer.inHelp = 0) AND timer.helpMode THEN
            now := Time.Now();
            delay := now - timer.outHelpTime;
            IF delay >= timer.outDelay THEN
              timer.helpMode := FALSE;
            ELSE
              wait := timer.outDelay - delay;
            END;
          END;
        END;

        (* The innermost T entered before changing to help mode should
           be notified of the change by setting the cage to empty, and thus
           insuring the delivery of a position event to it and all its
           enclosing parents. *)

        IF notify # NIL THEN
          LOCK VBT.mu DO
            VBT.SetCage(notify,VBT.EmptyCage);
          END;
        END;

        Thread.AlertPause(wait);
      EXCEPT
      | Thread.Alerted =>
      END;
    END;
  END ApplyTimer;

(* Walk the VBT tree to the root and get the timer from its property list.
   If there is no timer yet, one is created. By attaching the timer to
   the root window, all the T within the same Trestle top window share the
   same timer but T in other top level windows and screens have their
   own timer. *)

PROCEDURE GetTimer(v: VBT.T): HelpTimer =
  VAR
    prop: REFANY;
  BEGIN
    WHILE v.parent # NIL DO
      v := v.parent;
    END;
    prop := VBT.GetProp(v,TYPECODE(HelpTimer));

    (* There is no timer, one is created *)

    IF prop = NIL THEN
      prop := NEW(HelpTimer).init();
      VBT.PutProp(v,prop);
    END;
    RETURN NARROW(prop,HelpTimer);
  END GetTimer;

(* The timer is accessed and its status updated. If the change is significant,
   the timer thread is alerted. *)

PROCEDURE Enter(v: T) =
  VAR
    timer := GetTimer(v);
    now: LONGREAL;
  BEGIN
    LOCK timer DO
      IF timer.helpMode THEN
        Activate(v);
      ELSE
        now := Time.Now();

        (* A T was just entered after being out for more than outDelay,
           the timer is started to enter help mode in inDelay. *)

        IF timer.inHelp = 0 AND 
           ((now - timer.outHelpTime) > timer.outDelay) THEN
          timer.inHelpTime := now;
        END;
        Thread.Alert(timer.thread);
      END;

      (* v was just entered. The number of nested T is incremented. *)

      IF NOT v.in THEN
        v.in := TRUE;
        INC(timer.inHelp);
        timer.help := v;
      END;
    END;
  END Enter;

PROCEDURE Leave(v: T) =
  VAR
    timer := GetTimer(v);
    now := Time.Now();
  BEGIN
    LOCK timer DO

      (* the number of nested T is decremented *)

      IF v.in THEN
        v.in := FALSE;
        DEC(timer.inHelp);
      END;

      (* the time when the last nested T is left is remembered *)

      IF timer.inHelp = 0 THEN
        timer.help := NIL;
        timer.outHelpTime := now;
      END;

      Deactivate(v);

      IF timer.helpMode THEN
        Thread.Alert(timer.thread);
      END;
    END;
  END Leave;

PROCEDURE GetDelay(v: VBT.T; VAR inDelay, outDelay: LONGREAL) =
  VAR
    timer := GetTimer(v);
  BEGIN
    LOCK timer DO
      inDelay := timer.inDelay;
      outDelay := timer.outDelay;
    END;
  END GetDelay;

PROCEDURE SetDelay(v: VBT.T; inDelay, outDelay: LONGREAL) =
  VAR
    timer := GetTimer(v);
  BEGIN
    LOCK timer DO
      timer.inDelay := inDelay;
      timer.outDelay := outDelay;
    END;
  END SetDelay;

EXCEPTION FatalError;

PROCEDURE Crash () =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Crash;

BEGIN END AnchorHelpVBT.
    
