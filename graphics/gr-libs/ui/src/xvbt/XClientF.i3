(* Copyright C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* modified on Mon May 17 19:07:47 PDT 1993 by msm *)
(* modified on Fri May 7 16:43:36 PDT 1993 by mjordan *)
(* modified on Fri Apr 2 10:52:53 PST 1993 by steveg *)
(* modified on Mon Feb 24 13:59:43 PST 1992 by muller *)
(* modified on Sat Jan 11 19:03:47 PST 1992 by gnelson*)


<*PRAGMA LL*>

(* Partitioning following the efforts of
   Steve.Freeman@computer-lab.cambridge.ac.uk - 92-05-13 *)

UNSAFE INTERFACE XClientF;

IMPORT XClient, TrestleOnX, TrestleClass, Trestle, Rect, ProperSplit,
       IntRefTbl, IntTextTbl, TextIntTbl, X, XEventQueue, Thread,
       XScreenType, VBT, Point, Ctypes, XScrollQueue, Region, TrestleComm,
       ScrnPixmap;

REVEAL
  TrestleClass.RootVBT <: ProperSplit.T;
  TrestleOnX.Display <: T_Abs;

TYPE
  T_Abs <: T_Ext;                (* T_Ext revealed in XClientExt.i3 *)
  T_Ext <: T_Rel;
  T_Rel =
    XClient.T_Public OBJECT
      (* protection = self *)
      dead (*, hasMessenger, hasXFilter, hasSelectThread *) := FALSE;
      vbts : IntRefTbl.T;
      atoms: IntTextTbl.T;
      names: TextIntTbl.T;
      ungrab: ARRAY [0 .. 12] (*FIRST(Ungrab)..LAST(Ungrab)]*) OF X.KeyCode;
      sel: SelArray           := NIL;
      evq                     := XEventQueue.Empty;
      evc: Thread.Condition;
      (* signaled when evq.lo becomes different from evq.hi *)
      (* list of awaited events: *)
      qEmpty, qNonEmpty: Thread.Condition;
      (* qEmpty is signalled when the filterXInput thread discovers that
         the queue of events in Xlib is empty and wants the WaitForXInput
         thread to do a select on its behalf; qNonEmpty is signaled to
         alert the filterXInput thread that the queue in Xlib is non-empty,
         or that the error queue is non-empty. *)
      ooq := XEventQueue.Empty;  (* The queue of out-of-order events --
                                    such as MotionNotify from
                                    QueryPointer *)
      (*params: Trestle.Parameters;*)
      screens: REF ARRAY OF XScreenType.T;
      (* Types of the screens on the X server. *)
      defaultScreen: CARDINAL;
      (* index in screens of default screen for this X server. *)
      (* The next fields are protected by VBT.mu and self *)
      current, mouseFocus: VBT.T := NIL;
      (* The child that has received a FirstDown but no corresponding
         LastUp, or NIL if there is no such child. *)
      currentRoot, mouseFocusRoot := -1;
      (* If mouseFocus # NIL, mouseFocusRoot is the screen number of the
         root window containing it.  currentRoot is the screen number of
         the screen the cursor is on. *)
      currentRootWindow: X.Window := X.None;
      (* The root window of the current screen *)
      otherCages: BOOLEAN := FALSE;
      (* The remaining fields are protected by VBT.mu *)
      (* True if some VBT other than mouseFocus or current has a cage which
         fails to contain some point on currentRoot. *)
      takeFocus, deleteWindow, protocols, miscAtom, decTakeFocus, wmMoved,
        paNewScreen, paNewDisplay, paAddDisplay: X.Atom := X.None;
      errq                            := XEventQueue.Empty;
      eventHook: TrestleOnX.EventProc := NIL;
      (* protection = scheduler *)
      inst, fullinst: TEXT;
    END;

TYPE
  Child = ProperSplit.Child OBJECT
            (* fields below protected by parent lock *)
            nwValid            := FALSE;
            nw     : Point.T;
            (* The nw field is the location of the northwest corner of the
               window on the root window, if nwValid is TRUE.*)
            inside := FALSE;
            (* whether the cursor is inside the window. *)
            mapped := FALSE;
            (* whether the X window is mapped. *)
            isXFocus, underXFocus := FALSE;
            (* The boolean isXFocus is true if this window has the X
               keyboard focus, and underXFocus is true if the X keyboard
               focus is an ancestor of this window. *)
            owns           : OwnsArray := NIL;
            recentlyOutside            := TRUE;
            (* true if the cursor has been outside our window since the
               last time a takefocus message was sent *)
            width, height: CARDINAL;
            (* width and height of X window. *)
            serial             : Ctypes.unsigned_long;
            oldWidth, oldHeight                         := LAST(INTEGER);
            (* X exposure events that carry the given serial number and
               affect only the portion of the window outside of the old
               width and height can be discarded, since they are subsumed
               by a previous reshape. *)
            reshapeComing := FALSE;
            (* => a map, unmap, or configure event is in the parent's queue
               for this vbt. *)
            userPosition := FALSE;
            (* indicates whether the position to be set was generated by a
               user-specification in global coordinates *)
            w, xcage: X.Drawable;  (* xcage = X.None for offscreen VBTs *)
            cageRect: Rect.T;
            scrollQ                := XScrollQueue.Empty;
            (* The scroll queue contains the scrolling commands that have
               been issued but not yet acknowleged. *)
            badR := Region.Empty;
            (* The actual bad region of a Child ur is ur.badR union
               bad(ur.ch). *)
            sh, sv: VBT.SizeRange;
            (* The last hor and ver sizeranges that were reported to X. *)
            csid: X.Cursor;
            (* The last cursor id that was reported to X. *)
            cageCovered := FALSE;
            (* TRUE during delivery of a button click, to avoid setting the
               cage twice. *)
            decorated := FALSE;
            (* TRUE if the window is normal, FALSE if override-redirect;
               only valid after w is created. *)
            captureOnWrite: ScrnPixmap.T := NIL;
          END;

TYPE
  SelectionRecord = RECORD
                      v   : VBT.T         := NIL;
                      ts  : VBT.TimeStamp := 0;
                      name: X.Atom        := X.None
                    END;

  SelArray = REF ARRAY OF SelectionRecord;

  OwnsArray = REF ARRAY OF BOOLEAN;

TYPE
  NewScreenProp = REF RECORD
                        type, prop : X.Atom;
                        len, format: INTEGER;
                        data       : REF ARRAY OF Ctypes.char
                      END;

TYPE
  WaitFor =
    Thread.Condition OBJECT
      (* signalled when turn changes. *)
      ev     : X.XEvent;
      timeout: BOOLEAN;
      types               := ARRAY [0 .. 3] OF INTEGER{-1, ..};
      (* The types of events that this WaitFor might match, padded with
         -1s. *)
      (* remaining fields protected by the xcon containing this object in
         its await *)
      timelimit: INTEGER;
      (* -1 => no limit, else # of seconds until the waitfor times out. *)
      next: WaitFor := NIL;
      turn          := FALSE;
      (* FALSE => not yet matched; TRUE => matched and not yet processed *)
    METHODS
      match  (READONLY ev: X.XEvent): BOOLEAN;
      notify (READONLY evRec: X.XEvent; xcon: XClient.T);
      <* LL.sup = xcon *>
      (* the main input loop in XInput accepts an X event and performs some
         initial event type-specific processing.  If there is a WaitFor
         which matches the event, it calls /notify/.  The default
         implementation signals the WaitFor and blocks until it has been
         processed.  A SelectionRequest, however, starts selection
         threads *)
    END;

  SimpleWaitForPublic =
    WaitFor OBJECT
      d: X.Drawable;
      (* d # X.None => non-error events must contain d in order to
         match. *)
      reqno: Ctypes.unsigned_long;
      (* error events must contain this request no.  in order to match. *)
    END;

  SimpleWaitFor <: SimpleWaitForPublic;

PROCEDURE Kill (trsl: XClient.T);
<* LL.sup = trsl *>
(* clean way to close a Trestle *)

CONST Timeout = 1; (* not a valid value for an X event type *)

PROCEDURE Await (trsl: T_Abs; wf: WaitFor; timelimit: INTEGER := -1):
  INTEGER RAISES {TrestleComm.Failure};
<* LL.sup = trsl *>
(* convenience routine, calls RegisterWaiter, then WaitWaiter *)

PROCEDURE FindWaiter (trsl: XClient.T; READONLY ev: X.XEvent): WaitFor;
<* LL.sup = trsl *>
(* Find a waiter which matches "ev" *)

PROCEDURE RegisterWaiter (trsl: T_Abs; wf: WaitFor);
<* LL.sup = trsl *>
(* register /wf/ with /trsl/ to be notified when the the match method
   accepts an X event. *)

PROCEDURE WaitWaiter(trsl: T_Abs; wf: WaitFor; timelimit: INTEGER := -1):
  INTEGER RAISES {TrestleComm.Failure};
<* LL = trsl *>
(* Suspend execution of this thread until the timelimit expires, or until
   the WaitFor match method accepts an X event.  This routine releases trsl
   and regains it when the condition is met.  Returns the type of X
   event or Timeout.

   If /wf/ hasn't previously been registered with /trsl/, you might
   wait a very long time *)

(* ---------- various utilities ---------- *)
<* INLINE *> PROCEDURE ToRect (x, y, width, height: INTEGER): Rect.T;
(* utility to return a rectangle from X rectangle description *)

PROCEDURE NewAtom (v: XClient.T): X.Atom RAISES {TrestleComm.Failure};
PROCEDURE FreeAtom (v: XClient.T; VAR sym: X.Atom);

PROCEDURE BackDoor (v: XClient.T; READONLY ev: X.XEvent);
(* send an XEvent to the T *)

PROCEDURE SetUngrabs (trsl: XClient.T) RAISES {TrestleComm.Failure};

PROCEDURE ValidateNW (trsl: XClient.T; ch: Child; st: XScreenType.T)
  RAISES {TrestleComm.Failure};

PROCEDURE GetDomain (ur: Child; VAR (* OUT*) width, height: CARDINAL);
(* Return the domain of ur's X window, or 0,0 when the window is unmapped,
   and clear ur.reshapeComing.  LL = ur.ch.parent *)

PROCEDURE AdjustCoverage (xcon: XClient.T; d: [-1 .. 1] := 0)
  RAISES {TrestleComm.Failure};
(* see TrestleOnX.Enter() *)

PROCEDURE Delete (trsl: XClient.T; ch: VBT.T; ur: Child);

PROCEDURE Reshape (ch: VBT.T; width, height: CARDINAL; sendMoved := FALSE);
(* Reshape ch to new width and height.  LL = VBT.mu *)

PROCEDURE Connect (inst: TEXT; t: XClient.T := NIL): Trestle.T
  RAISES {TrestleComm.Failure};
(* If t is NIL, allocate a new T and return it.  In any case, connect t to
   the X server named inst. *)

(* ---------- connection management ---------- *)
PROCEDURE DoConnect (             self     : TrestleClass.ConnectClosure;
                                  inst     : TEXT;
                                  localOnly: BOOLEAN;
                     VAR (* OUT*) t        : Trestle.T                    ):
  BOOLEAN;
(* Apply procedure for TrestleClass.ConnectClosure.  Establishes connection
   with X server *)

END XClientF.

