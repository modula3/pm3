(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 14 19:51:11 PST 1994 by msm      *)
(*      modified on Tue Mar 10 19:05:45 1992 by steveg   *)
(*      modified on Mon Feb 24 13:57:17 PST 1992 by muller   *)

(*      modified on Tue Oct 22 20:16:39 PDT 1991 by gnelson  *)


<*PRAGMA LL*>

MODULE MouseSplit;

IMPORT VBT, VBTClass, ScrnCursor, Rect, VBTRep;

REVEAL MouseRef = BRANDED REF RECORD
   (* All fields protected by VBT.mu; mouseFocus, current, cache, and
      tracking are also protected by the parent *)
    mouseFocus: VBT.T := NIL;
    current: VBT.T := NIL;
    (* the child containing the last reported position of the
       cursor, or NIL if this position was not over any child. *)
    cache: VBT.Cage := VBT.GoneCage;
    (* If meth is the mouseRef of the VBT c and meth.cache.inOut =
       {FALSE} then for all points p in meth.cache.rect, Locate(c, p,
       ...)  returns meth.current.  Otherwise meth.cache.inOut = {TRUE},
       and the last position received by the parent was gone.  In any
       case, if meth.cache is non-empty then it contains the last
       position received by the parent.  *)
    tracking: BOOLEAN := FALSE;
    (* TRUE if some child other than current or the mouseFocus
       has a cage that does not contain GoneCage. *)
    link: MouseRef := NIL;
    (* For the free list *)
  END;

(* If v.mouseRef=NIL, then mouseFocus, and current are NIL, and cache 
    is VBT.GoneCage. *)

VAR
  mu              := NEW(MUTEX);
  avail: MouseRef := NIL;       (* The free-list; protected by mu. *)

(* Invariants:

|   (Q1) v.mouseFocus # NIL => v.effectiveCursor = v.mouseFocus.getcursor()
|
|   (Q2) v.mouseFocus = NIL AND v.current # NIL => v.effectiveCursor =
|        v.current.getcursor()
|
|   (Q3) v.mouseFocus = NIL AND v.current = NIL AND
|        last delivered position isn't gone => 
|        v..effectiveCursor = ScrnCursor.DontCare

   *)

PROCEDURE Getcursor (v: VBT.Split): ScrnCursor.T RAISES {} =
  BEGIN                         (* LL=v *)
    IF v.effectiveCursor = NIL THEN
      v.effectiveCursor := ScrnCursor.DontCare
    END;
    IF v.effectiveCursor = ScrnCursor.DontCare THEN
      RETURN VBT.Leaf.getcursor(v)
    ELSE
      RETURN v.effectiveCursor
    END
  END Getcursor;

PROCEDURE Setcursor(v: VBT.Split; ch: VBT.T) RAISES {} =
  VAR cs: ScrnCursor.T; BEGIN (* LL=ch *)
    LOCK v DO
      WITH r = v.mouseRef DO
        IF r = NIL OR
           ch # r.mouseFocus AND
            (ch # r.current OR r.mouseFocus # NIL) 
        THEN
          RETURN
        END
      END
    END;
    cs := ch.getcursor();
    LOCK v DO
      WITH r = v.mouseRef DO
        IF r # NIL AND 
           (ch = r.mouseFocus OR 
            ch = r.current AND r.mouseFocus = NIL) 
        THEN
          SetCursor2(v, cs)
        END
      END
    END
  END Setcursor;

<*INLINE*> PROCEDURE SetCursor2(v: VBT.Split; cs: ScrnCursor.T) RAISES {} =
  BEGIN (* LL=v *)
    IF cs # v.effectiveCursor THEN
      v.effectiveCursor := cs; 
      IF v.parent # NIL THEN v.parent.setcursor(v) END
    END
  END SetCursor2;

<*INLINE*> PROCEDURE SetCursor3(v: VBT.Split; ch: VBT.T) RAISES {} =
  BEGIN
    IF ch # NIL THEN
      LOCK ch DO
        VAR cs := ch.getcursor(); BEGIN
          LOCK v DO SetCursor2(v, cs) END
        END
      END
    ELSE 
      LOCK v DO SetCursor2(v, ScrnCursor.DontCare) END
    END
  END SetCursor3;

(* Cage setting depends on the following invariants:

   (R1) v's cage is contained in the intersection of its children's 
        cages.  This guarantees that v will get a position whenever 
        any child is owed one.

   (R2) v's cage is contained in v.cache.  This guarantees that v will 
        get a position whenever "current" should be changed.

   (R3) v.tracking OR for each ch # v.mouseFocus AND ch # v.current,
        ch.cage contains GoneCage.  

    When the parent receives a position its cage is set arbitrarily,
    so the invariants are destroyed.  It sets its cage to satisfy R2,
    and then delivers positions to its children.  The SetCages which
    they do in response to the positions reestablish R1 and R3 before
    the parent Position returns.  *)

PROCEDURE Setcage(v: VBT.Split; ch: VBT.T) RAISES {} =
  VAR cg := VBTClass.Cage(ch);
  BEGIN (*LL=ch*)
    LOCK v DO
      WITH r = v.mouseRef, notCurrent = (r = NIL) OR (ch # r.current) DO
        IF NOT (notCurrent IN cg.inOut) THEN cg := VBT.EmptyCage END;
        cg.inOut := VBT.InOut{FALSE, TRUE};
        IF notCurrent AND (r = NIL OR ch # r.mouseFocus) AND 
          ((NOT Rect.Equal(cg.rect, Rect.Full)) OR (cg.screen # VBT.AllScreens)) 
        THEN
           CreateMouseRef(r);
           r.tracking := TRUE
        END;
        IF r # NIL THEN VBTClass.SetCage(v, cg) END
      END
    END
  END Setcage;

PROCEDURE Position(v: VBT.Split; READONLY cd: VBT.PositionRec)
  RAISES {} =
  VAR
    current, mouseFocus, newCurrent: VBT.T := NIL;
    goneCd := cd;
    changed := TRUE;
    newCache: VBT.Cage;
    tracking := FALSE;
  BEGIN (* LL = VBT.mu *)
    goneCd.cp.gone := TRUE;
    WITH r = v.mouseRef DO
      IF r # NIL THEN
        current := r.current;
        mouseFocus := r.mouseFocus;
        tracking := r.tracking
      END;
      IF cd.cp.gone THEN
        changed := (current # NIL) OR (r # NIL) AND (FALSE IN r.cache.inOut);
        newCurrent := NIL;
        newCache := VBT.GoneCage;
        VBT.SetCage(v, newCache)
      ELSIF (r # NIL) AND NOT VBT.Outside(cd.cp, r.cache) THEN
        changed := FALSE;
        newCurrent := current;
        VBT.SetCage(v, r.cache)
      ELSE
        newCurrent := v.locate(cd.cp.pt, newCache.rect);
        IF newCurrent # NIL THEN
          newCache.rect := Rect.Meet(newCache.rect, newCurrent.domain)
        ELSE
          newCache.rect := Rect.Meet(newCache.rect, v.domain)
        END;
        newCache.inOut := VBT.InOut{FALSE};
        newCache.screen := cd.cp.screen;
        VBT.SetCage(v, newCache)
      END;
      IF changed OR tracking THEN
        LOCK v DO
          CreateMouseRef(r);
          r.current := newCurrent;
          r.cache := newCache;
          r.tracking := FALSE;
          CheckMouseRef(r)
        END
      END
    END;
    IF current # newCurrent THEN
      (* possibly deliver "gone" to old current;
         possibly change cursors. *)
      IF current # NIL AND current # mouseFocus THEN
        VBTClass.Position(current, goneCd)
      END;
      IF mouseFocus = NIL AND NOT cd.cp.gone THEN SetCursor3(v, newCurrent) END
    ELSIF newCurrent = NIL AND NOT cd.cp.gone THEN
      SetCursor3(v, NIL)
    END;
    IF mouseFocus # NIL AND mouseFocus # newCurrent THEN
      VBTClass.Position(mouseFocus, goneCd)
    END;
    IF tracking THEN
      VAR ch := v.succ(NIL); BEGIN
        WHILE ch # NIL DO
          IF ch # mouseFocus AND ch # current AND ch # newCurrent THEN
            VBTClass.Position(ch, goneCd)
          END;
          ch := v.succ(ch)
        END
      END
    END;
    IF newCurrent # NIL THEN
      VBTClass.Position(newCurrent, cd)
    END
  END Position;

PROCEDURE BecomeMF(v: VBT.Split; mf: VBT.T) =
  BEGIN
    LOCK v DO
      IF mf # NIL THEN CreateMouseRef(v.mouseRef) END;
      IF v.mouseRef # NIL THEN v.mouseRef.mouseFocus := mf END;
      IF mf = NIL THEN CheckMouseRef(v.mouseRef) END
    END;
    IF mf # NIL THEN
      SetCursor3(v, mf)
    ELSIF v.mouseRef # NIL THEN
      SetCursor3(v, v.mouseRef.current)
    ELSE
      SetCursor3(v, NIL)
    END
  END BecomeMF;
  


PROCEDURE Mouse(v: VBT.Split; READONLY cd: VBT.MouseRec) RAISES {} =
  VAR
    ch: VBT.T;
    junk: Rect.T;
    goneCd: VBT.MouseRec;
    (*r := v.mouseRef;*)

  BEGIN
    (* Set ch to the child containing the position of cd. *)
    WITH r = v.mouseRef DO
    IF cd.cp.gone THEN
      ch := NIL
    ELSIF r # NIL AND (FALSE IN r.cache.inOut) AND
            Rect.Member(cd.cp.pt, r.cache.rect) THEN
      ch := r.current
    ELSE
      ch := v.locate(cd.cp.pt, junk)
    END;
    (* Deliver the mouse code. *)
    IF ch # NIL THEN VBTClass.Mouse(ch, cd) END;
    (* Possibly deliver cd to the mouseFocus *)
    IF r # NIL AND r.mouseFocus # NIL
       AND r.mouseFocus # ch THEN
      goneCd := cd;
      goneCd.cp.gone := TRUE;
      VBTClass.Mouse(r.mouseFocus, goneCd)
    END
    END;
    (* reset the mouseFocus *)
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      BecomeMF(v, ch)
    ELSIF cd.clickType = VBT.ClickType.LastUp THEN
      BecomeMF(v, NIL)
    END
  END Mouse;

PROCEDURE InvalidateCache(v: VBT.Split) =
  BEGIN
    LOCK v DO
      WITH r = v.mouseRef DO
        IF r # NIL AND (FALSE IN r.cache.inOut) THEN
           r.cache.rect := Rect.Empty;
           VBTClass.SetCage(v, r.cache)
        END
      END
    END
  END InvalidateCache;

<*INLINE*> PROCEDURE CheckMouseRef(VAR r: MouseRef) =
  BEGIN
    IF r # NIL AND r.mouseFocus = NIL AND r.current = NIL 
       AND (TRUE IN r.cache.inOut) AND NOT r.tracking
    THEN
      LOCK mu DO r.link := avail; avail := r END;
      r := NIL
    END
  END CheckMouseRef;

<*INLINE*> PROCEDURE CreateMouseRef(VAR r: MouseRef) =
  BEGIN
    IF r = NIL THEN
      LOCK mu DO
        IF avail # NIL THEN
          r := avail;
          avail := avail.link
        ELSE
          r := NEW(MouseRef);
        END
      END
    END
  END CreateMouseRef;

BEGIN
END MouseSplit.          
          


