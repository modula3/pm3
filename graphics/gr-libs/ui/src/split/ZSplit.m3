(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* ZSplit.m3, coded Fri Oct 31 11:24:53 1986 by cgn *)
<*PRAGMA LL*>

(* Last modified on Mon Jan  8 14:17:08 PST 1996 by heydon  *)
(*      modified on Tue Jan 31 09:47:59 PST 1995 by kalsow  *)
(*      modified on Fri Jul  8 17:10:24 PDT 1994 by msm     *)
(*      modified on Mon Feb 24 13:55:29 PST 1992 by muller  *)
(*      modified on Sun Nov 10 18:14:50 PST 1991 by gnelson *)
(*      modified on Fri Feb  2 14:08:01 PST 1990 by glassman *)

UNSAFE MODULE ZSplit;

(* Unsafe when it traverses paint batches. *)

IMPORT VBT, Rect, Split, ProperSplit, Point, PolyRegion, PaintPrivate,
Region, Batch, BatchUtil, BatchRep, Axis, ScrnPixmap, Interval,
VBTTuning, VBTClass, Word, VBTRep;

FROM PaintPrivate IMPORT PaintCommand;

REVEAL
  Private = ProperSplit.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT
    (* Protection level VBT.mu *)
    saveBits: BOOLEAN;
    parlim: INTEGER;
    oldDom: REF Rect.T := NIL;
    (* Last non-empty parent domain when parent is empty;
       otherwise NIL.  Also NIL if the parent has never
       had a non-empty domain. *)
    affected: PolyRegion.T := PolyRegion.Empty;
    (* If non-nil, contains every pixel whose owning child may have
       changed since the last redisplay. NIL represents the
       empty region. *)
  OVERRIDES
    beChild := BeChild;
    replace := Replace;
    insert := SplitInsert;
    move := SplitMove;
    paintbatch := PaintBatch;
    capture := Capture;
    newShape := NewShape;
    reshape := Reshape;
    repaint := Repaint;
    rescreen := Rescreen;
    shape := Shape;
    redisplay := Redisplay;
    axisOrder := AxisOrder;
    init := Be
  END;

TYPE 
  Child = ProperSplit.Child OBJECT
    (* Protection level VBT.mu *)
    shapeChanged, mapped := FALSE;
    (* The mapped bit is set if the child is mapped. *)
    (* zc.upRef.shapeChanged = TRUE implies that zc's newshape method
       has been called and therefore its shape method will be called
       in order to possibly change the dimensions of the child.  *)
    dom: Dom := NIL;
    (* If zc.upRef.dom is non-NIL, then zc.upRef.dom.r is the 
       rectangle to which zc will be reshaped the next time
       zc.parent is redisplayed non-empty and zc is mapped. Also, 
       zc.upRef.dom.checked is set if the domain has been
       clipped into zc's shape range. *)
    reshapeControl: ReshapeControl := NIL;
    (* Protection level VBT.mu + ch *)
    clip: Clip := NIL;
    (* If clip = NIL, this child is unobscured; otherwise
        clip.rgn is the child's visible region, and
        clip.cache is a subset of clip.rgn. *)
  END;
  Clip = REF RECORD cache: Rect.T := Rect.Empty; rgn: Region.T END;
  Dom = REF RECORD r: Rect.T; checked, replacement := FALSE END;

VAR (*CONST*) EmptyClip := NEW(Clip, rgn := Region.Empty);

PROCEDURE Be(
    p: T;
    bg: VBT.T := NIL;
    saveBits := FALSE;
    parlim: INTEGER := -1): T =
(* p becomes the parent of a ZSplit containing the initial background
   child bg, which is mapped, or no children if bg=NIL.  The value of
   parlim is the minimum area of a child for which a separate thread
   will be forked to reformat or repaint it; if it is -1, it is set
   to an appropriate default (see the VBTTuning interface).  LL =
   VBT.mu; or LL <= VBT.mu if v is virginal.  *)
   BEGIN
     IF parlim = -1 THEN 
       p.parlim := zParlim; 
     ELSE
       p.parlim := parlim
     END;
     p.saveBits := saveBits;
     IF bg # NIL THEN 
       Insert(p, bg, p.domain);
       SetReshapeControl(bg, Background)
     END;
     RETURN p
   END Be;

VAR zParlim := VBTTuning.ZParlim;

PROCEDURE New(
    bg: VBT.T := NIL;
    saveBits := FALSE;
    parlim: INTEGER := -1): T = 
  BEGIN
    RETURN Be(NEW(T), bg, saveBits, parlim)
  END New;

PROCEDURE BeChild(v: T; ch: VBT.T) RAISES {} =
VAR  ur: Child;
  BEGIN
    IF ch.upRef = NIL THEN 
      ur := NEW(Child); 
      ch.upRef := ur
    ELSE
      ur := ch.upRef 
    END;
    ProperSplit.T.beChild(v, ch);
    VBTClass.ClearShortCircuit(ch);
    ur.reshapeControl := WNChains;
  END BeChild;

PROCEDURE NewShape(v: T; ch: VBT.T) RAISES {} =
  BEGIN
    WITH ur = NARROW(ch.upRef, Child) DO
      ur.shapeChanged := TRUE
    END;
    VBT.Mark(v);
    IF v.succ(ch) = NIL THEN VBT.NewShape(v) END
  END NewShape;

PROCEDURE Shape(v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange RAISES {} =
<*FATAL Split.NotAChild*>
  BEGIN
    WITH bg = Split.Pred(v, NIL) DO
      IF bg = NIL THEN 
        RETURN VBT.DefaultShape
      ELSE
        RETURN VBTClass.GetShape(bg, ax, n, FALSE)
      END
    END
  END Shape;

PROCEDURE AxisOrder(v: T): Axis.T = 
<*FATAL Split.NotAChild*>
  BEGIN
    WITH bg = Split.Pred(v, NIL) DO
      IF bg = NIL THEN 
        (* RETURN ProperSplit.T.axisOrder(v) *)
        RETURN VBTRep.AxisOrderDefault(v)
      ELSE
        RETURN bg.axisOrder()
      END
    END
  END AxisOrder;

PROCEDURE Repaint(v: T; READONLY rg: Region.T) RAISES {} =
  VAR ch := v.succ(NIL); rgn := rg;
  BEGIN
    WHILE (ch # NIL) AND NOT Region.IsEmpty(rgn) DO
      IF Region.OverlapRect(ch.domain, rgn) THEN
        VBTClass.Repaint(ch, Region.MeetRect(ch.domain, rgn));
        rgn := Region.Difference(rgn, Region.FromRect(ch.domain))
      END;
      ch := v.succ(ch)
    END
  END Repaint;

<*INLINE*> PROCEDURE RememberDomain(ch: VBT.T; ur: Child) =
  (* ch.upRef = ur *)
  BEGIN
    IF ur.dom = NIL THEN
      ur.dom := NEW(Dom, r := ch.domain, checked := TRUE)
    END
  END RememberDomain;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  VAR ch: VBT.T; prev, old: Rect.T;
  BEGIN
    IF Rect.IsEmpty(cd.new) THEN
      v.oldDom := NEW(REF Rect.T);
      v.oldDom^ := cd.prev
    ELSIF v.oldDom = NIL THEN 
      old := cd.prev 
    ELSE
      old := v.oldDom^;
      v.oldDom := NIL
    END;
    IF NOT Rect.IsEmpty(cd.new) AND NOT Rect.Equal(cd.new, old) THEN
      ch := v.succ(NIL);
      WHILE ch # NIL DO
        WITH ur = NARROW(ch.upRef, Child) DO
          RememberDomain(ch, ur);
          prev := ur.dom.r;
          ur.dom.r := ur.reshapeControl.apply(ch, old, cd.new, prev);
          IF ur.shapeChanged THEN
            ur.dom.checked := FALSE;
            ur.shapeChanged := FALSE;
            VBTClass.ClearNewShape(ch)
          ELSIF ur.dom.checked THEN 
            ur.dom.checked := Congruent(prev, ur.dom.r)
          END 
        END;
        ch := v.succ(ch)
      END
    END;
    IF Congruent(cd.new, cd.prev) THEN
      Redisplay2(v, TRUE, TRUE, cd.saved,
        Point.Sub(Rect.NorthWest(cd.new), Rect.NorthWest(cd.prev)))
    ELSE
      Redisplay2(v, TRUE, FALSE, cd.saved, Point.Origin)
    END
  END Reshape;

PROCEDURE Rescreen(v: T; READONLY cd: VBT.RescreenRec) RAISES {} =
  VAR ch: VBT.T;
  BEGIN
    IF v.oldDom = NIL THEN
      v.oldDom := NEW(REF Rect.T);
      v.oldDom^ := cd.prev
    END;
    ch := v.succ(NIL);
    WHILE ch # NIL DO
      RememberDomain(ch, ch.upRef);
      ch := v.succ(ch)
    END;
    VBT.Split.rescreen(v, cd);
    Redisplay2(v, TRUE, FALSE, Rect.Empty, Point.Origin)
  END Rescreen;

PROCEDURE Redisplay(v: T) RAISES {} =
  BEGIN Redisplay2(v, FALSE, FALSE, v.domain, Point.Origin) END Redisplay;
  
 TYPE
   ChildRec = RECORD ch: VBT.T; ur: Child; clip: Clip; winner: BOOLEAN END;
 
PROCEDURE Redisplay2(v: T; inReshape, translation: BOOLEAN; READONLY
saved: Rect.T; READONLY delta: Point.T)
  RAISES {} =
  VAR ch := v.succ(NIL); numch := 0;
    a1: ARRAY [0..9] OF ChildRec;
    a2: REF ARRAY OF ChildRec;
    replacement := FALSE;
  BEGIN
    VBTClass.LocateChanged(v);
    IF Rect.IsEmpty(v.domain) THEN
      WHILE ch # NIL DO
        WITH ur = NARROW(ch.upRef, Child) DO
          RememberDomain(ch, ur);
          IF ur.clip # NIL THEN LOCK ch DO ur.clip := NIL END END
        END; 
        IF NOT Rect.IsEmpty(ch.domain) THEN
          VBTClass.Reshape(ch, Rect.Empty, Rect.Empty)
        END;
        ch := v.succ(ch)
      END;
      v.affected := PolyRegion.Empty;
      RETURN
    END;
    translation := translation AND Rect.IsEmpty(v.affected.r);
    (* Check domains, expand affected, and blow away unmapped windows *)
    WHILE ch # NIL DO
      WITH ur = NARROW(ch.upRef, Child) DO
        IF NOT ur.mapped THEN
          IF NOT Rect.IsEmpty(ch.domain) THEN
            translation := FALSE;
            RememberDomain(ch, ur);
            VAR oldDom := ch.domain; BEGIN
              VBTClass.Reshape(ch, Rect.Empty, Rect.Empty);
              IF NOT inReshape THEN
                IF ur.clip # NIL THEN
                  PolyRegion.JoinRgn(v.affected, ur.clip.rgn);
                  LOCK ch DO ur.clip := NIL END
                ELSE
                  PolyRegion.JoinRect(v.affected, oldDom)
                END
              END
            END
          END
        ELSE
          IF (ur.dom # NIL) OR ur.shapeChanged THEN
            Move2(ch, ur, GetDomain(ch));
            ur.shapeChanged := FALSE;
          END;
          IF inReshape THEN
            IF translation THEN
              IF ur.dom = NIL THEN
                translation := Point.Equal(delta, Point.Origin)
              ELSE
                translation := Rect.Equal(Rect.Add(ch.domain, delta),
                 ur.dom.r)
              END
            END
          ELSIF (ur.dom # NIL) THEN
            IF ur.dom.replacement THEN
              replacement := TRUE
            ELSE
              PolyRegion.JoinRgn(v.affected,
                Region.SymmetricDifference(
                  Region.FromRect(ur.dom.r), Region.FromRect(ch.domain)))
            END
          END;
          IF (ur.dom # NIL) AND Rect.IsEmpty(ur.dom.r) THEN
            ur.dom := NIL;
            VBTClass.Reshape(ch, Rect.Empty, Rect.Empty);
            IF ur.clip # NIL THEN LOCK ch DO ur.clip := NIL END END
          ELSE
            INC(numch)
          END
        END
      END;
      ch := v.succ(ch)
    END;
    IF inReshape OR replacement OR NOT Rect.IsEmpty(v.affected.r) THEN 
      IF numch <= NUMBER(a1) THEN
        Redisplay3(v, a1, inReshape, translation, saved, delta)
      ELSE
        a2 := NEW(REF ARRAY OF ChildRec, numch);
        Redisplay3(v, a2^, inReshape, translation, saved, delta)
      END
    END
  END Redisplay2;

PROCEDURE ComputeClip(
  READONLY affected: Region.T; 
  VAR covered: PolyRegion.T;
  READONLY dom, pdom: Rect.T; 
  inReshape: BOOLEAN;
  oclip: Clip): Clip = 
  VAR cl, oc: Region.T; obs := PolyRegion.OverlapRect(covered, dom);
  BEGIN
    IF NOT obs AND Rect.Subset(dom, pdom) AND 
      ((oclip = NIL) OR inReshape OR Region.SubsetRect(dom, affected))
    THEN
      PolyRegion.JoinRect(covered, dom);
      RETURN NIL
    ELSE
      WITH ndom = Rect.Meet(dom, pdom) DO
        IF inReshape THEN
          cl := PolyRegion.Complement(covered, Region.FromRect(ndom))
        ELSE
          WITH af = Region.MeetRect(ndom, affected) DO
            IF obs THEN
              cl := PolyRegion.Complement(covered, af)
            ELSE
              cl := af
            END;
            IF NOT RegionEqRect(ndom, af) THEN
              IF oclip = NIL THEN
                oc := Region.FromRect(ndom)
              ELSE
                oc := Region.MeetRect(ndom, oclip.rgn)
              END;
              cl := Region.Join(cl, Region.Difference(oc, af))
            END
          END
        END;
        PolyRegion.JoinRect(covered, ndom)
      END;
      IF RegionEqRect(dom, cl) THEN
        RETURN NIL
      ELSIF Region.IsEmpty(cl) THEN
        RETURN EmptyClip
      ELSIF (oclip # NIL) AND Region.Equal(oclip.rgn, cl) THEN
        RETURN oclip
      ELSE
        RETURN NEW(Clip, rgn := cl)
      END
    END
  END ComputeClip;

<*INLINE*> PROCEDURE RegionEqRect(
    READONLY rect: Rect.T; 
    READONLY rgn: Region.T): BOOLEAN =
  BEGIN
    RETURN (rgn.p = NIL) AND Rect.Equal(rect, rgn.r)
  END RegionEqRect;
              
PROCEDURE ApplyClip(
  v: T; 
  VAR el: ChildRec; 
  READONLY dom: Rect.T;
  inReshape: BOOLEAN; 
  READONLY saved: Rect.T; 
  VAR secure:PolyRegion.T) =
  VAR nc: Clip;
  BEGIN
    WITH ur = el.ur DO
      IF ur.dom = NIL THEN
        (* set ch's clip to be meet of old and new clip, to
           prevent it from painting on windows that we are
           going to reshape. *)
        IF (el.clip = NIL) OR (ur.clip = EmptyClip) 
           OR (ur.clip = el.clip) THEN
          nc := ur.clip
        ELSIF (ur.clip = NIL) OR (el.clip = EmptyClip) THEN
          nc := el.clip
        ELSE
          nc := NEW(Clip, rgn := Region.Meet(el.clip.rgn, ur.clip.rgn))
        END;
        IF inReshape AND (nc = NIL) AND NOT Rect.Subset(dom, saved) THEN
          nc := NEW(Clip, rgn := Region.FromRect(Rect.Meet(dom, saved)))
        ELSIF inReshape AND (nc # NIL) AND 
              NOT Rect.Subset(nc.rgn.r, saved) THEN
          nc := NEW(Clip, rgn := Region.MeetRect(saved, nc.rgn))
        END;
        el.winner := FALSE  
      ELSIF v.saveBits AND (el.clip = NIL) AND (ur.clip = NIL)
        AND NOT Rect.IsEmpty(el.ch.domain) AND
        NOT PolyRegion.OverlapRect(secure, el.ch.domain) THEN
        el.winner := TRUE;
        PolyRegion.JoinRect(secure, dom);
        nc := NIL
      ELSE
        el.winner := FALSE;
        nc := EmptyClip;
      END;
      (* ch.clip := nc *)
      IF ur.clip # nc THEN
        LOCK el.ch DO 
          ur.clip := nc;
          VBTClass.ClearShortCircuit(el.ch)
        END
      END
    END
  END ApplyClip;
              
PROCEDURE Redisplay3(v: T; VAR a: ARRAY OF ChildRec; inReshape, translation:
  BOOLEAN; READONLY saved: Rect.T; READONLY delta: Point.T) =
  VAR ch := v.succ(NIL);
    covered := PolyRegion.Empty;
    secure := PolyRegion.Empty;
    affected, br: Region.T; nch := 0;
  BEGIN
    IF NOT inReshape THEN
      affected := PolyRegion.ToRegion(v.affected)
    END;
    v.affected := PolyRegion.Empty;
    (* Compute new regions. Find movers that don't get old domain, and throttle 
       them; also restrict painting on windows that get more obscured. *)
    WHILE ch # NIL DO
      WITH ur = NARROW(ch.upRef, Child), nd = Domain(ch, ur) DO
        IF ur.mapped AND (inReshape OR (ur.dom # NIL) OR 
            Region.OverlapRect(nd, affected))
        THEN
          WITH el = a[nch] DO
            IF translation THEN
              IF (ur.clip = NIL) OR (ur.clip = EmptyClip) OR
                  Point.Equal(delta, Point.Origin) THEN
                el.clip := ur.clip
              ELSE
                el.clip := NEW(Clip, rgn := Region.Add(ur.clip.rgn, delta),
                  cache := Rect.Add(ur.clip.cache, delta))
              END
            ELSE
              el.clip := 
                ComputeClip(affected, covered, nd, v.domain, inReshape, ur.clip)
            END;
            IF (ur.dom # NIL) OR (el.clip # ur.clip) OR
               (inReshape AND NOT Rect.Subset(nd, saved)) THEN
              el.ch := ch;
              el.ur := ur;
              INC(nch);
              ApplyClip(v, el, nd, inReshape, saved, secure)
            END
          END
        END
      END;
      ch := v.succ(ch)
    END;
    (* Move the ones that get old domain *)
    IF v.saveBits THEN
      FOR i := 0 TO nch - 1 DO
        WITH el = a[i] DO
          IF el.winner THEN
            VBTClass.Reshape(el.ch, el.ur.dom.r, saved);
            el.ur.dom := NIL
          END
        END
      END
    END;
    (* Deliver badrects and move the rest of the children *)
    FOR i := 0 TO nch - 1 DO
      WITH el = a[i] DO
        IF NOT el.winner THEN
          IF (el.ur.dom = NIL) AND (el.ur.clip # el.clip) THEN
            IF el.clip = NIL THEN
              br := Region.Difference(
                Region.FromRect(el.ch.domain), el.ur.clip.rgn)
            ELSE
              br := Region.Difference(el.clip.rgn, el.ur.clip.rgn)
            END;
            LOCK el.ch DO
              el.ur.clip := el.clip;
              VBTClass.ForceRepaint(el.ch, br, FALSE)
            END;
            VBTClass.Repaint(el.ch, Region.Empty)
          ELSIF el.ur.dom # NIL THEN
            LOCK el.ch DO el.ur.clip := el.clip END;
            VBTClass.Reshape(el.ch, el.ur.dom.r, Rect.Empty);
            el.ur.dom := NIL
          END
        END
      END
    END
  END Redisplay3;

PROCEDURE GetParentDomain(v: T): Rect.T =
  BEGIN
    IF v.oldDom # NIL THEN RETURN v.oldDom^ ELSE RETURN v.domain END
  END GetParentDomain;
        
PROCEDURE GetDomain(ch: VBT.T): Rect.T =
  <*FATAL Split.NotAChild*>
  VAR lastChild := Split.Succ(ch.parent,ch) = NIL;
  BEGIN
    WITH ur = NARROW(ch.upRef, Child), r = Domain(ch, ur) DO
      IF ur.shapeChanged OR (ur.dom # NIL) AND NOT ur.dom.checked THEN
        WITH 
          s = VBTClass.GetShapes(ch, ur.shapeChanged),
          hor = s[Axis.T.Hor], ver = s[Axis.T.Ver]
        DO
          IF ur.shapeChanged AND NOT lastChild THEN
            RETURN Rect.FromCorner(Rect.NorthWest(r), hor.pref,
            ver.pref)
          ELSE
            WITH hsize= Rect.HorSize(r), vsize = Rect.VerSize(r),  
              width = MIN(hor.hi-1, MAX(hor.lo, hsize)),
              height = MIN(ver.hi-1, MAX(ver.lo, vsize))
            DO
              IF (width = hsize) AND (height = vsize) OR lastChild THEN
                IF ur.dom # NIL THEN ur.dom.checked := TRUE END;
                RETURN r
              END;
              RETURN Rect.FromCorner(Rect.NorthWest(r), width, height)
            END
          END
        END
      ELSE
        RETURN r
      END
    END
  END GetDomain;

<*INLINE*> PROCEDURE Domain(ch: VBT.T; ur: Child): Rect.T =
  (* ur = ch.upRef. LL = VBT.mu*)
  BEGIN
    IF ur.dom = NIL THEN RETURN ch.domain ELSE RETURN ur.dom.r END
  END Domain;

PROCEDURE Replace(v: T; ch, new: VBT.T) RAISES {} =
  VAR 
    chur := NARROW(ch.upRef, Child); 
    wasLast := v.succ(ch) = NIL;
  BEGIN
    IF new # NIL THEN
      VBTClass.ClearNewShape(new);
      LOCK new DO
        LOCK v DO
          ProperSplit.Insert(v, chur, new)
        END;
        WITH ur = NARROW(new.upRef, Child) DO
          ur.dom := NEW(Dom, r := Domain(ch, chur), checked := FALSE,
             replacement := TRUE);
          ur.mapped := chur.mapped;
          ur.clip := chur.clip
        END
      END
    ELSE
      IF chur.clip # NIL THEN
        PolyRegion.JoinRgn(v.affected, chur.clip.rgn)
      ELSE
        PolyRegion.JoinRect(v.affected, ch.domain)
      END
    END;
    IF wasLast AND (new = NIL OR VBTClass.GetShapes(ch, FALSE) #
         VBTClass.GetShapes(new, FALSE)) THEN 
      VBT.NewShape(v) 
    END;
    ProperSplit.Delete(v, chur)
  END Replace;

PROCEDURE InsertAfter(
    v: T;
    pred, ch: VBT.T;
    READONLY dom: Rect.T;
    alsoMap: BOOLEAN := TRUE) RAISES {Split.NotAChild} =
  VAR 
    predCh := ProperSplit.PreInsert(v, pred, ch); 
  BEGIN
    VBTClass.ClearNewShape(ch);
    LOCK ch DO 
      LOCK v DO 
        ProperSplit.Insert(v, predCh, ch);
        WITH ur = NARROW(ch.upRef, Child) DO
          ur.dom := NEW(Dom, r := dom, checked := FALSE);
          ur.mapped := FALSE
        END
      END
    END;
    IF alsoMap THEN Map(ch) END;
    IF v.succ(ch) = NIL THEN VBT.NewShape(v) END
  END InsertAfter;

PROCEDURE Insert(
    p: T;
    ch: VBT.T;
    READONLY dom: Rect.T;
    alt := Altitude.Top;
    alsoMap: BOOLEAN := TRUE) =
  <*FATAL Split.NotAChild*>
  VAR pred: VBT.T;
  BEGIN
    IF alt = Altitude.Top THEN
      pred := NIL
    ELSE
      pred := Split.Pred(p, Split.Pred(p, NIL))
    END;
    InsertAfter(p, pred, ch, dom, alsoMap)
  END Insert;

PROCEDURE InsertAt(
    p: T;
    ch: VBT.T;
    at: Point.T;
    alt := Altitude.Top;
    alsoMap: BOOLEAN := TRUE) =
  BEGIN
    VBTClass.Rescreen(ch, VBT.ScreenTypeOf(p));
    WITH s = VBTClass.GetShapes(ch), hor = s[Axis.T.Hor], ver = s[Axis.T.Ver] DO
      Insert(p, ch, Rect.FromCorner(at, hor.pref, ver.pref), alt, alsoMap);
      WITH ur = NARROW(ch.upRef, Child) DO
        ur.dom.checked := TRUE
      END
    END
  END InsertAt;

PROCEDURE SplitInsert(v: T; pred, ch: VBT.T) =
  <*FATAL Split.NotAChild*>
  BEGIN
    VBTClass.Rescreen(ch, VBT.ScreenTypeOf(v));
    WITH s = VBTClass.GetShapes(ch), hor = s[Axis.T.Hor], 
      ver = s[Axis.T.Ver] DO
      InsertAfter(v, pred, ch, Rect.FromCorner(Rect.NorthWest(v.domain), 
        hor.pref, ver.pref), FALSE);
      WITH ur = NARROW(ch.upRef, Child) DO
        ur.dom.checked := TRUE
      END
    END
  END SplitInsert;

PROCEDURE Unmap(ch: VBT.T) =
  VAR v: T := ch.parent; ur := NARROW(ch.upRef, Child); 
  BEGIN
    IF ur.mapped THEN
      ur.mapped := FALSE;
      VBT.Mark(v)
    END
  END Unmap;
      
PROCEDURE Map(ch: VBT.T) =
  VAR v: T := ch.parent; ur := NARROW(ch.upRef, Child);
  BEGIN
    IF NOT ur.mapped THEN
      ur.mapped := TRUE;
      IF ur.dom # NIL THEN ur.dom.replacement := FALSE END;
      VBT.Mark(v)
    END
  END Map;

PROCEDURE IsMapped(ch: VBT.T): BOOLEAN =
  VAR ur := NARROW(ch.upRef, Child);
  BEGIN
    RETURN ur.mapped
  END IsMapped;

PROCEDURE Move(ch: VBT.T; READONLY dom: Rect.T) =
  VAR ur := NARROW(ch.upRef, Child);
  BEGIN
    Move2(ch, ur, dom);
    IF ur.dom # NIL THEN ur.dom.checked := Congruent(dom, ch.domain) END;
    VBT.Mark(ch.parent)
  END Move;

PROCEDURE Move2(ch: VBT.T; ur: Child; READONLY dom: Rect.T) =
  BEGIN
    IF Rect.Equal(dom, ch.domain) THEN
      ur.dom := NIL
    ELSIF ur.dom = NIL THEN
      ur.dom := NEW(Dom, r := dom)
    ELSIF NOT Rect.Equal(ur.dom.r, dom) THEN
      ur.dom.r := dom;
      ur.dom.replacement := FALSE
    END
  END Move2;

<*INLINE*> PROCEDURE Congruent(READONLY r1, r2: Rect.T): BOOLEAN =
  BEGIN 
    RETURN 
      Rect.HorSize(r1) = Rect.HorSize(r2) AND
      Rect.VerSize(r1) = Rect.VerSize(r2) 
  END Congruent;  
    
PROCEDURE LiftAfter(pred, ch: VBT.T) =
  <*FATAL Split.NotAChild*>
  VAR predFirst: BOOLEAN; v: T := ch.parent; w: VBT.T; predUr: Child;
    newLast := (v.succ(ch) = NIL) OR (v.succ(pred) = NIL);
  BEGIN
    IF pred = NIL THEN
      predUr := NIL
    ELSE
      predUr := pred.upRef;
      IF pred.parent # v THEN Crash() END;
    END;
    WITH ur = NARROW(ch.upRef, Child) DO
      IF (pred = ch) OR (Split.Pred(v, ch) = pred) THEN RETURN END;
      IF ur.mapped THEN
        IF pred = NIL THEN 
          predFirst := TRUE
        ELSE
          w := NIL;
          LOOP
            w := Split.Pred(v, w);
            IF w = pred THEN
              predFirst := FALSE;
              EXIT
            ELSIF w = ch THEN
              predFirst := TRUE;
              EXIT
            END
          END
        END;
        IF predFirst THEN
          IF ur.clip # NIL THEN
            PolyRegion.JoinRgn(v.affected,
              Region.Difference(Region.FromRect(ch.domain), ur.clip.rgn))
          END
        ELSIF ur.clip = NIL THEN
          PolyRegion.JoinRect(v.affected, ch.domain)
        ELSE
          PolyRegion.JoinRgn(v.affected, ur.clip.rgn)
        END
      END;
      ProperSplit.Move(v, predUr, ur)
    END;
    IF newLast THEN VBT.NewShape(v) END
  END LiftAfter;

PROCEDURE SplitMove(<*UNUSED*> v: T; pred, ch: VBT.T) =
  BEGIN LiftAfter(pred, ch) END SplitMove;

PROCEDURE Lift(ch: VBT.T; alt := Altitude.Top) =
  <*FATAL Split.NotAChild*>
  VAR pred: VBT.T; v: T := ch.parent;
  BEGIN
    IF alt = Altitude.Top THEN
      pred := NIL
    ELSE
      pred := Split.Pred(v, Split.Pred(v, NIL))
    END;
    LiftAfter(pred, ch)
  END Lift;
   
PROCEDURE PaintBatch(v: T; ch: VBT.T; ba: Batch.T) RAISES {} =
  VAR src, cache: Rect.T; fp: BOOLEAN;
  BEGIN
    WITH ur = NARROW(ch.upRef, Child) DO
      IF ur.clip = NIL THEN
        VBTClass.SetShortCircuit(ch);
        VBTClass.PaintBatch(v, ba)
      ELSIF ur.clip = EmptyClip THEN
        Batch.Free(ba)
      ELSE
        fp := Rect.Subset(ba.clip, ur.clip.cache);
        IF NOT fp AND (ba.clipped # BatchUtil.ClipState.Tight) THEN
          BatchUtil.Tighten(ba);
          fp := Rect.Subset(ba.clip, ur.clip.cache)
        END;
        IF NOT Rect.IsEmpty(ba.scrollSource) THEN
          src := ba.scrollSource;
          IF fp THEN 
            fp := Rect.Subset(src, ur.clip.cache);
            IF NOT fp AND (ba.clipped # BatchUtil.ClipState.Tight) THEN
              BatchUtil.Tighten(ba);
              src := ba.scrollSource;
              fp := Rect.Subset(src, ur.clip.cache);
            END
          END;
          IF NOT fp THEN src := Rect.Join(ba.clip, src) END
        ELSIF NOT fp THEN
          src := ba.clip
        END;
        IF NOT fp THEN
          cache := Region.MaxSubset(src, ur.clip.rgn);
          IF NOT Rect.IsEmpty(cache) THEN 
            ur.clip.cache := cache; 
            fp := TRUE
          END
        END;
        IF fp THEN
          VBTClass.PaintBatch(v, ba)
        ELSIF ur.clip.rgn.p = NIL THEN
          PaintSimplyObscured(v, ch, ur.clip.rgn.r, ba)
        ELSE
          (* Batch is tight *)
          WITH rgn = Region.MeetRect(src, ur.clip.rgn) DO
            IF rgn.p = NIL THEN
              PaintSimplyObscured(v, ch, rgn.r, ba)
            ELSE
              PaintObscured(v, ch, ur.clip, ba)
            END
          END
        END
      END
    END
  END PaintBatch;

PROCEDURE PaintSimplyObscured(v: T; ch: VBT.T; READONLY vis: Rect.T; ba: Batch.T) =
  VAR
    cptr: PaintPrivate.CommandPtr;
    sptr: PaintPrivate.ScrollPtr;
    br := Region.Empty;
    st, end, len: INTEGER;
    src: Rect.T;
  BEGIN
    ba.clip := Rect.Meet(ba.clip, vis);
    IF Rect.IsEmpty(ba.clip) THEN Batch.Free(ba); RETURN END;
    ba.clipped := BatchUtil.ClipState.Unclipped;
    st := 0;
    end := (ba.next - ADR(ba.b[0])) DIV ADRSIZE(Word.T);
    WHILE st # end DO
      cptr := LOOPHOLE(ADR(ba.b[st]), PaintPrivate.CommandPtr);
      IF cptr.command <= LAST(PaintPrivate.FixedSzCommand) THEN
        len := PaintPrivate.ComSize[cptr.command]
      ELSE
        len := LOOPHOLE(cptr, PaintPrivate.VarSzPtr).szOfRec
      END;
      INC(st, len);
      IF cptr.command = PaintCommand.ScrollCom THEN
        sptr := LOOPHOLE(cptr, PaintPrivate.ScrollPtr);
        IF NOT Region.IsEmpty(br) THEN
          br := Region.Join(br, 
            Region.MeetRect(sptr.clip, Region.Add(br, sptr.delta)))
        END;
        src := Rect.Meet(sptr.clip, Rect.Add(vis, sptr.delta));
        IF Rect.IsEmpty(src) THEN
          br := Region.JoinRect(sptr.clip, br)
        ELSIF NOT Rect.Equal(src, sptr.clip) THEN
          br := Region.Join(br, Region.Difference(Region.FromRect(sptr.clip), 
            Region.FromRect(src)))
        END;
        sptr.clip := src
      END
    END;
    VBTClass.PaintBatch(v, ba);
    VBTClass.ForceRepaint(ch, Region.MeetRect(vis, br));
  END PaintSimplyObscured;
  
PROCEDURE PaintObscured(v: T; ch: VBT.T; vis: Clip; ba: Batch.T) =
  VAR
    cptr, iptr: PaintPrivate.CommandPtr;
    br := Region.Empty;
    st, end, len: INTEGER;
    rl := NEW(REF ARRAY OF Rect.T, 4);
    canRepeat: BOOLEAN;
  CONST
    Rsize = PaintPrivate.ComSize[PaintCommand.RepeatCom];
  BEGIN
    st := 0;
    end := (ba.next - ADR(ba.b[0])) DIV ADRSIZE(Word.T);
    LOCK v DO
      WHILE st # end DO
        cptr := LOOPHOLE(ADR(ba.b[st]), PaintPrivate.CommandPtr);
        IF cptr.command <= LAST(PaintPrivate.FixedSzCommand) THEN
          len := PaintPrivate.ComSize[cptr.command]
        ELSE
          len := LOOPHOLE(cptr, PaintPrivate.VarSzPtr).szOfRec
        END;
        INC(st, len);
        IF cptr.command = PaintCommand.ScrollCom THEN
          Scroll(v, vis, LOOPHOLE(cptr, PaintPrivate.ScrollPtr), br, rl);
        ELSE
          iptr := cptr;
          LOOP 
            PaintSingle(v, vis, iptr, cptr, rl, canRepeat);
            IF st = end THEN EXIT END;
            cptr := LOOPHOLE(ADR(ba.b[st]), PaintPrivate.CommandPtr);
            IF cptr.command # PaintCommand.RepeatCom THEN EXIT END;
            INC(st, Rsize)
          END
        END
      END;
      VBTRep.ForceBatch(v)
    END;
    VBTClass.ForceRepaint(ch, Region.Meet(vis.rgn, br));
    Batch.Free(ba)
  END PaintObscured;

PROCEDURE PaintSingle(
  v: T;
  vis: Clip;
  iptr, cptr: PaintPrivate.CommandPtr;
  VAR rl: REF ARRAY OF Rect.T;
  VAR (* OUT; IN if cptr points to a RepeatCom *) canRepeat: BOOLEAN) =
  VAR fp, repeat: BOOLEAN; cache: Rect.T; i, j, n: INTEGER;
  BEGIN
    canRepeat := (cptr.command = PaintCommand.RepeatCom) AND canRepeat;
    IF Rect.IsEmpty(cptr.clip) THEN RETURN END;
    fp := Rect.Subset(cptr.clip, vis.cache);
    IF NOT fp THEN
      cache := Region.MaxSubset(cptr.clip, vis.rgn);
      IF NOT Rect.IsEmpty(cache) THEN vis.cache := cache; fp := TRUE END
    END;
    IF fp THEN
      rl[0] := cptr.clip;
      n := 1;
    ELSE
      n := PolyRegion.Factor(vis.rgn, cptr.clip, Point.Origin, rl)
    END;
    repeat := canRepeat;
    i := 0;
    WHILE i # n DO
      IF repeat THEN
        j := MIN(VBTRep.MaxRepeat(v), n-i);
        IF j > 0 THEN
          VBTRep.PaintRepeat(v, SUBARRAY(rl^, i, j));
          INC(i, j)
        END;
        repeat := FALSE
      ELSE
        VBTRep.PaintSingle(v, rl[i], iptr);
        INC(i);
        repeat := TRUE;
        canRepeat := TRUE
      END
    END
  END PaintSingle;
  
PROCEDURE Scroll(
  v: T;
  vis: Clip;
  cptr: PaintPrivate.ScrollPtr;
  VAR br: Region.T;
  VAR rl: REF ARRAY OF Rect.T) =
  VAR fp, srcObs: BOOLEAN; cache, src: Rect.T; n: INTEGER;
    srcvis: Region.T;
  BEGIN
    IF NOT Region.IsEmpty(br) THEN
      br := Region.Join(br, Region.MeetRect(cptr.clip, Region.Add(br, cptr.delta)))
    END;
    IF Rect.IsEmpty(cptr.clip) THEN RETURN END;
    src := Rect.Sub(cptr.clip, cptr.delta);
    fp := Rect.Subset(src, vis.cache);
    IF NOT fp THEN
      cache := Region.MaxSubset(src, vis.rgn);
      IF NOT Rect.IsEmpty(cache) THEN vis.cache := cache; fp := TRUE END
    END;
    srcObs := NOT fp;
    IF fp THEN
      fp := Rect.Subset(cptr.clip, vis.cache);
      IF NOT fp THEN
        cache := Region.MaxSubset(cptr.clip, vis.rgn);
        IF NOT Rect.IsEmpty(cache) THEN vis.cache := cache; fp := TRUE END
      END
    END;
    IF fp THEN
      VBTRep.Scroll(v, cptr.clip, cptr);
    ELSE
      IF srcObs THEN
        srcvis := Region.Add(Region.MeetRect(src, vis.rgn), cptr.delta);
        n := PolyRegion.Factor(Region.Meet(vis.rgn, srcvis), 
             cptr.clip, cptr.delta, rl);
        br := Region.Join(br,
          Region.Difference(Region.FromRect(cptr.clip), srcvis))
      ELSE
        n := PolyRegion.Factor(vis.rgn, cptr.clip, cptr.delta, rl)
      END;
      FOR i := 0 TO n - 1 DO VBTRep.Scroll(v, rl[i], cptr) END
    END
  END Scroll;

PROCEDURE Capture(
  v: T; 
  ch: VBT.T; 
  READONLY rect: Rect.T; 
  VAR (*out*) br: Region.T): 
  ScrnPixmap.T RAISES {} =
  BEGIN
    WITH ur = NARROW(ch.upRef, Child) DO
      IF ur.clip = EmptyClip THEN
        br := Region.FromRect(rect);
        RETURN NIL
      END;
      WITH res = VBT.Capture(v, rect, br) DO
        IF ur.clip # NIL THEN 
          br := Region.Join(br, 
            Region.Difference(Region.FromRect(rect), ur.clip.rgn))
        END;
        RETURN res
      END
    END
  END Capture;

PROCEDURE SetReshapeControl(
    ch: VBT.T;
    rc: ReshapeControl) =
  BEGIN
    WITH ur = NARROW(ch.upRef, Child) DO
      ur.reshapeControl := rc
    END
  END SetReshapeControl;

PROCEDURE ChainedReshape(
    self: ChainReshapeControl;
    <*UNUSED*> ch: VBT.T;
    READONLY oldParentDomain, newParentDomain, oldChildDomain: Rect.T)
    : Rect.T =
  VAR dw, de, dn, ds: INTEGER;
  BEGIN
    IF self.chains = ChainSet{Ch.W, Ch.E, Ch.N, Ch.S} AND
      Rect.IsEmpty(oldChildDomain) THEN
      RETURN newParentDomain
    END;
    (* W - E chains *)
    WITH
      dlo = newParentDomain.west - oldParentDomain.west,
      dhi = newParentDomain.east - oldParentDomain.east
    DO
      IF Ch.W IN self.chains THEN
        dw := dlo;
        IF Ch.E IN self.chains THEN de := dhi ELSE de := dlo END
      ELSE
        IF Ch.E IN self.chains THEN de := dhi ELSE de := 0 END;
        dw := de
      END
    END;
    (* N - S chains *)
    WITH
      dlo = newParentDomain.north - oldParentDomain.north,
      dhi = newParentDomain.south - oldParentDomain.south
    DO
      IF Ch.N IN self.chains THEN
        dn := dlo;
        IF Ch.S IN self.chains THEN ds := dhi ELSE ds := dlo END
      ELSE
        IF Ch.S IN self.chains THEN ds := dhi ELSE ds := 0 END;
        dn := ds
      END
    END;
    RETURN Rect.Change(oldChildDomain, dw, de, dn, ds)
  END ChainedReshape;

<*INLINE*>
PROCEDURE Scale(num, den, lo, hi, idelta, odelta: INTEGER): Interval.T =
  (* Scale lo+delta and hi+delta by num/den, and return the resulting interval
     shifted by odelta *)
  BEGIN
    RETURN Interval.FromBounds(
      ((lo+idelta)*num + den DIV 2) DIV den + odelta,
      ((hi+idelta)*num + den DIV 2) DIV den + odelta)
  END Scale; 

PROCEDURE ScaledReshape(
    <*UNUSED*> self: ReshapeControl;
    <*UNUSED*> ch: VBT.T;
    READONLY op, np, oc: Rect.T)
    : Rect.T =
  BEGIN
    IF Rect.IsEmpty(op) THEN RETURN oc END;
    WITH hor = Scale(Rect.HorSize(np), 
           Rect.HorSize(op), oc.west, oc.east, -op.west, np.west),
         ver = Scale(Rect.VerSize(np), 
           Rect.VerSize(op), oc.north, oc.south, -op.north, np.north) DO
      RETURN Rect.FromIntervals(hor, ver)
    END
  END ScaledReshape;
  
PROCEDURE BackgroundReshape(
    <*UNUSED*> self: ReshapeControl;
    <*UNUSED*> ch: VBT.T;
    <*UNUSED*> READONLY op: Rect.T;
    READONLY np: Rect.T;
    <*UNUSED*> READONLY oc: Rect.T)
    : Rect.T =
  BEGIN
    RETURN np
  END BackgroundReshape;

EXCEPTION FatalError;

PROCEDURE Crash() =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;
  
BEGIN
  NoChains := NEW(ChainReshapeControl, chains := ChainSet{});
  WChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.W});
  EChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.E});
  WEChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.W, Ch.E});
  NChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.N});
  WNChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.W, Ch.N});
  ENChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.E, Ch.N});
  WENChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.W, Ch.E, Ch.N});
  SChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.S});
  WSChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.W, Ch.S});
  ESChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.E, Ch.S});
  WESChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.W, Ch.E, Ch.S});
  NSChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.N, Ch.S});
  WNSChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.W, Ch.N, Ch.S});
  ENSChains := NEW(ChainReshapeControl, chains := ChainSet{Ch.E, Ch.N, Ch.S});
  WENSChains := NEW(ChainReshapeControl,
      chains := ChainSet{Ch.W, Ch.E, Ch.N, Ch.S});
  Scaled := NEW(ReshapeControl, apply := ScaledReshape);
  Background := NEW(ReshapeControl, apply := BackgroundReshape);
END ZSplit.
