(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File HVSplit.m3 coded by MSM and CGN, Mon Nov 18 21:52:15 1985 *)
<*PRAGMA LL*>

(* Last modified on Wed Mar  8 18:16:48 PST 1995 by msm     *)
(*      modified on Mon Feb 24 13:53:28 PST 1992 by muller  *)
(*      modified on Mon Nov 11 15:39:54 PST 1991 by gnelson *)
(*      modified on Mon Oct  1 14:36:21 PDT 1990 by steveg *)


MODULE HVSplit;

IMPORT VBT, Split, Axis, Rect, Interval, ProperSplit, VBTClass,
  VBTTuning, Word;

TYPE SizeCache = 
  RECORD 
    lastQuery := -1;
    lastRes: VBT.SizeRange
  END;

REVEAL 
  Private = ProperSplit.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT
    hv: Axis.T;
    saveBits, adjustable: BOOLEAN;
    parlim: INTEGER;
    cache: ARRAY Axis.T OF SizeCache;
    numCh: CARDINAL := 0
  OVERRIDES
    reshape := Reshape;
    shape := Shape;
    redisplay := Redisplay;
    beChild := BeChild;
    replace := Replace;
    insert := Insert;
    newShape := NewShape;
    index := Index;
    axisOrder := AxisOrder;
    init := Be
  END;

TYPE 
  Child = ProperSplit.Child OBJECT size: INTEGER END;
  (* size < 0 iff the old size was Word.Not(size) and the size has
     changed.  *)

VAR hvParlim := VBTTuning.HVParlim;

PROCEDURE Be(
    v: T;
    hv: Axis.T;
    saveBits := FALSE;
    parlim := -1;
    adjustable := TRUE): T RAISES {} =
  BEGIN
    IF parlim = -1 THEN parlim := hvParlim END;
    v.saveBits := saveBits;
    v.adjustable := adjustable;
    v.parlim := parlim;
    v.hv := hv;
    RETURN v
  END Be;

PROCEDURE New(
    hv: Axis.T;
    saveBits := FALSE;
    parlim := -1;
    adjustable := TRUE)
    : T RAISES {} =
 BEGIN
   RETURN Be(NEW(T), hv, saveBits, parlim, adjustable)
 END New;

PROCEDURE Cons(
    hv: Axis.T;
    ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8, ch9: VBT.T := NIL;
    saveBits := FALSE;
    parlim := -1;
    adjustable := TRUE)
    : T RAISES {} =
  BEGIN
    RETURN
      ConsArray(hv, 
        ARRAY OF VBT.T{ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8, ch9}, 
        saveBits, parlim, adjustable);
  END Cons;

PROCEDURE ConsArray(
    hv: Axis.T;
    READONLY ch: ARRAY OF VBT.T;
    saveBits := FALSE;
    parlim := -1;
    adjustable := TRUE)
    : T RAISES {} =
  VAR result := New(hv, saveBits, parlim, adjustable);
  BEGIN
    Split.AddChildArray(result, ch);
    RETURN result
  END ConsArray;

PROCEDURE InvalidateCache(v: T) RAISES {} =
  BEGIN
    FOR ax := FIRST(Axis.T) TO LAST(Axis.T) DO
      v.cache[ax].lastQuery := -1
    END;
    VBT.NewShape(v)
  END InvalidateCache;
  
PROCEDURE Insert(v: T; pred, ch: VBT.T) =
  BEGIN
    Public.insert(v, pred, ch);
    WITH up = NARROW(ch.upRef, Child) DO up.size := FIRST(INTEGER) END;
    InvalidateCache(v)
  END Insert;

PROCEDURE BeChild(v: T; ch: VBT.T) RAISES {} =
  BEGIN
    IF ch.upRef = NIL THEN ch.upRef := NEW(Child) END;
    ProperSplit.T.beChild(v, ch);
    INC(v.numCh);
    VBTClass.SetShortCircuit(ch)
  END BeChild;

PROCEDURE Replace (v: T; pred, new: VBT.T) RAISES {} =
  VAR
    predCh := NARROW(pred.upRef, Child);
    predsh := VBTClass.GetShapes(pred, FALSE);
    size   := predCh.size;
  BEGIN
    Public.replace(v, pred, new);
    IF new # NIL THEN
      WITH up = NARROW(new.upRef, Child) DO
        IF size < 0 THEN up.size := size ELSE up.size := Word.Not(size) END
      END
    END;
    IF new = NIL OR VBTClass.GetShapes(new, FALSE) # predsh THEN
      VBT.NewShape(v)
    END;
    DEC(v.numCh);
    InvalidateCache(v)
  END Replace;

PROCEDURE Index(v: T; ch: VBT.T): CARDINAL RAISES {} =
  BEGIN
    IF ch = NIL THEN RETURN v.numCh ELSE RETURN ProperSplit.T.index(v, ch)
    END
  END Index;

PROCEDURE Adjust(v: T; ch: VBT.T; totsz: INTEGER) RAISES {Split.NotAChild} =
  VAR iv := FeasibleRange(v, ch); sz := 0; w := v.succ(NIL);
      delta, newsz: INTEGER; orthSize := Rect.Size(Axis.Other[v.hv], v.domain);
  BEGIN
    IF Interval.IsEmpty(iv) OR (ch = NIL) THEN RETURN END;
    totsz := Interval.Project(iv, totsz);
    LOOP
      WITH p = NARROW(w.upRef, Child) DO
        INC(sz, p.size)
      END;
      IF w = ch THEN EXIT END;
      w := v.succ(w)
    END;
    delta := totsz - sz;
    IF delta = 0 THEN RETURN END;
    WHILE delta # 0 DO
      WITH
        p = NARROW(w.upRef, Child),
        sh = VBTClass.GetShape(w, v.hv, orthSize, FALSE)
      DO
        newsz := MAX(sh.lo, MIN(sh.hi-1, p.size+delta));
        DEC(delta, newsz-p.size);
        p.size := newsz
      END;
      w := v.pred(w)
    END;
    delta := sz - totsz;
    w := v.succ(ch);
    WHILE delta # 0 DO
      WITH
        p = NARROW(w.upRef, Child),
        sh = VBTClass.GetShape(w, v.hv, orthSize, FALSE)
      DO
        newsz := MAX(sh.lo, MIN(sh.hi-1, p.size+delta));
        DEC(delta, newsz-p.size);
        p.size := newsz
      END;
      w := v.succ(w)
    END;
    VBT.Mark(v)
  END Adjust;

PROCEDURE ResetSize(
    VAR size: INTEGER; 
    sh: VBT.SizeRange; 
    adjustable: BOOLEAN) =
  BEGIN
    IF size < 0 THEN 
      WITH sz = Word.Not(size) DO
        IF adjustable AND sh.lo <= sz AND sz < sh.hi THEN
          size := sz
        ELSIF NOT adjustable OR size = FIRST(INTEGER) THEN
          size := sh.pref 
        ELSIF sz < sh.lo THEN
          size := sh.lo
        ELSE
          size := sh.hi - 1
        END
      END
    END
  END ResetSize;


(* If ch # NIL then on exit child.size >= 0 for all children of v. *)
PROCEDURE FeasibleRange(v: T; ch: VBT.T): Interval.T RAISES {Split.NotAChild} =
  VAR w := v.succ(NIL); premin, premax, postmin, postmax, totsz := 0;
      orthSize := Rect.Size(Axis.Other[v.hv], v.domain);
  BEGIN
    IF ch = NIL THEN RETURN Interval.FromSize(1) END;
    LOOP
      WITH
        p = NARROW(w.upRef, Child),
        sh = VBTClass.GetShape(w, v.hv, orthSize, p.size < 0)
      DO
        ResetSize(p.size, sh, v.adjustable);
        INC(totsz, p.size);
        INC(premin, sh.lo);
        INC(premax, sh.hi-1);
      END;
      IF w = ch THEN EXIT END;
      w := v.succ(w);
      IF w = NIL THEN RAISE Split.NotAChild END
    END;
    LOOP
      w := v.succ(w);
      IF w = NIL THEN EXIT END;
      WITH
        p = NARROW(w.upRef, Child),
        sh = VBTClass.GetShape(w, v.hv, orthSize, p.size < 0)
      DO
        ResetSize(p.size, sh, v.adjustable);
        INC(totsz, p.size);
        INC(postmin, sh.lo);
        INC(postmax, sh.hi-1);
      END;
    END;
    RETURN Interval.FromBounds(MAX(premin, totsz-postmax),
      1+MIN(premax, totsz-postmin))
  END FeasibleRange;

PROCEDURE AvailSize(v: T): CARDINAL RAISES {} =
  VAR w := v.succ(NIL); totsz, totmin := 0;
      orthSize := Rect.Size(Axis.Other[v.hv], v.domain);
  BEGIN
    WHILE w # NIL DO
      WITH
        p = NARROW(w.upRef, Child),
        sh = VBTClass.GetShape(w, v.hv, orthSize, p.size < 0)
      DO
        ResetSize(p.size, sh, v.adjustable);
        INC(totsz, p.size);
        INC(totmin, sh.lo)
      END;
      w := v.succ(w)
    END;
    RETURN MAX(0, totsz-totmin)
  END AvailSize;
  
PROCEDURE AxisOf(v: T): Axis.T RAISES {} =
  BEGIN
    IF v = NIL THEN RETURN Axis.T.Hor ELSE RETURN v.hv END
  END AxisOf;

(* on exit, child.size >= 0 for all children of v *)
PROCEDURE Scale(v: T) RAISES {} =
(* Project sizes of all children into the size range.  Use the TeX rule
   to attempt to make the sum of the sizes equal to the size of the parent.
   Guarantees that infeasible splits are canonical, and
   that feasible ones are in feasible states. LL = VBT.mu *)
   VAR w := v.succ(NIL);
       postmin, postmax, postsz := 0;
       totsz := Rect.Size(v.hv, v.domain);
       orthSize := Rect.Size(Axis.Other[v.hv], v.domain);
       sz: CARDINAL;

  <* INLINE *> PROCEDURE OverFull(goal: CARDINAL) RAISES {} =
  VAR w := v.succ(NIL);
  BEGIN
    WHILE w # NIL DO
      WITH
        p = NARROW(w.upRef, Child),
        sh = VBTClass.GetShape(w, v.hv, orthSize, FALSE)
      DO
        p.size := MIN(goal, sh.lo);
        DEC(goal, p.size);
      END;
      w := v.succ(w)
    END
  END OverFull;

  <* INLINE *> PROCEDURE UnderFull(shortfall: CARDINAL) RAISES {} =
  VAR w := v.succ(NIL); sz: CARDINAL;
  BEGIN
    WHILE w # NIL DO
      WITH
        p = NARROW(w.upRef, Child),
        sh = VBTClass.GetShape(w, v.hv, orthSize, FALSE)
      DO
        IF postmax # postsz THEN
          sz :=
            TRUNC(
              (FLOAT(sh.hi - 1 - sh.pref) / FLOAT(postmax - postsz)) *
                FLOAT(shortfall));
          DEC(shortfall, sz);
          DEC(postmax, sh.hi - 1);
          DEC(postsz, sh.pref);
          p.size := sh.hi - 1 + sz;
        ELSE
          p.size := sh.hi - 1
        END
      END;
      w := v.succ(w)
    END;
    IF shortfall # 0 THEN
      w := v.pred(NIL);
      IF w # NIL THEN
        WITH p = NARROW(w.upRef, Child) DO INC(p.size, shortfall) END
      END
    END
  END UnderFull;

  <* INLINE *> PROCEDURE ScaleUp(delta: CARDINAL) RAISES {} =
  VAR w := v.succ(NIL); sz: CARDINAL;
  BEGIN
    WHILE (w # NIL) AND (delta # 0) DO
      WITH
        p = NARROW(w.upRef, Child),
        sh = VBTClass.GetShape(w, v.hv, orthSize, FALSE)
      DO
        sz :=
          TRUNC(
            (FLOAT(sh.hi - 1 - p.size) / FLOAT(postmax - postsz)) *
              FLOAT(delta));
        DEC(postsz, p.size);
        DEC(postmax, sh.hi - 1);
        DEC(delta, sz);
        INC(p.size, sz)
      END;
      w := v.succ(w)
    END
  END ScaleUp;
  
  <* INLINE *> PROCEDURE ScaleDown(delta: CARDINAL) RAISES {} =
  VAR w := v.succ(NIL); sz: CARDINAL;
  BEGIN
    WHILE (w # NIL) AND (delta # 0) DO
      WITH
        p = NARROW(w.upRef, Child),
        sh = VBTClass.GetShape(w, v.hv, orthSize, FALSE)
      DO
        sz :=
          TRUNC(
            (FLOAT(p.size - sh.lo) / FLOAT(postsz - postmin)) *
              FLOAT(delta));
        DEC(postsz, p.size);
        DEC(postmin, sh.lo);
        DEC(delta, sz);
        DEC(p.size, sz)
      END;
      w := v.succ(w)
    END
  END ScaleDown;
  
  BEGIN
    IF totsz = 0 THEN 
      WHILE w # NIL DO
        WITH p = NARROW(w.upRef, Child) DO
          IF p.size < 0 THEN
            VBTClass.ClearNewShape(w)
          END
        END;
        w := v.succ(w)
      END;
      RETURN 
    END;
    WHILE w # NIL DO
      WITH
        p = NARROW(w.upRef, Child),
        sh = VBTClass.GetShape(w, v.hv, orthSize, p.size < 0)
      DO
        IF p.size < 0 THEN
          ResetSize(p.size, sh, v.adjustable)
        ELSIF NOT v.adjustable THEN 
          p.size := sh.pref 
        END;
        sz := MAX(sh.lo, p.size);
        sz := MIN(sh.hi-1, sz);
        p.size := sz;
        INC(postsz, sz);
        INC(postmin, sh.lo);
        INC(postmax, sh.hi-1)
      END;
      w := v.succ(w)
    END;
    IF postmin > totsz THEN
      OverFull(totsz)
    ELSIF postmax < totsz THEN
      UnderFull(totsz - postmax)
    ELSIF totsz >= postsz THEN
      ScaleUp(totsz - postsz)
    ELSE
      ScaleDown(postsz - totsz)
    END
  END Scale;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  VAR w: VBT.T;
  BEGIN
    IF Rect.IsEmpty(cd.new) THEN
      w := v.succ(NIL);
      WHILE w # NIL DO
        VBTClass.Reshape(w, Rect.Empty, Rect.Empty);
        w := v.succ(w)
      END
    ELSE
      Redisplay2(v, TRUE, cd.saved)
    END
  END Reshape;

PROCEDURE Shape(v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange RAISES {} =
  VAR w := v.succ(NIL); lo, hi, pref := 0;
  BEGIN
    IF ax # v.hv THEN n := 0 END;
    WITH res = v.cache[ax].lastRes DO
      IF v.cache[ax].lastQuery # n THEN
        v.cache[ax].lastQuery := n;
        IF ax # v.hv THEN
          IF w = NIL THEN
            res := VBT.DefaultShape
          ELSE
            WITH sz = VBTClass.GetShape(w, ax, n, FALSE) DO
              lo := sz.lo;
              pref := sz.pref;
              hi := sz.hi
            END;
            w := v.succ(w);
            WHILE w # NIL DO
              WITH sz = VBTClass.GetShape(w, ax, n, FALSE) DO
                lo := MAX(sz.lo, lo);
                pref := MAX(pref, sz.pref);
                hi := MIN(sz.hi, hi)
              END;
              w := v.succ(w)
            END;
            hi := MAX(lo+1, hi);
            pref := MIN(hi-1, pref);
            res := VBT.SizeRange{lo := lo, hi := hi, pref := pref}
          END
        ELSE
          WHILE w # NIL DO
            WITH sz = VBTClass.GetShape(w, ax, n, FALSE) DO
              INC(pref, sz.pref);
              INC(lo, sz.lo);
              INC(hi, sz.hi - 1)
            END;
            w := v.succ(w)
          END;
          res := VBT.SizeRange{lo := lo, hi := hi+1, pref := pref}
        END
      END;
      RETURN res
    END
  END Shape;

PROCEDURE AxisOrder(v: T): Axis.T = 
  BEGIN RETURN Axis.Other[v.hv] END AxisOrder;
  
PROCEDURE NewShape(v: T; ch: VBT.T) RAISES {} =
  BEGIN
    WITH 
      p = NARROW(ch.upRef, Child)
    DO
      IF p.size >= 0 THEN p.size := Word.Not(p.size) END;
    END;
    VBT.Mark(v);
    InvalidateCache(v)
  END NewShape;

PROCEDURE Redisplay(v: T) RAISES {} =
  BEGIN
    Redisplay2(v, FALSE, v.domain)
  END Redisplay;

TYPE SeqRec = RECORD succ, m: CARDINAL; ch: VBT.T; val, newlo: INTEGER END;

PROCEDURE Redisplay2(v: T; inReshape: BOOLEAN; READONLY saved: Rect.T)
RAISES {} =
  VAR a: ARRAY [0..10] OF SeqRec; ra: REF ARRAY OF SeqRec;
  BEGIN
    VBTClass.LocateChanged(v);
    Scale(v);
    IF Rect.IsEmpty(saved) OR Rect.IsEmpty(v.domain) OR NOT v.saveBits THEN
      QuickRedisplay(v, inReshape); RETURN
    END;
    IF v.numCh < NUMBER(a) THEN
      Redisplay3(v, inReshape, saved, a)
    ELSE
      ra := NEW(REF ARRAY OF SeqRec, v.numCh + 1);
      Redisplay3(v, inReshape, saved, ra^)
    END
  END Redisplay2;

(* on entry, child.size >= 0 for all children of v *)
PROCEDURE QuickRedisplay(v: T; inReshape: BOOLEAN) RAISES {} =
  VAR w := v.succ(NIL); newdom := v.domain; lo: INTEGER;
  BEGIN
    IF Rect.IsEmpty(newdom) THEN
      WHILE w # NIL DO
        VBTClass.Reshape(w, Rect.Empty, Rect.Empty);
        w := v.succ(w)
      END
    ELSE
      IF v.hv = Axis.T.Ver THEN
        lo := v.domain.north
      ELSE
        lo := v.domain.west
      END;
      WHILE w # NIL DO
        WITH p = NARROW(w.upRef, Child) DO
          IF p.size # 0 THEN
            IF v.hv = Axis.T.Ver THEN
              newdom.north := lo;
              newdom.south := lo + p.size
            ELSE
              newdom.west := lo;
              newdom.east := lo + p.size
            END;
            INC(lo, p.size);
            IF inReshape OR NOT Rect.Equal(newdom, w.domain) THEN
              VBTClass.Reshape(w, newdom, Rect.Empty)
            END
          ELSE
            VBTClass.Reshape(w, Rect.Empty, Rect.Empty)
          END
        END;
        w := v.succ(w)
      END
    END
  END QuickRedisplay;

(* on entry, child.size >= 0 for all children of v *)
PROCEDURE Redisplay3(v: T; inReshape: BOOLEAN; READONLY saved: Rect.T;
  VAR a: ARRAY OF SeqRec) RAISES {} =
  VAR j,k,len,hd,tl: CARDINAL; w: VBT.T := NIL; lo: INTEGER;
    newdom: Rect.T;
  BEGIN
    (* Find longest descending sequence working backwards *)
    len := 0;
    a[0].m := 0;
    a[0].succ := 0;
    a[0].ch := NIL;
    a[0].val := LAST(INTEGER);
    (* len is the length of the longest ascending (in domain order)
       subsequence of the last i children in the linked list.  For
       each j <= len, a[j].m is the index in a of the head of the
       subsequence of length m which starts last in domain order among all
       such subsequences.  For each 1 <= j <= i, a[j].ch is the
       j'th-last child in linked list order, a[j].succ is the index
       of a[j].ch's successor in a maximal-length ascending subsequence
       starting at a[j].ch, and a[j].val is the south (or east) coordinate
       of a[j].ch.domain.  Empty children are ignored in terms of computing
       sequences. *)
    FOR i := 1 TO v.numCh DO
      w := v.pred(w);
      a[i].ch := w;
      IF Rect.IsEmpty(w.domain) THEN
        a[i].succ := 0
      ELSE
        IF v.hv = Axis.T.Ver THEN
          a[i].val := w.domain.south
        ELSE
          a[i].val := w.domain.east
        END;
        IF a[i].val <= a[a[len].m].val THEN
          a[i].succ := a[len].m;
          INC(len);
          a[len].m := i
        ELSE
          j := 0; k := len;
          WHILE j+1 # k DO
            WITH mid = (j+k) DIV 2 DO
              IF a[i].val <= a[a[mid].m].val THEN
                j := mid
              ELSE
                k := mid
              END
            END
          END;
          a[k].m := i;
          a[i].succ := a[j].m
        END
      END
    END;
    (* Compute domains for windows in best sequence and reshape others *)
    IF v.hv = Axis.T.Ver THEN
      lo := v.domain.north
    ELSE
      lo := v.domain.west
    END;
    j := a[len].m;
    w := v.succ(NIL);
    WHILE w # NIL DO
      IF w = a[j].ch THEN
        a[j].newlo := lo;
        j := a[j].succ
      ELSE
        VBTClass.Reshape(w, Rect.Empty, Rect.Empty)
      END;
      INC(lo, NARROW(w.upRef, Child).size);
      w := v.succ(w)
    END;
    (* Find suitable order to reshape windows that can use old domains *)
    j := a[len].m;
    hd := 0;
    tl := 0;
    (* Here, j, hd, and tl are lists of children linked by
       the succ field.  The proper order consists of the reverse of
       hd, followed by the proper order of j, followed by tl. *)
    WHILE j # 0 DO
      k := a[j].succ;
      WITH p = NARROW(a[j].ch.upRef, Child) DO
        IF a[j].newlo + p.size < a[j].val THEN
          a[j].succ := hd;
          hd := j
        ELSE
          a[j].succ := tl;
          tl := j
        END
      END;
      j := k
    END;
    (* The j list is now empty, so we next reverse hd onto tl. *)
    WHILE hd # 0 DO
      k := a[hd].succ;
      a[hd].succ := tl;
      tl := hd;
      hd := k
    END;
    (* Reshape using old domains *)
    newdom := v.domain;
    WHILE tl # 0 DO
      WITH w = a[tl].ch, p = NARROW(w.upRef, Child) DO
        IF p.size = 0 THEN VBTClass.Reshape(w, Rect.Empty, Rect.Empty)
        ELSE
          IF v.hv = Axis.T.Ver THEN
            newdom.north := a[tl].newlo;
            newdom.south := newdom.north + p.size
          ELSE
            newdom.west := a[tl].newlo;
            newdom.east := newdom.west + p.size
          END;
          IF inReshape OR NOT Rect.Equal(newdom, w.domain) THEN
            VBTClass.Reshape(w, newdom, Rect.Meet(w.domain, saved))
          END
        END
      END;
      tl := a[tl].succ
    END;
    (* Reshape everybody else *)
    w := v.succ(NIL);
    IF v.hv = Axis.T.Ver THEN
      lo := v.domain.north
    ELSE
      lo := v.domain.west
    END;
    WHILE w # NIL DO
      WITH p = NARROW(w.upRef, Child) DO
        IF Rect.IsEmpty(w.domain) AND (p.size # 0) THEN
          IF v.hv = Axis.T.Ver THEN
            newdom.north := lo;
            newdom.south := lo + p.size
          ELSE
            newdom.west := lo;
            newdom.east := lo + p.size
          END;
          INC(lo, p.size);
          VBTClass.Reshape(w, newdom, Rect.Empty)
        ELSE
          INC(lo, p.size)
        END
      END;
      w := v.succ(w)
    END
  END Redisplay3;

BEGIN
END HVSplit.
