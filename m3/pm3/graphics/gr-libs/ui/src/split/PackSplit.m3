(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Jan  8 14:18:00 PST 1996 by heydon   *)
(*      modified on Wed Mar  8 18:22:08 PST 1995 by msm      *)
(*      modified on Tue Mar 10 19:07:22 1992 by steveg   *)
(*      modified on Mon Feb 24 13:53:52 PST 1992 by muller   *)
(*      modified on Sun Nov 10 19:21:39 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE PackSplit EXPORTS PackSplit;

IMPORT VBT, PaintOp, Pixmap, Axis, Point, Rect, ProperSplit, VBTClass,
  Region, PolyRegion, Split;

REVEAL 
  Private = ProperSplit.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT
    ax: Axis.T;
    hgap, vgap: REAL;
    hg, vg: CARDINAL;
    txt : Pixmap.T; 
    op : PaintOp.T; 
    nwAlign, saveBits : BOOLEAN;
    bgChanged := TRUE;
    numCh : CARDINAL := 0;
    bgRegion: Region.T;
    shapeAsked := FALSE;
    lastSize := 0
  OVERRIDES
    reshape := Reshape;
    rescreen := Rescreen;
    repaint := Repaint;
    shape := Shape;
    redisplay := Redisplay;
    beChild := BeChild;
    replace := Replace;
    insert := Insert;
    move := Move;
    index := Index;
    newShape := NewShape;
    axisOrder := AxisOrder;
    init := Be
  END;

TYPE
  Child = ProperSplit.Child OBJECT
            shapeValid                           := FALSE;
            prefShapes: ARRAY Axis.T OF CARDINAL
          END;

PROCEDURE Be(
    v:     T;
    major := Axis.T.Hor;
    hgap, vgap := 1.5;
    txt: Pixmap.T := Pixmap.Solid; 
    op: PaintOp.T := PaintOp.Bg; 
    nwAlign := FALSE;
    saveBits := FALSE): T RAISES {} =
  BEGIN
    v.ax := major;
    v.hgap := hgap;
    v.vgap := vgap;
    v.hg := ROUND(VBT.MMToPixels(v, hgap, Axis.T.Hor));
    v.vg := ROUND(VBT.MMToPixels(v, vgap, Axis.T.Ver));
    v.txt := txt;
    v.op := op;
    v.nwAlign := nwAlign;
    v.bgRegion  := Region.FromRect(v.domain);
    v.saveBits := saveBits;
    RETURN v
  END Be;

PROCEDURE BeChild(v: T; ch: VBT.T) RAISES {} =
  BEGIN
    IF ch.upRef = NIL THEN ch.upRef := NEW(Child) END;
    ProperSplit.T.beChild(v, ch);
    INC(v.numCh);
    VBTClass.SetShortCircuit(ch)
  END BeChild;

PROCEDURE New(
    major := Axis.T.Hor;
    hgap, vgap := 1.5;
    txt: Pixmap.T := Pixmap.Solid; 
    op: PaintOp.T := PaintOp.Bg; 
    nwAlign := FALSE;
    saveBits := FALSE): T RAISES {} =
  BEGIN
    RETURN Be(NEW(T), major, hgap, vgap, txt, op, nwAlign, saveBits)
  END New;
    
PROCEDURE AxisOf(v: T): Axis.T =
  BEGIN RETURN v.ax END AxisOf;

PROCEDURE HGap(v: T): REAL =
  BEGIN RETURN v.hgap END HGap;

PROCEDURE VGap(v: T): REAL =
  BEGIN RETURN v.vgap END VGap;

PROCEDURE Insert(v: T; pred, newch: VBT.T) =
  BEGIN
    Public.insert(v, pred, newch);
    VBT.NewShape(v);
    v.shapeAsked := FALSE
  END Insert;

PROCEDURE Move (v: T; pred, ch: VBT.T) =
  BEGIN
    VBTClass.Reshape(ch, Rect.Empty, Rect.Empty);
    Public.move(v, pred, ch);
    VBT.NewShape(v);
    v.shapeAsked := FALSE
  END Move;
  
PROCEDURE Set(
  v: T;   
  txt: Pixmap.T; 
  op: PaintOp.T := PaintOp.BgFg;
  nwAlign := FALSE)
  RAISES {} =
  BEGIN
    IF (v.txt # txt) OR (v.op # op) OR (nwAlign # v.nwAlign) THEN
      v.txt := txt;
      v.op := op;
      v.nwAlign := nwAlign;
      v.bgChanged := TRUE;
      VBT.Mark(v)
    END
  END Set;

PROCEDURE Get(
  v: T;   
  VAR txt: Pixmap.T; 
  VAR op: PaintOp.T;
  VAR nwAlign: BOOLEAN) =
  BEGIN
    txt := v.txt;
    op := v.op;
    nwAlign := v.nwAlign
  END Get;

PROCEDURE GetShapes(v: VBT.T): ARRAY Axis.T OF CARDINAL =
  VAR ch := NARROW(v.upRef, Child); BEGIN
    IF NOT ch.shapeValid THEN
      WITH shapes = VBTClass.GetShapes(v) DO
        FOR ax := FIRST(Axis.T) TO LAST(Axis.T) DO
          ch.prefShapes[ax] := shapes[ax].pref
        END
      END;
      ch.shapeValid := TRUE
    END;
    RETURN ch.prefShapes
  END GetShapes;

PROCEDURE Replace (v: T; pred, new: VBT.T) RAISES {} =
  VAR  predsh := GetShapes(pred);
  BEGIN
    Public.replace(v, pred, new);
    IF new = NIL OR GetShapes(new) # predsh THEN
      VBT.NewShape(v);
      v.shapeAsked := FALSE
    END;
    DEC(v.numCh)
  END Replace;

PROCEDURE Index(v: T; ch: VBT.T): CARDINAL RAISES {} =
  BEGIN
    IF ch = NIL THEN RETURN v.numCh ELSE RETURN ProperSplit.T.index(v, ch)
    END
  END Index;

PROCEDURE Repaint(v: T; READONLY br: Region.T) RAISES {} =
  VAR ch := v.succ(NIL); delta: Point.T;
  BEGIN
    WHILE ch # NIL DO
      VBTClass.Repaint(ch, br);
      ch := v.succ(ch)
    END;
    IF v.nwAlign THEN 
      delta := Rect.NorthWest(v.domain) 
    ELSE 
      delta := Point.Origin
    END;
    VBT.PaintRegion(v, Region.Meet(v.bgRegion, br), v.op, v.txt, delta)
  END Repaint;

PROCEDURE Rescreen(v: T; READONLY cd: VBT.RescreenRec) RAISES {} =
  VAR ch := v.succ(NIL);
  BEGIN
    v.hg := ROUND(VBT.MMToPixels(v, v.hgap, Axis.T.Hor));
    v.vg := ROUND(VBT.MMToPixels(v, v.vgap, Axis.T.Ver));
    v.bgRegion := Region.Empty;
    WHILE ch # NIL DO
      WITH ur = NARROW(ch.upRef, Child) DO
        ur.shapeValid := FALSE
      END;
      ch := v.succ(ch)
    END;
    VBT.Split.rescreen(v,cd)
  END Rescreen;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  VAR size := Rect.Size(v.ax, cd.new); BEGIN
    Redisplay2(v, cd.saved, cd.prev);
    IF size # 0 AND size # v.lastSize THEN
      VBT.NewShape(v);
      v.shapeAsked := FALSE
    END
  END Reshape;

PROCEDURE Redisplay(v: T) RAISES {} =
  BEGIN
    Redisplay2(v, v.domain, v.domain)
  END Redisplay;

TYPE SeqRec = RECORD succ, m: CARDINAL; ch: VBT.T := NIL; nw: Point.T;
 dom: Rect.T END;

PROCEDURE Redisplay2(v: T; READONLY saved, prev: Rect.T) RAISES {} =
  VAR a: ARRAY [0..10] OF SeqRec; ra: REF ARRAY OF SeqRec;
  BEGIN
    VBTClass.LocateChanged(v);
    IF v.numCh < NUMBER(a) THEN
      Redisplay3(v, saved, prev, a)
    ELSE
      ra := NEW(REF ARRAY OF SeqRec, v.numCh + 1);
      Redisplay3(v, saved, prev, ra^)
    END
  END Redisplay2;

PROCEDURE ComputeNewDomains (         v     : T;
                             VAR      a     : ARRAY OF SeqRec;
                             READONLY pdom  : Rect.T;
                                      really: BOOLEAN          ):
  CARDINAL =
  VAR
    ch                    := v.succ(NIL);
    i                     := v.numCh;
    dh, dv     : CARDINAL;
    nw                    := Rect.NorthWest(pdom);
    maxOnLine  : CARDINAL := 0;
    firstOnLine           := TRUE;

  PROCEDURE AdvanceLine () =
    BEGIN
      IF v.ax = Axis.T.Hor THEN
        INC(nw.v, maxOnLine + v.vg);
        nw.h := pdom.west
      ELSE
        INC(nw.h, maxOnLine + v.hg);
        nw.v := pdom.north
      END;
      maxOnLine := 0;
      firstOnLine := TRUE
    END AdvanceLine;

  BEGIN
    WHILE ch # NIL DO
      IF really THEN a[i].ch := ch END;
      IF Rect.IsEmpty(pdom) THEN
        IF really THEN a[i].dom := Rect.Empty END
      ELSE
        WITH s = GetShapes(ch) DO
          dh := s[Axis.T.Hor];
          dv := s[Axis.T.Ver]
        END;
        IF Rect.Member(Point.MoveHV(nw, dh, dv), pdom) THEN
          IF really THEN
            a[i].dom := Rect.FromCorner(nw, dh, dv)
          END
        ELSE
          IF NOT firstOnLine
               AND (((v.ax = Axis.T.Hor) AND (nw.h + dh > pdom.east))
                      OR ((v.ax = Axis.T.Ver)
                            AND (nw.v + dv > pdom.south))) THEN
            AdvanceLine()
          END;
          IF really THEN
            a[i].dom := Rect.Meet(Rect.FromCorner(nw, dh, dv), pdom)
          END
        END;
        firstOnLine := FALSE;
        IF v.ax = Axis.T.Hor THEN
          INC(nw.h, dh + v.hg);
          maxOnLine := MAX(maxOnLine, dv)
        ELSE
          INC(nw.v, dv + v.vg);
          maxOnLine := MAX(maxOnLine, dh)
        END
      END;
      ch := v.succ(ch);
      DEC(i)
    END;
    IF v.ax = Axis.T.Hor THEN
      RETURN nw.v - pdom.north + maxOnLine
    ELSE
      RETURN nw.h - pdom.west + maxOnLine
    END
  END ComputeNewDomains;

PROCEDURE QuickRedisplay(v: T;
  READONLY saved: Rect.T;
  READONLY a: ARRAY OF SeqRec;
  useSaved: BOOLEAN) =
  BEGIN
    FOR i := 1 TO v.numCh DO
      IF NOT Rect.Equal(a[i].ch.domain, a[i].dom) THEN
        VBTClass.Reshape(a[i].ch, Rect.Empty, Rect.Empty)
      END
    END;
    FOR i := 1 TO v.numCh DO
      IF NOT Rect.Equal(a[i].ch.domain, a[i].dom) THEN
        VBTClass.Reshape(a[i].ch, a[i].dom, Rect.Empty)
      ELSIF NOT Rect.Subset(a[i].dom, saved) THEN
        VBTClass.Repaint(a[i].ch, Region.Difference(
          Region.FromRect(a[i].dom), Region.FromRect(saved)))
      END
    END;
    RedisplayBkg(v, saved, useSaved)
  END QuickRedisplay;

PROCEDURE LessThan(READONLY p1, p2: Point.T; ax: Axis.T): BOOLEAN =
  BEGIN
    IF ax = Axis.T.Hor THEN
      RETURN (p1.v < p2.v) OR ((p1.v = p2.v) AND (p1.h < p2.h))
    ELSE
      RETURN (p1.h < p2.h) OR ((p1.h = p2.h) AND (p1.v < p2.v))
    END
  END LessThan;

PROCEDURE Redisplay3(v:T; READONLY saved, prev: Rect.T; VAR a: ARRAY OF
 SeqRec) RAISES {} =
  VAR j,k,len,hd,tl: CARDINAL; w: VBT.T := NIL; used := PolyRegion.Empty;
    useSaved := v.txt = Pixmap.Solid OR 
      Point.Equal(Rect.NorthWest(prev), Rect.NorthWest(v.domain));
  BEGIN
    EVAL ComputeNewDomains(v, a, v.domain, TRUE);
    IF Rect.IsEmpty(saved) OR Rect.IsEmpty(v.domain) OR NOT v.saveBits THEN
      QuickRedisplay(v, saved, a, useSaved); RETURN
    END;
    (* Find longest descending sequence working backwards *)
    len := 0;
    a[0].m := 0;
    a[0].succ := 0;
    a[0].ch := NIL;
    a[0].nw := Rect.SouthEast(prev);
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
      w := a[i].ch;
      IF Rect.IsEmpty(w.domain) OR Rect.IsEmpty(a[i].dom) THEN
        a[i].succ := 0
      ELSE
        a[i].nw := Rect.NorthWest(w.domain);
        IF LessThan(a[i].nw, a[a[len].m].nw, v.ax) THEN
          a[i].succ := a[len].m;
          INC(len);
          a[len].m := i
        ELSE
          j := 0; k := len;
          WHILE j+1 # k DO
            WITH mid = (j+k) DIV 2 DO
              IF LessThan(a[i].nw, a[a[mid].m].nw, v.ax) THEN
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
    (* Reshape moving windows not in longest sequence to empty *)
    j := a[len].m;
    w := v.succ(NIL);
    WHILE w # NIL DO
      IF w = a[j].ch THEN
        j := a[j].succ
      ELSE 
        VBTClass.Reshape(w, Rect.Empty, Rect.Empty)
      END;
      w := v.succ(w)
    END;
    (* Find suitable order to reshape windows that can use old domains *)
    j := a[len].m;
    hd := 0;
    tl := 0;
    (* Here, j, hd, and tl are lists of children linked by
       the succ field.  The proper order consists of the reverse of
       hd, followed by the proper order of j, followed by tl. 
       used is the join of the old domains of elements of tl; nothing
       on tl can have a new domain which overlaps the old domain of
       anything later on tl *)
    WHILE j # 0 DO
      k := a[j].succ;
      IF LessThan(Rect.NorthWest(a[j].dom), a[j].nw, v.ax) THEN
        a[j].succ := hd;
        hd := j
      ELSIF PolyRegion.OverlapRect(used, a[j].dom) THEN
        VBTClass.Reshape(a[j].ch, Rect.Empty, Rect.Empty)
      ELSE
        PolyRegion.JoinRect(used, a[j].ch.domain);
        a[j].succ := tl;
        tl := j
      END;
      j := k
    END;
    (* The j list is now empty, so we next reverse hd onto tl. *)
    WHILE hd # 0 DO
      k := a[hd].succ;
      IF PolyRegion.OverlapRect(used, a[hd].dom) THEN
        VBTClass.Reshape(a[hd].ch, Rect.Empty, Rect.Empty)
      ELSE
        PolyRegion.JoinRect(used, a[hd].ch.domain);
        a[hd].succ := tl;
        tl := hd
      END;
      hd := k
    END;
    (* Reshape using old domains *)
    WHILE tl # 0 DO
      WITH w = a[tl].ch DO
        IF NOT Rect.Equal(w.domain, a[tl].dom) THEN
          VBTClass.Reshape(w, a[tl].dom, saved)
        ELSIF NOT Rect.Subset(w.domain, saved) THEN
          VBTClass.Repaint(w, Region.Difference(
            Region.FromRect(w.domain), Region.FromRect(saved)))
        END
      END;
      tl := a[tl].succ
    END;
    (* Reshape everybody else *)
    FOR i := 1 TO v.numCh DO
      WITH w = a[i].ch DO
        IF NOT Rect.Equal(w.domain, a[i].dom) THEN
          VBTClass.Reshape(w, a[i].dom, Rect.Empty)
        END
      END
    END;
    RedisplayBkg(v, saved, useSaved)
  END Redisplay3;

PROCEDURE RedisplayBkg(v: T; READONLY saved: Rect.T; useSaved: BOOLEAN) =
  (* redisplay the background of v.  LL = VBT.mu *)
  VAR
    pr := PolyRegion.Empty; 
    ch := v.succ(NIL); 
    delta: Point.T;    
    rgn:   Region.T;
  BEGIN
    WHILE ch # NIL DO
      PolyRegion.JoinRect(pr, ch.domain);
      ch := v.succ(ch)
    END;
    IF v.nwAlign THEN
      delta := Rect.NorthWest(v.domain)
    ELSE
      delta := Point.Origin
    END;
    rgn := v.bgRegion;
    useSaved := useSaved AND NOT v.bgChanged;
    v.bgChanged := FALSE;
    v.bgRegion  := PolyRegion.Complement(pr, Region.FromRect(v.domain));
    IF useSaved THEN
      VBT.PaintRegion(
        v, Region.Difference(v.bgRegion, Region.MeetRect(saved, rgn)),
        v.op, v.txt, delta)
    ELSE
      VBT.PaintRegion(v, v.bgRegion, v.op, v.txt, delta)
    END
  END RedisplayBkg;

PROCEDURE Shape (v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange RAISES {} =
  VAR
    a   : ARRAY [0 .. 0] OF SeqRec;
    res : VBT.SizeRange;
    r   : Rect.T;
    pref: CARDINAL;
  BEGIN
    IF ax = v.ax THEN
      res.lo := MaxMinChildSize(v, ax);
      res.hi := MAX(res.lo + 1, VBT.DefaultShape.hi);
      res.pref := MAX(res.lo, MIN(res.hi - 1, Rect.Size(ax, v.domain)));
      RETURN res
    END;
    IF n = 0 THEN n := Rect.Size(v.ax, v.domain) END;
    IF NOT v.shapeAsked THEN
      v.lastSize := n;
      v.shapeAsked := TRUE
    ELSIF v.lastSize # n THEN
      v.lastSize := 0
    END;
    IF n = 0 THEN
      res.lo := MaxMinChildSize(v, ax);
      res.hi := MAX(res.lo + 1, VBT.DefaultShape.hi);
      res.pref := MAX(res.lo, MIN(res.hi - 1, Rect.Size(ax, v.domain)));
      RETURN res
    END;
    IF ax = Axis.T.Ver THEN
      r := Rect.FromSize(n, LAST(INTEGER))
    ELSE
      r := Rect.FromSize(LAST(INTEGER), n)
    END;
    pref := ComputeNewDomains(v, a, r, FALSE);
    res.lo := pref;
    res.pref := pref;
    res.hi := pref + 1;
    RETURN res
  END Shape;

PROCEDURE MaxMinChildSize (v: T; ax: Axis.T): CARDINAL =
  <* FATAL Split.NotAChild *>
  VAR
    res: CARDINAL := 0;
    ch            := Split.Succ(v, NIL);
  BEGIN
    WHILE ch # NIL DO
      res := MAX(res, GetShapes(ch)[ax]);
      ch := Split.Succ(v, ch)
    END;
    RETURN res
  END MaxMinChildSize;

PROCEDURE AxisOrder (v: T): Axis.T =
  BEGIN
    RETURN v.ax
  END AxisOrder;
  
PROCEDURE NewShape (v: T; ch: VBT.T) RAISES {} =
  VAR child := NARROW(ch.upRef, Child);
  BEGIN
    child.shapeValid := FALSE;
    VBT.Mark(v);
    VBT.NewShape(v);
    v.shapeAsked := FALSE
  END NewShape;
  
BEGIN
END PackSplit.
