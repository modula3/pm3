(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Oct  9 23:39:55 PDT 1992 by mhb        *)
(*      modified on Mon Sep 28 13:08:39 PDT 1992 by steveg     *)
(*      modified on Fri Jul 24 17:45:35 PDT 1992 by mjordan    *)
(*      modified on Tue Jul 21 14:23:50 PDT 1992 by jdd        *)
<* PRAGMA LL *>

MODULE RectsVBT;

IMPORT Axis, PaintOp, Point, Pts, Rect, Region, VBT;

TYPE
  ItemInfo = RECORD
               existFg: BOOLEAN;    (* does the entry exist? *)
               posn   : RealRect;   (* in world coordinates *)
               op     : PaintOp.T;  (* color to paint item *)
             END;

REVEAL
  T = Public BRANDED OBJECT
        mu: MUTEX;
        (* protected by mu: *)
        N                 : INTEGER;
        items             : REF ARRAY OF ItemInfo;
        bg                : PaintOp.T;
        margin            : Rect.T;                 (* in pixels *)
        marginPts         : RealRect;               (* in points *)
        wc                : RealRect;
        minWd, minHt      : INTEGER;                (* in pixels *)
        minWdPts, minHtPts: REAL;                   (* in points *)
      OVERRIDES
        init      := Init;
        repaint   := Repaint;
        redisplay := Redisplay;
        rescreen  := Rescreen;
        shape     := Shape;
      END;


PROCEDURE Reset (v: T) =
  <* LL = mu *>
  (* call when need to convert pts to pixels *)
  BEGIN
    v.minWd := Pts.ToScreenPixels(v, v.minWdPts, Axis.T.Hor);
    v.minHt := Pts.ToScreenPixels(v, v.minHtPts, Axis.T.Ver);
    v.margin.north :=
      Pts.ToScreenPixels(v, v.marginPts.north, Axis.T.Ver);
    v.margin.south :=
      Pts.ToScreenPixels(v, v.marginPts.south, Axis.T.Ver);
    v.margin.west :=
      Pts.ToScreenPixels(v, v.marginPts.west, Axis.T.Hor);
    v.margin.east :=
      Pts.ToScreenPixels(v, v.marginPts.east, Axis.T.Hor);
  END Reset;

PROCEDURE Redisplay (v: T) =
  BEGIN
    LOCK v.mu DO Reset(v) END;
    Repaint(v, Region.Full)
  END Redisplay;

PROCEDURE Repaint (v: T; <*UNUSED*> READONLY rgn: Region.T) =
  BEGIN
    LOCK v.mu DO
      VBT.PaintTint(v, Rect.Full, v.bg);
      IF v.N > 0 THEN
        FOR i := 1 TO v.N DO PaintItem(v, v.items[i]) END;
        PaintItem(v, v.items[0]);
      END
    END
  END Repaint;

PROCEDURE Rescreen (v: T; <* UNUSED *> READONLY cd: VBT.RescreenRec) =
  BEGIN
    LOCK v.mu DO Reset(v) END
  END Rescreen;

PROCEDURE Shape (<* UNUSED *> v : T;
                 <* UNUSED *> ax: Axis.T;
                 <* UNUSED *> n : CARDINAL): VBT.SizeRange =
  BEGIN
    RETURN VBT.SizeRange{
             VBT.DefaultShape.lo, 100, VBT.DefaultShape.hi};
  END Shape;

PROCEDURE NonEmpty (v: T): BOOLEAN =
  BEGIN
    RETURN NOT Rect.IsEmpty(VBT.Domain(v))
  END NonEmpty;

PROCEDURE Init (v: T): T =
  BEGIN
    v.mu := NEW(MUTEX);
    LOCK v.mu DO
      v.N := 0;
      v.items := NIL;
      v.bg := PaintOp.Bg;
      v.marginPts.west := 0.0;
      v.marginPts.east := 0.0;
      v.marginPts.north := 0.0;
      v.marginPts.south := 0.0;
      v.wc.west := 0.0;
      v.wc.south := 0.0;
      v.wc.east := 1.0;
      v.wc.north := 1.0;
      v.minWdPts := 4.0;
      v.minHtPts := 4.0;
      Reset(v)
    END;
    RETURN v
  END Init;

PROCEDURE SetBg (v: T; op: PaintOp.T) =
  BEGIN
    LOCK v.mu DO v.bg := op; VBT.Mark(v) END
  END SetBg;

PROCEDURE SetMargin (v: T; west, south, east, north: REAL) =
  BEGIN
    LOCK v.mu DO
      v.marginPts.west := west;
      v.marginPts.south := south;
      v.marginPts.east := east;
      v.marginPts.north := north;
      VBT.Mark(v)
    END
  END SetMargin;

PROCEDURE SetWC (v: T; west, south, east, north: REAL) =
  BEGIN
    LOCK v.mu DO
      v.wc.west := west;
      v.wc.south := south;
      v.wc.east := east;
      v.wc.north := north;
      VBT.Mark(v)
    END
  END SetWC;

PROCEDURE SetMins (v: T; wd, ht: REAL) =
  BEGIN
    LOCK v.mu DO
      v.minWdPts := wd;
      v.minHtPts := ht;
      VBT.Mark(v)
    END
  END SetMins;


PROCEDURE Draw (v: T; i: CARDINAL) =
  BEGIN
    LOCK v.mu DO PaintItem(v, v.items[i]) END
  END Draw;

PROCEDURE Erase (v: T; i: CARDINAL) =
  BEGIN
    LOCK v.mu DO EraseItem (v, i) END
  END Erase;

PROCEDURE EraseItem (v: T; i: CARDINAL) =
  <* LL = mu *>
  VAR forged: ItemInfo;
  BEGIN
    IF v.items[i].existFg THEN
      InitItem(forged);
      forged.existFg := TRUE;
      forged.posn := v.items[i].posn;
      forged.op := v.bg;
      PaintItem(v, forged)
    END
  END EraseItem;


PROCEDURE SetN (v: T; N: CARDINAL; redisplayFg: BOOLEAN := FALSE) =
  BEGIN
    LOCK v.mu DO
      IF redisplayFg AND (v.N > 0) THEN
        FOR i := 1 TO v.N DO EraseItem(v, i) END;
        EraseItem(v, 0);
      END;
      v.N := N;
      v.items := NEW(REF ARRAY OF ItemInfo, v.N + 1);
      FOR i := 0 TO v.N DO InitItem(v.items[i]) END
    END
  END SetN;

PROCEDURE Exists (v: T; i: CARDINAL): BOOLEAN =
  BEGIN
    LOCK v.mu DO RETURN v.items[i].existFg END
  END Exists;

PROCEDURE Delete (v          : T;
                  i          : CARDINAL;
                  redisplayFg: BOOLEAN    := FALSE) =
  BEGIN
    LOCK v.mu DO
      IF redisplayFg THEN EraseItem(v, i); END;
      InitItem(v.items[i])
    END
  END Delete;


PROCEDURE Position (v                       : T;
                    i                       : CARDINAL;
                    west, south, east, north: REAL;
                    redisplayFg                          := FALSE) =
  BEGIN
    LOCK v.mu DO
      WITH item = v.items[i] DO
	IF redisplayFg THEN EraseItem (v, i) END;
        item.existFg := TRUE;
        item.posn.north := north;
        item.posn.south := south;
        item.posn.east := east;
        item.posn.west := west;
        IF redisplayFg THEN PaintItem(v, item) END
      END
    END
  END Position;

PROCEDURE Color (v          : T;
                 i          : CARDINAL;
                 op         : PaintOp.T;
                 redisplayFg: BOOLEAN     := FALSE) =
  BEGIN
    LOCK v.mu DO
      WITH item = v.items[i] DO
        item.existFg := TRUE;
        item.op := op;
        IF redisplayFg THEN PaintItem(v, item) END
      END
    END
  END Color;

EXCEPTION NoItem;

PROCEDURE GetColor (v          : T;
                    i          : CARDINAL): PaintOp.T =
  <* FATAL NoItem *>
  BEGIN
    LOCK v.mu DO
      WITH item = v.items[i] DO
        IF NOT item.existFg THEN RAISE NoItem END;
        RETURN item.op;
      END
    END;
  END GetColor;

PROCEDURE Locate (v: T; i: CARDINAL): Rect.T =
  BEGIN
    LOCK v.mu DO RETURN LocateItem(v, v.items[i]) END
  END Locate;


PROCEDURE VBT2WC (v: T; pt: Point.T): RealPoint =
  BEGIN
    LOCK v.mu DO RETURN UnmapPt(v, pt.h, pt.v) END
  END VBT2WC;

PROCEDURE WC2VBT (v: T; pt: RealPoint): Point.T =
  BEGIN
    LOCK v.mu DO RETURN MapPt(v, pt.h, pt.v) END
  END WC2VBT;

PROCEDURE Map (x, w1, w2: REAL; v1, v2: REAL): REAL =
  <* LL arbitrary *>
  BEGIN
    IF w2 = w1 THEN
      RETURN 0.0
    ELSE
      RETURN v1 + (x - w1) * (v2 - v1) / (w2 - w1)
    END
  END Map;

PROCEDURE MapPt (v: T; rh, rv: REAL): Point.T =
  <* LL = mu *>
  VAR r := VBT.Domain(v);
  BEGIN
    INC(r.north, v.margin.north);
    INC(r.west, v.margin.west);
    DEC(r.south, v.margin.south);
    DEC(r.east, v.margin.east);
    RETURN Point.FromCoords(
             TRUNC(0.5 + Map(rh, v.wc.west, v.wc.east,
                             FLOAT(r.west), FLOAT(r.east))),
             TRUNC(0.5 + Map(rv, v.wc.north, v.wc.south,
                             FLOAT(r.north), FLOAT(r.south))))
  END MapPt;

PROCEDURE UnmapPt (v: T; rh, rv: INTEGER): RealPoint =
  <* LL = mu *>
  VAR
    r : Rect.T;
    rp: RealPoint;
  BEGIN
    r := VBT.Domain(v);
    INC(r.north, v.margin.north);
    INC(r.west, v.margin.west);
    DEC(r.south, v.margin.south);
    DEC(r.east, v.margin.east);
    rp.h := Map(FLOAT(rh), FLOAT(r.west), FLOAT(r.east),
                v.wc.west, v.wc.east);
    rp.v := Map(FLOAT(rv), FLOAT(r.north), FLOAT(r.south),
                v.wc.north, v.wc.south);
    RETURN rp
  END UnmapPt;


PROCEDURE LocateItem (v: T; READONLY rect: ItemInfo): Rect.T =
  <* LL = mu *>
  VAR
    r     : Rect.T;
    wd, ht: INTEGER;
    nw, se: Point.T;
  BEGIN
    r := Rect.Empty;
    IF NonEmpty(v) AND rect.existFg THEN
      (* can't use Rect and Point package, since nw and se points
         might map to the same pixel. *)
      nw := MapPt(v, rect.posn.west, rect.posn.north);
      se := MapPt(v, rect.posn.east, rect.posn.south);
      r.north := nw.v;
      r.south := se.v;
      r.west := nw.h;
      r.east := se.h;
      wd := MAX(r.east - r.west, v.minWd);
      ht := MAX(r.south - r.north, v.minHt);
      IF (wd = v.minWd) OR (ht = v.minHt) THEN
        r := Center(FromSize(wd, ht), Middle(r));
      END;
    END;
    RETURN r
  END LocateItem;

PROCEDURE InitItem (VAR rect: ItemInfo) =
  <* LL = mu *>
  BEGIN
    rect.existFg := FALSE;
    rect.op := PaintOp.Fg;
  END InitItem;

PROCEDURE PaintItem (v: T; READONLY rect: ItemInfo) =
  <* LL = mu *>
  BEGIN
    VBT.PaintTint(v, LocateItem(v, rect), rect.op)
  END PaintItem;


PROCEDURE FromSize (hor, ver: CARDINAL): Rect.T =
  <* LL arbitrary *>
  (* like Rect.FromSize, but degenerate rects are OK *)
  VAR r: Rect.T;
  BEGIN
    r.west := 0;
    r.east := hor;
    r.north := 0;
    r.south := ver;
    RETURN r;
  END FromSize;

PROCEDURE Middle (READONLY r: Rect.T): Point.T =
  <* LL arbitrary *>
  (* like Point.Middle, but degenerate rects are OK *)
  VAR p: Point.T;
  BEGIN
    p.h := (r.west + r.east) DIV 2;
    p.v := (r.north + r.south) DIV 2;
    RETURN p;
  END Middle;

PROCEDURE Center (READONLY r: Rect.T; READONLY p: Point.T):
  Rect.T =
  <* LL arbitrary *>
  (* like Rect.Center, but degenerate rects are OK *)
  VAR
    res : Rect.T;
    h, v: INTEGER;
  BEGIN
    h := p.h - ((r.west + r.east) DIV 2);
    v := p.v - ((r.north + r.south) DIV 2);
    res.west := r.west + h;
    res.east := r.east + h;
    res.north := r.north + v;
    res.south := r.south + v;
    RETURN res
  END Center;


BEGIN
END RectsVBT.
