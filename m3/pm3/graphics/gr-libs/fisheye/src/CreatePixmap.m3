(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:32 PDT 1992 by muller                   *)

(* This module creates pixmaps for and rectangles circles, the pixmaps
   are returned as 1 for Fg for inside or border, 0 for Bg, and 2 for
   Transparent outside the circle, rectangle *) 

MODULE CreatePixmap;

IMPORT Pixmap, Palette, ScrnPixmap, ScreenType, TrestleComm;
IMPORT Rect, Point;

TYPE
  CirclePixmapClosure = Palette.PixmapClosure 
  OBJECT
    radius: REAL;
    borderThickness: REAL;
  OVERRIDES 
    apply := CirclePixmapApply 
  END;

PROCEDURE Circle(radius: REAL; borderThickness: REAL := - 1.0): Pixmap.T =
  BEGIN
    RETURN Palette.FromPixmapClosure(
      NEW(CirclePixmapClosure, 
        radius := radius, 
        borderThickness := borderThickness ));
  END Circle;


PROCEDURE CirclePixmapApply(
    cl: CirclePixmapClosure; 
    st: ScreenType.T): ScrnPixmap.T =
  BEGIN
    TRY
      RETURN st.pixmap.load(CirclePixmap(cl.radius, cl.borderThickness))
    EXCEPT
      TrestleComm.Failure => RETURN Palette.ResolvePixmap(st, Pixmap.Empty)
    END;
  END CirclePixmapApply;


PROCEDURE CircleBox(r: REAL): Rect.T =
VAR
  rf := FLOOR(-r); rc := FLOOR(r)+1;
BEGIN
  RETURN Rect.T{rf, rc-1, rf, rc-1};
END CircleBox;


PROCEDURE CirclePixmap(
    r: REAL; 
    borderThickness: REAL := 1.0): ScrnPixmap.Raw =
  VAR 
    rf := FLOOR(-r); rc := FLOOR(r)+1;
    res := ScrnPixmap.NewRaw(1, Rect.FromEdges(rf, rc, rf, rc)); 
    rSqr := FLOOR(r*r);
    innerRSqr := FLOOR((r-borderThickness) * (r-borderThickness));
    vSqr, hSqr: INTEGER;
  BEGIN
    innerRSqr := MIN (innerRSqr, rSqr);
    FOR h := rf TO rc-1 DO
      hSqr := h * h;
      FOR v := rf TO rc-1 DO
        vSqr := v * v;
        IF hSqr + vSqr <= rSqr THEN
          res.set(Point.T{h,v}, 1);
(*
          IF (hSqr + vSqr <= innerRSqr) THEN
            res.set(Point.T{h,v}, 0);
          ELSE
            res.set(Point.T{h,v}, 1);
          END;
*)
        ELSE
          res.set(Point.T{h,v}, 0)
        END
      END
    END;
    RETURN res
  END CirclePixmap;

(******************************* Rectangle *******************************)
TYPE
  RectanglePixmapClosure = Palette.PixmapClosure 
  OBJECT
    a, b: REAL;
    borderThickness: REAL;
  OVERRIDES 
    apply := RectanglePixmapApply 
  END;

PROCEDURE Rectangle(a, b: REAL; borderThickness: REAL := - 1.0): Pixmap.T =
  BEGIN
    RETURN Palette.FromPixmapClosure(
      NEW(RectanglePixmapClosure, 
        a := a,
        b := b, 
        borderThickness := borderThickness ));
  END Rectangle;


PROCEDURE RectanglePixmapApply(
    cl: RectanglePixmapClosure; 
    st: ScreenType.T): ScrnPixmap.T =
  BEGIN
    TRY
      RETURN
        st.pixmap.load(RectanglePixmap(cl.a, cl.b, cl.borderThickness))
    EXCEPT
      TrestleComm.Failure => RETURN Palette.ResolvePixmap(st, Pixmap.Empty)
    END;
  END RectanglePixmapApply;


PROCEDURE RectangleBox(a, b: REAL): Rect.T =
  BEGIN
    RETURN
      Rect.T{FLOOR(-a/2.0), FLOOR(a/2.0)+1, FLOOR(-b/2.0), FLOOR(b/2.0)+1};
  END RectangleBox;


PROCEDURE RectanglePixmap(
    a, b: REAL; 
    borderThickness: REAL := -1.0): ScrnPixmap.Raw =
  VAR 
    w, e, n, s, bor: INTEGER;
    innerRectangle: Rect.T;
    res: ScrnPixmap.Raw;
  BEGIN
    w := FLOOR(-a/2.0); e := FLOOR(a/2.0)+1;
    n := FLOOR(-b/2.0); s := FLOOR(b/2.0)+1;
    IF borderThickness >= 1.0 THEN
      bor := FLOOR(borderThickness);
      innerRectangle := Rect.T{w + bor, e - bor, n + bor, s - bor};
    ELSE
      innerRectangle := Rect.Empty;
    END;
    res := ScrnPixmap.NewRaw(1, Rect.FromEdges(w, e, n, s)); 

    FOR h := w TO e-1 DO
      FOR v := n TO s-1 DO
        IF Rect.Member(Point.T{h,v}, innerRectangle) THEN
          res.set(Point.T{h,v}, 0);
        ELSE
          res.set(Point.T{h,v}, 1);
        END;
      END
    END;
    RETURN res
  END RectanglePixmap;

BEGIN
END CreatePixmap.



