(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: TextVBT.m3, coded by cgn Sun Jun 28 16:13:38 1987 *)
(* Last modified on Thu Mar  9 18:01:01 PST 1995 by msm     *)
(*      modified on Tue Mar 10 19:08:01 1992 by steveg  *)
(*      modified on Mon Feb 24 13:54:51 PST 1992 by muller  *)
(*      modified on Tue Nov 19 19:11:50 PST 1991 by gnelson *)
<*PRAGMA LL*>

MODULE TextVBT;

IMPORT
  Axis, Font, Rect, PaintOp, Point, Region,
  Text, TextVBTClass, VBT, Palette, VBTClass, Pixmap;

REVEAL T = TextVBTClass.T BRANDED OBJECT
      <* LL >= {VBT.mu} *>
      font: Font.T;
      scheme: PaintOp.ColorQuad;
      marginMM: ARRAY Axis.T OF REAL; (* in millimeters *)
      margin: ARRAY Axis.T OF INTEGER; (* margin in pixels *)
      align: ARRAY Axis.T OF REAL;
      (* holds halign and valign *)
      textRect: Rect.T;
      (* The bounding rectangle of the text, properly positioned in the
         vbt's domain *)
      refpt: Point.T;
      (* The reference point of the text. *)
      selfClearing, displayingFocus, hasFocus := FALSE;
    OVERRIDES
      misc := Misc;
      read := Read;
      mouse := Mouse;
      repaint := Repaint;
      reshape := Reshape;
      rescreen := Rescreen;
      redisplay := Redisplay;
      shape := Shape;
      init := Be
    END;

PROCEDURE Misc(v: T; READONLY cd: VBT.MiscRec) =
  BEGIN
    IF cd.type = VBT.Lost AND cd.selection = VBT.Source AND v.hasFocus THEN
      v.hasFocus := FALSE;
      VBT.Mark(v)
    END
  END Misc;

PROCEDURE Read(v: T; sel: VBT.Selection; tc: CARDINAL): VBT.Value 
  RAISES {VBT.Error} =
  BEGIN
    IF sel # VBT.Source THEN 
      RAISE VBT.Error(VBT.ErrorCode.Unreadable)
    ELSIF tc # TYPECODE(TEXT) THEN
      RAISE VBT.Error(VBT.ErrorCode.WrongType)
    ELSE
      LOCK v DO RETURN VBT.FromRef(v.text) END
    END
  END Read;

PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown AND 
      VBT.Modifier.Control IN cd.modifiers AND 
      cd.whatChanged = VBT.Modifier.MouseL AND
      NOT v.hasFocus THEN
      TRY
        VBT.Acquire(v, VBT.Source, cd.time);
        v.hasFocus := TRUE;
        VBT.Mark(v)
      EXCEPT
        VBT.Error =>
      END
    END
  END Mouse;

PROCEDURE Be(
    v: T;
    txt: TEXT;
    halign, valign: REAL;
    hm, vm: REAL;
    fnt: Font.T;
    paintScheme: PaintOp.ColorQuad): T
  RAISES {} =
  BEGIN
    <* ASSERT txt # NIL *>
    LOCK v DO v.text := txt END;
    v.font := fnt;
    IF paintScheme = NIL THEN paintScheme := PaintOp.bgFg END;
    v.scheme := paintScheme;
    v.marginMM[Axis.T.Hor] := hm;
    v.marginMM[Axis.T.Ver] := vm;
    v.align[Axis.T.Hor] := halign;
    v.align[Axis.T.Ver] := valign;
    v.textRect := Rect.Empty;
    SetAndAlign(v);
    RETURN v
  END Be;

CONST Large = 99999;

PROCEDURE Shape(v: T; ax: Axis.T; <*UNUSED*> n: CARDINAL): VBT.SizeRange RAISES {} =
  VAR sr: VBT.SizeRange;
  BEGIN
    IF ax = Axis.T.Hor THEN
      sr.lo := Rect.HorSize(v.textRect) + 2 * v.margin[Axis.T.Hor]
    ELSE
      sr.lo := Rect.VerSize(v.textRect) + 2 * v.margin[Axis.T.Ver]
    END;
    sr.pref := sr.lo;
    sr.hi := Large;
    RETURN sr
  END Shape;

PROCEDURE SetAndAlign(v: T) =
  (* Set the textRect, refpt, fnt, op fields of v, using the text, font,
     screenType, and margin fields. LL = VBT.mu *)
  VAR txt: TEXT; emptyText := FALSE; BEGIN
    LOCK v DO txt := v.text END;
    FOR i := FIRST(Axis.T) TO LAST(Axis.T) DO
      v.margin[i] := ROUND(VBT.MMToPixels(v, v.marginMM[i], i))
    END;
    IF Text.Length(txt) = 0 THEN txt := "X"; emptyText := TRUE END;
    WITH bb = VBT.BoundingBox(v, txt, v.font), tr = v.textRect DO
      IF Rect.HorSize(tr) # Rect.HorSize(bb) OR
         Rect.VerSize(tr) # Rect.VerSize(bb) THEN
        VBT.NewShape(v) 
      END;
      v.textRect := bb;
      v.refpt := Point.Origin
    END;
    VAR st := VBT.ScreenTypeOf(v); BEGIN
      IF st = NIL THEN 
        v.selfClearing := TRUE
      ELSE
        VAR sf := Palette.ResolveFont(st, v.font); BEGIN
          v.selfClearing := sf # NIL AND sf.metrics # NIL 
          AND NOT emptyText AND sf.metrics.selfClearing
        END
      END
    END;
    Align(v)
  END SetAndAlign;

PROCEDURE Align(v: T) =
  (* Translate v.txtRect and v.refpt within v.domain to satisfy the
     alignment properties of v. LL = VBT.mu *)
  VAR delta: Point.T; 
  BEGIN
    IF VBT.ScreenTypeOf(v) = NIL THEN RETURN END;
    WITH dom = VBT.Domain(v) DO
      delta.h :=
        (dom.west + v.margin[Axis.T.Hor] - v.textRect.west) +
          TRUNC(
            v.align[Axis.T.Hor] *
              FLOAT(
                (Rect.HorSize(dom) - 2 * v.margin[Axis.T.Hor] -
                   Rect.HorSize(v.textRect))));
      delta.v :=
        (dom.north + v.margin[Axis.T.Ver] - v.textRect.north) +
          TRUNC(
            v.align[Axis.T.Ver] *
              FLOAT(
                (Rect.VerSize(dom) - 2 * v.margin[Axis.T.Ver] -
                   Rect.VerSize(v.textRect))))
    END;
    v.textRect := Rect.Move(v.textRect, delta);
    v.refpt := Point.Move(v.refpt, delta)
  END Align;

PROCEDURE New(
    txt: TEXT;
    halign: REAL := 0.5;
    valign: REAL := 0.5;
    hmarginMM: REAL := 0.66;
    vmarginMM: REAL := -1.0;
    fnt: Font.T := Font.BuiltIn;
    paintScheme: PaintOp.ColorQuad := NIL)
    : T =
  BEGIN
    IF vmarginMM = -1.0 THEN vmarginMM := hmarginMM END;
    RETURN Be(NEW(T), txt, halign, valign, 
      hmarginMM, vmarginMM, fnt, paintScheme)
  END New;

PROCEDURE Repaint (v: T; READONLY rgn: Region.T) RAISES {} =
  VAR
    a  : Rect.Partition;
    txt: TEXT;
  BEGIN
    LOCK v DO txt := v.text END;
    IF v.selfClearing AND NOT v.displayingFocus THEN
      Rect.Factor(rgn.r, v.textRect, a, 0, 0);
      FOR i := 0 TO 4 DO
        IF i # 2 THEN VBT.PaintTint(v, a[i], v.scheme.bg) END
      END;
      VBT.PaintText(v, a[2], v.refpt, v.font, txt, v.scheme.bgFg)
    ELSE
      IF NOT v.displayingFocus THEN
        VBT.PaintTint(v, rgn.r, v.scheme.bg)
      ELSE
        VBT.PaintTexture(v, rgn.r, v.scheme.bgFg, Pixmap.Gray)
      END;
      VBT.PaintText(v, rgn.r, v.refpt, v.font, txt, v.scheme.transparentFg)
    END
  END Repaint;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  BEGIN
    IF cd.marked THEN 
      SetAndAlign(v); 
      v.displayingFocus := v.hasFocus 
    ELSE 
      Align(v) 
    END;
    IF NOT Rect.IsEmpty(cd.new) THEN Repaint(v, Region.Full) END
    (* Or, as a test for oldomains:
    IF (Rect.HorSize(cd.new) = Rect.HorSize(cd.prev)) AND
       (Rect.VerSize(cd.new) = Rect.VerSize(cd.prev)) AND
       Rect.Equal(cd.prev, cd.saved) AND NOT cd.marked
    THEN
      VBT.Scroll(v, cd.new,
                 Point.Sub(Rect.NorthWest(cd.new), Rect.NorthWest(cd.prev)))
    ELSE
      Repaint(v, Region.FromRect(cd.new))
    END; *)
  END Reshape;

PROCEDURE Rescreen(v: T; READONLY cd: VBT.RescreenRec) RAISES {} =
  BEGIN
    SetAndAlign(v);
    IF cd.marked THEN v.displayingFocus := v.hasFocus END
  END Rescreen;

PROCEDURE Put(v: T; txt: TEXT) RAISES {} =
  BEGIN
    <* ASSERT txt # NIL *>
    LOCK v DO IF v.text = txt THEN RETURN ELSE v.text := txt END END;
    VBT.Mark(v)
  END Put;

PROCEDURE GetFont(v: T): Font.T =
  BEGIN RETURN v.font END GetFont;

PROCEDURE GetQuad(v: T): PaintOp.ColorQuad =
  BEGIN RETURN v.scheme END GetQuad;

PROCEDURE SetFont(
    v: T;
    fnt: Font.T;
    paintScheme : PaintOp.ColorQuad := NIL)
  RAISES {} =
  BEGIN
    v.font := fnt;
    IF paintScheme = NIL THEN paintScheme := PaintOp.bgFg END;
    v.scheme := paintScheme;
    VBT.Mark(v);
  END SetFont;

PROCEDURE Get(v: T): TEXT RAISES {} =
  BEGIN
    LOCK v DO RETURN v.text END
  END Get;

PROCEDURE Redisplay (v: T) RAISES {} =
  BEGIN
    SetAndAlign(v);
    v.displayingFocus := v.hasFocus;
    Repaint(v, Region.Full)
  END Redisplay;

PROCEDURE GetTextRect(v: T): Rect.T RAISES {} =
  BEGIN
    IF VBT.IsMarked(v) THEN SetAndAlign(v) END;
    RETURN v.textRect
  END GetTextRect;

BEGIN END TextVBT.
