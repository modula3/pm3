(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun  2 16:53:23 PDT 1993 by meehan   *)
(*      modified on Sat Jan 30 00:12:39 PST 1993 by mhb      *)
(*      modified on Tue Jun 16 13:08:49 PDT 1992 by muller   *)
(*      modified on Fri Jul 26 15:34:03 PDT 1991 by steveg   *)

MODULE FlexVBT;

IMPORT Axis, Filter, FilterClass, VBT, VBTClass;

REVEAL
  T = Public BRANDED OBJECT
        sh: Shape;
      OVERRIDES
        init     := Init;
        shape    := DoShape;
        rescreen := Rescreen
      END;

PROCEDURE Init (v: T; ch: VBT.T; READONLY sh := Default): T =
  BEGIN
    EVAL Filter.T.init (v, ch);
    v.sh := sh;
    RETURN v
  END Init;

PROCEDURE FromAxis (         ch: VBT.T;
                             ax: Axis.T;
                    READONLY sr: SizeRange := DefaultRange): T =
  VAR sh: Shape;
  BEGIN
    sh[ax] := sr;
    sh[Axis.Other[ax]] := DefaultRange;
    RETURN NEW(T).init(ch, sh);
  END FromAxis;

PROCEDURE Set (v: T; READONLY sh: Shape) =
  BEGIN
    v.sh := sh;
    VBT.NewShape(v);
  END Set;

PROCEDURE Get (v: T): Shape =
  BEGIN
    RETURN v.sh;
  END Get;

PROCEDURE SetRange (v: T; ax: Axis.T; READONLY sr: SizeRange) =
  BEGIN
    v.sh[ax] := sr;
    VBT.NewShape(v);
  END SetRange;

PROCEDURE DoShape (v: T; ax: Axis.T; n: CARDINAL):
  VBT.SizeRange =
  VAR range, chRange: VBT.SizeRange;
  BEGIN
    IF AnyMissing(v.sh[ax]) THEN
      chRange := VBTClass.GetShape(v.ch, ax, n);
      IF AllMissing(v.sh[ax]) THEN RETURN chRange END
    END;
    WITH sh = v.sh[ax] DO
      IF sh.natural = Missing THEN
        range.pref := chRange.pref
      ELSE
        range.pref := ROUND(VBT.MMToPixels(v, sh.natural, ax))
      END;
      IF sh.shrink = Missing THEN
        IF sh.natural # Missing THEN
          range.lo := range.pref
        ELSE
          range.lo := MIN(chRange.lo, range.pref)
        END;
      ELSE
        range.lo :=
          MAX(0, range.pref
                   - ROUND(VBT.MMToPixels(v, sh.shrink, ax)))
      END;
      IF sh.stretch = Missing THEN
        IF sh.natural # Missing THEN
          range.hi := range.pref + 1
        ELSE
          range.hi := MAX(chRange.hi, range.pref + 1)
        END;
      ELSE
        range.hi :=
          MIN(MAX(range.pref + 1, VBT.DefaultShape.hi),
              range.pref + 1
                + ROUND(VBT.MMToPixels(v, sh.stretch, ax)))
      END
    END;
    RETURN range
  END DoShape;

PROCEDURE Rescreen (v: T; READONLY cd: VBT.RescreenRec) =
  BEGIN
    Filter.T.rescreen (v, cd);
    VBT.NewShape (v);
  END Rescreen;

PROCEDURE AllMissing (READONLY range: SizeRange): BOOLEAN =
  BEGIN
    RETURN (range.natural = Missing) AND 
           (range.shrink  = Missing) AND 
           (range.stretch = Missing);
  END AllMissing;

PROCEDURE AnyMissing (READONLY range: SizeRange): BOOLEAN =
  BEGIN
    RETURN (range.natural = Missing) OR 
           (range.shrink  = Missing) OR 
           (range.stretch = Missing);
  END AnyMissing;

PROCEDURE RigidRange (natural: REAL): SizeRange =
  BEGIN
    RETURN SizeRange{natural, 0.0, 0.0}
  END RigidRange;

PROCEDURE Rigid (hNat, vNat: REAL): Shape =
  BEGIN
    RETURN
      Shape{SizeRange{hNat, 0.0, 0.0}, SizeRange{vNat, 0.0, 0.0}}
  END Rigid;

BEGIN
END FlexVBT.

