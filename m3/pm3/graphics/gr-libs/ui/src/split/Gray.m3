(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Sep 25 15:37:10 PDT 1992 by msm  *)
<*PRAGMA LL*>

MODULE Gray;
IMPORT Pixmap, Point, Rect, ScrnPixmap;

TYPE
  A3 = ARRAY [0..2] OF [0..9];
CONST
  Intense3 = ARRAY [0..2] OF A3 {A3 {7, 9, 5}, 
                                 A3 {2, 1, 4}, 
                                 A3 {6, 3, 8}};

PROCEDURE New3x3(intensity:[0..9]):Pixmap.T =
VAR bounds := Rect.FromSize(3, 3);
    raw := ScrnPixmap.NewRaw(1, bounds);
BEGIN
  FOR h := 0 TO 2 DO
    FOR v := 0 TO 2 DO
      IF intensity >= Intense3[h, v] THEN
        raw.set(Point.FromCoords(h,v), 1);
      ELSE
        raw.set(Point.FromCoords(h,v), 0);
      END;
    END;
  END;
  RETURN Pixmap.FromBitmap(raw);
END New3x3;

TYPE
  A4 = ARRAY [0..3] OF [0..16];
CONST
  Intense4 = ARRAY [0..3] OF A4 {A4 { 1,  9,  3, 11}, 
                                 A4 {13,  5, 15,  7}, 
                                 A4 { 4, 12,  2, 10},
                                 A4 {16,  8, 14,  6}};

PROCEDURE New4x4(intensity:[0..16]):Pixmap.T =
VAR bounds := Rect.FromSize(4, 4);
    raw := ScrnPixmap.NewRaw(1, bounds);
BEGIN
  FOR h := 0 TO 3 DO
    FOR v := 0 TO 3 DO
      IF intensity >= Intense4[h,v] THEN
        raw.set(Point.FromCoords(h,v), 1);
      ELSE
        raw.set(Point.FromCoords(h,v), 0);
      END;
    END;
  END;
  RETURN Pixmap.FromBitmap(raw);
END New4x4;

BEGIN
END Gray.
