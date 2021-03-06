(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Stephen Harrison and Steve Glassman *)
(*                                                                           *)
(* Last modified on Fri Jan 22 17:08:33 PST 1993 by steveg                   *)
(*      modified on Sun Jul 19 11:51:04 PDT 1992 by harrison                 *)

INTERFACE Matrix2D;

(* A Matrix.T is a simplified representation of a true 3 x 3 matrix.  We
   assume the last column is always {0, 0, 1} so we do not explicitly hold
   it.

   Here is the layout of the elements of our matrix.  A number is the
   corresponding index into the array, `x' means the element is not held
   explicitly.

|       Column
|      0   1   2
|    +---+---+---+
|   0| 0 | 1 | x |
| R  +---+---+---+
| o 1| 2 | 3 | x |
| w  +---+---+---+
|   2| 4 | 5 | x |
|    +---+---+---+

   *)

IMPORT R2;

TYPE T = ARRAY [0 .. 5] OF REAL;

CONST
  Identity = T{1.0, 0.0,        (* 0.0 *)
               0.0, 1.0,        (* 0.0 *)
               0.0, 0.0};       (* 1.0 *)

PROCEDURE Scale(READONLY sx, sy: REAL): T;
PROCEDURE Translate(READONLY tx, ty: REAL): T;
PROCEDURE Rotate(READONLY radians: REAL): T;
(* positive radians are clockwise *)

PROCEDURE Concat(READONLY m, n: T): T;
PROCEDURE Concat3(READONLY l, m, n: T): T;

PROCEDURE Inverse (READONLY m: T): T;

PROCEDURE Transform(READONLY m: T; READONLY p: R2.T): R2.T;

END Matrix2D.

