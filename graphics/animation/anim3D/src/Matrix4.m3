(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug 27 16:06:56 PDT 1996 by najork                   *)
(*       Created on Fri Mar 18 12:05:28 PST 1994 by najork                   *)


MODULE Matrix4;

IMPORT Math, Mth, Point3;


PROCEDURE Identity () : T =
  BEGIN
    RETURN Id;
  END Identity;


(***
PROCEDURE Translate (READONLY M : T; x, y, z : REAL) : T =
  VAR 
    N := T {Row {1.0, 0.0, 0.0, x},
            Row {0.0, 1.0, 0.0, y},
            Row {0.0, 0.0, 1.0, z},
            Row {0.0, 0.0, 0.0, 1.0}};
  BEGIN
    RETURN Multiply (N, M);
  END Translate;
***)


PROCEDURE Translate (READONLY M : T; x, y, z : REAL) : T =
   BEGIN
(*
     <* ASSERT M[3][0] = 0.0 *>
     <* ASSERT M[3][1] = 0.0 *>
     <* ASSERT M[3][2] = 0.0 *>
     <* ASSERT M[3][3] = 1.0 *>
*)
     RETURN T {Row{M[0][0], M[0][1], M[0][2], M[0][3] + x},
               Row{M[1][0], M[1][1], M[1][2], M[1][3] + y},
               Row{M[2][0], M[2][1], M[2][2], M[2][3] + z},
               Row{0.0, 0.0, 0.0, 1.0}};
  END Translate;


PROCEDURE Scale (READONLY M : T; x, y, z : REAL) : T =
  VAR 
    N := T {Row {  x, 0.0, 0.0, 0.0},
            Row {0.0,   y, 0.0, 0.0},
            Row {0.0, 0.0,   z, 0.0},
            Row {0.0, 0.0, 0.0, 1.0}};
  BEGIN
    RETURN Multiply (N, M);
  END Scale;


PROCEDURE RotateX (READONLY M : T; theta : REAL) : T =
  VAR 
    a := Mth.sin (theta);
    b := Mth.cos (theta);
    N := T {Row {1.0, 0.0, 0.0, 0.0},
            Row {0.0,   b,  -a, 0.0},
            Row {0.0,   a,   b, 0.0},
            Row {0.0, 0.0, 0.0, 1.0}};
  BEGIN
    RETURN Multiply (N, M);
  END RotateX;


PROCEDURE RotateY (READONLY M : T; theta : REAL) : T =
  VAR 
    a := Mth.sin (theta);
    b := Mth.cos (theta);
    N := T {Row {  b, 0.0,   a, 0.0},
            Row {0.0, 1.0, 0.0, 0.0},
            Row { -a, 0.0,   b, 0.0},
            Row {0.0, 0.0, 0.0, 1.0}};
  BEGIN
    RETURN Multiply (N, M);
  END RotateY;

  
PROCEDURE RotateZ (READONLY M : T; theta : REAL) : T =
  VAR 
    a := Mth.sin (theta);
    b := Mth.cos (theta);
    N := T {Row {  b,  -a, 0.0, 0.0},
            Row {  a,   b, 0.0, 0.0},
            Row {0.0, 0.0, 1.0, 0.0},
            Row {0.0, 0.0, 0.0, 1.0}};
  BEGIN
    RETURN Multiply (N, M);
  END RotateZ;


(**** 
PROCEDURE TransformPoint3 (READONLY M : T; READONLY p : Point3.T) : Point3.T =
  BEGIN
    RETURN Point3.T {M[0][0] * p.x + M[0][1] * p.y + M[0][2] * p.z + M[0][3],
                     M[1][0] * p.x + M[1][1] * p.y + M[1][2] * p.z + M[1][3],
                     M[2][0] * p.x + M[2][1] * p.y + M[2][2] * p.z + M[2][3]};
  END TransformPoint3;
***)


PROCEDURE TransformPoint3 (READONLY M : T; READONLY p : Point3.T) : Point3.T =
  BEGIN
    WITH w = M[3][0] * p.x + M[3][1] * p.y + M[3][2] * p.z + M[3][3] DO
      RETURN Point3.T {
                (M[0][0] * p.x + M[0][1] * p.y + M[0][2] * p.z + M[0][3]) / w,
                (M[1][0] * p.x + M[1][1] * p.y + M[1][2] * p.z + M[1][3]) / w,
                (M[2][0] * p.x + M[2][1] * p.y + M[2][2] * p.z + M[2][3]) / w};
    END;
  END TransformPoint3;


PROCEDURE Multiply (READONLY M, N : T) : T =
  VAR
    P : T;
  BEGIN
    FOR i := 0 TO 3 DO
      FOR j := 0 TO 3 DO
        P[i][j] := 0.0;
        FOR k := 0 TO 3 DO
          P[i][j] := P[i][j] + M[i][k] * N[k][j];
        END;
      END;
    END;
    RETURN P;
  END Multiply;


(* "Invert" is stolen from "Cube", a 3D Programming language that I did back in
   in Illinois.  Cube was implemented in Modula-3. *)


PROCEDURE Invert (<*NOWARN*> A : T) : T RAISES {Error} =

  VAR
    p : ARRAY [0 .. 3] OF INTEGER;
    B : T;

  (* 
   * LUP_Solve is taken pretty directly from 
   * [Cormen, Leiserson, Rivest, p. 753]
   *)
  PROCEDURE LUP_Solve (READONLY b : Row) : Row =
    VAR
      x : Row;
      y : Row;
    BEGIN
      FOR i := 0 TO 3 DO
        y[i] := b[p[i]];
        FOR j := 0 TO i-1 DO 
          y[i] := y[i] - A[i][j] * y[j];
        END;
      END;
      FOR i := 3 TO 0 BY -1 DO 
        x[i] := y[i];
        FOR j := i+1 TO 3 DO 
          x[i] := x[i] - A[i][j] * x[j];
        END;
        x[i] := x[i] / A[i][i];
      END;
      RETURN x;
    END LUP_Solve;

  (*
   * LUP_Decomposition is taken pretty directly from
   * [Cormen, Leiserson, Rivest, p. 759]
   *)
  PROCEDURE LUP_Decomposition () RAISES {Error} =
    VAR
      q : REAL;
      g : INTEGER;
    BEGIN
      FOR i := 0 TO 3 DO
        p[i] := i;
      END;
      FOR k := 0 TO 2 DO
        q := 0.0;
        FOR i := k TO 3 DO
          IF ABS(A[i][k]) > q THEN
            q := ABS(A[i][k]);
            g := i;
          END;
        END;
        IF q = 0.0 THEN
          (* Transformation matrix is singular *)
          RAISE Error;
        END;
        VAR t := p[k]; BEGIN p[k] := p[g]; p[g] := t; END; (* swap *)
        FOR i := 0 TO 3 DO
          VAR t := A[k][i]; BEGIN A[k][i] := A[g][i]; A[g][i] := t; END;
        END;
        FOR i := k+1 TO 3 DO
          A[i][k] := A[i][k] / A[k][k];
          FOR j := k+1 TO 3 DO
            A[i][j] := A[i][j] - A[i][k] * A[k][j];
          END;
        END;
      END;
    END LUP_Decomposition;

  (*
   * The main part of the inversion routine is taken pretty directly from
   * [Cormen, Leiserson, Rivest, p. 762-763]
   *)
  BEGIN
    LUP_Decomposition ();
    WITH v0 = LUP_Solve (Row {1.0, 0.0, 0.0, 0.0}), 
         v1 = LUP_Solve (Row {0.0, 1.0, 0.0, 0.0}),
         v2 = LUP_Solve (Row {0.0, 0.0, 1.0, 0.0}),
         v3 = LUP_Solve (Row {0.0, 0.0, 0.0, 1.0}) DO
      FOR i := 0 TO 3 DO
        B[i] := Row {v0[i], v1[i], v2[i], v3[i]};
      END;
    END;
    RETURN B;
  END Invert;


(* 
   In the old version of "TransformUnitCube", I computed the result matrix "M" 
   by using trigonometric functions (and I would be very embarrassed to tell 
   just how long it took me to get this function right). This approach was 
   of course motivated by the geometric interpretation on the function 
   (projecting the unit cube through scaling, rotations, and translation 
   onto the cube with corners "p0","a0","b0","c0"). 
*)

(*
PROCEDURE TransformUnitCube (p0, a0, b0, c0 : Point3.T) : T =
  VAR
    p, a, b, c : Point3.T;
    M     : T;
    N     : T;
    sx, sy, sz : REAL;
    angle1, angle2, angle3 : REAL;
  BEGIN
    M := Identity ();
    M := Translate (M, -p0.x, -p0.y, -p0.z);
    p := TransformPoint3 (M, p0);
    a := TransformPoint3 (M, a0);
    b := TransformPoint3 (M, b0);
    c := TransformPoint3 (M, c0);

    M := Identity ();
    (* We want to rotate vector "a" around the y axis such that it falls into 
       the x-y plane. So, we need to find the angle "angle1" between the
       projection of "a" onto the x-z plane and the x axis. *)
    IF a.z = 0.0 THEN
      (* If "a.z" = 0, then "a" is already in the x-y plane. *)
      angle1 := 0.0;
    ELSE 
      (* a.z # 0, hence Length ( (a.x, 0, a.z) ) > 0 *)
      angle1 := Mth.asin (a.z / Point3.Length (Point3.T {a.x, 0.0, a.z}));
    END;
    IF a.x < 0.0 THEN
      angle1 := Math.Pi - angle1;
    END;
    M := RotateY (M, angle1);
    p := TransformPoint3 (M, p);
    a := TransformPoint3 (M, a);
    b := TransformPoint3 (M, b);
    c := TransformPoint3 (M, c);

    M := Identity ();
    (* We want to rotate vector "a" around the z axis such that it falls onto 
       the x axis. So, we need to find the angle "angle2" between "a" and the
       x axis. Note that the previous rotation moved "a" into the x-y plane,
       hence "a.z" is 0, hence we do not need to project "a" onto any plane.
       Also, note that "a" is guaranteed to have a positive length. *)
    angle2 := - Mth.asin (a.y / Point3.Length (a));
    IF a.x < 0.0 THEN
      angle2 := Math.Pi - angle2;
    END;
    M := RotateZ (M, angle2);
    p := TransformPoint3 (M, p);
    a := TransformPoint3 (M, a);
    b := TransformPoint3 (M, b);
    c := TransformPoint3 (M, c);

    M := Identity ();
    (* At this point, "a" should be lying on the positive half of x axis, 
       and "b" and "c" should both be lying in the y-z plane. We want to 
       rotate "b" around the x axis so that it lies on the positive half 
       of the y axis. *)
    angle3 := -Mth.asin (b.z / Point3.Length (b));
    IF b.y < 0.0 THEN
      angle3 := Math.Pi - angle3;
    END;
    M := RotateX (M, angle3);
    p := TransformPoint3 (M, p);
    a := TransformPoint3 (M, a);
    b := TransformPoint3 (M, b);
    c := TransformPoint3 (M, c);

    sx := Point3.Length (a);
    sy := Point3.Length (b);
    sz := Point3.Length (c);

    (* Construct N *)
    N := Identity ();
    N := Scale (N, sx, sy, sz);
    N := RotateX (N, -angle3);
    N := RotateZ (N, -angle2);
    N := RotateY (N, -angle1);
    N := Translate (N, p0.x, p0.y, p0.z);

    RETURN N;
  END TransformUnitCube;
*)

PROCEDURE TransformUnitCube (p, a, b, c : Point3.T) : T =
  BEGIN
    RETURN T {Row {a.x - p.x, b.x - p.x, c.x - p.x, p.x},
              Row {a.y - p.y, b.y - p.y, c.y - p.y, p.y},
              Row {a.z - p.z, b.z - p.z, c.z - p.z, p.z},
              Row {      0.0,       0.0,       0.0, 1.0}};
  END TransformUnitCube;


PROCEDURE UnitSphereMaxSquishFactor (READONLY M : T) : REAL =

  (* Given a vector v, DecomposeVector returns a unit vector u parallel to v
     and the length l of v. In other words, u = Point3.Scale (v, 1.0) and 
     l = Point3.Length (v). *)

  PROCEDURE Iterate (READONLY AAt : T;
                     v            : Point3.T; 
                     VAR u        : Point3.T; 
                     VAR l        : REAL) =
    BEGIN
      v := TransformPoint3 (AAt, v);
      l := Mth.sqrt (v.x * v.x + v.y * v.y + v.z * v.z);
      u := Point3.T {v.x / l, v.y / l, v.z / l};
    END Iterate;

  CONST
    eps = 0.05;
  VAR 
    A, At, AAt    : T;
    v1, v2, v3, v : Point3.T;
    s1, s2, s3, s : REAL;
    delta         : REAL;
    s_prev        : REAL;
  BEGIN
    A := T {Row {M[0][0], M[0][1], M[0][2], 0.0},
            Row {M[1][0], M[1][1], M[1][2], 0.0},
            Row {M[2][0], M[2][1], M[2][2], 0.0},
            Row {    0.0,     0.0,     0.0, 1.0}};
    At := T {Row {M[0][0], M[1][0], M[2][0], 0.0},
             Row {M[0][1], M[1][1], M[2][1], 0.0},
             Row {M[0][2], M[1][2], M[2][2], 0.0},
             Row {    0.0,     0.0,     0.0, 1.0}};
    AAt := Multiply (A, At);

    (*** start with 3 mutually orthogonal unit vectors ***)
    Iterate (AAt, Point3.T {1.0, 0.0, 0.0}, v1, s1);
    Iterate (AAt, Point3.T {0.0, 1.0, 0.0}, v2, s2);
    Iterate (AAt, Point3.T {0.0, 0.0, 1.0}, v3, s3);

    (*** decide which one yielded the largest scaling ***)
    IF s1 >= s2 AND s1 >= s3 THEN
      v := v1;
      s_prev := s1;
    ELSIF s2 >= s1 AND s2 >= s3 THEN
      v := v2;
      s_prev := s2;
    ELSIF s3 >= s1 AND s3 >= s2 THEN
      v := v3;
      s_prev := s3;
    ELSE
      <* ASSERT FALSE *>
    END;

    (*** Iterate until the scale factor approaches a fixed point ***)
    REPEAT
      Iterate (AAt, v, v, s);
      delta := ABS (s / s_prev) - 1.0;
      s_prev := s;
    UNTIL delta < eps;

    RETURN Mth.sqrt (s);
  END UnitSphereMaxSquishFactor;



(* Basic Assertions:
   (1) M has been created by combining rotations, translations, and
       uniform(!) scalings.
   (2) s > 0
*)
(*
PROCEDURE Decompose ((* in *)  <*NOWARN*> M : T; 
                     (* out *) VAR tx, ty, tz, s, angX, angY, angZ : REAL) =
  VAR
    a, b, c: Point3.T;
  BEGIN
    <* ASSERT M[3][0] = 0.0 *>
    <* ASSERT M[3][1] = 0.0 *>
    <* ASSERT M[3][2] = 0.0 *>
    <* ASSERT M[3][3] = 1.0 *>

    (* separate the translation component *)
    tx := M[0][3];
    ty := M[1][3];
    tz := M[2][3];

    (* remove the translation component from M *)
    M[0][3] := 0.0;
    M[1][3] := 0.0;
    M[2][3] := 0.0;

    (* We assumed uniform scaling, which makes it very easy to determine s. *)
    WITH p0 = Point3.T {1.0, 0.0, 0.0},
         p1 = TransformPoint3 (M, p0) DO
      s := Point3.Length (p1);
    END;

    (* Also, for a uniform scaling S, SM = MS for any matrix M. 
       So, we can remove S easily. *)
    FOR i := 0 TO 2 DO
      FOR j := 0 TO 2 DO
        M[i][j] := M[i][j] / s;
      END;
    END;

    (* Take three orthogonal unit vectors *)
    a := Point3.T {1.0, 0.0, 0.0};
    b := Point3.T {0.0, 1.0, 0.0};
    c := Point3.T {0.0, 0.0, 1.0};

    (* Apply M to them *)
    a := TransformPoint3 (M, a);
    b := TransformPoint3 (M, b);
    c := TransformPoint3 (M, c);

    (* We want to rotate vector "a" around the z axis such that it falls into 
       the x-z plane. So, we need to find the angle "angZ" between the
       projection of "a" onto the x-y plane and the z axis. *)
    IF a.y = 0.0 THEN
      (* If "a.y" = 0, then "a" is already in the x-z plane. *)
      angZ := 0.0;
    ELSE 
      (* a.y # 0, hence Length ( (a.x, 0, a.y) ) > 0 *)
      angZ := - Mth.asin (a.y / Point3.Length (Point3.T {a.x, a.y, 0.0}));
    END;
    IF a.x < 0.0 THEN
      angZ := Math.Pi - angZ;
    END;
    WITH N = RotateZ (Id, angZ) DO
      a := TransformPoint3 (N, a);
      b := TransformPoint3 (N, b);
      c := TransformPoint3 (N, c);
    END;

    (* We want to rotate vector "a" around the y axis such that it falls onto 
       the x axis. So, we need to find the angle "angY" between "a" and the
       x axis. Note that the previous rotation moved "a" into the x-z plane,
       hence "a.y" is 0, hence we do not need to project "a" onto any plane.
       Also, note that "a" is guaranteed to have a positive length. *)
    angY := Mth.asin (a.z / Point3.Length (a));
    IF a.x < 0.0 THEN
      angY := Math.Pi - angY;
    END;
    WITH N = RotateY (Id, angY) DO
      a := TransformPoint3 (N, a);
      b := TransformPoint3 (N, b);
      c := TransformPoint3 (N, c);
    END;

    (* At this point, "a" should be lying on the positive half of x axis, 
       and "b" and "c" should both be lying in the y-z plane. We want to 
       rotate "b" around the x axis so that it lies on the positive half 
       of the y axis. *)
    angX := - Mth.asin (b.z / Point3.Length (b));
    IF b.y < 0.0 THEN
      angX := Math.Pi - angX;
    END;
    WITH N = RotateX (Id, angX) DO
      a := TransformPoint3 (N, a);
      b := TransformPoint3 (N, b);
      c := TransformPoint3 (N, c);
    END;

    angX := - angX;
    angY := - angY;
    angZ := - angZ;
  END Decompose;
*)

(* Basic Assertions:
   (1) M has been created by combining rotations, translations, and
       uniform(!) scalings.
   (2) s > 0
*)
PROCEDURE Decomp (<*NOWARN*> M : T; 
                  VAR tx, ty, tz, s : REAL) : T RAISES {Error} =
  BEGIN
    <* ASSERT M[3][0] = 0.0 *>
    <* ASSERT M[3][1] = 0.0 *>
    <* ASSERT M[3][2] = 0.0 *>
    <* ASSERT M[3][3] = 1.0 *>

    (* separate the translation component *)
    tx := M[0][3];
    ty := M[1][3];
    tz := M[2][3];

    (* remove the translation component from M *)
    M[0][3] := 0.0;
    M[1][3] := 0.0;
    M[2][3] := 0.0;

    (* We assumed uniform scaling, which makes it very easy to determine s. *)
    WITH p0 = Point3.T {1.0, 0.0, 0.0},
         p1 = TransformPoint3 (M, p0) DO
      s := Point3.Length (p1);
    END;

    (* Also, for a uniform scaling S, SM = MS for any matrix M. 
       So, we can remove S easily. *)
    FOR i := 0 TO 2 DO
      FOR j := 0 TO 2 DO
        M[i][j] := M[i][j] / s;
      END;
    END;

    IF NOT Orthonormal (M) THEN
      RAISE Error;
    ELSE
      RETURN M;
    END;
  END Decomp;


PROCEDURE Transpose (READONLY M : T) : T =
  BEGIN
    RETURN T {Row {M[0][0], M[1][0], M[2][0], M[3][0]},
              Row {M[0][1], M[1][1], M[2][1], M[3][1]},
              Row {M[0][2], M[1][2], M[2][2], M[3][2]},
              Row {M[0][3], M[1][3], M[2][3], M[3][3]}};
  END Transpose;


PROCEDURE Equal (READONLY A, B : T) : BOOLEAN =
  CONST
    eps = 0.0005;
  BEGIN
    FOR i := 0 TO 3 DO
      FOR j := 0 TO 3 DO
        IF ABS (A[i][j] - B[i][j]) > eps THEN
          RETURN FALSE;
        END;
      END;
    END;
    RETURN TRUE;
  END Equal;


PROCEDURE Orthonormal (READONLY U : T) : BOOLEAN =

  PROCEDURE DotProduct (u, v : Row) : REAL =
    BEGIN
      RETURN u[0]*v[0] + u[1]*v[1] + u[2]*v[2] + u[3]*v[3];
    END DotProduct;

  PROCEDURE Zero (x : REAL) : BOOLEAN =
    BEGIN
      RETURN ABS (x) < 0.0001;
    END Zero;

  PROCEDURE One (x : REAL) : BOOLEAN =
    BEGIN
      RETURN Zero (x - 1.0);
    END One;

  BEGIN
    WITH Ut = Transpose (U),
         u0 = SUBARRAY (Ut[0], 0, 4),
         u1 = SUBARRAY (Ut[1], 0, 4),
         u2 = SUBARRAY (Ut[2], 0, 4),
         u3 = SUBARRAY (Ut[3], 0, 4),
         d00 = DotProduct (u0, u0),
         d01 = DotProduct (u0, u1),
         d02 = DotProduct (u0, u2),
         d03 = DotProduct (u0, u3),
         d11 = DotProduct (u1, u1),
         d12 = DotProduct (u1, u2),
         d13 = DotProduct (u1, u3),
         d22 = DotProduct (u2, u2),
         d23 = DotProduct (u2, u3),
         d33 = DotProduct (u3, u3) DO
      RETURN One (d00) AND One (d11) AND One (d22) AND One (d33) AND
             Zero (d01) AND Zero (d02) AND Zero (d03) AND 
             Zero (d12) AND Zero (d13) AND Zero (d23);
    END;
  END Orthonormal;
      

PROCEDURE OrthoProjMatrix (height, aspect, near, far: REAL): T =
  VAR
    width := height * aspect;
    depth := near - far;
  BEGIN
    <* ASSERT depth # 0.0 AND width # 0.0 AND height # 0.0 *>
    RETURN T {Row{1.0 / width, 0.0,          0.0,         0.5},
              Row{0.0,         1.0 / height, 0.0,         0.5},
              Row{0.0,         0.0,          1.0 / depth, 1.0 - near / depth},
              Row{0.0,         0.0,          0.0,         1.0}};
  END OrthoProjMatrix;


PROCEDURE PerspProjMatrix (fovy, distance, aspect, near, far: REAL): T =
  VAR
    eyedist := distance - near;
    depth   := near - far;
    frac    := 1.0 + near / eyedist;
    fovy2   := MIN (ABS(fovy), Math.Pi) / 2.0;
    height  := 2.0 * eyedist * FLOAT (Math.tan (FLOAT (fovy2, LONGREAL)));
    width   := height * aspect;
  BEGIN
    <* ASSERT near > far AND fovy # 0.0 AND aspect # 0.0 AND distance > near *>
    RETURN T {Row {1.0 / width, 0.0,          -0.5 / eyedist,  frac / 2.0},
              Row {0.0,         1.0 / height, -0.5 / eyedist,  frac / 2.0},
              Row {0.0,         0.0,           1.0 / depth,   -far / depth},
              Row {0.0,         0.0,          -1.0 / eyedist,  frac}};
  END PerspProjMatrix;


(* The matrix returned by this function can be thought of as having 2 parts.  
   The first part (next to the coordinate point when it is being transformed) 
   moves the "to" point to the origin.  The second part performs the rotation 
   of the data.

   The three basis vectors of the rotation are obtained as follows:

   - The Z basis vector "b" is determined by subtracting "from" from "to" and
     normalizing it to unit length.  
   - The Y basis vector "e" is determined by calculating the vector 
     perpendicular to "b" and in the plane defined by the Z basis vector
     and the "up" vector and then normalizing it.  
   - The X basis vector "f" is calculated by "e CROSS b".

   This method is called the "Gram-Schmidt process". 
   See Foley/van Dam/Feiner/Hughes pages 1102f and 1112 for details.

   The resulting matrix looks like this:

	| fx fy fz 0 | | 1 0 0 -tox | | fx fy fz -to DOT f |
	| ex ey ez 0 |*| 0 1 0 -toy |=| ex ey ez -to DOT e |
	| bx by bz 0 | | 0 0 1 -toz | | bx by bz -to DOT b |
	|  0  0  0 1 | | 0 0 0   1  | |  0  0  0      1    |
*)

PROCEDURE LookatViewMatrix (from, to, up: Point3.T): T =
  BEGIN
    WITH b  = Point3.ScaleToLen (Point3.Minus (from, to), 1.0),
         e  = Point3.ScaleToLen (Point3.Minus (up, 
                   Point3.TimesScalar (b, Point3.DotProduct (b, up))), 1.0),
         f  = Point3.CrossProduct (e, b) DO
      RETURN T {Row {f.x, f.y, f.z, -Point3.DotProduct (to, f)},
                Row {e.x, e.y, e.z, -Point3.DotProduct (to, e)},
                Row {b.x, b.y, b.z, -Point3.DotProduct (to, b)},
                Row {0.0, 0.0, 0.0, 1.0}};
    END;
  END LookatViewMatrix;

BEGIN
END Matrix4.
