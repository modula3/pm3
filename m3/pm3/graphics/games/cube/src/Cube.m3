(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Oct  1 21:50:09 PDT 1992 by mhb    *)
(*      modified on Tue Jun 16 18:30:06 PDT 1992 by muller *)
(*      modified on Wed Apr  8  4:34:27 PDT 1992 by sclafani *)
(*      modified on Mon Mar 13 01:55:02     1989 by stolfi   *)
(* Created by Marc H. Brown before Mar 1988                  *)

MODULE Cube;

IMPORT Math, R4, R4x4;
IMPORT PaintOp, Path, Point, Rect, Region, ScrnPixmap, Trestle, TrestleComm,
  VBT;

CONST
  N = 7;

TYPE
  Object = ARRAY [0..N] OF R4.T;
  CRM = R4x4.T;

TYPE
  Face = {North, South, East, West, Front, Back};
  FaceList = ARRAY Face OF Face;
  Quad = ARRAY [0..3] OF INTEGER; (* which pts in obj make up qad *)
  Style = {Wireframe, Solid};

VAR
  colors: ARRAY Face OF PaintOp.T;

REVEAL 
  T = BRANDED REF RECORD
    mu:           MUTEX;
    vbt:          VBT.T;
    style:        Style;   (* wireframe or solid *)
    doubleBuffer: BOOLEAN; (* yes or no? *)
    degPerNotch:  INTEGER; (* how much to go each time *)
    deg:          INTEGER; (* how far we've gone cumulative *)
    spinCRM:      CRM;     (* advances the object one notch *)
    viewCRM:      CRM;     (* convert wc to view coords *)
    projCRM:      CRM;     (* does projection: ortho->ID; persp=>1/d *)
    imageCRM:     CRM;     (* convert from screen in view plane to VBT *)
    crm:          CRM;     (* imageCRM * projCRM * viewCRM *)
    obj:          Object;  (* in world coords *)
    framePath:    Path.T;
    facePath:     Path.T;
    offscreen:    VBT.Leaf;
  END;

CONST
  X = 0; (* 1st column *)
  Y = 1; (* 2nd column *)
  Z = 2; (* 3rd column *)
  W = 3; (* 4th column *)


PROCEDURE New (vbt: VBT.T): T =
  VAR cube: T;
  BEGIN
    Initialize ();
    cube := NEW (T);
    cube.mu := NEW (MUTEX);
    cube.vbt := vbt;
    InitCRM (cube.spinCRM);
    InitCRM (cube.viewCRM);
    InitCRM (cube.projCRM);
    InitCRM (cube.imageCRM);
    InitCRM (cube.crm);
    InitObject (cube.obj);
    cube.deg := 0;
    cube.degPerNotch := 0;
    cube.framePath := NEW (Path.T);
    cube.facePath  := NEW (Path.T);
    cube.offscreen := NIL;
    RETURN cube;
  END New;

PROCEDURE SetStyle (cube: T; style: INTEGER) =
  BEGIN
    LOCK cube.mu DO
      IF style = 0 THEN
        cube.style := Style.Wireframe
      ELSE
        cube.style := Style.Solid
      END;
    END;
  END SetStyle;

PROCEDURE SetSpin (cube: T; degree: INTEGER) =
  CONST Theta = 35.2633; (* sin(@) = 1/sqrt(3) *)
  BEGIN
    LOCK cube.mu DO
      cube.deg := 0;
      cube.degPerNotch := degree;
      InitObject (cube.obj);
      InitCRM (cube.spinCRM);
      TranslateCRM (cube.spinCRM, 1.0, 1.0, 1.0);
      YRotateCRM (cube.spinCRM, 45.0); (* diag above x-axis *)
      ZRotateCRM (cube.spinCRM, -Theta); (* diag now coincides with x-axis *)
      XRotateCRM (cube.spinCRM, FLOAT (degree));
      ZRotateCRM (cube.spinCRM, Theta);
      YRotateCRM (cube.spinCRM, -45.0);
      TranslateCRM (cube.spinCRM, -1.0, -1.0, -1.0);
    END;
  END SetSpin;

PROCEDURE SetView (cube: T; mu, theta, phi: REAL) =
  VAR csTheta, snTheta, csPhi, snPhi, x, y, z: REAL;
  BEGIN
    LOCK cube.mu DO
      InitCRM (cube.viewCRM);
      csTheta := FLOAT (Math.cos (DegToRadians (theta)));
      snTheta := FLOAT (Math.sin (DegToRadians (theta)));
      csPhi   := FLOAT (Math.cos (DegToRadians (phi)));
      snPhi   := FLOAT (Math.sin (DegToRadians (phi)));
      x       := mu * csTheta * snPhi;
      y       := -mu * snTheta;
      z       := mu * csTheta * csPhi;
      TranslateCRM (cube.viewCRM, -x, -y, -z);
      YRotateCRM (cube.viewCRM, -phi);
      XRotateCRM (cube.viewCRM, -theta);
      ScaleCRM (cube.viewCRM, 1.0, 1.0, -1.0);
    END;
  END SetView;

PROCEDURE SetProjection (cube: T; persp: BOOLEAN; d: REAL) =
  BEGIN
    LOCK cube.mu DO
      InitCRM (cube.projCRM);
      IF persp THEN PerspectiveCRM (cube.projCRM, d); END;
    END;
  END SetProjection;

<* FATAL TrestleComm.Failure *>

PROCEDURE SetImage (cube: T; dblBuffer: BOOLEAN; w: REAL) =
  VAR dx, dy, rx, lx, by, ty: REAL; dom: Rect.T;
  BEGIN
    LOCK cube.mu DO
      cube.doubleBuffer := dblBuffer;
      IF cube.offscreen # NIL THEN
        Trestle.Delete(cube.offscreen)
      ELSIF dblBuffer THEN
        cube.offscreen := NEW(VBT.Leaf);
      END;
      dom := VBT.Domain (cube.vbt);
      IF dblBuffer THEN
        VAR
          trsl := Trestle.ScreenOf(cube.vbt, Point.Origin).trsl;
          st := VBT.ScreenTypeOf(cube.vbt);
        BEGIN
          IF trsl # NIL AND st # NIL THEN
            Trestle.Attach(cube.offscreen, trsl);
            Trestle.InstallOffscreen(
              cube.offscreen, dom.east - dom.west, dom.south - dom.north,
              st);
            dom := VBT.Domain(cube.offscreen)
          END;
        END;
      END;
      InitCRM (cube.imageCRM);
      IF Rect.IsEmpty (dom) THEN RETURN END;
      lx := FLOAT (dom.west);
      rx := FLOAT (dom.east);
      dx := rx - lx;
      by := FLOAT (dom.south);
      ty := FLOAT (dom.north);
      dy := ty - by;
      TranslateCRM (cube.imageCRM, w, w, 0.0);
      ScaleCRM (cube.imageCRM, dx / (2.0 * w), dy / (2.0 * w), 1.0);
      TranslateCRM (cube.imageCRM, lx, by, 0.0);
    END;
  END SetImage;

PROCEDURE Advance (cube: T) =
  BEGIN
    LOCK cube.mu DO
      INC (cube.deg, cube.degPerNotch);
      IF cube.deg < 360 THEN
        MapObject (cube.spinCRM, cube.obj)
      ELSE
        cube.deg := 0;
        InitObject (cube.obj);
      END;
    END;
  END Advance;

PROCEDURE Display (cube: T) =
  BEGIN
    LOCK cube.mu DO
      CASE cube.style OF
        | Style.Wireframe => DisplayWireFrame (cube)
        | Style.Solid =>     DisplaySolid (cube)
      END;
    END;
  END Display;

PROCEDURE DisplayWireFrame (cube: T) =
  VAR obj: Object; vbt: VBT.T;
  BEGIN
    IF Rect.IsEmpty (VBT.Domain (cube.vbt)) THEN RETURN END;
    obj := cube.obj;
    MapObject (cube.viewCRM, obj);
    MapObject (cube.projCRM, obj);
    HomogenizeObject (obj);
    MapObject (cube.imageCRM, obj);
    BuildPath (cube.framePath, obj);
    vbt := UseHiddenBuffer (cube);
    VBT.Stroke (vbt, Rect.Full, cube.framePath, 1, VBT.EndStyle.Round,
                VBT.JoinStyle.Miter);
    CopyToVisibleBuffer (cube);
  END DisplayWireFrame;

PROCEDURE DisplaySolid (cube: T) =
  VAR
    obj:   Object;
    order: ARRAY Face OF Face;
    vbt:   VBT.T;
  BEGIN
    IF Rect.IsEmpty (VBT.Domain (cube.vbt)) THEN RETURN END;
    obj := cube.obj;
    MapObject (cube.viewCRM, obj);
    MapObject (cube.projCRM, obj);
    order := DepthOrder (obj);
    HomogenizeObject (obj);
    MapObject (cube.imageCRM, obj);
    vbt := UseHiddenBuffer (cube);
    FOR f := FIRST (Face) TO LAST (Face) DO
      BuildFacePath (cube.facePath, obj, FacePts (order[f]));
      VBT.Fill (vbt, Rect.Full, cube.facePath, VBT.WindingCondition.Odd,
                op := colors[order[f]]);
    END;
    CopyToVisibleBuffer (cube);
  END DisplaySolid;

PROCEDURE UseHiddenBuffer (cube: T): VBT.T =
  VAR dom := VBT.Domain(cube.vbt);
  BEGIN
    VBT.BeginGroup(cube.vbt);
    IF cube.doubleBuffer AND cube.offscreen # NIL THEN
      VBT.PaintTint(cube.offscreen, VBT.Domain(cube.offscreen), PaintOp.Bg);
      RETURN cube.offscreen;
    ELSE
      VBT.PaintTint(cube.vbt, dom, PaintOp.Bg);
      RETURN cube.vbt;
    END;
  END UseHiddenBuffer;

PROCEDURE CopyToVisibleBuffer (cube: T) =
  VAR dom, dom2: Rect.T; br: Region.T; pixmap: ScrnPixmap.T;
  BEGIN
    IF NOT cube.doubleBuffer THEN VBT.EndGroup (cube.vbt); RETURN END;

    dom := VBT.Domain (cube.vbt);
    dom2 := VBT.Domain(cube.offscreen);
    pixmap := VBT.Capture (cube.offscreen, dom2, br);
    VBT.PaintScrnPixmap (cube.vbt, src:=pixmap, delta:=Point.Sub(
      Rect.NorthWest(dom), Rect.NorthWest(dom2)));
    VBT.EndGroup (cube.vbt);
    VBT.Sync (cube.vbt);
    pixmap.free ();
  END CopyToVisibleBuffer;

PROCEDURE InitObject (VAR obj: Object) =
  BEGIN
    obj[0] := R4.FromCoords (-1.0, -1.0, -1.0, 1.0);
    obj[1] := R4.FromCoords (1.0, -1.0, -1.0, 1.0);
    obj[2] := R4.FromCoords (1.0, -1.0, 1.0, 1.0);
    obj[3] := R4.FromCoords (-1.0, -1.0, 1.0, 1.0);
    obj[4] := R4.FromCoords (-1.0, 1.0, 1.0, 1.0);
    obj[5] := R4.FromCoords (-1.0, 1.0, -1.0, 1.0);
    obj[6] := R4.FromCoords (1.0, 1.0, -1.0, 1.0);
    obj[7] := R4.FromCoords (1.0, 1.0, 1.0, 1.0);
  END InitObject;

PROCEDURE MapObject (READONLY crm: CRM; VAR obj: Object) =
  BEGIN
    FOR pt := 0 TO N DO
      obj[pt] := R4x4.TrMap (crm, obj[pt]);
    END;
  END MapObject;

PROCEDURE HomogenizeObject (VAR obj: Object) =
  BEGIN
    FOR pt := 0 TO N DO
      obj[pt][X] := obj[pt][X] / obj[pt][W];
      obj[pt][Y] := obj[pt][Y] / obj[pt][W];
      obj[pt][Z] := obj[pt][Z] / obj[pt][W];
      obj[pt][W] := 1.0;
    END
  END HomogenizeObject;

PROCEDURE DepthOrder (READONLY obj: Object): FaceList =
  VAR max, t: Face; a: FaceList; z: ARRAY Face OF REAL; tz: REAL;
  BEGIN
    FOR f := FIRST (Face) TO LAST (Face) DO
      z[f] := MaxZ (obj, f);
      a[f] := f;
    END;
    FOR i := FIRST (Face) TO VAL (ORD (LAST (Face)) - 1, Face) DO
      max := i;
      FOR j := VAL (ORD (i) + 1, Face) TO LAST (Face) DO
        IF z[j] > z[max] THEN max := j;  END;
      END;
      t      := a[max];
      a[max] := a[i];
      a[i]   := t;
      tz     := z[max];
      z[max] := z[i];
      z[i]   := tz;
    END;
    RETURN a;
  END DepthOrder;

PROCEDURE MaxZ (READONLY obj: Object; f: Face): REAL =
  VAR pts: Quad; max: REAL;
  BEGIN
    pts := FacePts (f);
    max := obj[pts[0]][Z];
    IF obj[pts[1]][Z] > max THEN max := obj[pts[1]][Z] END;
    IF obj[pts[2]][Z] > max THEN max := obj[pts[2]][Z] END;
    IF obj[pts[3]][Z] > max THEN max := obj[pts[3]][Z] END;
    RETURN max
  END MaxZ;

PROCEDURE PointFromR4 (READONLY r4pt: R4.T): Point.T =
  VAR x, y: REAL;
  BEGIN
    x := r4pt[X];
    y := r4pt[Y];
    RETURN Point.FromCoords (TRUNC (x), TRUNC (y));
  END PointFromR4;

PROCEDURE BuildFacePath (p: Path.T; READONLY obj: Object; READONLY q: Quad) =
  BEGIN
    Path.Reset (p);
    Path.MoveTo (p, PointFromR4 (obj[q[0]]));
    Path.LineTo (p, PointFromR4 (obj[q[1]]));
    Path.LineTo (p, PointFromR4 (obj[q[2]]));
    Path.LineTo (p, PointFromR4 (obj[q[3]]));
    Path.Close (p);
  END BuildFacePath;

PROCEDURE FacePts (f: Face): Quad =
  PROCEDURE P (a, b, c, d: INTEGER): Quad =
    VAR q: Quad;
    BEGIN
      q[0] := a;
      q[1] := b;
      q[2] := c;
      q[3] := d;
      RETURN q;
    END P;
  BEGIN
    CASE f OF
      | Face.North => RETURN P (4, 5, 6, 7);
      | Face.South => RETURN P (0, 1, 2, 3);
      | Face.East =>  RETURN P (1, 2, 7, 6);
      | Face.West =>  RETURN P (3, 4, 5, 0);
      | Face.Front => RETURN P (2, 3, 4, 7);
      | Face.Back =>  RETURN P (0, 1, 6, 5);
    END;
  END FacePts;

PROCEDURE BuildPath (p: Path.T; READONLY obj: Object) =
  VAR
    dcPts: ARRAY [0..N] OF Point.T;
  BEGIN
    FOR pt := 0 TO 7 DO dcPts[pt] := PointFromR4 (obj[pt]); END;

    Path.Reset (p);

          (* edges along bottom face *)
    Path.MoveTo (p, dcPts[0]);
    Path.LineTo (p, dcPts[1]);
    Path.LineTo (p, dcPts[2]);
    Path.LineTo (p, dcPts[3]);
    Path.LineTo (p, dcPts[0]);

          (* edges along top face *)
    Path.MoveTo (p, dcPts[4]);
    Path.LineTo (p, dcPts[5]);
    Path.LineTo (p, dcPts[6]);
    Path.LineTo (p, dcPts[7]);
    Path.LineTo (p, dcPts[4]);

          (* vertical edges *)
    Path.MoveTo (p, dcPts[0]);
    Path.LineTo (p, dcPts[5]);
    Path.MoveTo (p, dcPts[1]);
    Path.LineTo (p, dcPts[6]);
    Path.MoveTo (p, dcPts[2]);
    Path.LineTo (p, dcPts[7]);
    Path.MoveTo (p, dcPts[3]);
    Path.LineTo (p, dcPts[4]);

          (* diagonal *)
    Path.MoveTo (p, dcPts[0]);
    Path.LineTo (p, dcPts[7]);
  END BuildPath;

PROCEDURE DegToRadians (deg: REAL): LONGREAL =
  BEGIN
    RETURN FLOAT (Math.Degree * deg, LONGREAL);
  END DegToRadians;

PROCEDURE InitCRM (VAR crm: CRM) =
  BEGIN
    crm := R4x4.Identity ();
  END InitCRM;

PROCEDURE XRotateCRM (VAR crm: CRM; degrees: REAL) =
  VAR sn, cs: REAL; r0, r1, r2, r3: R4.T; m: R4x4.T;
  BEGIN
    cs  := FLOAT( Math.cos (DegToRadians (degrees)));
    sn  := FLOAT( Math.sin (DegToRadians (degrees)));
    r0  := R4.FromCoords (1.0, 0.0, 0.0, 0.0);
    r1  := R4.FromCoords (0.0, cs, -sn, 0.0);
    r2  := R4.FromCoords (0.0, sn, cs, 0.0);
    r3  := R4.FromCoords (0.0, 0.0, 0.0, 1.0);
    m   := R4x4.FromRows (r0, r1, r2, r3);
    crm := R4x4.Mul (m, crm);
  END XRotateCRM;

PROCEDURE YRotateCRM (VAR crm: CRM; degrees: REAL) =
  VAR sn, cs: REAL; r0, r1, r2, r3: R4.T; m: R4x4.T;
  BEGIN
    cs  := FLOAT( Math.cos (DegToRadians (degrees)));
    sn  := FLOAT( Math.sin (DegToRadians (degrees)));
    r0  := R4.FromCoords (cs, 0.0, sn, 0.0);
    r1  := R4.FromCoords (0.0, 1.0, 0.0, 0.0);
    r2  := R4.FromCoords (-sn, 0.0, cs, 0.0);
    r3  := R4.FromCoords (0.0, 0.0, 0.0, 1.0);
    m   := R4x4.FromRows (r0, r1, r2, r3);
    crm := R4x4.Mul (m, crm);
  END YRotateCRM;

PROCEDURE ZRotateCRM (VAR crm: CRM; degrees: REAL) =
  VAR sn, cs: REAL; r0, r1, r2, r3: R4.T; m: R4x4.T;
  BEGIN
    cs  := FLOAT( Math.cos (DegToRadians (degrees)));
    sn  := FLOAT( Math.sin (DegToRadians (degrees)));
    r0  := R4.FromCoords (cs, -sn, 0.0, 0.0);
    r1  := R4.FromCoords (sn, cs, 0.0, 0.0);
    r2  := R4.FromCoords (0.0, 0.0, 1.0, 0.0);
    r3  := R4.FromCoords (0.0, 0.0, 0.0, 1.0);
    m   := R4x4.FromRows (r0, r1, r2, r3);
    crm := R4x4.Mul (m, crm);
  END ZRotateCRM;

PROCEDURE TranslateCRM (VAR crm: CRM; dx, dy, dz: REAL) =
  VAR r0, r1, r2, r3: R4.T; m: R4x4.T;
  BEGIN
    r0  := R4.FromCoords (1.0, 0.0, 0.0, dx);
    r1  := R4.FromCoords (0.0, 1.0, 0.0, dy);
    r2  := R4.FromCoords (0.0, 0.0, 1.0, dz);
    r3  := R4.FromCoords (0.0, 0.0, 0.0, 1.0);
    m   := R4x4.FromRows (r0, r1, r2, r3);
    crm := R4x4.Mul (m, crm);
  END TranslateCRM;

PROCEDURE ScaleCRM (VAR crm: CRM; sx, sy, sz: REAL) =
  VAR r0, r1, r2, r3: R4.T; m: R4x4.T;
  BEGIN
    r0  := R4.FromCoords (sx, 0.0, 0.0, 0.0);
    r1  := R4.FromCoords (0.0, sy, 0.0, 0.0);
    r2  := R4.FromCoords (0.0, 0.0, sz, 0.0);
    r3  := R4.FromCoords (0.0, 0.0, 0.0, 1.0);
    m   := R4x4.FromRows (r0, r1, r2, r3);
    crm := R4x4.Mul (m, crm);
  END ScaleCRM;

PROCEDURE PerspectiveCRM (VAR crm: CRM; d: REAL) =
  VAR r0, r1, r2, r3: R4.T; m: R4x4.T;
  BEGIN
    IF d <= 0.0 THEN RETURN END;
    r0  := R4.FromCoords (1.0, 0.0, 0.0, 0.0);
    r1  := R4.FromCoords (0.0, 1.0, 0.0, 0.0);
    r2  := R4.FromCoords (0.0, 0.0, 1.0, 0.0);
    r3  := R4.FromCoords (0.0, 0.0, 1.0 / d, 0.0);
    m   := R4x4.FromRows (r0, r1, r2, r3);
    crm := R4x4.Mul (m, crm);
  END PerspectiveCRM;

VAR
  initialized := FALSE;

PROCEDURE Initialize () =
  BEGIN
    IF initialized THEN RETURN END;
    colors[Face.North] := 
      PaintOp.Pair (PaintOp.Bg, PaintOp.FromRGB (0.0, 0.0, 1.0));
    colors[Face.South] :=
      PaintOp.Pair (PaintOp.Bg, PaintOp.FromRGB (1.0, 0.0, 0.0));
    colors[Face.East] :=
      PaintOp.Pair (PaintOp.Bg, PaintOp.FromRGB (0.0, 1.0, 0.0));
    colors[Face.West] :=
      PaintOp.Pair (PaintOp.Bg, PaintOp.FromRGB (0.0, 1.0, 1.0));
    colors[Face.Front] :=
      PaintOp.Pair (PaintOp.Bg, PaintOp.FromRGB (1.0, 1.0, 0.0));
    colors[Face.Back] := 
      PaintOp.Pair (PaintOp.Bg, PaintOp.FromRGB (1.0, 0.0, 1.0));
    initialized := TRUE;
  END Initialize;

BEGIN
END Cube.
