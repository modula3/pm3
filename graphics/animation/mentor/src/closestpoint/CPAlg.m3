(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE CPAlg;

IMPORT Algorithm, CPAlgClass, CPIE, FormsVBT, RefList, VBT, ZeusPanel;
IMPORT Thread, R2;
IMPORT ZeusCodeView, Text;
IMPORT Math;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE
  T = CPAlgClass.T BRANDED OBJECT OVERRIDES run := Run; END (* OBJECT *);

  Link = REF RECORD
    p:R2.T;
    next: Link;
  END;


CONST MaxDistance = 1.0E30;

PROCEDURE TheAlg (alg: T) RAISES {Thread.Alerted} =

VAR
  pass    : INTEGER;
  min     : REAL    := 100.0 <* TRACE alg.varView.setReal *>;
  sentinel : Link; (* sentinel at the end linked lists *)
  cp1, cp2: R2.T;
  N: INTEGER := 0 <* TRACE alg.varView.setIntegerL *>; (* Number of
                                                         points *)
  big      : BOOLEAN;


PROCEDURE Merge (VAR a, b: Link; ): Link RAISES {Thread.Alerted} =
  VAR
    c,head : Link;
    comp  : BOOLEAN;
  BEGIN
 
    IF pass = 1 THEN
      comp := (a^.p[0] < b^.p[0]);
    ELSE
      comp := (a^.p[1] < b^.p[1]);
    END (* IF *);

    IF comp THEN
      c := a;
      a := a^.next;
    ELSE
      c := b;
      b := b^.next;
    END (* IF *);
 
    head:=c;
    WHILE (c # sentinel) DO
      IF pass = 1 THEN
        comp := (a^.p[0] < b^.p[0]);
      ELSE
        comp := (a^.p[1] < b^.p[1]);
      END (* IF *);

      IF comp THEN
        c^.next := a;
        c := a;
        a := a^.next;
      ELSE
        c^.next := b;
        c := b;
        b := b^.next;
      END (* IF *);
      IF Thread.TestAlert() THEN RAISE Thread.Alerted; END (* IF *);
    END (* WHILE *);

    RETURN head;
  END Merge;


PROCEDURE Check (p1, p2: R2.T) RAISES {Thread.Alerted} = 
  VAR dist: REAL;
  BEGIN
    ZeusCodeView.Enter(alg, procedureName := "CheckforMin");

    At(5);
    IF ((p1[1] # sentinel^.p[1]) AND (p2[1] # sentinel^.p[1])) THEN
      dist := FLOAT(Math.sqrt(FLOAT((p1[0] - p2[0]) * (p1[0] - p2[0])
                                      + (p1[1] - p2[1]) * (p1[1] - p2[1]),
                                    LONGREAL)));
      IF (dist < min) THEN
        CPIE.NewMin(alg, p1, p2, cp1, cp2, big);
        At(10);
        min := dist;
        cp1 := p1;
        cp2 := p2;
      ELSE
        (* CPIE.NoChangeMin(alg, p1, p2, cp1, cp2, big); *)
        (* does nothing at present *)
      END (* IF *);
    END (* IF *);
    ZeusCodeView.Exit(alg);
  END Check;


PROCEDURE SortbyX (c: Link; N: INTEGER): Link
  RAISES {Thread.Alerted} =
  VAR a, b, t1, t2: Link;
  BEGIN

    IF c^.next = sentinel THEN RETURN c; END (* IF *);

    a := c;

    FOR i := 2 TO (N DIV 2) DO
      c := c^.next;
      IF Thread.TestAlert() THEN
        RAISE Thread.Alerted;
      END (* IF *);
    END (* FOR *);

    b := c^.next;
    c^.next := sentinel;

    t1 := SortbyX(a, N DIV 2);
    t2 := SortbyX(b, N - (N DIV 2));

    c := Merge(t1, t2);

    RETURN c;
  END SortbyX;



PROCEDURE SortbyY (c: Link; N: INTEGER): Link RAISES {Thread.Alerted} =
  VAR
    a, b, t1, t2, last  : Link;
    middle, leftofmiddle: REAL;
    p1, p2, p3, p4      : R2.T;
    ar1, ar2, cr1, cr2  : REAL;
    lastbarpos          : REAL;
  BEGIN

    IF c^.next = sentinel THEN RETURN c; END (* IF *);

    ZeusCodeView.Enter(alg, procedureName := "FindClosestPair");

    At(5);
    a := c;

    At(10);
    FOR i := 2 TO (N DIV 2) DO
      c := c^.next;
      IF Thread.TestAlert() THEN RAISE Thread.Alerted; END (* IF *);
    END (* FOR *);

    (* while we're at it, figure the "last" x-coordinate as well *)
    last := c^.next;
    FOR i := 2 TO (N - (N DIV 2)) DO
      last := last^.next;
      IF Thread.TestAlert() THEN RAISE Thread.Alerted; END (* IF *);
    END;

    (* setup second half-list and the sentinel of first half-list *)
    b := c^.next;
    c^.next := sentinel;

    At(15);
    middle := (c^.p[0] + b^.p[0]) / 2.0;
    (* the point immediately to left of middle *)
    leftofmiddle := c^.p[0];
    (* initialize the active region's x-coordinates *)
    ar1 := a^.p[0];
    ar2 := last^.p[0];

    At(20);

    (* IF (N > 3) THEN CPIE.NotProcessed(alg, leftofmiddle, ar2, big); END
       (* IF *); *)

    t1 := SortbyY(a, N DIV 2);

    (* IF (N > 3) THEN CPIE.RemoveNotProcessed(alg, leftofmiddle, ar2,
       big); END (* IF *); *)

    At(25);

    t2 := SortbyY(b, N - (N DIV 2));

    (* IF (N > 3) THEN CPIE.Processed(alg, ar1, ar2, big); ELSE
       CPIE.Processed(alg, ar2, middle, big); END (* IF *); *)

    cr1 := MAX(middle - min, ar1);
    cr2 := MIN(middle + min, ar2);

    CPIE.Split(alg, middle, big);
    CPIE.ActiveR(alg, ar1, ar2, big);
    CPIE.CloseR(alg, cr1, cr2, big);
   

    At(30);
    c := Merge(t1, t2);

    At(35);
    a := c;
    p1 := sentinel^.p;
    p2 := sentinel^.p;
    p3 := sentinel^.p;
    p4 := sentinel^.p;
     
    REPEAT
      IF Thread.TestAlert() THEN RAISE Thread.Alerted; END (* IF *);
      At(40);
      IF (ABS(a^.p[0] - middle) < min) THEN

        CPIE.DrawBar(alg, a^.p[1], cr1, cr2, big);
        CPIE.SelectTrialPoint(alg, a^.p, big);

        At(45);
        IF (p1[0] # sentinel^.p[0]) THEN
          CPIE.SelectTargetPoint(alg, a^.p, p1, big);
          Check(a^.p, p1);
          CPIE.DeselectTargetPoint(alg, a^.p, p1, big);
        END (* IF *);

        At(50);
        IF (p2[0] # sentinel^.p[0]) THEN
          CPIE.SelectTargetPoint(alg, a^.p, p2, big);
          Check(a^.p, p2);
          CPIE.DeselectTargetPoint(alg, a^.p, p2, big);
        END (* IF *);

        At(55);
        IF (p3[0] # sentinel^.p[0]) THEN
          CPIE.SelectTargetPoint(alg, a^.p, p3, big);
          Check(a^.p, p3);
          CPIE.DeselectTargetPoint(alg, a^.p, p3, big);
        END (* IF *);

        At(60);
        IF (p4[0] # sentinel^.p[0]) THEN
          CPIE.SelectTargetPoint(alg, a^.p, p4, big);
          Check(a^.p, p4);
          CPIE.DeselectTargetPoint(alg, a^.p, p4, big);
        END (* IF *);

        At(65);
        p1 := p2;
        p2 := p3;
        p3 := p4;
        p4 := a^.p;
        CPIE.DeselectTrialPoint(alg, a^.p, big);
        lastbarpos := a^.p[1];
        (* CPIE.RemoveBar(alg, a^.p[1], cr1, cr2, big);*)
      END (* IF *);

      At(70);
      a := a^.next;
      IF Thread.TestAlert() THEN RAISE Thread.Alerted; END (* IF *);

      At(75);
    UNTIL a = sentinel;
    CPIE.RemoveBar(alg, lastbarpos, cr1, cr2, big);
    CPIE.SplitRemove(alg, middle, big);
    CPIE.RemoveActiveR(alg, ar1, ar2, big);
    CPIE.RemoveCloseR(alg, cr1, cr2, big);

    At(80);
    ZeusCodeView.Exit(alg);
    RETURN c;
  END SortbyY;


PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    ZeusCodeView.Event(alg, line);
  END At;

CONST
  selectedDataSmall = ARRAY [1 .. 8] OF
                        R2.T{
                        R2.T{0.1, 0.1}, R2.T{0.2, 0.74}, R2.T{0.3, 0.6},
                        R2.T{0.5, 0.4}, R2.T{0.6, 0.05}, 
                        R2.T{0.78, 0.77}, R2.T{0.88, 0.67},
                        R2.T{0.93, 0.35}};

  selectedDataLarge = ARRAY [1 .. 16] OF
                        R2.T{
                        R2.T{0.05, 0.5}, R2.T{0.1, 0.1}, R2.T{0.2, 0.02},
                        R2.T{0.22, 0.7}, R2.T{0.3, 0.25}, R2.T{0.4, 0.55},
                        R2.T{0.44, 0.87}, R2.T{0.5, 0.35}, R2.T{0.6, 0.15},
                        R2.T{0.67, 0.93}, R2.T{0.73, 0.68},
                        R2.T{0.78, 0.75}, R2.T{0.8, 0.13},
                        R2.T{0.85, 0.45}, R2.T{0.90, 0.83},
                        R2.T{0.95, 0.40}};

VAR
  curr, new: Link;
  head: Link;                   (* head^.next is header to the list of
                                   points *)
  random       : BOOLEAN;
  selectedsmall: BOOLEAN := TRUE;

BEGIN                           (* begin of procedure TheAlg *)

  ZeusCodeView.Enter(alg, procedureName := "Main");

  At(5);
  LOCK VBT.mu DO
    IF Text.Equal("random", FormsVBT.GetChoice(alg.data, "data")) THEN
      random := TRUE;
      N := FormsVBT.GetInteger(alg.data, "N");
    ELSE
      random := FALSE;
      IF Text.Equal("selectedsmall", 
               FormsVBT.GetChoice(alg.data, "selectedsize")) THEN
        selectedsmall := TRUE;
        N := 8;
      ELSE
        selectedsmall := FALSE;
        N := 16;
      END (* IF *);
    END (*IF*);
  END (* LOCK *);

  big := N > 16;

  sentinel := NEW(Link);
  sentinel^.next := sentinel;
  sentinel^.p[0] := 100.0;
  sentinel^.p[1] := 100.0;
  cp1 := sentinel^.p;
  cp2 := sentinel^.p;

  (* Init the array with random values *)
  CPIE.Setup(alg);
  head := NEW(Link);
  IF random THEN
    head^.p := R2.Throw(0.0, 1.0);
  ELSIF selectedsmall THEN
      head^.p := selectedDataSmall[1];
  ELSE
      head^.p := selectedDataLarge[1];
  END (* IF *);
  CPIE.AddPoint(alg, head^.p, N, big);

  curr := head;
  FOR i := 2 TO N DO
    (* The CP algorithm with N points *)
    new := NEW(Link);
    IF random THEN
      new^.p := R2.Throw(0.0, 1.0);
    ELSIF selectedsmall THEN
      new^.p := selectedDataSmall[i];
    ELSE
      new^.p := selectedDataLarge[i];
    END (* IF *);
    CPIE.AddPoint(alg, new^.p, N, big);
    curr^.next := new;
    curr := new;
  END (* FOR *);
  curr^.next := sentinel;
  min := MaxDistance;

  At(10);
  pass := 1;
  head := SortbyX(head, N);
  At(15);
  pass := 2;
  head := SortbyY(head, N);
  ZeusCodeView.Exit(alg);

END TheAlg;


PROCEDURE New (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("CPinput.fv");
  BEGIN
    RETURN
      NEW(
        T, data := fv, varRsrc := "CPVar.fv",
        codeViews :=
          RefList.List3(
            RefList.List2("M3 Code View", "CPCode.m3c"),
            RefList.List2("C Code View", "CPCode.cc"),
            RefList.List2("Pascal Code View", "CPCode.pasc"))).init()
  END New;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  BEGIN
    TheAlg(alg);
  END Run;

BEGIN
  ZeusPanel.RegisterAlg(New, "Closest Pair", "CP");
END CPAlg.
