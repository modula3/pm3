(* Copyright 1992 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Tue Jan 31 15:40:34 PST 1995 by kalsow  *)
(*      modified on Thu Jan  5 21:45:32 PST 1995 by najork  *)
(*      modified on Mon Jan 11 14:07:33 PST 1993 by steveg  *)
(*      modified on Sat Oct 17 13:34:43 PDT 1992 by ramshaw *)
(*      modified on Wed Jul 29 00:35:12 1992 by saxe        *)

MODULE GeomView;

IMPORT HullViewClass, Filter, FloatMode, GraphVBT, MyColors, R2, RefList,
       RealFloat, View, ZeusPanel, IntList, Site, SiteList, VBT, Thread;

CONST 
  Big : REAL = 6.0;
  dMul: REAL = 1.0;

TYPE
  MyVertex = GraphVBT.Vertex BRANDED OBJECT 
    rel: BOOLEAN := FALSE 
  END;
  Sites = REF ARRAY OF MyVertex;
  Edges = REF ARRAY OF GraphVBT.Edge;

  HP = REF RECORD 
    tail, head: INTEGER;  (* HP = HalfPlane *)
    vFront, vRight, vBack: GraphVBT.Vertex;
    pRight: GraphVBT.Polygon
  END;

  T = HullViewClass.T BRANDED OBJECT
    mg: GraphVBT.T;
    nSites: INTEGER;
    sites: Sites;
    edges: Edges;
    testLite: GraphVBT.VertexHighlight; 
    testLiteOn: BOOLEAN;
    tailLite, headLite: GraphVBT.VertexHighlight;
    tailLiteOn, headLiteOn: BOOLEAN;
    limbo: GraphVBT.Vertex;  
    tail, head: INTEGER;
    vTail, vHead, vFront, vRight, vBack, vLeft: GraphVBT.Vertex;
    pRight, pLeft: GraphVBT.Polygon;
    eBack, eShaft, eFront: GraphVBT.Edge;
    confirmed: RefList.T; (* of HP's *)
    hanging: RefList.T; (* of HP's *)
    bandData: BandData;   
  OVERRIDES
    startrun := Startrun;
    oeSetup := Setup;
    oeSetHalfPlane := SetHalfPlane;
    oeTestSite := TestSite;
    oeMoveHalfPlane := MoveHalfPlane;
    oeSwap := Swap;
    oeConfirm := Confirm;
    oeSentinel := Sentinel;
    oeStretch := Stretch;
    oeSnap := Snap;
    oeClearHead := ClearHead;
    oeClearTest := ClearTest;
    oeSetTail := SetTail;
  END;

  ConstPath = GraphVBT.AnimationPath BRANDED OBJECT
    p0: R2.T
    OVERRIDES pos := ConstPos
    END;
  AffinePath = GraphVBT.AnimationPath BRANDED OBJECT
    p0, p1: R2.T
    OVERRIDES pos := AffinePos
    END;
  JointPath = GraphVBT.AnimationPath BRANDED OBJECT
    pStart, pStop: R2.T;
    tStop: REAL
    OVERRIDES pos := JointPos
    END;
  ShoulderPath = GraphVBT.AnimationPath BRANDED OBJECT
    pStart, pBump, pStop: R2.T;
    tBump, tStop: REAL
    OVERRIDES pos := ShoulderPos
    END;
  BezierPath = GraphVBT.AnimationPath BRANDED OBJECT
    p0, p1, p2, p3: R2.T
    OVERRIDES pos := BezierPos
    END;
  Movie = REF RECORD
    head, tail: GraphVBT.AnimationPath;
    curTime: REAL;
    curHeadPos, curTailPos, curFrontPos, curBackPos, 
        curRightPos, curLeftPos: R2.T;
    END;
  FrontPath = GraphVBT.AnimationPath BRANDED OBJECT
      m: Movie OVERRIDES pos := FrontPos;
      END;
  BackPath = GraphVBT.AnimationPath BRANDED OBJECT
      m: Movie OVERRIDES pos := BackPos;
      END;
  RightPath = GraphVBT.AnimationPath BRANDED OBJECT
      m: Movie OVERRIDES pos := RightPos;
      END;
  LeftPath = GraphVBT.AnimationPath BRANDED OBJECT
      m: Movie OVERRIDES pos := LeftPos;
      END;


PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NEW(GraphVBT.T).init())
  END New;

PROCEDURE Startrun (view: T) =
  BEGIN
    LOCK VBT.mu DO
      EVAL Filter.Replace(view, NEW(GraphVBT.T).init())
    END;
    HullViewClass.T.startrun(view);
  END Startrun;

PROCEDURE Setup (view: T; trueSites, auxSites: SiteList.T) =
  VAR
    nTrueSites, nAuxSites: INTEGER;
    wc := GraphVBT.WorldRectangle{
            w := -1.3, s := -1.3, e := 1.3, n := 1.3};
    r : SiteList.T;
    s: Site.T;
    font: GraphVBT.WorldFont;
  BEGIN
    nTrueSites := SiteList.Length(trueSites);
    nAuxSites := SiteList.Length(auxSites);
    view.nSites := nTrueSites+2+nAuxSites;
    view.mg := NEW(GraphVBT.T, world := wc).init();
    font := view.mg.font(family := "Helvetica", weight := "bold", 
                         slant := GraphVBT.Slant.Roman, size := 0.1);
    LOCK VBT.mu DO
      EVAL Filter.Replace(view, view.mg);
    END;
    view.sites := NEW(Sites, view.nSites);
    r := SiteList.Append(trueSites, auxSites);
    WHILE r # NIL DO
      s := r.head;
      r := r.tail;
      IF s.bool THEN
        view.sites[s.uid]:=NEW(MyVertex, 
                               graph := view.mg,
                               pos   := R2.T {s.x, s.y},
                               shape := GraphVBT.VertexShape.Ellipse,
                               rel   := TRUE).init()
      ELSE
        view.sites[s.uid]:=NEW(MyVertex, 
                               graph := view.mg,
                               pos   := R2.T{s.x, s.y},
                               shape := GraphVBT.VertexShape.Ellipse,
                               color := MyColors.White(),  
                               font  := font,
                               fontColor := MyColors.Black(),  
                               border := 0.01,
                               label  := s.lab,
                               size   := R2.T{0.11, 0.11}).init()
      END;
    END;
    view.limbo := NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{0.0, 1.5},
            shape := GraphVBT.VertexShape.Ellipse,
            size := R2.T{0.0, 0.0}).init();
    view.sites[0] := NEW(MyVertex, graph := view.mg,
            pos := R2.T{0.0, 1.5},
            shape := GraphVBT.VertexShape.Ellipse,
            size := R2.T{0.0, 0.0}).init();
    view.sites[nTrueSites+1] := view.sites[0];
    view.edges := NEW(Edges, view.nSites);
    view.vHead := NEW(GraphVBT.Vertex, 
                      graph := view.mg,
                      pos   := view.limbo.pos,
                      shape := GraphVBT.VertexShape.Ellipse,
                      color := MyColors.Head(),
                      size  := R2.T{0.169, 0.169}).init();
    LOCK view.mg.mu DO view.vHead.toBack() END;
    view.vTail := NEW(GraphVBT.Vertex, 
                      graph := view.mg,
                      pos   := view.limbo.pos,
                      shape := GraphVBT.VertexShape.Rectangle,
                      size  := R2.T{0.0, 0.0}).init();
    view.vFront := NEW(GraphVBT.Vertex, 
                       graph := view.mg,
                       pos   := view.limbo.pos,
                       shape := GraphVBT.VertexShape.Rectangle,
                       size  := R2.T{0.0, 0.0}).init();
    view.vBack := NEW(GraphVBT.Vertex, 
                      graph := view.mg,
                      pos   := view.limbo.pos,
                      shape := GraphVBT.VertexShape.Rectangle,
                      size  := R2.T{0.0, 0.0}).init();
    view.vLeft := NEW(GraphVBT.Vertex, 
                      graph := view.mg,
                      pos   := view.limbo.pos,
                      shape := GraphVBT.VertexShape.Rectangle,
                      size  := R2.T{0.0, 0.0}).init();
    view.vRight := NEW(GraphVBT.Vertex, 
                       graph := view.mg,
                       pos   := view.limbo.pos,
                       shape := GraphVBT.VertexShape.Rectangle,
                       size  := R2.T{0.0, 0.0}).init();
    view.pRight := NEW(GraphVBT.Polygon, 
                       vertices := RefList.List3(view.vFront,
                                                 view.vRight,
                                                 view.vBack),
                       color := MyColors.Right()).init();
    view.pLeft := NEW(GraphVBT.Polygon, 
                      vertices := RefList.List3(view.vFront,
                                                view.vLeft,
                                                view.vBack),
                      color := MyColors.Left()).init();
    view.eShaft := NEW(GraphVBT.Edge, 
                       vertex0 := view.vTail,
                       vertex1 := view.vHead,
                       arrow := ARRAY [0..1] OF BOOLEAN{FALSE,TRUE},
                       color := MyColors.Shaft()).init();
    view.eFront := NEW(GraphVBT.Edge, 
                       vertex0 := view.vHead,
                       vertex1 := view.vFront,
                       color := MyColors.Front()).init();
    view.eBack := NEW(GraphVBT.Edge, 
                      vertex0 := view.vBack,
                      vertex1 := view.vTail,
                      color := MyColors.Back()).init();
    view.testLite :=
      NEW(GraphVBT.VertexHighlight, 
          vertex := view.limbo,
          color := MyColors.Test(),
          border := R2.T{0.042, 0.042}).init();
    view.testLiteOn := FALSE;
    view.tailLite :=
      NEW(GraphVBT.VertexHighlight, 
          vertex := view.limbo,
          color := MyColors.Tail(),
          border := R2.T{0.042, 0.042}).init();
    view.tailLiteOn := FALSE;
    view.headLite :=
      NEW(GraphVBT.VertexHighlight, vertex := view.limbo,
          color := MyColors.Head(),
          border := R2.T{0.021, 0.021}).init();
    view.headLiteOn := FALSE;
    view.mg.redisplay();
  END Setup;

PROCEDURE SetHalfPlane(view: T; tail, head: INTEGER) RAISES {Thread.Alerted} =
  VAR tailPos, headPos, center, del, delta, delta90: R2.T;
  BEGIN
  tailPos := view.sites[tail].pos;
  headPos := view.sites[head].pos;
  IF view.sites[tail].rel THEN
    <* ASSERT NOT view.sites[head].rel *>
    tailPos := R2.Add(headPos, tailPos);
  END;
  IF view.sites[head].rel THEN
    <* ASSERT NOT view.sites[tail].rel *>
    headPos := R2.Add(tailPos, headPos);
  END;

  IF NOT view.headLiteOn THEN
    LOCK view.mg.mu DO view.headLite.move(view.sites[head], FALSE) END;
    view.headLiteOn := TRUE;
  END;

    IF view.tailLiteOn AND NOT view.sites[view.tail].rel THEN
      LOCK view.mg.mu DO view.tailLite.move(view.sites[tail], TRUE) END;
      view.mg.animate(0.0, 1.0);
    ELSE
      LOCK view.mg.mu DO view.tailLite.move(view.sites[tail], FALSE) END;
      view.tailLiteOn := TRUE
    END;

  view.tail := tail;
  view.head := head;
  IF head # tail THEN
    del := R2.Sub(headPos, tailPos);
    delta := R2.Scale(Big/R2.L2Norm(del), del);
    delta90 := R2.T{delta[1], -delta[0]};
    center := R2.Mix(headPos, 0.5, tailPos, 0.5);
        view.vHead.move(headPos);
        view.vTail.move(tailPos);
        view.vFront.move(R2.Add(headPos, delta));
        view.vRight.move(R2.Add(center, delta90));
        view.vBack.move(R2.Sub(tailPos, delta));
        view.vLeft.move(R2.Sub(center, delta90));
  END;
  view.mg.redisplay();
  END SetHalfPlane;

PROCEDURE ClearHead(view: T) =
  BEGIN
    LOCK view.mg.mu DO
      view.headLiteOn := FALSE;
      view.headLite.move(view.limbo, FALSE);
    END;
    view.mg.redisplay();
  END ClearHead;

PROCEDURE ClearTest(view: T) =
  BEGIN
    LOCK view.mg.mu DO
      view.testLiteOn := FALSE;
      view.testLite.move(view.limbo, FALSE);
    END;
    view.mg.redisplay();
  END ClearTest;

PROCEDURE SetTail(view: T; i: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    IF view.tailLiteOn AND NOT view.sites[view.tail].rel THEN
      LOCK view.mg.mu DO
        view.tailLite.move(view.sites[i], TRUE);
      END;
      view.mg.animate(0.0, 1.0);
    ELSE
      LOCK view.mg.mu DO
        view.tailLite.move(view.sites[i], FALSE);
      END;
      view.tailLiteOn := TRUE;
      view.mg.redisplay();
    END;
  END SetTail;

PROCEDURE Swap(view: T; i, j: INTEGER) RAISES {Thread.Alerted} =
  VAR 
    iList : RefList.T := view.sites[i].vertexHighlights;
    jList : RefList.T := view.sites[j].vertexHighlights;
    h     : GraphVBT.VertexHighlight;
  BEGIN
    LOCK view.mg.mu DO
      WHILE iList # NIL DO
        h := iList.head;
        h.move (view.sites[j], TRUE);
        iList := iList.tail;
      END;
      WHILE jList # NIL DO
        h := jList.head;
        h.move (view.sites[i], TRUE);
        jList := jList.tail;
      END;
    END;
    view.mg.animate (0.5, 1.5);
  END Swap;

PROCEDURE TestSite(view: T; i: INTEGER) RAISES {Thread.Alerted} =
  <*FATAL FloatMode.Trap*>
  VAR p1, p2: R2.T;
      dist: REAL;
  BEGIN
    IF view.testLiteOn THEN
      p1 := view.testLite.vertex.pos;
      p2 := view.sites[i].pos;
      dist := R2.L2Dist(p1,p2);
      LOCK view.mg.mu DO view.testLite.move(view.sites[i], TRUE) END;
      view.mg.animate(0.0, RealFloat.Sqrt(RealFloat.Sqrt(dist)));
    ELSE
      LOCK view.mg.mu DO view.testLite.move(view.sites[i], FALSE) END;
      view.mg.redisplay();
      view.testLiteOn := TRUE
    END;
  END TestSite;

PROCEDURE MoveHalfPlane(view: T; tail, head: INTEGER) RAISES {Thread.Alerted} =
  VAR headPos, tailPos, center, del, delta, delta90: R2.T;
      m: Movie;
      headPath, tailPath: GraphVBT.AnimationPath;
  BEGIN
  IF view.head = view.tail THEN
    LOCK view.mg.mu DO view.headLite.move(view.sites[head], TRUE) END;
    view.mg.animate(0.0, 1.0);
    SetHalfPlane(view, tail, head);
  ELSE
  tailPos := view.sites[tail].pos;
  headPos := view.sites[head].pos;
  IF view.sites[tail].rel THEN
    <* ASSERT NOT view.sites[head].rel *>
    tailPos := R2.Add(headPos, tailPos);
  END;
  IF view.sites[head].rel THEN
    <* ASSERT NOT view.sites[tail].rel *>
    headPos := R2.Add(tailPos, headPos);
  END;
  del := R2.Sub(headPos, tailPos);
  delta := R2.Scale(Big/R2.L2Norm(del), del);
  delta90 := R2.T{delta[1], -delta[0]};
  center := R2.Mix(headPos, 0.5, tailPos, 0.5);

  IF view.sites[view.head].rel THEN
    headPath := NEW(BezierPath, p0 := view.vHead.pos,
                        p1 := R2.Add(view.vHead.pos, R2.T{-0.1, 0.0}),
                        p2 := R2.Add(view.vHead.pos, R2.T{-0.1, 0.0}),
                        p3 := headPos);
  ELSE headPath := NEW(AffinePath, p0 := view.vHead.pos,
                   p1 := headPos);
  END;
  tailPath := NEW(AffinePath, p0 := view.vTail.pos,
                   p1 := tailPos);
  m := NEW(Movie, head := headPath, tail := tailPath, curTime := -1.0);
  LOCK view.mg.mu DO
      view.headLite.move(view.sites[head], TRUE);
      view.vHead.move(headPos, TRUE, path := headPath);
      view.vTail.move(tailPos, TRUE, path := tailPath);
      view.vFront.move(R2.Add(headPos, delta), TRUE, 
                       path := NEW(FrontPath, m := m));
      view.vRight.move(R2.Add(center, delta90), TRUE, 
                       path := NEW(RightPath, m := m));
      view.vBack.move(R2.Sub(tailPos, delta), TRUE, 
                      path := NEW(BackPath, m := m));
      view.vLeft.move(R2.Sub(center, delta90), TRUE, 
                      path := NEW(LeftPath, m := m));
  END;
  view.mg.animate(0.0, 1.0);
  view.head := head;
  view.tail := tail;
  END;
  END MoveHalfPlane;

PROCEDURE Confirm(view: T; tail, head: INTEGER) =
  VAR hp: HP;
  BEGIN
  <*ASSERT tail = view.tail AND head = view.head *>
  LOCK view.mg.mu DO 
    view.testLiteOn := FALSE;
    view.testLite.move(view.limbo, FALSE);
  END;
  hp := NEW(HP, tail := view.tail,
                head := view.head,
                vFront := NEW(GraphVBT.Vertex, 
                              graph := view.mg,
                              pos := view.vFront.pos,
                              shape := GraphVBT.VertexShape.Ellipse,
                              size := R2.T{0.0, 0.0}).init(),
                vRight := NEW(GraphVBT.Vertex, 
                              graph := view.mg,
                              pos := view.vRight.pos,
                              shape := GraphVBT.VertexShape.Ellipse,
                              size := R2.T{0.0, 0.0}).init(),
                vBack := NEW(GraphVBT.Vertex, 
                             graph := view.mg,
                             pos := view.vBack.pos,
                             shape := GraphVBT.VertexShape.Ellipse,
                             size := R2.T{0.0, 0.0}).init());
  hp.pRight := NEW (GraphVBT.Polygon, 
                    vertices := RefList.List3(hp.vFront, hp.vRight, hp.vBack),
                    color := MyColors.Outside()).init();
  view.confirmed := RefList.Cons (hp, view.confirmed);
  IF NOT view.sites[tail].rel AND NOT view.sites[head].rel THEN
     EVAL NEW(GraphVBT.Edge, vertex0 := view.sites[hp.tail],
                      vertex1 := view.sites[hp.head]).init() END;
  LOCK view.mg.mu DO
      view.sites[hp.tail].setColor(MyColors.Black());
      view.sites[hp.tail].setFontColor(MyColors.White());
      view.sites[hp.tail].setBorder(0.0);
      view.sites[hp.head].setColor(MyColors.Black());
      view.sites[hp.head].setFontColor(MyColors.White());
      view.sites[hp.head].setBorder(0.0);
    view.vFront.move(view.limbo.pos, FALSE);
    view.vRight.move(view.limbo.pos, FALSE);
    view.vBack.move(view.limbo.pos, FALSE);
    view.vLeft.move(view.limbo.pos, FALSE);
    view.vHead.move(view.limbo.pos, FALSE);
    view.vTail.move(view.limbo.pos, FALSE);
  END;
  view.mg.redisplay();
  END Confirm;

PROCEDURE ConfirmOne(view: T; tail, head: INTEGER) =
  VAR hp: HP;
    del, delta, delta90, center, headPos, tailPos: R2.T;
  BEGIN
  headPos := view.sites[head].pos;
  tailPos := view.sites[tail].pos;
  del := R2.Sub(headPos, tailPos);
  delta := R2.Scale(Big/R2.L2Norm(del), del);
  delta90 := R2.T{delta[1], -delta[0]};
  center := R2.Mix(headPos, 0.5, tailPos, 0.5);
  hp := NEW(HP, tail := tail,
                head := head,
                vFront := NEW(GraphVBT.Vertex, graph := view.mg,
                                    pos := R2.Add(headPos, delta),
                                    shape := GraphVBT.VertexShape.Ellipse,
                                    size := R2.T{0.0, 0.0}).init(),
                vRight := NEW(GraphVBT.Vertex, graph := view.mg,
                                    pos := R2.Add(center, delta90),
                                    shape := GraphVBT.VertexShape.Ellipse,
                                    size := R2.T{0.0, 0.0}).init(),
               vBack := NEW(GraphVBT.Vertex, graph := view.mg,
                                    pos := R2.Sub(tailPos, delta),
                                    shape := GraphVBT.VertexShape.Ellipse,
                                    size := R2.T{0.0, 0.0}).init());
  hp.pRight := NEW (GraphVBT.Polygon, 
                    vertices := RefList.List3 (hp.vFront, hp.vRight, hp.vBack),
                    color := MyColors.Outside()).init();
  view.confirmed := RefList.Cons (hp, view.confirmed);
  IF NOT view.sites[tail].rel AND NOT view.sites[head].rel THEN
     EVAL NEW(GraphVBT.Edge, 
              vertex0 := view.sites[tail],
              vertex1 := view.sites[head]).init() END;
  LOCK view.mg.mu DO
      view.sites[hp.tail].setColor(MyColors.Black());
      view.sites[hp.tail].setFontColor(MyColors.White());
      view.sites[hp.tail].setBorder(0.0);
      view.sites[hp.head].setColor(MyColors.Black());
      view.sites[hp.head].setFontColor(MyColors.White());
      view.sites[hp.head].setBorder(0.0);
  END;
  END ConfirmOne;

PROCEDURE Sentinel(view: T; i, j: INTEGER) =
  BEGIN
  view.sites[i]:=view.sites[j];
  END Sentinel;

TYPE BandData = REF RECORD
    jntStarts, jntStops, lshStarts,
    lshBumps, lshStops, rshStarts, 
    rshBumps, rshStops: REF ARRAY OF R2.T;
    jntStopTimes, lshStopTimes, rshStopTimes: REF ARRAY OF REAL;
    jnt, lsh, rsh: REF ARRAY OF GraphVBT.Vertex;
    hullSiteUids: REF ARRAY OF INTEGER;
    END;

PROCEDURE Stretch(             view      : T; 
                               hullSites : IntList.T; 
                  <* UNUSED *> otherSites: IntList.T) =
  VAR b: BandData := NEW(BandData);
      n: INTEGER;
      hs: REF ARRAY OF R2.T;
      hsl: IntList.T;
      minX, maxX, minY, maxY, rad, len: REAL;
      center, del, del90: R2.T;
      band: Edges;
  BEGIN
  view.bandData := b;
  n := IntList.Length (hullSites);
  hsl := hullSites;
  hs := NEW (REF ARRAY OF R2.T, n+2);
  b.hullSiteUids := NEW (REF ARRAY OF INTEGER, n+2);
  FOR i := 1 TO n DO 
    b.hullSiteUids[i] := hsl.head;
    hs[i] := view.sites[hsl.head].pos;
    hsl := hsl.tail;
  END;
  hs[0] := hs[n];
  hs[n+1] := hs[1];
  b.hullSiteUids[0] := b.hullSiteUids[n];
  b.hullSiteUids[n+1] := b.hullSiteUids[1];
  minX := 100.0;
  maxX := -100.0;
  minY := 100.0;
  maxY := -100.0;
  FOR i := 1 TO n DO
    minX := MIN(minX, hs[i][0]);
    maxX := MAX(maxX, hs[i][0]);
    minY := MIN(minY, hs[i][1]);
    maxY := MAX(maxY, hs[i][1]);
  END;
  center := R2.Mix(R2.T{minX, minY}, 0.5, R2.T{maxX, maxY}, 0.5);
  b.jntStarts := NEW(REF ARRAY OF R2.T, n+2);
  b.jntStops := NEW(REF ARRAY OF R2.T, n+2);
  b.lshStarts := NEW(REF ARRAY OF R2.T, n+2);
  b.lshBumps := NEW(REF ARRAY OF R2.T, n+2);
  b.lshStops := NEW(REF ARRAY OF R2.T, n+2);
  b.rshStarts := NEW(REF ARRAY OF R2.T, n+2);
  b.rshBumps := NEW(REF ARRAY OF R2.T, n+2);
  b.rshStops := NEW(REF ARRAY OF R2.T, n+2);
  b.jntStopTimes := NEW(REF ARRAY OF REAL, n+2);
  b.lshStopTimes := NEW(REF ARRAY OF REAL, n+2);
  b.rshStopTimes := NEW(REF ARRAY OF REAL, n+2);

  rad := 0.0;
  FOR i := 1 TO n DO
    del := R2.Sub(hs[i], center);
    rad := MAX(rad, R2.L2Norm(del));
  END;
  rad := MAX(1.1 * rad, 1.2);

  FOR i := 0 TO n+1 DO
    del := R2.Sub(hs[i], center);
    b.jntStopTimes[i] := dMul * (rad - R2.L2Norm(del));
    b.jntStarts[i] := R2.Add(center, R2.Scale(rad/R2.L2Norm(del), del));
    b.jntStops[i] := hs[i];
  END;

  FOR i := 1 TO n DO
    del := R2.Sub(hs[i], center);
    len := R2.L2Norm(del);
    del := R2.Scale(1.0/len, del);
    del90 := R2.T{del[1], -del[0]};
    del := R2.Scale(0.5 * R2.L2Dist(b.jntStarts[i-1], b.jntStarts[i]), del90);
    b.lshStarts[i] := R2.Add(b.jntStarts[i], del);
    b.lshBumps[i] := R2.Mix(center, 1.0 - len/rad, b.lshStarts[i], len/rad);
    b.lshStops[i] := R2.Mix(b.jntStops[i], 2.0/3.0, b.jntStops[i-1], 1.0/3.0);
    b.lshStopTimes[i] := dMul * (rad - R2.L2Dist(center, b.lshStops[i]));

    del := R2.Scale(0.5 * R2.L2Dist(b.jntStarts[i+1], b.jntStarts[i]), del90);
    b.rshStarts[i] := R2.Sub(b.jntStarts[i], del);
    b.rshBumps[i] := R2.Mix(center, 1.0 - len/rad, b.rshStarts[i], len/rad);
    b.rshStops[i] := R2.Mix(b.jntStops[i], 2.0/3.0, b.jntStops[i+1], 1.0/3.0);
    b.rshStopTimes[i] := dMul * (rad - R2.L2Dist(center, b.rshStops[i]));
  END;

  b.jntStarts[0] := b.jntStarts[n];
  b.jntStops[0] := b.jntStops[n];
  b.lshStarts[0] := b.lshStarts[n];
  b.lshBumps[0] := b.lshBumps[n];
  b.lshStops[0] := b.lshStops[n];
  b.rshStarts[0] := b.rshStarts[n];
  b.rshBumps[0] := b.rshBumps[n];
  b.rshStops[0] := b.rshStops[n];
  b.jntStopTimes[0] := b.jntStopTimes[n];
  b.lshStopTimes[0] := b.lshStopTimes[n];
  b.rshStopTimes[0] := b.rshStopTimes[n];

  b.jntStarts[n+1] := b.jntStarts[1];
  b.jntStops[n+1] := b.jntStops[1];
  b.lshStarts[n+1] := b.lshStarts[1];
  b.lshBumps[n+1] := b.lshBumps[1];
  b.lshStops[n+1] := b.lshStops[1];
  b.rshStarts[n+1] := b.rshStarts[1];
  b.rshBumps[n+1] := b.rshBumps[1];
  b.rshStops[n+1] := b.rshStops[1];
  b.jntStopTimes[n+1] := b.jntStopTimes[1];
  b.lshStopTimes[n+1] := b.lshStopTimes[1];
  b.rshStopTimes[n+1] := b.rshStopTimes[1];

  b.jnt := NEW(REF ARRAY OF GraphVBT.Vertex, n+2);
  b.lsh := NEW(REF ARRAY OF GraphVBT.Vertex, n+2);
  b.rsh := NEW(REF ARRAY OF GraphVBT.Vertex, n+2);

  FOR i := 1 TO n DO
    b.jnt[i]:=NEW(GraphVBT.Vertex, graph := view.mg,
             pos := b.jntStarts[i],
            shape := GraphVBT.VertexShape.Ellipse,
            size := R2.T{0.0, 0.0}).init();
    b.lsh[i]:=NEW(GraphVBT.Vertex, graph := view.mg,
             pos := b.lshStarts[i],
            shape := GraphVBT.VertexShape.Ellipse,
            size := R2.T{0.0, 0.0}).init();
    b.rsh[i]:=NEW(GraphVBT.Vertex, graph := view.mg,
             pos := b.rshStarts[i],
            shape := GraphVBT.VertexShape.Ellipse,
            size := R2.T{0.0, 0.0}).init();
  END;
  b.jnt[0] := b.jnt[n];
  b.jnt[n+1] := b.jnt[1];
  b.lsh[0] := b.lsh[n];
  b.lsh[n+1] := b.lsh[1];
  b.rsh[0] := b.rsh[n];
  b.rsh[n+1] := b.rsh[1];
  band := NEW(Edges, n+2);
  FOR i := 1 TO n DO
    band[i] := NEW(GraphVBT.Edge,
                vertex0 := b.jnt[i],
                vertex1 := b.jnt[i+1],
                control0 := b.rsh[i],
                control1 := b.lsh[i+1],
                color := MyColors.Band()).init();
  END;
  view.mg.redisplay();
  END Stretch;

PROCEDURE Snap (             view      : T; 
                             hullSites : IntList.T; 
                <* UNUSED *> otherSites: IntList.T) RAISES {Thread.Alerted} =
  VAR n: INTEGER;
      b: BandData;
  BEGIN
  b := view.bandData;
  n := IntList.Length (hullSites);
  LOCK view.mg.mu DO
    FOR i := 1 TO n DO
      b.jnt[i].move(b.jntStops[i], TRUE, 
                    path := NEW(JointPath, 
                                pStart := b.jntStarts[i],
                                pStop := b.jntStops[i],
                                tStop := b.jntStopTimes[i]));
      b.lsh[i].move(b.lshStops[i], TRUE, 
                    path := NEW(ShoulderPath, 
                                pStart := b.lshStarts[i],
                                pBump := b.lshBumps[i],
                                pStop := b.lshStops[i],
                                tBump := b.jntStopTimes[i],
                                tStop := b.lshStopTimes[i]));
      b.rsh[i].move(b.rshStops[i], TRUE, 
                    path := NEW(ShoulderPath, 
                                pStart := b.rshStarts[i],
                                pBump := b.rshBumps[i],
                                pStop := b.rshStops[i],
                                tBump := b.jntStopTimes[i],
                                tStop := b.rshStopTimes[i]));
    END;
  END;
  view.mg.animate(0.0, 2.0);
  FOR i := 1 TO n DO
    ConfirmOne(view, b.hullSiteUids[i], b.hullSiteUids[i+1]);
  END;
  view.mg.redisplay();
  END Snap;

PROCEDURE ConstPos(path: ConstPath; <* UNUSED *> t: REAL): R2.T =
  BEGIN RETURN(path.p0) END ConstPos;

PROCEDURE AffinePos(path: AffinePath; t: REAL): R2.T =
  BEGIN RETURN(R2.Mix(path.p0, 1.0-t, path.p1, t)) END AffinePos;

PROCEDURE JointPos(path: JointPath; t: REAL): R2.T =
  BEGIN 
    IF t < path.tStop THEN
      RETURN(R2.Mix(path.pStart, (path.tStop - t)/path.tStop, 
                  path.pStop, t/path.tStop)) 
    ELSE
      RETURN(path.pStop);
    END;
  END JointPos;

PROCEDURE ShoulderPos(path: ShoulderPath; t: REAL): R2.T =
  BEGIN 
    IF t < path.tBump THEN
      RETURN(R2.Mix(path.pStart, (path.tBump - t)/path.tBump, 
                  path.pBump, t/path.tBump)) 
    ELSIF t < path.tStop THEN
      RETURN(R2.Mix(path.pBump, (path.tStop - t)/(path.tStop - path.tBump), 
                  path.pStop, (t - path.tBump)/(path.tStop - path.tBump))) 
    ELSE
      RETURN(path.pStop);
    END;
  END ShoulderPos;

PROCEDURE BezierPos(path: BezierPath; t: REAL): R2.T =
  VAR q0, q1, q2, r0, r1: R2.T;
  BEGIN
  q0 := R2.Mix(path.p0, 1.0-t, path.p1, t);
  q1 := R2.Mix(path.p1, 1.0-t, path.p2, t);
  q2 := R2.Mix(path.p2, 1.0-t, path.p3, t);

  r0 := R2.Mix(q0, 1.0-t, q1, t);
  r1 := R2.Mix(q1, 1.0-t, q2, t);

  RETURN(R2.Mix(r0, 1.0-t, r1, t));
  END BezierPos;

PROCEDURE FrontPos(path: FrontPath; t: REAL): R2.T =
  BEGIN
  IF path.m.curTime # t THEN Update(path.m, t) END;
  RETURN(path.m.curFrontPos)
  END FrontPos;

PROCEDURE BackPos(path: BackPath; t: REAL): R2.T =
  BEGIN
  IF path.m.curTime # t THEN Update(path.m, t) END;
  RETURN(path.m.curBackPos)
  END BackPos;

PROCEDURE RightPos(path: RightPath; t: REAL): R2.T =
  BEGIN
  IF path.m.curTime # t THEN Update(path.m, t) END;
  RETURN(path.m.curRightPos)
  END RightPos;

PROCEDURE LeftPos(path: LeftPath; t: REAL): R2.T =
  BEGIN
  IF path.m.curTime # t THEN Update(path.m, t) END;
  RETURN(path.m.curLeftPos)
  END LeftPos;

PROCEDURE Update(m: Movie; t: REAL) =
  VAR center, del, delta, delta90: R2.T;
  BEGIN
  m.curHeadPos := m.head.pos(t);
  m.curTailPos := m.tail.pos(t);
  del := R2.Sub(m.curHeadPos, m.curTailPos);
  delta := R2.Scale(Big/R2.L2Norm(del), del);
  delta90 := R2.T{delta[1], -delta[0]};
  center := R2.Mix(m.curHeadPos, 0.5, m.curTailPos, 0.5);
  m.curFrontPos := R2.Add(m.curHeadPos, delta);
  m.curRightPos := R2.Add(center, delta90);
  m.curBackPos := R2.Sub(m.curTailPos, delta);
  m.curLeftPos := R2.Sub(center, delta90);
  m.curTime := t;
  END Update;



BEGIN
  ZeusPanel.RegisterView (New, "Geometry View", "Hull");
END GeomView.


