(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Jan  5 16:27:34 PST 1995 by najork   *)
(*      modified on Tue Aug 17 12:23:01 PDT 1993 by comba    *)
(*      modified on Mon Aug 16 19:56:39 PDT 1993 by harrison *)
(*      modified on Sun Jul 11 21:00:11 PDT 1993 by mhb *)

<* PRAGMA LL *>

MODULE ViewIncrementalReal;

IMPORT AnimationPath, BresenhamViewClass, Filter, GraphVBT, Math, 
       PaintOp, R2, RefList, Thread, VBT, View, ZeusPanel;


CONST
  NSUBDIV   = 16 ;
  NPOINTS   = 6 + NSUBDIV ;
  IR        = 0.67 ;
  LupeBorder = 0.05;
  SR        = 0.75 * IR ;
  LR        = 1.5 * IR ;
  XC        = 0.5 ;
  YC        = 0.5 ;  

  ArrowAtFarEnd = ARRAY [0 .. 1] OF BOOLEAN{FALSE, TRUE};

TYPE
  T = BresenhamViewClass.T BRANDED OBJECT
        graph: GraphVBT.T; (* drawing canvas that fills the view *)
        font: GraphVBT.WorldFont; (* set by Setup *)        
        width, height: INTEGER;
        x1, y1, x2, y2 : INTEGER ;
        p_low, p_high : INTEGER ;
        p             : INTEGER ;
        firstPixel := TRUE;
        nrPixels      : INTEGER ;
        
        error_edge: GraphVBT.Edge;
        error_offset: REAL;
        error_pos0, error_pos1: R2.T;
      OVERRIDES
        <* LL=0 *>
        oeErrorInit := ErrorInit ;
        oeSetup    := Setup;
        oeNewLine  := NewLine ;
        oeShowPixel := ShowPixel;
        oeFindError := FindError;
        oeChangeError := ChangeError;
        oeMove     := Move ;
      END;

VAR
  resid := FALSE ;

  pV : ARRAY [0 .. 9] OF GraphVBT.Vertex ;
  pH : ARRAY [0 .. 9] OF GraphVBT.Vertex ; 

  h0, h1, h2, h3,
  v0, v1, v2, v3  : GraphVBT.Edge ;

  lPt0, lPt1       : R2.T ;
  linePt0, linePt1 : GraphVBT.Vertex ;

  errorPt1, errorPt2 : GraphVBT.Vertex ;
  errorLine          : GraphVBT.Edge ;

  pixel1, pixel2, residual : GraphVBT.Vertex ;

  e1Line, e2Line : GraphVBT.Edge ;

  e1Size, e2Size : REAL ;

  s0, s1, s2, s3 : GraphVBT.Vertex ;

  Black := PaintOp.FromRGB(0.0, 0.0, 0.0);

  bgColor    := PaintOp.FromRGB(0.6, 0.6, 0.6);
  lineColor  := PaintOp.FromRGB(0.2, 1.0, 0.2);
  errCelClr  := PaintOp.FromRGB(0.4, 0.4, 0.4);
  gridColor  := PaintOp.FromRGB(0.0, 0.0, 0.0);
  pixelColor := PaintOp.FromRGB(0.0, 0.0, 0.0);
  e1Color    := PaintOp.FromRGB(1.0, 1.0, 0.0);
  e2Color    := PaintOp.FromRGB(1.0, 0.0, 0.0);
  errorColor := PaintOp.FromRGB(0.0, 0.0, 1.0);

PROCEDURE New (): View.T =
  BEGIN RETURN NEW(T).init(NIL) END New;

PROCEDURE DrawLupe(view: T; color: PaintOp.T; radius: REAL) = 
  VAR
    polygon : ARRAY [0..NPOINTS-1] OF REFANY ; 
    angIncr := Math.Pi / FLOAT(NSUBDIV) ; 
    ang     : LONGREAL ;
  BEGIN
    polygon [0] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC-radius, YC}).init();
    polygon [1] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC-LR, YC}).init();
    polygon [2] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC-LR, YC+LR}).init();
    polygon [3] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC+LR+1.0, YC+LR}).init();
    polygon [4] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC+LR+1.0, YC}).init();
    polygon [5] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC+radius+1.0, YC}).init();


    FOR i := 0 TO NSUBDIV-1 DO
      ang := FLOAT(FLOAT(i) * angIncr, LONGREAL);
      polygon[i+6]:=
        NEW(GraphVBT.Vertex, graph:=view.graph, 
            pos:=R2.T{
               FLOAT(Math.cos(ang)) * radius + XC, 
               FLOAT(Math.sin(ang)) * radius + YC
            }).init();
    END ;

    NEW(GraphVBT.Polygon,
      vertices := RefList.FromArray (polygon),
      color := color).init().toFront(GraphVBT.ZOrder.Foreground);

    polygon [0] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC-radius, YC}).init();
    polygon [1] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC-LR, YC}).init();
    polygon [2] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC-LR, YC-LR}).init();
    polygon [3] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC+LR+1.0, YC-LR}).init();
    polygon [4] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC+LR+1.0, YC}).init();
    polygon [5] := 
        NEW(GraphVBT.Vertex, graph:=view.graph, pos:=R2.T{XC+radius+1.0, YC}).init();

    FOR i := 0 TO NSUBDIV-1 DO
      ang := FLOAT(FLOAT(i) * angIncr, LONGREAL);
      polygon[i+6]:=
        NEW(GraphVBT.Vertex, graph:=view.graph, 
            pos:=R2.T{
               FLOAT(Math.cos(ang)) * radius + XC, 
               YC - FLOAT(Math.sin(ang)) * radius
            }).init();
    END ;

    NEW(GraphVBT.Polygon,
      vertices := RefList.FromArray (polygon),
      color := color).init().toFront(GraphVBT.ZOrder.Foreground);
  END DrawLupe;

PROCEDURE Setup (view: T; width, height: INTEGER; <*UNUSED*> show : BOOLEAN) =
  BEGIN
    view.graph := 
      NEW(GraphVBT.T, 
        world := GraphVBT.WorldRectangle{
             w := -0.2, s := -0.2, e := 1.7, n := 1.2}).init();
    view.width := width;
    view.height := height;
    view.font := view.graph.font(
                   family := "Helvetica", weight := "bold",
                   slant := GraphVBT.Slant.Roman, size := 1.0);

    view.firstPixel := TRUE;
    view.nrPixels := 0 ;

    LOCK VBT.mu DO 
      EVAL Filter.Replace(view, view.graph) 
    END;

    NEW(GraphVBT.Vertex,
             graph := view.graph,
             pos := R2.T{0.5, 0.5},
             size := R2.T{1.6, 1.6},
             color := bgColor).init().toBack(GraphVBT.ZOrder.Background);

    DrawLupe(view, Black, IR - LupeBorder);
    DrawLupe(view, bgColor, IR);

    pH[0] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC-IR, YC-LR}).init();
    pH[1] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC+IR, YC-LR}).init();
    pH[2] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC-IR, YC-SR}).init();
    pH[3] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC+IR, YC-SR}).init();
    pH[4] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC-IR, YC}).init();
    pH[5] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC+IR, YC}).init();
    pH[6] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC-IR, YC+SR}).init();
    pH[7] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC+IR, YC+SR}).init();
    pH[8] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC-IR, YC+LR}).init();
    pH[9] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC+IR, YC+LR}).init();

    h0 := 
      NEW(GraphVBT.Edge,
        vertex0 := pH[2],
        vertex1 := pH[3],
        width := 0.007,
        color := gridColor
      ).init() ;
    h0.toFront (GraphVBT.ZOrder.Normal) ;

    h1 := 
      NEW(GraphVBT.Edge,
        vertex0 := pH[4],
        vertex1 := pH[5],
        width := 0.007,
        color := gridColor
      ).init() ;
    h1.toFront (GraphVBT.ZOrder.Normal) ;

    h2 := 
      NEW(GraphVBT.Edge,
        vertex0 := pH[6],
        vertex1 := pH[7],
        width := 0.007,
        color := gridColor
      ).init() ;
    h2.toFront (GraphVBT.ZOrder.Normal) ;

    pV[0] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC-LR, YC-IR}).init();
    pV[1] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC-LR, YC+IR}).init();
    pV[2] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC-SR, YC-IR}).init();
    pV[3] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC-SR, YC+IR}).init();
    pV[4] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC, YC-IR}).init();
    pV[5] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC, YC+IR}).init();
    pV[6] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC+SR, YC-IR}).init();
    pV[7] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC+SR, YC+IR}).init();
    pV[8] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC+LR, YC-IR}).init();
    pV[9] := NEW(GraphVBT.Vertex,graph:= view.graph, pos := R2.T{XC+LR, YC+IR}).init();

    v0 := 
      NEW(GraphVBT.Edge,
        vertex0 := pV[2],
        vertex1 := pV[3],
        width := 0.007,
        color := gridColor
      ).init() ;
    v0.toFront (GraphVBT.ZOrder.Normal) ;

    v1 := 
      NEW(GraphVBT.Edge,
        vertex0 := pV[4],
        vertex1 := pV[5],        
        width := 0.007,
        color := gridColor
      ).init() ;
    v1.toFront (GraphVBT.ZOrder.Normal) ;

    v2 := 
      NEW(GraphVBT.Edge,
        vertex0 := pV[6],
        vertex1 := pV[7], 
        width := 0.007,
        color := gridColor
      ).init() ;
    v2.toFront (GraphVBT.ZOrder.Normal) ;

    s0 := 
       NEW(GraphVBT.Vertex,
            graph := view.graph,
            pos := R2.T{0.25, 0.25},
            size := R2.T{0.05, 0.05},
            color := errCelClr,
            shape := GraphVBT.VertexShape.Ellipse).init() ;
    s0.toFront (GraphVBT.ZOrder.Normal) ;

    s1 :=
        NEW(GraphVBT.Vertex,
            graph := view.graph,
            pos := R2.T{0.75, 0.25},
            size := R2.T{0.05, 0.05},
            color := errCelClr,
            shape := GraphVBT.VertexShape.Ellipse).init() ;
    s1.toFront (GraphVBT.ZOrder.Foreground) ;

    s2 := 
       NEW(GraphVBT.Vertex,
            graph := view.graph,
            pos := R2.T{0.25, 0.75},
            size := R2.T{0.05, 0.05},
            color := errCelClr,
            shape := GraphVBT.VertexShape.Ellipse).init() ;
    s2.toFront (GraphVBT.ZOrder.Foreground) ;

    s3 :=
        NEW(GraphVBT.Vertex,
            graph := view.graph,
            pos := R2.T{0.75, 0.75},
            size := R2.T{0.05, 0.05},
            color := errCelClr,
            shape := GraphVBT.VertexShape.Ellipse).init() ;
    s3.toFront (GraphVBT.ZOrder.Foreground) ;

    NEW(GraphVBT.Edge,
      vertex0 := s0,
      vertex1 := s1,
      width := 0.003,
      color := errCelClr
    ).init().toFront (GraphVBT.ZOrder.Foreground) ;

    NEW(GraphVBT.Edge,
      vertex0 := s2,
      vertex1 := s3,
      width := 0.003,
      color := errCelClr
    ).init().toFront (GraphVBT.ZOrder.Foreground) ;

    NEW(GraphVBT.Edge,
      vertex0 := s0,
      vertex1 := s2,
      width := 0.003,
      color := errCelClr
    ).init().toFront (GraphVBT.ZOrder.Foreground) ;

    NEW(GraphVBT.Edge,
      vertex0 := s1,
      vertex1 := s3,
      width := 0.003,
      color := errCelClr
    ).init().toFront (GraphVBT.ZOrder.Foreground) ;

    view.graph.redisplay()
  END Setup;

PROCEDURE ErrorInit (view : T) RAISES {Thread.Alerted} =
  BEGIN
    VAR
      v0 := e1Line.vertex0;
      v1 := e1Line.vertex1;
      p0 := R2.T{1.3, 0.5};
      p1 := R2.T{1.3, 0.5 + e1Size};
      path0 := NEW(AnimationPath.StraightPath).init(v0.pos, p0) ;        
      path1 := NEW(AnimationPath.StraightPath).init(v1.pos, p1);
    BEGIN
      v0.move (
          pos := p0,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path0) ;
      v1.move (
          pos := p1,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path1) ;
    END;
      
    VAR
      v0 := e2Line.vertex0;
      v1 := e2Line.vertex1;
      p0 := R2.T{1.3, 0.5};
      p1 := R2.T{1.3, 0.5 - e2Size};
      path0 := NEW(AnimationPath.StraightPath).init(v0.pos, p0) ;        
      path1 := NEW(AnimationPath.StraightPath).init(v1.pos, p1);
    BEGIN
      v0.move (
          pos := p0,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path0) ;
      v1.move (
          pos := p1,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path1) ;
    END;
      
    view.graph.animate(0.0, 1.0) ; 

    errorPt1 := NEW(GraphVBT.Vertex, graph := view.graph,
                    pos := R2.T{0.75, 0.25}).init();
    errorPt2 := NEW(GraphVBT.Vertex, graph := view.graph,
                    pos := R2.T{0.75, 0.25 + e1Size}).init();

    errorLine :=
      NEW(GraphVBT.Edge, vertex0 := errorPt1, vertex1 := errorPt2,
          width := 0.01, color := errorColor).init();

    view.graph.redisplay()
  END ErrorInit ;

PROCEDURE NewLine (view: T; x1, y1, x2, y2: INTEGER) =
  VAR slope: REAL;
  BEGIN
    view.x1 := x1;
    view.y1 := y1;
    view.x2 := x2;
    view.y2 := y2;
    view.p_low := 2 * (view.y2 - view.y1 - (view.x2 - view.x1));
    view.p_high := 2 * (view.y2 - view.y1) - 1;

    lPt0 := R2.T{0.25, 0.25};
    linePt0 := NEW(GraphVBT.Vertex, graph := view.graph, pos := lPt0,
                   color := lineColor).init();
    linePt0.toFront(GraphVBT.ZOrder.Normal);

    lPt1 := R2.T{0.25 + FLOAT(view.width - 1) * 0.5,
                 0.25 + FLOAT(view.height - 1) * 0.5};
    linePt1 := NEW(GraphVBT.Vertex, graph := view.graph, pos := lPt1,
                   size := R2.T{0.05, 0.05}, color := errCelClr,
                   shape := GraphVBT.VertexShape.Ellipse).init();
    linePt1.toFront(GraphVBT.ZOrder.Normal);

    NEW(GraphVBT.Edge,
      vertex0 := linePt0,
      vertex1 := linePt1,
      width := 0.1,
      color := lineColor
    ).init().toFront (GraphVBT.ZOrder.Normal) ;

    NEW(GraphVBT.Edge,
      vertex0 := linePt0,
      vertex1 := linePt1,
      width := 0.001,
      color := Black
    ).init().toFront (GraphVBT.ZOrder.Normal) ;

    slope := (FLOAT(y2 - y1) / FLOAT(x2 - x1)) / 2.0;
 
    e1Size := slope ;
    e2Size := 0.5 - slope ;

    e1Line :=
      NEW(
        GraphVBT.Edge,
        vertex0 :=
          NEW(GraphVBT.Vertex, graph := view.graph, pos := R2.T{0.75, 0.25}).init(),
        vertex1 := NEW(GraphVBT.Vertex, graph := view.graph,
                       pos := R2.T{0.75, 0.25 + e1Size}).init(),
        arrow := ArrowAtFarEnd,
        width := 0.01, color := e1Color).init();
    e1Line.toFront(GraphVBT.ZOrder.Foreground);

    e2Line :=
      NEW(
        GraphVBT.Edge, vertex0 := NEW(GraphVBT.Vertex, graph := view.graph,
                                      pos := R2.T{0.75, 0.75}).init(),
        vertex1 := NEW(GraphVBT.Vertex, graph := view.graph,
                       pos := R2.T{0.75, 0.75 - e2Size}).init(),
        arrow := ArrowAtFarEnd,
        width := 0.01, color := e2Color).init();
    e2Line.toFront(GraphVBT.ZOrder.Foreground);

    view.graph.redisplay()
  END NewLine;

PROCEDURE FindError(view: T; p: INTEGER) RAISES {Thread.Alerted} =
  VAR
    yInt := ((FLOAT(p)/(2.0*FLOAT(view.width)))+ 0.5) / 2.0 + 0.25 ;
    pos := R2.T{0.8, yInt};

  PROCEDURE AnimateEdge(line: GraphVBT.Edge) = 
    VAR
      v0 := line.vertex0;
      v1 := line.vertex1;
      p0 := pos;
      p1 := R2.Add(pos, R2.Sub(v1.pos, v0.pos));
      path0 := NEW(AnimationPath.StraightPath).init(v0.pos, p0) ;        
      path1 := NEW(AnimationPath.StraightPath).init(v1.pos, p1);
    BEGIN
      v0.move (
          pos := p0,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path0) ;
      v1.move (
          pos := p1,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path1) ;

      view.graph.redisplay() ;      
    END AnimateEdge;

  BEGIN
    IF p < 0 THEN
      view.error_edge := e1Line;
      view.error_offset := e1Size;
    ELSE
      view.error_edge := e2Line;
      view.error_offset := -e2Size;
    END;

    view.error_pos0 := view.error_edge.vertex0.pos;
    view.error_pos1 := view.error_edge.vertex1.pos;

    AnimateEdge(view.error_edge);

    view.graph.animate(0.0, 1.0) ; 
    view.graph.redisplay() ;
  END FindError;

PROCEDURE ChangeError(view: T; <*UNUSED*> p: INTEGER) RAISES {Thread.Alerted} =

  PROCEDURE AnimateLine(yOffset: REAL) =
    VAR
      v := errorLine.vertex1 ;
      p := R2.Add(v.pos, R2.T{0.0, yOffset}) ;
      path := NEW(AnimationPath.StraightPath).init(v.pos, p) ;
    BEGIN
      v.move (pos      := p, 
              animated := TRUE, 
              start    := 0.0, 
              stop     := 1.0, 
              path     := path) ; 
      END AnimateLine;
      
  BEGIN
    AnimateLine(view.error_offset);

    view.graph.animate(0.0, 1.0) ; 

    view.error_edge.vertex0.move(view.error_pos0);
    view.error_edge.vertex1.move(view.error_pos1);

    view.graph.redisplay() ;
  END ChangeError;

PROCEDURE ShowPixel(view: T; 
         <*UNUSED*> x:    INTEGER;
         <*UNUSED*> y:    INTEGER;
                    p1:   INTEGER;
         <*UNUSED*> p2:   INTEGER) RAISES {Thread.Alerted} =
  VAR
    pt0 := R2.T{0.25,0.25} ;
    pt1 := R2.T{0.75,0.25} ;
    pt2 := R2.T{0.75,0.75} ;
    path01 := NEW(AnimationPath.StraightPath).init(pt0, pt1) ; 
    path02 := NEW(AnimationPath.StraightPath).init(pt0, pt2) ; 
  BEGIN
    IF view.firstPixel THEN 
      pixel1 := 
        NEW(GraphVBT.Vertex,
            graph := view.graph,
            pos := R2.T{0.25, 0.25},
            size := R2.T{0.5, 0.5},
            color := pixelColor,
            shape := GraphVBT.VertexShape.Ellipse).init();

      pixel1.toFront(GraphVBT.ZOrder.Background);       
    ELSE      
      pixel2 := NEW(GraphVBT.Vertex,
            graph := view.graph,
            pos := R2.T{0.25, 0.25},
            size := R2.T{0.5, 0.5},
            color := pixelColor,
            shape := GraphVBT.VertexShape.Ellipse).init();
      pixel2.toFront(GraphVBT.ZOrder.Background);
      IF p1 < 0 THEN         
        pixel2.move (
          pos := pt1,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path01) ;
        view.graph.animate(0.0, 1.0) ;
      ELSE
        pixel2.move (
          pos := pt2,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path02) ;
        view.graph.animate(0.0, 1.0) ;    
      END ;
    END ;

    view.firstPixel := FALSE;

    INC (view.nrPixels) ;

    view.graph.redisplay() ;
  END ShowPixel ;

PROCEDURE Move (view : T; p : INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    v3 := 
      NEW(GraphVBT.Edge,
        vertex0 := pV[8],
        vertex1 := pV[9], 
        width := 0.007,
        color := gridColor
      ).init() ;
    v3.toFront (GraphVBT.ZOrder.Normal) ;
    IF p < 0 THEN
      VAR
        p0    := R2.T{0.75,0.25} ; 
        p1    := R2.T{0.25,0.25} ; 
        p2    := R2.T{-0.25,0.25} ;
        p3    := R2.T{-0.5,-0.25} ;
        nPt0  := R2.T{lPt0[0]-0.5, lPt0[1]} ;
        nPt1  := R2.T{lPt1[0]-0.5, lPt1[1]} ;        
        path0 := NEW(AnimationPath.StraightPath).init(p0, p1) ;
        path1 := NEW(AnimationPath.StraightPath).init(p1, p2) ;
        path2 := NEW(AnimationPath.StraightPath).init(p2, p3) ;
        path0l:= NEW(AnimationPath.StraightPath).init(lPt0, nPt0) ;
        path1l:= NEW(AnimationPath.StraightPath).init(lPt1, nPt1) ;
      BEGIN
        pixel2.move (pos := p1, animated := TRUE, path := path0) ;
        pixel1.move (pos := p2, animated := TRUE, path := path1) ;
        IF resid AND (view.nrPixels > 2) THEN
          residual.move (pos := p3, animated := TRUE, path := path2) ;
        END ;

        v0.move   (vertex0:=pV[0], vertex1:=pV[1], animated:=TRUE) ;
        v1.move   (vertex0:=pV[2], vertex1:=pV[3], animated:=TRUE) ;
        v2.move   (vertex0:=pV[4], vertex1:=pV[5], animated:=TRUE) ;
        v3.move   (vertex0:=pV[6], vertex1:=pV[7], animated:=TRUE) ;

        linePt0.move (pos:= nPt0, animated:=TRUE, path:= path0l) ;
        linePt1.move (pos:= nPt1, animated:=TRUE, path:= path1l) ;

        view.graph.animate(0.0, 1.0) ;          
        v0 := v1 ;
        v1 := v2 ;
        v2 := v3 ;
        residual := pixel1 ;
        pixel1   := pixel2 ;
        resid    := TRUE ;
        lPt0     := nPt0 ;
        lPt1     := nPt1 ;
      END ;
    ELSE
      VAR
        p0    := R2.T{0.75,0.75} ; 
        p1    := R2.T{0.25,0.25} ; 
        p2    := R2.T{-0.25,-0.25} ;
        p2a   := R2.T{-0.25,0.25} ;
        p3    := R2.T{-0.5,-0.25} ;        
        nPt0  := R2.T{lPt0[0]-0.5, lPt0[1]-0.5} ;
        nPt1  := R2.T{lPt1[0]-0.5, lPt1[1]-0.5} ;               
        path0 := NEW(AnimationPath.StraightPath).init(p0, p1) ;
        path1 := NEW(AnimationPath.StraightPath).init(p1, p2) ;
        path2 := NEW(AnimationPath.StraightPath).init(p2a, p3) ;
        path0l:= NEW(AnimationPath.StraightPath).init(lPt0, nPt0) ;
        path1l:= NEW(AnimationPath.StraightPath).init(lPt1, nPt1) ;      
      BEGIN
        pixel2.move (pos := p1, animated := TRUE, path := path0) ;
        pixel1.move (pos := p2, animated := TRUE, path := path1) ;
        IF resid AND (view.nrPixels > 2) THEN
          residual.move (pos := p3, animated := TRUE, path := path2) ;
        END ; 
     
        h3 := 
          NEW(GraphVBT.Edge,
            vertex0 := pH[8],
            vertex1 := pH[9],
            width := 0.007,
            color := gridColor
          ).init() ;
        h3.toFront (GraphVBT.ZOrder.Normal) ;    
        v0.move (vertex0:=pV[0], vertex1:=pV[1], animated:=TRUE) ;
        v1.move (vertex0:=pV[2], vertex1:=pV[3], animated:=TRUE) ;
        v2.move (vertex0:=pV[4], vertex1:=pV[5], animated:=TRUE) ;
        v3.move (vertex0:=pV[6], vertex1:=pV[7], animated:=TRUE) ;
        h0.move (vertex0:=pH[0], vertex1:=pH[1], animated:=TRUE) ;
        h1.move (vertex0:=pH[2], vertex1:=pH[3], animated:=TRUE) ;
        h2.move (vertex0:=pH[4], vertex1:=pH[5], animated:=TRUE) ;
        h3.move (vertex0:=pH[6], vertex1:=pH[7], animated:=TRUE) ;

        linePt0.move (pos:= nPt0, animated:=TRUE, path:= path0l) ;
        linePt1.move (pos:= nPt1, animated:=TRUE, path:= path1l) ;

        view.graph.animate(0.0, 1.0) ;          
        v0 := v1 ;
        v1 := v2 ;
        v2 := v3 ;
        h0 := h1 ;
        h1 := h2 ;
        h2 := h3 ;  
        residual := pixel1 ;  
        pixel1   := pixel2 ;
        resid    := FALSE ;
        lPt0     := nPt0 ;
        lPt1     := nPt1 ;      
      END ;      
    END ; 

    view.graph.redisplay()  
  END Move ;

BEGIN
  ZeusPanel.RegisterView (New, "Incremental Real", "Bresenham");
END ViewIncrementalReal.



