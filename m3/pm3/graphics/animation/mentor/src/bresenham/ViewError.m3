(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Jan  5 16:23:08 PST 1995 by najork   *)
(*      modified on Tue Aug 17 12:22:46 PDT 1993 by comba    *)
(*      modified on Mon Aug 16 19:50:14 PDT 1993 by harrison *)
(*      modified on Sun Jul 11 21:00:11 PDT 1993 by mhb *)
<* PRAGMA LL *>

MODULE ViewError;

IMPORT BresenhamViewClass, Filter, GraphVBT, 
       PaintOp, R2, VBT, View, ZeusPanel, Thread;

IMPORT RefList, Math ;

IMPORT AnimationPath ;

CONST
  NSUBDIV   = 16 ;
  NPOINTS   = 6 + NSUBDIV ;
  IR        = 0.67 ;
  LupeBorder = 0.05;
  SR        = 0.75 * IR ;
  LR        = 1.5 * IR ;
  XC        = 0.5 ;
  YC        = 0.5 ;  

TYPE
  T = BresenhamViewClass.T BRANDED OBJECT
        graph: GraphVBT.T; (* drawing canvas that fills the view *)
        font: GraphVBT.WorldFont; (* set by Setup *)        
        width, height: INTEGER;
        x1, y1, x2, y2 : INTEGER ;
        p_low, p_high : INTEGER ;
        firstPixel    := TRUE ;
        nrPixels      : INTEGER ;
      OVERRIDES
        <* LL=0 *>
        oeSetup    := Setup;
        oeNewLine  := NewLine ;
        oeShowPixel := ShowPixel;
        oeFindError := FindError;
        oeCompareError := CompareError;
        oeShowNextPixel := ShowNextPixel;
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

  pixel1, pixel2, residual : GraphVBT.Vertex ;

  e1, e2 : GraphVBT.Edge ;

  s0, s1, s2, s3 : GraphVBT.Vertex ;

  Black := PaintOp.FromRGB(0.0, 0.0, 0.0);

  bgColor    := PaintOp.FromRGB(0.6, 0.6, 0.6);
  lineColor  := PaintOp.FromRGB(0.2, 1.0, 0.2);
  errCelClr  := PaintOp.FromRGB(0.4, 0.4, 0.4);
  gridColor  := PaintOp.FromRGB(0.0, 0.0, 0.0);
  pixelColor := PaintOp.FromRGB(0.0, 0.0, 0.0);
  frameColor := PaintOp.FromRGB(0.8, 0.8, 0.8);
  e1Color    := PaintOp.FromRGB(1.0, 1.0, 0.0);
  e2Color    := PaintOp.FromRGB(1.0, 0.0, 0.0);

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

    view.firstPixel := TRUE ;
    view.nrPixels   := 0 ;

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

PROCEDURE NewLine (view: T; x1, y1, x2, y2: INTEGER) =

  BEGIN
    view.x1 := x1 ; view.y1 := y1 ;
    view.x2 := x2 ; view.y2 := y2 ;
    view.p_low := 2 * (view.y2 - view.y1 - (view.x2 - view.x1)) ;
    view.p_high := 2 * (view.y2 - view.y1) - 1 ;

    lPt0 := R2.T{0.25, 0.25} ;
    linePt0 := 
       NEW(GraphVBT.Vertex,
            graph := view.graph,
            pos := lPt0,
            color := lineColor).init() ;
    linePt0.toFront (GraphVBT.ZOrder.Normal) ;

    lPt1 := R2.T{0.25+FLOAT(view.width-1)*0.5, 0.25+FLOAT(view.height-1)*0.5} ;
    linePt1 :=
        NEW(GraphVBT.Vertex,
            graph := view.graph,
            pos := lPt1,
            size := R2.T{0.05, 0.05},
            color := errCelClr,
            shape := GraphVBT.VertexShape.Ellipse).init() ;
    linePt1.toFront (GraphVBT.ZOrder.Normal) ;

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

    view.graph.redisplay()  
  END NewLine ;

PROCEDURE FindError(view: T; p: INTEGER) RAISES {Thread.Alerted} =
  VAR 
    yInt := ((FLOAT(p)/(2.0*FLOAT(view.width)))+ 0.5) / 2.0 + 0.25 ;
    ptInt := R2.T{0.75, yInt} ;
    pt00 := R2.T{0.75, 0.75} ;
    v00 :=
        NEW(GraphVBT.Vertex, graph := view.graph,pos := pt00).init();
    pt01 := R2.T{0.75, 0.75} ;
    v01 :=
        NEW(GraphVBT.Vertex, graph := view.graph,pos := pt01).init();
    pt10 := R2.T{0.75, 0.25} ;
    v10 := 
        NEW(GraphVBT.Vertex, graph := view.graph,pos := pt10).init();
    pt11 := R2.T{0.75, 0.25} ;
    v11 :=
        NEW(GraphVBT.Vertex, graph := view.graph,pos := pt11).init();

    path00 := NEW(AnimationPath.StraightPath).init(pt00, ptInt) ;        
    path10 := NEW(AnimationPath.StraightPath).init(pt10, ptInt) ;

  BEGIN
    e1 := NEW(GraphVBT.Edge,
      vertex0 := v00,
      vertex1 := v01,
      width := 0.01,
      color := e1Color
    ).init() ;
    e1.toFront (GraphVBT.ZOrder.Foreground) ;
    e2 := NEW(GraphVBT.Edge,
      vertex0 := v10,
      vertex1 := v11,
      width := 0.01,
      color := e2Color
    ).init() ;    
    e2.toFront (GraphVBT.ZOrder.Foreground) ;
    v00.move (
          pos := ptInt,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path00) ;
    v10.move (
          pos := ptInt,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path10) ;

    view.graph.animate(0.0, 1.0) ; 
    view.graph.redisplay() ;
  END FindError;

PROCEDURE CompareError(view: T; p: INTEGER) RAISES {Thread.Alerted} =
  VAR 
    yInt := ((FLOAT(p)/(2.0*FLOAT(view.width)))+ 0.5) / 2.0 + 0.25 ;
    ptInt := R2.T{0.75, yInt} ;
    pt00 := R2.T{0.75, 0.75} ;
    v00 := e1.vertex0;
    pt01 := R2.T{0.75, 0.75} ;
    v01 := e1.vertex1;
    pt10 := R2.T{0.75, 0.25} ;
    v10 := e2.vertex0;
    pt11 := R2.T{0.75, 0.25} ;
    v11 := e2.vertex1;

    path00 := NEW(AnimationPath.StraightPath).init(pt00, ptInt) ;        
    path10 := NEW(AnimationPath.StraightPath).init(pt10, ptInt) ;

    path01,
    path11 : AnimationPath.StraightPath ;

    pt02, pt12 : R2.T ;
  BEGIN
    pt00 := R2.T {1.3, 0.5} ;
    pt02 := R2.T {1.3, 0.5 + ABS(pt01[1]-ptInt[1])} ;

    pt10 := R2.T {1.4, 0.5} ;
    pt12 := R2.T {1.4, 0.5 + ptInt[1]-pt11[1]} ;

    path00 := NEW(AnimationPath.StraightPath).init(ptInt, pt00) ;
    path01 := NEW(AnimationPath.StraightPath).init(pt01, pt02) ;

    path10 := NEW(AnimationPath.StraightPath).init(ptInt, pt12) ;
    path11 := NEW(AnimationPath.StraightPath).init(pt11, pt10) ;

    v00.move (
          pos := pt00,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path00) ;

    v01.move (
          pos := pt02,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path01) ;  

    v10.move (
          pos := pt12,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path10) ;

    v11.move (
          pos := pt10,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path11) ;    

    view.graph.animate(0.0, 1.0) ; 
    view.graph.redisplay();
  END CompareError;

PROCEDURE ShowNextPixel(view: T; p: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    IF p < 0 THEN
      FOR i := 0 TO 3 DO
        e2.setColor (e2Color) ;
        s1.setColor (e2Color) ;
        view.graph.redisplay() ;
        view.graph.animate(0.0 + FLOAT(i)*0.25, 0.0 + FLOAT(i+1)*0.25) ;
        e2.setColor (frameColor) ;
        s1.setColor (errCelClr) ;
        view.graph.redisplay() ;
      END ;      
    ELSE
      FOR i := 0 TO 3 DO
        e1.setColor (e1Color) ;
        s3.setColor (e1Color) ;
        view.graph.redisplay() ;
        view.graph.animate(0.0 + FLOAT(i)*0.25, 0.0 + FLOAT(i+1)*0.25) ;
        e1.setColor (frameColor) ;
        s3.setColor (errCelClr) ;
        view.graph.redisplay() ;
      END ;      
    END ;

    e1.remove() ;
    e2.remove() ;

    view.graph.redisplay()
  END ShowNextPixel;

PROCEDURE ShowPixel(view: T; 
         <*UNUSED*> x : INTEGER; 
         <*UNUSED*> y : INTEGER;
                    p1: INTEGER;
         <*UNUSED*> p2: INTEGER) RAISES {Thread.Alerted} =
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

    view.firstPixel := FALSE ;

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
    IF (p < 0) THEN
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
        IF resid AND (view.nrPixels > 2)THEN
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
  ZeusPanel.RegisterView (New, "Error", "Bresenham");
END ViewError.



