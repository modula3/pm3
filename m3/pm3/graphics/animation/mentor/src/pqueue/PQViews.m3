(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Wed May  4 09:18:15 PDT 1994 by najork     *)
(*      modified on Wed Jan  6 15:44:49 PST 1993 by steveg     *)
(*      modified on Fri Jul 31 08:18:17 PDT 1992 by owicki     *)
(*      modified on Wed Jul 22 01:10:06 1992 by mhb        *)

MODULE PQViews;

IMPORT PQueueViewClass, Filter, GraphVBT, PQueue, R2, Fmt,
       TextVBT, Thread, View, ZeusPanel, RefList, PaintOp, VBT;

CONST 
      SortedX = -0.1;

TYPE
  T = PQueueViewClass.T BRANDED OBJECT
        mg: GraphVBT.T;
        size : INTEGER;
        width, height: INTEGER;
        offScreenPos: R2.T;
        sortedY, incrY: REAL;
        nodes: REF ARRAY OF GraphVBT.Vertex;
        edges: REF ARRAY OF GraphVBT.Edge;
        edgeList: RefList.T;
        highlights: REF ARRAY OF GraphVBT.VertexHighlight;
        activeHlt1, activeHlt2, activeHlt3: GraphVBT.VertexHighlight;
        workNode: GraphVBT.Vertex;
        vertexWidth: REAL;
        vertexHeight: REAL;
      OVERRIDES
        startrun := Startrun;
        oeSetup := Setup;
        oeInitSort := InitSort;
        oeHeapOpInit := HeapOpInit;
        oeHeapStep := HeapStep;
        oePlaceElement := PlaceElement;
        oeSortStep := SortStep;
        oeInsert := Insert;
        oeRemove := Remove;
        oePause := Pause;
        oeCompare := Compare;
      END;     

PROCEDURE Startrun (view: T) =
  (* sleazy hack: remove the old GraphVBT and just ignore it;
     heck, what else are VM and GC good for? *)
  BEGIN
    LOCK VBT.mu DO EVAL Filter.Replace(view, TextVBT.New("")); END;
    PQueueViewClass.T.startrun(view);
  END Startrun;

PROCEDURE Setup (view: T; size: INTEGER; doSort: BOOLEAN) =
  VAR level, numInLevel, node: INTEGER;
      pos, incr, lineStartPos: REAL;   
     graphSize := 1;
     levels:= 1;
     wc: GraphVBT.WorldRectangle; 
     north, west, south: REAL;
  BEGIN
    WHILE size > graphSize DO
      INC(levels);  graphSize := graphSize*2+1;
    END;
    view.size := size;
    view.width := (graphSize+1) DIV 2; view.height := levels;
    south := 0.5;
    IF doSort THEN
      north := 1.5 * FLOAT(view.height)+0.5;
      west := -0.2;
    ELSE
      north := FLOAT(view.height)+0.5;
      west := 0.0;
    END;

    wc := GraphVBT.WorldRectangle{
            w := west, s := south, e := 1.0, n := north};
     view.mg := NEW(GraphVBT.T, world := wc, margin := 0.05).init();
    LOCK VBT.mu DO EVAL Filter.Replace(view, view.mg); END;
    view.offScreenPos := R2.T{0.5, north+1.0};
    view.vertexWidth := MIN(0.80 / FLOAT(view.width), 0.18);
    IF doSort THEN
      view.vertexHeight := MIN(0.90 * (north - south) / FLOAT(size),
                              0.60 * (FLOAT(view.height)) / FLOAT(levels));
    ELSE
      view.vertexHeight := 0.60 * (north - south) / FLOAT(levels);
      view.size := 0;
    END;

    (* Initialize the nodes for the tree, but don't display them yet *)
    view.nodes := NEW(REF ARRAY OF GraphVBT.Vertex, size+1);
    view.edges := NEW(REF ARRAY OF GraphVBT.Edge, size+1);
    view.highlights := NEW(REF ARRAY OF GraphVBT.VertexHighlight, size+1);
    view.edgeList := NIL;
    level := view.height;
    node :=1; numInLevel := 1;
    lineStartPos := 0.5;
    incr := lineStartPos * 2.0;
    WHILE node <= size DO
      pos := lineStartPos;
      FOR j :=1 TO numInLevel DO
        view.nodes[node] := NEW(GraphVBT.Vertex, graph := view.mg,
             pos := R2.T{pos, FLOAT(level)},
             color := PQueue.StartColor,
             fontColor := PQueue.Black,
             size := R2.T{view.vertexWidth, view.vertexHeight});
          view.highlights[node] := NEW(GraphVBT.VertexHighlight, 
               vertex := view.nodes[node],
               color := PQueue.HighlightColor,
               border := R2.T{PQueue.HighlightWidth, PQueue.HighlightWidth});
        INC(node);
        pos := pos + incr;
        IF node > size THEN EXIT; END
      END;
      view.activeHlt1 := view.highlights[1];
      view.activeHlt2 := view.highlights[1];
      view.activeHlt3 := view.highlights[1];
      numInLevel := numInLevel*2;
      lineStartPos := lineStartPos / 2.0;
      incr := incr / 2.0;
      DEC(level);
    END;
    view.sortedY := 1.0;
    view.incrY := (1.5 * FLOAT(view.height)-0.5) / FLOAT(size);
  END Setup;

PROCEDURE InitSort(view: T; vals: REF ARRAY OF INTEGER) RAISES {Thread.Alerted}  =
  BEGIN
    FOR node := 1 TO view.size DO
      EVAL view.nodes[node].init();
      LOCK view.mg.mu DO view.nodes[node].setLabel(Fmt.Int(vals[node])); END;
      IF node > 1 THEN
        view.edges[node] := NEW(GraphVBT.Edge, vertex0 := view.nodes[node],
                      vertex1 := view.nodes[node DIV 2],
                      color := PQueue.NotInHeapEdgeColor,
                      width := PQueue.DefaultEdgeWidth).init();
      END;
    END;
    view.mg.redisplay();
    Pause(view);
  END InitSort;

PROCEDURE HeapOpInit(view: T; k: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    view.workNode := MakeDummy(view, k);
    view.mg.redisplay();
    LOCK view.mg.mu DO 
      view.workNode.move(R2.T{0.75, FLOAT(view.height)}, animated := TRUE);
      FOR j := 2*k TO 2*k + 1 DO
        IF j <= view.size THEN view.edges[j].setColor(PQueue.Black) END;
      END;
    END;  
    view.mg.animate(0.0, 1.0);
  END HeapOpInit;

PROCEDURE HeapStep(view: T; k,n: INTEGER; down: BOOLEAN) 
    RAISES {Thread.Alerted} =
  VAR newNode: GraphVBT.Vertex;
    edgeToHighlight: INTEGER;
  BEGIN
    IF down THEN edgeToHighlight := n ELSE edgeToHighlight := k END;
    newNode := MakeDummy(view, n);
    ClearHighlights(view);
    LOCK view.mg.mu DO 
      newNode.move(view.nodes[k].pos, animated := TRUE);
      view.edges[edgeToHighlight].setColor(PQueue.WorkColor);
      view.edges[edgeToHighlight].setWidth(PQueue.ThickEdgeWidth);
      view.edgeList := RefList.Cons (view.edges[edgeToHighlight], view.edgeList);
    END; 
    view.mg.animate(0.0, 1.0);  
    RemoveDummy(view, k, newNode, PQueue.StartColor); 
    view.mg.redisplay();
END HeapStep;

PROCEDURE PlaceElement(view: T; k: INTEGER) RAISES {Thread.Alerted} =
  VAR 
    edge: GraphVBT.Edge;
  BEGIN
    ClearHighlights(view);
    LOCK view.mg.mu DO
      view.workNode.toFront();
      view.workNode.move(view.nodes[k].pos, animated := TRUE);
    END;
    view.mg.animate(0.0, 1.0);
    RemoveDummy(view, k, view.workNode, PQueue.StartColor);
    LOCK view.mg.mu DO
      WHILE view.edgeList # NIL DO
        edge := view.edgeList.head;
        view.edgeList := view.edgeList.tail;
        edge.setWidth(PQueue.DefaultEdgeWidth);
        edge.setColor(PQueue.Black);
      END;
    END;
    view.mg.redisplay();
  END PlaceElement;

PROCEDURE SortStep(view: T; k: INTEGER) RAISES {Thread.Alerted} =
  VAR sorted, newRoot: GraphVBT.Vertex;
  BEGIN
    IF k > 1 THEN
      sorted := MakeDummy(view, 1);
      newRoot := MakeDummy(view, k);
      LOCK view.mg.mu DO 
        sorted.setColor(PQueue.SortedColor);
        sorted.setFontColor(PQueue.White);
        sorted.move(R2.T{SortedX, view.sortedY}, animated := TRUE);
        view.sortedY := view.sortedY + view.incrY;
        newRoot.move(view.nodes[1].pos, animated := TRUE);
        view.nodes[k].remove(); 
      END;
      view.mg.animate(0.0, 1.0);
      RemoveDummy(view, 1, newRoot, PQueue.StartColor);
      view.mg.redisplay();
    ELSE
      LOCK view.mg.mu DO
        view.nodes[1].setColor(PQueue.SortedColor);
        view.nodes[1].setFontColor(PQueue.White);
        view.nodes[1].move(R2.T{SortedX, view.sortedY}, animated := TRUE);
      END;
      view.mg.animate(0.0, 1.0);
    END;
END SortStep;

PROCEDURE Insert(view: T; k: INTEGER) =
  BEGIN
      INC(view.size);
      EVAL view.nodes[view.size].init();
      LOCK view.mg.mu DO view.nodes[view.size].setLabel(Fmt.Int(k)) END;
      IF view.size > 1 THEN
        view.edges[view.size] := NEW(GraphVBT.Edge,
                      vertex0 := view.nodes[view.size],
                      vertex1 := view.nodes[view.size DIV 2],
                      width := PQueue.DefaultEdgeWidth).init();
      END;    
    view.mg.redisplay();
  END Insert;

PROCEDURE Remove(view: T) RAISES {Thread.Alerted} =
  (* On entry, view.size > 0 *)
  VAR
    away, newRoot: GraphVBT.Vertex;
  BEGIN
    IF view.size > 1 THEN
      away := MakeDummy(view, 1);
      newRoot := MakeDummy(view, view.size);
      LOCK view.mg.mu DO
        away.move(view.offScreenPos, animated := TRUE);
        newRoot.move(view.nodes[1].pos, animated := TRUE);
        view.nodes[view.size].setColor(PQueue.StartColor);
        view.nodes[view.size].remove();
      END;
      view.mg.animate(0.0, 1.0);
      RemoveDummy(view, 1, newRoot, PQueue.StartColor);
      view.mg.redisplay();
    ELSE (* view.size = 1 : we don't get called with view.size < 1 *)
      away := MakeDummy(view, 1);
      LOCK view.mg.mu DO
        away.move(view.offScreenPos, animated := TRUE);
        view.nodes[view.size].setColor(PQueue.StartColor);
        view.nodes[view.size].remove();
      END;
      view.mg.animate(0.0, 1.0);  
    END;
    LOCK view.mg.mu DO away.remove(); END;
    DEC(view.size);
  END Remove;

(* Return a new node positioned on top of node n that has the same color and
   label as node n.  Make node n gray, and set its label to "" *)

PROCEDURE MakeDummy(view: T; n: INTEGER): GraphVBT.Vertex =
  VAR newNode: GraphVBT.Vertex;
  BEGIN
    newNode := NEW(GraphVBT.Vertex, graph := view.mg,
               pos := view.nodes[n].pos,
               color := view.nodes[n].color,
               fontColor := view.nodes[n].fontColor,
               label := view.nodes[n].label,
               shape := view.nodes[n].shape,
               size := view.nodes[n].size).init();
    LOCK view.mg.mu DO
      newNode.toFront();
      view.nodes[n].setColor(PQueue.WorkColor);
      view.nodes[n].setLabel("");
    END;
    RETURN newNode;
  END MakeDummy;

(* Remove "dummy", leaving vertex "n", which should be underneath it
   with the label in "dummy".  Change the color of vertex "n" to
   "color". *)

PROCEDURE RemoveDummy(view: T; n: INTEGER; dummy: GraphVBT.Vertex;
             color: PaintOp.T) =
  BEGIN
    LOCK view.mg.mu DO
      view.nodes[n].setColor(color);
      view.nodes[n].setLabel(dummy.label);
      dummy.remove();
    END;
  END RemoveDummy;

PROCEDURE Pause(view:T) RAISES {Thread.Alerted} =
  VAR
    offScreenNode: GraphVBT.Vertex;
  BEGIN
    offScreenNode := NEW(GraphVBT.Vertex, graph := view.mg,
        pos := R2.T{-1000.0, -1000.0},
        size := R2.T{1.0, 1.0}).init();
    LOCK view.mg.mu DO
      offScreenNode.move(R2.T{-1001.0, -1001.0}, animated := TRUE);
    END;
    view.mg.animate(0.0, 1.0);
    LOCK view.mg.mu DO offScreenNode.remove(); END;
  END Pause;

PROCEDURE Compare(view: T; k: INTEGER; n: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
      ClearHighlights(view);
      view.activeHlt1 := view.highlights[k].init(); 
      IF n > 0 THEN
        view.activeHlt2 := view.highlights[n].init();
      END;
      view.activeHlt3 := NEW(GraphVBT.VertexHighlight, 
               vertex := view.workNode,
               color := PQueue.HighlightColor,
               border := R2.T{PQueue.HighlightWidth, PQueue.HighlightWidth}
                             ).init();
    view.mg.redisplay();
    Pause(view);
  END Compare;

PROCEDURE ClearHighlights(view: T) =
  BEGIN
    LOCK view.mg.mu DO
      view.activeHlt1.remove();
      view.activeHlt2.remove();
      view.activeHlt3.remove();
    END;
  END ClearHighlights;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NEW(GraphVBT.T).init())
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Tree View", "PQueue");
END PQViews.
