(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Tue Jan 31 15:40:30 PST 1995 by kalsow                   *)
(*      modified on Mon Aug  9 18:14:15 PDT 1993 by heydon                   *)

MODULE Topology;

IMPORT AlgGreedy, Graph;
IMPORT PktRouteIE;
IMPORT Rd, Text, Fmt, Thread, Sx, RefList, Word;

PROCEDURE FromName(nm: TEXT): Kind =
  BEGIN
    IF Text.Equal(nm, "grid") THEN RETURN Kind.Grid
    ELSIF Text.Equal(nm, "torus") THEN RETURN Kind.Torus
    ELSIF Text.Equal(nm, "butterfly") THEN RETURN Kind.Butterfly
    ELSIF Text.Equal(nm, "networkFromFile") THEN RETURN Kind.FromFile
    ELSE <* ASSERT FALSE *>
    END
  END FromName;

TYPE
  Grid = Graph.Sparse OBJECT
    w, h: CARDINAL;
  OVERRIDES
   nodeName := GridNodeName
  END;

PROCEDURE GridNodeName(g: Grid; id: CARDINAL): TEXT =
  VAR x := (id MOD g.w) + 1; y := (id DIV g.w) + 1; BEGIN
    RETURN Fmt.Int(y) & "," & Fmt.Int(x)
  END GridNodeName;

PROCEDURE NewGridBody(alg: AlgGreedy.T; w, h: CARDINAL): Graph.T
    RAISES {Thread.Alerted} =
  PROCEDURE NodeId(x, y: CARDINAL): CARDINAL =
    BEGIN RETURN x + y * w END NodeId;
  VAR g := NEW(Grid, w := w, h := h).init(sizeHint := h * w); BEGIN
    (* Add new nodes *)
    FOR j := 0 TO h - 1 DO
      FOR i := 0 TO w - 1 DO
        EVAL g.newNode();
        PktRouteIE.NewNode(alg, NodeId(i, j), FLOAT(i+2), FLOAT(j+2))
      END
    END;
    (* Add vertical edges *)
    FOR i := 0 TO w - 1 DO
      FOR j := 0 TO h - 2 DO
        VAR id1 := NodeId(i, j); id2 := NodeId(i, j+1); BEGIN
          g.newEdge(id1, id2);
          PktRouteIE.NewStraightEdge(alg, id1, id2)
        END
      END
    END;
    (* Add horizontal edges *)
    FOR i := 0 TO w - 2 DO
      FOR j := 0 TO h - 1 DO
        VAR id1 := NodeId(i, j); id2 := NodeId(i+1, j); BEGIN
          g.newEdge(id1, id2);
          PktRouteIE.NewStraightEdge(alg, id1, id2)
        END
      END
    END;
    (* Add grid labels *)
    FOR i := 1 TO w DO
      PktRouteIE.NewLabel(alg, Fmt.Int(i), FLOAT(i+1), 1.0)
    END;
    FOR j := 1 TO h DO
      PktRouteIE.NewLabel(alg, Fmt.Int(j), 1.0, FLOAT(j+1))
    END;
    RETURN g
  END NewGridBody;

PROCEDURE NewGrid(
    alg: AlgGreedy.T;
    w, h: CARDINAL;
    maxQueueSize: CARDINAL;
    bounded: BOOLEAN)
  : Graph.T
  RAISES {Thread.Alerted} =
  VAR res: Graph.T; BEGIN
    PktRouteIE.StartGraph(alg, w * h, maxQueueSize, bounded,
      FLOAT(w+2), FLOAT(h+2));
    res := NewGridBody(alg, w, h);
    PktRouteIE.EndGraph(alg);
    RETURN res
  END NewGrid;

PROCEDURE NewTorus(
    alg: AlgGreedy.T;
    w, h: CARDINAL;
    maxQueueSize: CARDINAL;
    bounded: BOOLEAN)
  : Graph.T
  RAISES {Thread.Alerted} =
  PROCEDURE NodeId(x, y: CARDINAL): CARDINAL =
    BEGIN RETURN x + y * w END NodeId;
  CONST Out = -1.0; Between = 0.5;
  VAR res: Graph.T; BEGIN
    PktRouteIE.StartGraph(alg, w * h, maxQueueSize, bounded,
      FLOAT(w+2), FLOAT(h+2));
    res := NewGridBody(alg, w, h);
    IF h > 1 THEN
      FOR i := 0 TO w - 1 DO
	VAR id1 := NodeId(i, 0); id2 := NodeId(i, h-1); BEGIN
	  res.newEdge(id1, id2);
	  PktRouteIE.NewCurvedEdge(alg, id1, id2,
	    FLOAT(i+2)+Between, 2.0-Out, FLOAT(i+2)+Between, FLOAT(h+1)+Out)
	END
      END
    END;
    IF w > 1 THEN
      FOR j := 0 TO h - 1 DO
	VAR id1 := NodeId(0, j); id2 := NodeId(w-1, j); BEGIN
	  res.newEdge(id1, id2);
	  PktRouteIE.NewCurvedEdge(alg, id1, id2,
	    2.0-Out, FLOAT(j+2)+Between, FLOAT(w+1)+Out, FLOAT(j+2)+Between)
	END
      END
    END;
    PktRouteIE.EndGraph(alg);
    RETURN res
  END NewTorus;

PROCEDURE NewButterfly(
    alg: AlgGreedy.T;
    dim: CARDINAL;
    maxQueueSize: CARDINAL;
    bounded: BOOLEAN)
  : Graph.T
  RAISES {Thread.Alerted} =
  PROCEDURE Exp(n: CARDINAL): CARDINAL =
    VAR res: CARDINAL := 1; BEGIN
      WHILE n > 0 DO res := res * 2; DEC(n) END;
      RETURN res
    END Exp;
  VAR
    w := dim + 1; h := Exp(dim); mask := h DIV 2;
    g := NEW(Grid, w := w, h := h).init(sizeHint := h * w);
  PROCEDURE NodeId(x, y: CARDINAL): CARDINAL =
    BEGIN RETURN x + y * w END NodeId;
  BEGIN
    PktRouteIE.StartGraph(alg, w * h, maxQueueSize, bounded,
      FLOAT(2*w), FLOAT(h+1));
    (* Add new nodes *)
    FOR j := 0 TO h - 1 DO
      FOR i := 0 TO w - 1 DO
        EVAL g.newNode();
        PktRouteIE.NewNode(alg, NodeId(i, j), FLOAT((2*i)+1), FLOAT(j+1))
      END
    END;
    (* Add edges *)
    FOR i := 0 TO w - 2 DO
      FOR j := 0 TO h - 1 DO
        VAR id1 := NodeId(i, j); id2a := NodeId(i+1, j); BEGIN
          g.newEdge(id1, id2a);
          PktRouteIE.NewStraightEdge(alg, id1, id2a);
          VAR id2b := NodeId(i+1, Word.Xor(j, mask)); BEGIN
            g.newEdge(id1, id2b);
            PktRouteIE.NewStraightEdge(alg, id1, id2b)
          END
        END
      END;
      mask := mask DIV 2
    END;
    PktRouteIE.EndGraph(alg);
    RETURN g
  END NewButterfly;

TYPE
  Coord = RECORD x, y: REAL END;
  Coords = REF ARRAY OF Coord;

PROCEDURE NewFromFile(
    alg: AlgGreedy.T;
    rd: Rd.T;
    maxQueueSize: CARDINAL;
    bounded: BOOLEAN)
  : Graph.T
  RAISES {BadGraph, Thread.Alerted} =
  VAR sx: Sx.T; curr: RefList.T; BEGIN
    TRY sx := Sx.Read(rd) EXCEPT
      Rd.EndOfFile => RAISE BadGraph("Unexpected end-of-file")
    | Sx.ReadError (msg) => RAISE BadGraph(msg)
    END;
    TYPECASE sx OF RefList.T (v) => curr := v ELSE
      RAISE BadGraph("File must be a list")
    END;
    VAR w, h: REAL; num: CARDINAL; g: Graph.Sparse; coords: Coords; BEGIN
      ReadCoord(curr.head, w, h); curr := curr.tail;
      num := RefList.Length(curr.head);
      g := NEW(Graph.Sparse).init(sizeHint := num);
      PktRouteIE.StartGraph(alg, num, maxQueueSize, bounded, w, h);
      coords := ReadNodes(alg, curr.head, g); curr := curr.tail;
      ReadEdges(alg, curr.head, g); curr := curr.tail;
      ReadLabels(alg, curr.head);
      IF curr.tail # NIL THEN
        RAISE BadGraph("Too many elements in top-level list")
      END;
      PktRouteIE.EndGraph(alg);
      RETURN g
    END
  END NewFromFile;

PROCEDURE ReadNodes(alg: AlgGreedy.T; l: RefList.T; g: Graph.Sparse): Coords
    RAISES {BadGraph, Thread.Alerted} =
  VAR id := 0; res: Coords;
  PROCEDURE ReadNode(node: RefList.T) RAISES {BadGraph, Thread.Alerted} =
    VAR x, y: REAL; BEGIN
      ReadCoord(node, x, y);
      res[id] := Coord{x := x, y := y};
      EVAL g.newNode();
      PktRouteIE.NewNode(alg, id, x, y)
    END ReadNode;
  BEGIN
    res := NEW(Coords, RefList.Length(l));
    WHILE l # NIL DO
      TYPECASE l.head OF RefList.T (r) => ReadNode(r) ELSE
        RAISE BadGraph("Bad node specification")
      END;
      INC(id);
      l := l.tail
    END;
    RETURN res
  END ReadNodes;

PROCEDURE ReadEdges(
    alg: AlgGreedy.T;
    l: RefList.T;
    g: Graph.Sparse)
    RAISES {BadGraph, Thread.Alerted} =
  PROCEDURE ReadEdge(edge: RefList.T) RAISES {BadGraph, Thread.Alerted} =
    VAR from, to: INTEGER; n := g.numNodes(); len:=RefList.Length(edge); BEGIN
      IF len # 2 AND len # 4 THEN
        RAISE BadGraph("Edge specification has wrong number of elements")
      END;
      TYPECASE edge.head OF REF INTEGER (i) => from := i^ ELSE
        RAISE BadGraph("Bad 'from' node in edge")
      END;
      edge := edge.tail;
      TYPECASE edge.head OF REF INTEGER (i) => to := i^ ELSE
        RAISE BadGraph("Bad 'to' node in edge")
      END;
      edge := edge.tail;
      IF NOT (0 <= from AND from < n AND 0 <= to AND to < n) THEN
        RAISE BadGraph("Bad node id in edge specification")
      END;
      g.newEdge(from, to);
      CASE len OF <* NOWARN *>
        2 => PktRouteIE.NewStraightEdge(alg, from, to)
      | 4 =>
          VAR x1, y1, x2, y2: REAL; BEGIN
            ReadCoord(edge.head, x1, y1); edge := edge.tail;
            ReadCoord(edge.head, x2, y2);
            PktRouteIE.NewCurvedEdge(alg, from, to, x1, y1, x2, y2)
          END
      END
    END ReadEdge;
  BEGIN
    WHILE l # NIL DO
      TYPECASE l.head OF RefList.T (r) => ReadEdge(r) ELSE
        RAISE BadGraph("Bad edge specification")
      END;
      l := l.tail
    END
  END ReadEdges;

PROCEDURE ReadLabels(alg: AlgGreedy.T; l: RefList.T)
    RAISES {BadGraph, Thread.Alerted} =
  PROCEDURE ReadLabel(label: RefList.T) RAISES {BadGraph, Thread.Alerted} =
    VAR txt: TEXT; x, y: REAL; BEGIN
      IF RefList.Length(label) # 3 THEN
        RAISE BadGraph("Label specification has wrong number of elements")
      END;
      TYPECASE label.head OF TEXT (t) => txt := t ELSE
        RAISE BadGraph("Bad label specification")
      END;
      label := label.tail;
      ReadCoord(label.head, x, y);
      PktRouteIE.NewLabel(alg, txt, x, y)
    END ReadLabel;
  BEGIN
    WHILE l # NIL DO
      TYPECASE l.head OF RefList.T (r) => ReadLabel(r) ELSE
        RAISE BadGraph("Bad label specification")
      END;
      l := l.tail
    END
  END ReadLabels;

PROCEDURE ReadCoord(l: RefList.T; VAR (*OUT*) x, y: REAL) RAISES {BadGraph} =
(* Requires "l" to be a list of 2 real numbers. Sets "x" and "y" to these two
   values; raises "BadGraph" otherwise. *)
  BEGIN
    TYPECASE l.head OF REF REAL (r) => x := r^ ELSE
      RAISE BadGraph("Bad x coordinate")
    END;
    l := l.tail;
    TYPECASE l.head OF REF REAL (r) => y := r^ ELSE
      RAISE BadGraph("Bad y coordinate")
    END;
    IF l.tail # NIL THEN
      RAISE BadGraph("Too many elements in coordinate pair")
    END
  END ReadCoord;

BEGIN
END Topology.
